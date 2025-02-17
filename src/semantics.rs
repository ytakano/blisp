use super::{parser, Pos};
use crate::{
    r#macro::Macros,
    runtime::{Environment, RTData},
    TypingContext,
};
use alloc::{
    boxed::Box,
    collections::{
        btree_map::{self, BTreeMap},
        btree_set::BTreeSet,
        linked_list::LinkedList,
    },
    fmt, format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use num_bigint::BigInt;

type ID = u64;
type Sbst = BTreeMap<ID, Type>;

#[derive(Clone, Debug)]
enum Type {
    TCon(Tycon),
    TVar(ID),
}

#[derive(Clone, Debug)]
struct Tycon {
    id: String,
    args: Option<Vec<Type>>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::TCon(t) => t.fmt(f),
            Type::TVar(id) => write!(f, "'t{}", id),
        }
    }
}

impl fmt::Display for Tycon {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(args) = &self.args {
            if self.id == "->" {
                let mut iter = args.iter();
                let e = format!("{}", iter.next().unwrap());
                let args_str = format!("{}", iter.next().unwrap());
                let ret = format!("{}", iter.next().unwrap());
                write!(f, "({} (-> {} {}))", e, args_str, ret)
            } else {
                let mut s = "".to_string();
                for it in args {
                    s = format!("{} {}", s, it);
                }

                if self.id == "Arguments" {
                    write!(f, "({})", &s[1..])
                } else {
                    write!(f, "({}{})", self.id, s)
                }
            }
        } else if self.id == "Arguments" {
            write!(f, "()")
        } else {
            write!(f, "{}", self.id)
        }
    }
}

fn ty_bool() -> Type {
    Type::TCon(Tycon {
        id: "Bool".to_string(),
        args: None,
    })
}

fn ty_int() -> Type {
    Type::TCon(Tycon {
        id: "Int".to_string(),
        args: None,
    })
}

fn ty_string() -> Type {
    Type::TCon(Tycon {
        id: "String".to_string(),
        args: None,
    })
}

fn ty_char() -> Type {
    Type::TCon(Tycon {
        id: "Char".to_string(),
        args: None,
    })
}

fn ty_var(n: ID) -> Type {
    Type::TVar(n)
}

fn ty_tuple(types: Vec<Type>) -> Type {
    Type::TCon(Tycon {
        id: "Tuple".to_string(),
        args: Some(types),
    })
}

fn ty_list(ty: Type) -> Type {
    Type::TCon(Tycon {
        id: "List".to_string(),
        args: Some(vec![ty]),
    })
}

fn ty_args(types: Vec<Type>) -> Type {
    if types.is_empty() {
        Type::TCon(Tycon {
            id: "Arguments".to_string(),
            args: None,
        })
    } else {
        Type::TCon(Tycon {
            id: "Arguments".to_string(),
            args: Some(types),
        })
    }
}

fn ty_fun(effect: &Effect, args: Vec<Type>, ret: Type) -> Type {
    let tuple = ty_args(args);
    let ty_effect = match effect {
        Effect::Pure => Type::TCon(Tycon {
            id: "Pure".to_string(),
            args: None,
        }),
        Effect::IO => Type::TCon(Tycon {
            id: "IO".to_string(),
            args: None,
        }),
    };
    Type::TCon(Tycon {
        id: "->".to_string(),
        args: Some(vec![ty_effect, tuple, ret]),
    })
}

fn ty_fun_gen_effect(n: ID, args: Vec<Type>, ret: Type) -> Type {
    let tuple = ty_args(args);
    let ty_effect = ty_var(n);
    Type::TCon(Tycon {
        id: "->".to_string(),
        args: Some(vec![ty_effect, tuple, ret]),
    })
}

pub struct FunTypes {
    fun_types: BTreeMap<String, LinkedList<Type>>,
}

impl FunTypes {
    fn new() -> FunTypes {
        FunTypes {
            fun_types: BTreeMap::new(),
        }
    }

    fn insert(&mut self, key: &str, val: Type) {
        match self.fun_types.get_mut(key) {
            Some(list) => {
                list.push_back(val);
            }
            None => {
                let mut list = LinkedList::new();
                list.push_back(val);
                self.fun_types.insert(key.to_string(), list);
            }
        }
    }

    fn contains(&self, key: &str, val: &Type) -> bool {
        match self.fun_types.get(key) {
            Some(list) => {
                for t in list {
                    if unify(val, t).is_some() {
                        return true;
                    }
                }
                false
            }
            None => false,
        }
    }
}

struct VarType {
    var_stack: LinkedList<BTreeMap<String, LinkedList<Type>>>,
}

impl VarType {
    fn new() -> VarType {
        let mut var_type = VarType {
            var_stack: LinkedList::new(),
        };
        var_type.push();
        var_type
    }

    fn push(&mut self) {
        self.var_stack.push_back(BTreeMap::new());
    }

    fn pop(&mut self) {
        self.var_stack.pop_back();
    }

    fn insert(&mut self, key: String, val: Type) {
        match self.var_stack.back_mut() {
            Some(m) => match m.get_mut(&key) {
                Some(v) => {
                    v.push_back(val);
                }
                None => {
                    let mut v = LinkedList::new();
                    v.push_back(val);
                    m.insert(key, v);
                }
            },
            None => {
                panic!("failed to insert");
            }
        }
    }

    fn get(&self, key: &str) -> Option<&Type> {
        for m in self.var_stack.iter().rev() {
            if let Some(list) = m.get(key) {
                return list.back();
            }
        }

        None
    }
}

#[derive(Debug)]
pub struct TypingErr {
    pub msg: String,
    pub pos: Pos,
}

impl TypingErr {
    fn new(msg: &str, ast: &parser::Expr) -> TypingErr {
        TypingErr {
            msg: msg.to_string(),
            pos: ast.get_pos(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum LangExpr {
    IfExpr(Box<IfNode>),
    LetExpr(Box<LetNode>),
    LitStr(StrNode),
    LitChar(CharNode),
    LitNum(NumNode),
    LitBool(BoolNode),
    IDExpr(IDNode),
    DataExpr(DataNode),
    MatchExpr(Box<MatchNode>),
    ApplyExpr(Apply),
    ListExpr(Exprs),
    TupleExpr(Exprs),
    LambdaExpr(Box<Lambda>),
}

impl LangExpr {
    pub(crate) fn get_pos(&self) -> Pos {
        match self {
            LangExpr::IfExpr(e) => e.pos,
            LangExpr::LetExpr(e) => e.pos,
            LangExpr::LitStr(e) => e.pos,
            LangExpr::LitChar(e) => e.pos,
            LangExpr::LitNum(e) => e.pos,
            LangExpr::LitBool(e) => e.pos,
            LangExpr::IDExpr(e) => e.pos,
            LangExpr::DataExpr(e) => e.pos,
            LangExpr::MatchExpr(e) => e.pos,
            LangExpr::ApplyExpr(e) => e.pos,
            LangExpr::ListExpr(e) => e.pos,
            LangExpr::TupleExpr(e) => e.pos,
            LangExpr::LambdaExpr(e) => e.pos,
        }
    }

    fn apply_sbst(&mut self, sbst: &Sbst) {
        let app = |opty: &Option<Type>| opty.as_ref().map(|t| t.apply_sbst(sbst));

        match self {
            LangExpr::IfExpr(e) => {
                e.cond_expr.apply_sbst(sbst);
                e.then_expr.apply_sbst(sbst);
                e.else_expr.apply_sbst(sbst);
                e.ty = app(&e.ty);
            }
            LangExpr::LetExpr(e) => {
                for dv in e.def_vars.iter_mut() {
                    dv.pattern.apply_sbst(sbst);
                    dv.expr.apply_sbst(sbst);
                    dv.ty = app(&dv.ty);
                }
                e.expr.apply_sbst(sbst);
                e.ty = app(&e.ty);
            }
            LangExpr::LitStr(_)
            | LangExpr::LitChar(_)
            | LangExpr::LitNum(_)
            | LangExpr::LitBool(_) => (),
            LangExpr::IDExpr(e) => {
                e.ty = app(&e.ty);
            }
            LangExpr::DataExpr(e) => {
                for it in e.exprs.iter_mut() {
                    it.apply_sbst(sbst);
                }
                e.ty = app(&e.ty);
            }
            LangExpr::MatchExpr(e) => {
                e.ty = app(&e.ty);
                e.expr.apply_sbst(sbst);
                for cs in e.cases.iter_mut() {
                    cs.pattern.apply_sbst(sbst);
                    cs.expr.apply_sbst(sbst);
                    cs.ty = app(&cs.ty);
                }
            }
            LangExpr::ApplyExpr(e) => {
                for it in e.exprs.iter_mut() {
                    it.apply_sbst(sbst);
                }
                e.ty = app(&e.ty);
            }
            LangExpr::ListExpr(e) => {
                for it in e.exprs.iter_mut() {
                    it.apply_sbst(sbst);
                }
                e.ty = app(&e.ty);
            }
            LangExpr::TupleExpr(e) => {
                for it in e.exprs.iter_mut() {
                    it.apply_sbst(sbst);
                }
                e.ty = app(&e.ty);
            }
            LangExpr::LambdaExpr(e) => {
                for it in e.args.iter_mut() {
                    it.ty = app(&it.ty);
                }
                e.expr.apply_sbst(sbst);
                e.ty = app(&e.ty);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Lambda {
    pub(crate) args: Vec<IDNode>,
    pub(crate) expr: LangExpr,
    pub(crate) pos: Pos,
    pub(crate) vars: Vec<String>,
    pub(crate) ident: u64,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct StrNode {
    pub(crate) str: String,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct CharNode {
    pub(crate) c: char,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct NumNode {
    pub(crate) num: BigInt,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct BoolNode {
    pub(crate) val: bool,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct IDNode {
    pub(crate) id: String,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct Extern {
    pub(crate) id: IDNode,
    pub(crate) fun_type: TypeExpr,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct IfNode {
    pub(crate) cond_expr: LangExpr,
    pub(crate) then_expr: LangExpr,
    pub(crate) else_expr: LangExpr,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct LetNode {
    pub(crate) def_vars: Vec<DefVar>,
    pub(crate) expr: LangExpr,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct DefVar {
    pub(crate) pattern: Pattern,
    pub(crate) expr: LangExpr,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct MatchNode {
    pub(crate) expr: LangExpr,
    pub(crate) cases: Vec<MatchCase>,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct DataNode {
    pub(crate) label: TIDNode,
    pub(crate) exprs: Vec<LangExpr>,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) enum Pattern {
    PatStr(StrNode),
    PatChar(CharNode),
    PatNum(NumNode),
    PatBool(BoolNode),
    PatID(IDNode),
    PatTuple(PatTupleNode),
    PatData(PatDataNode),
    PatNil(PatNilNode),
}

impl Pattern {
    pub(crate) fn get_pos(&self) -> Pos {
        match self {
            Pattern::PatStr(e) => e.pos,
            Pattern::PatChar(e) => e.pos,
            Pattern::PatNum(e) => e.pos,
            Pattern::PatBool(e) => e.pos,
            Pattern::PatID(e) => e.pos,
            Pattern::PatTuple(e) => e.pos,
            Pattern::PatData(e) => e.pos,
            Pattern::PatNil(e) => e.pos,
        }
    }

    fn get_type(&self) -> &Option<Type> {
        match self {
            Pattern::PatStr(e) => &e.ty,
            Pattern::PatChar(e) => &e.ty,
            Pattern::PatNum(e) => &e.ty,
            Pattern::PatBool(e) => &e.ty,
            Pattern::PatID(e) => &e.ty,
            Pattern::PatTuple(e) => &e.ty,
            Pattern::PatData(e) => &e.ty,
            Pattern::PatNil(e) => &e.ty,
        }
    }

    fn apply_sbst(&mut self, sbst: &Sbst) {
        let app = |opty: &Option<Type>| opty.as_ref().map(|t| t.apply_sbst(sbst));

        match self {
            Pattern::PatID(e) => {
                e.ty = app(&e.ty);
            }
            Pattern::PatTuple(e) => {
                for pat in e.pattern.iter_mut() {
                    pat.apply_sbst(sbst);
                }
                e.ty = app(&e.ty);
            }
            Pattern::PatData(e) => {
                for pat in e.pattern.iter_mut() {
                    pat.apply_sbst(sbst);
                }
                e.ty = app(&e.ty);
            }
            Pattern::PatNil(e) => {
                e.ty = app(&e.ty);
            }
            _ => (),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PatTupleNode {
    pub(crate) pattern: Vec<Pattern>,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct PatDataNode {
    pub(crate) label: TIDNode,
    pub(crate) pattern: Vec<Pattern>,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct PatNilNode {
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct MatchCase {
    pub(crate) pattern: Pattern,
    pub(crate) expr: LangExpr,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct Apply {
    pub(crate) exprs: Vec<LangExpr>,
    pub(crate) pos: Pos,
    pub(crate) is_tail: bool,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct Exprs {
    pub(crate) exprs: Vec<LangExpr>,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub(crate) struct TIDNode {
    pub(crate) id: String,
    pub(crate) pos: Pos,
}

#[derive(Clone, Debug)]
pub(crate) struct TEBoolNode;

#[derive(Clone, Debug)]
pub(crate) struct TEIntNode;

#[derive(Clone, Debug)]
pub(crate) struct TEStringNode;

#[derive(Clone, Debug)]
pub(crate) struct TECharNode;

#[derive(Clone, Debug)]
pub(crate) struct DataType {
    pub(crate) name: DataTypeName,
    pub(crate) members: Vec<DataTypeMem>,
    pub(crate) pos: Pos,
}

#[derive(Clone, Debug)]
pub(crate) struct DataTypeName {
    pub(crate) id: TIDNode,
    pub(crate) type_args: Vec<IDNode>,
    pub(crate) pos: Pos,
}

#[derive(Clone, Debug)]
pub(crate) struct DataTypeMem {
    pub(crate) id: TIDNode,
    pub(crate) types: Vec<TypeExpr>,
    pub(crate) pos: Pos,
}

#[derive(Clone, Debug)]
pub(crate) enum TypeExpr {
    Bool(TEBoolNode),
    Int(TEIntNode),
    String(TEStringNode),
    Char(TECharNode),
    List(TEListNode),
    Tuple(TETupleNode),
    Fun(TEFunNode),
    Data(TEDataNode),
    Id(IDNode),
}

#[derive(Clone, Debug)]
pub(crate) struct TEListNode {
    pub(crate) ty: Box<TypeExpr>,
    pub(crate) pos: Pos,
}

#[derive(Clone, Debug)]
pub(crate) struct TETupleNode {
    pub(crate) ty: Vec<TypeExpr>,
    pub(crate) pos: Pos,
}

#[derive(Clone, Debug)]
enum Effect {
    IO,
    Pure,
}

#[derive(Clone, Debug)]
pub(crate) struct TEFunNode {
    effect: Effect,
    pub(crate) args: Vec<TypeExpr>,
    pub(crate) ret: Box<TypeExpr>,
    pub(crate) pos: Pos,
}

#[derive(Clone, Debug)]
pub(crate) struct TEDataNode {
    pub(crate) id: TIDNode,
    pub(crate) type_args: Vec<TypeExpr>,
    pub(crate) pos: Pos,
}

#[derive(Clone, Debug)]
pub(crate) struct Defun {
    exported: bool,
    pub(crate) id: IDNode,
    pub(crate) args: Vec<IDNode>,
    pub(crate) fun_type: TypeExpr,
    effect: Effect,
    pub(crate) expr: LangExpr,
    pub(crate) pos: Pos,
    ty: Option<Type>,
}

trait TApp: Sized {
    fn apply(&self, ty: &BTreeMap<String, TypeExpr>) -> Result<Self, TypingErr>;
}

impl TApp for DataType {
    fn apply(&self, ty: &BTreeMap<String, TypeExpr>) -> Result<DataType, TypingErr> {
        let mut mems = Vec::new();
        for m in self.members.iter() {
            mems.push(m.apply(ty)?);
        }

        Ok(DataType {
            name: self.name.clone(),
            members: mems,
            pos: self.pos,
        })
    }
}

impl TApp for DataTypeMem {
    fn apply(&self, ty: &BTreeMap<String, TypeExpr>) -> Result<DataTypeMem, TypingErr> {
        let mut v = Vec::new();
        for it in self.types.iter() {
            v.push(it.apply(ty)?);
        }

        Ok(DataTypeMem {
            id: self.id.clone(),
            types: v,
            pos: self.pos,
        })
    }
}

impl TApp for TypeExpr {
    fn apply(&self, ty: &BTreeMap<String, TypeExpr>) -> Result<TypeExpr, TypingErr> {
        match self {
            TypeExpr::Data(data) => Ok(TypeExpr::Data(data.apply(ty)?)),
            TypeExpr::List(list) => Ok(TypeExpr::List(list.apply(ty)?)),
            TypeExpr::Tuple(tuple) => Ok(TypeExpr::Tuple(tuple.apply(ty)?)),
            TypeExpr::Fun(fun) => Ok(TypeExpr::Fun(fun.apply(ty)?)),
            TypeExpr::Id(id) => match ty.get(&id.id) {
                Some(t) => Ok(t.clone()),
                _ => Ok(TypeExpr::Id(id.clone())),
            },
            _ => Ok(self.clone()),
        }
    }
}

impl TApp for TEListNode {
    fn apply(&self, ty: &BTreeMap<String, TypeExpr>) -> Result<TEListNode, TypingErr> {
        Ok(TEListNode {
            ty: Box::new(self.ty.apply(ty)?),
            pos: self.pos,
        })
    }
}

impl TApp for TETupleNode {
    fn apply(&self, ty: &BTreeMap<String, TypeExpr>) -> Result<TETupleNode, TypingErr> {
        let mut v = Vec::new();
        for it in self.ty.iter() {
            v.push(it.apply(ty)?);
        }

        Ok(TETupleNode {
            ty: v,
            pos: self.pos,
        })
    }
}

impl TApp for TEFunNode {
    fn apply(&self, ty: &BTreeMap<String, TypeExpr>) -> Result<TEFunNode, TypingErr> {
        let mut v = Vec::new();
        for it in self.args.iter() {
            v.push(it.apply(ty)?);
        }

        Ok(TEFunNode {
            effect: self.effect.clone(),
            args: v,
            ret: Box::new(self.ret.apply(ty)?),
            pos: self.pos,
        })
    }
}

impl TApp for TEDataNode {
    fn apply(&self, ty: &BTreeMap<String, TypeExpr>) -> Result<TEDataNode, TypingErr> {
        let mut v = Vec::new();
        for it in self.type_args.iter() {
            v.push(it.apply(ty)?);
        }

        Ok(TEDataNode {
            id: self.id.clone(),
            type_args: v,
            pos: self.pos,
        })
    }
}

pub type CallbackFn = Box<dyn Fn(&BigInt, &BigInt, &BigInt) -> Option<BigInt> + Send>;
pub type FFIFn = BTreeMap<&'static str, fn(env: &mut Environment<'_>, args: &[RTData]) -> RTData>;

pub struct Context {
    pub(crate) funs: BTreeMap<String, Defun>,
    pub(crate) ext_funs: BTreeMap<String, Extern>,
    pub(crate) ext_ffi: FFIFn,
    pub(crate) lambda: BTreeMap<u64, Lambda>,
    pub(crate) lambda_ident: u64,
    pub(crate) data: BTreeMap<String, DataType>,
    pub(crate) built_in: BTreeSet<String>,
    label2data: BTreeMap<String, String>,
    pub(crate) callback: CallbackFn,
    pub(crate) macros: Macros,
}

impl Context {
    fn new(
        funs: BTreeMap<String, Defun>,
        ext_funs: BTreeMap<String, Extern>,
        ext_ffi: FFIFn,
        data: BTreeMap<String, DataType>,
        macros: Macros,
    ) -> Context {
        let mut built_in = BTreeSet::new();

        built_in.insert("+".to_string());
        built_in.insert("-".to_string());
        built_in.insert("*".to_string());
        built_in.insert("/".to_string());
        built_in.insert(">>".to_string());
        built_in.insert("<<".to_string());
        built_in.insert("band".to_string());
        built_in.insert("bor".to_string());
        built_in.insert("bxor".to_string());
        built_in.insert("pow".to_string());
        built_in.insert("sqrt".to_string());
        built_in.insert("chars".to_string());
        built_in.insert("str".to_string());
        built_in.insert("%".to_string());
        built_in.insert("<".to_string());
        built_in.insert(">".to_string());
        built_in.insert("=".to_string());
        built_in.insert("!=".to_string());
        built_in.insert("<=".to_string());
        built_in.insert(">=".to_string());
        built_in.insert("lt".to_string());
        built_in.insert("gt".to_string());
        built_in.insert("eq".to_string());
        built_in.insert("neq".to_string());
        built_in.insert("leq".to_string());
        built_in.insert("geq".to_string());
        built_in.insert("and".to_string());
        built_in.insert("or".to_string());
        built_in.insert("xor".to_string());
        built_in.insert("not".to_string());
        built_in.insert("call-rust".to_string());

        Context {
            funs,
            ext_funs,
            ext_ffi,
            data,
            built_in,
            label2data: BTreeMap::new(),
            lambda: BTreeMap::new(),
            lambda_ident: 0,
            callback: Box::new(|_, _, _| None),
            macros,
        }
    }

    pub fn set_callback(&mut self, func: CallbackFn) {
        self.callback = func;
    }

    fn typing(&mut self) -> Result<(), TypingErr> {
        self.check_data_def()?;
        self.check_label()?;
        self.check_data_rec()?;
        self.check_defun_type()?;
        self.typing_functions()?;
        self.check_defun_type_after_infer()?;
        self.check_match_exhaustive()?;
        self.find_tail_call();
        self.get_free_var_in_lambda();

        Ok(())
    }

    fn check_match_exhaustive(&self) -> Result<(), TypingErr> {
        for fun in self.funs.values() {
            exhaustive_expr(&fun.expr, self)?;
        }
        Ok(())
    }

    fn find_tail_call(&mut self) {
        for fun in self.funs.values_mut() {
            tail_call(&mut fun.expr);
        }
        for fun in self.lambda.values_mut() {
            tail_call(&mut fun.expr);
        }
    }

    fn check_label(&mut self) -> Result<(), TypingErr> {
        for dt in self.data.values() {
            for mem in &dt.members {
                if self.label2data.contains_key(&mem.id.id) {
                    let msg = format!("{} is multiply defined", mem.id.id);
                    return Err(TypingErr {
                        msg,
                        pos: mem.id.pos,
                    });
                }

                self.label2data
                    .insert(mem.id.id.clone(), dt.name.id.id.clone());
            }
        }

        Ok(())
    }

    fn typing_functions(&mut self) -> Result<(), TypingErr> {
        let mut ext_funs = BTreeMap::new();
        for (_, ext_fun) in self.ext_funs.iter() {
            let mut fun = ext_fun.clone();
            self.typing_extern(&mut fun)?;
            ext_funs.insert(fun.id.id.to_string(), fun);
        }

        self.ext_funs = ext_funs;

        let mut funs = BTreeMap::new();
        for (_, defun) in self.funs.iter() {
            let mut defun = defun.clone();
            self.typing_defun(&mut defun)?;
            funs.insert(defun.id.id.to_string(), defun);
        }

        self.funs = funs;

        Ok(())
    }

    fn typing_extern(&self, fun: &mut Extern) -> Result<(), TypingErr> {
        let mut num_tv = 0;
        let fun_type = self.to_type(&fun.fun_type, &mut num_tv).unwrap(); // defined type
        fun.ty = Some(fun_type);
        Ok(())
    }

    fn typing_defun(&self, defun: &mut Defun) -> Result<(), TypingErr> {
        let mut var_type = VarType::new();
        let mut num_tv = 0;
        let mut args_orig = Vec::new();

        // initialize types of arguments
        for t in &defun.args {
            let tv = ty_var(num_tv);
            var_type.insert(t.id.to_string(), tv.clone());
            args_orig.push(tv);
            num_tv += 1;
        }

        // infer type of the expression
        let sbst = Sbst::new();
        let (ret, sbst) = self.typing_expr(&mut defun.expr, sbst, &mut var_type, &mut num_tv)?;

        let args = args_orig.iter().map(|x| x.apply_sbst(&sbst)).collect();

        let fun_type1 = self.to_type(&defun.fun_type, &mut num_tv).unwrap(); // defined type
        let fun_type2 = ty_fun(&defun.effect, args, ret); // inferred type

        // check defined function types with inferred type

        let s1 = match unify(&fun_type1, &fun_type2) {
            None => {
                let msg = format!(
                    "function type was mismatched\n    inferred: {}\n     defined: {}",
                    fun_type2, fun_type1
                );
                return Err(TypingErr {
                    msg,
                    pos: defun.pos,
                });
            }
            Some(s) => s,
        };

        let sbst = compose(&s1, &sbst);

        // update function type
        defun.ty = Some(fun_type1.apply_sbst(&sbst));

        // update types in the expression
        defun.expr.apply_sbst(&sbst);

        // update types of arguments
        for (arg, ty) in defun.args.iter_mut().zip(args_orig.iter()) {
            arg.ty = Some(ty.apply_sbst(&sbst));
        }

        Ok(())
    }

    fn typing_expr(
        &self,
        expr: &mut LangExpr,
        sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        match expr {
            LangExpr::LitStr(_) => Ok((ty_string(), sbst)),
            LangExpr::LitChar(_) => Ok((ty_char(), sbst)),
            LangExpr::LitBool(_) => Ok((ty_bool(), sbst)),
            LangExpr::LitNum(_) => Ok((ty_int(), sbst)),
            LangExpr::IfExpr(e) => self.typing_if(e, sbst, var_type, num_tv),
            LangExpr::IDExpr(e) => self.typing_var(e, sbst, var_type, num_tv),
            LangExpr::LetExpr(e) => self.typing_let(e, sbst, var_type, num_tv),
            LangExpr::MatchExpr(e) => self.typing_match(e, sbst, var_type, num_tv),
            LangExpr::TupleExpr(e) => self.typing_tuple(e, sbst, var_type, num_tv),
            LangExpr::ListExpr(e) => self.typing_list(e, sbst, var_type, num_tv),
            LangExpr::ApplyExpr(e) => self.typing_app(e, sbst, var_type, num_tv),
            LangExpr::DataExpr(e) => self.typing_data(e, sbst, var_type, num_tv),
            LangExpr::LambdaExpr(e) => self.typing_lambda(e, sbst, var_type, num_tv),
        }
    }

    fn typing_lambda(
        &self,
        expr: &mut Lambda,
        sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        var_type.push();

        // generate new type variables for arguments
        for arg in &mut expr.args {
            let ty = ty_var(*num_tv);
            *num_tv += 1;
            arg.ty = Some(ty.clone());

            if arg.id != "_" {
                var_type.insert(arg.id.to_string(), ty.clone());
            }
        }

        // infer the type of the expression
        let (ret_ty, sbst) = self.typing_expr(&mut expr.expr, sbst, var_type, num_tv)?;

        var_type.pop();

        // generate function type
        let mut v = Vec::new();
        for arg in &mut expr.args {
            match &arg.ty {
                Some(t) => {
                    let t2 = t.apply_sbst(&sbst);
                    v.push(t2.clone());
                    arg.ty = Some(t2);
                }
                None => (),
            }
        }

        let ty = ty_fun(&Effect::Pure, v, ret_ty);

        expr.ty = Some(ty.clone());

        Ok((ty, sbst))
    }

    fn typing_data(
        &self,
        expr: &mut DataNode,
        mut sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        let data_type;
        let label_types;
        // get type of label and types of label's elements
        match self.get_type_of_label(&expr.label.id, num_tv) {
            Ok((t, m)) => {
                data_type = t;
                label_types = m;
            }
            Err(msg) => {
                return Err(TypingErr { msg, pos: expr.pos });
            }
        }

        // check the number of elements
        if label_types.len() != expr.exprs.len() {
            let msg = format!(
                "{} requires exactly {} arguments but actually passed {}",
                expr.label.id,
                label_types.len(),
                expr.exprs.len()
            );
            return Err(TypingErr { msg, pos: expr.pos });
        }

        // check types of the elements and arguments
        for (e, ty) in expr.exprs.iter_mut().zip(label_types.iter()) {
            let r = self.typing_expr(e, sbst, var_type, num_tv)?;
            sbst = r.1;
            let t0 = ty.apply_sbst(&sbst);
            let t1 = r.0.apply_sbst(&sbst);
            let s1 = match unify(&t0, &t1) {
                Some(s) => s,
                None => {
                    let msg = format!("mismatched type\n  expected: {}\n    actual: {}", t0, t1);
                    return Err(TypingErr {
                        msg,
                        pos: e.get_pos(),
                    });
                }
            };
            sbst = compose(&s1, &sbst);
        }

        let ty = data_type.apply_sbst(&sbst);
        expr.ty = Some(ty.clone());

        Ok((ty, sbst))
    }

    fn typing_app(
        &self,
        expr: &mut Apply,
        mut sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        let mut iter = expr.exprs.iter_mut();

        // get function
        let e1 = match iter.next() {
            Some(e) => e,
            None => {
                return Err(TypingErr {
                    msg: "require function".to_string(),
                    pos: expr.pos,
                });
            }
        };

        // get function type
        let r = self.typing_expr(e1, sbst, var_type, num_tv)?;
        sbst = r.1;
        let t1 = r.0; // function type

        // get arguments
        let mut v = Vec::new();
        for e in iter {
            let r = self.typing_expr(e, sbst, var_type, num_tv)?;
            sbst = r.1;
            v.push(r.0);
        }

        // get return type
        let ret = ty_var(*num_tv);
        *num_tv += 1;

        // get inferred function type
        let fun_ty = ty_fun_gen_effect(*num_tv, v, ret.clone());
        *num_tv += 1;

        match unify(&t1, &fun_ty) {
            Some(s1) => {
                sbst = compose(&s1, &sbst);
            }
            None => {
                let msg = format!(
                    "mismatched type\n  expected: {}\n    actual: {}",
                    fun_ty, t1
                );
                return Err(TypingErr { msg, pos: expr.pos });
            }
        }

        let t = ret.apply_sbst(&sbst);
        expr.ty = Some(t.clone());

        Ok((t, sbst))
    }

    fn typing_tuple(
        &self,
        expr: &mut Exprs,
        mut sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        let mut v = Vec::new();
        for e in expr.exprs.iter_mut() {
            let (t, s) = self.typing_expr(e, sbst, var_type, num_tv)?;
            sbst = s;
            v.push(t);
        }

        let ty = ty_tuple(v);
        expr.ty = Some(ty.clone());

        Ok((ty, sbst))
    }

    fn typing_list(
        &self,
        expr: &mut Exprs,
        mut sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        let mut ty = None; // type of the first element

        for e in expr.exprs.iter_mut() {
            let (t, s) = self.typing_expr(e, sbst, var_type, num_tv)?;
            sbst = s;
            match &ty {
                None => {
                    ty = Some(t);
                }
                Some(t0) => {
                    let t0 = t0.apply_sbst(&sbst);
                    // check current element's type is same as the first element's type
                    match unify(&t0, &t) {
                        Some(s1) => {
                            sbst = compose(&s1, &sbst);
                        }
                        None => {
                            let msg =
                                format!("mismatched type\n  expected: {}\n    actual: {}", t0, t);
                            return Err(TypingErr {
                                msg,
                                pos: e.get_pos(),
                            });
                        }
                    }
                }
            }
        }

        match ty {
            Some(t0) => {
                let tyls = ty_list(t0.apply_sbst(&sbst));
                expr.ty = Some(tyls.clone());
                Ok((tyls, sbst))
            }
            None => {
                // Nil
                let t = ty_var(*num_tv);
                let tyls = ty_list(t);
                *num_tv += 1;
                expr.ty = Some(tyls.clone());
                Ok((tyls, sbst))
            }
        }
    }

    fn typing_match(
        &self,
        expr: &mut MatchNode,
        mut sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        // for (match e_0 (c_1 e_1) (c_2 e_2) ... (c_n e_n))

        // get e_0's type
        let r = self.typing_expr(&mut expr.expr, sbst, var_type, num_tv)?;
        let mut type_head = r.0;
        sbst = r.1;

        let mut e_ty = None;
        for cs in expr.cases.iter_mut() {
            var_type.push();

            // get c_n's type
            let (pat_ty, s) = self.typing_pat(&mut cs.pattern, sbst, var_type, num_tv)?;
            sbst = s;

            // check types of e_0 and c_n are same
            type_head = type_head.apply_sbst(&sbst);
            let s1 = match unify(&type_head, &pat_ty) {
                Some(s) => s,
                None => {
                    let msg = format!(
                        "mismatched type\n  expected: {}\n    actual: {}",
                        type_head, pat_ty
                    );
                    return Err(TypingErr {
                        msg,
                        pos: cs.pattern.get_pos(),
                    });
                }
            };

            sbst = compose(&s1, &sbst);

            // get e_n's type
            let (ty, s) = self.typing_expr(&mut cs.expr, sbst, var_type, num_tv)?;
            sbst = s;

            // check types of e_{n-1} and e_n are same
            if let Some(t_prev) = e_ty {
                let s1 = match unify(&t_prev, &ty) {
                    Some(s) => s,
                    None => {
                        let msg = format!(
                            "mismatched type\n  expected: {}\n    actual: {}",
                            t_prev, ty
                        );
                        return Err(TypingErr { msg, pos: cs.pos });
                    }
                };

                sbst = compose(&sbst, &s1);
            }

            let ty = ty.apply_sbst(&sbst);
            cs.ty = Some(ty.clone());
            e_ty = Some(ty);

            var_type.pop();
        }

        expr.ty = e_ty.clone();

        Ok((e_ty.unwrap(), sbst))
    }

    fn typing_var(
        &self,
        expr: &mut IDNode,
        sbst: Sbst,
        var_type: &VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        let ty = match var_type.get(&expr.id) {
            Some(t) => t.apply_sbst(&sbst),
            None => {
                // look up external function
                if let Some(fun) = self.ext_funs.get(&expr.id) {
                    self.to_type(&fun.fun_type, num_tv).unwrap()
                } else {
                    // look up function
                    match self.funs.get(&expr.id) {
                        Some(defun) => self.to_type(&defun.fun_type, num_tv).unwrap(),
                        None => {
                            match expr.id.as_ref() {
                                // built-in functions
                                "+" | "-" | "*" | "/" | "%" | "band" | "bor" | "bxor" => {
                                    ty_fun(&Effect::Pure, vec![ty_int(), ty_int()], ty_int())
                                }
                                "=" | "<" | ">" | "<=" | ">=" | "!=" => {
                                    let tv = ty_var(*num_tv);
                                    *num_tv += 1;
                                    ty_fun(&Effect::Pure, vec![tv.clone(), tv], ty_bool())
                                }
                                "eq" | "lt" | "gt" | "leq" | "geq" | "neq" => {
                                    let tv1 = ty_var(*num_tv);
                                    *num_tv += 1;
                                    let tv2 = ty_var(*num_tv);
                                    *num_tv += 1;
                                    ty_fun(&Effect::Pure, vec![tv1, tv2], ty_bool())
                                }
                                "and" | "or" | "xor" => {
                                    ty_fun(&Effect::Pure, vec![ty_bool(), ty_bool()], ty_bool())
                                }
                                "not" => ty_fun(&Effect::Pure, vec![ty_bool()], ty_bool()),
                                "sqrt" => ty_fun(
                                    &Effect::Pure,
                                    vec![ty_int()],
                                    Type::TCon(Tycon {
                                        id: "Option".to_string(),
                                        args: Some(vec![ty_int()]),
                                    }),
                                ),
                                "pow" | "<<" | ">>" => {
                                    // (Pure (-> (Int Int) (Option Int)))
                                    ty_fun(
                                        &Effect::Pure,
                                        vec![ty_int(), ty_int()],
                                        Type::TCon(Tycon {
                                            id: "Option".to_string(),
                                            args: Some(vec![ty_int()]),
                                        }),
                                    )
                                }
                                "chars" => {
                                    // (Pure (-> (String) (List Char)))
                                    ty_fun(
                                        &Effect::Pure,
                                        vec![ty_string()],
                                        Type::TCon(Tycon {
                                            id: "List".to_string(),
                                            args: Some(vec![ty_char()]),
                                        }),
                                    )
                                }
                                "str" => {
                                    // (Pure (-> ((List Char)) String))
                                    ty_fun(
                                        &Effect::Pure,
                                        vec![Type::TCon(Tycon {
                                            id: "List".to_string(),
                                            args: Some(vec![ty_char()]),
                                        })],
                                        ty_string(),
                                    )
                                }
                                "call-rust" => {
                                    // (IO (-> (Int Int Int) (Option Int)))
                                    ty_fun(
                                        &Effect::IO,
                                        vec![ty_int(), ty_int(), ty_int()],
                                        Type::TCon(Tycon {
                                            id: "Option".to_string(),
                                            args: Some(vec![ty_int()]),
                                        }),
                                    )
                                }
                                _ => {
                                    let msg = format!("{} is not defined 5", expr.id);
                                    return Err(TypingErr { msg, pos: expr.pos });
                                }
                            }
                        }
                    }
                }
            }
        };

        expr.ty = Some(ty.clone());

        Ok((ty, sbst))
    }

    fn typing_if(
        &self,
        expr: &mut IfNode,
        sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        // condition
        let (ty_cond, sbst) = self.typing_expr(&mut expr.cond_expr, sbst, var_type, num_tv)?;

        // check the type of condition is Bool
        let s1 = match unify(&ty_bool(), &ty_cond) {
            Some(s) => s,
            None => {
                let msg = format!(
                    "condition of if expression must be Bool, but found {}",
                    ty_cond
                );
                return Err(TypingErr {
                    msg,
                    pos: expr.cond_expr.get_pos(),
                });
            }
        };

        let sbst = compose(&s1, &sbst);

        // then and else expressions
        let (ty_then, sbst) = self.typing_expr(&mut expr.then_expr, sbst, var_type, num_tv)?;
        let (ty_else, sbst) = self.typing_expr(&mut expr.else_expr, sbst, var_type, num_tv)?;

        // check types of expressions are same
        let s1 = match unify(&ty_then, &ty_else) {
            Some(s) => s,
            None => {
                let msg = format!(
                    "when (if c e1 e2), the types of e1 and e2 must be same\n  e1: {}\n  e2: {}",
                    ty_then, ty_else
                );
                return Err(TypingErr {
                    msg,
                    pos: expr.else_expr.get_pos(),
                });
            }
        };

        let sbst = compose(&s1, &sbst);
        let ty = ty_then.apply_sbst(&sbst);

        expr.ty = Some(ty.clone());

        Ok((ty, sbst))
    }

    fn typing_let(
        &self,
        expr: &mut LetNode,
        mut sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        var_type.push();

        for dv in expr.def_vars.iter_mut() {
            let (t1, s) = self.typing_expr(&mut dv.expr, sbst, var_type, num_tv)?;
            let (t2, s) = self.typing_pat(&mut dv.pattern, s, var_type, num_tv)?;
            sbst = s;

            let s1 = match unify(&t1, &t2) {
                Some(s) => s,
                None => {
                    let msg = format!("mismatched type\n   left: {}\n  right: {}", t2, t1);
                    return Err(TypingErr { msg, pos: dv.pos });
                }
            };
            sbst = compose(&s1, &sbst);
            dv.ty = Some(t1.apply_sbst(&sbst));
        }

        let r = self.typing_expr(&mut expr.expr, sbst, var_type, num_tv)?;

        var_type.pop();
        expr.ty = Some(r.0.clone());

        Ok(r)
    }

    fn typing_pat(
        &self,
        expr: &mut Pattern,
        sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        match expr {
            Pattern::PatStr(_) => Ok((ty_string(), sbst)),
            Pattern::PatChar(_) => Ok((ty_char(), sbst)),
            Pattern::PatBool(_) => Ok((ty_bool(), sbst)),
            Pattern::PatNum(_) => Ok((ty_int(), sbst)),
            Pattern::PatID(e) => Ok(self.typing_pat_id(e, sbst, var_type, num_tv)),
            Pattern::PatData(e) => self.typing_pat_data(e, sbst, var_type, num_tv),
            Pattern::PatTuple(e) => self.typing_pat_tuple(e, sbst, var_type, num_tv),
            Pattern::PatNil(e) => Ok(self.typing_pat_nil(e, sbst, num_tv)),
        }
    }

    fn typing_pat_tuple(
        &self,
        expr: &mut PatTupleNode,
        mut sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        let mut v = Vec::new();
        for pat in expr.pattern.iter_mut() {
            let (t, s) = self.typing_pat(pat, sbst, var_type, num_tv)?;
            sbst = s;
            v.push(t);
        }

        let ty = ty_tuple(v);
        expr.ty = Some(ty.clone());

        Ok((ty, sbst))
    }

    fn typing_pat_id(
        &self,
        expr: &mut IDNode,
        sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> (Type, Sbst) {
        // generate new type variable (internal representation)
        let ty = ty_var(*num_tv);
        *num_tv += 1;
        expr.ty = Some(ty.clone());

        if expr.id != "_" {
            var_type.insert(expr.id.to_string(), ty.clone());
        }

        (ty, sbst)
    }

    fn typing_pat_data(
        &self,
        expr: &mut PatDataNode,
        mut sbst: Sbst,
        var_type: &mut VarType,
        num_tv: &mut ID,
    ) -> Result<(Type, Sbst), TypingErr> {
        // get the type of label and the types of label's elements
        let data_type; // type of label
        let label_types; // types of label's elements
        match self.get_type_of_label(&expr.label.id, num_tv) {
            Ok((t, m)) => {
                data_type = t;
                label_types = m;
            }
            Err(msg) => {
                return Err(TypingErr {
                    msg,
                    pos: expr.label.pos,
                });
            }
        }

        // check the number of arguments
        if label_types.len() != expr.pattern.len() {
            let msg = format!(
                "{} requires exactly {} arguments but actually passed {}",
                expr.label.id,
                label_types.len(),
                expr.pattern.len()
            );
            return Err(TypingErr {
                msg,
                pos: expr.label.pos,
            });
        }

        // check type of each element
        for (pat, lt) in expr.pattern.iter_mut().zip(label_types.iter()) {
            let r = self.typing_pat(pat, sbst, var_type, num_tv)?;
            sbst = r.1;
            let lt = lt.apply_sbst(&sbst);
            let s1 = match unify(&lt, &r.0) {
                Some(s) => s,
                None => {
                    let msg = format!("mismatched type\n  expected: {}\n    actual: {}", lt, r.0);
                    return Err(TypingErr {
                        msg,
                        pos: pat.get_pos(),
                    });
                }
            };
            sbst = compose(&s1, &sbst);
        }

        let ty = data_type.apply_sbst(&sbst);
        expr.ty = Some(ty.clone());

        Ok((ty, sbst))
    }

    fn typing_pat_nil(&self, expr: &mut PatNilNode, sbst: Sbst, num_tv: &mut ID) -> (Type, Sbst) {
        let tv = ty_var(*num_tv);
        *num_tv += 1;
        let ty = ty_list(tv);
        expr.ty = Some(ty.clone());
        (ty, sbst)
    }

    fn to_type(&self, expr: &TypeExpr, num_tv: &mut ID) -> Result<Type, String> {
        let mut tv2type = BTreeMap::new();
        get_tv2type_from_type_expr(expr, num_tv, &mut tv2type);
        apply_tv2type_to_type_expr(expr, &tv2type)
    }

    /// If
    /// ```lisp
    /// (data (Tree t)
    ///   (Node (Tree t) (Tree t))
    ///   Leaf)
    /// ```
    /// then get_type_of_label("Node", 2)
    /// returns Ok((Tree (TVar 2)), vec!((Tree (TVar 2)), ((Tree (TVar 2))))
    fn get_type_of_label(&self, label: &str, num_tv: &mut ID) -> Result<(Type, Vec<Type>), String> {
        // find the name of data of the label
        let data_name;
        match self.label2data.get(label) {
            Some(n) => {
                data_name = n;
            }
            None => {
                match label {
                    // built-in data type
                    // (data (List a)
                    //     (Cons a (List a))
                    //     Nil)
                    "Cons" => {
                        let tv = ty_var(*num_tv);
                        let ty = ty_list(tv.clone());
                        *num_tv += 1;
                        return Ok((ty.clone(), vec![tv, ty]));
                    }
                    "Nil" => {
                        let tv = ty_var(*num_tv);
                        let ty = ty_list(tv);
                        *num_tv += 1;
                        return Ok((ty, vec![]));
                    }
                    _ => {
                        let msg = format!("{} is not defined", label);
                        return Err(msg);
                    }
                }
            }
        }

        // find corresponding data
        let data_node = match self.data.get(data_name) {
            Some(n) => n,
            None => {
                let msg = format!("could not find data of label {}", label);
                return Err(msg);
            }
        };

        // get the type of the data
        let mut types = Vec::new();
        for i in 0..data_node.name.type_args.len() {
            types.push(ty_var(i as ID + *num_tv));
        }
        *num_tv += data_node.name.type_args.len() as u64;

        // generate a map from type variable to type
        let mut tv2type = BTreeMap::new();
        for (k, v) in data_node.name.type_args.iter().zip(types.iter()) {
            tv2type.insert(k.id.clone(), v.clone());
        }

        // find corresponding member
        let mut mem = None;
        for m in &data_node.members {
            if m.id.id == *label {
                mem = Some(m);
                break;
            }
        }

        // return type of label and label's type
        match mem {
            Some(mem) => {
                let mut label_types = Vec::new();
                for t in &mem.types {
                    label_types.push(apply_tv2type_to_type_expr(t, &tv2type)?);
                }

                // the type of the data
                let data_type = Type::TCon(Tycon {
                    id: data_name.to_string(),
                    args: if types.is_empty() { None } else { Some(types) },
                });

                Ok((data_type, label_types))
            }
            None => {
                let msg = format!("could not find label {}", label);
                Err(msg)
            }
        }
    }

    fn check_data_def(&self) -> Result<(), TypingErr> {
        for (_, d) in self.data.iter() {
            self.check_data_def_data(d)?;
        }

        Ok(())
    }

    fn check_data_def_data(&self, data: &DataType) -> Result<(), TypingErr> {
        let mut args = BTreeSet::new();
        for arg in data.name.type_args.iter() {
            if args.contains(&arg.id) {
                let msg = format!("{} is multiply used", arg.id);
                return Err(TypingErr { msg, pos: arg.pos });
            }

            args.insert(arg.id.clone());
        }

        for mem in data.members.iter() {
            self.check_data_def_mem(mem, &args)?;
        }

        Ok(())
    }

    fn check_data_def_mem(
        &self,
        mem: &DataTypeMem,
        args: &BTreeSet<String>,
    ) -> Result<(), TypingErr> {
        for it in mem.types.iter() {
            self.check_def_type(it, Some(args))?
        }

        Ok(())
    }

    fn check_def_type(
        &self,
        ty: &TypeExpr,
        args: Option<&BTreeSet<String>>,
    ) -> Result<(), TypingErr> {
        match ty {
            TypeExpr::Id(id) => {
                if let Some(m) = args {
                    if !m.contains(&id.id) {
                        let msg = format!("{} is undefined", id.id);
                        return Err(TypingErr { msg, pos: id.pos });
                    }
                }
            }
            TypeExpr::List(list) => {
                self.check_def_type(&list.ty, args)?;
            }
            TypeExpr::Tuple(tuple) => {
                for it in tuple.ty.iter() {
                    self.check_def_type(it, args)?;
                }
            }
            TypeExpr::Data(data) => {
                match self.data.get(&data.id.id) {
                    Some(dt) => {
                        if dt.name.type_args.len() != data.type_args.len() {
                            let msg = format!(
                                "{} takes {} type arguments but actually passed {}",
                                data.id.id,
                                dt.name.type_args.len(),
                                data.type_args.len()
                            );
                            return Err(TypingErr {
                                msg,
                                pos: data.id.pos,
                            });
                        }
                    }
                    None => {
                        let msg = format!("{} is unkown type", data.id.id);
                        return Err(TypingErr {
                            msg,
                            pos: data.id.pos,
                        });
                    }
                }

                for it in data.type_args.iter() {
                    self.check_def_type(it, args)?;
                }
            }
            TypeExpr::Fun(fun) => {
                for it in fun.args.iter() {
                    self.check_def_type(it, args)?
                }

                self.check_def_type(&fun.ret, args)?
            }
            _ => {}
        }

        Ok(())
    }

    /// check data definition is not infinite recursive
    fn check_data_rec(&self) -> Result<(), TypingErr> {
        let mut checked = LinkedList::new();
        for (_, d) in self.data.iter() {
            let mut visited = BTreeSet::new();
            let mut inst = LinkedList::new();
            inst.push_back(d.pos);
            if self.check_data_rec_data(d, &mut visited, &mut checked, &mut inst)? {
                let msg = format!("{}'s definition is infinitely recursive", d.name.id.id);
                return Err(TypingErr {
                    msg,
                    pos: d.name.id.pos,
                });
            }
            checked.push_back(d.clone());
        }

        Ok(())
    }

    /// Ok(true) if the type is inifinite recursive
    /// Ok(false) if the type is not recursive or limited recursive
    ///
    /// infinite recursive data
    /// ```lisp
    /// (data Num (Succ Num))
    /// ```
    ///
    /// limited recursive date
    /// ```lisp
    /// (data (Tree t)
    ///   (Node (Tree t) (Tree t))
    ///   Leaf)
    ///
    /// (data Num
    ///   (Succ Num)
    ///   Zero)
    /// ```
    fn check_data_rec_data(
        &self,
        data: &DataType,
        visited: &mut BTreeSet<String>,
        checked: &mut LinkedList<DataType>,
        inst: &mut LinkedList<Pos>,
    ) -> Result<bool, TypingErr> {
        if visited.contains(&data.name.id.id) {
            return Ok(true);
        }

        let mut ret = true;

        visited.insert(data.name.id.id.clone());
        for mem in data.members.iter() {
            inst.push_back(mem.pos);
            let result = self.check_data_rec_mem(mem, visited, checked, inst)?;
            ret = result && ret;
            inst.pop_back();
        }

        Ok(ret)
    }

    fn check_data_rec_mem(
        &self,
        mem: &DataTypeMem,
        visited: &mut BTreeSet<String>,
        checked: &mut LinkedList<DataType>,
        inst: &mut LinkedList<Pos>,
    ) -> Result<bool, TypingErr> {
        let mut ret = false;

        for ty in mem.types.iter() {
            if self.check_data_rec_ty(ty, visited, checked, inst)? {
                ret = true;
            }
        }

        Ok(ret)
    }

    fn check_data_rec_ty(
        &self,
        ty: &TypeExpr,
        visited: &mut BTreeSet<String>,
        checked: &mut LinkedList<DataType>,
        inst: &mut LinkedList<Pos>,
    ) -> Result<bool, TypingErr> {
        match ty {
            TypeExpr::List(_list) => Ok(false),
            TypeExpr::Tuple(tuple) => {
                let mut ret = false;

                inst.push_back(tuple.pos);
                for it in tuple.ty.iter() {
                    if self.check_data_rec_ty(it, visited, checked, inst)? {
                        ret = true;
                    }
                }
                inst.pop_back();

                Ok(ret)
            }
            TypeExpr::Data(data) => {
                let dt = self.type_data_node2data_type(data)?;
                inst.push_back(data.pos);
                let ret = self.check_data_rec_data(&dt, visited, checked, inst);
                inst.pop_back();
                ret
            }
            TypeExpr::Fun(_fun) => Ok(false),
            _ => Ok(false),
        }
    }

    fn type_data_node2data_type(&self, data: &TEDataNode) -> Result<DataType, TypingErr> {
        let dt = match self.data.get(&data.id.id) {
            Some(t) => t,
            None => {
                return Err(TypingErr {
                    msg: "no such type".to_string(),
                    pos: data.id.pos,
                });
            }
        };

        if data.type_args.len() != dt.name.type_args.len() {
            let msg = format!(
                "{} takes {} type arguments but actually passed {}",
                data.id.id,
                dt.name.type_args.len(),
                data.type_args.len()
            );
            return Err(TypingErr { msg, pos: data.pos });
        }

        let mut map = BTreeMap::new();
        for (k, v) in dt.name.type_args.iter().zip(data.type_args.iter()) {
            map.insert(k.id.clone(), v.clone());
        }

        dt.apply(&map)
    }

    fn check_defun_type(&self) -> Result<(), TypingErr> {
        for (_, fun) in self.funs.iter() {
            self.check_def_type(&fun.fun_type, None)?;
        }

        Ok(())
    }

    fn check_defun_type_after_infer(&mut self) -> Result<(), TypingErr> {
        let mut m = FunTypes::new();
        for (_, fun) in self.funs.iter() {
            self.check_type_infer(fun, &mut m)?;
        }

        Ok(())
    }

    /// check type inference has been correctly done?
    ///
    /// If an inferred type of defun has no type variables,
    /// then the types of the expression must not contain type variables.
    ///
    /// If an effect of a function is Pure,
    /// then the expression must not contain IO function.
    fn check_type_infer(&self, defun: &Defun, fun_types: &mut FunTypes) -> Result<(), TypingErr> {
        // check effect
        check_type_has_io(&defun.ty, &defun.pos, &Sbst::new(), &defun.effect)?;

        // if function type contains type variables, just return Ok
        match &defun.ty {
            Some(t) => {
                if has_tvar(t) {
                    return Ok(());
                }
            }
            None => {
                return Err(TypingErr {
                    msg: "function type has not inferred yet".to_string(),
                    pos: defun.pos,
                });
            }
        }

        // get arguments
        let mut vars = VarType::new();
        vars.push();
        for arg in &defun.args {
            match &arg.ty {
                Some(t) => {
                    vars.insert(arg.id.to_string(), t.clone());
                }
                None => {
                    return Err(TypingErr {
                        msg: "argument type has not inferred yet".to_string(),
                        pos: arg.pos,
                    });
                }
            }
        }

        self.check_expr_type(
            &defun.expr,
            fun_types,
            &mut vars,
            &Sbst::new(),
            &defun.effect,
            true,
        )
    }

    fn check_expr_type(
        &self,
        expr: &LangExpr,
        fun_types: &mut FunTypes,
        vars: &mut VarType,
        sbst: &Sbst,
        effect: &Effect,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        match expr {
            LangExpr::IDExpr(e) => self.check_id_type(e, fun_types, vars, sbst, effect, chk_rec),
            LangExpr::IfExpr(e) => self.check_if_type(e, fun_types, vars, sbst, effect, chk_rec),
            LangExpr::LetExpr(e) => self.check_let_type(e, fun_types, vars, sbst, effect, chk_rec),
            LangExpr::MatchExpr(e) => {
                self.check_match_type(e, fun_types, vars, sbst, effect, chk_rec)
            }
            LangExpr::ApplyExpr(e) => {
                self.check_apply_type(e, fun_types, vars, sbst, effect, chk_rec)
            }
            LangExpr::ListExpr(e) => {
                self.check_exprs_type(e, fun_types, vars, sbst, effect, chk_rec)
            }
            LangExpr::TupleExpr(e) => {
                self.check_exprs_type(e, fun_types, vars, sbst, effect, chk_rec)
            }
            LangExpr::DataExpr(e) => {
                self.check_data_type(e, fun_types, vars, sbst, effect, chk_rec)
            }
            LangExpr::LambdaExpr(e) => self.check_lambda_type(e, fun_types, vars, sbst, chk_rec),
            _ => Ok(()),
        }
    }

    fn check_lambda_type(
        &self,
        expr: &Lambda,
        fun_types: &mut FunTypes,
        vars: &mut VarType,
        sbst: &Sbst,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        check_type_has_no_tvars(&expr.ty, &expr.pos, sbst)?;
        check_type_has_io(&expr.ty, &expr.pos, sbst, &Effect::Pure)?;

        vars.push();
        for arg in &expr.args {
            vars.insert(arg.id.to_string(), arg.ty.as_ref().unwrap().clone());
        }
        self.check_expr_type(&expr.expr, fun_types, vars, sbst, &Effect::Pure, chk_rec)?;
        vars.pop();

        Ok(())
    }

    fn check_data_type(
        &self,
        expr: &DataNode,
        fun_types: &mut FunTypes,
        vars: &mut VarType,
        sbst: &Sbst,
        effect: &Effect,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        check_type_has_no_tvars(&expr.ty, &expr.pos, sbst)?;
        check_type_has_io(&expr.ty, &expr.pos, sbst, effect)?;

        for e in &expr.exprs {
            self.check_expr_type(e, fun_types, vars, sbst, effect, chk_rec)?;
        }
        Ok(())
    }

    fn check_apply_type(
        &self,
        expr: &Apply,
        fun_types: &mut FunTypes,
        vars: &mut VarType,
        sbst: &Sbst,
        effect: &Effect,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        check_type_has_no_tvars(&expr.ty, &expr.pos, sbst)?;
        check_type_has_io(&expr.ty, &expr.pos, sbst, effect)?;

        for e in &expr.exprs {
            self.check_expr_type(e, fun_types, vars, sbst, effect, chk_rec)?;
        }
        Ok(())
    }

    fn check_exprs_type(
        &self,
        expr: &Exprs,
        fun_types: &mut FunTypes,
        vars: &mut VarType,
        sbst: &Sbst,
        effect: &Effect,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        check_type_has_no_tvars(&expr.ty, &expr.pos, sbst)?;
        check_type_has_io(&expr.ty, &expr.pos, sbst, effect)?;

        for e in &expr.exprs {
            self.check_expr_type(e, fun_types, vars, sbst, effect, chk_rec)?;
        }
        Ok(())
    }

    fn check_match_type(
        &self,
        expr: &MatchNode,
        fun_types: &mut FunTypes,
        vars: &mut VarType,
        sbst: &Sbst,
        effect: &Effect,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        check_type_has_no_tvars(&expr.ty, &expr.pos, sbst)?;
        check_type_has_io(&expr.ty, &expr.pos, sbst, effect)?;

        self.check_expr_type(&expr.expr, fun_types, vars, sbst, effect, chk_rec)?;

        for c in &expr.cases {
            vars.push();
            check_pat_type(&c.pattern, vars, sbst, effect)?;
            self.check_expr_type(&c.expr, fun_types, vars, sbst, effect, chk_rec)?;
            vars.pop();
        }

        Ok(())
    }

    fn check_id_type(
        &self,
        expr: &IDNode,
        fun_types: &mut FunTypes,
        vars: &mut VarType,
        sbst: &Sbst,
        effect: &Effect,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        check_type_has_no_tvars(&expr.ty, &expr.pos, sbst)?;
        check_type_has_io(&expr.ty, &expr.pos, sbst, effect)?;
        match vars.get(&expr.id.to_string()) {
            Some(_) => (),
            None => {
                if self.ext_funs.get(&expr.id).is_some() {
                    if !chk_rec {
                        let msg = format!("{} is not defined", expr.id);
                        return Err(TypingErr { msg, pos: expr.pos });
                    }
                } else {
                    match self.funs.get(&expr.id) {
                        Some(defun) => {
                            if !chk_rec && !defun.exported {
                                let msg = format!("{} is not defined 2", expr.id);
                                return Err(TypingErr { msg, pos: expr.pos });
                            }

                            let call_ty = expr.ty.as_ref().unwrap().apply_sbst(sbst);
                            self.check_defun_type_recur(&call_ty, defun, fun_types, true)?;
                        }
                        None => {
                            if self.built_in.contains(&expr.id) {
                                if !chk_rec && expr.id == "call-rust" {
                                    let msg = format!("{} is not defined 3", expr.id);
                                    return Err(TypingErr { msg, pos: expr.pos });
                                }
                            } else {
                                let msg = format!("{} is not defined 4", expr.id);
                                return Err(TypingErr { msg, pos: expr.pos });
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn check_defun_type_recur(
        &self,
        call_ty: &Type,
        defun: &Defun,
        fun_types: &mut FunTypes,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        // check only functions whose type has type variables
        let defun_ty = match &defun.ty {
            Some(t) => {
                if !has_tvar(t) {
                    return Ok(());
                }
                t
            }
            None => {
                return Err(TypingErr {
                    msg: "function type has not inferred yet".to_string(),
                    pos: defun.pos,
                });
            }
        };

        // already checked?
        let id = defun.id.id.to_string();
        if fun_types.contains(&id, call_ty) {
            return Ok(());
        }

        fun_types.insert(&id, call_ty.clone());

        // check type with caller side
        let sbst = match unify(call_ty, defun_ty) {
            Some(s) => s,
            None => {
                let msg = format!(
                    "mismatched type\n  expected: {}\n    actual: {}",
                    call_ty, defun_ty
                );
                return Err(TypingErr {
                    msg,
                    pos: defun.pos,
                });
            }
        };

        // check effect
        check_type_has_io(&defun.ty, &defun.pos, &sbst, &defun.effect)?;

        // get arguments
        let mut vars = VarType::new();
        vars.push();
        for arg in &defun.args {
            match &arg.ty {
                Some(t) => {
                    vars.insert(arg.id.to_string(), t.apply_sbst(&sbst));
                }
                None => {
                    return Err(TypingErr {
                        msg: "argument type has not inferred yet".to_string(),
                        pos: arg.pos,
                    });
                }
            }
        }

        // check function type recursively
        self.check_expr_type(
            &defun.expr,
            fun_types,
            &mut vars,
            &sbst,
            &defun.effect,
            chk_rec,
        )
    }

    fn check_if_type(
        &self,
        expr: &IfNode,
        fun_types: &mut FunTypes,
        vars: &mut VarType,
        sbst: &Sbst,
        effect: &Effect,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        check_type_has_no_tvars(&expr.ty, &expr.pos, sbst)?;
        check_type_has_io(&expr.ty, &expr.pos, sbst, effect)?;
        self.check_expr_type(&expr.cond_expr, fun_types, vars, sbst, effect, chk_rec)?;
        self.check_expr_type(&expr.then_expr, fun_types, vars, sbst, effect, chk_rec)?;
        self.check_expr_type(&expr.else_expr, fun_types, vars, sbst, effect, chk_rec)?;
        Ok(())
    }

    fn check_let_type(
        &self,
        expr: &LetNode,
        fun_types: &mut FunTypes,
        vars: &mut VarType,
        sbst: &Sbst,
        effect: &Effect,
        chk_rec: bool,
    ) -> Result<(), TypingErr> {
        check_type_has_no_tvars(&expr.ty, &expr.pos, sbst)?;
        check_type_has_io(&expr.ty, &expr.pos, sbst, effect)?;

        vars.push();

        for def in &expr.def_vars {
            self.check_expr_type(&def.expr, fun_types, vars, sbst, effect, chk_rec)?;
            check_pat_type(&def.pattern, vars, sbst, effect)?;
        }

        self.check_expr_type(&expr.expr, fun_types, vars, sbst, effect, chk_rec)?;
        vars.pop();

        Ok(())
    }

    /// collect lambda expressions and
    /// free variables in the expressions
    fn get_free_var_in_lambda(&mut self) {
        let mut funs = BTreeSet::new();
        for name in self.funs.keys() {
            funs.insert(name.to_string());
        }

        for fun in self.funs.values_mut() {
            let mut local_vars = VarType::new();
            for arg in &fun.args {
                local_vars.insert(arg.id.to_string(), arg.ty.clone().unwrap());
            }
            get_free_var_expr(
                &mut fun.expr,
                &funs,
                &mut local_vars,
                &mut Vec::new(),
                &mut self.lambda_ident,
                &mut self.lambda,
            );
        }
    }
}

fn get_tv2type_from_type_expr(
    expr: &TypeExpr,
    num_tv: &mut ID,
    tv2type: &mut BTreeMap<String, Type>,
) {
    match expr {
        TypeExpr::List(e) => {
            get_tv2type_from_type_expr(&e.ty, num_tv, tv2type);
        }
        TypeExpr::Tuple(e) => {
            for it in &e.ty {
                get_tv2type_from_type_expr(it, num_tv, tv2type);
            }
        }
        TypeExpr::Fun(e) => {
            for it in &e.args {
                get_tv2type_from_type_expr(it, num_tv, tv2type);
            }
            get_tv2type_from_type_expr(&e.ret, num_tv, tv2type);
        }
        TypeExpr::Data(e) => {
            for it in &e.type_args {
                get_tv2type_from_type_expr(it, num_tv, tv2type);
            }
        }
        TypeExpr::Id(e) => {
            tv2type.insert(e.id.clone(), ty_var(*num_tv));
            *num_tv += 1;
        }
        _ => (),
    }
}

/// If
/// ```lisp
/// (data (Tree t)
///   (Node (Tree t) (Tree t))
///   Leaf)
/// ```
/// and tv2type = {t: Int} then
/// apply_tv2type_to_type_expr((Tree t), tv2type) returns (Tree Int)
fn apply_tv2type_to_type_expr(
    type_expr: &TypeExpr,
    tv2type: &BTreeMap<String, Type>,
) -> Result<Type, String> {
    match type_expr {
        TypeExpr::Char(_) => Ok(ty_char()),
        TypeExpr::String(_) => Ok(ty_string()),
        TypeExpr::Bool(_) => Ok(ty_bool()),
        TypeExpr::Int(_) => Ok(ty_int()),
        TypeExpr::List(list) => {
            let t = apply_tv2type_to_type_expr(&list.ty, tv2type)?;
            Ok(ty_list(t))
        }
        TypeExpr::Tuple(tuple) => {
            let mut v = Vec::new();
            for t in &tuple.ty {
                v.push(apply_tv2type_to_type_expr(t, tv2type)?);
            }
            Ok(ty_tuple(v))
        }
        TypeExpr::Fun(fun) => {
            let mut args = Vec::new();
            for a in &fun.args {
                args.push(apply_tv2type_to_type_expr(a, tv2type)?);
            }
            let r = apply_tv2type_to_type_expr(&fun.ret, tv2type)?;
            Ok(ty_fun(&fun.effect, args, r))
        }
        TypeExpr::Data(data) => {
            let mut v = Vec::new();
            for t in &data.type_args {
                v.push(apply_tv2type_to_type_expr(t, tv2type)?);
            }

            Ok(Type::TCon(Tycon {
                id: data.id.id.to_string(),
                args: if v.is_empty() { None } else { Some(v) },
            }))
        }
        TypeExpr::Id(id) => match tv2type.get(&id.id) {
            Some(t) => Ok(t.clone()),
            None => {
                let msg = format!("type variable {} is undefined", id.id);
                Err(msg)
            }
        },
    }
}

fn check_pat_type(
    expr: &Pattern,
    vars: &mut VarType,
    sbst: &Sbst,
    effect: &Effect,
) -> Result<(), TypingErr> {
    match expr {
        Pattern::PatID(e) => {
            check_type_has_no_tvars(&e.ty, &e.pos, sbst)?;
            check_type_has_io(&e.ty, &e.pos, sbst, effect)?;
            vars.insert(e.id.to_string(), e.ty.as_ref().unwrap().clone());
            Ok(())
        }
        Pattern::PatTuple(e) => {
            check_type_has_no_tvars(&e.ty, &e.pos, sbst)?;
            check_type_has_io(&e.ty, &e.pos, sbst, effect)?;
            for it in &e.pattern {
                check_pat_type(it, vars, sbst, effect)?;
            }
            Ok(())
        }
        Pattern::PatData(e) => {
            check_type_has_no_tvars(&e.ty, &e.pos, sbst)?;
            check_type_has_io(&e.ty, &e.pos, sbst, effect)?;
            for it in &e.pattern {
                check_pat_type(it, vars, sbst, effect)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn get_free_var_expr(
    expr: &mut LangExpr,
    funs: &BTreeSet<String>,
    local_vars: &mut VarType,
    ext_vars: &mut Vec<String>,
    ident: &mut u64,
    lambda: &mut BTreeMap<u64, Lambda>,
) {
    match expr {
        LangExpr::IfExpr(e) => get_free_var_if(e, funs, local_vars, ext_vars, ident, lambda),
        LangExpr::LetExpr(e) => get_free_var_let(e, funs, local_vars, ext_vars, ident, lambda),
        LangExpr::IDExpr(e) => get_free_var_id(e, funs, local_vars, ext_vars),
        LangExpr::DataExpr(e) => get_free_var_data(e, funs, local_vars, ext_vars, ident, lambda),
        LangExpr::MatchExpr(e) => get_free_var_match(e, funs, local_vars, ext_vars, ident, lambda),
        LangExpr::ApplyExpr(e) => get_free_var_apply(e, funs, local_vars, ext_vars, ident, lambda),
        LangExpr::ListExpr(e) => get_free_var_exprs(e, funs, local_vars, ext_vars, ident, lambda),
        LangExpr::TupleExpr(e) => get_free_var_exprs(e, funs, local_vars, ext_vars, ident, lambda),
        LangExpr::LambdaExpr(e) => {
            get_free_var_lambda(e, funs, local_vars, ext_vars, ident, lambda)
        }
        _ => {}
    }
}

fn get_free_var_apply(
    exprs: &mut Apply,
    funs: &BTreeSet<String>,
    local_vars: &mut VarType,
    ext_vars: &mut Vec<String>,
    ident: &mut u64,
    lambda: &mut BTreeMap<u64, Lambda>,
) {
    for e in &mut exprs.exprs {
        get_free_var_expr(e, funs, local_vars, ext_vars, ident, lambda);
    }
}

fn get_free_var_exprs(
    exprs: &mut Exprs,
    funs: &BTreeSet<String>,
    local_vars: &mut VarType,
    ext_vars: &mut Vec<String>,
    ident: &mut u64,
    lambda: &mut BTreeMap<u64, Lambda>,
) {
    for e in &mut exprs.exprs {
        get_free_var_expr(e, funs, local_vars, ext_vars, ident, lambda);
    }
}

fn get_free_var_match(
    expr: &mut MatchNode,
    funs: &BTreeSet<String>,
    local_vars: &mut VarType,
    ext_vars: &mut Vec<String>,
    ident: &mut u64,
    lambda: &mut BTreeMap<u64, Lambda>,
) {
    get_free_var_expr(&mut expr.expr, funs, local_vars, ext_vars, ident, lambda);

    for c in &mut expr.cases {
        local_vars.push();
        get_free_var_pattern(&c.pattern, local_vars);
        get_free_var_expr(&mut c.expr, funs, local_vars, ext_vars, ident, lambda);
        local_vars.pop();
    }
}

fn get_free_var_data(
    expr: &mut DataNode,
    funs: &BTreeSet<String>,
    local_vars: &mut VarType,
    ext_vars: &mut Vec<String>,
    ident: &mut u64,
    lambda: &mut BTreeMap<u64, Lambda>,
) {
    for e in &mut expr.exprs {
        get_free_var_expr(e, funs, local_vars, ext_vars, ident, lambda);
    }
}

fn get_free_var_id(
    expr: &mut IDNode,
    funs: &BTreeSet<String>,
    local_vars: &mut VarType,
    ext_vars: &mut Vec<String>,
) {
    let key = expr.id.to_string();
    match local_vars.get(&key) {
        Some(_) => (),
        None => {
            if !funs.contains(&key) {
                ext_vars.push(expr.id.to_string());
            }
        }
    }
}

fn get_free_var_let(
    expr: &mut LetNode,
    funs: &BTreeSet<String>,
    local_vars: &mut VarType,
    ext_vars: &mut Vec<String>,
    ident: &mut u64,
    lambda: &mut BTreeMap<u64, Lambda>,
) {
    local_vars.push();
    for dv in &mut expr.def_vars {
        get_free_var_expr(&mut dv.expr, funs, local_vars, ext_vars, ident, lambda);
        get_free_var_pattern(&dv.pattern, local_vars);
    }

    get_free_var_expr(&mut expr.expr, funs, local_vars, ext_vars, ident, lambda);
    local_vars.pop();
}

fn get_free_var_if(
    expr: &mut IfNode,
    funs: &BTreeSet<String>,
    local_vars: &mut VarType,
    ext_vars: &mut Vec<String>,
    ident: &mut u64,
    lambda: &mut BTreeMap<u64, Lambda>,
) {
    get_free_var_expr(
        &mut expr.cond_expr,
        funs,
        local_vars,
        ext_vars,
        ident,
        lambda,
    );
    get_free_var_expr(
        &mut expr.then_expr,
        funs,
        local_vars,
        ext_vars,
        ident,
        lambda,
    );
    get_free_var_expr(
        &mut expr.else_expr,
        funs,
        local_vars,
        ext_vars,
        ident,
        lambda,
    );
}

fn get_free_var_lambda(
    expr: &mut Lambda,
    funs: &BTreeSet<String>,
    local_vars: &mut VarType,
    ext_vars: &mut Vec<String>,
    ident: &mut u64,
    lambda: &mut BTreeMap<u64, Lambda>,
) {
    {
        let mut local_vars = VarType::new();
        for arg in &expr.args {
            local_vars.insert(arg.id.to_string(), arg.ty.clone().unwrap());
        }

        let mut ext_vars = Vec::new();

        get_free_var_expr(
            &mut expr.expr,
            funs,
            &mut local_vars,
            &mut ext_vars,
            ident,
            lambda,
        );

        expr.vars = ext_vars;
    }

    // if
    // (lambda (x) (lambda (y) z))
    // then
    // (lambda (x) ...) contains a free variable "z"
    for var in &expr.vars {
        match local_vars.get(var) {
            Some(_) => (),
            None => {
                ext_vars.push(var.to_string());
            }
        }
    }

    expr.ident = *ident;
    lambda.insert(*ident, expr.clone());
    *ident += 1;
}

fn get_free_var_pattern(pat: &Pattern, local_vars: &mut VarType) {
    match pat {
        Pattern::PatID(id) => {
            if id.id != "_" {
                local_vars.insert(id.id.to_string(), id.ty.clone().unwrap());
            }
        }
        Pattern::PatTuple(tuple) => {
            for it in &tuple.pattern {
                get_free_var_pattern(it, local_vars);
            }
        }
        Pattern::PatData(data) => {
            for it in &data.pattern {
                get_free_var_pattern(it, local_vars);
            }
        }
        Pattern::PatNum(_)
        | Pattern::PatBool(_)
        | Pattern::PatNil(_)
        | Pattern::PatStr(_)
        | Pattern::PatChar(_) => (),
    }
}

fn check_type_has_no_tvars(ty: &Option<Type>, pos: &Pos, sbst: &Sbst) -> Result<(), TypingErr> {
    match ty {
        Some(t) => {
            if has_tvar(&t.apply_sbst(sbst)) {
                let msg = format!("inferred type still contains type variables\n  type: {}", t);
                return Err(TypingErr { msg, pos: *pos });
            }
        }
        None => {
            return Err(TypingErr {
                msg: "type has not inferred yet".to_string(),
                pos: *pos,
            });
        }
    }
    Ok(())
}

fn check_type_has_io(
    ty: &Option<Type>,
    pos: &Pos,
    sbst: &Sbst,
    effect: &Effect,
) -> Result<(), TypingErr> {
    match ty {
        Some(t) => {
            if let Effect::Pure = effect {
                if has_io(&t.apply_sbst(sbst)) {
                    let msg = format!("Pure function contains an IO function\n type: {}", t);
                    return Err(TypingErr { msg, pos: *pos });
                }
            }
        }
        None => {
            return Err(TypingErr {
                msg: "type has not inferred yet".to_string(),
                pos: *pos,
            });
        }
    }

    Ok(())
}

/// Does type contain IO?
fn has_io(ty: &Type) -> bool {
    match ty {
        Type::TCon(t) => {
            if t.id == "IO" {
                return true;
            }

            if let Some(args) = &t.args {
                for arg in args {
                    if has_io(arg) {
                        return true;
                    }
                }
            }

            false
        }
        Type::TVar(_) => false,
    }
}

/// Does type contain type variables?
fn has_tvar(ty: &Type) -> bool {
    match ty {
        Type::TCon(t) => {
            if let Some(args) = &t.args {
                for arg in args {
                    if has_tvar(arg) {
                        return true;
                    }
                }
            }
            false
        }
        Type::TVar(_) => true,
    }
}

pub(crate) fn typing_expr(
    expr: &parser::Expr,
    ctx: &Context,
) -> Result<(LangExpr, BTreeMap<u64, Lambda>), TypingErr> {
    let mut expr = expr2typed_expr(expr)?;
    let mut num_tv = 0;
    let (_, sbst) = ctx.typing_expr(&mut expr, Sbst::new(), &mut VarType::new(), &mut num_tv)?;

    expr.apply_sbst(&sbst);

    // check call only exported functions
    ctx.check_expr_type(
        &expr,
        &mut FunTypes::new(),
        &mut VarType::new(),
        &Sbst::new(),
        &Effect::IO,
        false,
    )?;

    exhaustive_expr(&expr, ctx)?;

    // capture free variables
    // TODO: should be cached
    let mut funs = BTreeSet::new();
    for name in ctx.funs.keys() {
        funs.insert(name.to_string());
    }

    let mut ident = ctx.lambda_ident;
    let mut lambda = BTreeMap::new();
    get_free_var_expr(
        &mut expr,
        &funs,
        &mut VarType::new(),
        &mut Vec::new(),
        &mut ident,
        &mut lambda,
    );

    for (_, v) in lambda.iter_mut() {
        tail_call(&mut v.expr);
    }

    Ok((expr, lambda))
}

pub fn exprs2context(typing_context: TypingContext) -> Result<Context, TypingErr> {
    let mut funs = BTreeMap::new();
    let mut ext_funs = BTreeMap::new();
    let mut ext_ffi = BTreeMap::new();
    let mut data = BTreeMap::new();
    let msg = "top expression must be data, defun, or export";

    for e in typing_context.exprs.iter() {
        match e {
            parser::Expr::Apply(es, _) => {
                let mut iter = es.iter();

                match iter.next() {
                    Some(parser::Expr::ID(id, _)) => {
                        if id == "defun" || id == "export" {
                            let f = expr2defun(e)?;

                            if ext_funs.contains_key(&f.id.id) {
                                let msg = format!("{} is multiply defined", f.id.id);
                                return Err(TypingErr { msg, pos: f.id.pos });
                            }

                            if let btree_map::Entry::Vacant(entry) = funs.entry(f.id.id.to_string())
                            {
                                entry.insert(f);
                            } else {
                                let msg = format!("{} is multiply defined", f.id.id);
                                return Err(TypingErr { msg, pos: f.id.pos });
                            }
                        } else if id == "extern" {
                            let f = expr2extern(e)?;

                            if funs.contains_key(&f.id.id) {
                                let msg = format!("{} is multiply defined", f.id.id);
                                return Err(TypingErr { msg, pos: f.id.pos });
                            }

                            if let btree_map::Entry::Vacant(entry) =
                                ext_funs.entry(f.id.id.to_string())
                            {
                                entry.insert(f);
                            } else {
                                let msg = format!("{} is multiply defined", f.id.id);
                                return Err(TypingErr { msg, pos: f.id.pos });
                            }
                        } else if id == "data" {
                            let d = expr2data(e)?;
                            if data.contains_key(&d.name.id.id) {
                                let msg = format!("data type {} is multiply defined", d.name.id.id);
                                return Err(TypingErr {
                                    msg,
                                    pos: d.name.pos,
                                });
                            }

                            data.insert(d.name.id.id.clone(), d);
                        } else if id == "macro" {
                            // Do nothing.
                        } else {
                            return Err(TypingErr::new(msg, e));
                        }
                    }
                    _ => {
                        return Err(TypingErr::new(msg, e));
                    }
                }
            }
            _ => {
                return Err(TypingErr::new(msg, e));
            }
        }
    }

    for ffi in typing_context.ext_funs.iter() {
        ext_ffi.insert(ffi.name(), ffi.ffi());
    }

    let mut ctx = Context::new(funs, ext_funs, ext_ffi, data, typing_context.macros);
    ctx.typing()?;

    Ok(ctx)
}

/// $DATA := ( data $DATA_NAME $MEMBER+ )
fn expr2data(expr: &parser::Expr) -> Result<DataType, TypingErr> {
    match expr {
        parser::Expr::Apply(exprs, pos) => {
            let mut iter = exprs.iter();
            iter.next(); // must be "data"

            // $DATA_NAME
            let data_name = match iter.next() {
                Some(e) => expr2data_name(e)?,
                _ => return Err(TypingErr::new("require data name", expr)),
            };

            // $MEMBER+
            let mut mems = Vec::new();
            for mem in iter {
                let data_mem = expr2data_mem(mem)?;
                mems.push(data_mem);
            }

            Ok(DataType {
                name: data_name,
                members: mems,
                pos: *pos,
            })
        }
        _ => Err(TypingErr::new("syntax error on data definition", expr)),
    }
}

/// $DATA_NAME := $TID | ( $TID $ID* )
fn expr2data_name(expr: &parser::Expr) -> Result<DataTypeName, TypingErr> {
    match expr {
        parser::Expr::ID(_, pos) => {
            let tid = expr2type_id(expr)?;
            Ok(DataTypeName {
                id: tid,
                type_args: Vec::new(),
                pos: *pos,
            })
        }
        parser::Expr::Apply(exprs, pos) => {
            let mut args = Vec::new();
            let mut iter = exprs.iter();
            let tid;

            match iter.next() {
                Some(e) => {
                    tid = expr2type_id(e)?;
                }
                _ => {
                    return Err(TypingErr::new(
                        "must type identifier (with type arguments)",
                        expr,
                    ))
                }
            }

            for it in iter {
                let id = expr2id(it)?;
                args.push(id);
            }

            Ok(DataTypeName {
                id: tid,
                type_args: args,
                pos: *pos,
            })
        }
        _ => Err(TypingErr::new(
            "must type identifier (with type arguments)",
            expr,
        )),
    }
}

fn expr2type_id(expr: &parser::Expr) -> Result<TIDNode, TypingErr> {
    match expr {
        parser::Expr::ID(id, pos) => match id.chars().next() {
            Some(c) => {
                if c.is_ascii_uppercase() {
                    Ok(TIDNode {
                        id: id.to_string(),
                        pos: *pos,
                    })
                } else {
                    Err(TypingErr::new("the first character must be captal", expr))
                }
            }
            _ => Err(TypingErr::new("error", expr)),
        },
        _ => Err(TypingErr::new("must be type identifier", expr)),
    }
}

fn expr2id(expr: &parser::Expr) -> Result<IDNode, TypingErr> {
    match expr {
        parser::Expr::ID(id, pos) => match id.chars().next() {
            Some(c) => {
                if c.is_ascii_uppercase() {
                    Err(TypingErr::new(
                        "the first character must not be captal",
                        expr,
                    ))
                } else {
                    Ok(IDNode {
                        id: id.to_string(),
                        pos: *pos,
                        ty: None,
                    })
                }
            }
            _ => Err(TypingErr::new("error", expr)),
        },
        _ => Err(TypingErr::new("must be identifier", expr)),
    }
}

/// $MEMBER := $TID | ( $TID $TYPE* )
fn expr2data_mem(expr: &parser::Expr) -> Result<DataTypeMem, TypingErr> {
    match expr {
        parser::Expr::ID(_, pos) => {
            // $TID
            let tid = expr2type_id(expr)?;
            Ok(DataTypeMem {
                id: tid,
                types: Vec::new(),
                pos: *pos,
            })
        }
        parser::Expr::Apply(exprs, pos) => {
            // ( $TID $TYPE* )
            let mut iter = exprs.iter();
            let tid = match iter.next() {
                Some(e) => expr2type_id(e)?,
                _ => return Err(TypingErr::new("must type identifier", expr)),
            };

            let mut types = Vec::new();
            for it in iter {
                let pt = expr2type(it)?;
                types.push(pt);
            }

            Ok(DataTypeMem {
                id: tid,
                types,
                pos: *pos,
            })
        }
        _ => Err(TypingErr::new("must be type identifier (with types)", expr)),
    }
}

/// Convert `parser::Expr`, which is untyped to `Extern`, which is typed.
///
/// $EXTERN := ( extern $ID $TYPE_ARROW )
fn expr2extern(expr: &parser::Expr) -> Result<Extern, TypingErr> {
    match expr {
        parser::Expr::Apply(exprs, _pos) => {
            let mut iter = exprs.iter();

            // extern
            match iter.next() {
                Some(parser::Expr::ID(id, _)) => {
                    if id != "extern" {
                        return Err(TypingErr::new("require extern", expr));
                    }
                }
                _ => {
                    return Err(TypingErr::new("require extern", expr));
                }
            }

            // $ID
            let id = match iter.next() {
                Some(e) => expr2id(e)?,
                _ => {
                    return Err(TypingErr::new("require function name", expr));
                }
            };

            // $TYPE_ARROW
            let pos_ty;
            let (args, ret) = match iter.next() {
                Some(e) => {
                    pos_ty = e.get_pos();
                    expr2type_arrow(e)?
                }
                _ => {
                    return Err(TypingErr::new("require function type", expr));
                }
            };

            Ok(Extern {
                id,
                fun_type: TypeExpr::Fun(TEFunNode {
                    effect: Effect::IO,
                    args,
                    ret: Box::new(ret),
                    pos: pos_ty,
                }),
                ty: None,
            })
        }
        _ => Err(TypingErr::new("syntax error on extern", expr)),
    }
}

/// Convert `parser::Expr`, which is untyped to `Defun`, which is typed.
///
/// $DEFUN := ( $HEAD_DEFUN $ID ( $ID* ) $TYPE_FUN $EXPR )
fn expr2defun(expr: &parser::Expr) -> Result<Defun, TypingErr> {
    match expr {
        parser::Expr::Apply(exprs, pos) => {
            let mut iter = exprs.iter();

            // $HEAD_DEFUN := export | defun
            let exported = match iter.next() {
                Some(parser::Expr::ID(id, _)) => id == "export",
                _ => {
                    return Err(TypingErr::new("require defun or export", expr));
                }
            };

            // $ID
            let id = match iter.next() {
                Some(e) => expr2id(e)?,
                _ => {
                    return Err(TypingErr::new("require function name", expr));
                }
            };

            // ( $ID* )
            let mut args = Vec::new();
            match iter.next() {
                Some(parser::Expr::Apply(exprs, _)) => {
                    for it in exprs.iter() {
                        let arg = expr2id(it)?;
                        args.push(arg);
                    }
                }
                _ => {
                    return Err(TypingErr::new("require arguments", expr));
                }
            }

            // $TYPE_FUN
            let fun = match iter.next() {
                Some(e) => expr2type_fun(e)?,
                _ => {
                    return Err(TypingErr::new("require function type", expr));
                }
            };

            // $EXPR
            let body = match iter.next() {
                Some(e) => expr2typed_expr(e)?,
                _ => {
                    return Err(TypingErr::new("require expression", expr));
                }
            };

            let effect = match &fun {
                TypeExpr::Fun(e) => e.effect.clone(),
                _ => {
                    panic!("failed to get effect");
                }
            };

            Ok(Defun {
                exported,
                id,
                args,
                fun_type: fun,
                effect,
                expr: body,
                pos: *pos,
                ty: None,
            })
        }
        _ => Err(TypingErr::new("syntax error on function definition", expr)),
    }
}

/// $TYPE_ARROW = ( -> $TYPES $TYPE )
fn expr2type_arrow(expr: &parser::Expr) -> Result<(Vec<TypeExpr>, TypeExpr), TypingErr> {
    let args;
    let ret;
    match expr {
        parser::Expr::Apply(exprs2, _) => {
            let mut iter = exprs2.iter();
            let e2 = iter.next();
            match e2 {
                Some(parser::Expr::ID(arr, _)) => {
                    if arr != "->" {
                        return Err(TypingErr::new("must be \"->\"", e2.unwrap()));
                    }
                }
                _ => {
                    return Err(TypingErr::new("require \"->\"", expr));
                }
            }

            // $TYPES
            match iter.next() {
                Some(t) => {
                    args = expr2types(t)?;
                }
                _ => {
                    return Err(TypingErr::new("require types for arguments", expr));
                }
            }

            // $TYPE
            match iter.next() {
                Some(t) => {
                    ret = expr2type(t)?;
                }
                _ => {
                    return Err(TypingErr::new("require type for return value", expr));
                }
            }
        }
        _ => {
            return Err(TypingErr::new("require function type", expr));
        }
    }

    Ok((args, ret))
}

/// $TYPE_FUN := ( $EFFECT $TYPE_ARROW )
fn expr2type_fun(expr: &parser::Expr) -> Result<TypeExpr, TypingErr> {
    match expr {
        parser::Expr::Apply(exprs, _pos) => {
            let mut iter = exprs.iter();

            // $EFFECT := Pure | IO
            let effect;
            let e = iter.next();
            match e {
                Some(parser::Expr::ID(eff, _)) => {
                    if eff == "IO" {
                        effect = Effect::IO;
                    } else if eff == "Pure" {
                        effect = Effect::Pure;
                    } else {
                        return Err(TypingErr::new(
                            "effect must be \"Pure\" or \"IO\"",
                            e.unwrap(),
                        ));
                    }
                }
                _ => {
                    return Err(TypingErr::new("invalid effect", expr));
                }
            }

            // $TYPE_ARROW
            if let Some(e1) = iter.next() {
                let (args, ret) = expr2type_arrow(e1)?;
                Ok(TypeExpr::Fun(TEFunNode {
                    effect,
                    args,
                    ret: Box::new(ret),
                    pos: e1.get_pos(),
                }))
            } else {
                Err(TypingErr::new("require a function type", expr))
            }
        }
        _ => Err(TypingErr::new("must be function type", expr)),
    }
}

/// $TYPES := ( $TYPE* )
fn expr2types(expr: &parser::Expr) -> Result<Vec<TypeExpr>, TypingErr> {
    match expr {
        parser::Expr::Apply(types, _) => {
            // ( $TYPES* )
            Ok(list_types2vec_types(types)?)
        }
        _ => Err(TypingErr::new("require types of arguments", expr)),
    }
}

/// $TYPE := Int | Bool | String | Char | $TYPE_LIST | $TYPE_TUPLE | $TYPE_FUN | $TYPE_DATA | $ID
fn expr2type(expr: &parser::Expr) -> Result<TypeExpr, TypingErr> {
    match expr {
        parser::Expr::ID(id, pos) => {
            // Int | Bool | String | $TID
            match id.as_ref() {
                "Int" => Ok(TypeExpr::Int(TEIntNode)),
                "Bool" => Ok(TypeExpr::Bool(TEBoolNode)),
                "String" => Ok(TypeExpr::String(TEStringNode)),
                "Char" => Ok(TypeExpr::Char(TECharNode)),
                _ => {
                    let c = id.chars().next().unwrap();
                    if c.is_ascii_uppercase() {
                        let tid = expr2type_id(expr)?;
                        Ok(TypeExpr::Data(TEDataNode {
                            id: tid,
                            type_args: Vec::new(),
                            pos: *pos,
                        }))
                    } else {
                        Ok(TypeExpr::Id(expr2id(expr)?))
                    }
                }
            }
        }
        parser::Expr::List(list, _) => {
            // $TYPE_LIST := '( $TYPE )
            if list.len() != 1 {
                return Err(TypingErr::new(
                    "require exactly one type as a type argument for list type",
                    expr,
                ));
            }

            match list.iter().next() {
                Some(e) => {
                    let ty = Box::new(expr2type(e)?);
                    Ok(TypeExpr::List(TEListNode {
                        ty,
                        pos: e.get_pos(),
                    }))
                }
                _ => Err(TypingErr::new("require type", expr)),
            }
        }
        parser::Expr::Tuple(tuple, pos) => {
            // $TYPE_TUPLE := [ $TYPE* ]
            let mut types = Vec::new();
            for it in tuple {
                types.push(expr2type(it)?);
            }

            Ok(TypeExpr::Tuple(TETupleNode {
                ty: types,
                pos: *pos,
            }))
        }
        parser::Expr::Apply(exprs, pos) => {
            // ( $TID $TYPE* )
            let mut iter = exprs.iter();

            // $TID
            let e = iter.next();
            let tid = match e {
                Some(parser::Expr::ID(id, _)) => {
                    // $TYPE_FUN
                    if id == "Pure" || id == "IO" {
                        let ty = expr2type_fun(expr)?;
                        return Ok(ty);
                    }
                    expr2type_id(e.unwrap())?
                }
                _ => {
                    return Err(TypingErr::new("require type", expr));
                }
            };

            // $TYPE*
            let mut args = Vec::new();
            for it in iter {
                args.push(expr2type(it)?);
            }

            Ok(TypeExpr::Data(TEDataNode {
                id: tid,
                type_args: args,
                pos: *pos,
            }))
        }
        _ => Err(TypingErr::new("must be type", expr)),
    }
}

fn list_types2vec_types(exprs: &LinkedList<parser::Expr>) -> Result<Vec<TypeExpr>, TypingErr> {
    let mut v = Vec::new();
    for e in exprs {
        v.push(expr2type(e)?);
    }

    Ok(v)
}

/// Convert `parser::Expr`, which is untyped to `LangExpr`, which is typed.
///
/// $EXPR := $LITERAL | $ID | $TID | $LET | $IF | $LAMBDA | $MATCH | $LIST | $TUPLE | $GENDATA | $APPLY
fn expr2typed_expr(expr: &parser::Expr) -> Result<LangExpr, TypingErr> {
    match expr {
        parser::Expr::Char(c, pos) => Ok(LangExpr::LitChar(CharNode {
            c: *c,
            pos: *pos,
            ty: Some(ty_string()),
        })),
        parser::Expr::Str(str, pos) => Ok(LangExpr::LitStr(StrNode {
            str: str.clone(),
            pos: *pos,
            ty: Some(ty_string()),
        })),
        parser::Expr::Num(num, pos) => Ok(LangExpr::LitNum(NumNode {
            num: num.clone(),
            pos: *pos,
            ty: Some(ty_int()),
        })),
        parser::Expr::Bool(val, pos) => Ok(LangExpr::LitBool(BoolNode {
            val: *val,
            pos: *pos,
            ty: Some(ty_bool()),
        })),
        parser::Expr::ID(id, pos) => {
            let c = id.chars().next().unwrap();
            if c.is_ascii_uppercase() {
                // $TID
                let tid = expr2type_id(expr)?;
                Ok(LangExpr::DataExpr(DataNode {
                    label: tid,
                    exprs: Vec::new(),
                    pos: *pos,
                    ty: None,
                }))
            } else {
                Ok(LangExpr::IDExpr(IDNode {
                    id: id.to_string(),
                    pos: *pos,
                    ty: None,
                }))
            }
        }
        parser::Expr::List(list, pos) => {
            let mut elms = Vec::new();
            for it in list {
                elms.push(expr2typed_expr(it)?);
            }
            Ok(LangExpr::ListExpr(Exprs {
                exprs: elms,
                pos: *pos,
                ty: None,
            }))
        }
        parser::Expr::Tuple(tuple, pos) => {
            let mut elms = Vec::new();
            for it in tuple {
                elms.push(expr2typed_expr(it)?);
            }
            Ok(LangExpr::TupleExpr(Exprs {
                exprs: elms,
                pos: *pos,
                ty: None,
            }))
        }
        parser::Expr::Apply(exprs, pos) => {
            if exprs.is_empty() {
                return Err(TypingErr::new("empty expression", expr));
            }

            let mut iter = exprs.iter();

            match iter.next() {
                Some(parser::Expr::ID(id, _)) => {
                    let c = id.chars().next().unwrap();
                    if c.is_ascii_uppercase() {
                        // $TID
                        return expr2data_expr(expr);
                    } else if id == "if" {
                        return expr2if(expr);
                    } else if id == "let" {
                        return expr2let(expr);
                    } else if id == "match" {
                        return expr2match(expr);
                    } else if id == "lambda" {
                        return expr2lambda(expr);
                    }
                }
                Some(_) => (),
                None => {
                    return Err(TypingErr::new("require function application", expr));
                }
            }

            let mut elms = Vec::new();
            for it in exprs {
                elms.push(expr2typed_expr(it)?);
            }
            Ok(LangExpr::ApplyExpr(Apply {
                exprs: elms,
                pos: *pos,
                is_tail: false,
                ty: None,
            }))
        }
    }
}

/// $GENDATA := ( $TID $EXPR* )
fn expr2data_expr(expr: &parser::Expr) -> Result<LangExpr, TypingErr> {
    let exprs = match expr {
        parser::Expr::Apply(e, _) => e,
        _ => {
            return Err(TypingErr::new("not data expression", expr));
        }
    };

    let mut iter = exprs.iter();
    let tid = expr2type_id(iter.next().unwrap())?;

    let mut v = Vec::new();
    for e in iter {
        v.push(expr2typed_expr(e)?);
    }

    Ok(LangExpr::DataExpr(DataNode {
        label: tid,
        exprs: v,
        pos: expr.get_pos(),
        ty: None,
    }))
}

/// $IF := ( if $EXPR $EXPR $EXPR )
fn expr2if(expr: &parser::Expr) -> Result<LangExpr, TypingErr> {
    let exprs = match expr {
        parser::Expr::Apply(e, _) => e,
        _ => {
            return Err(TypingErr::new("not if expression", expr));
        }
    };

    let mut iter = exprs.iter();
    iter.next(); // must be "if"

    let f = |next, msg| match next {
        Some(e) => expr2typed_expr(e),
        _ => Err(TypingErr::new(msg, expr)),
    };

    let cond = f(iter.next(), "if requires condition")?;
    let then = f(iter.next(), "if requires then expression")?;
    let else_expr = f(iter.next(), "if requires else expression")?;

    Ok(LangExpr::IfExpr(Box::new(IfNode {
        cond_expr: cond,
        then_expr: then,
        else_expr,
        pos: expr.get_pos(),
        ty: None,
    })))
}

/// $LET := ( let ( $DEFVAR+ ) $EXPR )
fn expr2let(expr: &parser::Expr) -> Result<LangExpr, TypingErr> {
    let exprs = match expr {
        parser::Expr::Apply(e, _) => e,
        _ => {
            return Err(TypingErr::new("not apply expression", expr));
        }
    };

    let mut iter = exprs.iter();
    iter.next(); // must be "let"

    // ( $DEFVAR+ )
    let mut def_vars = Vec::new();
    let e = iter.next();
    match e {
        Some(parser::Expr::Apply(dvs, _)) => {
            if dvs.is_empty() {
                return Err(TypingErr::new("require variable binding", e.unwrap()));
            }

            for it in dvs.iter() {
                def_vars.push(expr2def_vars(it)?);
            }
        }
        _ => {
            return Err(TypingErr::new("require variable binding", expr));
        }
    }

    // $EXPR
    let e = iter.next();
    let body = match e {
        Some(body_expr) => expr2typed_expr(body_expr)?,
        _ => {
            return Err(TypingErr::new("require body", expr));
        }
    };

    Ok(LangExpr::LetExpr(Box::new(LetNode {
        def_vars,
        expr: body,
        pos: expr.get_pos(),
        ty: None,
    })))
}

/// $LETPAT := $ID | [ $LETPAT+ ] | ($TID $LETPAT+ )
fn expr2letpat(expr: &parser::Expr) -> Result<Pattern, TypingErr> {
    match expr {
        parser::Expr::ID(id, pos) => {
            // $ID
            let c = id.chars().next().unwrap();
            if c.is_ascii_uppercase() {
                Err(TypingErr::new("invalid pattern", expr))
            } else {
                Ok(Pattern::PatID(IDNode {
                    id: id.to_string(),
                    pos: *pos,
                    ty: None,
                }))
            }
        }
        parser::Expr::Tuple(tuple, pos) => {
            // [ $LETPAT+ ]
            if tuple.is_empty() {
                return Err(TypingErr::new("require at least one pattern", expr));
            }

            let mut pattern = Vec::new();
            for it in tuple {
                pattern.push(expr2letpat(it)?);
            }

            Ok(Pattern::PatTuple(PatTupleNode {
                pattern,
                pos: *pos,
                ty: None,
            }))
        }
        parser::Expr::Apply(exprs, pos) => {
            // ($TID $LETPAT+ )
            if exprs.len() < 2 {
                return Err(TypingErr::new(
                    "require label and at least one pattern",
                    expr,
                ));
            }

            let mut iter = exprs.iter();
            let tid = expr2type_id(iter.next().unwrap())?;

            let mut v = Vec::new();
            for it in iter {
                v.push(expr2letpat(it)?);
            }

            Ok(Pattern::PatData(PatDataNode {
                label: tid,
                pattern: v,
                pos: *pos,
                ty: None,
            }))
        }
        _ => Err(TypingErr::new("invalid pattern", expr)),
    }
}

/// $DEFVAR := ( $LETPAT $EXPR )
fn expr2def_vars(expr: &parser::Expr) -> Result<DefVar, TypingErr> {
    match expr {
        parser::Expr::Apply(exprs, pos) => {
            if exprs.len() != 2 {
                return Err(TypingErr::new("invalid variable definition", expr));
            }

            let mut iter = exprs.iter();

            let pattern = expr2letpat(iter.next().unwrap())?; // $LETPAT
            let body = expr2typed_expr(iter.next().unwrap())?; // $EXPR

            Ok(DefVar {
                pattern,
                expr: body,
                pos: *pos,
                ty: None,
            })
        }
        _ => Err(TypingErr::new("must be variable definition(s)", expr)),
    }
}

/// $PATTERN := $LITERAL | $ID | $TID | [ $PATTERN+ ] | ( $TID $PATTERN* ) | '()
fn expr2mpat(expr: &parser::Expr) -> Result<Pattern, TypingErr> {
    match expr {
        parser::Expr::ID(id, pos) => {
            let c = id.chars().next().unwrap();
            if c.is_ascii_uppercase() {
                // $TID
                let tid = expr2type_id(expr)?;
                Ok(Pattern::PatData(PatDataNode {
                    label: tid,
                    pattern: Vec::new(),
                    pos: *pos,
                    ty: None,
                }))
            } else {
                // $ID
                let id_node = expr2id(expr)?;
                Ok(Pattern::PatID(id_node))
            }
        }
        parser::Expr::Char(c, pos) => {
            // $LITERAL
            Ok(Pattern::PatChar(CharNode {
                c: *c,
                pos: *pos,
                ty: Some(ty_string()),
            }))
        }
        parser::Expr::Str(str, pos) => {
            // $LITERAL
            Ok(Pattern::PatStr(StrNode {
                str: str.clone(),
                pos: *pos,
                ty: Some(ty_string()),
            }))
        }
        parser::Expr::Bool(val, pos) => {
            // $LITERAL
            Ok(Pattern::PatBool(BoolNode {
                val: *val,
                pos: *pos,
                ty: Some(ty_bool()),
            }))
        }
        parser::Expr::Num(num, pos) => {
            // $LITERAL
            Ok(Pattern::PatNum(NumNode {
                num: num.clone(),
                pos: *pos,
                ty: Some(ty_int()),
            }))
        }
        parser::Expr::Tuple(exprs, pos) => {
            // [ $PATTERN+ ]
            let mut pattern = Vec::new();
            for it in exprs {
                pattern.push(expr2mpat(it)?);
            }

            Ok(Pattern::PatTuple(PatTupleNode {
                pattern,
                pos: *pos,
                ty: None,
            }))
        }
        parser::Expr::Apply(exprs, pos) => {
            // ( $TID $PATTERN* )
            let mut iter = exprs.iter();
            let first = iter.next();

            let tid = match first {
                Some(e) => expr2type_id(e)?,
                _ => {
                    return Err(TypingErr::new("invalid pattern", expr));
                }
            };

            let mut pattern = Vec::new();
            for it in iter {
                pattern.push(expr2mpat(it)?);
            }

            Ok(Pattern::PatData(PatDataNode {
                label: tid,
                pattern,
                pos: *pos,
                ty: None,
            }))
        }
        parser::Expr::List(list, pos) => {
            if !list.is_empty() {
                return Err(TypingErr::new("list pattern is not supported", expr));
            }

            Ok(Pattern::PatNil(PatNilNode {
                pos: *pos,
                ty: None,
            }))
        }
    }
}

/// $CASE := ( $PATTERN $EXPR )
fn expr2case(expr: &parser::Expr) -> Result<MatchCase, TypingErr> {
    match expr {
        parser::Expr::Apply(exprs, pos) => {
            if exprs.len() != 2 {
                return Err(TypingErr::new("case require exactly 2 expressions", expr));
            }

            let mut iter = exprs.iter();
            let pattern = expr2mpat(iter.next().unwrap())?;
            let body = expr2typed_expr(iter.next().unwrap())?;

            Ok(MatchCase {
                pattern,
                expr: body,
                pos: *pos,
                ty: None,
            })
        }
        _ => Err(TypingErr::new("invalid case", expr)),
    }
}

/// $MATCH := ( match $EXPR $CASE+ )
fn expr2match(expr: &parser::Expr) -> Result<LangExpr, TypingErr> {
    match expr {
        parser::Expr::Apply(exprs, pos) => {
            let mut iter = exprs.iter();
            iter.next(); // must be "match"

            let cond = match iter.next() {
                Some(e) => expr2typed_expr(e)?,
                _ => {
                    return Err(TypingErr::new("no condition", expr));
                }
            };

            let mut cases = Vec::new();
            for it in iter {
                cases.push(expr2case(it)?);
            }

            if cases.is_empty() {
                return Err(TypingErr::new("require at least one case", expr));
            }

            let node = MatchNode {
                expr: cond,
                cases,
                pos: *pos,
                ty: None,
            };
            Ok(LangExpr::MatchExpr(Box::new(node)))
        }
        _ => Err(TypingErr::new("invalid match", expr)),
    }
}

/// $LAMBDA := (lambda ($ID*) $EXPR)
fn expr2lambda(expr: &parser::Expr) -> Result<LangExpr, TypingErr> {
    let exprs;
    let pos;
    match expr {
        parser::Expr::Apply(e, p) => {
            exprs = e;
            pos = p;
        }
        _ => {
            return Err(TypingErr::new("not lambda expression", expr));
        }
    }

    let mut iter = exprs.iter();
    iter.next(); // must be "lambda"

    // get arguments
    let args = match iter.next() {
        Some(parser::Expr::Apply(e, _)) => e,
        _ => {
            return Err(TypingErr::new("require arguments", expr));
        }
    };

    let mut v = Vec::new();
    for a in args {
        v.push(expr2id(a)?);
    }

    // get expression
    let body = match iter.next() {
        Some(e) => expr2typed_expr(e)?,
        _ => {
            return Err(TypingErr::new("require arguments", expr));
        }
    };

    Ok(LangExpr::LambdaExpr(Box::new(Lambda {
        args: v,
        expr: body,
        pos: *pos,
        vars: Vec::new(),
        ident: 0,
        ty: None,
    })))
}

impl Type {
    fn has_tvar(&self, id: ID) -> bool {
        match self {
            Type::TVar(n) => id == *n,
            Type::TCon(tc) => tc.has_tvar(id),
        }
    }

    fn apply_sbst(&self, sbst: &Sbst) -> Type {
        match self {
            Type::TVar(n) => match sbst.get(n) {
                Some(t) => t.clone(),
                None => self.clone(),
            },
            Type::TCon(tc) => tc.apply_sbst(sbst),
        }
    }
}

impl Tycon {
    fn has_tvar(&self, id: ID) -> bool {
        if let Some(args) = &self.args {
            for t in args {
                if t.has_tvar(id) {
                    return true;
                }
            }
        }

        false
    }

    fn apply_sbst(&self, sbst: &Sbst) -> Type {
        let mut v = Vec::new();
        if let Some(args) = &self.args {
            for t in args {
                v.push(t.apply_sbst(sbst));
            }
        }

        Type::TCon(Tycon {
            id: self.id.clone(),
            args: if v.is_empty() { None } else { Some(v) },
        })
    }
}

fn unify(lhs: &Type, rhs: &Type) -> Option<Sbst> {
    let mut sbst = Sbst::new();
    match (lhs, rhs) {
        (Type::TVar(id1), Type::TVar(id2)) => {
            if id1 != id2 {
                sbst.insert(*id1, rhs.clone());
            }
            Some(sbst)
        }
        (Type::TVar(id), _) => {
            if rhs.has_tvar(*id) {
                return None;
            }
            sbst.insert(*id, rhs.clone());
            Some(sbst)
        }
        (_, Type::TVar(id)) => {
            if lhs.has_tvar(*id) {
                return None;
            }
            sbst.insert(*id, lhs.clone());
            Some(sbst)
        }
        (Type::TCon(ty_lhs), Type::TCon(ty_rhs)) => {
            if ty_lhs.id != ty_rhs.id {
                return None;
            }

            match (&ty_lhs.args, &ty_rhs.args) {
                (None, Some(_)) => None,
                (Some(_), None) => None,
                (None, None) => Some(sbst),
                (Some(args1), Some(args2)) => {
                    if args1.len() != args2.len() {
                        return None;
                    }

                    for (t1, t2) in args1.iter().zip(args2.iter()) {
                        let s = unify(&t1.apply_sbst(&sbst), &t2.apply_sbst(&sbst))?;
                        sbst = compose(&s, &sbst);
                    }
                    Some(sbst)
                }
            }
        }
    }
}

/// - S: substitution
/// - x: type variable
/// - T: type
///
/// S := x : T, S
///
/// S1・S2
/// compose(S1, S2) = {
///   x : T.apply_sbst(S1) if x : T in S2
///   x : T                if x : T in S1 and x not in domain(S2)
/// }
fn compose(s1: &Sbst, s2: &Sbst) -> Sbst {
    let mut sbst = Sbst::new();

    for (x, t) in s2.iter() {
        sbst.insert(*x, t.apply_sbst(s1));
    }

    for (x, t) in s1.iter() {
        sbst.entry(*x).or_insert_with(|| t.clone());
    }

    sbst
}

/// find tail call
fn tail_call(expr: &mut LangExpr) {
    let l = tail_call_expr(expr);

    for e in l {
        if let LangExpr::ApplyExpr(app) = e {
            app.is_tail = true
        }
    }
}

fn tail_call_expr(expr: &mut LangExpr) -> LinkedList<&mut LangExpr> {
    match expr {
        LangExpr::ApplyExpr(_) => {
            let mut l = LinkedList::new();
            l.push_back(expr);
            l
        }
        LangExpr::IfExpr(e) => {
            let mut l = tail_call_expr(&mut e.then_expr);
            l.append(&mut tail_call_expr(&mut e.else_expr));
            l
        }
        LangExpr::MatchExpr(e) => {
            let mut l = LinkedList::new();
            for c in e.cases.iter_mut() {
                l.append(&mut tail_call_expr(&mut c.expr));
            }
            l
        }
        LangExpr::LetExpr(e) => tail_call_expr(&mut e.expr),
        LangExpr::LambdaExpr(e) => {
            tail_call(&mut e.expr);
            LinkedList::new()
        }
        _ => LinkedList::new(),
    }
}

fn exhaustive_expr(expr: &LangExpr, ctx: &Context) -> Result<(), TypingErr> {
    match expr {
        LangExpr::MatchExpr(e) => exhaustive_match(e, ctx),
        LangExpr::IfExpr(e) => exhaustive_if(e, ctx),
        LangExpr::LetExpr(e) => exhaustive_let(e, ctx),
        LangExpr::LambdaExpr(e) => exhaustive_expr(&e.expr, ctx),
        LangExpr::DataExpr(e) => exhaustive_exprs(&e.exprs, ctx),
        LangExpr::ApplyExpr(e) => exhaustive_exprs(&e.exprs, ctx),
        LangExpr::ListExpr(e) => exhaustive_exprs(&e.exprs, ctx),
        LangExpr::TupleExpr(e) => exhaustive_exprs(&e.exprs, ctx),
        _ => Ok(()),
    }
}

fn exhaustive_exprs(exprs: &[LangExpr], ctx: &Context) -> Result<(), TypingErr> {
    for e in exprs {
        exhaustive_expr(e, ctx)?;
    }
    Ok(())
}

fn exhaustive_let(expr: &LetNode, ctx: &Context) -> Result<(), TypingErr> {
    for dv in &expr.def_vars {
        exhaustive_expr(&dv.expr, ctx)?;
    }

    exhaustive_expr(&expr.expr, ctx)?;

    Ok(())
}

fn exhaustive_if(expr: &IfNode, ctx: &Context) -> Result<(), TypingErr> {
    exhaustive_expr(&expr.cond_expr, ctx)?;
    exhaustive_expr(&expr.then_expr, ctx)?;
    exhaustive_expr(&expr.else_expr, ctx)?;
    Ok(())
}

fn exhaustive_match(expr: &MatchNode, ctx: &Context) -> Result<(), TypingErr> {
    exhaustive_expr(&expr.expr, ctx)?;

    let mut patterns = LinkedList::new();
    for cs in &expr.cases {
        patterns.push_back(&cs.pattern);
    }

    check_pattern_exhaustive(&patterns, ctx, &expr.pos)?;

    for cs in &expr.cases {
        exhaustive_expr(&cs.expr, ctx)?;
    }

    Ok(())
}

struct Patterns<'a> {
    pat: BTreeMap<(String, usize), LinkedList<&'a Pattern>>,
}

impl<'a> Patterns<'a> {
    fn new() -> Patterns<'a> {
        Patterns {
            pat: BTreeMap::new(),
        }
    }

    fn insert(&mut self, label: &str, idx: usize, p: &'a Pattern) {
        match self.pat.get_mut(&(label.to_string(), idx)) {
            Some(lst) => {
                lst.push_back(p);
            }
            None => {
                let mut lst = LinkedList::new();
                lst.push_back(p);
                self.pat.insert((label.to_string(), idx), lst);
            }
        }
    }
}

fn check_pattern_exhaustive(
    patterns: &LinkedList<&Pattern>,
    ctx: &Context,
    pos: &Pos,
) -> Result<(), TypingErr> {
    if patterns.is_empty() {
        return Err(TypingErr {
            msg: "no pattern".to_string(),
            pos: *pos,
        });
    }

    let Some(ty) = patterns.front().unwrap().get_type() else {
        return Ok(());
    };

    // list up labels of type
    // example:
    // if
    //   (data (Maybe a)
    //     (Just a)
    //     Nothing)
    // then
    //   pat = [Just, Nothing]
    let mut pat = BTreeSet::new();
    match &ty {
        Type::TCon(tc) => {
            match tc.id.as_ref() {
                "Tuple" => {
                    pat.insert("Tuple".to_string());
                }
                "List" => {
                    pat.insert("Cons".to_string());
                    pat.insert("Nil".to_string());
                }
                "Bool" => {
                    pat.insert("true".to_string());
                    pat.insert("false".to_string());
                }
                "Int" => {
                    // integer type must be matched by general pattern
                    pat.insert("'dummy".to_string());
                }
                "String" => {
                    // string type must be matched by general pattern
                    pat.insert("'dummy".to_string());
                }
                "Char" => {
                    // char type must be matched by general pattern
                    pat.insert("'dummy".to_string());
                }
                _ => match ctx.data.get(&tc.id) {
                    Some(data) => {
                        pat = BTreeSet::new();
                        for mem in &data.members {
                            pat.insert(mem.id.id.clone());
                        }
                    }
                    None => {
                        let msg = format!("could not found \"{}\" type", ty);
                        return Err(TypingErr { msg, pos: *pos });
                    }
                },
            }
        }
        _ => {
            return Ok(());
        }
    }

    // remove labels specified in patterns
    // example 1:
    // if
    //   pat = [Just, Nothing]
    //   and
    //   (match (Just 10)
    //     ((Just x) x))
    // then
    //   pat = [Nothing]
    //
    // if variable pattern occurs then "is_all" becomes true
    // example 2:
    // if
    //   (match (Just 10)
    //     (x x))
    // then
    //   is_all = true
    let mut is_all = false;
    for p in patterns {
        match p {
            Pattern::PatID(_) => {
                is_all = true;
            }
            Pattern::PatData(p) => {
                // TODO: warning
                // if is_all then unreachable
                pat.remove(&p.label.id);
            }
            Pattern::PatBool(p) => {
                // TODO: warning
                // if is_all then unreachable
                if p.val {
                    pat.remove("true");
                } else {
                    pat.remove("false");
                }
            }
            Pattern::PatTuple(_) => {
                // TODO: warning
                // if is_all then unreachable
                pat.remove("Tuple");
            }
            Pattern::PatNil(_) => {
                pat.remove("Nil");
            }
            _ => {}
        }
    }

    if is_all {
        // success
        Ok(())
    } else if pat.is_empty() {
        // success but need to check recursively
        let mut ps = Patterns::new();
        for p in patterns {
            match p {
                Pattern::PatData(e) => {
                    for (i, p2) in e.pattern.iter().enumerate() {
                        ps.insert(&e.label.id, i, p2);
                    }
                }
                Pattern::PatTuple(e) => {
                    for (i, p2) in e.pattern.iter().enumerate() {
                        ps.insert("Tuple", i, p2);
                    }
                }
                _ => {}
            }
        }

        for plst in ps.pat.values() {
            check_pattern_exhaustive(plst, ctx, pos)?;
        }

        Ok(())
    } else {
        // fail
        Err(TypingErr {
            msg: "pattern is not exhaustive".to_string(),
            pos: *pos,
        })
    }
}
