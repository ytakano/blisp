use super::parser;
use super::semantics;
use super::{LispErr, Pos};

use alloc::boxed::Box;
use alloc::{
    collections::{btree_map::BTreeMap, linked_list::LinkedList, vec_deque::VecDeque},
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::{
    cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd},
    ops::{Shl, Shr},
    pin::Pin,
    ptr::{read_volatile, write_volatile},
};

use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero};

type Expr = semantics::LangExpr;
type Pattern = semantics::Pattern;

struct RuntimeErr {
    msg: String,
    pos: Pos,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Variables {
    vars: VecDeque<BTreeMap<String, RTData>>,
}

impl Variables {
    fn new() -> Variables {
        let mut list = VecDeque::new();
        list.push_back(BTreeMap::new());
        Variables { vars: list }
    }

    fn push(&mut self) {
        self.vars.push_back(BTreeMap::new());
    }

    fn pop(&mut self) {
        self.vars.pop_back();
    }

    fn insert(&mut self, id: String, data: RTData) {
        let m = self.vars.back_mut().unwrap();
        m.insert(id, data);
    }

    fn get(&mut self, id: &str) -> Option<&RTData> {
        for m in self.vars.iter().rev() {
            if let Some(val) = m.get(id) {
                return Some(val);
            }
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum TCall {
    Defun(String),
    Lambda(u64),
}

#[derive(Eq, Debug, Clone)]
struct IntType(*mut (BigInt, bool));

impl IntType {
    fn get_int(&self) -> &BigInt {
        unsafe { &(*self.0).0 }
    }

    fn get_ref(&mut self) -> &mut bool {
        unsafe { &mut (*self.0).1 }
    }
}

impl Ord for IntType {
    fn cmp(&self, other: &Self) -> Ordering {
        let s1 = self.get_int();
        let s2 = other.get_int();
        s1.cmp(s2)
    }
}

impl PartialOrd for IntType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let s1 = self.get_int();
        let s2 = other.get_int();
        Some(s1.cmp(s2))
    }
}

impl PartialEq for IntType {
    fn eq(&self, other: &Self) -> bool {
        let s1 = self.get_int();
        let s2 = other.get_int();
        s1 == s2
    }
}

#[derive(Eq, Debug, Clone)]
struct StrType(*mut (String, bool));

impl StrType {
    fn get_string(&self) -> &String {
        unsafe { &(*self.0).0 }
    }

    fn get_ref(&mut self) -> &mut bool {
        unsafe { &mut (*self.0).1 }
    }
}

impl Ord for StrType {
    fn cmp(&self, other: &Self) -> Ordering {
        let s1 = self.get_string();
        let s2 = other.get_string();
        s1.cmp(s2)
    }
}

impl PartialOrd for StrType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let s1 = self.get_string();
        let s2 = other.get_string();
        Some(s1.cmp(s2))
    }
}

impl PartialEq for StrType {
    fn eq(&self, other: &Self) -> bool {
        let s1 = self.get_string();
        let s2 = other.get_string();
        s1 == s2
    }
}

#[derive(Eq, Debug, Clone)]
struct ClojureType(*mut (Clojure, bool));

impl ClojureType {
    fn get_clojure(&self) -> &Clojure {
        unsafe { &(*self.0).0 }
    }

    fn get_clojure_mut(&mut self) -> &mut Clojure {
        unsafe { &mut (*self.0).0 }
    }

    fn get_ref(&mut self) -> &mut bool {
        unsafe { &mut (*self.0).1 }
    }
}

impl Ord for ClojureType {
    fn cmp(&self, other: &Self) -> Ordering {
        let s1 = self.get_clojure();
        let s2 = other.get_clojure();
        s1.cmp(s2)
    }
}

impl PartialOrd for ClojureType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let s1 = self.get_clojure();
        let s2 = other.get_clojure();
        Some(s1.cmp(s2))
    }
}

impl PartialEq for ClojureType {
    fn eq(&self, other: &Self) -> bool {
        let s1 = self.get_clojure();
        let s2 = other.get_clojure();
        s1 == s2
    }
}

#[derive(Eq, Debug, Clone)]
struct LDataType(*mut (LabeledData, bool));

impl LDataType {
    fn get_ldata(&self) -> &LabeledData {
        unsafe { &(*self.0).0 }
    }

    fn get_ldata_mut(&mut self) -> &mut LabeledData {
        unsafe { &mut (*self.0).0 }
    }

    fn get_ref(&mut self) -> &mut bool {
        unsafe { &mut (*self.0).1 }
    }
}

impl Ord for LDataType {
    fn cmp(&self, other: &Self) -> Ordering {
        let s1 = self.get_ldata();
        let s2 = other.get_ldata();
        s1.cmp(s2)
    }
}

impl PartialOrd for LDataType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let s1 = self.get_ldata();
        let s2 = other.get_ldata();
        Some(s1.cmp(s2))
    }
}

impl PartialEq for LDataType {
    fn eq(&self, other: &Self) -> bool {
        let s1 = self.get_ldata();
        let s2 = other.get_ldata();
        s1 == s2
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, PartialEq, Eq)]
enum RTData {
    Str(StrType),
    Char(char),
    Int(IntType),
    Bool(bool),
    Defun(String),
    Lambda(ClojureType),
    LData(LDataType),
    TailCall(TCall, Variables),
}

fn escape_char(c: char) -> String {
    match c {
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        '\0' => "\\0".to_string(),
        _ => c.to_string(),
    }
}

impl RTData {
    fn get_in_lisp(&self, list_head: bool) -> String {
        match self {
            RTData::Str(n) => {
                let mut str = "\"".to_string();
                for s in n.get_string().chars() {
                    if s == '"' {
                        str.push_str("\\\"");
                    } else {
                        str.push_str(&escape_char(s));
                    }
                }
                str.push('"');
                str
            }
            RTData::Char(c) => {
                if *c == '`' {
                    "`\\``".to_string()
                } else {
                    let s = escape_char(*c);
                    format!("`{}`", s)
                }
            }
            RTData::Int(n) => {
                format!("{}", n.get_int())
            }
            RTData::Bool(n) => n.to_string(),
            RTData::Defun(n) => n.to_string(),
            RTData::Lambda(n) => format!("(Lambda {})", n.get_clojure().ident),
            RTData::LData(n) => {
                let label = &n.get_ldata().label;
                if label == "Cons" {
                    let e1;
                    let e2;
                    match n.get_ldata().data.as_ref() {
                        Some(ld) => {
                            e1 = ld[0].get_in_lisp(true);
                            e2 = ld[1].get_in_lisp(false);
                        }
                        None => panic!("invalid list"),
                    }
                    if list_head {
                        if e2.is_empty() {
                            format!("'({})", e1)
                        } else {
                            format!("'({} {})", e1, e2)
                        }
                    } else if e2.is_empty() {
                        e1
                    } else {
                        format!("{} {}", e1, e2)
                    }
                } else if label == "Nil" {
                    if list_head {
                        "'()".to_string()
                    } else {
                        "".to_string()
                    }
                } else if label == "Tuple" {
                    match n.get_ldata().data.as_ref() {
                        Some(ld) => {
                            let mut msg = "".to_string();
                            let len = (*ld).len();
                            let mut i = 1;
                            for d in ld.iter() {
                                if i == len {
                                    msg = format!("{}{}", msg, d.get_in_lisp(true));
                                } else {
                                    msg = format!("{}{} ", msg, d.get_in_lisp(true));
                                }
                                i += 1;
                            }
                            format!("[{}]", msg)
                        }
                        None => "[]".to_string(),
                    }
                } else {
                    match n.get_ldata().data.as_ref() {
                        Some(ld) => {
                            let mut msg = format!("({}", label);
                            for d in ld.iter() {
                                msg = format!("{} {}", msg, d.get_in_lisp(true));
                            }
                            format!("{})", msg)
                        }
                        None => label.to_string(),
                    }
                }
            }
            RTData::TailCall(TCall::Defun(f), _) => format!("(TailCall (Defun {}))", f),
            RTData::TailCall(TCall::Lambda(f), _) => format!("(TailCall (Lambda {}))", f),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct LabeledData {
    label: String,
    data: Option<Vec<RTData>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Clojure {
    ident: u64,
    data: Option<BTreeMap<String, RTData>>,
}

const MIN_GC_NUM: usize = 1024;

struct RootObject {
    objects: LinkedList<Pin<Box<(LabeledData, bool)>>>,
    clojure: LinkedList<Pin<Box<(Clojure, bool)>>>,
    integers: LinkedList<Pin<Box<(BigInt, bool)>>>,
    strings: LinkedList<Pin<Box<(String, bool)>>>,
    threshold: usize,
}

impl RootObject {
    fn new() -> RootObject {
        RootObject {
            objects: LinkedList::new(),
            clojure: LinkedList::new(),
            integers: LinkedList::new(),
            strings: LinkedList::new(),
            threshold: MIN_GC_NUM,
        }
    }

    fn len(&self) -> usize {
        self.objects.len() + self.clojure.len() + self.integers.len() + self.strings.len()
    }

    fn make_int(&mut self, n: BigInt) -> IntType {
        self.integers.push_back(Box::pin((n, false)));
        let ptr = self.integers.back_mut().unwrap();
        IntType(unsafe { ptr.as_mut().get_unchecked_mut() as *mut (BigInt, bool) })
    }

    fn make_str(&mut self, str: String) -> StrType {
        self.strings.push_back(Box::pin((str, false)));
        let ptr = self.strings.back_mut().unwrap();
        StrType(unsafe { ptr.as_mut().get_unchecked_mut() as *mut (String, bool) })
    }

    fn make_obj(&mut self, label: String, data: Option<Vec<RTData>>) -> LDataType {
        let obj = LabeledData { label, data };
        self.objects.push_back(Box::pin((obj, false)));
        let ptr = self.objects.back_mut().unwrap();
        LDataType(unsafe { ptr.as_mut().get_unchecked_mut() as *mut (LabeledData, bool) })
    }

    fn make_clojure(&mut self, ident: u64, data: Option<BTreeMap<String, RTData>>) -> ClojureType {
        let obj = Clojure { ident, data };
        self.clojure.push_back(Box::pin((obj, false)));
        let ptr = self.clojure.back_mut().unwrap();
        ClojureType(unsafe { ptr.as_mut().get_unchecked_mut() as *mut (Clojure, bool) })
    }
}

pub(crate) fn eval(
    code: &str,
    ctx: &semantics::Context,
) -> Result<LinkedList<Result<String, String>>, LispErr> {
    let mut ps = parser::Parser::new(code, crate::FILE_ID_EVAL);
    let exprs;
    match ps.parse() {
        Ok(e) => {
            exprs = e;
        }
        Err(e) => {
            let msg = format!("Syntax Error: {}", e.msg);
            return Err(LispErr { msg, pos: e.pos });
        }
    }

    let mut typed_exprs = LinkedList::new();
    for expr in &exprs {
        match semantics::typing_expr(expr, ctx) {
            Ok(e) => {
                typed_exprs.push_back(e);
            }
            Err(e) => {
                let msg = format!("Typing Error: {}", e.msg);
                return Err(LispErr { msg, pos: e.pos });
            }
        }
    }

    let mut root = RootObject::new();
    let mut result = LinkedList::new();
    for (expr, lambda) in &typed_exprs {
        let mut vars = VecDeque::new();
        vars.push_back(Variables::new());
        match eval_expr(expr, lambda, ctx, &mut root, &mut vars) {
            Ok(val) => {
                result.push_back(Ok(val.get_in_lisp(true)));
            }
            Err(e) => {
                let msg = format!(
                    "(RuntimeErr [{} (Pos {} {})])",
                    e.msg, e.pos.line, e.pos.column
                );
                result.push_back(Err(msg));
                return Ok(result);
            }
        }
    }

    Ok(result)
}

fn get_data_of_id(id: &str, vars: &mut VecDeque<Variables>) -> RTData {
    match vars.back_mut().unwrap().get(id) {
        Some(data) => data.clone(),
        None => RTData::Defun(id.to_string()),
    }
}

fn eval_expr(
    expr: &Expr,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    match expr {
        Expr::LitStr(e) => Ok(RTData::Str(root.make_str(e.str.clone()))),
        Expr::LitNum(e) => Ok(RTData::Int(root.make_int(e.num.clone()))),
        Expr::LitChar(e) => Ok(RTData::Char(e.c)),
        Expr::LitBool(e) => Ok(RTData::Bool(e.val)),
        Expr::IfExpr(e) => eval_if(&e, lambda, ctx, root, vars),
        Expr::DataExpr(e) => eval_data(&e, lambda, ctx, root, vars),
        Expr::ListExpr(e) => eval_list(&e, lambda, ctx, root, vars),
        Expr::LetExpr(e) => eval_let(&e, lambda, ctx, root, vars),
        Expr::MatchExpr(e) => eval_match(&e, lambda, ctx, root, vars),
        Expr::IDExpr(e) => Ok(eval_id(&e, vars)),
        Expr::ApplyExpr(e) => eval_apply(&e, lambda, ctx, root, vars),
        Expr::TupleExpr(e) => eval_tuple(&e, lambda, ctx, root, vars),
        Expr::LambdaExpr(e) => Ok(eval_lambda(&e, root, vars)),
    }
}

fn eval_lambda(
    expr: &semantics::Lambda,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> RTData {
    let data = if !expr.vars.is_empty() {
        let mut m = BTreeMap::new();
        for v in &expr.vars {
            m.insert(v.to_string(), get_data_of_id(v, vars));
        }
        Some(m)
    } else {
        None
    };

    let ptr = root.make_clojure(expr.ident, data);
    RTData::Lambda(ptr)
}

fn eval_tuple(
    expr: &semantics::Exprs,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    let mut v = Vec::new();
    for e in expr.exprs.iter() {
        v.push(eval_expr(e, lambda, ctx, root, vars)?);
    }

    let ptr = root.make_obj("Tuple".to_string(), Some(v));

    Ok(RTData::LData(ptr))
}

fn get_fun<'a>(
    ctx: &'a semantics::Context,
    fun_name: &str,
    expr: &Expr,
) -> Result<&'a semantics::Defun, RuntimeErr> {
    let fun;
    match ctx.funs.get(fun_name) {
        Some(f) => {
            fun = f;
        }
        None => {
            let pos = expr.get_pos();
            let msg = format!("{} is not defined", fun_name);
            return Err(RuntimeErr { msg, pos });
        }
    }

    Ok(fun)
}

fn get_lambda<'a>(
    ctx: &'a semantics::Context,
    lambda: &'a BTreeMap<u64, semantics::Lambda>,
    id: u64,
    expr: &Expr,
) -> Result<&'a semantics::Lambda, RuntimeErr> {
    let fun;
    match ctx.lambda.get(&id) {
        Some(f) => {
            fun = f;
        }
        None => match lambda.get(&id) {
            Some(f) => {
                fun = f;
            }
            None => {
                let pos = expr.get_pos();
                let msg = format!("could not find (Lambda {})", id);
                return Err(RuntimeErr { msg, pos });
            }
        },
    }

    Ok(fun)
}

fn call_lambda(
    expr: &semantics::Apply,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
    cloj: &Clojure,
    iter: core::slice::Iter<semantics::LangExpr>,
    fun_expr: &semantics::LangExpr,
) -> Result<RTData, RuntimeErr> {
    // look up lambda
    let ident = cloj.ident;
    let fun = get_lambda(ctx, lambda, ident, fun_expr)?;

    // set up arguments
    let mut vars_fun = Variables::new();
    for (e, arg) in iter.zip(fun.args.iter()) {
        let data = eval_expr(&e, lambda, ctx, root, vars)?;
        vars_fun.insert(arg.id.to_string(), data);
    }

    // set up free variables
    match &cloj.data {
        Some(d) => {
            for (key, val) in d {
                vars_fun.insert(key.to_string(), val.clone());
            }
        }
        None => (),
    }

    // tail call optimization
    if expr.is_tail {
        Ok(RTData::TailCall(TCall::Lambda(ident), vars_fun))
    } else {
        vars.push_back(vars_fun);
        let result = eval_tail_call(&fun.expr, lambda, ctx, root, vars);
        vars.pop_back();
        result
    }
}

fn eval_apply(
    expr: &semantics::Apply,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    let mut iter = expr.exprs.iter();
    let fun_expr;
    match iter.next() {
        Some(e) => {
            fun_expr = e;
        }
        None => {
            let pos = expr.pos;
            return Err(RuntimeErr {
                msg: "empty application".to_string(),
                pos,
            });
        }
    }

    match eval_expr(&fun_expr, lambda, ctx, root, vars)? {
        RTData::Defun(fun_name) => {
            // call built-in function
            if ctx.built_in.contains(&fun_name) {
                let mut v = Vec::new();
                for e in iter {
                    let data = eval_expr(&e, lambda, ctx, root, vars)?;
                    v.push(data);
                }
                return eval_built_in(fun_name, &v, expr.pos, root, ctx);
            }

            // look up defun
            if let Ok(fun) = get_fun(ctx, &fun_name, fun_expr) {
                // set up arguments
                let mut vars_fun = Variables::new();
                for (e, arg) in iter.zip(fun.args.iter()) {
                    let data = eval_expr(&e, lambda, ctx, root, vars)?;
                    vars_fun.insert(arg.id.to_string(), data);
                }

                // tail call optimization
                if expr.is_tail {
                    Ok(RTData::TailCall(TCall::Defun(fun_name), vars_fun))
                } else {
                    vars.push_back(vars_fun);
                    let result = eval_tail_call(&fun.expr, lambda, ctx, root, vars)?;
                    vars.pop_back();
                    Ok(result)
                }
            } else {
                // call clojure
                if let Some(RTData::Lambda(cloj)) = vars.back_mut().unwrap().get(&fun_name) {
                    let c = cloj.clone();
                    return call_lambda(
                        expr,
                        lambda,
                        ctx,
                        root,
                        vars,
                        c.get_clojure(),
                        iter,
                        fun_expr,
                    );
                }

                // could not find such function
                let pos = fun_expr.get_pos();
                let msg = format!("{} is not defined", fun_name);
                Err(RuntimeErr { msg, pos })
            }
        }
        RTData::Lambda(f) => {
            let f = f.get_clojure();
            call_lambda(expr, lambda, ctx, root, vars, f, iter, fun_expr)
        }
        _ => {
            let pos = fun_expr.get_pos();
            Err(RuntimeErr {
                msg: "not function".to_string(),
                pos,
            })
        }
    }
}

fn eval_tail_call<'a>(
    mut expr: &'a Expr,
    lambda: &'a BTreeMap<u64, semantics::Lambda>,
    ctx: &'a semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    loop {
        match eval_expr(expr, lambda, ctx, root, vars)? {
            RTData::TailCall(TCall::Defun(fun_name), vars_fun) => {
                let fun = get_fun(ctx, &fun_name, expr)?;
                expr = &fun.expr;
                vars.pop_back();
                vars.push_back(vars_fun);
                collect_garbage(vars, root); // mark and sweep
            }
            RTData::TailCall(TCall::Lambda(id), vars_fun) => {
                let fun = get_lambda(ctx, lambda, id, expr)?;
                expr = &fun.expr;
                vars.pop_back();
                vars.push_back(vars_fun);
                collect_garbage(vars, root); // mark and sweep
            }
            x => {
                return Ok(x);
            }
        }
    }
}

fn get_int(args: &[RTData], pos: Pos) -> Result<*const BigInt, RuntimeErr> {
    match &args[0] {
        RTData::Int(n) => Ok(n.get_int()),
        _ => Err(RuntimeErr {
            msg: "there must be exactly 2 integers".to_string(),
            pos,
        }),
    }
}

fn get_int_int(args: &[RTData], pos: Pos) -> Result<(*const BigInt, *const BigInt), RuntimeErr> {
    match (&args[0], &args[1]) {
        (RTData::Int(n1), RTData::Int(n2)) => Ok((n1.get_int(), n2.get_int())),
        _ => Err(RuntimeErr {
            msg: "there must be exactly 2 integers".to_string(),
            pos,
        }),
    }
}

fn get_int_int_int(
    args: &[RTData],
    pos: Pos,
) -> Result<(*const BigInt, *const BigInt, *const BigInt), RuntimeErr> {
    match (&args[0], &args[1], &args[2]) {
        (RTData::Int(n1), RTData::Int(n2), RTData::Int(n3)) => {
            Ok((n1.get_int(), n2.get_int(), n3.get_int()))
        }
        _ => Err(RuntimeErr {
            msg: "there must be exactly 3 integers".to_string(),
            pos,
        }),
    }
}

fn get_bool_bool(args: &[RTData], pos: Pos) -> Result<(bool, bool), RuntimeErr> {
    match (args[0].clone(), args[1].clone()) {
        (RTData::Bool(n1), RTData::Bool(n2)) => Ok((n1, n2)),
        _ => Err(RuntimeErr {
            msg: "there must be exactly 2 boolean values".to_string(),
            pos,
        }),
    }
}

fn get_bool(args: &[RTData], pos: Pos) -> Result<bool, RuntimeErr> {
    match args[0].clone() {
        RTData::Bool(n) => Ok(n),
        _ => Err(RuntimeErr {
            msg: "there must be exactly 1 boolean value".to_string(),
            pos,
        }),
    }
}

fn eval_built_in(
    fun_name: String,
    args: &[RTData],
    pos: Pos,
    root: &mut RootObject,
    ctx: &semantics::Context,
) -> Result<RTData, RuntimeErr> {
    match fun_name.as_str() {
        "+" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let n = unsafe { &*n1 + &*n2 };
            Ok(RTData::Int(root.make_int(n)))
        }
        "-" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let n = unsafe { &*n1 - &*n2 };
            Ok(RTData::Int(root.make_int(n)))
        }
        "*" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let n = unsafe { &*n1 * &*n2 };
            Ok(RTData::Int(root.make_int(n)))
        }
        "/" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let n = unsafe { &*n1 / &*n2 };
            Ok(RTData::Int(root.make_int(n)))
        }
        "%" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let n = unsafe { &*n1 % &*n2 };
            Ok(RTData::Int(root.make_int(n)))
        }
        "=" | "eq" => Ok(RTData::Bool(args[0] == args[1])),
        "!=" | "neq" => Ok(RTData::Bool(args[0] != args[1])),
        "<=" | "leq" => Ok(RTData::Bool(args[0] <= args[1])),
        ">=" | "geq" => Ok(RTData::Bool(args[0] >= args[1])),
        ">" | "gt" => Ok(RTData::Bool(args[0] > args[1])),
        "<" | "lt" => Ok(RTData::Bool(args[0] < args[1])),
        "and" => {
            let (n1, n2) = get_bool_bool(args, pos)?;
            Ok(RTData::Bool(n1 && n2))
        }
        "or" => {
            let (n1, n2) = get_bool_bool(args, pos)?;
            Ok(RTData::Bool(n1 || n2))
        }
        "xor" => {
            let (n1, n2) = get_bool_bool(args, pos)?;
            Ok(RTData::Bool(n1 ^ n2))
        }
        "not" => {
            let n = get_bool(args, pos)?;
            Ok(RTData::Bool(!n))
        }
        "band" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let n = unsafe { &*n1 & &*n2 };
            Ok(RTData::Int(root.make_int(n)))
        }
        "bor" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let n = unsafe { &*n1 | &*n2 };
            Ok(RTData::Int(root.make_int(n)))
        }
        "bxor" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let n = unsafe { &*n1 ^ &*n2 };
            Ok(RTData::Int(root.make_int(n)))
        }
        "sqrt" => {
            let n = get_int(args, pos)?;
            if unsafe { (*n) >= Zero::zero() } {
                let n = unsafe { (*n).sqrt() };
                let n = RTData::Int(root.make_int(n));
                let ptr = root.make_obj("Some".to_string(), Some(vec![n]));
                Ok(RTData::LData(ptr))
            } else {
                let ptr = root.make_obj("None".to_string(), None);
                Ok(RTData::LData(ptr))
            }
        }
        "pow" => {
            let (n1, n2) = get_int_int(args, pos)?;
            if let Some(e) = unsafe { (*n2).to_u32() } {
                let n = unsafe { (*n1).pow(e) };
                let n = RTData::Int(root.make_int(n));
                let ptr = root.make_obj("Some".to_string(), Some(vec![n]));
                Ok(RTData::LData(ptr))
            } else {
                let ptr = root.make_obj("None".to_string(), None);
                Ok(RTData::LData(ptr))
            }
        }
        ">>" => {
            let (n1, n2) = get_int_int(args, pos)?;
            if let Some(e) = unsafe { (*n2).to_u64() } {
                let n = unsafe { (*n1).clone() };
                let n = n.shr(e);
                let n = RTData::Int(root.make_int(n));
                let ptr = root.make_obj("Some".to_string(), Some(vec![n]));
                Ok(RTData::LData(ptr))
            } else {
                let ptr = root.make_obj("None".to_string(), None);
                Ok(RTData::LData(ptr))
            }
        }
        "<<" => {
            let (n1, n2) = get_int_int(args, pos)?;
            if let Some(e) = unsafe { (*n2).to_u64() } {
                let n = unsafe { (*n1).clone() };
                let n = n.shl(e);
                let n = RTData::Int(root.make_int(n));
                let ptr = root.make_obj("Some".to_string(), Some(vec![n]));
                Ok(RTData::LData(ptr))
            } else {
                let ptr = root.make_obj("None".to_string(), None);
                Ok(RTData::LData(ptr))
            }
        }
        "chars" => {
            let mut tail = RTData::LData(root.make_obj("Nil".to_string(), None));
            if let RTData::Str(st) = &args[0] {
                let s = st.get_string();
                for c in s.chars().rev() {
                    let c = RTData::Char(c);
                    let cons =
                        RTData::LData(root.make_obj("Cons".to_string(), Some(vec![c, tail])));
                    tail = cons;
                }
            }
            Ok(tail)
        }
        "str" => {
            let mut head = &args[0];
            let mut s = "".to_string();
            loop {
                if let RTData::LData(data) = head {
                    if data.get_ldata().label == "Cons" {
                        if let Some(d) = &data.get_ldata().data {
                            if let RTData::Char(c) = &d[0] {
                                s.push(*c);
                                head = &d[1];
                            } else {
                                return Err(RuntimeErr {
                                    msg: "not char".to_string(),
                                    pos,
                                });
                            }
                        } else {
                            return Err(RuntimeErr {
                                msg: "invalid cons".to_string(),
                                pos,
                            });
                        }
                    } else if data.get_ldata().label == "Nil" {
                        break;
                    } else {
                        return Err(RuntimeErr {
                            msg: "not list".to_string(),
                            pos,
                        });
                    }
                }
            }
            let ptr = root.make_str(s);
            Ok(RTData::Str(ptr))
        }
        "call-rust" => {
            let (n1, n2, n3) = get_int_int_int(args, pos)?;
            let n = unsafe { (ctx.callback)(&*n1, &*n2, &*n3) };
            if let Some(n) = n {
                let n = RTData::Int(root.make_int(n));
                let ptr = root.make_obj("Some".to_string(), Some(vec![n]));
                Ok(RTData::LData(ptr))
            } else {
                let ptr = root.make_obj("None".to_string(), None);
                Ok(RTData::LData(ptr))
            }
        }
        _ => Err(RuntimeErr {
            msg: "unknown built-in function".to_string(),
            pos,
        }),
    }
}

fn eval_match(
    expr: &semantics::MatchNode,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    let data = eval_expr(&expr.expr, lambda, ctx, root, vars)?;

    for c in &expr.cases {
        vars.back_mut().unwrap().push();
        if eval_pat(&c.pattern, data.clone(), vars) {
            let retval = eval_expr(&c.expr, lambda, ctx, root, vars)?;
            vars.back_mut().unwrap().pop();
            return Ok(retval);
        }
        vars.back_mut().unwrap().pop();
    }

    let pos = expr.pos;
    Err(RuntimeErr {
        msg: "pattern-matching is not exhaustive".to_string(),
        pos,
    })
}

fn eval_id(expr: &semantics::IDNode, vars: &mut VecDeque<Variables>) -> RTData {
    let id = expr.id.to_string();
    get_data_of_id(&id, vars)
}

fn eval_list(
    expr: &semantics::Exprs,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    let mut elm = root.make_obj("Nil".to_string(), None);
    for e in expr.exprs.iter().rev() {
        let val = eval_expr(e, lambda, ctx, root, vars)?;
        elm = root.make_obj("Cons".to_string(), Some(vec![val, RTData::LData(elm)]));
    }

    Ok(RTData::LData(elm))
}

fn eval_if(
    expr: &semantics::IfNode,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    let cond = eval_expr(&expr.cond_expr, lambda, ctx, root, vars)?;
    let flag;
    match cond {
        RTData::Bool(e) => {
            flag = e;
        }
        _ => {
            let pos = expr.cond_expr.get_pos();
            return Err(RuntimeErr {
                msg: "type mismatched".to_string(),
                pos,
            });
        }
    }

    if flag {
        eval_expr(&expr.then_expr, lambda, ctx, root, vars)
    } else {
        eval_expr(&expr.else_expr, lambda, ctx, root, vars)
    }
}

fn eval_data(
    expr: &semantics::DataNode,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    let data = if expr.exprs.is_empty() {
        None
    } else {
        let mut v = Vec::new();
        for e in &expr.exprs {
            v.push(eval_expr(e, lambda, ctx, root, vars)?);
        }
        Some(v)
    };

    let ptr = root.make_obj(expr.label.id.to_string(), data);

    Ok(RTData::LData(ptr))
}

fn eval_let(
    expr: &semantics::LetNode,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    vars.back_mut().unwrap().push();

    for def in &expr.def_vars {
        let data = eval_expr(&def.expr, lambda, ctx, root, vars)?;
        if !eval_pat(&def.pattern, data, vars) {
            let pos = def.pattern.get_pos();
            return Err(RuntimeErr {
                msg: "failed pattern matching".to_string(),
                pos,
            });
        }
    }

    let result = eval_expr(&expr.expr, lambda, ctx, root, vars)?;
    vars.back_mut().unwrap().pop();

    Ok(result)
}

fn eval_pat(pat: &Pattern, data: RTData, vars: &mut VecDeque<Variables>) -> bool {
    match pat {
        Pattern::PatID(p) => {
            vars.back_mut().unwrap().insert(p.id.to_string(), data);
            true
        }
        Pattern::PatStr(p) => match data {
            RTData::Str(n) => n.get_string() == &p.str,
            _ => false,
        },
        Pattern::PatChar(p) => match data {
            RTData::Char(n) => n == p.c,
            _ => false,
        },
        Pattern::PatNum(p) => match data {
            RTData::Int(n) => n.get_int() == &p.num,
            _ => false,
        },
        Pattern::PatBool(p) => match data {
            RTData::Bool(n) => n == p.val,
            _ => false,
        },
        Pattern::PatNil(_) => match data {
            RTData::LData(ptr) => ptr.get_ldata().label == "Nil",
            _ => false,
        },
        Pattern::PatTuple(p) => match data {
            RTData::LData(ptr) => {
                if ptr.get_ldata().label != "Tuple" {
                    return false;
                }

                match &ptr.get_ldata().data {
                    Some(rds) => {
                        for (pat2, rd) in p.pattern.iter().zip(rds.iter()) {
                            if !eval_pat(pat2, rd.clone(), vars) {
                                return false;
                            }
                        }
                        true
                    }
                    None => true,
                }
            }
            _ => false,
        },
        Pattern::PatData(p) => match data {
            RTData::LData(ptr) => {
                if ptr.get_ldata().label != p.label.id {
                    return false;
                }

                match &ptr.get_ldata().data {
                    Some(rds) => {
                        for (pat2, rd) in p.pattern.iter().zip(rds.iter()) {
                            if !eval_pat(pat2, rd.clone(), vars) {
                                return false;
                            }
                        }
                        true
                    }
                    None => true,
                }
            }
            _ => false,
        },
    }
}

/// do garbage collection
fn collect_garbage(vars: &mut VecDeque<Variables>, root: &mut RootObject) {
    let n = root.len();
    if n < root.threshold {
        return;
    }

    mark(vars);
    sweep(&mut root.clojure);
    sweep(&mut root.objects);
    sweep(&mut root.integers);
    sweep(&mut root.strings);

    let n = root.len();
    root.threshold = n * 2;
    if root.threshold < (MIN_GC_NUM >> 1) {
        root.threshold = MIN_GC_NUM;
    }
}

/// mark reachable objects
fn mark(vars: &mut VecDeque<Variables>) {
    for v in vars.iter_mut() {
        for var in v.vars.iter_mut() {
            for (_, v) in var.iter_mut() {
                mark_obj(v);
            }
        }
    }
}

/// mark reachable objects recursively
fn mark_obj(data: &mut RTData) {
    match data {
        RTData::Str(ptr) => unsafe {
            write_volatile(ptr.get_ref(), true);
        },
        RTData::Int(ptr) => unsafe {
            write_volatile(ptr.get_ref(), true);
        },
        RTData::Lambda(ptr) => unsafe {
            if !read_volatile(ptr.get_ref()) {
                write_volatile(ptr.get_ref(), true);
                if let Some(data) = &mut ptr.get_clojure_mut().data {
                    for (_, v) in data.iter_mut() {
                        mark_obj(v);
                    }
                }
            }
        },
        RTData::LData(ptr) => unsafe {
            if !read_volatile(ptr.get_ref()) {
                write_volatile(ptr.get_ref(), true);
                if let Some(data) = &mut ptr.get_ldata_mut().data {
                    for v in data.iter_mut() {
                        mark_obj(v);
                    }
                }
            }
        },
        _ => (),
    }
}

/// remove unreachable objects
fn sweep<T>(root: &mut LinkedList<Pin<Box<(T, bool)>>>) {
    let mut tail = root.split_off(0);
    loop {
        if tail.is_empty() {
            break;
        }

        // take head
        let mut head;
        if tail.len() == 1 {
            head = tail.split_off(0);
        } else {
            let tmp = tail.split_off(1);
            head = tail;
            tail = tmp;
        };

        // check the head is reachable or not
        let h = head.front_mut().unwrap();
        let marked = unsafe { read_volatile(&h.as_ref().1) };
        let flag = if marked {
            // the head is reachable
            let h = h.as_mut();
            unsafe {
                h.get_unchecked_mut().1 = false;
            }
            true
        } else {
            // the head unreachable
            false
        };

        // if reachable, append the head
        if flag {
            root.append(&mut head);
        }
    }
}
