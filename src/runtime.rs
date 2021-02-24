use super::parser;
use super::semantics;
use super::{LispErr, Pos};

use alloc::boxed::Box;
use alloc::collections::btree_map::BTreeMap;
use alloc::collections::linked_list::LinkedList;
use alloc::collections::vec_deque::VecDeque;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::pin::Pin;
use core::ptr::{read_volatile, write_volatile};
use num_bigint::BigInt;

type Expr = semantics::LangExpr;
type Pattern = semantics::Pattern;

struct RuntimeErr {
    msg: String,
    pos: Pos,
}

#[derive(Debug, Clone)]
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

    fn get(&mut self, id: &String) -> Option<&RTData> {
        for m in self.vars.iter().rev() {
            if let Some(val) = m.get(id) {
                return Some(val);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
enum TCall {
    Defun(String),
    Lambda(u64),
}

#[derive(Debug, Clone)]
enum RTData {
    Int(*mut (BigInt, bool)),
    Bool(bool),
    Defun(String),
    Lambda(*mut (Clojure, bool)),
    LData(*mut (LabeledData, bool)),
    TailCall(TCall, Variables),
}

impl RTData {
    fn get_in_lisp(&self, list_head: bool) -> String {
        match self {
            RTData::Int(n) => {
                println!("n = {:?}", *n);
                format!("{}", unsafe { &(**n).0 })
            }
            RTData::Bool(n) => format!("{}", n),
            RTData::Defun(n) => format!("{}", n),
            RTData::Lambda(n) => format!("(Lambda {})", unsafe { &(*(*n)).0.ident }),
            RTData::LData(n) => {
                let label = unsafe { &(*(*n)).0.label };
                if label == "Cons" {
                    let e1;
                    let e2;
                    match unsafe { (*(*n)).0.data.as_ref() } {
                        Some(ld) => {
                            e1 = ld[0].get_in_lisp(true);
                            e2 = ld[1].get_in_lisp(false);
                        }
                        None => panic!("invalid list"),
                    }
                    if list_head {
                        if e2 == "" {
                            format!("'({})", e1)
                        } else {
                            format!("'({} {})", e1, e2)
                        }
                    } else {
                        if e2 == "" {
                            e1
                        } else {
                            format!("{} {}", e1, e2)
                        }
                    }
                } else if label == "Nil" {
                    if list_head {
                        "'()".to_string()
                    } else {
                        "".to_string()
                    }
                } else if label == "Tuple" {
                    match unsafe { (*(*n)).0.data.as_ref() } {
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
                    match unsafe { (*(*n)).0.data.as_ref() } {
                        Some(ld) => {
                            let mut msg = format!("({}", label);
                            for d in ld.iter() {
                                msg = format!("{} {}", msg, d.get_in_lisp(true));
                            }
                            format!("{})", msg)
                        }
                        None => format!("{}", label),
                    }
                }
            }
            RTData::TailCall(TCall::Defun(f), _) => format!("(TailCall (Defun {}))", f),
            RTData::TailCall(TCall::Lambda(f), _) => format!("(TailCall (Lambda {}))", f),
        }
    }
}

#[derive(Debug)]
struct LabeledData {
    label: String,
    data: Option<Vec<RTData>>,
}

#[derive(Debug)]
struct Clojure {
    ident: u64,
    data: Option<BTreeMap<String, RTData>>,
}

struct RootObject {
    objects: LinkedList<Pin<Box<(LabeledData, bool)>>>,
    clojure: LinkedList<Pin<Box<(Clojure, bool)>>>,
    integers: LinkedList<Pin<Box<(BigInt, bool)>>>,
}

impl RootObject {
    fn new() -> RootObject {
        RootObject {
            objects: LinkedList::new(),
            clojure: LinkedList::new(),
            integers: LinkedList::new(),
        }
    }

    fn make_int(&mut self, n: BigInt) -> *mut (BigInt, bool) {
        self.integers.push_back(Box::pin((n, false)));
        let ptr = self.integers.back_mut().unwrap();

        let h = unsafe { ptr.as_mut().get_unchecked_mut() };
        println!("alloc int {:x}", h as *const (BigInt, bool) as usize);

        unsafe { ptr.as_mut().get_unchecked_mut() as *mut (BigInt, bool) }
    }

    fn make_obj(&mut self, label: String, data: Option<Vec<RTData>>) -> *mut (LabeledData, bool) {
        let obj = LabeledData {
            label: label,
            data: data,
        };
        self.objects.push_back(Box::pin((obj, false)));
        let ptr = self.objects.back_mut().unwrap();

        let h = unsafe { ptr.as_mut().get_unchecked_mut() };
        println!("alloc data {:x}", h as *const (LabeledData, bool) as usize);

        unsafe { ptr.as_mut().get_unchecked_mut() as *mut (LabeledData, bool) }
    }

    fn make_clojure(
        &mut self,
        ident: u64,
        data: Option<BTreeMap<String, RTData>>,
    ) -> *mut (Clojure, bool) {
        let obj = Clojure {
            ident: ident,
            data: data,
        };
        self.clojure.push_back(Box::pin((obj, false)));
        let ptr = self.clojure.back_mut().unwrap();

        let h = unsafe { ptr.as_mut().get_unchecked_mut() };
        println!("alloc lambda {:x}", h as *const (Clojure, bool) as usize);

        unsafe { ptr.as_mut().get_unchecked_mut() as *mut (Clojure, bool) }
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
            return Err(LispErr {
                msg: msg,
                pos: e.pos,
            });
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
                return Err(LispErr {
                    msg: msg,
                    pos: e.pos,
                });
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
                println!("val = {:?}", val);
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

fn get_data_of_id(id: &String, vars: &mut VecDeque<Variables>) -> RTData {
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
        Expr::LitNum(e) => Ok(RTData::Int(root.make_int(e.num.clone()))),
        Expr::LitBool(e) => Ok(RTData::Bool(e.val)),
        Expr::IfExpr(e) => eval_if(&e, lambda, ctx, root, vars),
        Expr::DataExpr(e) => eval_data(&e, lambda, ctx, root, vars),
        Expr::ListExpr(e) => eval_list(&e, lambda, ctx, root, vars),
        Expr::LetExpr(e) => eval_let(&e, lambda, ctx, root, vars),
        Expr::MatchExpr(e) => eval_match(&e, lambda, ctx, root, vars),
        Expr::IDExpr(e) => eval_id(&e, vars),
        Expr::ApplyExpr(e) => eval_apply(&e, lambda, ctx, root, vars),
        Expr::TupleExpr(e) => eval_tuple(&e, lambda, ctx, root, vars),
        Expr::LambdaExpr(e) => eval_lambda(&e, root, vars),
    }
}

fn eval_lambda(
    expr: &semantics::Lambda,
    root: &mut RootObject,
    vars: &mut VecDeque<Variables>,
) -> Result<RTData, RuntimeErr> {
    let data = if expr.vars.len() > 0 {
        let mut m = BTreeMap::new();
        for v in &expr.vars {
            m.insert(v.to_string(), get_data_of_id(v, vars));
        }
        Some(m)
    } else {
        None
    };

    let ptr = root.make_clojure(expr.ident, data);
    Ok(RTData::Lambda(ptr))
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
    fun_name: &String,
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
            return Err(RuntimeErr { msg: msg, pos: pos });
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
                return Err(RuntimeErr { msg: msg, pos: pos });
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
    cloj: *const Clojure,
    iter: core::slice::Iter<semantics::LangExpr>,
    fun_expr: &semantics::LangExpr,
) -> Result<RTData, RuntimeErr> {
    // look up lambda
    let ident = unsafe { (*cloj).ident };
    let fun = get_lambda(ctx, lambda, ident, fun_expr)?;

    // set up arguments
    let mut vars_fun = Variables::new();
    for (e, arg) in iter.zip(fun.args.iter()) {
        let data = eval_expr(&e, lambda, ctx, root, vars)?;
        vars_fun.insert(arg.id.to_string(), data);
    }

    // set up free variables
    match unsafe { &(*cloj).data } {
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
                pos: pos,
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
                return eval_built_in(fun_name, v, expr.pos, root, ctx);
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
                if let Some(f) = vars.back_mut().unwrap().get(&fun_name) {
                    if let RTData::Lambda(cloj) = f {
                        let cloj = unsafe { &mut (**cloj).0 };
                        return call_lambda(expr, lambda, ctx, root, vars, cloj, iter, fun_expr);
                    }
                }

                // could not find such function
                let pos = fun_expr.get_pos();
                let msg = format!("{} is not defined", fun_name);
                Err(RuntimeErr { msg: msg, pos: pos })
            }
        }
        RTData::Lambda(f) => {
            let f = unsafe { &(*f).0 };
            call_lambda(expr, lambda, ctx, root, vars, f, iter, fun_expr)
        }
        _ => {
            let pos = fun_expr.get_pos();
            return Err(RuntimeErr {
                msg: "not function".to_string(),
                pos: pos,
            });
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
            }
            RTData::TailCall(TCall::Lambda(id), vars_fun) => {
                let fun = get_lambda(ctx, lambda, id, expr)?;
                expr = &fun.expr;
                vars.pop_back();
                vars.push_back(vars_fun);
            }
            x => {
                return Ok(x);
            }
        }
    }
}

fn get_int_int(args: Vec<RTData>, pos: Pos) -> Result<(*const BigInt, *const BigInt), RuntimeErr> {
    match (&args[0], &args[1]) {
        (RTData::Int(n1), RTData::Int(n2)) => unsafe { Ok((&(**n1).0, &(**n2).0)) },
        _ => Err(RuntimeErr {
            msg: "there must be exactly 2 integers".to_string(),
            pos: pos,
        }),
    }
}

fn get_int_int_int(
    args: Vec<RTData>,
    pos: Pos,
) -> Result<(*const BigInt, *const BigInt, *const BigInt), RuntimeErr> {
    match (&args[0], &args[1], &args[2]) {
        (RTData::Int(n1), RTData::Int(n2), RTData::Int(n3)) => unsafe {
            Ok((&(**n1).0, &(**n2).0, &(**n3).0))
        },
        _ => Err(RuntimeErr {
            msg: "there must be exactly 3 integers".to_string(),
            pos: pos,
        }),
    }
}

fn get_bool_bool(args: Vec<RTData>, pos: Pos) -> Result<(bool, bool), RuntimeErr> {
    match (args[0].clone(), args[1].clone()) {
        (RTData::Bool(n1), RTData::Bool(n2)) => Ok((n1, n2)),
        _ => Err(RuntimeErr {
            msg: "there must be exactly 2 boolean values".to_string(),
            pos: pos,
        }),
    }
}

fn get_bool(args: Vec<RTData>, pos: Pos) -> Result<bool, RuntimeErr> {
    match args[0].clone() {
        RTData::Bool(n) => Ok(n),
        _ => Err(RuntimeErr {
            msg: "there must be exactly 1 boolean value".to_string(),
            pos: pos,
        }),
    }
}

fn eval_built_in(
    fun_name: String,
    args: Vec<RTData>,
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
        "<" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let b = unsafe { &*n1 < &*n2 };
            Ok(RTData::Bool(b))
        }
        ">" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let b = unsafe { &*n1 > &*n2 };
            Ok(RTData::Bool(b))
        }
        "=" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let b = unsafe { &*n1 == &*n2 };
            Ok(RTData::Bool(b))
        }
        "<=" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let b = unsafe { &*n1 <= &*n2 };
            Ok(RTData::Bool(b))
        }
        ">=" => {
            let (n1, n2) = get_int_int(args, pos)?;
            let b = unsafe { &*n1 >= &*n2 };
            Ok(RTData::Bool(b))
        }
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
            pos: pos,
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
        pos: pos,
    })
}

fn eval_id(expr: &semantics::IDNode, vars: &mut VecDeque<Variables>) -> Result<RTData, RuntimeErr> {
    let id = expr.id.to_string();
    Ok(get_data_of_id(&id, vars))
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
                pos: pos,
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
    let data = if expr.exprs.len() == 0 {
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
                pos: pos,
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
        Pattern::PatNum(p) => match data {
            RTData::Int(n) => (unsafe { &(*n).0 }) == &p.num,
            _ => false,
        },
        Pattern::PatBool(p) => match data {
            RTData::Bool(n) => n == p.val,
            _ => false,
        },
        Pattern::PatNil(_) => match data {
            RTData::LData(ptr) => unsafe { (*ptr).0.label == "Nil" },
            _ => false,
        },
        Pattern::PatTuple(p) => match data {
            RTData::LData(ptr) => {
                if unsafe { &(*ptr).0.label } != "Tuple" {
                    return false;
                }

                match unsafe { &(*ptr).0.data } {
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
                if unsafe { (*ptr).0.label != p.label.id } {
                    return false;
                }

                match unsafe { &(*ptr).0.data } {
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

fn collect_garbage(vars: &mut VecDeque<Variables>, root: &mut RootObject) {
    println!("start GC");
    mark(vars);
    sweep(&mut root.clojure);
    sweep(&mut root.objects);
    sweep(&mut root.integers);
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

fn mark_obj(data: &mut RTData) {
    match data {
        RTData::Int(ptr) => unsafe {
            println!("mark int {:x}", *ptr as usize);

            (**ptr).1 = true;
        },
        RTData::Lambda(ptr) => unsafe {
            if !(**ptr).1 {
                println!("mark lambda {:x}", *ptr as usize);
                write_volatile(&mut (**ptr).1, true);
                //(**ptr).1 = true;
                println!("{}", (**ptr).1);
                if let Some(data) = &mut (**ptr).0.data {
                    for (_, v) in data.iter_mut() {
                        mark_obj(v);
                    }
                }
            }
        },
        RTData::LData(ptr) => unsafe {
            if !(**ptr).1 {
                println!("mark data {:x}", *ptr as usize);
                write_volatile(&mut (**ptr).1, true);
                if let Some(data) = &mut (**ptr).0.data {
                    for v in data.iter_mut() {
                        mark_obj(v);
                    }
                }
            }
        },
        RTData::TailCall(_, _) => {
            println!("tail call");
        }
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
            // reachable
            let h = h.as_mut();
            unsafe {
                h.get_unchecked_mut().1 = false;
            }
            true
        } else {
            // unreachable
            let h = unsafe { h.as_mut().get_unchecked_mut() };
            println!("freed {:x}", h as *const (T, bool) as usize);
            println!("{}", marked);
            false
        };

        // if reachable, append the head
        if flag {
            root.append(&mut head);
        }
    }
}
