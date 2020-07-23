use super::parser;
use super::semantics;
use super::{LispErr, Pos};

// use crate::driver::uart;

use alloc::collections::btree_map::BTreeMap;
use alloc::collections::linked_list::LinkedList;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

type Expr = semantics::LangExpr;
type Pattern = semantics::Pattern;

struct RuntimeErr {
    msg: String,
    pos: Pos,
}

#[derive(Debug, Clone)]
struct Variables {
    vars: LinkedList<BTreeMap<String, RTData>>,
}

impl Variables {
    fn new() -> Variables {
        let mut list = LinkedList::new();
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
        let m = self.vars.back_mut().unwrap();
        m.get(id)
    }
}

#[derive(Debug, Clone)]
enum TCall {
    Defun(String),
    Lambda(u64),
}

#[derive(Debug, Clone)]
enum RTData {
    Int(i64),
    Bool(bool),
    Defun(String),
    Lambda(*const Clojure),
    LData(*const LabeledData),
    TailCall(TCall, Variables),
}

impl RTData {
    fn get_in_lisp(&self) -> String {
        match self {
            RTData::Int(n) => format!("{:?}", n),
            RTData::Bool(n) => format!("{:?}", n),
            RTData::Defun(n) => format!("{}", n),
            RTData::Lambda(n) => format!("(Lambda {})", unsafe { &(*(*n)).ident }),
            RTData::LData(n) => {
                let mut msg = format!("({}", unsafe { &(*(*n)).label });
                match unsafe { (*(*n)).data.as_ref() } {
                    Some(ld) => {
                        for d in ld.iter() {
                            msg = format!("{} {}", msg, d.get_in_lisp());
                        }
                        format!("{})", msg)
                    }
                    None => format!("{})", msg),
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
    objects: LinkedList<LabeledData>,
    clojure: LinkedList<Clojure>,
}

impl RootObject {
    fn new() -> RootObject {
        RootObject {
            objects: LinkedList::new(),
            clojure: LinkedList::new(),
        }
    }

    fn make_obj(&mut self, label: String, data: Option<Vec<RTData>>) -> *const LabeledData {
        let obj = LabeledData {
            label: label,
            data: data,
        };
        self.objects.push_back(obj);
        self.objects.back().unwrap() as *const LabeledData
    }

    fn make_clojure(
        &mut self,
        ident: u64,
        data: Option<BTreeMap<String, RTData>>,
    ) -> *const Clojure {
        let obj = Clojure {
            ident: ident,
            data: data,
        };
        self.clojure.push_back(obj);
        self.clojure.back().unwrap() as *const Clojure
    }
}

pub(crate) fn eval(code: &str, ctx: &semantics::Context) -> Result<LinkedList<String>, LispErr> {
    let mut ps = parser::Parser::new(code);
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
        let mut vars = Variables::new();
        match eval_expr(expr, lambda, ctx, &mut root, &mut vars) {
            Ok(val) => {
                result.push_back(val.get_in_lisp());
            }
            Err(e) => {
                let msg = format!(
                    "(RuntimeErr [{:?} (Pos {:?} {:?})])",
                    e.msg, e.pos.line, e.pos.column
                );
                result.push_back(msg);
                return Ok(result);
            }
        }
    }

    Ok(result)
}

fn get_data_of_id(id: &String, vars: &mut Variables) -> RTData {
    match vars.get(id) {
        Some(data) => data.clone(),
        None => RTData::Defun(id.to_string()),
    }
}

fn eval_expr(
    expr: &Expr,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut Variables,
) -> Result<RTData, RuntimeErr> {
    match expr {
        Expr::LitNum(e) => Ok(RTData::Int(e.num)),
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
    vars: &mut Variables,
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

    Ok(RTData::Lambda(root.make_clojure(expr.ident, data)))
}

fn eval_tuple(
    expr: &semantics::Exprs,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut Variables,
) -> Result<RTData, RuntimeErr> {
    let mut v = Vec::new();
    for e in expr.exprs.iter() {
        v.push(eval_expr(e, lambda, ctx, root, vars)?);
    }

    let elm = root.make_obj("Tuple".to_string(), Some(v));

    Ok(RTData::LData(elm))
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
            let msg = format!("{:?} is not defined", fun_name);
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

fn eval_apply(
    expr: &semantics::Apply,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut Variables,
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
                return eval_built_in(fun_name, v, expr.pos, ctx);
            }

            // look up defun
            let fun = get_fun(ctx, &fun_name, fun_expr)?;

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
                eval_tail_call(&fun.expr, lambda, ctx, root, &mut vars_fun)
            }
        }
        RTData::Lambda(f) => {
            // look up lambda
            let ident = unsafe { (*f).ident };
            let fun = get_lambda(ctx, lambda, ident, fun_expr)?;

            // set up arguments
            let mut vars_fun = Variables::new();
            for (e, arg) in iter.zip(fun.args.iter()) {
                let data = eval_expr(&e, lambda, ctx, root, vars)?;
                vars_fun.insert(arg.id.to_string(), data);
            }

            // set up free variables
            match unsafe { &(*f).data } {
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
                eval_tail_call(&fun.expr, lambda, ctx, root, &mut vars_fun)
            }
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
    vars: &mut Variables,
) -> Result<RTData, RuntimeErr> {
    let mut vs;
    let mut vars = vars;
    loop {
        match eval_expr(expr, lambda, ctx, root, vars)? {
            RTData::TailCall(TCall::Defun(fun_name), vars_fun) => {
                let fun = get_fun(ctx, &fun_name, expr)?;
                vs = vars_fun;
                expr = &fun.expr;
                vars = &mut vs;
            }
            RTData::TailCall(TCall::Lambda(id), vars_fun) => {
                let fun = get_lambda(ctx, lambda, id, expr)?;
                vs = vars_fun;
                expr = &fun.expr;
                vars = &mut vs;
            }
            x => {
                return Ok(x);
            }
        }
    }
}

fn get_int_int(args: Vec<RTData>, pos: Pos) -> Result<(i64, i64), RuntimeErr> {
    match (args[0].clone(), args[1].clone()) {
        (RTData::Int(n1), RTData::Int(n2)) => Ok((n1, n2)),
        _ => Err(RuntimeErr {
            msg: "there must be exactly 2 integers".to_string(),
            pos: pos,
        }),
    }
}

fn get_int_int_int(args: Vec<RTData>, pos: Pos) -> Result<(i64, i64, i64), RuntimeErr> {
    match (args[0].clone(), args[1].clone(), args[2].clone()) {
        (RTData::Int(n1), RTData::Int(n2), RTData::Int(n3)) => Ok((n1, n2, n3)),
        _ => Err(RuntimeErr {
            msg: "there must be exactly 2 integers".to_string(),
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
            msg: "there must be exactly 2 boolean values".to_string(),
            pos: pos,
        }),
    }
}

fn eval_built_in(
    fun_name: String,
    args: Vec<RTData>,
    pos: Pos,
    ctx: &semantics::Context,
) -> Result<RTData, RuntimeErr> {
    match fun_name.as_str() {
        "+" => {
            let (n1, n2) = get_int_int(args, pos)?;
            Ok(RTData::Int(n1 + n2))
        }
        "-" => {
            let (n1, n2) = get_int_int(args, pos)?;
            Ok(RTData::Int(n1 - n2))
        }
        "*" => {
            let (n1, n2) = get_int_int(args, pos)?;
            Ok(RTData::Int(n1 * n2))
        }
        "/" => {
            let (n1, n2) = get_int_int(args, pos)?;
            Ok(RTData::Int(n1 / n2))
        }
        "<" => {
            let (n1, n2) = get_int_int(args, pos)?;
            Ok(RTData::Bool(n1 < n2))
        }
        ">" => {
            let (n1, n2) = get_int_int(args, pos)?;
            Ok(RTData::Bool(n1 > n2))
        }
        "=" => {
            let (n1, n2) = get_int_int(args, pos)?;
            Ok(RTData::Bool(n1 == n2))
        }
        "<=" => {
            let (n1, n2) = get_int_int(args, pos)?;
            Ok(RTData::Bool(n1 <= n2))
        }
        ">=" => {
            let (n1, n2) = get_int_int(args, pos)?;
            Ok(RTData::Bool(n1 >= n2))
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
            Ok(RTData::Int((ctx.callback)(n1, n2, n3)))
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
    vars: &mut Variables,
) -> Result<RTData, RuntimeErr> {
    let data = eval_expr(&expr.expr, lambda, ctx, root, vars)?;

    for c in &expr.cases {
        vars.push();
        if eval_pat(&c.pattern, data.clone(), vars) {
            let retval = eval_expr(&c.expr, lambda, ctx, root, vars)?;
            vars.pop();
            return Ok(retval);
        }
        vars.pop();
    }

    let pos = expr.pos;
    Err(RuntimeErr {
        msg: "pattern-matching is not exhaustive".to_string(),
        pos: pos,
    })
}

fn eval_id(expr: &semantics::IDNode, vars: &mut Variables) -> Result<RTData, RuntimeErr> {
    let id = expr.id.to_string();
    Ok(get_data_of_id(&id, vars))
}

fn eval_list(
    expr: &semantics::Exprs,
    lambda: &BTreeMap<u64, semantics::Lambda>,
    ctx: &semantics::Context,
    root: &mut RootObject,
    vars: &mut Variables,
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
    vars: &mut Variables,
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
    vars: &mut Variables,
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
    vars: &mut Variables,
) -> Result<RTData, RuntimeErr> {
    vars.push();

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
    vars.pop();

    Ok(result)
}

fn eval_pat(pat: &Pattern, data: RTData, vars: &mut Variables) -> bool {
    match pat {
        Pattern::PatID(p) => {
            vars.insert(p.id.to_string(), data);
            true
        }
        Pattern::PatNum(p) => match data {
            RTData::Int(n) => n == p.num,
            _ => false,
        },
        Pattern::PatBool(p) => match data {
            RTData::Bool(n) => n == p.val,
            _ => false,
        },
        Pattern::PatNil(_) => match data {
            RTData::LData(ptr) => unsafe { (*ptr).label == "Nil" },
            _ => false,
        },
        Pattern::PatTuple(p) => match data {
            RTData::LData(ptr) => {
                if unsafe { (*ptr).label == "Tuple" } {
                    return false;
                }

                match unsafe { &(*ptr).data } {
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
                if unsafe { (*ptr).label != p.label.id } {
                    return false;
                }

                match unsafe { &(*ptr).data } {
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
