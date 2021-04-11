use super::semantics as S;

use alloc::collections::LinkedList;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

pub(crate) fn to_coq_type(
    expr: &S::TypeExpr,
    depth: usize,
    targs: &mut LinkedList<String>,
) -> String {
    match expr {
        S::TypeExpr::TEBool(_) => "bool".to_string(),
        S::TypeExpr::TEInt(_) => "Z".to_string(),
        S::TypeExpr::TEString(_) => "string".to_string(),
        S::TypeExpr::TEChar(_) => "ascii".to_string(),
        S::TypeExpr::TEID(e) => {
            if let Some(c) = e.id.chars().next() {
                if ('a'..='z').contains(&c) {
                    let mut flag = false;
                    for s in targs.iter() {
                        if *s == e.id {
                            flag = true;
                        }
                    }

                    if !flag {
                        targs.push_back(e.id.clone());
                    }
                }
            }
            e.id.clone()
        }
        S::TypeExpr::TETuple(e) => {
            if e.ty.len() == 0 {
                return "unit".to_string();
            }

            let mut i = 0;
            let mut s = "".to_string();

            for t in e.ty.iter() {
                i += 1;
                if i == e.ty.len() {
                    s = format!("{}{}", s, to_coq_type(t, depth + 1, targs));
                } else {
                    s = format!("{}{} * ", s, to_coq_type(t, depth + 1, targs));
                }
            }

            if depth > 0 {
                format!("({})", s)
            } else {
                s
            }
        }
        S::TypeExpr::TEList(e) => {
            if depth == 0 {
                format!("list {}", to_coq_type(&e.ty, depth + 1, targs))
            } else {
                format!("(list {})", to_coq_type(&e.ty, depth + 1, targs))
            }
        }
        S::TypeExpr::TEData(e) => {
            if e.type_args.len() == 0 {
                e.id.id.clone()
            } else {
                let mut args = "".to_string();
                for arg in e.type_args.iter() {
                    args = format!("{}{}", args, to_coq_type(arg, depth + 1, targs));
                }

                if depth == 0 {
                    format!("{} {}", e.id.id, args)
                } else {
                    format!("({} {})", e.id.id, args)
                }
            }
        }
        S::TypeExpr::TEFun(e) => {
            let mut s = "".to_string();

            let mut i = 0;
            for arg in e.args.iter() {
                if i == 0 {
                    s = format!("{}", to_coq_type(arg, depth + 1, targs));
                } else {
                    s = format!("{} -> {}", s, to_coq_type(arg, depth + 1, targs));
                }
                i += 1;
            }

            if depth > 0 {
                format!("({})", s)
            } else {
                s
            }
        }
    }
}

pub(crate) fn import() -> &'static str {
    "Require Import ZArith."
}

pub(crate) fn to_coq_data(expr: &S::DataType) -> String {
    let mut mem = "".to_string();
    let mut i = 0;
    for d in expr.members.iter() {
        i += 1;
        if i == expr.members.len() {
            mem = format!("{}{}.\n", mem, to_coq_data_mem(d));
        } else {
            mem = format!("{}{}\n", mem, to_coq_data_mem(d));
        }
    }

    format!("Inductive {}\n{}", to_coq_data_def(&expr.name), mem)
}

fn to_coq_data_def(expr: &S::DataTypeName) -> String {
    let mut args = "(".to_string();
    let mut i = 0;
    for t in expr.type_args.iter() {
        i += 1;
        if expr.type_args.len() == i {
            args = format!("{}{}: Type)", args, t.id);
        } else {
            args = format!("{}{} ", args, t.id);
        }
    }

    if expr.type_args.len() > 0 {
        format!("{} {}: Type :=", expr.id.id, args)
    } else {
        format!("{}: Type :=", expr.id.id)
    }
}

fn to_coq_data_mem(expr: &S::DataTypeMem) -> String {
    let mut i = 0;
    let mut mem = "".to_string();
    for t in expr.types.iter() {
        let mut targs = LinkedList::new();
        if expr.types.len() == i + 1 {
            mem = format!("{}(x{}: {})", mem, i, to_coq_type(t, 0, &mut targs));
        } else {
            mem = format!("{}(x{}: {}) ", mem, i, to_coq_type(t, 0, &mut targs));
        }
        i += 1;
    }

    if expr.types.len() > 0 {
        format!("| {} {}", expr.id.id, mem)
    } else {
        format!("| {}", expr.id.id)
    }
}

fn to_args_type(args: &LinkedList<String>, ty: &str) -> String {
    let mut s = "(".to_string();
    let mut i = 0;
    for a in args {
        i += 1;
        if i == args.len() {
            s = format!("{}{}: {})", s, a, ty);
        } else {
            s = format!("{}{} ", s, a);
        }
    }

    s
}

pub(crate) fn to_coq_func(expr: &S::Defun) -> String {
    let head;
    if is_recursive(expr) {
        head = format!("Fixpoint {}", expr.id.id);
    } else {
        head = format!("Definition {}", expr.id.id);
    }

    let fun_type = if let S::TypeExpr::TEFun(e) = &expr.fun_type {
        e
    } else {
        return "".to_string();
    };

    // transpile arguments
    // arguments whose types are same are aggregated
    let mut args = "".to_string();
    let mut targs = LinkedList::new();
    let mut args_list = LinkedList::new();
    let mut prev = "".to_string();
    for (arg, t) in expr.args.iter().zip(fun_type.args.iter()) {
        let ta = to_coq_type(t, 0, &mut targs);
        if prev == "" {
            prev = ta.clone();
        }

        if prev != ta {
            let s = to_args_type(&args_list, &prev);
            args = format!("{} {}", args, s);
            args_list.clear();
            args_list.push_back(arg.id.clone());
            prev = ta;
        } else {
            args_list.push_back(arg.id.clone());
        }
    }

    let s = to_args_type(&args_list, &prev);
    args = format!("{} {}", args, s);

    // transpile return type
    let ret = to_coq_type(&fun_type.ret, 0, &mut targs);

    // if there is no type argument, then return
    if targs.len() == 0 {
        return format!("{}{}: {} :=\n", head, args, ret);
    }

    // transpile type arguments
    let mut s_targs = "{".to_string();
    let mut i = 0;
    for targ in &targs {
        i += 1;
        if i == targs.len() {
            s_targs = format!("{}{}", s_targs, targ);
        } else {
            s_targs = format!("{}{} ", s_targs, targ);
        }
    }

    s_targs = format!("{}: Type}}", s_targs);

    format!("{} {}{}: {} :=\n", head, s_targs, args, ret)
}

fn is_recursive(expr: &S::Defun) -> bool {
    is_recursive_expr(&expr.expr, &expr.id.id)
}

fn is_recursive_expr(expr: &S::LangExpr, id: &String) -> bool {
    match expr {
        S::LangExpr::IfExpr(e) => {
            is_recursive_expr(&e.cond_expr, id)
                || is_recursive_expr(&e.then_expr, id)
                || is_recursive_expr(&e.else_expr, id)
        }
        S::LangExpr::LetExpr(e) => is_recursive_expr(&e.expr, id),
        S::LangExpr::LitStr(_) => false,
        S::LangExpr::LitChar(_) => false,
        S::LangExpr::LitNum(_) => false,
        S::LangExpr::LitBool(_) => false,
        S::LangExpr::IDExpr(e) => e.id == *id,
        S::LangExpr::DataExpr(e) => is_recursive_exprs(&e.exprs, id),
        S::LangExpr::MatchExpr(e) => {
            if is_recursive_expr(&e.expr, id) {
                return true;
            }

            for c in e.cases.iter() {
                if is_recursive_expr(&c.expr, id) {
                    return true;
                }
            }

            false
        }
        S::LangExpr::ApplyExpr(e) => is_recursive_exprs(&e.exprs, id),
        S::LangExpr::ListExpr(e) => is_recursive_exprs(&e.exprs, id),
        S::LangExpr::TupleExpr(e) => is_recursive_exprs(&e.exprs, id),
        S::LangExpr::LambdaExpr(e) => is_recursive_expr(&e.expr, id),
    }
}

fn is_recursive_exprs(exprs: &Vec<S::LangExpr>, id: &String) -> bool {
    for e in exprs.iter() {
        if is_recursive_expr(e, id) {
            return true;
        }
    }
    false
}
