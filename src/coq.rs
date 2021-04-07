use super::semantics as S;

use alloc::string::{String, ToString};
use alloc::vec::Vec;

pub(crate) fn to_coq_type(expr: &S::TypeExpr, depth: usize) -> String {
    match expr {
        S::TypeExpr::TEBool(_) => "bool".to_string(),
        S::TypeExpr::TEInt(_) => "Z".to_string(),
        S::TypeExpr::TEString(_) => "string".to_string(),
        S::TypeExpr::TEChar(_) => "ascii".to_string(),
        S::TypeExpr::TEID(e) => e.id.clone(),
        S::TypeExpr::TETuple(e) => {
            if e.ty.len() == 0 {
                return "unit".to_string();
            }

            let mut i = 0;
            let mut s = "".to_string();

            for t in e.ty.iter() {
                i += 1;
                if i == e.ty.len() {
                    s = format!("{}{}", s, to_coq_type(t, depth + 1));
                } else {
                    s = format!("{}{} * ", s, to_coq_type(t, depth + 1));
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
                format!("list {}", to_coq_type(&e.ty, depth + 1))
            } else {
                format!("(list {})", to_coq_type(&e.ty, depth + 1))
            }
        }
        S::TypeExpr::TEData(e) => {
            if e.type_args.len() == 0 {
                e.id.id.clone()
            } else {
                let mut args = "".to_string();
                for arg in e.type_args.iter() {
                    args = format!("{}{}", args, to_coq_type(arg, depth + 1));
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
                    s = format!("{}", to_coq_type(arg, depth + 1));
                } else {
                    s = format!("{} -> {}", s, to_coq_type(arg, depth + 1));
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
        if expr.types.len() == i + 1 {
            mem = format!("{}(x{}: {})", mem, i, to_coq_type(t, 0));
        } else {
            mem = format!("{}(x{}: {}) ", mem, i, to_coq_type(t, 0));
        }
        i += 1;
    }

    if expr.types.len() > 0 {
        format!("| {} {}", expr.id.id, mem)
    } else {
        format!("| {}", expr.id.id)
    }
}

pub(crate) fn to_coq_func(expr: &S::Defun) -> String {
    let head;
    if is_recursive(expr) {
        head = format!("Fixpoint {}", expr.id.id);
    } else {
        head = format!("Definition {}", expr.id.id);
    }

    let fun_type =
    if let S::TypeExpr::TEFun(e) = &expr.fun_type {
        e
    } else {
        return "".to_string();
    };

    let mut args = "".to_string();
    for (arg, t) in expr.args.iter().zip(fun_type.args.iter()) {
        args = format!("{} ({}: {})", args, arg.id, to_coq_type(t, 0));
    }

    let ret = to_coq_type(&fun_type.ret, 0);

    format!("{}{}: {} :=\n", head, args, ret)
}

fn is_recursive(expr: &S::Defun) -> bool {
    is_recursive_expr(&expr.expr, &expr.id.id)
}

fn is_recursive_expr(expr: &S::LangExpr, id: &String) -> bool {
    match expr {
        S::LangExpr::IfExpr(e) => {
            is_recursive_expr(&e.cond_expr, id) ||
            is_recursive_expr(&e.then_expr, id) ||
            is_recursive_expr(&e.else_expr, id)
        },
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
        },
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
