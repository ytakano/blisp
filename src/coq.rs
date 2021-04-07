use super::semantics as S;

use alloc::string::{String, ToString};

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
        _ => panic!("not yet implemented"),
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
