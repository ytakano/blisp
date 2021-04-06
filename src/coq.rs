use super::semantics as S;

use alloc::string::{String, ToString};

pub(crate) fn to_coq_type(expr: &S::TypeExpr, depth: usize) -> String {
    match expr {
        S::TypeExpr::TEBool(_) => "bool".to_string(),
        S::TypeExpr::TEInt(_) => "Z".to_string(),
        S::TypeExpr::TEString(_) => "string".to_string(),
        S::TypeExpr::TEChar(_) => "ascii".to_string(),
        S::TypeExpr::TEList(e) => {
            if depth == 0 {
                format!("list {}", to_coq_type(&e.ty, depth + 1))
            } else {
                format!("(list {})", to_coq_type(&e.ty, depth + 1))
            }
        }
        _ => panic!("not yet implemented")
    }
}