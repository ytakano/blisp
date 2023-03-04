use crate::parser::Expr;
use alloc::{
    collections::{btree_map::Entry, BTreeMap, LinkedList},
    string::String,
};

pub fn expand() {
    // TODO
}

pub fn match_pattern(e1: &Expr, e2: &Expr, ctx: &mut BTreeMap<String, LinkedList<Expr>>) -> bool {
    match (e1, e2) {
        (Expr::ID(left, _), _) => {
            if let Some('$') = left.chars().nth(0) {
                let entry = ctx.entry(left.clone());
                match entry {
                    Entry::Vacant(ent) => {
                        let mut list = LinkedList::new();
                        list.push_back(e2.clone());
                        ent.insert(list);
                    }
                    Entry::Occupied(ent) => {
                        ent.into_mut().push_back(e2.clone());
                    }
                }

                true
            } else {
                match e2 {
                    Expr::ID(right, _) if left == right => true,
                    _ => false,
                }
            }
        }
        (Expr::Bool(left, _), Expr::Bool(right, _)) => left == right,
        (Expr::Char(left, _), Expr::Char(right, _)) => left == right,
        (Expr::Num(left, _), Expr::Num(right, _)) => left == right,
        (Expr::Str(left, _), Expr::Str(right, _)) => left == right,
        (Expr::Tuple(left, _), Expr::Tuple(right, _)) => match_list(left, right, ctx),
        (Expr::Apply(left, _), Expr::Apply(right, _)) => match_list(left, right, ctx),
        (Expr::List(left, _), Expr::List(right, _)) => match_list(left, right, ctx),
        _ => false,
    }
}

pub fn match_list(
    left: &LinkedList<Expr>,
    right: &LinkedList<Expr>,
    ctx: &mut BTreeMap<String, LinkedList<Expr>>,
) -> bool {
    if left.len() == right.len() {
        for (e1, e2) in left.iter().zip(right.iter()) {
            if !match_pattern(e1, e2, ctx) {
                return false;
            }
        }

        true
    } else {
        false
    }
}
