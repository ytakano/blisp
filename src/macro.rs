use crate::parser::Expr;
use alloc::{
    collections::{btree_map::Entry, BTreeMap, LinkedList},
    string::String,
};

pub fn expand() {
    // TODO
}

/// `e1` is a pattern and `e2` is an expression to be matched.
pub fn match_pattern(e1: &Expr, e2: &Expr, ctx: &mut BTreeMap<String, LinkedList<Expr>>) -> bool {
    match (e1, e2) {
        (Expr::ID(left, _), _) => {
            if let Some('$') = left.chars().nth(0) {
                // If `e1` is `$id`, then a map from `$id` to `e1` is added to `ctx`.
                let entry = ctx.entry(left.clone());
                match entry {
                    Entry::Vacant(ent) => {
                        let mut list = LinkedList::new();
                        list.push_back(e2.clone());
                        ent.insert(list);
                        true
                    }
                    Entry::Occupied(ent) => {
                        let exprs = ent.get();
                        if exprs.len() != 1 {
                            false
                        } else {
                            eq_expr(exprs.front().unwrap(), e2)
                        }
                    }
                }
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
    if left.len() <= right.len() {
        if let Some(Expr::ID(dots, _)) = left.back() {
            if dots == "..." {
                let mut it_left = left.iter();
                let mut it_right = right.iter();

                for _ in 0..left.len() - 1 {
                    let e1 = it_left.next().unwrap();
                    let e2 = it_right.next().unwrap();
                    if !match_pattern(e1, e2, ctx) {
                        return false;
                    }
                }

                let mut rev = left.iter().rev();
                let Some(_) = rev.next() else { return false };
                let Some(back_next) = rev.next() else { return false };

                if let Expr::ID(id, _) = back_next {
                    if let Some('$') = id.chars().nth(0) {
                        let Some(exprs) = ctx.get_mut(id) else { return false; };
                        for expr in it_right {
                            exprs.push_back(expr.clone());
                        }

                        return true;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }

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
    } else {
        false
    }
}

fn eq_expr(e1: &Expr, e2: &Expr) -> bool {
    match (e1, e2) {
        (Expr::ID(left, _), Expr::ID(right, _)) => left == right,
        (Expr::Bool(left, _), Expr::Bool(right, _)) => left == right,
        (Expr::Char(left, _), Expr::Char(right, _)) => left == right,
        (Expr::Num(left, _), Expr::Num(right, _)) => left == right,
        (Expr::Str(left, _), Expr::Str(right, _)) => left == right,
        (Expr::Tuple(left, _), Expr::Tuple(right, _)) => eq_exprs(left, right),
        (Expr::Apply(left, _), Expr::Apply(right, _)) => eq_exprs(left, right),
        (Expr::List(left, _), Expr::List(right, _)) => eq_exprs(left, right),
        _ => false,
    }
}

fn eq_exprs(es1: &LinkedList<Expr>, es2: &LinkedList<Expr>) -> bool {
    if es1.len() != es2.len() {
        return false;
    }

    es1.iter().zip(es2.iter()).all(|(e1, e2)| eq_expr(e1, e2))
}
