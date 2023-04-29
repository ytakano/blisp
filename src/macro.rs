use crate::{parser::Expr, Pos};
use alloc::{
    collections::{btree_map::Entry, BTreeMap, LinkedList},
    string::String,
};

#[derive(Debug)]
pub struct MacroErr {
    pub pos: Pos,
    pub msg: &'static str,
}

pub fn expand() -> usize {
    todo!()
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

pub(crate) fn process_macros(exprs: &mut LinkedList<Expr>) -> Result<(), MacroErr> {
    find_macros(exprs)?;

    Ok(())
}

type Macros = BTreeMap<String, LinkedList<MacroRule>>;

struct MacroRule {
    pattern: Expr,
    template: Expr,
}

fn find_macros(exprs: &LinkedList<Expr>) -> Result<Macros, MacroErr> {
    let mut result = BTreeMap::new();

    for e in exprs.iter() {
        if let Expr::Apply(es, _) = e {
            let mut it = es.iter();

            let Some(front) = it.next() else { continue; };

            if let Expr::ID(id_macro, _) = front {
                if id_macro == "macro" {
                    let id = it.next();
                    let Some(Expr::ID(id, _)) = id else {
                        return Err(MacroErr { pos: e.get_pos(), msg: "invalid macro" });
                    };

                    let mut rules = LinkedList::new();
                    for rule in it {
                        let Expr::Apply(rule_exprs, _) = rule else {
                            return Err(MacroErr { pos: rule.get_pos(), msg: "invalid macro rule" });
                        };

                        if rule_exprs.len() != 2 {
                            return Err(MacroErr {
                                pos: rule.get_pos(),
                                msg: "the number of argument of macro rule is not 2",
                            });
                        }

                        let mut rule_it = rule_exprs.iter();
                        let pattern = rule_it.next().unwrap().clone();
                        let template = rule_it.next().unwrap().clone();

                        rules.push_back(MacroRule { pattern, template });
                    }

                    result.insert(id.clone(), rules);
                }
            }
        }
    }

    Ok(result)
}
