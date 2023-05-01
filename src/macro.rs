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

/// `e1` is a pattern and `e2` is an expression to be matched.
pub fn match_pattern(e1: &Expr, e2: &Expr, ctx: &mut BTreeMap<String, LinkedList<Expr>>) -> bool {
    match (e1, e2) {
        (Expr::ID(left, _), _) => {
            if let Some('$') = left.chars().next() {
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
                matches!(e2, Expr::ID(right, _) if left == right)
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
    let mut prev = None;
    let mut it_left = left.iter();
    let mut it_right = right.iter();

    loop {
        match (it_left.next(), it_right.next()) {
            (Some(e1), Some(e2)) => {
                if let Expr::ID(id, _) = e1 {
                    if id == "..." {
                        if let Some(key) = &prev {
                            let Some(exprs) = ctx.get_mut(key) else { return false; };
                            exprs.push_back(e2.clone());
                            break;
                        }
                    } else {
                        prev = Some(id.clone());
                    }
                }

                if !match_pattern(e1, e2, ctx) {
                    return false;
                }
            }
            (Some(e1), None) => {
                if let Expr::ID(id, _) = e1 {
                    return id == "...";
                } else {
                    return false;
                }
            }
            (None, Some(_)) => return false,
            _ => return true,
        }
    }

    let key = prev.unwrap();
    let exprs = ctx.get_mut(&key).unwrap();
    for expr in it_right {
        exprs.push_back(expr.clone());
    }

    true
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

pub(crate) fn process_macros(exprs: &mut LinkedList<Expr>) -> Result<Macros, MacroErr> {
    let macros = parse_macros(exprs)?;

    #[cfg(test)]
    for (key, value) in macros.iter() {
        println!("{key}:");
        for rule in value {
            println!("pattern: {}", rule.pattern);
            println!("template: {}", rule.template);
        }
    }

    for expr in exprs.iter_mut() {
        apply_macros(&macros, expr)?;
    }

    Ok(macros)
}

pub(crate) fn apply(expr: &mut Expr, macros: &Macros) -> Result<(), MacroErr> {
    apply_macros(macros, expr)
}

fn apply_macros(macros: &Macros, expr: &mut Expr) -> Result<(), MacroErr> {
    if let Expr::Apply(exprs, _) = expr {
        if let Some(Expr::ID(id, _)) = exprs.front() {
            if id == "macro" {
                return Ok(());
            }
        }
    }

    apply_macros_recursively(macros, expr, 0)
}

fn apply_macros_expr(
    pos: Pos,
    macros: &Macros,
    expr: &Expr,
    count: u8,
) -> Result<Option<Expr>, MacroErr> {
    if count == 0xff {
        return Err(MacroErr {
            pos,
            msg: "too deep macro",
        });
    }

    for (_, rules) in macros.iter() {
        let mut ctx = BTreeMap::new();

        for rule in rules.iter() {
            if match_pattern(&rule.pattern, expr, &mut ctx) {
                let expr = expand(pos, &rule.template, &ctx).pop_front().unwrap();

                if let Some(e) = apply_macros_expr(pos, macros, &expr, count + 1)? {
                    return Ok(Some(e));
                } else {
                    return Ok(Some(expr));
                }
            }
        }
    }

    Ok(None)
}

fn apply_macros_recursively(macros: &Macros, expr: &mut Expr, count: u8) -> Result<(), MacroErr> {
    if count == 0xff {
        panic!("{}: too deep macro", expr.get_pos());
    }

    if let Some(e) = apply_macros_expr(expr.get_pos(), macros, expr, count)? {
        *expr = e;
    }

    match expr {
        Expr::Apply(exprs, _) | Expr::List(exprs, _) | Expr::Tuple(exprs, _) => {
            for expr in exprs {
                apply_macros_recursively(macros, expr, count + 1)?;
            }
        }
        _ => (),
    }

    Ok(())
}

pub(crate) type Macros = BTreeMap<String, LinkedList<MacroRule>>;

#[derive(Debug)]
pub(crate) struct MacroRule {
    pattern: Expr,
    template: Expr,
}

fn parse_macros(exprs: &LinkedList<Expr>) -> Result<Macros, MacroErr> {
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
                                msg: "the number of arguments of a macro rule is not 2",
                            });
                        }

                        let mut rule_it = rule_exprs.iter();

                        let mut pattern = rule_it.next().unwrap().clone();
                        if let Expr::Apply(arguments, _) = &mut pattern {
                            if let Some(Expr::ID(front, _)) = arguments.front_mut() {
                                #[cfg(test)]
                                println!("front = {front}");

                                if front == "_" {
                                    *front = id.clone();
                                } else if front != id {
                                    return Err(MacroErr {
                                        pos: pattern.get_pos(),
                                        msg: "invalid macro pattern",
                                    });
                                }
                            }

                            #[cfg(test)]
                            println!("pattern = {pattern}");

                            let template = rule_it.next().unwrap().clone();

                            rules.push_back(MacroRule { pattern, template });
                        } else {
                            return Err(MacroErr {
                                pos: pattern.get_pos(),
                                msg: "invalid macro pattern",
                            });
                        };
                    }

                    if let Entry::Vacant(entry) = result.entry(id.clone()) {
                        entry.insert(rules);
                    } else {
                        return Err(MacroErr {
                            pos: e.get_pos(),
                            msg: "multiply defined",
                        });
                    }
                }
            }
        }
    }

    Ok(result)
}

fn expand(pos: Pos, template: &Expr, ctx: &BTreeMap<String, LinkedList<Expr>>) -> LinkedList<Expr> {
    match template {
        Expr::ID(id, _) => {
            if let Some(exprs) = ctx.get(id) {
                let expr = exprs.front().unwrap();
                let mut result = LinkedList::new();
                result.push_back(expr.clone());
                result
            } else {
                let mut result: LinkedList<Expr> = LinkedList::new();
                result.push_back(template.clone());
                result
            }
        }
        Expr::Apply(templates, _) => {
            let exprs = expand_list(pos, templates, ctx);
            let mut result = LinkedList::new();

            // TODO: rename variables

            result.push_back(Expr::Apply(exprs, pos));
            result
        }
        Expr::List(templates, _) => {
            let exprs = expand_list(pos, templates, ctx);
            let mut result = LinkedList::new();
            result.push_back(Expr::List(exprs, pos));
            result
        }
        Expr::Tuple(templates, _) => {
            let exprs = expand_list(pos, templates, ctx);
            let mut result = LinkedList::new();
            result.push_back(Expr::Tuple(exprs, pos));
            result
        }
        expr => {
            let mut result = LinkedList::new();
            result.push_back(expr.clone());
            result
        }
    }
}

fn expand_list(
    pos: Pos,
    templates: &LinkedList<Expr>,
    ctx: &BTreeMap<String, LinkedList<Expr>>,
) -> LinkedList<Expr> {
    let mut result = LinkedList::new();

    let mut prev = None;

    for template in templates {
        if let Expr::ID(id, _) = template {
            if id == "..." {
                if let Some(p) = &prev {
                    if let Some(exprs) = ctx.get(p) {
                        let mut it = exprs.iter();
                        let _ = it.next();

                        for expr in it {
                            result.push_back(expr.clone());
                        }
                    } else {
                        prev = None;
                    }
                } else {
                    prev = None;
                }

                continue;
            } else {
                prev = Some(id.clone());
            }
        } else {
            prev = None;
        }

        let mut exprs = expand(pos, template, ctx);
        result.append(&mut exprs);
    }

    result
}
