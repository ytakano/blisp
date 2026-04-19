use crate::{parser::Expr, Pos};
use alloc::{
    collections::{btree_map::Entry, BTreeMap, LinkedList},
    format,
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
            } else if left == "_" {
                true
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
                            let Some(exprs) = ctx.get_mut(key) else {
                                return false;
                            };
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
    let mut expander = MacroExpander::new(&macros);

    for expr in exprs.iter_mut() {
        expander.apply_macros(expr)?;
    }

    Ok(macros)
}

pub(crate) fn apply(expr: &mut Expr, macros: &Macros) -> Result<(), MacroErr> {
    MacroExpander::new(macros).apply_macros(expr)
}

struct MacroExpander<'a> {
    macros: &'a Macros,
    fresh_counter: u64,
}

impl<'a> MacroExpander<'a> {
    fn new(macros: &'a Macros) -> MacroExpander<'a> {
        MacroExpander {
            macros,
            fresh_counter: 0,
        }
    }

    fn apply_macros(&mut self, expr: &mut Expr) -> Result<(), MacroErr> {
        if let Expr::Apply(exprs, _) = expr {
            if let Some(Expr::ID(id, _)) = exprs.front() {
                if id == "macro" {
                    return Ok(());
                }
            }
        }

        self.apply_macros_recursively(expr, 0)
    }

    fn apply_macros_expr(
        &mut self,
        pos: Pos,
        expr: &Expr,
        count: u8,
    ) -> Result<Option<Expr>, MacroErr> {
        if count == 0xff {
            return Err(MacroErr {
                pos,
                msg: "too deep macro",
            });
        }

        for (_, rules) in self.macros.iter() {
            for rule in rules.iter() {
                let mut ctx = BTreeMap::new();
                if match_pattern(&rule.pattern, expr, &mut ctx) {
                    let expr = self.expand(pos, &rule.template, &ctx).pop_front().unwrap();

                    if let Some(e) = self.apply_macros_expr(pos, &expr, count + 1)? {
                        return Ok(Some(e));
                    } else {
                        return Ok(Some(expr));
                    }
                }
            }
        }

        Ok(None)
    }

    fn apply_macros_recursively(&mut self, expr: &mut Expr, count: u8) -> Result<(), MacroErr> {
        if count == 0xff {
            panic!("{}: too deep macro", expr.get_pos());
        }

        if let Some(e) = self.apply_macros_expr(expr.get_pos(), expr, count)? {
            *expr = e;
        }

        match expr {
            Expr::Apply(exprs, _) | Expr::List(exprs, _) | Expr::Tuple(exprs, _) => {
                for expr in exprs {
                    self.apply_macros_recursively(expr, count + 1)?;
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn expand(
        &mut self,
        pos: Pos,
        template: &Expr,
        ctx: &BTreeMap<String, LinkedList<Expr>>,
    ) -> LinkedList<Expr> {
        let template = self.freshen_template(template);
        expand_expr(pos, &template, ctx)
    }

    fn freshen_template(&mut self, template: &Expr) -> Expr {
        let mut env = BTreeMap::new();
        self.rename_expr(template, &mut env)
    }

    fn rename_expr(&mut self, expr: &Expr, env: &mut BTreeMap<String, String>) -> Expr {
        match expr {
            Expr::ID(id, pos) => {
                if let Some(renamed) = env.get(id) {
                    Expr::ID(renamed.clone(), *pos)
                } else {
                    expr.clone()
                }
            }
            Expr::Bool(_, _) | Expr::Char(_, _) | Expr::Num(_, _) | Expr::Str(_, _) => expr.clone(),
            Expr::List(exprs, pos) => Expr::List(self.rename_exprs(exprs, env), *pos),
            Expr::Tuple(exprs, pos) => Expr::Tuple(self.rename_exprs(exprs, env), *pos),
            Expr::Apply(exprs, pos) => {
                let Some(head) = exprs.front() else {
                    return expr.clone();
                };

                match head {
                    Expr::ID(id, _) if id == "lambda" => self.rename_lambda(exprs, *pos, env),
                    Expr::ID(id, _) if id == "let" => self.rename_let(exprs, *pos, env),
                    Expr::ID(id, _) if id == "match" => self.rename_match(exprs, *pos, env),
                    _ => Expr::Apply(self.rename_exprs(exprs, env), *pos),
                }
            }
        }
    }

    fn rename_exprs(
        &mut self,
        exprs: &LinkedList<Expr>,
        env: &mut BTreeMap<String, String>,
    ) -> LinkedList<Expr> {
        let mut result = LinkedList::new();

        for expr in exprs {
            result.push_back(self.rename_expr(expr, env));
        }

        result
    }

    fn rename_lambda(
        &mut self,
        exprs: &LinkedList<Expr>,
        pos: Pos,
        env: &mut BTreeMap<String, String>,
    ) -> Expr {
        let mut result = LinkedList::new();
        let mut iter = exprs.iter();

        result.push_back(iter.next().unwrap().clone());

        if let Some(args) = iter.next() {
            let mut local_env = env.clone();
            result.push_back(self.rename_lambda_args(args, &mut local_env));

            for expr in iter {
                result.push_back(self.rename_expr(expr, &mut local_env));
            }
        }

        Expr::Apply(result, pos)
    }

    fn rename_lambda_args(
        &mut self,
        args: &Expr,
        env: &mut BTreeMap<String, String>,
    ) -> Expr {
        match args {
            Expr::Apply(exprs, pos) => {
                let mut renamed = LinkedList::new();
                for expr in exprs {
                    renamed.push_back(self.rename_binder(expr, env));
                }
                Expr::Apply(renamed, *pos)
            }
            _ => self.rename_expr(args, env),
        }
    }

    fn rename_let(
        &mut self,
        exprs: &LinkedList<Expr>,
        pos: Pos,
        env: &mut BTreeMap<String, String>,
    ) -> Expr {
        let mut result = LinkedList::new();
        let mut iter = exprs.iter();

        result.push_back(iter.next().unwrap().clone());

        let mut body_env = env.clone();
        if let Some(bindings) = iter.next() {
            result.push_back(self.rename_let_bindings(bindings, env, &mut body_env));
        }

        for expr in iter {
            result.push_back(self.rename_expr(expr, &mut body_env));
        }

        Expr::Apply(result, pos)
    }

    fn rename_let_bindings(
        &mut self,
        bindings: &Expr,
        env: &mut BTreeMap<String, String>,
        body_env: &mut BTreeMap<String, String>,
    ) -> Expr {
        match bindings {
            Expr::Apply(defs, pos) => {
                let mut renamed_defs = LinkedList::new();

                for def in defs {
                    match def {
                        Expr::Apply(def_exprs, def_pos) if def_exprs.len() == 2 => {
                            let mut def_iter = def_exprs.iter();
                            let pattern = def_iter.next().unwrap();
                            let value = def_iter.next().unwrap();

                            let value = self.rename_expr(value, env);
                            let pattern = self.rename_pattern(pattern, body_env);

                            let mut renamed_def = LinkedList::new();
                            renamed_def.push_back(pattern);
                            renamed_def.push_back(value);
                            renamed_defs.push_back(Expr::Apply(renamed_def, *def_pos));
                        }
                        _ => renamed_defs.push_back(self.rename_expr(def, env)),
                    }
                }

                Expr::Apply(renamed_defs, *pos)
            }
            _ => self.rename_expr(bindings, env),
        }
    }

    fn rename_match(
        &mut self,
        exprs: &LinkedList<Expr>,
        pos: Pos,
        env: &mut BTreeMap<String, String>,
    ) -> Expr {
        let mut result = LinkedList::new();
        let mut iter = exprs.iter();

        result.push_back(iter.next().unwrap().clone());

        if let Some(cond) = iter.next() {
            result.push_back(self.rename_expr(cond, env));
        }

        for case in iter {
            result.push_back(self.rename_match_case(case, env));
        }

        Expr::Apply(result, pos)
    }

    fn rename_match_case(&mut self, case: &Expr, env: &mut BTreeMap<String, String>) -> Expr {
        match case {
            Expr::Apply(case_exprs, pos) if case_exprs.len() == 2 => {
                let mut iter = case_exprs.iter();
                let pattern = iter.next().unwrap();
                let body = iter.next().unwrap();

                let mut case_env = env.clone();
                let pattern = self.rename_pattern(pattern, &mut case_env);
                let body = self.rename_expr(body, &mut case_env);

                let mut renamed_case = LinkedList::new();
                renamed_case.push_back(pattern);
                renamed_case.push_back(body);
                Expr::Apply(renamed_case, *pos)
            }
            _ => self.rename_expr(case, env),
        }
    }

    fn rename_pattern(&mut self, pattern: &Expr, env: &mut BTreeMap<String, String>) -> Expr {
        match pattern {
            Expr::ID(_, _) => self.rename_binder(pattern, env),
            Expr::Tuple(exprs, pos) => {
                let mut renamed = LinkedList::new();
                for expr in exprs {
                    renamed.push_back(self.rename_pattern(expr, env));
                }
                Expr::Tuple(renamed, *pos)
            }
            Expr::Apply(exprs, pos) => {
                let mut renamed = LinkedList::new();
                let mut iter = exprs.iter();

                if let Some(head) = iter.next() {
                    renamed.push_back(self.rename_expr(head, env));
                }

                for expr in iter {
                    renamed.push_back(self.rename_pattern(expr, env));
                }

                Expr::Apply(renamed, *pos)
            }
            _ => pattern.clone(),
        }
    }

    fn rename_binder(&mut self, expr: &Expr, env: &mut BTreeMap<String, String>) -> Expr {
        match expr {
            Expr::ID(id, pos) => {
                if id == "_" || is_pattern_var(id) {
                    expr.clone()
                } else {
                    let fresh = self.fresh_name(id);
                    env.insert(id.clone(), fresh.clone());
                    Expr::ID(fresh, *pos)
                }
            }
            _ => self.rename_expr(expr, env),
        }
    }

    fn fresh_name(&mut self, id: &str) -> String {
        let fresh = format!("__blisp_macro_{}_{}", self.fresh_counter, id);
        self.fresh_counter += 1;
        fresh
    }
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

            let Some(front) = it.next() else {
                continue;
            };

            if let Expr::ID(id_macro, _) = front {
                if id_macro == "macro" {
                    let id = it.next();
                    let Some(Expr::ID(id, _)) = id else {
                        return Err(MacroErr {
                            pos: e.get_pos(),
                            msg: "invalid macro",
                        });
                    };

                    let mut rules = LinkedList::new();
                    for rule in it {
                        let Expr::Apply(rule_exprs, _) = rule else {
                            return Err(MacroErr {
                                pos: rule.get_pos(),
                                msg: "invalid macro rule",
                            });
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
                                if front == "_" {
                                    *front = id.clone();
                                } else if front != id {
                                    return Err(MacroErr {
                                        pos: pattern.get_pos(),
                                        msg: "invalid macro pattern",
                                    });
                                }
                            }

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

fn is_pattern_var(id: &str) -> bool {
    matches!(id.chars().next(), Some('$'))
}

fn expand_expr(pos: Pos, template: &Expr, ctx: &BTreeMap<String, LinkedList<Expr>>) -> LinkedList<Expr> {
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

        let mut exprs = expand_expr(pos, template, ctx);
        result.append(&mut exprs);
    }

    result
}
