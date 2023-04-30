use super::semantics as S;
use alloc::{
    collections::LinkedList,
    format,
    string::{String, ToString},
    vec::Vec,
};

pub(crate) fn to_coq_type(
    expr: &S::TypeExpr,
    depth: usize,
    targs: &mut LinkedList<String>,
) -> String {
    match expr {
        S::TypeExpr::Bool(_) => "bool".to_string(),
        S::TypeExpr::Int(_) => "Z".to_string(),
        S::TypeExpr::String(_) => "string".to_string(),
        S::TypeExpr::Char(_) => "ascii".to_string(),
        S::TypeExpr::Id(e) => {
            if let Some(c) = e.id.chars().next() {
                if c.is_ascii_lowercase() {
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
        S::TypeExpr::Tuple(e) => {
            if e.ty.is_empty() {
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
        S::TypeExpr::List(e) => {
            if depth == 0 {
                format!("list {}", to_coq_type(&e.ty, depth + 1, targs))
            } else {
                format!("(list {})", to_coq_type(&e.ty, depth + 1, targs))
            }
        }
        S::TypeExpr::Data(e) => {
            if e.type_args.is_empty() {
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
        S::TypeExpr::Fun(e) => {
            let mut s = "".to_string();

            // ここがおかしいかも
            for (i, arg) in e.args.iter().enumerate() {
                if i == 0 {
                    s = to_coq_type(arg, depth + 1, targs);
                } else {
                    //s = format!("{} -> {}", s, to_coq_type(arg, depth + 1, targs));
                    s = format!("{} -> {}", s, to_coq_type(arg, depth + 1, targs));
                }
            }
            s = format!("{} -> {}", s, to_coq_type(&e.ret, depth + 1, targs));

            if depth > 0 {
                format!("({})", s)
            } else {
                s
            }
        }
    }
}

pub(crate) fn import() -> &'static str {
    "Require Import ZArith.
Require Import Coq.Lists.List."
    /*\n
    Inductive tuple5 (A, B, C, D, E:Type): Type :=
        | tup5 (x0: A, x1: B, x2: C, x3: D, x4: E).
    Inductive tuple4 (A, B, C, D:Type): Type :=
        | tup4 (x0: A, x1: B, x2: C, x3: D).
    Inductive tuple3 (A, B, C:Type): Type :=
        | tup3 (x0: A, x1: B, x2: C).
    Inductive tuple2 (A, B:Type): Type :=
        | tup2 (x0: A, x1: B).
    Inductive tuple1 (A:Type): Type :=
        | tup1 (x0: A).
    Inductive tuple0 : Type :=
        | tup0."*/
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

    if expr.members.is_empty() {
        format!("Inductive {}\n{}", to_coq_data_def(&expr.name), mem)
    } else {
        let extend = inductive_arguments(expr);
        format!(
            "Inductive {}\n{}{}\n",
            to_coq_data_def(&expr.name),
            mem,
            extend
        )
    }
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

    if !expr.type_args.is_empty() {
        format!("{} {}: Type :=", expr.id.id, args)
    } else {
        format!("{}: Type :=", expr.id.id)
    }
}

fn to_coq_data_mem(expr: &S::DataTypeMem) -> String {
    let mut mem = "".to_string();
    for (i, t) in expr.types.iter().enumerate() {
        let mut targs = LinkedList::new();
        if expr.types.len() == i + 1 {
            mem = format!("{}(x{}: {})", mem, i, to_coq_type(t, 0, &mut targs));
        } else {
            mem = format!("{}(x{}: {}) ", mem, i, to_coq_type(t, 0, &mut targs));
        }
    }

    if !expr.types.is_empty() {
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

fn inductive_arguments(expr: &S::DataType) -> String {
    let mut add_expr = "".to_string();
    for t in &expr.members {
        let mut temp = "".to_string();
        for (i, _id) in expr.name.type_args.iter().enumerate() {
            match i {
                0 => temp = format!("{}{}", temp, _id.id),
                _ => temp = format!("{} {}", temp, _id.id),
            }
        }
        add_expr = format!("{}\nArguments {}{{{}}}.", add_expr, t.id.id, temp);
    }
    add_expr
}

pub(crate) fn to_coq_func(expr: &S::Defun) -> String {
    let head = if is_recursive(expr) {
        format!("Fixpoint {}", expr.id.id)
    } else {
        format!("Definition {}", expr.id.id)
    };

    let fun_type = if let S::TypeExpr::Fun(e) = &expr.fun_type {
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
        if prev.is_empty() {
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

    //indent count
    let mut tab_count = 0;
    let tl_expr = func_analyze(&expr.expr, &mut tab_count);

    // if there is no type argument, then return
    if targs.is_empty() {
        return format!("{}{}: {} :=\n{}.\n", head, args, ret, tl_expr);
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

    format!("{} {}{}: {} :=\n{}.\n", head, s_targs, args, ret, tl_expr)
}

fn func_analyze(expr: &S::LangExpr, count: &mut i32) -> String {
    match expr {
        S::LangExpr::IfExpr(ex) => {
            let mut if_expr = "".to_string();
            if_expr = format!(
                "{}match {} with\n",
                if_expr,
                func_analyze(&ex.cond_expr, count)
            );
            *count += 2;
            let tab_expr = tabb(*count);

            if_expr = format!(
                "{}{}| true => {}\n",
                if_expr,
                tab_expr,
                func_analyze(&ex.then_expr, count)
            );
            if_expr = format!(
                "{}{}| false => {}\n",
                if_expr,
                tab_expr,
                func_analyze(&ex.else_expr, count)
            );

            *count -= 2;
            format!("{}{}end", if_expr, tabb(*count + 2))
        }
        S::LangExpr::LetExpr(ex) => {
            let mut let_expr = "".to_string();
            if ex.def_vars.is_empty() {
                return let_expr;
            }
            for t in &ex.def_vars {
                let_expr = format!(
                    "{}let {} = {} in\n",
                    let_expr,
                    pattern_analyze(&t.pattern),
                    func_analyze(&t.expr, count)
                );
            }
            let_expr
        }
        S::LangExpr::LitStr(ex) => ex.str.to_string(),
        S::LangExpr::LitChar(ex) => ex.c.to_string(),
        S::LangExpr::LitNum(ex) => ex.num.to_string(),
        S::LangExpr::LitBool(ex) => ex.val.to_string(),
        S::LangExpr::IDExpr(ex) => ex.id.to_string(),
        S::LangExpr::DataExpr(ex) => {
            let mut data_expr = "".to_string();
            let temp: &str = &ex.label.id;
            let temp1 = match temp {
                "Cons" => "cons".to_string(),
                _ => temp.to_string(),
            };
            data_expr = format!("{}({}", data_expr, temp1);
            if !&ex.exprs.is_empty() {
                for t in &ex.exprs {
                    data_expr = format!("{} {}", data_expr, func_analyze(t, count));
                }
            }
            format!("{})", data_expr)
        }
        S::LangExpr::MatchExpr(ex) => {
            let mut match_expr = "match".to_string();
            match_expr = format!("{} {} with", match_expr, func_analyze(&ex.expr, count));

            *count += 2;
            let tab_expr = tabb(*count);

            let mut case_expr = "".to_string();
            for t in &ex.cases {
                case_expr = format!(
                    "{}\n{}| {} => {}",
                    case_expr,
                    tab_expr,
                    pattern_analyze(&t.pattern),
                    func_analyze(&t.expr, count)
                );
            }
            *count -= 2;
            format!("{}{}\n{}end", match_expr, case_expr, tabb(*count + 2))
        }
        S::LangExpr::ApplyExpr(ex) => {
            let mut apply_expr = "(".to_string();
            let mut store: Option<String> = None;
            for t in &ex.exprs {
                let temp = func_analyze(t, count);
                match apply_arith(temp.clone()) {
                    Some(_) => {
                        store = apply_arith(temp);
                    }
                    None => {
                        match store {
                            Some(y) => apply_expr = format!("{} {} {}", apply_expr, temp, y),
                            None => apply_expr = format!("{}{} ", apply_expr, temp),
                        }
                        store = None;
                    }
                }
            }
            format!("{})", apply_expr)
        }
        S::LangExpr::ListExpr(ex) => {
            if ex.exprs.is_empty() {
                return "nil".to_string();
            }
            let mut list_expr = "".to_string();
            let mut temp = "".to_string();
            for (_i, t) in ex.exprs.iter().enumerate() {
                list_expr = format!("{}(cons {} ", list_expr, func_analyze(t, count));
                temp = format!("{})", temp);
            }
            format!("{}nil{}", list_expr, temp)
        }
        S::LangExpr::TupleExpr(ex) => {
            let length = &ex.exprs.len();
            let mut tupple_expr = format!("tup{}", &length);
            match length {
                0 => tupple_expr,
                _ => {
                    tupple_expr = format!("{} (", tupple_expr);
                    for t in &ex.exprs {
                        tupple_expr = format!("{} {}", tupple_expr, func_analyze(t, count));
                    }
                    format!("{})", tupple_expr)
                }
            }
        }
        S::LangExpr::LambdaExpr(ex) => {
            let mut lambda_expr = "fun".to_string();
            if ex.args.is_empty() {
                lambda_expr = format!("{} _", lambda_expr);
            } else {
                for t in &ex.args {
                    lambda_expr = format!("{} {}", lambda_expr, t.id);
                }
            }
            format!("{} => {}", lambda_expr, func_analyze(&ex.expr, count))
        }
    }
}

fn pattern_analyze(pattern: &S::Pattern) -> String {
    match pattern {
        S::Pattern::PatStr(ex) => ex.str.to_string(),
        S::Pattern::PatChar(ex) => ex.c.to_string(),
        S::Pattern::PatNum(ex) => ex.num.to_string(),
        S::Pattern::PatBool(ex) => ex.val.to_string(),
        S::Pattern::PatID(ex) => ex.id.to_string(),
        S::Pattern::PatTuple(ex) => {
            let mut pattern_expr = "".to_string();
            let length = &ex.pattern.len();
            if ex.pattern.is_empty() {
                return format!("{}tup{}", pattern_expr, length);
            }
            pattern_expr = format!("{}tup{} (", pattern_expr, length);
            for t in &ex.pattern {
                pattern_expr = format!("{} {}", pattern_expr, pattern_analyze(t));
            }
            format!("{})", pattern_expr)
        }
        S::Pattern::PatData(ex) => {
            let mut pattern_expr = "".to_string();
            let temp: &str = &ex.label.id;
            let temp = match temp {
                "Cons" => "cons".to_string(),
                _ => temp.to_string(),
            };
            pattern_expr = format!("{}({}", pattern_expr, temp);
            for t in &ex.pattern {
                pattern_expr = format!("{} {}", pattern_expr, pattern_analyze(t));
            }
            format!("{})", pattern_expr)
        }
        S::Pattern::PatNil(_) => "_".to_string(),
    }
}

fn apply_arith(expr: String) -> Option<String> {
    let temp: Vec<char> = expr.chars().collect();
    match temp[0] {
        '+' => Some(String::from("+")),
        '-' => Some(String::from("-")),
        '*' => Some(String::from("*")),
        '/' => Some(String::from("/")),
        '%' => Some(String::from("%")),
        _ => None,
    }
}

fn tabb(count: i32) -> String {
    let mut tab_expr = "".to_string();
    for _ in 1..=count {
        tab_expr = format!("{} ", tab_expr);
    }
    tab_expr
}

fn is_recursive(expr: &S::Defun) -> bool {
    is_recursive_expr(&expr.expr, &expr.id.id)
}

fn is_recursive_expr(expr: &S::LangExpr, id: &str) -> bool {
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

fn is_recursive_exprs(exprs: &[S::LangExpr], id: &str) -> bool {
    for e in exprs.iter() {
        if is_recursive_expr(e, id) {
            return true;
        }
    }
    false
}
