#![no_std]

#[macro_use]
extern crate alloc;

use alloc::collections::linked_list::LinkedList;
use alloc::string::String;

pub mod parser;
pub mod runtime;
pub mod semantics;

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct LispErr {
    pub msg: String,
    pub pos: Pos,
}

impl LispErr {
    fn new(msg: String, pos: Pos) -> LispErr {
        LispErr { msg: msg, pos: pos }
    }
}

pub fn init(code: &str) -> Result<LinkedList<parser::Expr>, LispErr> {
    let prelude = include_str!("prelude.lisp");
    let mut ps = parser::Parser::new(prelude);
    let mut exprs = match ps.parse() {
        Ok(e) => e,
        Err(e) => {
            let msg = format!("Syntax Error: {}", e.msg);
            return Err(LispErr::new(msg, e.pos));
        }
    };

    let mut ps = parser::Parser::new(code);
    match ps.parse() {
        Ok(mut e) => {
            exprs.append(&mut e);
            Ok(exprs)
        }
        Err(e) => {
            let msg = format!("Syntax Error: {}", e.msg);
            Err(LispErr::new(msg, e.pos))
        }
    }
}

pub fn typing(exprs: &LinkedList<parser::Expr>) -> Result<semantics::Context, LispErr> {
    match semantics::exprs2context(exprs) {
        Ok(c) => Ok(c),
        Err(e) => {
            let msg = format!("Typing Error: {}", e.msg);
            Err(LispErr::new(msg, e.pos))
        }
    }
}

pub fn eval(
    code: &str,
    ctx: &semantics::Context,
) -> Result<LinkedList<Result<String, String>>, LispErr> {
    runtime::eval(code, ctx)
}

#[cfg(test)]
#[macro_use]
extern crate std;

#[cfg(test)]
mod tests {
    use crate::{eval, init, semantics, typing};

    fn eval_result(code: &str, ctx: &semantics::Context) {
        for r in eval(code, &ctx).unwrap() {
            r.unwrap();
        }
    }

    #[test]
    fn add() {
        let exprs = init("").unwrap();
        let ctx = typing(&exprs).unwrap();
        eval_result("(+ 10 20)", &ctx);
    }

    #[test]
    fn lambda() {
        let expr = "(export lambda-test (f)
    (Pure (-> ((Pure (-> (Int Int) Int))) Int))
    (f 10 20))
";
        let exprs = init(expr).unwrap();
        let ctx = typing(&exprs).unwrap();
        let e = "(lambda-test (lambda (x y) (* x y)))";
        eval_result(e, &ctx);
    }

    #[test]
    fn list() {
        let expr = "
(export head (x) (Pure (-> ('(Int)) (Option Int)))
    (match x
        ((Cons n _) (Some n))
        (_ None)))

(export tail (x) (Pure (-> ('(Int)) (Option Int)))
    ; match expression
    (match x
        (Nil None)
        ((Cons n Nil) (Some n))
        ((Cons _ l) (tail l))))
";
        let exprs = init(expr).unwrap();
        let ctx = typing(&exprs).unwrap();

        let e = "(head '(30 40 50))";
        eval_result(e, &ctx);

        let e = "(tail '(30 40 50))";
        eval_result(e, &ctx);
    }

    #[test]
    fn tuple() {
        let expr = "(export first (x) (Pure (-> ([Int Bool]) Int))
    (match x
        ([n _] n)))
";
        let exprs = init(expr).unwrap();
        let ctx = typing(&exprs).unwrap();
        let e = "(first [10 false])";
        eval_result(e, &ctx);
    }

    #[test]
    fn prelude() {
        let expr = "";
        let exprs = init(expr).unwrap();
        let ctx = typing(&exprs).unwrap();

        let e = "(Some 10)";
        eval_result(e, &ctx);

        let e = "(car '(1 2 3))";
        eval_result(e, &ctx);

        let e = "(cdr '(1 2 3))";
        eval_result(e, &ctx);

        let e = "(map (lambda (x) (* x 2)) '(1 2 3))";
        eval_result(e, &ctx);

        let e = "(fold (lambda (x y) (+ x y)) 0 '(1 2 3))";
        eval_result(e, &ctx);
    }
}
