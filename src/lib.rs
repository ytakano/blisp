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
    let mut ps = parser::Parser::new(code);
    match ps.parse() {
        Ok(e) => Ok(e),
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

pub fn eval(code: &str, ctx: &semantics::Context) -> Result<LinkedList<String>, LispErr> {
    runtime::eval(code, ctx)
}

#[cfg(test)]
mod tests {
    use crate::{eval, init, typing};

    #[test]
    fn add() {
        let exprs = init("").unwrap();
        let ctx = typing(&exprs).unwrap();
        eval("(+ 10 20)", &ctx).unwrap();
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
        eval(e, &ctx).unwrap();
    }
}
