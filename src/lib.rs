//! # BLisp
//!
//! BLisp is a well typed Lisp like programming language which adopts effect
//! system for no_std environments.
//! BLisp supports higher order RPCs like higher order functions
//! of functional programing languages.
//!
//! This repository provides only a library crate.
//! Please see [blisp-repl](https://github.com/ytakano/blisp-repl) to use BLisp,
//! or [baremetalisp](https://github.com/ytakano/baremetalisp) which is a toy OS.
//!
//! ## Example
//!
//! ```
//! let code = "(export factorial (n) (Pure (-> (Int) Int))
//!    (if (<= n 0)
//!        1
//!        (* n (factorial (- n 1)))))";
//!
//! let exprs = blisp::init(code).unwrap();
//! let ctx = blisp::typing(&exprs).unwrap();
//! let expr = "(factorial 30)";
//! for result in blisp::eval(expr, &ctx).unwrap() {
//!    println!("{}", result.unwrap());
//! }
//! ```
//!
//! ## Features
//!
//! - Algebraic data type
//! - Generics
//! - Hindley–Milner based type inference
//! - Effect system to separate side effects from pure functions
//! - Big integer
//! - Supporting no_std environments

#![no_std]

#[macro_use]
extern crate alloc;

use alloc::collections::linked_list::LinkedList;
use alloc::string::String;

pub mod parser;
pub mod runtime;
pub mod semantics;

const FILE_ID_PRELUD: usize = 0;
const FILE_ID_USER: usize = 1;
pub(crate) const FILE_ID_EVAL: usize = 2;

/// indicate a position of file
#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub file_id: usize, // file identifier, 0 is prelude.lisp
    pub line: usize,    // line number, 0 origin
    pub column: usize,  // column number, 0 origin
}

/// error message
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

/// initialize BLisp with code
///
/// # Example
///
/// ```
/// let code = "(export factorial (n) (Pure (-> (Int) Int))
///    (if (<= n 0)
///        1
///        (* n (factorial (- n 1)))))";
///
/// blisp::init(code).unwrap();
/// ```
pub fn init(code: &str) -> Result<LinkedList<parser::Expr>, LispErr> {
    let prelude = include_str!("prelude.lisp");
    let mut ps = parser::Parser::new(prelude, FILE_ID_PRELUD);
    let mut exprs = match ps.parse() {
        Ok(e) => e,
        Err(e) => {
            let msg = format!("Syntax Error: {}", e.msg);
            return Err(LispErr::new(msg, e.pos));
        }
    };

    let mut ps = parser::Parser::new(code, FILE_ID_USER);
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

/// perform type checking and inference
///
/// # Example
///
/// ```
/// let code = "(export factorial (n) (Pure (-> (Int) Int))
///    (if (<= n 0)
///        1
///        (* n (factorial (- n 1)))))";
///
/// let exprs = blisp::init(code).unwrap();
/// blisp::typing(&exprs).unwrap();
/// ```
pub fn typing(exprs: &LinkedList<parser::Expr>) -> Result<semantics::Context, LispErr> {
    match semantics::exprs2context(exprs) {
        Ok(c) => Ok(c),
        Err(e) => {
            let msg = format!("Typing Error: {}", e.msg);
            Err(LispErr::new(msg, e.pos))
        }
    }
}

/// evaluate an expression
///
/// # Example
///
/// ```
/// let code = "(export factorial (n) (Pure (-> (Int) Int))
///    (if (<= n 0)
///        1
///        (* n (factorial (- n 1)))))";
///
/// let exprs = blisp::init(code).unwrap();
/// let ctx = blisp::typing(&exprs).unwrap();
/// let expr = "(factorial 30)";
/// for result in blisp::eval(expr, &ctx).unwrap() {
///    println!("{}", result.unwrap());
/// }
/// ```
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
