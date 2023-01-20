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
//! [Homepage](https://ytakano.github.io/blisp/) is here.
//!
//! ## Features
//!
//! - Algebraic data type
//! - Generics
//! - Hindley–Milner based type inference
//! - Effect system to separate side effects from pure functions
//! - Big integer
//! - Supporting no_std environments
//!
//! ## Examples
//!
//! ### Simple Eval
//!
//! ```
//! let code = "
//! (export factorial (n) (Pure (-> (Int) Int))
//!     (factorial' n 1))
//!
//! (defun factorial' (n total) (Pure (-> (Int Int) Int))
//!     (if (<= n 0)
//!         total
//!         (factorial' (- n 1) (* n total))))";
//!
//! let exprs = blisp::init(code).unwrap();
//! let ctx = blisp::typing(&exprs).unwrap();
//! let expr = "(factorial 10)";
//! for result in blisp::eval(expr, &ctx).unwrap() {
//!    println!("{}", result.unwrap());
//! }
//! ```
//!
//! ### Foreign Function Interface
//!
//! ```
//! use blisp;
//! use num_bigint::BigInt;
//!
//! let expr = "
//! (export callback (x y z)
//!     (IO (-> (Int Int Int) (Option Int)))
//!     (call-rust x y z))";
//! let exprs = blisp::init(expr).unwrap();
//! let mut ctx = blisp::typing(&exprs).unwrap();
//!
//! let fun = |x: &BigInt, y: &BigInt, z: &BigInt| {
//!     let n = x * y * z;
//!     println!("n = {}", n);
//!     Some(n)
//! };
//! ctx.set_callback(Box::new(fun));
//!
//! let e = "(callback 100 2000 30000)";
//! blisp::eval(e, &ctx);
//! ```
//!
//! ### Expressions
//!
//! ```lisp
//! "Hello, World!" ; string
//! (+ 0x10 0x20)   ; 48
//! (+ 0b111 0b101) ; 12
//! (+ 0o777 0o444) ; 803
//! (car '(1 2 3))  ; (Some 1)
//! (cdr '(1 2 3))  ; '(2 3)
//! (map (lambda (x) (* x 2)) '(8 9 10)) ; '(16 18 20)
//! (fold + 0 '(1 2 3 4 5 6 7 8 9))      ; 45
//! (reverse '(1 2 3 4 5 6 7 8 9))       ; '(9 8 7 6 5 4 3 2 1)
//! (filter (lambda (x) (= (% x 2) 0)) '(1 2 3 4 5 6 7 8 9)) ; '(2 4 6 8)
//! ```

//#![no_std]

extern crate alloc;
extern crate proc_macro;

use alloc::collections::linked_list::LinkedList;
use alloc::format;
use alloc::string::{String, ToString};
use num_bigint::{BigInt, ToBigInt};
use embed_macro::embedded;

pub mod coq;
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
        LispErr { msg, pos }
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
    // let prelude = include_str!("prelude.lisp");
    let prelude = "";
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

pub fn transpile(ctx: &semantics::Context) -> String {
    let mut s = "".to_string();
    for (_, d) in ctx.data.iter() {
        println!("AST(Data):\n{:#?}", d);
        s = format!("{}{}\n", s, coq::to_coq_data(d));
    }

    for (_, f) in ctx.funs.iter() {
        //println!("AST(Defun):\n{:#?}", f);
        s = format!("{}{}\n", s, coq::to_coq_func(f));
    }

    format!("{}\n\n{}", coq::import(), s)
}

/////////////////////////////////////////////////////////////////////
#[embedded]
fn test2(
    _z: BigInt,
    _a: Vec<BigInt>, 
    _b: (BigInt, BigInt),
    _c: Option<BigInt>,
    _d: Result<BigInt, String>,
) -> Option<BigInt> {
    let temp = 5.to_bigint();
    temp
}
/////////////////////////////////////////////////////////////////////


#[cfg(test)]
#[macro_use]
extern crate std;

#[cfg(test)]
mod tests {
    use crate::{eval, init, semantics, transpile, typing};

    fn eval_result(code: &str, ctx: &semantics::Context) {
        for r in eval(code, &ctx).unwrap() {
            println!("{} -> {}", code, r.unwrap());
        }
    }

    #[test]
    fn ops() {
        let exprs = init("").unwrap();
        let ctx = typing(&exprs).unwrap();
        eval_result("(neq (Some \"Hello\") 10)", &ctx);
        eval_result("(chars \"Hello, World!\")", &ctx);
        eval_result("(str '(`H` `e` `l` `l` `o`))", &ctx);
        eval_result("`\\``", &ctx);
        eval_result("(= `h` `h`)", &ctx);
        eval_result("(<< 8 4)", &ctx);
        eval_result("(>> 128 4)", &ctx);
        eval_result("\"Hello, World!\"", &ctx);
        eval_result("(= \"Hello, World!\" \"Hello, World!\")", &ctx);
        eval_result("(= (Some 1) (Some 2))", &ctx);
        eval_result("(< (Some 1) (Some 2))", &ctx);
        eval_result("(> (Some 1) (Some 2))", &ctx);
        eval_result("(= \"Hello\" \"Hel\")", &ctx);
        eval_result("(eq \"Hello\" 10)", &ctx);
        eval_result("(lt \"Hello\" 10)", &ctx);
        eval_result("(lt 5 10)", &ctx);
        eval_result("(+ 0x10 0x20)", &ctx);
        eval_result("(+ 0b111 0b101)", &ctx);
        eval_result("(+ 0o777 0o444)", &ctx);
        eval_result("(+ 10 20)", &ctx);
        eval_result("(pow 10 20)", &ctx);
        eval_result("(band 1 0)", &ctx);
        eval_result("(band 1 1)", &ctx);
        eval_result("(bor 1 0)", &ctx);
        eval_result("(bor 1 1)", &ctx);
        eval_result("(bxor 1 0)", &ctx);
        eval_result("(bxor 1 1)", &ctx);
        eval_result("(sqrt 16)", &ctx);
        eval_result("(sqrt -1)", &ctx);
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

        let e = "(lambda-test +)";
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
        let expr = "
(export factorial (n) (Pure (-> (Int) Int))
    (factorial' n 1))
(defun factorial' (n total) (Pure (-> (Int Int) Int))
    (if (<= n 0)
        total
        (factorial' (- n 1) (* n total))))
";
        let exprs = init(expr).unwrap();
        let ctx = typing(&exprs).unwrap();

        let e = "(Some 10)";
        eval_result(e, &ctx);

        let e = "(car '(1 2 3))";
        eval_result(e, &ctx);

        let e = "(cdr '(1 2 3))";
        eval_result(e, &ctx);

        let e = "(map (lambda (x) (* x 2)) '(8 9 10))";
        eval_result(e, &ctx);

        let e = "(fold + 0 '(1 2 3 4 5 6 7 8 9))";
        eval_result(e, &ctx);

        let e = "(reverse '(1 2 3 4 5 6 7 8 9))";
        eval_result(e, &ctx);

        let e = "(filter (lambda (x) (= (% x 2) 0)) '(1 2 3 4 5 6 7 8 9))";
        eval_result(e, &ctx);

        let e = "(factorial 2000)";
        eval_result(e, &ctx);
    }

    #[test]
    fn callback() {
        let expr = "
(export callback (x y z) (IO (-> (Int Int Int) (Option Int)))
    (call-rust x y z))";
        let exprs = init(expr).unwrap();
        let mut ctx = typing(&exprs).unwrap();

        use num_bigint::BigInt;
        use std::boxed::Box;
        let fun = |x: &BigInt, y: &BigInt, z: &BigInt| {
            let n = x * y * z;
            println!("n = {}", n);
            Some(n)
        };
        ctx.set_callback(Box::new(fun));

        let e = "(callback 100 2000 30000)";
        eval_result(e, &ctx);
    }

    #[test]
    fn do_transpile() {
        //let expr = "(data D (A []) (B [Int Int]) (C '(Int)))";
        //let expr = "(export t () (Pure (-> () Bool)) (true))";
        /*(export double (x)
        (Pure (-> (Int) Int))
        (* 2 x))
        
        let expr = "
        (data (Option t)
            (Some t)
            None)

        (data (Result t e)
            (Ok t)
            (Err e))
        
        (export cdr (x) (Pure (-> ('(t)) '(t)))
            (match x
                ((Cons _ l) l)
                (_ '())))

        (export car (x) (Pure (-> ('(t)) (Option t)))
            (match x
                ((Cons n _) (Some n))
                (_ None)))

        (export double (x)
            (Pure (-> (Int) Int))
            (* 2 x))
        ";
        */
        // let expr = "
        // (data (Option t)
        // (Some t)
        // None)

        // (data (Result t e)
        // (Ok t)
        // (Err e))
        
        // (defun filter' (f x l)
        // (Pure (-> (
        //     (Pure (-> (t) Bool)) '(t) '(t))
        //     '(t)))
        // (match x
        //     ((Cons h a) (if (f h) (filter' f a (Cons h l)) (filter' f a l) ))
        //     (_ l)))";
        /*"(defun filter' (f x l)
        (Pure (-> (
            (Pure (-> (t) Bool)) '(t) '(t))
            '(t)))
        (match x
            ((Cons h a) (if (f h) (filter' f a (Cons h l)) (filter' f a l) ))
            (_ l)))";*/
        /*"(export map (f x) (Pure (-> ((Pure (-> (a) b)) '(a)) '(b)))
        (match x
            ((Cons h l) (Cons (f h) (map f l)))
            (_ '())))";*/
        let expr = "
        (defun snoc (l y)
        (Pure (-> (
            '(t) t)
        '(t)))
        (match l
            (nil (Cons y nil))
            ((Cons h b) (Cons h (snoc b y)))))
            
        (defun rev (l)
        (Pure (-> (
            '(t))
        '(t)))
        (match l
            (nil nil)
            ((Cons h t) (snoc (rev t) h))))
            ";
        let exprs = init(expr).unwrap();
        let ctx = typing(&exprs).unwrap();

        println!("{}", transpile(&ctx));
    }
}