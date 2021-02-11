# BLisp

This repository provides only a library crate.
Please see [blisp-repl](https://github.com/ytakano/blisp-repl) to use BLisp.

## How to Use

```rust
use blisp;

fn main() {
    let code = "(export factorial (n) (Pure (-> (Int) Int))
    (if (<= n 0)
        1
        (* n (factorial (- n 1)))))";
    let exprs = blisp::init(code).unwrap();
    let ctx = blisp::typing(&exprs).unwrap();

    let e = "(factorial 10)";
    blisp::eval(e, &ctx).unwrap();
}
```

Cargo.toml

```toml
[dependencies]
blisp = { git = "https://github.com/ytakano/blisp.git" }
```

## Internal Types and Functions

```lisp
(data (Option t)
    (Some t)
    None)

(data (Result t e)
    (Ok t)
    (Err e))

(export car (x) (Pure (-> ('(Int)) (Option Int)))
    (match x
        ((Cons n _) (Some n))
        (_ None)))

(export cdr (x) (Pure (-> ('(Int)) '(Int)))
    (match x
        ((Cons _ l) l)
        (_ '())))
```
