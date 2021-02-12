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

## Internally Defined Types and Functions

```common-lisp
(data (Option t)
    (Some t)
    None)

(data (Result t e)
    (Ok t)
    (Err e))

(export car (x) (Pure (-> ('(t)) (Option t)))
    (match x
        ((Cons n _) (Some n))
        (_ None)))

(export cdr (x) (Pure (-> ('(t)) '(t)))
    (match x
        ((Cons _ l) l)
        (_ '())))

(export map (f x) (Pure (-> ((Pure (-> (a) b)) '(a)) '(b)))
    (match x
        ((Cons h l) (Cons (f h) (map f l)))
        (_ '())))

(export fold (f init x) (Pure (-> ((Pure (-> (a b) b)) b '(a)) b))
    (match x
        ((Cons h l) (fold f (f h init) l))
        (_ init)))
```
