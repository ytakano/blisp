# BLisp

BLisp is a statically typed Lisp like programming language which adopts effect system for no_std environments.
BLisp supports higher order RPC like higher order functions of functional programing languages.

This repository provides only a library crate.
Please see [blisp-repl](https://github.com/ytakano/blisp-repl) to use BLisp,
and [baremetalisp](https://github.com/ytakano/baremetalisp) which is a toy OS.

## Features

- Algebraic data type
- Generics
- Hindley–Milner based type inference
- Effect system to separate side effects from pure functions
- Big integer
- Supporting no_std environments

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

If Rust compiler or linker says warning of fmod,
please add fmod manually as follows.

```rust
#[no_mangle]
extern "C" fn fmod(x: f64, y: f64) -> f64 {
    libm::fmod(x, y)
}
```

Cargo.toml

```toml
[dependencies.blisp]
version = "0.2"
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
