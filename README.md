# BLisp

BLisp is a statically typed Lisp like programming language which adopts effect system for no_std environments.
BLisp supports higher order RPC like higher order functions of functional programming languages.

This repository provides only a library crate.
Please see [blisp-repl](https://github.com/ytakano/blisp-repl) to use BLisp,
and [baremetalisp](https://github.com/ytakano/baremetalisp) which is a toy OS.

[Homepage](https://ytakano.github.io/blisp/) is here.

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
version = "0.3.2"
```

## Version History

### 0.3.2

- add pow to compute exponent
  - example: (pow 10 20)
  - type of pow: (Pure (-> (Int Int) (Option Int)))
  - the exponent portion is greater or equal to 2^32, then return None
- add sqrt
  - example: (sqrt 16)
  - type of sqrt: (Pure (-> (Int) (Option Int)))
  - if the value is less than 0, then return None
- add bitwise operations
  - band, bor, bxor

### 0.3.1

- garbage collection is ready (mark and sweep)

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
