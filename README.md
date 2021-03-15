# BLisp

BLisp is a statically typed Lisp like programming language which adopts effect system for no_std environments.
BLisp supports higher order RPC like higher order functions of functional programming languages.

This repository provides only a library crate.
Please see [blisp-repl](https://github.com/ytakano/blisp-repl) to use BLisp,
and [baremetalisp](https://github.com/ytakano/baremetalisp) which is a toy OS.

**[Homepage](https://ytakano.github.io/blisp/) is here.**

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
version = "0.3.4"
```

## Examples

```lisp
"Hello, World!" ; string
(+ 0x10 0x20)   ; 48
(+ 0b111 0b101) ; 12
(+ 0o777 0o444) ; 803
(car '(1 2 3))  ; (Some 1)
(cdr '(1 2 3))  ; '(2 3)
(map (lambda (x) (* x 2)) '(8 9 10)) ; '(16 18 20)
(fold + 0 '(1 2 3 4 5 6 7 8 9))      ; 45
(reverse '(1 2 3 4 5 6 7 8 9))       ; '(9 8 7 6 5 4 3 2 1)
(filter (lambda (x) (= (% x 2) 0)) '(1 2 3 4 5 6 7 8 9)) ; '(2 4 6 8)
```

## Version History

### 0.3.4

- fix bugs on typing
  - bug 1: some locally defined functions are cannot be called
  - bug 2: empty list cannot be typed properly
- add filter and reverse functions to prelude

### 0.3.3

- add hexadecimal, octal, and binary
  - 0xabcDEF
  - 0o777
  - 0b1010

### 0.3.2

- add pow to compute exponent
  - example: (pow 10 20)
  - type of pow: (Pure (-> (Int Int) (Option Int)))
  - if the exponent portion is greater or equal to 2^32, then return None
- add sqrt
  - example: (sqrt 16)
  - type of sqrt: (Pure (-> (Int) (Option Int)))
  - if the value is less than 0, then return None
- add bitwise operations
  - band, bor, bxor

### 0.3.1

- garbage collection is ready (mark and sweep)
