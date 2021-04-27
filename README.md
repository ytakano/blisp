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
version = "0.3"
```

## Examples

```lisp
"Hello, World!" ; "Hello, World!"
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

### 0.3.9

- fix a bug on the garbage collector

### 0.3.8

- fix a bug on typing
  - check the number of arguments
  - (+ 10) was passed typing rule
- add != and neq
  - (!= 10 10)
  - (neq (Some "Hello") 10)

### 0.3.7

- add bit shift operators
  - (<< 8 4)   ; shift left
  - (>> 128 4) ; shift right
- add Char type
- add character literal
  - \`H\`
- add chars and str functions
  - chars converts String to (List Char)
    - (chars "Hello, World!")
  - str converts (List Char) to String
    - (str '(\`H\` \`e\` \`l\` \`l\` \`o\`))

### 0.3.6

- make <, >, <=, >= functions' type (Pure (-> (t t) Bool))
  - perform comparison between 2 values whose types are same
  - (< "Hello" "World")
  - (>= (Some 5) (Some 19))
- add lt, gt, leq, geq functions
  - perform comparison between any 2 values
  - function type is (Pure (-> (t1 t2) Bool))
  - (eq "Hello" 10)
  - (lt (Some 6) "Hello")

### 0.3.5

- add string type and literal
  - String
  - "Hello World!"
- make equal function generics
  - it can be used for non-integer types
  - (= "Hello" "Hello")
  - (= (Some 1) (Some 2))
- fix a bug on typing
  - (= (Some 1) (Some 2)) could not be typed properly

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
