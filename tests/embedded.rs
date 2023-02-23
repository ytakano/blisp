use blisp::embedded;
use num_bigint::{BigInt, ToBigInt};

#[embedded]
fn test_fun(
    _z: BigInt,
    _a: Vec<BigInt>,
    _b: (BigInt, BigInt),
    _c: Option<BigInt>,
    _d: Result<BigInt, String>,
) -> Option<BigInt> {
    let temp = 5.to_bigint();
    temp
}

#[test]
fn test_embedded() {
    let code = "(defun call_test_fun ()
        (IO (-> () (Option Int)))
        (test_fun 1 '(2 3) [4 5] (Some 6) (Ok 7))
    )";
    let exprs = blisp::init(code, &[TEST_FUN]).unwrap();
    let ctx = blisp::typing(&exprs).unwrap();
}
