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

#[embedded]
fn add_four_ints(a: BigInt, b: (BigInt, BigInt), c: Option<BigInt>) -> Result<BigInt, String> {
    let mut result = a + b.0 + b.1;
    if let Some(n) = c {
        result += n;
    }

    Ok(result)
}

#[test]
fn test_embedded() {
    // test_fun
    let code = "(export call_test_fun ()
        (IO (-> () (Option Int)))
        (test_fun 1 '(2 3) [4 5] (Some 6) (Ok 7))
    )";
    let exprs = blisp::init(code, vec![Box::new(TestFun)]).unwrap();
    let ctx = blisp::typing(exprs).unwrap();
    let result = blisp::eval("(call_test_fun)", &ctx).unwrap();

    let front = result.front().unwrap().as_ref().unwrap();
    assert_eq!(front, "(Some 5)");

    // add_for_ints
    let code = "(export call_add_four_ints (n)
        (IO (-> ((Option Int)) (Result Int String)))
        (add_four_ints 1 [2 3] n)
    )";
    let exprs = blisp::init(code, vec![Box::new(AddFourInts)]).unwrap();
    let ctx = blisp::typing(exprs).unwrap();
    let result = blisp::eval("(call_add_four_ints (Some 4))", &ctx).unwrap();

    let front = result.front().unwrap().as_ref().unwrap();
    assert_eq!(front, "(Ok 10)");
}
