use embed_macro::embedded;
use num_bigint::{BigInt, ToBigInt};

//test_a用
fn get_pw() -> String {
    String::from("pass")
}

#[embedded]
fn test_a(
    id: BigInt,
    id_list: Vec<BigInt>,
) -> Option<String> {
    if id_list.into_iter().any(|x| x == id) {
        let new_pw = get_pw();
        Some(new_pw)
    } else {
        None
    }
}

// #[embedded]
// fn test2(
//     _z: BigInt,
//     _a: Vec<BigInt>, 
//     _b: (BigInt, BigInt),
//     _c: Option<BigInt>,
//     _d: Result<BigInt, String>,
// ) -> Option<BigInt> {
//     let temp = 5.to_bigint();
//     temp
// }

fn main() {}
