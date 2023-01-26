// This test checks that an attribute macro #[sorted] exists and is imported
// correctly in the module system. If you make the macro return an empty token
// stream or exactly the original token stream, this test will already pass!
//
// Be aware that the meaning of the return value of an attribute macro is
// slightly different from that of a derive macro. Derive macros are only
// allowed to *add* code to the caller's crate. Thus, what they return is
// compiled *in addition to* the struct/enum that is the macro input. On the
// other hand attribute macros are allowed to add code to the caller's crate but
// also modify or remove whatever input the attribute is on. The TokenStream
// returned by the attribute macro completely replaces the input item.
//
// Before moving on to the next test, I recommend also parsing the input token
// stream as a syn::Item. In order for Item to be available you will need to
// enable `features = ["full"]` of your dependency on Syn, since by default Syn
// only builds the minimum set of parsers needed for derive macros.
//
// After parsing, the macro can return back exactly the original token stream so
// that the input enum remains in the callers code and continues to be usable by
// code in the rest of the crate.
//
//
// Resources:
//
//   - The Syn crate for parsing procedural macro input:
//     https://github.com/dtolnay/syn
//
//   - The syn::Item type which represents a parsed enum as a syntax tree:
//     https://docs.rs/syn/1.0/syn/enum.Item.html

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

// #[embedded]
// fn test_fn(a: String, _a: Option<BigInt>) -> bool {
//     match &*a {
//         "a" => true,
//         _ => false,
//     }
// }

fn main() {
    // rust_function(2.to_bigint().unwrap(), 3.to_bigint().unwrap());
}
