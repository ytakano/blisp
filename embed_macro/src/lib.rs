extern crate proc_macro;

use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{parse_macro_input, FnArg, GenericArgument, Item, PathArguments, Signature, Type}; //PathSegment

#[proc_macro_attribute]
pub fn embedded(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut out = input.clone();

    let ty = parse_macro_input!(input as Item);
    let item_fn = match ty {
        Item::Fn(ref n) => n,
        _ => panic!("function is only allowed."),
    };

    let fn_name = &item_fn.sig.ident.clone();
    let fn_name_cap = {
        let mut temp = format!("{}", fn_name);
        temp = temp.to_ascii_uppercase();
        Ident::new(&temp, Ident::span(fn_name))
    };

    // Generate BLisp.
    let fn_data = &item_fn.sig;
    let inputs_parse = inputs_type(fn_data);
    let output_ty = output_type(fn_data);

    let fn_body = format!("(extern {} (-> ({}) {}))", fn_name, inputs_parse, output_ty);

    // Generate FFI
    let fn_name_ffi = {
        let temp = format!("{fn_name}_ffi");
        Ident::new(&temp, Ident::span(fn_name))
    };

    let ffi_body = generate_ffi_body(fn_data, &fn_name_ffi);

    let expanded = quote! {
        const #fn_name_cap: &str = #fn_body;
        fn #fn_name_ffi(args: &[blisp::runtime::RTData]) -> blisp::runtime::RTData {
            #ffi_body
            todo!()
        }
    };

    out.extend(proc_macro::TokenStream::from(expanded));
    out
}

fn generate_ffi_body(data: &Signature, fn_name: &Ident) -> TokenStream {
    let mut result = quote! {};
    for (i, arg) in data.inputs.iter().enumerate() {
        let arg_type = match arg {
            FnArg::Typed(pat) => &*pat.ty,
            _ => panic!("Need an explicitly typed input pattern "),
        };

        let arg_dst = {
            let temp = format!("arg{i}");
            Ident::new(&temp, Ident::span(fn_name))
        };

        let arg_src = {
            quote! {
                (&args[#i])
            }
        };

        let casting = typecast(arg_type, arg_dst, arg_src);

        result = quote! {
            #result
            #casting
        };
    }

    result
}

fn typecast(ty: &Type, arg_dst: Ident, arg_src: TokenStream) -> TokenStream {
    match ty {
        Type::Tuple(tup) => {
            quote! {
                // TODO
            }
        }
        Type::Path(path) => match &path.path.segments.first().unwrap().arguments {
            PathArguments::None => {
                ex_typecast(&path.path.segments.first().unwrap().ident, arg_dst, arg_src)
            }
            PathArguments::AngleBracketed(ang) => {
                let type_name = &path.path.segments.first().unwrap().ident;
                let type_name_str = format!("{}", &type_name);

                match type_name_str.as_str() {
                    "Vec" => quote! {
                        let #arg_dst: #ty = #arg_src.into();
                    },
                    "Option" => quote! {
                        let #arg_dst: #ty = blisp::runtime::option_to_option(#arg_src);
                    },
                    "Result" => quote! {
                        let #arg_dst: #ty = #arg_src.into();
                    },
                    _ => panic!("only Vec/Option/Result generics types are allowed"),
                }
            }
            _ => panic!("no parentheses at PathArgument"),
        },
        _ => panic!("parse type miss"),
    }
}

fn ex_typecast(id: &Ident, arg_dst: Ident, arg_src: TokenStream) -> TokenStream {
    let id_str = format!("{}", &id);
    match &*id_str {
        "BigInt" => quote! {
            let #arg_dst: BigInt = #arg_src.into();
        },
        "char" => quote! {
            let #arg_dst: char = #arg_src.into();
        },
        "bool" => quote! {
            let #arg_dst: bool = #arg_src.into();
        },
        "String" => quote! {
            let #arg_dst: String = #arg_src.into();
        },
        _ => panic!("arguments must be BigInt, char, bool, or String"),
    }
}

fn inputs_type(data: &Signature) -> String {
    let ret = data.inputs.iter().map(|arg| match arg {
        FnArg::Typed(pat) => parse_type(&*pat.ty),
        _ => panic!("Need an explicitly typed input pattern "),
    });

    let mut statements = String::from("");
    for (i, data) in ret.enumerate() {
        if i == 0 {
            statements = format!("{}{}", statements, data);
        } else {
            statements = format!("{} {}", statements, data);
        }
    }
    statements
}

fn output_type(data: &Signature) -> String {
    let ret = match &data.output {
        syn::ReturnType::Default => panic!("return type is necessary"),
        syn::ReturnType::Type(_, ty) => parse_type(&*ty),
    };

    ret
}

fn parse_type(ty: &Type) -> String {
    match ty {
        Type::Tuple(tup) => {
            let mut statements = String::from("[");

            for (i, data) in tup.elems.iter().enumerate() {
                if i == 0 {
                    statements = format!("{}{}", statements, parse_type(data));
                } else {
                    statements = format!("{} {}", statements, parse_type(data));
                }
            }
            format!("{}]", statements)
        }
        Type::Path(path) => {
            let mut args_str = String::from("");
            match &path.path.segments.first().unwrap().arguments {
                // not generic type (eg BigInt)
                PathArguments::None => ex_type_check(&path.path.segments.first().unwrap().ident),

                // generic type (vec, option, result)
                PathArguments::AngleBracketed(ang) => {
                    let args = ang.args.iter().map(|a| match a {
                        GenericArgument::Type(gene_type) => parse_type(gene_type),
                        _ => panic!("GenericArgument is only Type"),
                    });

                    for (i, data) in args.enumerate() {
                        if i == 0 {
                            args_str = format!("{}{}", args_str, data);
                        } else {
                            args_str = format!("{} {}", args_str, data);
                        }
                    }

                    let type_name = &path.path.segments.first().unwrap().ident;
                    let type_name_str = format!("{}", &type_name);

                    match type_name_str.as_str() {
                        "Vec" => format!("'({})", args_str),
                        "Option" => format!("(Option {})", args_str),
                        "Result" => format!("(Result {})", args_str),
                        _ => panic!("Generic Type only Vec/Option/Result"),
                    }
                }
                _ => panic!("no parentheses at PathArgument"),
            }
        }
        _ => panic!("parse type miss"),
    }
}

fn ex_type_check(id: &Ident) -> String {
    let id_str = format!("{}", &id);
    match &*id_str {
        "BigInt" => String::from("Int"),
        "char" => String::from("Char"),
        "String" => String::from("String"),
        "bool" => String::from("Bool"),
        _ => panic!("arguments must be BigInt, char, bool, or String"),
    }
}
