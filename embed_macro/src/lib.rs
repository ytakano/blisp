extern crate proc_macro;

use quote::quote;
use syn::{parse_macro_input, Item, FnArg, GenericArgument, Signature, Type, PathArguments};    //PathSegment
use proc_macro2::Ident;


#[proc_macro_attribute]
pub fn embedded(_args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
        syn::Ident::new(&temp, proc_macro2::Ident::span(fn_name))
    };

    let fn_data = &item_fn.sig;
    let inputs_parse = inputs_type(fn_data);
    let output_ty = output_type(fn_data);

    let fn_body = format!("(extern {} (-> ({}) {}))", fn_name, inputs_parse, output_ty);
    let expanded = quote!{
        const #fn_name_cap: &str = #fn_body;
    };

    out.extend(proc_macro::TokenStream::from(expanded));
    out
}

fn inputs_type(data: &Signature) -> String {
    let ret = data.inputs.iter().map(|arg| {
        match arg {
            FnArg::Typed(pat) => parse_type(&*pat.ty),
            _ => panic!("Need an explicitly typed input pattern "),
        }
    });

    let mut statements = String::from("");
    for (i, data) in ret.enumerate() {
        if i==0 { statements = format!("{}{}", statements, data); }
        else    { statements = format!("{} {}", statements, data); }
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
                if i==0 { statements = format!("{}{}", statements, parse_type(data)); }
                else    { statements = format!("{} {}", statements, parse_type(data)); }
            }
            format!("{}]", statements)
        },
        Type::Path(path) => {
            let mut args_str = String::from("");
            match &path.path.segments.first().unwrap().arguments {
                // not generic type (eg BigInt)
                PathArguments::None => ex_type_check(&path.path.segments.first().unwrap().ident),

                // generic type (vec, option, result)
                PathArguments::AngleBracketed(ang) => {
                    let args = ang.args
                        .iter()
                        .map(|a| match a {
                            GenericArgument::Type(gene_type) => {
                                parse_type(gene_type)
                            },
                            _ => panic!("GenericArgument is only Type"),
                        });

                    for (i, data) in args.enumerate() {
                        if i==0 { args_str = format!("{}{}", args_str, data); }
                        else    { args_str = format!("{} {}", args_str, data); }
                    }

                    let type_name = &path.path.segments.first().unwrap().ident;
                    let type_name_str = format!("{}", &type_name);

                    match &* type_name_str {
                        "Vec"    => format!("'({})", args_str),
                        "Option" => format!("(Option {})", args_str),
                        "Result" => format!("(Result {})", args_str),
                        _ => panic!("Generic Type only Vec/Option/Result"),
                    }
                },
                _ => panic!("no parentheses at PathArgument"),
            }
        },
        _ => panic!("parse type miss"),
    }
}

fn ex_type_check(id: &Ident) -> String {
    let id_str = format!("{}", &id);
    match &*id_str {
        "BigInt"             => String::from("Int"),
        "char"               => String::from("Char"),
        "String" | "&str"    => String::from("String"),
        "bool"               => String::from("Bool"),
        // "Vec"                => Some(Ident::new("Bool", span)),
        _ => panic!("no match String pattern"),
    }
}

