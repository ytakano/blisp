extern crate proc_macro;

use convert_case::{Case, Casing};
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{parse_macro_input, FnArg, GenericArgument, Item, PathArguments, Signature, Type};

#[proc_macro_attribute]
pub fn embedded(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut out = input.clone();

    let ty = parse_macro_input!(input as Item);
    let item_fn = match ty {
        Item::Fn(ref n) => n,
        _ => panic!("only function is allowed"),
    };

    let fn_name = &item_fn.sig.ident.clone();

    let fn_name_camel = {
        let mut temp = format!("{}", fn_name);
        temp = temp.to_case(Case::Pascal);
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

    let fn_name_str = format!("{fn_name}");

    let ffi_body = generate_ffi_body(fn_data, &fn_name, &fn_name_ffi);

    let expanded = quote! {
        struct #fn_name_camel;

        impl blisp::runtime::FFI for #fn_name_camel {
            fn blisp_extern(&self) -> &'static str { #fn_body }

            fn ffi(&self) -> fn(&mut blisp::runtime::Environment<'_>, &[blisp::runtime::RTData]) -> blisp::runtime::RTData {
                use blisp::runtime::{Environment, RTData, RTDataToRust, RustToRTData};
                fn ffi_inner(env: &mut Environment<'_>, args: &[RTData]) ->RTData {
                    #ffi_body
                }
                ffi_inner
            }

            fn name(&self) -> &'static str {
                #fn_name_str
            }
        }
    };

    out.extend(proc_macro::TokenStream::from(expanded));
    out
}

fn generate_ffi_body(data: &Signature, fn_name: &Ident, fn_name_ffi: &Ident) -> TokenStream {
    let mut body = quote! {};
    for (i, arg) in data.inputs.iter().enumerate() {
        let arg_type = match arg {
            FnArg::Typed(pat) => &*pat.ty,
            _ => panic!("Need an explicitly typed input pattern "),
        };

        let arg_dst = {
            let temp = format!("arg{i}");
            Ident::new(&temp, Ident::span(fn_name_ffi))
        };

        let arg_src = {
            quote! {
                &args[#i]
            }
        };

        let casting = typecast(arg_type, arg_dst, arg_src);

        body = quote! {
            #body
            #casting
        };
    }

    let ffi_invoke = call_ffi(data.inputs.len(), fn_name);

    quote! {
        #body
        let result = #ffi_invoke;
        RustToRTData::from(env, result)
    }
}

fn call_ffi(len: usize, fn_name: &Ident) -> TokenStream {
    match len {
        0 => quote! {
            #fn_name()
        },
        1 => quote! {
            #fn_name(arg0)
        },
        2 => quote! {
            #fn_name(arg0, arg1)
        },
        3 => quote! {
            #fn_name(arg0, arg1, arg2)
        },
        4 => quote! {
            #fn_name(arg0, arg1, arg2, arg3)
        },
        5 => quote! {
            #fn_name(arg0, arg1, arg2, arg3, arg4)
        },
        6 => quote! {
            #fn_name(arg0, arg1, arg2, arg3, arg4, arg5)
        },
        7 => quote! {
            #fn_name(arg0, arg1, arg2, arg3, arg4, arg5, arg6)
        },
        8 => quote! {
            #fn_name(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
        },
        9 => quote! {
            #fn_name(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
        },
        _ => panic!("too many arguments"),
    }
}

fn typecast(ty: &Type, arg_dst: Ident, arg_src: TokenStream) -> TokenStream {
    match ty {
        Type::Tuple(_tup) => {
            quote! {
                let #arg_dst: #ty = RTDataToRust::into(#arg_src);
            }
        }
        Type::Path(path) => match &path.path.segments.first().unwrap().arguments {
            PathArguments::None => {
                quote! {
                    let #arg_dst: #ty = RTDataToRust::into(#arg_src);
                }
            }
            PathArguments::AngleBracketed(_ang) => {
                let type_name = &path.path.segments.first().unwrap().ident;
                let type_name_str = format!("{}", &type_name);

                match type_name_str.as_str() {
                    "Vec" | "Option" | "Result" => quote! {
                        let #arg_dst: #ty = RTDataToRust::into(#arg_src);
                    },
                    _ => panic!("only Vec, Option, or Result generics types are allowed"),
                }
            }
            _ => panic!("no parentheses at PathArgument"),
        },
        _ => panic!("parse type miss"),
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
                        _ => panic!("only Vec, Option, or Result generics types are allowed"),
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
