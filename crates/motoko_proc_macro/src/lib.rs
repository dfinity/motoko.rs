extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn parse_static(input: TokenStream) -> TokenStream {
    let input = get_input_string(input);
    expand_static(
        &motoko::check::parse(&input).expect("Parse error"),
        "::motoko::ast::Prog",
    )
}

// Temporarily removed due to possible confusion around the different set of feature flags

// #[proc_macro]
// pub fn eval_static(input: TokenStream) -> TokenStream {
//     let input = get_input_string(input);
//     expand_static(
//         &motoko::vm::eval(&input).expect("Evaluation error"),
//         "::motoko::value::Value",
//     )
// }

fn expand_static<'de, T: Sized + serde::Serialize + serde::Deserialize<'de>>(
    value: &T,
    type_path: &'static str,
) -> TokenStream {
    let serialized = motoko::proc_macro::serialize(value).expect("Serialization error");
    let serialized_ast = proc_macro2::Literal::byte_string(&serialized);
    let typ = syn::Type::Verbatim(type_path.parse().expect("Type parse error"));

    TokenStream::from(quote! {{
        // Derived from the `lazy_static!` macro expansion
        struct STATIC;
        impl std::ops::Deref for STATIC {
            type Target = #typ;
            fn deref(&self) -> &#typ {
                //struct _AssertSync where #typ: std::marker::Sync;
                struct _AssertSized where #typ: std::marker::Sized;
                static ONCE: std::sync::Once = std::sync::Once::new();
                static mut VALUE: *mut #typ = 0 as *mut #typ;
                unsafe {
                    ONCE.call_once(|| VALUE = Box::into_raw(Box::new(
                        ::motoko::proc_macro::deserialize::<#typ>(#serialized_ast).expect("Deserialization error")
                    )));
                    &*VALUE
                }
            }
        };
        &*STATIC
    }})
}

fn get_input_string(input: TokenStream) -> String {
    syn::parse::<syn::LitStr>(input)
        .expect("Unexpected macro input")
        .value()
}
