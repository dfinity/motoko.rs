extern crate proc_macro;
use proc_macro::TokenStream;

use syn::{Expr, ExprLit, Lit};

// https://doc.rust-lang.org/reference/procedural-macros.html

#[proc_macro]
pub fn eval(stream: TokenStream) -> TokenStream {
    let ast: Expr = syn::parse(stream).unwrap();

    match ast {
        Expr::Lit(ExprLit {
            lit: Lit::Str(s), ..
        }) => {
            let input = s.value();
            let value = motoko::vm::eval(&input).unwrap();
            TokenStream::from(value)
        }
        _ => unimplemented!("Expected a string literal as macro input"),
    }

    // "fn answer() -> u32 { 42 }".parse().unwrap()
}
