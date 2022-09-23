extern crate lazy_static;
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;

// https://doc.rust-lang.org/reference/procedural-macros.html

#[proc_macro]
pub fn parse_static(stream: TokenStream) -> TokenStream {
    let input = syn::parse::<syn::LitStr>(stream)
        .expect("Unexpected macro input")
        .value();
    let prog = motoko::check::parse(&input).expect("Evaluation error");
    let serialized = motoko::proc_macro::serialize(&prog).expect("Serialization error");
    let serialized_ast = proc_macro2::Literal::byte_string(&serialized);
    TokenStream::from(quote! {{
        motoko::proc_macro::deserialize::<motoko::ast::Prog>(#serialized_ast as &[u8]).unwrap()
    }})
}
