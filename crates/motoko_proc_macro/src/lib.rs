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
    // let value: motoko::value::Value = bincode::deserialize(&serialized).unwrap();

    let serialized_ast = proc_macro2::Literal::byte_string(&serialized);

    let _ = motoko::proc_macro::deserialize::<motoko::ast::Prog>(&serialized)
        // .map_err(|e| e.to_string())
        .unwrap();

    TokenStream::from(quote! {{
        // lazy_static! {
        //     static ref VALUE: Value = deserialize_from(&#serialized_tokens).unwrap();
        // }
        // VALUE
        serde_json::deserialize::<motoko::ast::Prog>(&#serialized_ast)/* .map_err(|e| e.to_string()) */.unwrap()
    }})
}
