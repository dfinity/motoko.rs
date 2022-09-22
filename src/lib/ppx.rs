extern crate proc_macro;
use proc_macro::TokenStream;

use crate::check::parse;

// https://doc.rust-lang.org/reference/procedural-macros.html

#[proc_macro]
pub fn parse(stream: TokenStream) -> TokenStream {

    

    // "fn answer() -> u32 { 42 }".parse().unwrap()
}
