use crate::ast::Prog;
use crate::format::format_one_line;
use crate::lexer::{create_token_tree, TokenTree};

pub fn parse(input: &str) -> Result<Prog, ()> {
    // crate::parser::ExpParser::new().parse(input).map_err(|_| ())
    Ok(crate::parser::ProgParser::new().parse(input).unwrap())
}

#[allow(unused_variables)]
pub fn assert_lex(input: &str, expected: &str) -> TokenTree {
    println!("testing {}", input);
    let tree = create_token_tree(input).unwrap();
    println!(" * input {}", input);
    println!(" * parsed {:?}", tree);
    let formatted = format!("{}", tree);
    println!(" * formatted {}", formatted);
    assert_eq!(formatted, expected);
    tree
}

#[allow(unused_variables)]
pub fn assert_lex_roundtrip(input: &str) -> TokenTree {
    assert_lex(input, input)
}

#[allow(unused_variables)]
pub fn assert_parse(input: &str, expected: &str) -> Prog {
    println!("testing {}", input);
    let prog = parse(input).unwrap();
    println!(" * input {}", input);
    println!(" * parsed {:?}", prog);
    let formatted = format_one_line(&prog);
    println!(" * formatted {}", formatted);
    assert_eq!(&formatted, expected);
    prog
}

pub fn assert_roundtrip(input: &str) {
    let _ = assert_parse(input, input);
}
