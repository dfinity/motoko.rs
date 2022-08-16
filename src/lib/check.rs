use crate::ast::{Loc, Prog};
use crate::format::{format_one_line, format_pretty};
use crate::lexer::create_token_tree;
use crate::lexer_types::{GroupType, Token, TokenTree};

// TODO: refactor lalrpop lexer details
impl crate::parser::__ToTriple for Loc<Token> {
    fn to_triple(
        Loc(token, src): Self,
    ) -> Result<
        (crate::ast::Source, Token, crate::ast::Source),
        lalrpop_util::ParseError<crate::ast::Source, Token, &'static str>,
    > {
        Ok((src.clone(), token, src))
    }
}

fn filter_token_tree(tt: TokenTree) -> Option<TokenTree> {
    match tt {
        TokenTree::Token(Loc(ref t, _)) => match t {
            Token::Space(_) | Token::LineComment(_) | Token::MultiLineSpace(_) => None,
            _ => Some(tt),
        },
        TokenTree::Group(_, GroupType::Comment, _) => None,
        TokenTree::Group(trees, group, open_close) => Some(TokenTree::Group(
            trees.into_iter().filter_map(filter_token_tree).collect(),
            group,
            open_close,
        )),
    }
}

pub fn parse(input: &str) -> Result<Prog, ()> {
    let tt = create_token_tree(input)?;
    let tokens = filter_token_tree(tt).ok_or(())?.flatten();

    println!("{:?}", tokens); ////

    Ok(crate::parser::ProgParser::new()
        .parse(tokens.into_iter())
        .unwrap())
}

#[allow(unused_variables)]
pub fn assert_lex(input: &str, expected: &str, width: Option<usize>) -> TokenTree {
    println!("testing {}", input);
    let tree = create_token_tree(input).unwrap();
    println!(" * input {}", input);
    println!(" * parsed {:?}", tree);
    let result = width
        .map(|width| format_pretty(&tree, width))
        .unwrap_or_else(|| format_one_line(&tree));
    let formatted = format!("{}", result);
    println!(" * formatted:\n{}", formatted);
    assert_eq!(formatted, expected);
    tree
}

#[allow(unused_variables)]
pub fn assert_lex_roundtrip(input: &str, width: Option<usize>) -> TokenTree {
    assert_lex(input, input, width)
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

pub fn assert_vm_eval(input_prog: &str, expected_result: &str) {
    println!(
        "\nassert_vm_eval(\"{}\", \"{}\")",
        input_prog, expected_result
    );
    let v1 = crate::vm::eval(input_prog).unwrap();
    let v2 = crate::vm::eval(expected_result).unwrap();
    assert_eq!(v1, v2)
}
