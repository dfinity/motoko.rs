use crate::ast::{Loc, Prog, Source};
use crate::format::{format_one_line, format_pretty};
use crate::lexer::create_token_tree;
use crate::lexer_types::{GroupType, Token, TokenTree};
use crate::parser_types::SyntaxError;
use regex::Regex;
use structopt::lazy_static::lazy_static;

// Replace a token tree with equivalent whitespace
fn spacify_token_tree(tt: TokenTree) -> TokenTree {
    lazy_static! {
        static ref REPLACE_REGEX: Regex = Regex::new(r"\S").unwrap();
    }
    TokenTree::Token(Loc(
        Token::Space(
            REPLACE_REGEX
                .replace_all(&format!("{}", tt), " ")
                .to_string(),
        ),
        Source::Unknown,
    ))
}

fn prepare_token_tree(tt: TokenTree) -> TokenTree {
    match tt {
        TokenTree::Token(Loc(ref token, _)) => match token {
            Token::LineComment(_) | Token::BlockComment(_) => spacify_token_tree(tt),
            // Token::Space(_) | Token::MultiLineSpace(_) => None,
            _ => tt,
        },
        TokenTree::Group(_, GroupType::Comment, _) => spacify_token_tree(tt),
        TokenTree::Group(trees, group, pair) => TokenTree::Group(
            trees.into_iter().map(prepare_token_tree).collect(),
            group,
            pair,
        ),
    }
}

pub fn parse(input: &str) -> Result<Prog, SyntaxError> {
    use log::info;
    let tt = create_token_tree(input).map_err(|_| SyntaxError::Custom {
        message: "Unknown lexer error".to_string(),
    })?;
    info!("parse::tt= {:?}", tt);
    let prepared_tt = prepare_token_tree(tt);
    let input = format!("{}", prepared_tt);

    crate::parser::ProgParser::new()
        // .parse(tokens.into_iter())
        .parse(&line_col::LineColLookup::new(&input), &input)
        .map_err(SyntaxError::from_parse_error)
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
    let formatted = result;
    println!(" * formatted:\n{}", formatted);
    assert_eq!(formatted, expected);
    tree
}

#[allow(unused_variables)]
pub fn assert_lex_roundtrip(input: &str, width: Option<usize>) -> TokenTree {
    assert_lex(input, input, width)
}

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
    log::info!(
        "\nassert_vm_eval(\"{}\", \"{}\")",
        input_prog,
        expected_result
    );
    let v1 = crate::vm::eval(input_prog).unwrap();
    let v2 = crate::vm::eval(expected_result).unwrap();
    assert_eq!(v1, v2)
}

pub fn assert_vm_fast_eval(input_prog: &str, expected_result: &str) {
    let v1 = crate::vm_fast::eval(input_prog).unwrap();
    let v2 = crate::vm_fast::eval(expected_result).unwrap();
    assert_eq!(v1, v2)
}

pub fn assert_vm_interruption(
    input_prog: &str,
    expected_interruption: &crate::vm_types::Interruption,
) {
    log::info!(
        "\nassert_vm_interruption(\"{:?}\", \"{:?}\")",
        input_prog,
        expected_interruption
    );
    match crate::vm::eval(input_prog) {
        Err(ref i) => assert_eq!(i, expected_interruption),
        Ok(ref v) => {
            unreachable!("expected Err({:?}), not Ok({:?})", expected_interruption, v)
        }
    }
}
