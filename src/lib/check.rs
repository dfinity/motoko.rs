use line_col::LineColLookup;

use crate::ast::{Loc, Prog};
use crate::format::{format_one_line, format_pretty};
use crate::lexer::create_token_tree;
use crate::lexer_types::{GroupType, Token, TokenTree};
use crate::vm_types::Interruption;

// TODO: refactor lalrpop lexer details
// impl crate::parser::__ToTriple for Loc<Token> {
//     fn to_triple(
//         Loc(token, src): Self,
//     ) -> Result<
//         (crate::ast::Source, Token, crate::ast::Source),
//         lalrpop_util::ParseError<crate::ast::Source, Token, &'static str>,
//     > {
//         Ok((src.clone(), token, src))
//     }
// }

fn filter_token_tree(tt: TokenTree) -> Option<TokenTree> {
    match tt {
        TokenTree::Token(Loc(ref t, _)) => match t {
            Token::Space(_) | Token::LineComment(_) | Token::MultiLineSpace(_) | Token::Error => {
                None
            }
            _ => Some(tt),
        },
        TokenTree::Group(_, GroupType::Comment, _) => None,
        TokenTree::Group(trees, group, pair) => Some(TokenTree::Group(
            trees.into_iter().filter_map(filter_token_tree).collect(),
            group,
            pair,
        )),
    }
}

pub fn parse(input: &str) -> Result<Prog, ()> {
    let tt = create_token_tree(input)?;
    println!("TT: {:?}", tt);
    // let tokens = filter_token_tree(tt)
    //     .map(TokenTree::flatten)
    //     .unwrap_or_else(|| vec![]);
    let input = filter_token_tree(tt)
        .map(|tt| format!("{}", tt))
        .unwrap_or("".to_string());

    Ok(crate::parser::ProgParser::new()
        // .parse(tokens.into_iter())
        .parse(LineColLookup::new(&input), &input)
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

pub fn assert_vm_interruption(input_prog: &str, expected_interruption: &Interruption) {
    println!(
        "\nassert_vm_interruption(\"{:?}\", \"{:?}\")",
        input_prog, expected_interruption
    );
    match crate::vm::eval_(input_prog) {
        Err(ref i) => assert_eq!(i, expected_interruption),
        Ok(ref v) => {
            unreachable!("expected Err({:?}), not Ok({:?})", expected_interruption, v)
        }
    }
}
