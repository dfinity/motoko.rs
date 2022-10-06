use crate::{
    ast::{Loc, Source},
    lexer_types::{GroupType, Token, TokenTree, Tokens},
};
use line_col::LineColLookup;
use logos::Logos;

pub const KEYWORDS: &[&str] = &[
    "actor",
    "and",
    "async",
    "assert",
    "await",
    "break",
    "case",
    "catch",
    "class",
    "continue",
    "debug",
    "else",
    "false",
    "for",
    "func",
    "if",
    "in",
    "import",
    "module",
    "not",
    "null",
    "object",
    "or",
    "label",
    "let",
    "loop",
    "private",
    "public",
    "return",
    "shared",
    "try",
    "throw",
    "debug_show",
    "query",
    "switch",
    "true",
    "type",
    "var",
    "while",
    "stable",
    "flexible",
    "system",
    "ignore",
    "to_candid",
    "from_candid",
    "with",
];

pub fn is_keyword(ident: &str) -> bool {
    KEYWORDS.contains(&ident)
}

pub type LexResult<T> = Result<T, ()>;

pub fn create_token_tree(input: &str) -> LexResult<TokenTree> {
    group(create_token_vec(input)?)
}

pub fn create_token_vec(input: &str) -> LexResult<Tokens> {
    let line_col = LineColLookup::new(input);
    let tokens = Token::lexer(input)
        .spanned()
        .map(|(t, span)| {
            let t = match t {
                Token::Error => Token::Unknown(input[span.clone()].to_string()),
                t => t,
            };
            let (line, col) = line_col.get(span.start);
            Loc(t, Source::Known { span, line, col })
        })
        .collect();
    Ok(tokens)
}

pub fn group(tokens: Tokens) -> LexResult<TokenTree> {
    Ok(TokenTree::Group(
        group_(&tokens)?,
        GroupType::Unenclosed,
        None,
    ))
}

fn group_(tokens: &[Loc<Token>]) -> LexResult<Vec<TokenTree>> {
    let mut result = vec![];
    let mut i = 0;
    while i < tokens.len() {
        let token = &tokens[i];
        result.push(match &token.0 {
            Token::Open((_, g)) => {
                let start = i;
                if let Some(end) = find_closing(g, tokens, i) {
                    i = end;
                    // println!("{}  ---  {}", tokens[start].0, tokens[end].0); //////////
                    TokenTree::Group(
                        group_(&tokens[start + 1..i])?,
                        g.clone(),
                        Some((token.clone(), tokens[i].clone())),
                    )
                } else {
                    // Extraneous opening token
                    TokenTree::Token(token.clone())
                }
            }
            _ => TokenTree::Token(token.clone()),
        });
        i += 1;
    }
    Ok(result)
}

fn find_closing(sort: &GroupType, tokens: &[Loc<Token>], start: usize) -> Option<usize> {
    // println!(">  {:?} {}", sort, start);///////
    let mut i = start + 1;
    let mut depth: usize = 0;
    while i < tokens.len() {
        let Loc(t, _) = &tokens[i];

        if let Token::Open((_, g)) = t {
            if g == sort {
                depth += 1;
            } else if
            /* sort!=&GroupType::Comment */
            g == &GroupType::BlockComment {
                // Skip depth check in block comments
                if let Some(j) = find_closing(g, tokens, i) {
                    i = j;
                }
            }
        };
        if let Token::Close((_, g)) = t {
            if g == sort {
                if depth == 0 {
                    return Some(i);
                }
                depth -= 1;
            }
        };
        i += 1;
    }
    // println!("<  {:?} {}", sort, start);///////
    None
}
