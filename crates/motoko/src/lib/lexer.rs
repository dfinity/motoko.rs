use crate::{
    ast::{Loc, Source, Span},
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
    "composite",
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
    let mut tokens = vec![];
    // Tokenize source code (excluding comments)
    let tokenize_source = |tokens: &mut Tokens, input: &str| {
        tokens.extend(Token::lexer(input).spanned().map(|(t, span)| {
            // Convert errors to the `Unknown` token type
            let t = match t {
                Token::Error => Token::Unknown(input[span.clone()].to_string()),
                t => t,
            };
            let (line, col) = line_col.get(span.start);
            Loc(t, Source::Known { span, line, col })
        }));
    };
    let comment_spans = find_comment_spans(input);
    // Tokenize everything before the first comment (or end of input)
    tokenize_source(
        &mut tokens,
        &input[..comment_spans.get(0).map(|s| s.start).unwrap_or(input.len())],
    );
    for (i, span) in comment_spans.iter().enumerate() {
        // Add comment token
        let comment = input[span.clone()].to_string();
        let (line, col) = line_col.get(span.start);
        tokens.push(Loc(
            if comment.starts_with("//") {
                Token::LineComment(comment)
            } else {
                Token::BlockComment(comment)
            },
            Source::Known {
                span: span.clone(),
                line,
                col,
            },
        ));
        // Tokenize source after comment
        tokenize_source(
            &mut tokens,
            &input[span.end
                ..comment_spans
                    .get(i + 1)
                    .map(|s| s.start)
                    .unwrap_or(input.len())],
        );
    }
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
    let mut i = start + 1;
    let mut depth: usize = 0;
    while i < tokens.len() {
        let Loc(t, _) = &tokens[i];

        if let Token::Open((_, g)) = t {
            if g == sort {
                depth += 1;
            } else if g == &GroupType::Comment {
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
    None
}

pub fn find_comment_spans(input: &str) -> Vec<Span> {
    let mut iter = input.char_indices().peekable();
    let mut results = vec![];
    let mut block_start: Option<usize> = None;
    let mut nest_depth = 0;
    while let Some((i, c)) = iter.next() {
        match c {
            '"' | '\'' if nest_depth == 0 => {
                // String literal
                let mut escaped = false;
                while let Some((_, c1)) = iter.next() {
                    if escaped {
                        // Skip escaped character
                        escaped = false;
                    } else if c1 == '\\' {
                        // Escape next character
                        escaped = true;
                    } else if c1 == c {
                        // End string literal
                        break;
                    }
                }
            }
            '/' => match iter.peek() {
                Some((_, '*')) => {
                    // Start block comment
                    iter.next().unwrap();
                    if nest_depth == 0 {
                        block_start = Some(i);
                    }
                    nest_depth += 1;
                }
                Some((_, '/')) if nest_depth == 0 => {
                    // Line comment
                    loop {
                        match iter.next() {
                            Some((j, '\n')) => {
                                // Newline
                                results.push(i..j);
                                break;
                            }
                            None => {
                                // End of input
                                results.push(i..input.len());
                                break;
                            }
                            _ => (),
                        }
                    }
                }
                _ => (),
            },
            '*' if nest_depth > 0 => {
                if let Some((_, '/')) = iter.peek() {
                    // End block comment
                    nest_depth -= 1;
                    if nest_depth == 0 {
                        let (end, _) = iter.next().unwrap();
                        results.push(block_start.unwrap()..end + 1);
                        block_start = None;
                    }
                }
            }
            _ => (),
        }
    }
    results
}
