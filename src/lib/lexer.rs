use crate::ast::PrimType;
use logos::{Lexer, Logos, Span};

const KEYWORDS: &'static [&'static str] = &[
    "public",
    "actor",
    "let",
    "let",
    "let",
    "let",
    "let",
    "let",
    "let" // TODO
];

pub fn is_keyword(ident: &str)->bool {
    return KEYWORDS.contains(&ident);
}

pub type LexResult<T> = Result<T, ()>;

pub fn create_token_tree(input: &str) -> LexResult<TokenTree> {
    group(create_token_vec(input)?)
}

pub fn create_token_vec(input: &str) -> LexResult<Tokens> {
    let tokens = Token::lexer(input)
        .spanned()
        .map(|(t, s)| {
            let t = match t {
                Token::Error => Token::Unknown(input[s.clone()].to_string()),
                t => t,
            };
            (t, s)
        })
        .collect();
    Ok(tokens)
}

// pub fn simplify_token_vec(tokens: Tokens) -> LexResult<Tokens> {
//     fn should_keep(token: &Token) -> LexResult<bool> {
//         if let Token::Error = token {
//             Err(())
//         } else {
//             Ok(match token {
//                 Token::Space(_) => false,
//                 _ => true,
//             })
//         }
//     }
//     let mut results = vec![];
//     let mut previous: Option<(Token, Span)> = None;
//     for next in tokens {
//         if let Some(previous) = previous {
//             use Token::*;
//             let keep = match (&previous.0, &next.0) {
//                 // TODO
//                 (BlockComment(a), BlockComment(b)) => Some(BlockComment(format!("{}{}", a, b))),
//                 (Space(a), Space(b)) => Some(Space(format!("{}{}", a, b))),
//                 (_, t) => {
//                     if should_keep(t) {
//                         Some(t)
//                     } else {
//                         None
//                     }
//                 }
//             };
//             if let Some(keep) = keep {
//                 results.push(keep);
//             }
//         } else {
//             if should_keep(&next.0)? {
//                 results.push(next);
//             }
//         };
//         previous = Some(next);
//     }
//     Ok(results)
// }

pub fn group(tokens: Tokens) -> LexResult<TokenTree> {
    Ok(TokenTree::Group(
        group_(&tokens)?,
        GroupSort::Unenclosed,
        None,
    ))
}

fn group_(tokens: &[(Token, Span)]) -> LexResult<Vec<TokenTree>> {
    let mut result = vec![];
    let mut i = 0;
    while i < tokens.len() {
        let (t, s) = &tokens[i];
        result.push(match t {
            Token::Open((_, g)) => {
                let start = i;
                if let Some(end) = find_closing(g, tokens, i) {
                    i = end;
                    // println!("{}  ---  {}", tokens[start].0, tokens[end].0); //////////
                    TokenTree::Group(
                        group_(&tokens[start + 1..i])?,
                        g.clone(),
                        Some(((t.clone(), s.clone()), tokens[i].clone())),
                    )
                } else {
                    // Extraneous opening token
                    TokenTree::Token(t.clone(), s.clone())
                }
            }
            t => TokenTree::Token(t.clone(), s.clone()),
        });
        i += 1;
    }
    Ok(result)
}

fn find_closing(sort: &GroupSort, tokens: &[(Token, Span)], start: usize) -> Option<usize> {
    // println!(">  {:?} {}", sort, start);///////
    let mut i = start + 1;
    let mut depth: usize = 0;
    while i < tokens.len() {
        let (t, _) = &tokens[i];

        if let Token::Open((_, g)) = t {
            if g == sort {
                depth += 1;
            } else if g == &GroupSort::Comment {
                // Skip depth check in block comments
                if let Some(j) = find_closing(&g, tokens, i) {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GroupSort {
    Unenclosed,
    Paren,
    Curly,
    Square,
    Angle,
    Comment,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Delim {
    Comma,
    Semi,
}

impl GroupSort {
    pub fn token_pair(&self) -> Option<(Token, Token)> {
        match self {
            GroupSort::Unenclosed => None,
            _ => Some((
                Token::Open((self.left_str().to_string(), self.clone())),
                Token::Close((self.right_str().to_string(), self.clone())),
            )),
        }
    }

    pub fn left_str(&self) -> &'static str {
        match self {
            GroupSort::Unenclosed => "",
            GroupSort::Paren => "(",
            GroupSort::Curly => "{",
            GroupSort::Square => "[",
            GroupSort::Angle => "<",
            GroupSort::Comment => "/*",
        }
    }
    pub fn right_str(&self) -> &'static str {
        match self {
            GroupSort::Unenclosed => "",
            GroupSort::Paren => ")",
            GroupSort::Curly => "}",
            GroupSort::Square => "]",
            GroupSort::Angle => ">",
            GroupSort::Comment => "*/",
        }
    }
}

type Tokens = Vec<(Token, Span)>;

type Data = String;

fn data(lex: &mut Lexer<Token>) -> Option<Data> {
    Some(lex.slice().to_string())
}

macro_rules! data {
    ($e:expr) => {
        |lex| Some((data(lex)?, $e))
    };
}

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // BlockComment(Data),
    #[regex(r"//[^\n]*", data)]
    LineComment(Data),

    #[token("(", data!(GroupSort::Paren))]
    #[token("{", data!(GroupSort::Curly))]
    #[token("[", data!(GroupSort::Square))]
    // #[token("<", data!(GroupType::Angle))]
    #[token("/*", data!(GroupSort::Comment))]
    Open((Data, GroupSort)),

    #[token(")", data!(GroupSort::Paren))]
    #[token("}", data!(GroupSort::Curly))]
    #[token("]", data!(GroupSort::Square))]
    // #[token(">", data!(GroupType::Angle))]
    #[token("*/", data!(GroupSort::Comment))]
    Close((Data, GroupSort)),

    #[token(".", data)]
    Dot(Data),

    #[token(":", data)]
    Colon(Data),

    #[token("=", data)]
    Assign(Data),

    #[token(",", data!(Delim::Comma))]
    #[token(";", data!(Delim::Semi))]
    Delim((Data, Delim)),

    #[regex(r"[+\-*/%&|^!?:=<>@]+", data)]
    Operator(Data),

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", data)]
    Ident(Data),

    #[token("true", data!(PrimType::Bool))]
    #[token("false", data!(PrimType::Bool))]
    #[token("null", data!(PrimType::Null))]
    #[regex(r"[0-9]+([0-9_]*[0-9]+)?", data!(PrimType::Nat))]
    #[regex(r"-[0-9]+([0-9_]*[0-9]+)?", data!(PrimType::Int))]
    #[regex(r"-?[0-9]+([0-9_]*[0-9]+)\.[0-9]*([0-9_]*[0-9]+)?", data!(PrimType::Float))]
    #[regex(r"'([^']|')'", data!(PrimType::Char))]
    #[regex(r#""(?:[^\\"]|\.)*""#, data!(PrimType::Text))]
    Literal((Data, PrimType)),

    #[regex(r"\s+", data)]
    Space(Data),

    Unknown(Data),

    #[error]
    Error,
}

impl Token {
    pub fn data<'a>(&'a self) -> LexResult<&'a Data> {
        // it works if it works
        use Token::*;
        match self {
            Error => Err(()),
            LineComment(x) | Open((x, _)) | Close((x, _)) | Dot(x) | Colon(x) | Assign(x)
            | Operator(x) | Ident(x) | Delim((x, _)) | Literal((x, _)) | Space(x) | Unknown(x) => {
                Ok(x)
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TokenTree {
    Token(Token, Span),
    Group(
        Vec<TokenTree>,
        GroupSort,
        Option<((Token, Span), (Token, Span))>,
    ),
}

impl TokenTree {
    pub fn flatten(self) -> Tokens {
        let mut vec = vec![];
        self.flatten_(&mut vec);
        vec
    }

    fn flatten_(self, tokens: &mut Tokens) {
        match self {
            TokenTree::Token(t, s) => tokens.push((t, s)),
            TokenTree::Group(trees, _, pair) => {
                for tt in trees {
                    if let Some((open, close)) = /* TODO no clone */ pair.clone() {
                        tokens.push(open);
                        tt.flatten_(tokens);
                        tokens.push(close);
                    } else {
                        tt.flatten_(tokens);
                    }
                }
            }
        }
    }
}

impl std::fmt::Debug for TokenTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenTree::*;
        match self {
            Token(t, s) => write!(f, "<{:?} {:?}>", t, s),
            Group(trees, sort, _pair) => {
                write!(f, "{:?}(", sort)?;
                for (i, tt) in trees.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", tt)?
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

impl std::fmt::Display for TokenTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenTree::*;
        match self {
            Token(t, _) => write!(f, "{}", t),
            Group(trees, _, pair) => {
                if let Some(((open, _), (close, _))) = pair {
                    write!(f, "{}", open)?;
                    for t in trees {
                        write!(f, "{}", t)?;
                    }
                    write!(f, "{}", close)?;
                }
                Ok(())
            }
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{}", get_data(self).unwrap())
        write!(f, "{}", self.data().map_err(|_| std::fmt::Error)?)
    }
}
