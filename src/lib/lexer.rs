use crate::ast::PrimType;
use logos::{Lexer, Logos, Span};

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
    //TODO
    Ok(TokenTree::Group(
        tokens
            .into_iter()
            .map(|(t, s)| TokenTree::Token(t, s))
            .collect(),
        GroupSort::Unenclosed,
        None,
    ))
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
    // #[regex(r"/\*[^]*", data)]
    #[regex(r"/\*", data)]
    BlockComment(Data),

    #[regex(r"//[^\n]*", data)]
    LineComment(Data),

    #[token("(", data!(GroupSort::Paren))]
    #[token("{", data!(GroupSort::Curly))]
    #[token("[", data!(GroupSort::Square))]
    // #[token("<", data!(GroupType::Angle))]
    Open((Data, GroupSort)),

    #[token(")", data!(GroupSort::Paren))]
    #[token("}", data!(GroupSort::Curly))]
    #[token("]", data!(GroupSort::Square))]
    // #[token(">", data!(GroupType::Angle))]
    Close((Data, GroupSort)),

    #[token(".", data)]
    Dot(Data),

    #[token(",", data!(Delim::Comma))]
    #[token(";", data!(Delim::Semi))]
    Delim((Data, Delim)),

    #[regex(r"[+\-*/%&|^!?:=<>@]+", data)]
    Symbol(Data),

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
            BlockComment(x) | LineComment(x) | Open((x, _)) | Close((x, _)) | Dot(x)
            | Symbol(x) | Ident(x) | Delim((x, _)) | Literal((x, _)) | Space(x) | Unknown(x) => {
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
                for t in trees {
                    if let Some(((open, _), (close, _))) = pair {
                        write!(f, "{}{}{}", open, t, close)?
                    }
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
