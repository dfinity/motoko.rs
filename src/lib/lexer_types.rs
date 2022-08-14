use crate::{
    ast::{Loc, PrimType, Source},
    lexer::LexResult,
};
use logos::{Lexer, Logos};
use serde::{Deserialize, Serialize};

type Data = String;

fn data(lex: &mut Lexer<Token>) -> Option<Data> {
    Some(lex.slice().to_string())
}

macro_rules! data {
    ($e:expr) => {
        |lex| Some((data(lex)?, $e))
    };
}

pub type Token_ = Loc<Token>;
pub type Tokens = Vec<Token_>;

#[derive(Logos, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

    #[regex(r"[+\-*/%&|^!?:<>@#]+=?", data)]
    #[token("==", data)]
    // #[regex(r" >>=?", data)]
    Operator(Data),

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", data)]
    Ident(Data),

    #[token("true", data!(PrimType::Bool))]
    #[token("false", data!(PrimType::Bool))]
    #[token("null", data!(PrimType::Null))]
    #[regex(r"[0-9]+([0-9_]*[0-9]+)?", data!(PrimType::Nat))]
    #[regex(r"[+-]+[0-9]+([0-9_]*[0-9]+)?", data!(PrimType::Int))]
    #[regex(r"-?[0-9]+([0-9_]*[0-9]+)\.[0-9]*([0-9_]*[0-9]+)?", data!(PrimType::Float))]
    #[regex(r"'([^']|')'", data!(PrimType::Char))]
    #[regex(r#""(?:[^\\"]|\.)*""#, data!(PrimType::Text))]
    Literal((Data, PrimType)),

    #[regex(r"\s+", data)]
    Space(Data),

    #[regex(r"[^\S\n]*\n[^\S\n]*\n\s*", data)]
    MultiLineSpace(Data),

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
            | Operator(x) | Ident(x) | Delim((x, _)) | Literal((x, _)) | Space(x)
            | MultiLineSpace(x) | Unknown(x) => Ok(x),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum GroupSort {
    Unenclosed,
    Paren,
    Curly,
    Square,
    Angle,
    Comment,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TokenTree {
    Token(Token_),
    Group(Vec<TokenTree>, GroupSort, Option<(Token_, Token_)>),
}

impl TokenTree {
    pub fn flatten(self) -> Tokens {
        let mut vec = vec![];
        self.flatten_(&mut vec);
        vec
    }

    fn flatten_(self, tokens: &mut Tokens) {
        match self {
            TokenTree::Token(t) => tokens.push(t),
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
            Token(Loc(t, s)) => match s {
                Source::Known { line, col, .. } => write!(f, "<{:?} {}:{}>", t, line, col),
                Source::Unknown => write!(f, "<{:?}>", t),
            },
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
            Token(t) => write!(f, "{}", t.0),
            Group(trees, _, pair) => {
                if let Some((Loc(open, _), Loc(close, _))) = pair {
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
