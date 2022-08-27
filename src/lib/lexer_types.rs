use crate::{
    ast::{Loc, PrimType},
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
#[serde(tag = "token_type", content = "data")]
pub enum Token {
    #[regex(r"//[^\n]*", data)]
    LineComment(Data),

    #[token("(", data!(GroupType::Paren))]
    #[token("{", data!(GroupType::Curly))]
    #[token("[", data!(GroupType::Square))]
    #[token("<", data!(GroupType::Angle))]
    #[token("/*", data!(GroupType::BlockComment))]
    Open((Data, GroupType)),

    #[token(")", data!(GroupType::Paren))]
    #[token("}", data!(GroupType::Curly))]
    #[token("]", data!(GroupType::Square))]
    #[token(">", data!(GroupType::Angle))]
    #[token("*/", data!(GroupType::BlockComment))]
    Close((Data, GroupType)),

    #[token(".", data)]
    Dot(Data),

    #[token(":", data)]
    Colon(Data),

    #[token("=", data)]
    Assign(Data),

    #[token(",", data!(Delim::Comma))]
    #[token(";", data!(Delim::Semi))]
    Delim((Data, Delim)),

    #[token("?", data)]
    #[token("!", data)]
    #[token("^", data)]
    #[token("<:", data)]
    #[regex(r"(\+|-|\*\*?|/|&|\|)%?=?", data)]
    #[regex(r"([\^]|<<>?|( |<)>>|#)(|=)", data)]
    #[regex("[:%!=<>]=", data)]
    #[regex(r" [<>] ", data)]
    Operator(Data),

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", data)]
    Ident(Data),

    #[token("_", data)]
    Wild(Data),

    #[token("true", data!(PrimType::Bool))]
    #[token("false", data!(PrimType::Bool))]
    #[token("null", data!(PrimType::Null))]
    #[regex(r"[0-9]([0-9_]*[0-9]+)?", data!(PrimType::Nat))]
    #[regex(r"0x[0-9a-fA-F]+", data!(PrimType::Nat))] // hexadecimal
    // #[regex(r"[+-][0-9]([0-9_]*[0-9]+)?", data!(PrimType::Int))]
    #[regex(r"[0-9]([0-9_]*[0-9])?[Ee][0-9]([0-9_]*[0-9])?", data!(PrimType::Float))] // exponential without decimal
    #[regex(r"[0-9]([0-9_]*[0-9])?\.([0-9]([0-9_]*[0-9])?)?([Ee][0-9]([0-9_]*[0-9])?)?", data!(PrimType::Float))] // exponential with decimal
    #[regex(r"'(?:[^\\']|\\.)*'", data!(PrimType::Char))]
    #[regex(r#""(?:[^\\"]|\\.)*""#, data!(PrimType::Text))]
    Literal((Data, PrimType)),

    #[regex(r"[ \t]+", data)]
    Space(Data),

    #[token("\n", data)]
    Line(Data),

    #[regex(r"[ \t\n]*\n[ \t]*\n\s*", data)]
    MultiLine(Data),

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
            LineComment(s) | Open((s, _)) | Close((s, _)) | Dot(s) | Colon(s) | Assign(s)
            | Operator(s) | Ident(s) | Wild(s) | Delim((s, _)) | Literal((s, _)) | Space(s)
            | Line(s) | MultiLine(s) | Unknown(s) => Ok(s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum GroupType {
    Unenclosed,
    Paren,
    Curly,
    Square,
    Angle,
    BlockComment,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Delim {
    Comma,
    Semi,
}

impl GroupType {
    pub fn token_pair(&self) -> Option<(Token, Token)> {
        match self {
            GroupType::Unenclosed => None,
            _ => Some((
                Token::Open((self.left_str().to_string(), self.clone())),
                Token::Close((self.right_str().to_string(), self.clone())),
            )),
        }
    }

    pub fn left_str(&self) -> &'static str {
        match self {
            GroupType::Unenclosed => "",
            GroupType::Paren => "(",
            GroupType::Curly => "{",
            GroupType::Square => "[",
            GroupType::Angle => "<",
            GroupType::BlockComment => "/*",
        }
    }
    pub fn right_str(&self) -> &'static str {
        match self {
            GroupType::Unenclosed => "",
            GroupType::Paren => ")",
            GroupType::Curly => "}",
            GroupType::Square => "]",
            GroupType::Angle => ">",
            GroupType::BlockComment => "*/",
        }
    }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "token_tree_type", content = "data")]
pub enum TokenTree {
    Token(Token_),
    Group(Vec<TokenTree>, GroupType, Option<(Token_, Token_)>),
}

impl TokenTree {
    pub fn flatten(self) -> Tokens {
        let mut vec = vec![];
        Self::flatten_(self, &mut vec);
        vec
    }

    fn flatten_(value: Self, tokens: &mut Tokens) {
        match value {
            TokenTree::Token(t) => tokens.push(t),
            TokenTree::Group(trees, _, pair) => {
                if let Some((ref open, _)) = pair {
                    tokens.push(open.clone()); // TODO no clone?
                }
                for tt in trees {
                    Self::flatten_(tt, tokens);
                }
                if let Some((_, ref close)) = pair {
                    tokens.push(close.clone()); // TODO no clone?
                }
            }
        }
    }
}

impl std::fmt::Debug for TokenTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenTree::*;
        match self {
            Token(token) => {
                write!(f, "{:?}", token)
            }
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
                } else {
                    for t in trees {
                        write!(f, "{}", t)?;
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
