use crate::ast::PrimType;
use logos::{Lexer, Logos, Span};

pub fn create_lex_tree(input: &str) -> Result<TokenTree, ()> {
    group(create_token_vec(input)?)
}

pub fn create_token_vec(input: &str) -> Result<Tokens, ()> {
    let tokens = Token::lexer(input).spanned().collect();
    Ok(tokens)
}

pub fn flatten(tt: TokenTree) -> Tokens {
    let mut vec = vec![];
    flatten_(tt, &mut vec);
    vec
}

pub fn group(tokens: Tokens) -> Result<TokenTree, ()> {
    //TODO
    Ok(TokenTree::Group(
        tokens
            .into_iter()
            .map(|(t, s)| TokenTree::Token(t, s))
            .collect(),
    ))
}

fn flatten_(tt: TokenTree, tokens: &mut Tokens) {
    match tt {
        TokenTree::Token(t, s) => tokens.push((t, s)),
        TokenTree::Group(tt) => {
            for t in tt {
                flatten_(t, tokens)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GroupType {
    Paren,
    Curly,
    Square,
    Angle,
}

impl GroupType {
    pub fn left(&self) -> &'static str {
        match self {
            GroupType::Paren => "(",
            GroupType::Curly => "{",
            GroupType::Square => "[",
            GroupType::Angle => "<",
        }
    }
    pub fn right(&self) -> &'static str {
        match self {
            GroupType::Paren => ")",
            GroupType::Curly => "}",
            GroupType::Square => "]",
            GroupType::Angle => ">",
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
    BlockCommentPart(Data),

    #[regex(r"//[^\n]*", data)]
    LineComment(Data),

    #[token("(", data!(GroupType::Paren))]
    #[token("{", data!(GroupType::Curly))]
    #[token("[", data!(GroupType::Square))]
    // #[token("<", data!(GroupType::Angle))]
    Open((Data, GroupType)),

    #[token(")", data!(GroupType::Paren))]
    #[token("}", data!(GroupType::Curly))]
    #[token("]", data!(GroupType::Square))]
    // #[token(">", data!(GroupType::Angle))]
    Close((Data, GroupType)),

    #[token(".", data)]
    Dot(Data),

    #[regex(r"[+\-*/%&|^!?=<>@]+", data)]
    Symbol(Data),

    #[regex(r"[a-zA-Z][a-zA-Z_0-9]*", data)]
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

    // #[regex(r".", data)]
    // Unknown(Data),
    #[error]
    Error,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TokenTree {
    Token(Token, Span),
    Group(Vec<TokenTree>),
}

impl std::fmt::Debug for TokenTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenTree::*;
        match self {
            Token(t, s) => write!(f, "<{:?} {:?}>", t, s),
            Group(tt) => {
                write!(f, "(")?;
                for (i, t) in tt.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", t)?
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
            Group(tt) => {
                for t in tt {
                    write!(f, "{}", t)?
                }
                Ok(())
            }
        }
    }
}

fn get_data<'a>(token: &'a Token) -> Result<&'a Data, ()> {
    // it works if it works
    use Token::*;
    match token {
        Error => Err(()),
        BlockCommentPart(x) | LineComment(x) | Open((x, _)) | Close((x, _)) | Dot(x)
        | Symbol(x) | Ident(x) | Literal((x, _)) | Space(x) => Ok(x),
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{}", get_data(self).unwrap())
        write!(f, "{}", get_data(self).map_err(|_| std::fmt::Error)?)
    }
}
