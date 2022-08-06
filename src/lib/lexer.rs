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

type Tokens = Vec<(Token, Span)>;

type Data = String;

fn data(lex: &mut Lexer<Token>) -> Option<Data> {
    Some(lex.slice().to_string())
}

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // #[token("/*", data)]
    // StartBlockComment(Data),
    // #[token("*/", data)]
    // EndBlockComment(Data),
    #[regex(r"/*", data)]
    BlockCommentPart(Data),

    #[regex(r"//[^\n]\n", data)]
    LineComment(Data),

    #[token("{", data)]
    LBrace(Data),
    #[token("}", data)]
    RBrace(Data),

    #[token("(", data)]
    LParen(Data),
    #[token(")", data)]
    RParen(Data),

    #[token("[", data)]
    LSquareBracket(Data),
    #[token("]", data)]
    RSquareBracket(Data),

    #[token("<", data)]
    LAngleBracket(Data),
    #[token(">", data)]
    RAngleBracket(Data),

    #[token(".", data)]
    Dot(Data),

    #[regex(r"[+\-*/%&|^!?=<>@]+", data)]
    Symbol(Data),

    #[regex(r"[a-zA-Z][a-zA-Z_0-9]*", data)]
    Ident(Data),

    #[regex(r"[0-9]+([0-9_]*[0-9]+)?", data)]
    Nat(Data),
    #[regex(r"-[0-9]+([0-9_]*[0-9]+)?", data)]
    Int(Data),
    #[regex(r"-?[0-9]+([0-9_]*[0-9]+)\.[0-9]*([0-9_]*[0-9]+)?", data)]
    Float(Data),
    #[regex(r"'([^']|')'", data)]
    Char(Data),
    #[regex(r#""(?:[^\\"]|\.)*""#, data)]
    String(Data),

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
        BlockCommentPart(x)|/* StartBlockComment(x) | EndBlockComment(x) | */ LineComment(x) | LBrace(x) | RBrace(x)
        | LParen(x) | RParen(x) | LSquareBracket(x) | RSquareBracket(x) | LAngleBracket(x)
        | RAngleBracket(x) | Dot(x) | Symbol(x) | Ident(x) | Space(x) | Nat(x) | Int(x)
        | Float(x) | Char(x) | String(x) => Ok(x),
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{}", get_data(self).unwrap())
        write!(f, "{}", get_data(self).map_err(|_| std::fmt::Error)?)
    }
}
