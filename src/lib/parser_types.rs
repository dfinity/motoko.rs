use serde::{Deserialize, Serialize};

// TODO: move?
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum SyntaxError {
    InvalidToken {
        location: usize,
    },
    UnrecognizedEOF {
        location: usize,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: String,
        start: usize,
        end: usize,
        expected: Vec<String>,
    },
    ExtraToken {
        token: String,
        start: usize,
        end: usize,
    },
    Custom(String),
}

impl SyntaxError {
    pub fn from_parse_error<'input>(
        err: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'input>, &'static str>,
    ) -> SyntaxError {
        use lalrpop_util::ParseError::*;
        match err {
            InvalidToken { location } => Self::InvalidToken { location },
            UnrecognizedEOF { location, expected } => Self::UnrecognizedEOF { location, expected },
            UnrecognizedToken { token, expected } => Self::UnrecognizedToken {
                token: token.1.to_string(),
                start: token.0,
                end: token.2,
                expected,
            },
            ExtraToken { token } => Self::ExtraToken {
                token: token.1.to_string(),
                start: token.0,
                end: token.2,
            },
            User { error } => Self::Custom(error.to_owned()),
        }
    }
}
