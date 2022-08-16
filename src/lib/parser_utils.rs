use crate::{
    ast::{Dec, Dec_, Delim, Exp, Exp_, Loc, Node, Source},
    lexer::create_token_vec,
    lexer_types::{Token, Tokens},
};

struct Lexer<'input> {
    tokens: Tokens,
    index: usize,
    input: &'input str,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            tokens: create_token_vec(input).unwrap(), ////
            index: 0,
            input,
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = &'input Loc<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.tokens.get(self.index) {
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }
}

pub fn get_one<T>(d: Delim<T>) -> Result<Box<T>, Delim<T>> {
    if d.vec.len() == 1 && !d.has_trailing
    /* preserve trailing comma for parenthesized w/comma */
    {
        Ok(Box::new(d.vec.into_iter().nth(0).unwrap()))
    } else {
        Err(d)
    }
}

pub fn dec_into_exp(d: Dec_) -> Exp_ {
    Loc(
        Box::new(match *d.0 {
            Dec::Exp(e) => e,
            _ => Exp::Block(Delim {
                vec: vec![d],
                has_trailing: false,
            }),
        }),
        d.1,
    )
}

pub fn node<T>(value: T, src: Source) -> Node<T> {
    Loc(Box::new(value), src)
}
