use crate::{
    ast::{Dec, Dec_, Delim, Exp, Loc, Node, Source},
    lexer::create_token_vec,
    lexer_types::{Token, Tokens},
};

pub struct Lexer<'input> {
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
    type Item = (Source, Token, Source);

    fn next(&mut self) -> Option<Self::Item> {
        let Loc(token, src) = self.tokens.get(self.index)?;
        self.index += 1;
        Some((src.clone(), token.clone(), src.clone())) // TODO: optimize?
    }
}

pub fn get_one<T>(d: Delim<T>) -> Result<T, Delim<T>> {
    /* preserve trailing delim for parenthesized w/delim */
    if d.vec.len() == 1 && !d.has_trailing {
        Ok(d.vec.into_iter().nth(0).unwrap())
    } else {
        Err(d)
    }
}

pub fn dec_node_into_exp(d: Dec_) -> Exp {
    match *d.0 {
        Dec::Exp(e) => e,
        _ => Exp::Block(Delim {
            vec: vec![d],
            has_trailing: false,
        }),
    }
}

pub fn node<T>(value: T, src: Source) -> Node<T> {
    Loc(Box::new(value), src)
}
