use crate::ast::{Dec, Delim, Exp, Exp_, Id, Type, Type_};

pub fn get_one<T>(d: Delim<T>) -> Result<Box<T>, Delim<T>> {
    if d.vec.len() == 1 && !d.has_trailing
    /* preserve trailing comma for parenthesized w/comma */
    {
        Ok(Box::new(d.vec.into_iter().nth(0).unwrap()))
    } else {
        Err(d)
    }
}

pub fn dec_into_exp(d: Dec) -> Exp {
    match d {
        Dec::Exp(e) => e,
        d => Exp::Block(Delim {
            vec: vec![d],
            has_trailing: false,
        }),
    }
}
