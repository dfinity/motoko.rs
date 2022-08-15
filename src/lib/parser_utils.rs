use crate::ast::{Dec, Dec_, Delim, Exp, Exp_, Loc, Source, Node};

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

pub fn loc_box<T>(value: T) -> Node<T> {
    Loc(Box::new(value), Source::Unknown) // TODO: pass source
}
