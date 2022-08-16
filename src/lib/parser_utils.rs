use crate::ast::{Dec, Dec_, Delim, Exp, Loc, Node, Source};

pub fn get_one<T>(d: Delim<T>) -> Result<T, Delim<T>> {
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
