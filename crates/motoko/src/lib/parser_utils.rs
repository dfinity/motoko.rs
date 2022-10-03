use crate::{ast::{Dec, Dec_, Delim, Exp, Node, Source, Exp_, NodeData}, shared::Share};

pub fn get_one<T:Clone>(d: Delim<T>) -> Result<T, Delim<T>> {
    if d.vec.len() == 1 && !d.has_trailing {
        Ok(d.vec.into_iter().nth(0).unwrap())
    } else {
        Err(d)
    }
}

pub fn dec_node_into_exp(d: Dec_) -> Exp_ {
    match d.as_ref().0 {
        Dec::Exp(e) => e,
        _ => NodeData(Exp::Block(Delim {
            vec: vec![d].into(),
            has_trailing: false,
        }), d.1).share(),
    }
}

// pub fn node<T>(value: T, src: Source) -> Node<T> {
//     NodeData(value, src).share()
// }
