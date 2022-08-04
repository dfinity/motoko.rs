use crate::ast::{Id, Dec, Delim, Exp, Exp_, Type, Type_};

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

pub fn unit_exp() -> Exp {
    Exp::Block(Delim{vec:vec!(), has_trailing: false})
}

pub fn annot_exp(e: Exp, t: Option<Type_>) -> Exp {
    match t {
        Some(t) => Exp::Annot(Box::new(e), t),
        None => e,
    }
}

pub fn annot_exp_(e: Exp_, t: Option<Type_>) -> Exp_ {
    Box::new(annot_exp(*e, t))
}

pub fn varexp_into_id(e: &Exp) -> Id {
    match e {
        Exp::Var(x) => x.clone(),
        _ => unreachable!(),
    }
}
