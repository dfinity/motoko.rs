use crate::ast::Literal;
use eq_float::F64;
use im_rc::vector;
use im_rc::HashMap;
use im_rc::Vector;
use num_bigint::{BigInt, BigUint, ParseBigIntError};

/// Permit sharing, and fast concats.
pub type Text = Vector<String>;

#[derive(Debug, Clone)] // , PartialEq, Eq
pub struct Mut(bool);

#[derive(Debug, Clone)] // , PartialEq, Eq
pub struct FieldValue {
    pub mut_: Mut,
    pub value: Value,
}

#[derive(Debug, Clone)] // , PartialEq, Eq
pub enum Value {
    Null,
    Bool(bool),
    Unit,
    Nat(BigUint),
    Int(BigInt),
    Float(F64), // TODO: test NaN equality compared to Motoko
    Char(char),
    Text(Text),
    Blob(Vec<u8>),
    Array(Mut, Vector<Value>),
    Tuple(Vector<Value>),
    Object(HashMap<String, FieldValue>),
}

impl Value {
    pub fn from_literal(l: Literal) -> Result<Value, ParseBigIntError> {
        use Value::*;
        Ok(match l {
            Literal::Null => Null,
            Literal::Bool(b) => Bool(b),
            Literal::Unit => Unit,
            Literal::Nat(n) => Nat(n.parse()?),
            Literal::Int(i) => Int(i.parse()?),
            Literal::Text(s) => Text(vector![s]),
            Literal::Blob(v) => Blob(v),
            _ => unimplemented!(),
        })
    }
}
