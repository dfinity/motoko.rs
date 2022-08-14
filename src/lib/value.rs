use crate::ast::{Dec, Decs, Exp, Id_, Literal};
use im_rc::vector;
use im_rc::HashMap;
use im_rc::Vector;
use num_bigint::{BigInt, BigUint, ParseBigIntError};
use serde::{Deserialize, Serialize};
// use float_cmp::ApproxEq; // in case we want to implement the `Eq` trait for `Value`

/// Permit sharing, and fast concats.
pub type Text = Vector<String>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Mut(bool);

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FieldValue {
    pub mut_: Mut,
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Value {
    Null,
    Bool(bool),
    Unit,
    Nat(BigUint),
    Int(BigInt),
    //    Float(f64),
    Char(char),
    Text(Text),
    Blob(Vec<u8>),
    Array(Mut, Vector<Value>),
    Tuple(Vector<Value>),
    Object(HashMap<String, FieldValue>),
    Variant(Id_, Option<Value_>),
}

pub type Value_ = Box<Value>;

pub enum ValueFromExpError {
    ParseBigIntError(ParseBigIntError),
    NotAValue,
}

impl Value {
    pub fn from_dec(dec: Dec) -> Result<Value, ValueFromExpError> {
        match dec {
            Dec::Exp(e) => Value::from_exp(*e.0),
            _ => Err(ValueFromExpError::NotAValue),
        }
    }

    pub fn from_decs(decs: Decs) -> Result<Value, ValueFromExpError> {
        if decs.vec.len() > 1 {
            Err(ValueFromExpError::NotAValue)
        } else {
            Value::from_dec(*decs.vec[0].0.clone())
        }
    }

    pub fn from_exp(e: Exp) -> Result<Value, ValueFromExpError> {
        use Exp::*;
        match e {
            Literal(l) => Value::from_literal(*l.0).map_err(ValueFromExpError::ParseBigIntError),
            Paren(e) => Value::from_exp(*e.0),
            Annot(e, _) => Value::from_exp(*e.0),
            Return(e) => Value::from_exp(*e.0),
            Do(e) => Value::from_exp(*e.0),
            Block(decs) => Value::from_decs(decs),
            _ => Err(ValueFromExpError::NotAValue),
        }
    }

    pub fn from_literal(l: Literal) -> Result<Value, ParseBigIntError> {
        use Value::*;
        Ok(match l {
            Literal::Null => Null,
            Literal::Bool(b) => Bool(b),
            Literal::Unit => Unit,
            Literal::Nat(n) => Nat(n.parse()?),
            // Literal::Int(i) => Int(i.parse()?),
            Literal::Text(s) => Text(vector![s]),
            Literal::Blob(v) => Blob(v),
            _ => unimplemented!(),
        })
    }
}
