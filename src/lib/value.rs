use std::char::ParseCharError;
use std::num::ParseFloatError;

use crate::ast::{Dec, Decs, Exp, Id, Id_, Literal, Mut};
use im_rc::vector;
use im_rc::HashMap;
use im_rc::Vector;
use num_bigint::{BigInt, BigUint, ParseBigIntError};
use serde::{Deserialize, Serialize};
// use float_cmp::ApproxEq; // in case we want to implement the `Eq` trait for `Value`

/// Permit sharing, and fast concats.
pub type Text = Vector<String>;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FieldValue {
    pub mut_: Mut,
    pub value: Value,
}

pub type Value_ = Box<Value>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Null,
    Bool(bool),
    Unit,
    Nat(BigUint),
    Int(BigInt),
    Float(f64),
    Char(char),
    Text(Text),
    Blob(Vec<u8>),
    Array(Mut, Vector<Value>),
    Tuple(Vector<Value>),
    Object(HashMap<Id, FieldValue>),
    Variant(Id_, Option<Value_>),
    Pointer(crate::vm_types::Pointer),
}

// TODO: custom `PartialEq` implementation for approximate f64 equality?
impl Eq for Value {}

impl Value {
    pub fn from_dec(dec: Dec) -> Result<Value, ValueError> {
        match dec {
            Dec::Exp(e) => Value::from_exp(e),
            _ => Err(ValueError::NotAValue),
        }
    }

    pub fn from_decs(decs: Decs) -> Result<Value, ValueError> {
        if decs.vec.len() > 1 {
            Err(ValueError::NotAValue)
        } else {
            Value::from_dec((*decs.vec[0].0).clone())
        }
    }

    pub fn from_exp(e: Exp) -> Result<Value, ValueError> {
        use Exp::*;
        match e {
            Literal(l) => Value::from_literal(l),
            Paren(e) => Value::from_exp(*e.0),
            Annot(e, _) => Value::from_exp(*e.0),
            Return(e) => match e {
                Some(e) => Value::from_exp(*e.0),
                None => Ok(Value::Unit),
            },
            Do(e) => Value::from_exp(*e.0),
            Block(decs) => Value::from_decs(decs),
            _ => Err(ValueError::NotAValue),
        }
    }

    pub fn from_literal(l: Literal) -> Result<Value, ValueError> {
        use Value::*;
        Ok(match l {
            Literal::Null => Null,
            Literal::Bool(b) => Bool(b),
            Literal::Unit => Unit,
            Literal::Nat(n) => Nat({
                let n = n.replace('_', "");
                if n.starts_with("0x") {
                    use num_traits::Num;
                    BigUint::from_str_radix(&n[2..], 16).map_err(ValueError::ParseBigIntError)?
                } else {
                    n.parse().map_err(ValueError::ParseBigIntError)?
                }
            }),
            // Literal::Int(i) => Int(i.parse()?),
            Literal::Float(n) => Float(
                n.replace('_', "")
                    .parse()
                    .map_err(ValueError::ParseFloatError)?,
            ),
            Literal::Char(s) => Char(
                s[1..s.len() - 1]
                    .parse()
                    .map_err(ValueError::ParseCharError)?,
            ),
            Literal::Text(s) => Text(vector![s[1..s.len() - 1].to_string()]),
            Literal::Blob(v) => Blob(v),
        })
    }
}

pub enum ValueError {
    ParseCharError(ParseCharError),
    ParseBigIntError(ParseBigIntError),
    ParseFloatError(ParseFloatError),
    NotAValue,
}
