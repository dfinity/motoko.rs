use crate::ast::{Dec, Decs, Exp, Function, Id, Id_, Literal, Mut};
use crate::vm_types::Env;

use im_rc::vector;
use im_rc::HashMap;
use im_rc::Vector;
use num_bigint::{BigInt, BigUint};
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
// use float_cmp::ApproxEq; // in case we want to implement the `Eq` trait for `Value`

/// Permit sharing, and fast concats.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Text(pub Vector<String>);

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct FieldValue {
    pub mut_: Mut,
    pub id: Id,
    pub val: Value,
}

pub type Value_ = Box<Value>;

pub type Pointer = crate::vm_types::Pointer;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct ClosedFunction(pub Closed<Function>);

pub type Float = OrderedFloat<f64>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Value {
    Null,
    Bool(bool),
    Unit,
    Nat(BigUint),
    Int(BigInt),
    Float(Float),
    Char(char),
    Text(Text),
    Blob(Vec<u8>),
    Array(Mut, Vector<Value>),
    Tuple(Vector<Value>),
    Object(HashMap<Id, FieldValue>),
    Option(Value_),
    Variant(Id_, Option<Value_>),
    Pointer(Pointer),
    ArrayOffset(Pointer, usize),
    Function(ClosedFunction),
    PrimFunction(PrimFunction),
    Collection(Collection),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Collection {
    HashMap(#[serde(with = "crate::serde::im_rc_hashmap")] HashMap<Value, Value>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum PrimFunction {
    DebugPrint,
    Collection(CollectionFunction),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum CollectionFunction {
    HashMap(HashMapFunction),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum HashMapFunction {
    New,
    Put,
    Get,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Closed<X> {
    pub env: Env,
    pub content: X,
}

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
                    BigUint::from_str_radix(&n[2..], 16).map_err(|_| ValueError::BigInt)?
                } else {
                    n.parse().map_err(|_| ValueError::BigInt)?
                }
            }),
            // Literal::Int(i) => Int(i.parse()?),
            Literal::Float(n) => {
                Value::Float(n.replace('_', "").parse().map_err(|_| ValueError::Float)?)
            }
            Literal::Char(s) => Char(s[1..s.len() - 1].parse().map_err(|_| ValueError::Char)?),
            Literal::Text(s) => Text(crate::value::Text(vector![s[1..s.len() - 1].to_string()])),
            Literal::Blob(v) => Blob(v),
        })
    }
}

// #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
// pub enum ValueErrorKind {
//     Empty,
//     Invalid,
// }

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ValueError {
    Char,   //(ValueErrorKind),
    BigInt, //(ValueErrorKind),
    Float,  //(ValueErrorKind),
    NotAValue,
}
