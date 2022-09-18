use crate::ast::{Dec, Decs, Exp, Function, Id, Id_, Literal, Mut};
use crate::vm_types::Env;

use im_rc::vector;
use im_rc::HashMap;
use im_rc::Vector;
use num_bigint::{BigInt, BigUint};
use num_traits::ToPrimitive;
use ordered_float::OrderedFloat;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
// use float_cmp::ApproxEq; // in case we want to implement the `Eq` trait for `Value`

/// Permit sharing, and fast concats.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Text(pub Vector<String>);

impl ToString for Text {
    fn to_string(&self) -> String {
        self.0.iter().cloned().collect()
    }
}

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
#[serde(tag = "value_type", content = "value")]
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

impl Value {
    pub fn as_bool(self) -> Option<bool> {
        match self {
            Value::Bool(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_nat(self) -> Option<BigUint> {
        match self {
            Value::Nat(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_int(self) -> Option<BigInt> {
        match self {
            Value::Int(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_float(self) -> Option<f64> {
        match self {
            Value::Float(x) => Some(x.0),
            _ => None,
        }
    }
    pub fn as_char(self) -> Option<char> {
        match self {
            Value::Char(x) => Some(x),
            // Value::Text(x)=>Some(x.0.),
            _ => None,
        }
    }
    pub fn as_text(self) -> Option<Text> {
        match self {
            Value::Text(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_string(self) -> Option<String> {
        Some(self.as_text()?.to_string())
    }
    pub fn as_blob(self) -> Option<Vec<u8>> {
        match self {
            Value::Blob(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_array(self) -> Option<Vector<Value>> {
        match self {
            Value::Array(_, x) => Some(x),
            _ => None,
        }
    }
    pub fn as_tuple(self) -> Option<Vector<Value>> {
        match self {
            Value::Tuple(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_object(self) -> Option<HashMap<Id, Value>> {
        match self {
            Value::Object(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_option(self) -> Option<Value> {
        match self {
            Value::Null => Some(Value::Null),
            Value::Option(x) => Some(*x),
            _ => None,
        }
    }
    pub fn as_variant(self) -> Option<(String, Option<Value>)> {
        match self {
            Value::Variant(x, y) => Some((*x.0, *y)),
            _ => None,
        }
    }
    pub fn as_pointer(self) -> Option<Pointer> {
        match self {
            Value::Pointer(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_array_offset(self) -> Option<(Pointer, usize)> {
        match self {
            Value::ArrayOffset(x, y) => Some((x, y)),
            _ => None,
        }
    }
    pub fn as_function(self) -> Option<ClosedFunction> {
        match self {
            Value::Function(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_prim_function(self) -> Option<PrimFunction> {
        match self {
            Value::PrimFunction(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_collection(self) -> Option<Collection> {
        match self {
            Value::Collection(x) => Some(x),
            _ => None,
        }
    }

    /// Create a JSON-style representation of the Motoko value.
    pub fn json_value(&self) -> Result<serde_json::Value, ValueError> {
        use serde_json::json;
        use serde_json::Value::*;
        Ok(match self {
            Value::Null => Null,
            Value::Bool(b) => Bool(*b),
            Value::Unit => Array(vec![]),
            Value::Nat(n) => Number(n.to_u64().ok_or(ValueError::BigInt)?.into()),
            Value::Int(n) => Number(n.to_i64().ok_or(ValueError::BigInt)?.into()),
            Value::Float(f) => json!(f.0),
            Value::Char(c) => String(c.to_string()),
            Value::Text(s) => String(s.to_string()),
            Value::Blob(b) => Array(b.iter().map(|u| Number((*u as u64).into())).collect()),
            Value::Array(_, vs) => Array(
                vs.into_iter()
                    .map(|v| v.json_value())
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Value::Tuple(vs) => Array(
                vs.into_iter()
                    .map(|v| v.json_value())
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Value::Object(m) => {
                let mut map = serde_json::Map::new();
                for (k, v) in m {
                    map.insert(k.to_string(), v.val.json_value()?);
                }
                Object(map)
            }
            Value::Option(v) => v.as_ref().json_value()?,
            Value::Variant(s, v) => match v {
                Some(v) => Array(vec![String(*s.0.clone()), v.as_ref().json_value()?]),
                None => Null,
            },
            Value::Pointer(_) => Err(ValueError::NotConvertible("Pointer".to_string()))?,
            Value::ArrayOffset(_, _) => Err(ValueError::NotConvertible("ArrayOffset".to_string()))?,
            Value::Function(_) => Err(ValueError::NotConvertible("Function".to_string()))?,
            Value::PrimFunction(_) => Err(ValueError::NotConvertible("PrimFunction".to_string()))?,
            Value::Collection(c) => match c {
                Collection::HashMap(m) => Array(
                    m.iter()
                        .map(|(k, v)| Ok(Array(vec![k.json_value()?, v.json_value()?])))
                        .collect::<Result<Vec<_>, _>>()?,
                ),
            },
        })
    }

    /// Convert to any deserializable Rust type.
    pub fn convert<T: DeserializeOwned>(&self) -> Result<T, ValueError> {
        serde_json::from_value(self.json_value()?)
            .map_err(|e| ValueError::NotConvertible(e.to_string()))
    }
}

// TODO: implement `TryInto` rather than `Into` if possible
impl<'a, T: DeserializeOwned> Into<Result<T, ValueError>> for &'a Value {
    fn into(self) -> Result<T, ValueError> {
        serde_json::from_value(self.json_value()?)
            .map_err(|e| ValueError::NotConvertible(e.to_string()))
    }
}

// #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
// pub enum ValueErrorKind {
//     Empty,
//     Invalid,
// }

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ValueError {
    Char,
    BigInt,
    Float,
    NotAValue,
    NotConvertible(String),
}
