use std::fmt::Display;
use std::num::Wrapping;
use std::rc::Rc;

use crate::ast::{Dec, Decs, Exp, Function, Id, Literal, Mut, ToId};
use crate::dynamic::Dynamic;
use crate::shared::{FastClone, Share, Shared};
use crate::vm_types::{def::Actor as ActorDef, def::CtxId, def::Module as ModuleDef, Env};
use crate::Interruption;

use im_rc::HashMap;
use im_rc::Vector;
use num_bigint::{BigInt, BigUint};
use num_traits::ToPrimitive;
use ordered_float::OrderedFloat;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
// use float_cmp::ApproxEq; // in case we want to implement the `Eq` trait for `Value`

pub type Result<T = Value, E = ValueError> = std::result::Result<T, E>;

/// Permit sharing and fast concats.
#[derive(Clone, Debug)]
pub enum Text {
    String(Box<String>),
    Concat(Vector<String>),
}

impl Text {
    #[allow(dead_code)]
    fn into_string(self) -> String {
        match self {
            Text::String(s) => *s,
            Text::Concat(v) => v.into_iter().collect(),
        }
    }
}

impl PartialEq for Text {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Text::String(a), Text::String(b)) => a == b,
            _ => self.to_string() == other.to_string(), // TODO: possibly optimize
        }
    }
}
impl Eq for Text {}

impl std::hash::Hash for Text {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state)
    }
}

impl ToString for Text {
    fn to_string(&self) -> String {
        match self {
            Text::String(s) => s.to_string(),
            Text::Concat(v) => v.iter().cloned().collect(),
        }
    }
}

impl<S: Into<String>> From<S> for Text {
    fn from(value: S) -> Self {
        Text::String(Box::new(value.into()))
    }
}

impl Serialize for Text {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Text {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: String = serde::Deserialize::deserialize(deserializer)?;
        Ok(Text::from(s))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct FieldValue {
    #[serde(rename = "mut")]
    pub mut_: Mut,
    // pub id: Id,
    pub val: Value_,
}

pub type Value_ = Shared<Value>;

pub type Pointer = crate::vm_types::Pointer;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct ClosedFunction(pub Closed<Function>);

pub type Float = OrderedFloat<f64>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Value {
    Null,
    Bool(bool),
    Unit,
    Nat(#[serde(with = "crate::serde_utils::biguint")] BigUint),
    Int(#[serde(with = "crate::serde_utils::bigint")] BigInt),
    Float(Float),
    Char(char),
    Text(Text),
    Blob(Vec<u8>),
    Array(Mut, Vector<Value_>),
    Tuple(Vector<Value_>),
    Object(HashMap<Id, FieldValue>),
    Option(Value_),
    Variant(Id, Option<Value_>),
    /// `var` pointers are implicitly dereferenced (as R-values, but not as L-values).
    Pointer(Pointer),
    /// an opaque pointer is not implicitly dereferenced during evaluation (unlike `var` `Pointer`s).
    Opaque(Pointer),
    Index(Pointer, Value_),
    Function(ClosedFunction),
    PrimFunction(PrimFunction),
    Collection(Collection),
    Dynamic(DynamicValue),
    // DynamicRef(DynamicRef),
    Actor(Actor),
    ActorMethod(ActorMethod),
    Module(ModuleDef),
}

/// Actor value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Actor {
    /// Provides the public interface for message sends (for sanity checks, warnings).
    pub def: Option<ActorDef>,
    /// Provides the operational target of message sends.
    pub id: ActorId,
}

/// Actor identifier (key part of an actor value).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ActorId {
    /// Actor is identified by a local name in the Agent program that creates it.
    Local(Id),
    /// Actor is named by some external source (e.g., `dfx.json`), outside of the source program.
    Alias(Id),
}

/// Actor method value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ActorMethod {
    pub actor: ActorId,
    pub method: Id,
}

// #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
// pub struct DynamicValue(); // to do --

pub struct DynamicValue(pub Rc<std::cell::RefCell<dyn Dynamic>>);

impl DynamicValue {
    pub fn dynamic(&self) -> std::cell::Ref<dyn Dynamic> {
        self.0.borrow()
    }

    pub fn dynamic_mut(&self) -> std::cell::RefMut<dyn Dynamic> {
        (*self.0).borrow_mut()
    }
}

impl<'a> FastClone<DynamicValue> for &'a DynamicValue {
    fn fast_clone(self) -> DynamicValue {
        self.clone()
    }
}

impl std::fmt::Debug for DynamicValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Serialize for DynamicValue {
    fn serialize<S>(&self, _ser: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}

impl<'de> Deserialize<'de> for DynamicValue {
    fn deserialize<D>(_des: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}

impl Clone for DynamicValue {
    fn clone(&self) -> Self {
        // TODO: replace `Box` with `Rc`
        DynamicValue(Rc::clone(&self.0))
    }
}

impl PartialEq for DynamicValue {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}
impl Eq for DynamicValue {}

impl std::hash::Hash for DynamicValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.borrow().dyn_hash(state)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Collection {
    HashMap(#[serde(with = "crate::serde_utils::im_rc_hashmap")] HashMap<Value_, Value_>),
    FastRandIter(FastRandIter),
}

/// Fast randomness, for data used in performance tests.
/// Not appropriate for security-critical randomness.
///
/// See also <https://github.com/dfinity/canister-profiling/tree/main/collections>
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct FastRandIter {
    state: Wrapping<u32>,
    size: Option<u32>,
    ind: u32,
}

impl FastRandIter {
    pub fn new(size: Option<u32>, seed: u32) -> FastRandIter {
        FastRandIter {
            size,
            state: Wrapping(seed),
            ind: 0,
        }
    }
    pub fn next(&mut self) -> Option<Value> {
        if let Some(size) = self.size {
            self.ind += 1;
            if self.ind > size {
                return None;
            }
        }
        self.state = (self.state * Wrapping(48271)) % Wrapping(0x7fffffff);
        Some(Value::Nat(BigUint::from(self.state.0)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum PrimFunction {
    AtSignVar(String),
    DebugPrint,
    NatToText,
    #[cfg(feature = "to-motoko")]
    #[cfg(feature = "value-reflection")]
    ReifyValue,
    #[cfg(feature = "value-reflection")]
    ReflectValue,
    #[cfg(feature = "to-motoko")]
    #[cfg(feature = "core-reflection")]
    ReifyCore,
    #[cfg(feature = "core-reflection")]
    ReflectCore,
    Collection(CollectionFunction),
}

impl PrimFunction {
    pub fn resolve(name: String) -> Result<PrimFunction, String> {
        use CollectionFunction::*;
        use PrimFunction::*;
        Ok(match name.as_str() {
            "\"print\"" => DebugPrint,
            "\"natToText\"" => NatToText,
            "\"hashMapNew\"" => Collection(HashMap(HashMapFunction::New)),
            "\"hashMapPut\"" => Collection(HashMap(HashMapFunction::Put)),
            "\"hashMapGet\"" => Collection(HashMap(HashMapFunction::Get)),
            "\"hashMapRemove\"" => Collection(HashMap(HashMapFunction::Remove)),
            "\"fastRandIterNew\"" => Collection(FastRandIter(FastRandIterFunction::New)),
            "\"fastRandIterNext\"" => Collection(FastRandIter(FastRandIterFunction::Next)),
            #[cfg(feature = "to-motoko")]
            #[cfg(feature = "value-reflection")]
            "\"reifyValue\"" => ReifyValue,
            #[cfg(feature = "value-reflection")]
            "\"reflectValue\"" => ReflectValue,
            #[cfg(feature = "to-motoko")]
            #[cfg(feature = "core-reflection")]
            "\"reifyCore\"" => ReifyCore,
            #[cfg(feature = "core-reflection")]
            "\"reflectCore\"" => ReflectCore,
            _ => Err(name.to_string())?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum CollectionFunction {
    HashMap(HashMapFunction),
    FastRandIter(FastRandIterFunction),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum HashMapFunction {
    New,
    Put,
    Get,
    Remove,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum FastRandIterFunction {
    New,
    Next,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Closed<X> {
    pub ctx: CtxId,
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    pub env: Env,
    pub content: X,
}

impl Value {
    pub fn from_dec(dec: &Dec) -> Result {
        match dec {
            Dec::Exp(e) => Value::from_exp(e.as_ref().data_ref()),
            _ => Err(ValueError::NotAValue),
        }
    }

    pub fn from_decs(decs: &Decs) -> Result {
        if decs.vec.len() > 1 {
            Err(ValueError::NotAValue)
        } else {
            Value::from_dec(decs.vec[0].as_ref().data_ref())
        }
    }

    pub fn from_exp(e: &Exp) -> Result {
        use Exp::*;
        match e {
            Literal(l) => Value::from_literal(l),
            Paren(e) => Value::from_exp(&e.0),
            Annot(_, e, _) => Value::from_exp(&e.0),
            Return(e) => match e {
                Some(e) => Value::from_exp(&e.0),
                None => Ok(Value::Unit),
            },
            Do(e) => Value::from_exp(&e.0),
            Block(decs) => Value::from_decs(decs),
            _ => Err(ValueError::NotAValue),
        }
    }

    pub fn from_literal(l: &Literal) -> Result {
        use Value::*;
        Ok(match l {
            Literal::Null => Null,
            Literal::Bool(b) => Bool(*b),
            Literal::Unit => Unit,
            Literal::Nat(n) => Nat({
                let n = n.replace('_', "");
                if let Some(n) = n.strip_prefix("0x") {
                    use num_traits::Num;
                    BigUint::from_str_radix(n, 16).map_err(|_| ValueError::BigInt)?
                } else {
                    n.parse().map_err(|_| ValueError::BigInt)?
                }
            }),
            // Literal::Int(i) => Int(i.parse()?),
            Literal::Float(n) => {
                Value::Float(n.replace('_', "").parse().map_err(|_| ValueError::Float)?)
            }
            Literal::Char(s) => Char(s[1..s.len() - 1].parse().map_err(|_| ValueError::Char)?),
            Literal::Text(s) => Text(crate::value::Text::from(s[1..s.len() - 1].to_string())),
            Literal::Blob(v) => Blob(v.clone()),
        })
    }
}

impl Value {
    // /// Create a `serde-reflection` representation of the Motoko value.
    // fn reflection_value(&self) -> Result<serde_reflection::Value> {
    //     use serde_reflection::Value::*;
    //     Ok(match self {
    //         Value::Unit => Unit,
    //         Value::Null => Option(None),
    //         Value::Bool(b) => Bool(*b),
    //         Value::Nat(n) => U64(n.to_u64().ok_or(ValueError::BigInt)?.into()),
    //         Value::Int(n) => I64(n.to_i64().ok_or(ValueError::BigInt)?.into()),
    //         Value::Float(f) => F64(f.0),
    //         Value::Char(c) => Char(*c),
    //         Value::Text(s) => Str(s.to_string()),
    //         Value::Blob(b) => Bytes(b.clone()),
    //         Value::Array(_, vs) => Seq(vs
    //             .into_iter()
    //             .map(|v| v.reflection_value())
    //             .collect::<Result<Vec<_>, _>>()?),
    //         Value::Tuple(vs) => Seq(vs
    //             .into_iter()
    //             .map(|v| v.reflection_value())
    //             .collect::<Result<Vec<_>, _>>()?),
    //         Value::Object(m) => {
    //             // let mut map = serde_json::Map::new();
    //             // for (k, v) in m {
    //             //     map.insert(k.to_string(), v.val.json_value()?);
    //             // }
    //             // (map)
    //             Seq(m
    //                 .iter()
    //                 .map(|(k, v)| Seq(vec![Str(k.to_string()), v.reflection_value()]))
    //                 .collect())
    //         }
    //         Value::Option(v) => Option(Some(Box::new(v.as_ref().reflection_value()?))),
    //         Value::Variant(tag, v) => {
    //             // let mut map = serde_json::Map::new();
    //             // map.insert(
    //             //     tag.to_string(),
    //             //     v.as_ref()
    //             //         .map(|v| v.reflection_value())
    //             //         .unwrap_or(Ok(None))?,
    //             // );
    //             // Object(map)
    //             Variant((), ())
    //         }
    //         Value::Pointer(_) => Err(ValueError::ToRust("Pointer".to_string()))?,
    //         Value::Index(_, _) => Err(ValueError::ToRust("Index".to_string()))?,
    //         Value::Function(_) => Err(ValueError::ToRust("Function".to_string()))?,
    //         Value::PrimFunction(_) => Err(ValueError::ToRust("PrimFunction".to_string()))?,
    //         Value::Collection(c) => match c {
    //             // Collection::HashMap(m) => Array(
    //             //     m.iter()
    //             //         .map(|(k, v)| Ok(Array(vec![k.json_value()?, v.json_value()?])))
    //             //         .collect::<Result<Vec<_>, _>>()?,
    //             // ),
    //             Collection::HashMap(m) => Array(
    //                 m.iter()
    //                     .map(|(k, v)| Ok(Array(vec![k.json_value()?, v.json_value()?])))
    //                     .collect::<Result<Vec<_>, _>>()?,
    //             ),
    //             Collection::FastRandIter(..) => {
    //                 Err(ValueError::ToRust("FastRandIter".to_string()))?
    //             }
    //         },
    //     })
    // }

    /// Create a JSON-style representation of the Motoko value.
    fn json_value(&self) -> Result<serde_json::Value> {
        use serde_json::json;
        use serde_json::Value::*;
        Ok(match self {
            Value::Unit => Null,
            Value::Null => Null,
            Value::Bool(b) => Bool(*b),
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
            Value::Variant(tag, v) => {
                let mut map = serde_json::Map::new();
                map.insert(
                    tag.to_string(),
                    v.as_ref().map(|v| v.json_value()).unwrap_or(Ok(Null))?,
                );
                Object(map)
            }
            Value::Pointer(_) => Err(ValueError::ToRust("Pointer".to_string()))?,
            Value::Actor(_) => Err(ValueError::ToRust("Actor".to_string()))?,
            Value::ActorMethod(_) => Err(ValueError::ToRust("ActorMethod".to_string()))?,
            Value::Opaque(_) => Err(ValueError::ToRust("Opaque".to_string()))?,
            Value::Index(_, _) => Err(ValueError::ToRust("Index".to_string()))?,
            Value::Function(_) => Err(ValueError::ToRust("Function".to_string()))?,
            Value::PrimFunction(_) => Err(ValueError::ToRust("PrimFunction".to_string()))?,
            Value::Module(_) => Err(ValueError::ToRust("Module".to_string()))?,
            Value::Collection(c) => match c {
                Collection::HashMap(m) => Array(
                    m.iter()
                        .map(|(k, v)| Ok(Array(vec![k.json_value()?, v.json_value()?])))
                        .collect::<Result<Vec<_>, _>>()?,
                ),
                // Collection::HashMap(m) => {
                //     let mut map = serde_json::Map::new();
                //     for (k, v) in m {
                //         map.insert(k.to_rust()?, v.json_value()?);
                //     }
                //     Object(map)
                // }
                Collection::FastRandIter(..) => {
                    Err(ValueError::ToRust("FastRandIter".to_string()))?
                }
            },
            Value::Dynamic(d) => {
                serde_json::to_value(d).map_err(|e| ValueError::ToRust(e.to_string()))?
            }
        })
    }

    // /// Create a RON-style representation of the Motoko value.
    // fn ron_value(&self) -> Result<ron::Value> {
    //     use ron::Number::*;
    //     use ron::Value::*;
    //     Ok(match self {
    //         Value::Unit => Unit,
    //         Value::Null => Option(None),
    //         Value::Bool(b) => Bool(*b),
    //         Value::Nat(n) => Number(Integer(n.to_i64().ok_or(ValueError::BigInt)?.into())),
    //         Value::Int(n) => Number(Integer(n.to_i64().ok_or(ValueError::BigInt)?.into())),
    //         Value::Float(f) => Number(Float(ron::value::Float::new(f.0))),
    //         Value::Char(c) => Char(*c),
    //         Value::Text(s) => String(s.to_string()),
    //         Value::Blob(b) => Seq(b.iter().map(|u| Number((*u as u64).into())).collect()),
    //         Value::Array(_, vs) => Seq(vs
    //             .into_iter()
    //             .map(|v| v.ron_value())
    //             .collect::<Result<Vec<_>, _>>()?),
    //         Value::Tuple(vs) => Seq(vs
    //             .into_iter()
    //             .map(|v| v.ron_value())
    //             .collect::<Result<Vec<_>, _>>()?),
    //         Value::Object(m) => {
    //             let mut map = ron::Map::new();
    //             for (k, v) in m {
    //                 map.insert(String(k.to_string()), v.val.ron_value()?);
    //             }
    //             Map(map)
    //         }
    //         Value::Option(v) => Option(Some(Box::new(v.as_ref().ron_value()?))),
    //         Value::Variant(s, v) => {
    //             // let mut map = ron::Map::new();
    //             // map.insert(
    //             //     String(s.to_string()),
    //             //     Option(match v {
    //             //         Some(v) => Some(Box::new(v.as_ref().ron_value()?)),
    //             //         None => None,
    //             //     }),
    //             // );
    //             // Map(map)
    //             println!("{:?}",ron::Value::from_str("Unit()").unwrap().into_rust::<Value>());
    //             let tag = String(s.to_string());
    //             match v {
    //                 Some(v) => Seq(vec![v.as_ref().ron_value()?]),
    //                 // None => Seq(vec![tag]),
    //                 // None => tag,
    //                 None => Seq(vec![]),
    //             }
    //         }
    //         Value::Pointer(_) => Err(ValueError::ToRust("Pointer".to_string()))?,
    //         Value::Index(_, _) => Err(ValueError::ToRust("Index".to_string()))?,
    //         Value::Function(_) => Err(ValueError::ToRust("Function".to_string()))?,
    //         Value::PrimFunction(_) => Err(ValueError::ToRust("PrimFunction".to_string()))?,
    //         Value::Collection(c) => match c {
    //             Collection::HashMap(m) => {
    //                 let mut map = ron::Map::new();
    //                 for (k, v) in m {
    //                     map.insert(k.ron_value()?, v.ron_value()?);
    //                 }
    //                 Map(map)
    //             }
    //             Collection::FastRandIter(..) => {
    //                 Err(ValueError::ToRust("FastRandIter".to_string()))?
    //             }
    //         },
    //     })
    // }

    /// Convert to any deserializable Rust type.
    #[cfg(not(feature = "serde-paths"))]
    pub fn to_rust<T: DeserializeOwned>(&self) -> Result<T> {
        serde_json::from_value(self.json_value()?).map_err(|e| ValueError::ToRust(e.to_string()))
    }
    #[cfg(feature = "serde-paths")]
    pub fn to_rust<T: DeserializeOwned>(&self) -> Result<T> {
        // Include paths in error messages
        let s: String = serde_json::to_string(&self.json_value()?).unwrap();
        let des = &mut serde_json::Deserializer::from_str(&s);
        Ok(serde_path_to_error::deserialize(des)
            .map_err(|err| err.to_string())
            .unwrap())
    }

    #[cfg(feature = "to-motoko")]
    pub fn from_rust<T: ToMotoko>(value: T) -> Result {
        value.to_motoko()
    }

    pub fn get_field_or<E>(&self, f: &str, err: E) -> Result<Value_, E> {
        match self {
            Value::Object(m) => {
                // todo -- somehow avoid creating this string just for the lookup.
                // pushing the allocation to the caller usually doesn't help much.
                match m.get(&f.to_id()) {
                    None => Err(err),
                    Some(v) => Ok(v.val.fast_clone()),
                }
            }
            _ => Err(err),
        }
    }
}

#[cfg(feature = "to-motoko")]
pub trait ToMotoko {
    fn to_motoko(self) -> Result;

    fn to_shared(self) -> Result<Value_>
    where
        Self: Sized,
    {
        Ok(self.to_motoko()?.share())
    }
}

#[cfg(feature = "to-motoko")]
impl<T: Serialize> ToMotoko for T {
    fn to_motoko(self) -> Result {
        // println!("{}",ron::to_string(&self).unwrap());//////
        self.serialize(crate::convert::ser::Serializer)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ValueError {
    Char,
    BigInt,
    Float,
    ToRust(String),
    ToMotoko(String),
    Candid(String),
    NotAValue,
}

impl Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ValueError {}

impl serde::ser::Error for ValueError {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        ValueError::ToMotoko(msg.to_string())
    }
}

impl serde::de::Error for ValueError {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        ValueError::ToRust(msg.to_string())
    }
}

pub fn string_from_value(v: &Value) -> Result<String, Interruption> {
    //Ok(crate::format::format_one_line(v))
    Ok(format!("{:?}", v))
}

#[inline]
pub fn usize_from_biguint(n: &BigUint) -> Result<usize, Interruption> {
    n.to_usize()
        .ok_or(Interruption::ValueError(ValueError::BigInt))
}
