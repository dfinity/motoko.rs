use num_bigint::{BigInt, BigUint};
use eq_float::F64;
use crate::ast::Literal;

#[derive(Debug, Clone)] // , PartialEq, Eq
pub enum Value {
    Null,
    Bool(bool),
    Unit,
    Nat(BigUint),
    Nat8(u8),
    Nat16(u16),
    Nat32(u32),
    Nat64(u64),
    Int(BigInt),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Float(F64), // TODO: test NaN equality compared to Motoko
    Char(char),
    Text(String),
    Blob(Vec<u8>),
    Tuple(Vec<Value>),
    Object(Vec<(String, Value)>),
}

impl Value {
    pub fn from_literal(l: Literal) -> Result<Value, () /* TODO */> {
        use Value::*;
        Ok(match l {
            Literal::Null => Null,
            Literal::Bool(b) => Bool(b),
            Literal::Unit => Unit,
            Literal::Nat(n) => Nat(n),
            Literal::Nat8(n) => Nat8(n),
            Literal::Nat16(n) => Nat16(n),
            Literal::Nat32(n) => Nat32(n),
            Literal::Nat64(n) => Nat64(n),
            Literal::Int(i) => Int(i),
            Literal::Int8(i) => Int8(i),
            Literal::Int16(i) => Int16(i),
            Literal::Int32(i) => Int32(i),
            Literal::Int64(i) => Int64(i),
            Literal::Float(f) => Float(F64::from(f.parse::<f64>().map_err(|_| ())?)),
            Literal::Char(c) => Char(c),
            Literal::Text(s) => Text(s),
            Literal::Blob(v) => Blob(v),
        })
    }
}
