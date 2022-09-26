use std::fmt::Display;

use im_rc::{HashMap, Vector};
use num_bigint::{BigInt, BigUint};
use serde::{serde_if_integer128, Serialize};

use crate::{
    ast::Mut,
    value::{FieldValue, Text, Value, ValueError},
};

type Error = ValueError;
type Result<T> = std::result::Result<T, Error>;

pub struct Serializer;

impl Serializer {
    #[inline]
    pub fn serialize_nat(self, value: BigUint) -> Result<Value> {
        Ok(Value::Nat(value))
    }

    #[inline]
    pub fn serialize_int(self, value: BigInt) -> Result<Value> {
        Ok(Value::Int(value))
    }
}

impl serde::Serializer for Serializer {
    type Ok = Value;
    type Error = Error;

    type SerializeSeq = SerializeVec;
    type SerializeTuple = SerializeTuple;
    type SerializeTupleStruct = SerializeTuple;
    type SerializeTupleVariant = SerializeTupleVariant;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeStruct;
    type SerializeStructVariant = SerializeStructVariant;

    #[inline]
    fn serialize_bool(self, value: bool) -> Result<Value> {
        Ok(Value::Bool(value))
    }

    #[inline]
    fn serialize_i8(self, value: i8) -> Result<Value> {
        self.serialize_int(value.into())
    }

    #[inline]
    fn serialize_i16(self, value: i16) -> Result<Value> {
        self.serialize_int(value.into())
    }

    #[inline]
    fn serialize_i32(self, value: i32) -> Result<Value> {
        self.serialize_int(value.into())
    }

    #[inline]
    fn serialize_i64(self, value: i64) -> Result<Value> {
        self.serialize_int(value.into())
    }

    serde_if_integer128! {
        fn serialize_i128(self, value: i128) -> Result<Value> {
            self.serialize_int(value.into())
        }
    }

    #[inline]
    fn serialize_u8(self, value: u8) -> Result<Value> {
        self.serialize_nat(value.into())
    }

    #[inline]
    fn serialize_u16(self, value: u16) -> Result<Value> {
        self.serialize_nat(value.into())
    }

    #[inline]
    fn serialize_u32(self, value: u32) -> Result<Value> {
        self.serialize_nat(value.into())
    }

    #[inline]
    fn serialize_u64(self, value: u64) -> Result<Value> {
        self.serialize_nat(value.into())
    }

    serde_if_integer128! {
        fn serialize_u128(self, value: u128) -> Result<Value> {
            self.serialize_nat(value.into())
        }
    }

    #[inline]
    fn serialize_f32(self, value: f32) -> Result<Value> {
        self.serialize_f64(value as f64)
    }

    #[inline]
    fn serialize_f64(self, value: f64) -> Result<Value> {
        Ok(Value::Float(value.into()))
    }

    #[inline]
    fn serialize_char(self, value: char) -> Result<Value> {
        Ok(Value::Char(value))
    }

    #[inline]
    fn serialize_str(self, value: &str) -> Result<Value> {
        Ok(Value::Text(Text::from(value.to_string())))
    }

    fn serialize_bytes(self, value: &[u8]) -> Result<Value> {
        Ok(Value::Blob(value.to_vec()))
    }

    #[inline]
    fn serialize_unit(self) -> Result<Value> {
        Ok(Value::Unit)
    }

    #[inline]
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Value> {
        self.serialize_unit()
    }

    #[inline]
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Value> {
        Ok(Value::Variant(variant.to_string(), None))
    }

    #[inline]
    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        Ok(Value::Variant(
            variant.to_string(),
            Some(Box::new(value.serialize(self)?)),
        ))
    }

    #[inline]
    fn serialize_none(self) -> Result<Value> {
        Ok(Value::Null)
    }

    #[inline]
    fn serialize_some<T>(self, value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        Ok(Value::Option(Box::new(value.serialize(self)?)))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SerializeVec { vec: Vector::new() })
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        Ok(SerializeTuple { vec: Vector::new() })
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_tuple(len)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Ok(SerializeTupleVariant {
            name: String::from(variant),
            vec: Vector::new(),
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(SerializeMap {
            map: HashMap::new(),
            next_key: None,
        })
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        Ok(SerializeStruct {
            map: HashMap::new(),
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Ok(SerializeStructVariant {
            name: String::from(variant),
            map: HashMap::new(),
        })
    }

    fn collect_str<T>(self, value: &T) -> Result<Value>
    where
        T: ?Sized + Display,
    {
        Ok(Value::Text(Text::from(value.to_string())))
    }
}

pub struct SerializeVec {
    vec: Vector<Value>,
}

pub struct SerializeTuple {
    vec: Vector<Value>,
}

pub struct SerializeTupleVariant {
    name: String,
    vec: Vector<Value>,
}

pub struct SerializeMap {
    map: HashMap<Value, Value>,
    next_key: Option<Value>,
}

pub struct SerializeStruct {
    map: HashMap<String, FieldValue>,
}

pub struct SerializeStructVariant {
    name: String,
    map: HashMap<String, FieldValue>,
}

impl serde::ser::SerializeSeq for SerializeVec {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.vec.push_back(value.serialize(Serializer)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::Array(Mut::Var, self.vec)) // Mutable by default
    }
}

impl serde::ser::SerializeTuple for SerializeTuple {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.vec.push_back(value.serialize(Serializer)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::Tuple(self.vec)) // Mutable by default
    }
}

impl serde::ser::SerializeTupleStruct for SerializeTuple {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        serde::ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<Value> {
        serde::ser::SerializeTuple::end(self)
    }
}

impl serde::ser::SerializeTupleVariant for SerializeTupleVariant {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.vec.push_back(value.serialize(Serializer)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::Variant(
            self.name,
            Some(Box::new(Value::Tuple(self.vec))),
        ))
    }
}

impl serde::ser::SerializeMap for SerializeMap {
    type Ok = Value;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.next_key = Some(key.serialize(Serializer)?);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let key = self
            .next_key
            .take()
            .expect("serialize_value called before serialize_key");
        self.map.insert(key, value.serialize(Serializer)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::Collection(crate::value::Collection::HashMap(
            self.map,
        )))
    }
}

impl serde::ser::SerializeStruct for SerializeStruct {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.map.insert(
            String::from(key),
            FieldValue {
                mut_: Mut::Var, // Mutable by default
                val: value.serialize(Serializer)?,
            },
        );
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::Object(self.map))
    }
}

impl serde::ser::SerializeStructVariant for SerializeStructVariant {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.map.insert(
            String::from(key),
            FieldValue {
                mut_: Mut::Var, // Mutable by default
                val: value.serialize(Serializer)?,
            },
        );
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::Variant(
            self.name,
            Some(Box::new(Value::Object(self.map))),
        ))
    }
}
