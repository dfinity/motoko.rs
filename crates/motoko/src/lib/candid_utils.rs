use candid::parser::value::{IDLArgs, IDLValue, VariantValue};
use candid::types::Label;
use im_rc::{HashMap, Vector};

use crate::ast::{Id, Mut, ToId};
use crate::value::{FieldValue, Result};
use crate::{Share, Value, ValueError};

fn resolve_id(label: Label) -> Result<Id> {
    use candid::types::Label::*;
    Ok(match label {
        // TODO: implement `Id` and `Unnamed` keys
        Id(i) => Err(ValueError::ToMotoko(format!(
            "Candid: cannot convert `Id` ({}) to field name",
            i
        )))?,
        Named(s) => s.to_id(),
        Unnamed(i) => Err(ValueError::ToMotoko(format!(
            "Candid: cannot convert `Unnamed` ({}) to field name",
            i
        )))?,
    })
}

pub fn to_value(value: IDLValue) -> Result<Value> {
    Ok(match value {
        IDLValue::Bool(b) => Value::Bool(b),
        IDLValue::Null => Value::Null,
        IDLValue::Text(t) => Value::Text(t.into()),
        // IDLValue::Number(n) => Value::,
        IDLValue::Opt(o) => Value::Option(to_value(*o)?.share()),
        IDLValue::Vec(v) => Value::Array(
            Mut::Const,
            v.into_iter()
                .map(|v| to_value(v).map(|v| v.share()))
                .collect::<Result<_>>()?,
        ),
        IDLValue::Record(r) => Value::Object({
            let mut map = HashMap::new();
            for field in r {
                map.insert(
                    resolve_id(field.id)?,
                    FieldValue {
                        mut_: Mut::Const,
                        val: to_value(field.val)?.share(),
                    },
                );
            }
            map
        }), // TODO: refactor `motoko::Value::Object` to `motoko::Value::Record`?
        IDLValue::Variant(VariantValue(field, _)) => {
            Value::Variant(resolve_id(field.id)?, Some(to_value(field.val)?.share()))
        }
        IDLValue::Principal(_) => todo!(), // TODO
        IDLValue::Service(_) => todo!(),   // TODO
        IDLValue::Func(_, _) => todo!(),
        IDLValue::None => Value::Null,
        IDLValue::Nat(i) => Value::Nat(i.0),
        IDLValue::Nat8(i) => Value::Nat(i.into()),  // TODO
        IDLValue::Nat16(i) => Value::Nat(i.into()), // TODO
        IDLValue::Nat32(i) => Value::Nat(i.into()), // TODO
        IDLValue::Nat64(i) => Value::Nat(i.into()), // TODO
        IDLValue::Int(i) => Value::Int(i.0),
        IDLValue::Int8(i) => Value::Int(i.into()),  // TODO
        IDLValue::Int16(i) => Value::Int(i.into()), // TODO
        IDLValue::Int32(i) => Value::Int(i.into()), // TODO
        IDLValue::Int64(i) => Value::Int(i.into()), // TODO
        IDLValue::Float32(f) => Value::Float((f as f64).into()), // TODO: 32-bit float values?
        IDLValue::Float64(f) => Value::Float(f.into()),
        IDLValue::Number(n) => Err(ValueError::ToMotoko(n))?, // TODO
        IDLValue::Reserved => Err(ValueError::ToMotoko("Reserved Candid value".to_string()))?,
    })
}

pub fn decode_candid_args(bytes: &[u8]) -> Result<Value> {
    let args = IDLArgs::from_bytes(bytes).map_err(|e| ValueError::Candid(e.to_string()))?;

    Ok(Value::Tuple(
        args.args
            .into_iter()
            .map(|v| to_value(v).map(|v| v.share()))
            .collect::<Result<_>>()?,
    ))
}
