use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use crate::ast::Inst;
use crate::value::{DynamicValue, Value, Value_};
use crate::vm_types::Interruption;

pub use dyn_clone::DynClone;

pub type Result<T = Value_> = std::result::Result<T, Interruption>;

pub trait Dynamic: Debug + DynClone + DynHash {
    fn into_value(self) -> Value
    where
        Self: 'static + Sized,
    {
        Value::Dynamic(DynamicValue(Rc::new(RefCell::new(self))))
    }

    fn get_index(&self, _index: &Value) -> Result {
        Err(Interruption::IndexOutOfBounds)
    }

    fn set_index(&mut self, _index: Value_, _value: Value_) -> Result<()> {
        Err(Interruption::IndexOutOfBounds)
    }

    fn get_field(&self, name: &str) -> Result {
        Err(Interruption::UnboundIdentifer(name.to_string()))
    }

    // fn set_field(&mut self, name: &str, _value: Value_) -> Result<()> {
    //     Err(Interruption::UnboundIdentifer(name.to_string()))
    // }

    fn call(&self, _inst: &Option<Inst>, _args: Value_) -> Result {
        Err(Interruption::TypeMismatch)
    }
}

pub trait DynHash {
    fn dyn_hash(&self, state: &mut dyn std::hash::Hasher);
}

impl<T: std::hash::Hash + ?Sized> DynHash for T {
    fn dyn_hash(&self, mut state: &mut dyn std::hash::Hasher) {
        self.hash(&mut state);
    }
}
