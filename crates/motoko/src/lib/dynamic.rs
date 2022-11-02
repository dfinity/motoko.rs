use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use crate::ast::{Inst, ToId};
use crate::value::{DynamicValue, Value, Value_};
use crate::vm_types::{Interruption, Store, CoreSource};

pub use dyn_clone::DynClone;

pub type Result<T = Value_, E = Interruption> = std::result::Result<T, E>;

use crate::type_mismatch;

// todo 20221015 -- generalize uses of Core struct into uses of Active trait.
pub trait Dynamic: Debug + DynClone + DynHash {
    fn into_value(self) -> Value
    where
        Self: 'static + Sized,
    {
        Value::Dynamic(DynamicValue(Rc::new(RefCell::new(self))))
    }

    fn get_index(&self, _store: &Store, _index: Value_) -> Result {
        Err(Interruption::IndexOutOfBounds)
    }

    fn set_index(&mut self, _store: &mut Store, _index: Value_, _value: Value_) -> Result<()> {
        Err(Interruption::IndexOutOfBounds)
    }

    fn get_field(&self, _store: &Store, name: &str) -> Result {
        Err(Interruption::UnboundIdentifer(name.to_id()))
    }

    // fn set_field(&mut self, name: &str, _value: Value_) -> Result<()> {
    //     Err(Interruption::UnboundIdentifer(name.to_string()))
    // }

    fn call(&mut self, _store: &mut Store, _inst: &Option<Inst>, _args: Value_) -> Result {
        type_mismatch!(file!(), line!())
    }

    fn iter_next(&mut self, _store: &mut Store) -> Result {
        type_mismatch!(file!(), line!())
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
