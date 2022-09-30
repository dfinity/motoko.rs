use std::fmt::Debug;
use std::rc::Rc;

use crate::value::Value;

pub use dyn_clone::DynClone;

pub trait Dynamic: Debug + DynClone + DynHash {
    fn get_index(&self, _index: &Value) -> Option<Rc<Value>> {
        None
    }

    fn get_field(&self, _name: &str) -> Option<Rc<Value>> {
        None
    }
}

pub trait DynHash {
    fn dyn_hash(&self, state: &mut dyn std::hash::Hasher);
}

impl<H: std::hash::Hash + ?Sized> DynHash for H {
    fn dyn_hash(&self, mut state: &mut dyn std::hash::Hasher) {
        self.hash(&mut state);
    }
}
