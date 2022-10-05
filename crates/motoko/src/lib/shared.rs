use core::ops::Deref;
use std::rc::Rc;

use serde::{Deserialize, Serialize};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Shared<T: ?Sized> {
    rc: Rc<T>,
}

impl<T: std::fmt::Debug> std::fmt::Debug for Shared<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.rc)
    }
}

impl<T: Serialize> Serialize for Shared<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serde::Serialize::serialize(&self.rc, serializer)
    }
}

impl<'de, T: Clone + Deserialize<'de>> Deserialize<'de> for Shared<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde::Deserialize::deserialize(deserializer).map(Shared::new)
    }
}

impl<T: Clone> Shared<T> {
    /// Call `value.share()` to construct a `Shared` value.
    pub fn new(x: T) -> Shared<T> {
        Shared { rc: Rc::new(x) }
    }

    #[inline(always)]
    pub fn get(&self) -> T {
        self.rc.deref().clone()
    }
}

impl<T> Deref for Shared<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &T {
        self.rc.deref()
    }
}

impl<T> AsRef<T> for Shared<T> {
    fn as_ref(&self) -> &T {
        self.rc.as_ref()
    }
}

pub trait FastClone: Clone {
    /// A more explicit alternative to `clone()` (same use case as calling `Rc::clone(_)`).
    fn fast_clone(&self) -> Self {
        self.clone()
    }
}

impl<T: Clone> FastClone for Shared<T> {}
impl<T: Clone> FastClone for Rc<T> {}
impl<T: FastClone> FastClone for Option<T> {}
impl<T: Clone> FastClone for im_rc::Vector<T> {}
impl<K: Clone, V: Clone> FastClone for im_rc::HashMap<K, V> {}

pub trait Share {
    /// TODO: gradually minimize the number of calls to this function.
    fn share(self) -> Shared<Self>;
}

// impl<T: Clone> Share<T> for T {
//     fn share(self) -> Shared<T> {
//         Shared::new(self)
//     }
// }

impl<T: Clone> Share for crate::ast::NodeData<T> {
    fn share(self) -> Shared<Self> {
        Shared::new(self)
    }
}

impl Share for String {
    fn share(self) -> Shared<Self> {
        Shared::new(self)
    }
}

impl Share for crate::value::Value {
    fn share(self) -> Shared<Self> {
        use crate::value::Value;
        std::thread_local! {
            static UNIT: Shared<Value> = Shared::new(Value::Unit);
            static NULL: Shared<Value> = Shared::new(Value::Null);
            static TRUE: Shared<Value> = Shared::new(Value::Bool(true));
            static FALSE: Shared<Value> = Shared::new(Value::Bool(false));
            // TODO: common literals such as 0, 1, "", etc?
        };
        match self {
            Value::Unit => UNIT.with(|s| s.fast_clone()),
            Value::Null => NULL.with(|s| s.fast_clone()),
            Value::Bool(true) => TRUE.with(|s| s.fast_clone()),
            Value::Bool(false) => FALSE.with(|s| s.fast_clone()),
            _ => Shared::new(self),
        }
    }
}
