use core::ops::Deref;
use std::rc::Rc;

use serde::{Deserialize, Serialize};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Shared<T> {
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
    fn new(x: T) -> Shared<T> {
        Shared { rc: Rc::new(x) }
    }

    /// A more explicit alternative to `clone()` (same use case as calling `Rc::clone(_)`).
    pub fn fast_clone(&self) -> Self {
        self.clone()
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

pub trait Share<T> {
    /// TODO: gradually minimize the number of calls to this function.
    fn share(self) -> Shared<T>;
}

impl<T: Clone> Share<T> for T {
    fn share(self) -> Shared<T> {
        Shared::new(self)
    }
}

pub fn fast_option<X: Clone>(o: &Option<&Shared<X>>) -> Option<Shared<X>> {
    o.map(|x| x.fast_clone())
}

pub fn fast_option_<X: Clone>(o: &Option<Shared<X>>) -> Option<Shared<X>> {
    o.map(|x| x.fast_clone())
}
