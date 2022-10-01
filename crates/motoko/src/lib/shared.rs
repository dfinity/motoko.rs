use core::ops::Deref;
use std::rc::Rc;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Shared<T> {
    rc: Rc<T>,
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
    fn share(self) -> Shared<T>;
}

impl<T: Clone> Share<T> for T {
    fn share(self) -> Shared<T> {
        Shared::new(self)
    }
}
