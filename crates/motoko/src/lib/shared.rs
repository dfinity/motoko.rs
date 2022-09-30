use std::rc::Rc;
use core::ops::Deref;

#[derive(Clone)]
pub struct Shared<T> {
    rc: Rc<T>
}

impl<T:Clone> Shared<T> {
    fn new(x:T) -> Shared<T> {
        Shared{ rc: Rc::new(x) }
    }
    #[inline(always)]
    fn get(&self) -> T {
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
