#![allow(unused_imports)]
use crate::ast::Exp_ as Exp; // Maybe we could use `type Exp_ = Box<Exp>; enum Exp = { ... }` to simplify imports?

use std::fmt;

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Exp::*;
        match self {
            Hole => write!(f, "_?_"),
            _ => write!(f, "(UNIMPLEMENTED)"),
        }
    }
}
