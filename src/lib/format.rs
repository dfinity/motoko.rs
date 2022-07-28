#![allow(unused_imports)]
use crate::ast::Exp;

use std::fmt;

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Exp::*;
        match self {
            Hole => write!(f, "_?_"),
        }
    }
}
