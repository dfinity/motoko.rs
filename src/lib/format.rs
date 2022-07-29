#![allow(unused_imports)]
use crate::ast::{Exp, Literal};

use std::fmt;

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;
        match self {
            Null => write!(f, "null"),
            Int32(i) => write!(f, "{}", i),
            _ => write!(f, "Display-TODO={:?}", self),
        }
    }
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Exp::*;
        match self {
            Hole => write!(f, "_?_"),
            Return(e) => write!(f, "return {}", e),
            Literal(l) => write!(f, "{}", l),
            _ => write!(f, "Display-TODO={:?}", self),
        }
    }
}
