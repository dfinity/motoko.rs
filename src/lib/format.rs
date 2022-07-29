#![allow(unused_imports)]
use crate::ast::{BinOp, Exp, Literal, UnOp};

use std::fmt;

struct Commas<'a, X>(&'a Vec<X>);

impl<'a, X: fmt::Display> fmt::Display for Commas<'a, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut it = self.0.iter().peekable();
        while let Some(elm) = it.next() {
            write!(f, "{}", elm)?;
            if self.0.len() == 1 || !it.peek().is_none() {
                write!(f, ", ")?;
            };
        }
        Ok(())
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;
        match self {
            Null => write!(f, "null"),
            Unit => write!(f, "()"),
            Nat(n) => write!(f, "{}", n),
            Int(i) => write!(f, "{}", i),
            Int32(i) => write!(f, "{}", i),
            _ => write!(f, "Display-TODO={:?}", self),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnOp::*;
        match self {
            Pos => write!(f, "+"),
            Neg => write!(f, "-"),
            Not => write!(f, "not"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinOp::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),
            Pow => write!(f, "**"),
            And => write!(f, "&"),
            Or => write!(f, "|"),
            Xor => write!(f, "^"),
            ShL => write!(f, "<<"),
            ShR => write!(f, " >>"),
            RotL => write!(f, "<<>"),
            RotR => write!(f, "<>>"),
            WAdd => write!(f, "+%"),
            WSub => write!(f, "-%"),
            WMul => write!(f, "*%"),
            WPow => write!(f, "**%"),
            Cat => write!(f, "#"),
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
            Un(u, e2) => write!(f, "{} {}", u, e2),
            Bin(e1, b, e2) => write!(f, "{} {} {}", e1, b, e2),
            Tup(es) => write!(f, "({})", Commas(es)),
            _ => write!(f, "Display-TODO={:?}", self),
        }
    }
}
