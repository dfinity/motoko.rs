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
            Bool(true) => write!(f, "true"),
            Bool(false) => write!(f, "false"),
            Unit => write!(f, "()"),
            Nat(n) => write!(f, "{}", n),
            Nat8(n) => write!(f, "{}", n),
            Nat16(n) => write!(f, "{}", n),
            Nat32(n) => write!(f, "{}", n),
            Nat64(n) => write!(f, "{}", n),
            Int(i) => write!(f, "{}", i),
            Int8(i) => write!(f, "{}", i),
            Int16(i) => write!(f, "{}", i),
            Int32(i) => write!(f, "{}", i),
            Int64(i) => write!(f, "{}", i),
            Float(s) => write!(f, "{}", s),
            Text(t) => write!(f, "{}", t),
            Char(c) => write!(f, "{}", c),
            Blob(v) => todo!(),
            // _ => write!(f, "Display-TODO={:?}", self),
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
            Tuple(es) => write!(f, "({})", Commas(es)),
            Prim(_) => todo!(),
            Var(id) => write!(f, "{}", id),
            ActorUrl(_) => todo!(),
            Rel(_, _) => todo!(),
            Show(_) => todo!(),
            ToCandid(_) => todo!(),
            FromCandid(_) => todo!(),
            Proj(_, _) => todo!(),
            Opt(_) => todo!(),
            DoOpt(_) => todo!(),
            Bang(_) => todo!(),
            ObjectBlock(_, _) => todo!(),
            Object(_) => todo!(),
            Variant(_, _) => todo!(),
            Dot(_, _) => todo!(),
            Assign(_, _) => todo!(),
            Array(_, _) => todo!(),
            Idx(_, _) => todo!(),
            Function(_, _, _, _, _, _) => todo!(),
            Call(_, _, _) => todo!(),
            Block(_) => todo!(),
            Not(_) => todo!(),
            And(_, _) => todo!(),
            Or(_, _) => todo!(),
            Of(_, _, _) => todo!(),
            Switch(_, _) => todo!(),
            While(_, _) => todo!(),
            Loop(_, _) => todo!(),
            For(_, _, _) => todo!(),
            Label(_, _, _) => todo!(),
            Break(_, _) => todo!(),
            Debug(_) => todo!(),
            Async(_, _) => todo!(),
            Await(_) => todo!(),
            Assert(_) => todo!(),
            Annot(_, _) => todo!(),
            Import(_) => todo!(),
            Throw(_) => todo!(),
            Try(_, _) => todo!(),
            Ignore(_) => todo!(),
            // _ => write!(f, "Display-TODO={:?}", self),
        }
    }
}
