// Reference: https://github.com/dfinity/candid/blob/master/rust/candid/src/bindings/candid.rs

use crate::ast::{
    BinOp, Dec, Delim, Exp, Literal, Mut, ObjSort, Pat, PrimType, Type, TypeBind,
    UnOp, TypeField,
};
use crate::pretty::*;
use pretty::RcDoc;

pub fn format_pretty(to_doc: &dyn ToDoc, width: usize) -> String {
    let mut w = Vec::new();
    to_doc.doc().render(width, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

pub fn format(to_doc: &dyn ToDoc) -> String {
    format_pretty(to_doc, usize::MAX)
}

pub trait ToDoc {
    fn doc(&self) -> RcDoc;
}

// impl fmt::Display for dyn ToDoc {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{}", format_pretty(self, usize::MAX))
//     }
// }

// optional delimiter on the right
fn delim<'a, T: ToDoc>(d: &'a Delim<T>, sep: &'a str) -> RcDoc<'a> {
    let doc = concat(d.vec.iter().map(|x| x.doc()), sep);
    if d.has_trailing {
        doc.append(sep)
    } else {
        doc
    }
}

// optional delimiter on the left
fn delim_left<'a, T: ToDoc>(d: &'a Delim<T>, sep: &'a str) -> RcDoc<'a> {
    let doc = concat(d.vec.iter().map(|x| x.doc()), sep);
    if d.has_trailing {
        str(sep).append(doc)
    } else {
        doc
    }
}

fn block<'a, T: ToDoc>(d: &'a Delim<T>) -> RcDoc<'a> {
    enclose_space("{", delim(d, ";"), "}")
}

fn tuple<'a, T: ToDoc>(d: &'a Delim<T>) -> RcDoc<'a> {
    enclose("(", delim(d, ","), ")")
}

fn field_block<'a, T: ToDoc>(d: &'a Delim<T>) -> RcDoc<'a> {
    enclose("{", delim(d, ","), "}")
}

fn array<'a, T: ToDoc>(m: &'a Mut, d: &'a Delim<T>) -> RcDoc<'a> {
    let m_doc = if m == &Mut::Var {
        kwd("var")
    } else {
        RcDoc::nil()
    };
    enclose("[", m_doc.append(delim(d, ",")), "]")
}

fn bind<'a, T: ToDoc>(d: &'a Delim<T>) -> RcDoc<'a> {
    if d.vec.len() == 0 {
        RcDoc::nil()
    } else {
        enclose("<", delim(d, ","), ">")
    }
}

fn bin_op<'a, E: ToDoc>(e1: &'a E, b: RcDoc<'a>, e2: &'a E) -> RcDoc<'a> {
    e1.doc()
        .append(RcDoc::space())
        .append(b)
        .append(RcDoc::space())
        .append(e2.doc())
}

impl<T> ToDoc for Box<T>
where
    T: ToDoc,
{
    fn doc(&self) -> RcDoc {
        self.as_ref().doc()
    }
}

impl ToDoc for Literal {
    fn doc(&self) -> RcDoc {
        use Literal::*;
        str(match self {
            Null => "null",
            Bool(true) => "true",
            Bool(false) => "false",
            Unit => "()",
            Nat(n) => n,
            Int(i) => i,
            Float(f) => f,
            Text(t) => t,
            // Char(c) => format!("{}", c),
            Blob(_) => unimplemented!(),
            _ => todo!(),
            // _ => text("Display-TODO={:?}", self),
        })
    }
}

impl ToDoc for UnOp {
    fn doc(&self) -> RcDoc {
        use UnOp::*;
        str(match self {
            Pos => "+",
            Neg => "-",
            Not => "^",
        })
    }
}

impl ToDoc for BinOp {
    fn doc(&self) -> RcDoc {
        use BinOp::*;
        str(match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            Pow => "**",
            And => "&",
            Or => "|",
            Xor => "^",
            ShL => "<<",
            ShR => " >>",
            RotL => "<<>",
            RotR => "<>>",
            WAdd => "+%",
            WSub => "-%",
            WMul => "*%",
            WPow => "**%",
            Cat => "#",
        })
    }
}

impl ToDoc for Exp {
    fn doc(&self) -> RcDoc {
        use Exp::*;
        match self {
            Hole => str("_?_"),
            Return(e) => kwd("return").append(e.doc()),
            Literal(l) => l.doc(),
            Un(u, e2) => u.doc().append(e2.doc()),
            Bin(e1, b, e2) => bin_op(e1, b.doc(), e2),
            Tuple(es) => tuple(es),
            Prim(_) => unimplemented!(),
            Var(id) => str(id),
            ActorUrl(_) => todo!(),
            Rel(_, _) => todo!(),
            Show(e) => kwd("debug_show").append(e.doc()),
            ToCandid(_) => todo!(),
            FromCandid(_) => todo!(),
            Proj(e1, n) => e1.doc().append(format!(".{}", n)),
            Opt(e) => str("?").append(e.doc()), // TODO parentheses
            DoOpt(e) => kwd("do ?").append(e.doc()),
            Bang(e) => e.doc().append("!"), // TODO parentheses
            ObjectBlock(_, _) => todo!(),
            Object(_) => todo!(),
            Variant(_, _) => todo!(),
            Dot(e, s) => e.doc().append(".").append(s), // TODO parentheses
            Assign(_, _) => todo!(),
            Array(m, es) => array(m, es),
            Idx(e, idx) => e.doc().append("[").append(idx.doc()).append("]"),
            Function(_, _, _, _, _, _, _) => todo!(),
            Call(e, b, a) => e.doc().append(bind(b)).append(enclose("(", a.doc(), ")")),
            Block(decs) => block(decs),
            DoBlock(decs) => kwd("do").append(block(decs)),
            Not(e) => kwd("not").append(e.doc()),
            And(e1, e2) => bin_op(e1, str("and"), e2),
            Or(e1, e2) => bin_op(e1, str("or"), e2),
            Switch(_, _) => todo!(),
            While(c, e) => kwd("while")
                .append(c.doc())
                .append(RcDoc::space())
                .append(e.doc()),
            Loop(_, _) => todo!(),
            For(p, c, e) => kwd("for")
                .append(p.doc())
                .append(" of ")
                .append(c.doc())
                .append(RcDoc::space())
                .append(e.doc()),
            Label(_, _, _) => todo!(),
            Break(_, _) => todo!(),
            Debug(_) => todo!(),
            Async(_, _) => todo!(),
            Await(e) => kwd("await").append(e.doc()),
            Assert(e) => kwd("assert").append(e.doc()),
            Annot(e, t) => e.doc().append(" : ").append(t.doc()),
            Import(_) => todo!(),
            Throw(e) => kwd("throw").append(e.doc()),
            Try(_, _) => todo!(),
            Ignore(e) => kwd("ignore").append(e.doc()),
            Paren(e) => enclose("(", e.doc(), ")"),
        }
        // _ => text("Display-TODO={:?}", self),
    }
}

impl ToDoc for Delim<Dec> {
    fn doc(&self) -> RcDoc {
        delim(self, ";")
    }
}

impl ToDoc for Dec {
    fn doc(&self) -> RcDoc {
        use Dec::*;
        match self {
            Exp(e) => e.doc(),
            Let(p, e) => kwd("let")
                .append(p.doc())
                .append(str(" = "))
                .append(e.doc()),
            Var(s, e) => kwd("var").append(kwd(s)).append(str(" = ")).append(e.doc()),
            Typ(i, b, t) => kwd("type")
                .append(i)
                .append(bind(b))
                .append(" = ")
                .append(t.doc()),
            Class(_, _, _, _, _, _, _, _) => todo!(),
        }
    }
}

impl ToDoc for ObjSort {
    fn doc(&self) -> RcDoc {
        str(match self {
            ObjSort::Object => "object",
            ObjSort::Actor => "actor",
            ObjSort::Module => "module",
        })
    }
}

impl ToDoc for Type {
    fn doc(&self) -> RcDoc {
        use Type::*;
        match self {
            Prim(p) => p.doc(),
            Object(s, fs) => s.doc().append(RcDoc::space()).append(field_block(fs)),
            Array(m, d) => array(m, d),
            Optional(t) => str("?").append(t.doc()),
            Tuple(d) => tuple(d),
            Function(_, _, _, _) => todo!(),
            // Async(s, t) => kwd("async").append(t.doc()),
            Async(s, t) => unimplemented!(), // scope?
            And(e1, e2) => bin_op(e1, str("and"), e2),
            Or(e1, e2) => bin_op(e1, str("or"), e2),
            Paren(e) => enclose("(", e.doc(), ")"),
            Named(id, t) => str(id).append(" : ").append(t.doc()),
        }
    }
}

impl ToDoc for PrimType {
    fn doc(&self) -> RcDoc {
        use PrimType::*;
        str(match self {
            Unit => "()",
            Bool => "Bool",
            Nat => "Nat",
            Nat8 => "Nat8",
            Nat16 => "Nat16",
            Nat32 => "Nat32",
            Nat64 => "Nat64",
            Int => "Int",
            Int8 => "Int8",
            Int16 => "Int16",
            Int32 => "Int32",
            Int64 => "Int64",
            Principal => "Principal",
            Text => "Text",
        })
    }
}

impl ToDoc for TypeBind {
    fn doc(&self) -> RcDoc {
        unimplemented!() // type vs. scope bindings?
                         // str(self.var).append("")
    }
}

impl ToDoc for Pat {
    fn doc(&self) -> RcDoc {
        use Pat::*;
        match self {
            Wild => str("_"),
            Var(s) => str(s),
            Literal(l) => l.doc(),
            Signed(u, p) => u.doc().append(p.doc()),
            Tuple(ps) => tuple(ps),
            Object(_) => todo!(),
            Optional(p) => str("?").append(p.doc()),
            Variant(s, p) => str("#")
                .append(kwd(s))
                .append(p.as_ref().map(|p| p.doc()).unwrap_or(RcDoc::nil())),
            Alt(d) => delim_left(d, " |"),
            Annot(p, t) => p.doc().append(" : ").append(t.doc()),
            Paren(p) => enclose("(", p.doc(), ")"),
        }
    }
}

trait FieldValue {
    fn sep<'a>() -> &'a str;
}

impl ToDoc for TypeField {
    fn doc(&self) -> RcDoc {
        str(&self.id).append(" = ").append(self.typ.doc())
    }
}
