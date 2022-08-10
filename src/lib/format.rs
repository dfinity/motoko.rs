// Reference: https://github.com/dfinity/candid/blob/master/rust/candid/src/bindings/candid.rs

use crate::ast::{
    BinOp, BindSort, Case, Dec, DecField, Delim, Exp, ExpField, Literal, Mut, ObjSort, Pat,
    PrimType, RelOp, Stab, Type, TypeBind, TypeField, UnOp, Vis,
};
use crate::format_utils::*;
use crate::lexer::{is_keyword, GroupSort, Token, TokenTree};
use pretty::RcDoc;

fn format_(doc: RcDoc, width: usize) -> String {
    let mut w = Vec::new();
    doc.render(width, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

pub fn format_pretty(to_doc: &dyn ToDoc, width: usize) -> String {
    format_(to_doc.doc(), width)
}

pub fn format_one_line(to_doc: &dyn ToDoc) -> String {
    format_(to_doc.doc().group(), usize::MAX)
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
    let doc = strict_concat(d.vec.iter().map(|x| x.doc()), sep);
    if d.has_trailing {
        doc.append(sep)
    } else {
        doc
    }
}

// optional delimiter on the left
fn delim_left<'a, T: ToDoc>(d: &'a Delim<T>, sep: &'a str) -> RcDoc<'a> {
    let doc = strict_concat(d.vec.iter().map(|x| x.doc()), sep);
    if d.has_trailing {
        str(sep).append(RcDoc::space()).append(doc)
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
    enclose("[", m.doc().append(delim(d, ",")), "]")
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

// impl<'a> ToDoc for RcDoc<'a> {
//     fn doc(&'a self) -> RcDoc<'a> {
//         self // TODO no clone
//     }
// }

impl ToDoc for String {
    fn doc(&self) -> RcDoc {
        str(&self)
    }
}

impl<T: ToDoc> ToDoc for Box<T> {
    fn doc(&self) -> RcDoc {
        self.as_ref().doc()
    }
}

impl<T: ToDoc> ToDoc for Option<T> {
    fn doc(&self) -> RcDoc {
        match self {
            None => RcDoc::nil(),
            Some(value) => value.doc(),
        }
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
            Char(c) => c,
            Blob(_) => unimplemented!(),
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
            And => "and",
            Or => "or",
            BitAnd => "&",
            BitOr => "|",
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

impl ToDoc for RelOp {
    fn doc(&self) -> RcDoc {
        use RelOp::*;
        str(match self {
            Eq => "==",
            Neq => "!=",
            Lt => "<",
            Gt => ">",
            Le => "<=",
            Ge => ">=",
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
            Rel(e1, r, e2) => bin_op(e1, r.doc(), e2),
            Show(e) => kwd("debug_show").append(e.doc()),
            ToCandid(_) => todo!(),
            FromCandid(_) => todo!(),
            Proj(e1, n) => e1.doc().append(format!(".{}", n)),
            Opt(e) => str("?").append(e.doc()),
            DoOpt(e) => kwd("do ?").append(e.doc()),
            Bang(e) => e.doc().append("!"),
            ObjectBlock(s, fs) => s.doc().append(RcDoc::space()).append(block(fs)),
            Object(fs) => block(fs),
            Variant(id, e) => str("#").append(id.doc()).append(match e {
                None => RcDoc::nil(),
                Some(e) => RcDoc::space().append(e.doc()),
            }),
            Dot(e, s) => e.doc().append(".").append(s),
            Assign(from, to) => from.doc().append(str(" := ")).append(to.doc()),
            Array(m, es) => array(m, es),
            Idx(e, idx) => e.doc().append("[").append(idx.doc()).append("]"),
            Function(_, _, _, _, _, _, _) => todo!(),
            Call(e, b, a) => e.doc().append(bind(b)).append(enclose("(", a.doc(), ")")),
            Block(decs) => block(decs),
            Do(e) => kwd("do").append(e.doc()),
            Not(e) => kwd("not").append(e.doc()),
            And(e1, e2) => bin_op(e1, str("and"), e2),
            Or(e1, e2) => bin_op(e1, str("or"), e2),
            If(e1, e2, e3) => kwd("if")
                .append(e1.doc())
                .append(RcDoc::space())
                .append(e2.doc())
                .append(match e3 {
                    None => RcDoc::nil(),
                    Some(e3) => RcDoc::space().append(kwd("else")).append(e3.doc()),
                }),
            Switch(e, cs) => kwd("switch")
                .append(e.doc())
                .append(RcDoc::space())
                .append(enclose_space("{", delim(cs, ";"), "}")),
            While(c, e) => kwd("while")
                .append(c.doc())
                .append(RcDoc::space())
                .append(e.doc()),
            Loop(e, w) => kwd("loop").append(e.doc()).append(match w {
                None => RcDoc::nil(),
                Some(w) => RcDoc::space().append(w.doc()),
            }),
            For(p, c, e) => kwd("for")
                .append(p.doc())
                .append(" of ")
                .append(c.doc())
                .append(RcDoc::space())
                .append(e.doc()),
            Label(id, t, e) => kwd("label")
                .append(id)
                .append(match t {
                    None => RcDoc::nil(),
                    Some(t) => str(" : ").append(t.doc()),
                })
                .append(RcDoc::space())
                .append(e.doc()),
            Break(id, e) => kwd("break").append(id).append(match e {
                None => RcDoc::nil(),
                Some(e) => RcDoc::space().append(e.doc()),
            }),
            Debug(e) => kwd("debug").append(e.doc()),
            Async(_, _) => todo!(),
            Await(e) => kwd("await").append(e.doc()),
            Assert(e) => kwd("assert").append(e.doc()),
            Annot(e, t) => e.doc().append(" : ").append(t.doc()),
            Import(s, _) => kwd("import").append(s), // new permissive syntax?
            Throw(e) => kwd("throw").append(e.doc()),
            Try(e, cs) => {
                let mut doc = kwd("try").append(e.doc());
                // ?????
                for c in cs {
                    doc = doc
                        .append(RcDoc::line())
                        .append(kwd("catch"))
                        .append(c.pat.doc())
                        .append(RcDoc::space())
                        .append(c.exp.doc())
                }
                doc
            }
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
            Var(p, e) => kwd("var")
                .append(p.doc())
                .append(str(" = "))
                .append(e.doc()),
            Typ(i, b, t) => kwd("type")
                .append(i)
                .append(bind(b))
                .append(" = ")
                .append(t.doc()),
            Class(_, _, _, _, _, _, _, _) => todo!(),
        }
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
            Async(_, _) => unimplemented!(), // scope?
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
            Null => "Null",
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
            Float => "Float",
            Char => "Char",
            Text => "Text",
            Principal => "Principal",
        })
    }
}

impl ToDoc for TypeBind {
    fn doc(&self) -> RcDoc {
        use BindSort::*;
        match self.sort {
            Scope => str("$"), // ?
            Type => RcDoc::nil(),
        }
        .append(&self.var)
        .append(" : ")
        .append(self.bound.doc())
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
                .append(s.doc())
                .append(p.as_ref().map(|p| p.doc()).unwrap_or(RcDoc::nil())),
            Alt(d) => delim_left(d, " |"),
            Annot(p, t) => p.doc().append(" : ").append(t.doc()),
            Paren(p) => enclose("(", p.doc(), ")"),
        }
    }
}

impl ToDoc for Case {
    fn doc(&self) -> RcDoc {
        kwd("case")
            .append(self.pat.doc())
            .append(RcDoc::line())
            .append(self.exp.doc())
            .group()
    }
}

impl ToDoc for TypeField {
    fn doc(&self) -> RcDoc {
        str(&self.id).append(" = ").append(self.typ.doc())
    }
}

impl ToDoc for DecField {
    fn doc(&self) -> RcDoc {
        match &self.vis {
            None => RcDoc::nil(),
            Some(v) => v.doc().append(RcDoc::space()),
        }
        .append(match &self.stab {
            None => RcDoc::nil(),
            Some(s) => s.doc().append(RcDoc::space()),
        })
        .append(self.dec.doc())
    }
}

impl ToDoc for ExpField {
    fn doc(&self) -> RcDoc {
        self.mut_
            .doc()
            .append(&self.id)
            .append(match &self.typ {
                None => RcDoc::nil(),
                Some(typ) => str(" : ").append(typ.doc()),
            })
            .append(" = ")
            .append(self.exp.doc())
    }
}

impl ToDoc for Vis {
    fn doc(&self) -> RcDoc {
        use Vis::*;
        match self {
            Public(Some(_)) => todo!(), // ??
            Public(None) => str("public"),
            Private => str("private"),
            System => str("system"),
        }
    }
}

impl ToDoc for Stab {
    fn doc(&self) -> RcDoc {
        use Stab::*;
        str(match self {
            Stable => "stable",
            Flexible => "flexible",
        })
    }
}

impl ToDoc for Mut {
    fn doc(&self) -> RcDoc {
        use Mut::*;
        match self {
            Var => kwd("var"), // includes space after keyword
            Const => RcDoc::nil(),
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

// Token-based code formatter

fn filter_whitespace(trees: &[TokenTree]) -> Vec<&TokenTree> {
    let mut results = vec![];
    filter_whitespace_(trees, &mut results);
    results
}

fn filter_whitespace_<'a>(trees: &'a [TokenTree], results: &mut Vec<&'a TokenTree>) {
    for tt in trees {
        if match tt {
            TokenTree::Token(Token::Space(_), _) => false,
            _ => true,
        } {
            results.push(tt);
        }
    }
}

fn get_space<'a>(a: &'a TokenTree, b: &'a TokenTree) -> RcDoc<'a> {
    use crate::lexer::Token::*;
    use GroupSort::*;
    use TokenTree::*;
    match (a, b) {
        // TODO: refactor these rules to a text-based configuration file
        (Token(Space(_), _), _) | (_, Token(Space(_), _)) => nil(),
        (Token(MultiLineSpace(_), _), _) | (_, Token(MultiLineSpace(_), _)) => nil(),
        (Token(Ident(s), _), Group(_, g, _))
            if !is_keyword(s) && (g == &Paren || g == &Square || g == &Angle) =>
        {
            nil()
        }
        (Token(Open(_), _), _) | (_, Token(Close(_), _)) => nil(),
        (_, Token(Delim(_), _)) => nil(),
        (Token(Delim(_), _), _) => line(),
        (Token(Dot(_), _), _) => nil(),
        (Token(Hash(_), _), _) => nil(),
        (_, Token(Dot(_), _)) => wrap_(),
        (Token(Assign(_), _), _) => wrap(),
        (_, Group(_, Comment, _)) => wrap(),
        (Group(_, Comment, _), _) => line(),
        _ => space(),
    }
}

impl ToDoc for TokenTree {
    fn doc(&self) -> RcDoc {
        use GroupSort::*;
        use TokenTree::*;
        match self {
            Token(t, _) => t.doc(),
            Group(trees, sort, pair) => {
                let trees = filter_whitespace(trees);
                let doc = match trees.first() {
                    None => RcDoc::nil(),
                    Some(tt) => {
                        let mut doc = tt.doc();
                        for i in 0..trees.len() - 1 {
                            let (a, b) = (trees[i], trees[i + 1]);
                            doc = doc.append(get_space(a, b)).append(b.doc());
                        }
                        doc
                    }
                };
                // let concat = RcDoc::concat(docs);
                let (open, close) = if let Some(((open, _), (close, _))) = pair {
                    (&open.data().unwrap()[..], &close.data().unwrap()[..])
                } else {
                    ("", "") // TODO refactor GroupSort into Option
                };
                match sort {
                    Unenclosed => doc,
                    Curly => enclose_space(open, doc, close),
                    Paren | Square | Angle => enclose(open, doc, close),
                    // Comment => str(open).append(format!("{}", self)).append(close),
                    Comment => str("").append(format!("{}", self)),
                }
            }
        }
    }
}

impl ToDoc for Token {
    fn doc(&self) -> RcDoc {
        use Token::*;
        match self {
            &MultiLineSpace(_) => RcDoc::hardline().append(RcDoc::hardline()),
            t => str(t.data().unwrap()),
        }
        // str(self.data().unwrap())
    }
}
