use logos::Span;

use crate::ast::{
    Case, Class, Dec, DecField, Exp, ExpField, Function, Loc, Node, Pat, PatField, Source, Type,
    TypeBind, TypeField,
};

// TODO: move to another file
pub fn get_breakpoint_span_from_line<'a>(
    tree: &'a Loc<SyntaxTree<'a>>,
    breakpoint_line: usize,
) -> Option<Span> {
    match &tree.1 {
        Source::Known { span, line, .. } => {
            if *line == breakpoint_line {
                Some(span.clone())
            } else if *line < breakpoint_line {
                let mut span = None;
                tree.for_each_child(|x| {
                    match (&span, get_breakpoint_span_from_line(x, breakpoint_line)) {
                        (None, Some(s)) => span = Some(s),
                        _ => {}
                    }
                });
                span
            } else {
                None
            }
        }
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxTree<'a> {
    Exp(&'a Exp),
    Dec(&'a Dec),
    Pat(&'a Pat),
    Type(&'a Type),
    ExpField(&'a ExpField),
    DecField(&'a DecField),
    PatField(&'a PatField),
    TypeField(&'a TypeField),
    Case(&'a Case),
    TypeBind(&'a TypeBind),
}

pub trait ToTree<'a> {
    fn tree(&'a self) -> Loc<SyntaxTree<'a>>;
}

impl<'a, S: ToNode> ToTree<'a> for Node<S> {
    fn tree(&'a self) -> Loc<SyntaxTree<'a>> {
        S::node_tree(self)
    }
}

impl<'a> ToTree<'a> for Loc<SyntaxTree<'a>> {
    fn tree(&'a self) -> Loc<SyntaxTree<'a>> {
        Loc(self.0.clone(), self.1.clone()) // TODO pointer instead of clone?
    }
}

pub trait ToNode: Sized {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>>;

    fn node(self, src: Source) -> Node<Self> {
        Loc(Box::new(self), src)
    }
}

impl ToNode for Exp {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::Exp(&*node.0), node.1.clone())
    }
}

impl ToNode for Dec {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::Dec(&*node.0), node.1.clone())
    }
}

impl ToNode for Pat {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::Pat(&*node.0), node.1.clone())
    }
}

impl ToNode for Type {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::Type(&*node.0), node.1.clone())
    }
}

impl ToNode for ExpField {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::ExpField(&*node.0), node.1.clone())
    }
}

impl ToNode for DecField {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::DecField(&*node.0), node.1.clone())
    }
}

impl ToNode for PatField {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::PatField(&*node.0), node.1.clone())
    }
}

impl ToNode for TypeField {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::TypeField(&*node.0), node.1.clone())
    }
}

impl ToNode for Case {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::Case(&*node.0), node.1.clone())
    }
}

impl ToNode for TypeBind {
    fn node_tree<'a>(node: &'a Node<Self>) -> Loc<SyntaxTree<'a>> {
        Loc(SyntaxTree::TypeBind(&*node.0), node.1.clone())
    }
}

pub trait Traverse {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, f: F);

    fn for_each_recursive<'a, F: FnMut(&Loc<SyntaxTree>)>(&'a self, mut f: F)
    where
        Self: ToTree<'a>,
    {
        f(&self.tree());
        self.for_each_child(|x| x.for_each_recursive(&mut f))
    }
}

impl<'a> Traverse for Loc<SyntaxTree<'a>> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, f: F) {
        match self.0 {
            SyntaxTree::Exp(x) => Loc(x, self.1.clone()).for_each_child(f),
            SyntaxTree::Dec(x) => Loc(x, self.1.clone()).for_each_child(f),
            SyntaxTree::Pat(x) => Loc(x, self.1.clone()).for_each_child(f),
            SyntaxTree::Type(x) => Loc(x, self.1.clone()).for_each_child(f),
            SyntaxTree::ExpField(x) => Loc(x, self.1.clone()).for_each_child(f),
            SyntaxTree::DecField(x) => Loc(x, self.1.clone()).for_each_child(f),
            SyntaxTree::PatField(x) => Loc(x, self.1.clone()).for_each_child(f),
            SyntaxTree::TypeField(x) => Loc(x, self.1.clone()).for_each_child(f),
            SyntaxTree::Case(x) => Loc(x, self.1.clone()).for_each_child(f),
            SyntaxTree::TypeBind(x) => Loc(x, self.1.clone()).for_each_child(f),
        }
    }
}

impl<C: Traverse> Traverse for Option<C> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, f: F) {
        match self {
            Some(c) => c.for_each_child(f),
            None => {}
        }
    }
}

impl<'a> Traverse for Loc<&'a Exp> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        match self.0 {
            Exp::Hole => {}
            Exp::Prim(_) => {}
            Exp::Var(_) => {}
            Exp::Literal(_) => {}
            Exp::ActorUrl(e) => f(&e.tree()),
            Exp::Un(_, e) => f(&e.tree()),
            Exp::Bin(e1, _, e2) => {
                f(&e1.tree());
                f(&e2.tree());
            }
            Exp::Rel(e1, _, e2) => {
                f(&e1.tree());
                f(&e2.tree());
            }
            Exp::Show(e) => f(&e.tree()),
            Exp::ToCandid(es) => es.iter().for_each(|e| f(&e.tree())),
            Exp::FromCandid(e) => f(&e.tree()),
            Exp::Tuple(es) => es.vec.iter().for_each(|e| f(&e.tree())),
            Exp::Proj(e, _) => f(&e.tree()),
            Exp::Opt(e) => f(&e.tree()),
            Exp::DoOpt(e) => f(&e.tree()),
            Exp::Bang(e) => f(&e.tree()),
            Exp::ObjectBlock(_, ds) => ds.vec.iter().for_each(|e| f(&e.tree())),
            Exp::Object(es) => es.vec.iter().for_each(|e| f(&e.tree())),
            Exp::Variant(_, e) => {
                if let Some(e) = e {
                    f(&e.tree());
                }
            }
            Exp::Dot(e, _) => f(&e.tree()),
            Exp::Assign(e1, e2) => {
                f(&e1.tree());
                f(&e2.tree());
            }
            Exp::Array(_, es) => es.vec.iter().for_each(|e| f(&e.tree())),
            Exp::Idx(e1, e2) => {
                f(&e1.tree());
                f(&e2.tree());
            }
            Exp::Function(Function {
                binds,
                input,
                output,
                exp,
                ..
            }) => {
                if let Some(binds) = binds {
                    binds.vec.iter().for_each(|e| f(&e.tree()));
                }
                f(&input.tree());
                if let Some(output) = output {
                    f(&output.tree());
                };
                f(&exp.tree())
            }
            Exp::Call(e1, ts, e2) => {
                f(&e1.tree());
                if let Some(ts) = ts {
                    ts.vec.iter().for_each(|t| f(&t.tree()));
                };
                f(&e2.tree());
            }
            Exp::Block(ds) => ds.vec.iter().for_each(|e| f(&e.tree())),
            Exp::Do(e) => f(&e.tree()),
            Exp::Not(e) => f(&e.tree()),
            Exp::And(e1, e2) => {
                f(&e1.tree());
                f(&e2.tree());
            }
            Exp::Or(e1, e2) => {
                f(&e1.tree());
                f(&e2.tree());
            }
            Exp::If(cond, true_, false_) => {
                f(&cond.tree());
                f(&true_.tree());
                if let Some(false_) = false_ {
                    f(&false_.tree());
                }
            }
            Exp::Switch(e, cs) => {
                f(&e.tree());
                cs.vec.iter().for_each(|c| f(&c.tree()))
            }
            Exp::While(cond, e) => {
                f(&cond.tree());
                f(&e.tree());
            }
            Exp::Loop(e1, e2) => {
                f(&e1.tree());
                if let Some(e2) = e2 {
                    f(&e2.tree());
                }
            }
            Exp::For(p, cond, e) => {
                f(&p.tree());
                f(&cond.tree());
                f(&e.tree());
            }
            Exp::Label(_, t, e) => {
                if let Some(t) = t {
                    f(&t.tree());
                }
                f(&e.tree());
            }
            Exp::Break(_, e) => {
                if let Some(e) = e {
                    f(&e.tree());
                }
            }
            Exp::Return(e) => {
                if let Some(e) = e {
                    f(&e.tree());
                }
            }
            Exp::Debug(e) => f(&e.tree()),
            Exp::Async(ts, e) => {
                f(&ts.tree());
                f(&e.tree());
            }
            Exp::Await(e) => f(&e.tree()),
            Exp::Assert(e) => f(&e.tree()),
            Exp::Annot(e, t) => {
                f(&e.tree());
                f(&t.tree());
            }
            Exp::Import(_, _) => {}
            Exp::Throw(e) => f(&e.tree()),
            Exp::Try(e, es) => {
                f(&e.tree());
                es.iter().for_each(|e| f(&e.tree()));
            }
            Exp::Ignore(e) => f(&e.tree()),
            Exp::Paren(e) => f(&e.tree()),
        }
    }
}

impl<'a> Traverse for Loc<&'a Dec> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        match &self.0 {
            Dec::Exp(e) => f(&e.clone().node(self.1.clone()).tree()), // TODO: no clone
            Dec::Let(p, e) => {
                f(&p.tree());
                f(&e.tree());
            }
            Dec::LetModule(_, _, _) => todo!(),
            Dec::Func(_) => todo!(),
            Dec::Var(p, e) => {
                f(&p.tree());
                f(&e.tree());
            }
            Dec::Type(_, ts, t) => {
                ts.vec.iter().for_each(|ts| f(&ts.tree()));
                f(&t.tree());
            }
            Dec::Class(Class {
                shared,
                binds,
                input,
                typ,
                fields,
                ..
            }) => {
                if let Some(shared) = shared {
                    f(&shared.pat.tree());
                }
                if let Some(binds) = binds {
                    binds.vec.iter().for_each(|t| f(&t.tree()));
                }
                f(&input.tree());
                if let Some(typ) = typ {
                    f(&typ.tree());
                }
                fields.vec.iter().for_each(|d| f(&d.tree()));
            }
        }
    }
}

impl<'a> Traverse for Loc<&'a Pat> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        match &self.0 {
            Pat::Wild => {}
            Pat::Var(_) => {}
            Pat::Literal(_) => {}
            Pat::Signed(_, p) => f(&p.tree()),
            Pat::Tuple(ps) => ps.vec.iter().for_each(|p| f(&p.tree())),
            Pat::Object(ps) => ps.vec.iter().for_each(|p| f(&p.tree())),
            Pat::Optional(p) => f(&p.tree()),
            Pat::Variant(_, p) => {
                if let Some(p) = p {
                    f(&p.tree());
                }
            }
            Pat::Alt(ps) => ps.vec.iter().for_each(|p| f(&p.tree())),
            Pat::Annot(p, t) => {
                f(&p.tree());
                f(&t.tree());
            }
            Pat::Paren(p) => f(&p.tree()),
        }
    }
}

impl<'a> Traverse for Loc<&'a Type> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        match &self.0 {
            Type::Prim(_) => todo!(),
            Type::Object(_, ts) => ts.vec.iter().for_each(|t| f(&t.tree())),
            Type::Array(_, ts) => ts.vec.iter().for_each(|t| f(&t.tree())),
            Type::Optional(t) => f(&t.tree()),
            Type::Tuple(ts) => ts.vec.iter().for_each(|t| f(&t.tree())),
            Type::Function(_, tbs, ts, t) => {
                tbs.vec.iter().for_each(|t| f(&t.tree()));
                ts.vec.iter().for_each(|t| f(&t.tree()));
                f(&t.tree());
            }
            Type::Async(t1, t2) => {
                f(&t1.tree());
                f(&t2.tree());
            }
            Type::And(t1, t2) => {
                f(&t1.tree());
                f(&t2.tree());
            }
            Type::Or(t1, t2) => {
                f(&t1.tree());
                f(&t2.tree());
            }
            Type::Paren(t) => f(&t.tree()),
            Type::Unknown(_) => {}
            Type::Known(_, t) => f(&t.tree()),
        }
    }
}

impl<'a> Traverse for Loc<&'a ExpField> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        if let Some(t) = &self.0.typ {
            f(&t.tree());
        }
        f(&self.0.exp.tree());
    }
}

impl<'a> Traverse for Loc<&'a DecField> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        f(&self.0.dec.tree());
    }
}

impl<'a> Traverse for Loc<&'a PatField> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        f(&self.0.pat.tree());
    }
}

impl<'a> Traverse for Loc<&'a TypeField> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        f(&self.0.typ.tree());
    }
}

impl<'a> Traverse for Loc<&'a Case> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        f(&self.0.pat.tree());
        f(&self.0.exp.tree());
    }
}

impl<'a> Traverse for Loc<&'a TypeBind> {
    fn for_each_child<F: FnMut(&Loc<SyntaxTree>)>(&self, mut f: F) {
        f(&self.0.bound.tree());
    }
}
