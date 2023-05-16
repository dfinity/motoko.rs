use crate::ast::{Exp_, Sym};
use crate::value::Value_;
use im_rc::HashMap;

/// Node names.
#[derive(Debug, Clone)]
pub enum Name {
    Value_(Value_), // uses Rc for O(1) copying.
    Exp_(Exp_), // uses Rc for O(1) copying.
    Sym(Sym),
}

/// Nodes in a demand-driven dependency graph.
pub struct Node {
    initial_exp: Option<Exp_>,
    final_value: Option<Value_>,
}

#[derive(Debug, Clone)]
pub enum Trace {
    Seq(Vec<Trace>),
    Nest(Name, Vec<Trace>), // for `memo` and `do @ _ { }`.
    Put(Name, Value_),
    Get(Name, Value_),
    Force(Name, Vec<Trace>),
    Ret(Value_),
}

pub struct Core {
    graph: HashMap<Name, Node>,
    trace: Trace,
}
