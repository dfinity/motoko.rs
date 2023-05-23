use crate::ast::{Exp_, Sym};
use crate::value::Value_;
use im_rc::{HashMap, Vector};
use serde::{Deserialize, Serialize};

/// Node names.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Name {
    Value_(Value_), // uses Rc for O(1) copying.
    Exp_(Exp_),     // uses Rc for O(1) copying.
    Sym(Sym),
}

/// Nodes in a demand-driven dependency graph.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Node {
    initial_exp: Option<Exp_>,
    final_value: Option<Value_>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum TraceAction {
    Nest(Name, Trace),  // for `memo`, `do @ _ { }`.
    Force(Name, Trace), // Q: keep separate from Nest?
    Put(Name, Value_),
    Get(Name, Value_),
    Ret(Value_),
}

pub type Trace = Vector<TraceAction>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Core {
    graph: HashMap<Name, Node>,
    trace: Trace,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum Interruption {
    NomGet(Name),
}

impl Core {
    pub fn new() -> Core {
        Core {
            graph: HashMap::new(),
            trace: Vector::new(),
        }
    }

    pub fn force_begin(&mut self, name: Name) {}

    pub fn force_end(&mut self, name: Name) {}

    pub fn nest_begin(&mut self, name: Name) {}

    pub fn nest_end(&mut self, name: Name) {}

    pub fn put(&mut self, name: Name, val: Value_) {}

    pub fn get(&mut self, name: Name) -> Result<Value_, Interruption> {
        Err(Interruption::NomGet(name))
    }
}
