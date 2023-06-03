use crate::ast::{Exp_, Sym};
use crate::value::{Closed, Value_};
use im_rc::{HashMap, Vector};
use serde::{Deserialize, Serialize};

/// Node names.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Name {
    Exp_(Closed<Exp_>),
    Sym(Sym),
}

/// Nodes in a demand-driven dependency graph.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Node {
    put: Value_,
    force: Option<Value_>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum TraceAction {
    Memo(Name, Trace),
    DoAt(Name, Trace),
    Force(Name, Trace),
    Put(Name, Value_),
    Get(Name, Value_),
    Ret(Value_),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum TraceNestKind {
    Memo,
    DoAt,
    Force,
}

pub type Trace = Vector<TraceAction>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TraceStackFrame {
    pub kind: TraceNestKind,
    pub name: Name,
    pub trace: Trace,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Core {
    pub graph: HashMap<Name, Node>,
    pub trace_stack: Vector<TraceStackFrame>, // pushed for nested traces.
    pub trace: Trace,
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
            trace_stack: Vector::new(),
        }
    }

    pub fn nest_begin(&mut self, kind: TraceNestKind, name: Name) {
        self.trace_stack.push_back(TraceStackFrame {
            kind,
            name,
            trace: Vector::new(),
        })
    }

    pub fn nest_end(&mut self, return_value: Value_) {
        self.trace_action(TraceAction::Ret(return_value));
        match self.trace_stack.pop_back() {
            None => unreachable!(),
            Some(frame) => {
                let ta = match frame.kind {
                    TraceNestKind::Memo => TraceAction::Memo(frame.name, frame.trace),
                    TraceNestKind::Force => TraceAction::Force(frame.name, frame.trace),
                    TraceNestKind::DoAt => TraceAction::DoAt(frame.name, frame.trace),
                };
                self.trace_action(ta);
            }
        }
    }

    fn trace_action(&mut self, ta: TraceAction) {
        match self.trace_stack.back_mut() {
            None => self.trace.push_back(ta),
            Some(frame) => frame.trace.push_back(ta),
        }
    }

    pub fn put_(&mut self, name: Name, val: Value_) {
        let node = Node {
            put: val.clone(),
            force: None,
        };
        let _ = self.graph.insert(name.clone(), node);
        self.trace_action(TraceAction::Put(name, val));
    }

    pub fn put(&mut self, name: Name, val: Value_) {
        self.put_(name.clone(), val.clone());
        self.trace_action(TraceAction::Put(name, val));
    }

    pub fn set_force_val(&mut self, name: &Name, val: Value_) {
        self.trace_action(TraceAction::Ret(val.clone()));
        match self.graph.get_mut(name) {
            Some(node) => node.force = Some(val),
            None => unreachable!(),
        }
    }

    pub fn get_force_val(&mut self, name: &Name) -> Option<Value_> {
        match self.graph.get(name) {
            Some(node) => node.force.clone(),
            None => unreachable!(),
        }
    }

    pub fn memo_exists(&mut self, _closed_exp: &Closed<Exp_>) -> bool {
        // to do.
        false
    }

    pub fn memo_put(&mut self, closed_exp: &Closed<Exp_>, val: Value_) {
        let name = Name::Exp_(closed_exp.clone());
        self.put_(name, val)
    }

    pub fn get(&mut self, name: &Name) -> Result<Value_, Interruption> {
        let v = match self.graph.get(name) {
            Some(node) => Ok(node.put.clone()),
            None => Err(Interruption::NomGet(name.clone())),
        };
        let v = v?;
        self.trace_action(TraceAction::Get(name.clone(), v.clone()));
        Ok(v)
    }
}
