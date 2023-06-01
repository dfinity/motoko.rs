use crate::ast::{Exp_, Sym};
use crate::shared::Share;
use crate::value::{Closed, Value, Value_};
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
    initial_exp: Option<Closed<Exp_>>,
    final_value: Option<Value_>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum TraceAction {
    Nest(Name, Trace), // for `memo`, `do @ _ { }`, `force`
    Put(Name, Value_),
    Get(Name, Value_),
    Ret(Value_),
}

pub type Trace = Vector<TraceAction>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TraceStackFrame {
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

    pub fn nest_begin(&mut self, name: Name) {
        self.trace_stack.push_back(TraceStackFrame {
            name,
            trace: Vector::new(),
        })
    }

    pub fn nest_end(&mut self) {
        match self.trace_stack.pop_back() {
            None => unreachable!(),
            Some(frame) => self.trace_action(TraceAction::Nest(frame.name, frame.trace)),
        }
    }

    fn trace_action(&mut self, ta: TraceAction) {
        match self.trace_stack.back_mut() {
            None => self.trace.push_back(ta),
            Some(frame) => frame.trace.push_back(ta),
        }
    }

    pub fn put_(&mut self, name: Name, val: Value_) {
        let node = match &*val {
            Value::Thunk(closed_exp) => Node {
                initial_exp: Some(closed_exp.clone()),
                final_value: None,
            },
            _ => Node {
                initial_exp: None,
                final_value: Some(val.clone()),
            },
        };
        let _ = self.graph.insert(name.clone(), node);
        self.trace_action(TraceAction::Put(name, val));
    }

    pub fn put(&mut self, name: Name, val: Value_) {
        self.put_(name.clone(), val.clone());
        self.trace_action(TraceAction::Put(name, val));
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
            Some(node) => {
                // Either the final_value is known, or the node
                // represents a thunk with a known, closed initial expression.
                match &node.final_value {
                    Some(v) => Ok(v.clone()),
                    None => match &node.initial_exp {
                        None => unreachable!(),
                        Some(ce) => Ok(Value::Thunk(ce.clone()).share()),
                    },
                }
            }
            None => Err(Interruption::NomGet(name.clone())),
        };
        let v = v?;
        self.trace_action(TraceAction::Get(name.clone(), v.clone()));
        Ok(v)
    }
}
