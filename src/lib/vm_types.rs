use im_rc::{HashMap, Vector};

use crate::ast::{Dec, Exp_, Id as Identifier};
use crate::value::Value;

/// Or maybe a string?
#[derive(Debug, Clone)]
pub struct Id(u64);

/// Or maybe a string?
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Pointer(u64);

/// Local continuation as a Dec sequence.  This Vector permits
/// sharing.
///
/// For a block, a local continuation is a list of Decs left to
/// evaluate.  A single expression injects into this type as a
/// singleton vector holding a Dec::Exp.  A fnal Value is not syntax
/// (its extensional, not intensional) and stands as its own case.
#[derive(Debug, Clone)]
pub enum Cont {
    Taken,
    Decs(Vector<Dec>),
    Exp_(Exp_),
    Value(Value),
}

/// Some(Value) exists if and only if the continuation is fully evaluated.
pub fn cont_is_value(_env: &Env, _c: Cont) -> Option<Value> {
    unimplemented!()
}

pub mod stack {
    use super::{Cont, Env};
    use crate::ast::{BinOp, Exp, Exp_, Pat};
    use crate::value::Value;

    /// Local continuation, stored in a stack frame.
    #[derive(Debug, Clone)]
    pub enum FrameCont {
        Let(Pat, Cont),
        Var(Pat, Cont),
        BinOp1(BinOp, Exp_),
        BinOp2(Value, BinOp),
    }
    #[derive(Debug, Clone)]
    pub struct Frame {
        pub env: Env,
        pub cont: FrameCont,
    }
    pub type Frames = im_rc::Vector<Frame>;
}

pub type Stack = stack::Frames;

/// Local environment as a mapping from identifiers to values.
/// This HashMap permits sharing.
pub type Env = HashMap<Identifier, Value>;

/// Store holds mutable variables, mutable arrays and mutable
/// records.
pub type Store = HashMap<Pointer, Value>;

/// Counts. Some ideas of how we could count and limit what the VM
/// does, to interject some "slow interactivity" into its execution.
#[derive(Clone, Debug)]
pub struct Counts {
    pub step: usize,
    pub stack: usize,
    pub call: usize,
    pub alloc: usize,
    pub send: usize,
}

/// Encapsulates VM state for "core Motoko code",
/// excluding message and actor operations.
///
/// The cost of copying this state is O(1), permitting us to
/// eventually version it and generate a DAG of relationships.
#[derive(Clone, Debug)]
pub struct Core {
    pub store: Store,
    pub stack: Stack,
    pub env: Env,
    pub cont: Cont,
    pub counts: Counts,
}

/// Encapsulates the VM state running Motoko code locally,
/// as a script interacting with the internet computer from the
/// outside of the IC.
///
/// Ideally, permits multiple "actors" to run locally and send
/// messages to IC canisters and get responses, as well as
/// interact with each other, within an interpreted context that
/// additionall permits meta-level VM-like operations (suspension,
/// inspection, resumption, switch active Core contex, etc.).
pub struct Local {
    // to do
    // - one "active" Core.
    pub active: Core,
    // - a DAG of inactive Cores, related to the active one.
    // - DAG is initially empty.
}

/// Like Local, except within the IC itself.
pub struct Canister {
    // Maybe a Core plus some extra stuff we may need?
    // Q: Unclear how the use of the ic-agent is affected.
    // Unclear how these changes affect the state we need.
}
