use im_rc::{HashMap, Vector};

use crate::ast::{Dec, Id as Identifer};
use crate::value::Value;

/// Or maybe a string?
#[derive(Clone)]
pub struct Id(u64);

/// Or maybe a string?
#[derive(Clone)]
pub struct Pointer(u64);

/// Local continuation as a Dec sequence.  This Vector permits
/// sharing.
///
/// For a block, a local continuation is a list of Decs left to
/// evaluate.  A single expression injects into this type as a
/// singleton vector holding a Dec::Exp.
pub type Decs = Vector<Dec>;

pub mod stack {
    use super::Decs;
    use crate::ast::Pat;

    /// Local continuation, stored in a stack frame.
    #[derive(Clone)]
    pub enum Cont {
        Let(Pat, Decs),
        Var(Pat, Decs),
    }
    #[derive(Clone)]
    pub struct Frame {
        pub cont: Cont,
    }
    pub type Frames = im_rc::Vector<Frame>;
}

pub type Stack = stack::Frames;

/// Local environment as a mapping from identifiers to values.
/// This HashMap permits sharing.
pub type Env = HashMap<Identifer, Value>;

/// Store holds mutable variables, mutable arrays and mutable
/// records.
pub type Store = HashMap<Pointer, Value>;

/// Encapsulates VM state for "core Motoko code",
/// excluding message and actor operations.
///
/// The cost of copying this state is O(1), permitting us to
/// eventually version it and generate a DAG of relationships.
#[derive(Clone)]
pub struct Core {
    pub store: Store,
    pub stack: Stack,
    pub env: Env,
    pub cont: Decs,
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
    // - a DAG of inactive Cores, related to the active one.
    // - DAG is initially empty.
}

/// Like Local, except within the IC itself.
pub struct Canister {
    // Maybe a Core plus some extra stuff we may need?
    // Q: Unclear how the use of the ic-agent is affected.
    // Unclear how these changes affect the state we need.
}
