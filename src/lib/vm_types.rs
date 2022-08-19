use im_rc::{HashMap, Vector};
use serde::{Deserialize, Serialize};

use crate::ast::{Dec_, Exp_, Id as Identifier, PrimType, Source, Span};
use crate::value::Value;

/// Or maybe a string?
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Id(u64);

/// Or maybe a string?
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct Pointer(pub usize);

/// Local continuation as a Dec sequence.  This Vector permits
/// sharing.
///
/// For a block, a local continuation is a list of Decs left to
/// evaluate.  A single expression injects into this type as a
/// singleton vector holding a Dec::Exp.  A fnal Value is not syntax
/// (its extensional, not intensional) and stands as its own case.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Cont {
    Taken,
    Decs(Vector<Dec_>),
    Exp_(Exp_, Vector<Dec_>),
    Value(Value), // Should we retain source locations for these values?
}

pub mod stack {
    use super::{Cont, Env, Pointer, Vector};
    use crate::ast::{BinOp, Cases, Dec_, Exp_, Id, Id_, Pat, PrimType, Source, Type_, UnOp};
    use crate::value::Value;
    use serde::{Deserialize, Serialize};

    /// Local continuation, stored in a stack frame.
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum FrameCont {
        Let(Pat, Cont),
        Var(Id, Cont),
        UnOp(UnOp),
        BinOp1(BinOp, Exp_),
        BinOp2(Value, BinOp),
        Paren,
        Variant(Id_),
        Switch(Cases),
        Do,
        Block,
        Decs(Vector<Dec_>),
        Tuple(Vector<Value>, Vector<Exp_>),
        Annot(Type_),
        Assign1(Exp_),
        Assign2(Pointer),
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Frame {
        pub env: Env,
        pub cont: FrameCont,
        pub cont_prim_type: Option<PrimType>,
        pub source: Source,
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
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Counts {
    pub step: usize,
    /*
    pub call: usize,
    pub alloc: usize,
    pub send: usize,
     */
}

/// Encapsulates VM state for "core Motoko code",
/// excluding message and actor operations.
///
/// The cost of copying this state is O(1), permitting us to
/// eventually version it and generate a DAG of relationships.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Core {
    pub store: Store,
    pub stack: Stack,
    pub env: Env,
    pub cont: Cont,
    pub cont_source: Source,
    /// `Some(t)` when evaluating under an annotation of type `t`.
    /// (`e : Nat8`  makes `Nat8` the `cont_prim_type` for `e`)
    pub cont_prim_type: Option<PrimType>,
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

// Some ideas of how we could count and limit what the VM does,
// to interject some "slow interactivity" into its execution.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Limits {
    pub breakpoints: Vec<Breakpoint>,

    pub step: Option<usize>,
    /*
    pub stack: Option<usize>,
    pub call: Option<usize>,
    pub alloc: Option<usize>,
    pub send: Option<usize>,
     */
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum Limit {
    Step,
    Stack,
    Call,
    Alloc,
    Send,
}

// to do Q -- how much detail to provide about stepping?
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Step {
    // - new context ID?
    // - log of lexical regions of steps?
    // - log of kind of steps (expression kinds)?
}

// interruptions are events that prevent steppping from progressing.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum Interruption {
    Done(Value),
    Dangling(Pointer),
    TypeMismatch,
    NoMatchingCase,
    ParseError,
    UnboundIdentifer(Identifier),
    BlockedAwaiting,
    Limit(Limit),
    DivideByZero,
    AmbiguousOperation,
    Unknown,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Signal {
    Done(Value),
    Breakpoint(Breakpoint),
    Interruption(Interruption),
}

pub type Breakpoint = Span;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Error {
    ICAgentError,
    // etc
}
