use im_rc::{HashMap, Vector};
use serde::{Deserialize, Serialize};

use crate::ast::{Dec_, Exp_, Id as Identifier, Id_, PrimType, Source, Span};
#[cfg(feature = "parser")]
use crate::parser_types::SyntaxError;
use crate::value::{Value, ValueError};

pub mod def {
    use crate::ast::{Stab_, Vis_};
    use im_rc::HashMap;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub struct CtxId(usize);

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Ctx {
        parent: Option<CtxId>,
        fields: HashMap<CtxId, Field>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Field {
        pub def: Def,
        pub vis: Option<Vis_>,
        pub stab: Option<Stab_>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub enum Def {
        Module(Module),
        Function(Function),
        Value(crate::value::Value),
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Module {
        parent: CtxId,
        fields: CtxId,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Function {
        context: CtxId,
        function: crate::ast::Function,
    }
}

/// Or maybe a string?
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Id(u64);

/// Or maybe a string?
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct Pointer(pub usize);

/// Local continuation as a Dec sequence.  This Vector permits
/// sharing.
///
/// For a block, a local continuation is a list of Decs left to
/// evaluate.  A single expression injects into this type as a
/// singleton vector holding a Dec::Exp.  A fnal Value is not syntax
/// (its extensional, not intensional) and stands as its own case.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "cont_type", content = "value")]
pub enum Cont {
    Taken,
    Decs(Vector<Dec_>),
    Exp_(Exp_, Vector<Dec_>),
    Value(Value),
    LetVarRet(Source, Option<Id_>),
}

pub mod stack {
    use super::{Cont, Env, Vector};
    use crate::ast::{
        BinOp, Cases, Dec_, ExpField_, Exp_, Id, Id_, Inst, Mut, Pat, Pat_, PrimType, RelOp,
        Source, Type_, UnOp,
    };
    use crate::value::{ClosedFunction, PrimFunction, Value};
    use serde::{Deserialize, Serialize};

    /// Local continuation, stored in a stack frame.
    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(tag = "frame_cont_type", content = "value")]
    pub enum FrameCont {
        Let(Pat, Cont),
        Var(Id, Cont),
        UnOp(UnOp),
        BinOp1(BinOp, Exp_),
        BinOp2(Value, BinOp),
        Idx1(Exp_),
        Idx2(Value),
        Paren,
        Variant(Id_),
        Switch(Cases),
        Do,
        Assert,
        Ignore,
        Debug,
        Block,
        Decs(Vector<Dec_>),
        Tuple(Vector<Value>, Vector<Exp_>),
        Array(Mut, Vector<Value>, Vector<Exp_>),
        Object(Vector<FieldValue>, FieldContext, Vector<ExpField_>),
        Annot(Type_),
        Assign1(Exp_),
        Assign2(Value),
        Proj(usize),
        Dot(Id_),
        If(Exp_, Option<Exp_>),
        RelOp1(RelOp, Exp_),
        RelOp2(Value, RelOp),
        While1(Exp_, Exp_),
        While2(Exp_, Exp_),
        For1(Pat_, Exp_, Exp_),
        For2(Pat_, Exp_, Exp_),
        And1(Exp_),
        And2,
        Or1(Exp_),
        Or2,
        Not,
        Opt,
        DoOpt,
        Bang,
        Call1(Option<Inst>, Exp_),
        Call2(ClosedFunction, Option<Inst>),
        Call2Prim(PrimFunction, Option<Inst>),
        Call2Pointer(super::Pointer, Option<Inst>),
        Call3,
        Return,
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Frame {
        #[serde(with = "crate::serde_utils::im_rc_hashmap")]
        pub env: Env,
        pub cont: FrameCont,
        pub cont_prim_type: Option<PrimType>,
        pub source: Source,
    }
    pub type Frames = im_rc::Vector<Frame>;
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FieldValue {
        pub mut_: Mut,
        pub id: Id_,
        pub typ: Option<Type_>,
        pub val: Value,
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FieldContext {
        pub mut_: Mut,
        pub id: Id_,
        pub typ: Option<Type_>,
    }
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
    pub redex: usize,
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
    pub cont: Cont,
    pub cont_source: Source,
    /// `Some(t)` when evaluating under an annotation of type `t`.
    /// (`e : Nat8`  makes `Nat8` the `cont_prim_type` for `e`)
    pub cont_prim_type: Option<PrimType>,
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    pub env: Env,
    pub stack: Stack,
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    pub store: Store,
    pub debug_print_out: Vector<crate::value::Text>,

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
    pub redex: Option<usize>,
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
    Redex,
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
#[serde(tag = "interruption_type", content = "value")]
pub enum Interruption {
    Done(Value),
    Breakpoint(Breakpoint),
    Dangling(Pointer),
    TypeMismatch,
    NoMatchingCase,
    #[cfg(feature = "parser")]
    SyntaxError(SyntaxError),
    ValueError(ValueError),
    UnboundIdentifer(Identifier),
    UnrecognizedPrim(String),
    BlockedAwaiting,
    Limit(Limit),
    DivideByZero,
    AmbiguousOperation,
    AssertionFailure,
    IndexOutOfBounds,
    NoDoQuestBangNull,
    MisplacedReturn,
    NotYetImplemented(NYI),
    Unknown,
    Impossible,
    EvalInitError(EvalInitError),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "evaliniterror_type", content = "value")]
pub enum EvalInitError {
    NonEmptyStack,
    NonValueCont,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Deserialize)]
pub struct NYI {
    pub file: String,
    pub line: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Signal {
    Done(Value),
    Interruption(Interruption),
}

pub type Breakpoint = Span;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Error {
    ICAgentError,
    // etc
}
