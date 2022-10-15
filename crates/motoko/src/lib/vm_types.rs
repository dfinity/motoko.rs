use im_rc::{HashMap, Vector};
use serde::{Deserialize, Serialize};

#[cfg(feature = "parser")]
use crate::parser_types::SyntaxError;
use crate::value::ValueError;
use crate::{
    ast::{Dec_, Exp_, Id as Identifier, Id_, PrimType, Source, Span},
    value::Value_,
};

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
        Value(crate::value::Value_),
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
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct Pointer(pub usize);

impl<'a> crate::shared::FastClone<Pointer> for &'a Pointer {
    fn fast_clone(self) -> Pointer {
        self.clone()
    }
}

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
    // Value(Value_),
    Value_(Value_),
    LetVarRet(Source, Option<Id_>),
}

pub mod stack {
    use super::{Cont, Env, Pointer, Vector};
    use crate::ast::{
        BinOp, Cases, Dec_, ExpField_, Exp_, Id_, Inst, Mut, Pat_, PrimType, RelOp, Source, Type_,
        UnOp,
    };
    use crate::value::{Value, Value_};
    use serde::{Deserialize, Serialize};

    /// Local continuation, stored in a stack frame.
    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(tag = "frame_cont_type", content = "value")]
    pub enum FrameCont {
        Let(Pat_, Cont),
        Var(Id_, Cont),
        UnOp(UnOp),
        BinOp1(BinOp, Exp_),
        BinOp2(Value_, BinOp),
        Idx1(Exp_),
        Idx2(Value_),
        Paren,
        Variant(Id_),
        Switch(Cases),
        Do,
        Assert,
        Ignore,
        Debug,
        Block,
        Decs(Vector<Dec_>),
        Tuple(Vector<Value_>, Vector<Exp_>),
        Array(Mut, Vector<Value_>, Vector<Exp_>),
        Object(Vector<FieldValue>, FieldContext, Vector<ExpField_>),
        Annot(Type_),
        Assign1(Exp_),
        Assign2(Value_),
        Proj(usize),
        Dot(Id_),
        If(Exp_, Option<Exp_>),
        RelOp1(RelOp, Exp_),
        RelOp2(Value_, RelOp),
        While1(Exp_, Exp_),
        While2(Exp_, Exp_),
        // For1 is waiting for iterator expression to become a value.
        For1(Pat_, Exp_),
        // For2 is waiting for iterator .next() to evaluate.
        For2(Pat_, Value_, Exp_),
        // For3 is waiting for for-loop body to evaluate.
        For3(Pat_, Value_, Exp_),
        // For-loop iterator is an opaque object in store.
        ForOpaqueIter(Pat_, Pointer, Exp_),
        And1(Exp_),
        And2,
        Or1(Exp_),
        Or2,
        Not,
        Opt,
        DoOpt,
        Bang,
        Call1(Option<Inst>, Exp_),
        Call2(Value_, Option<Inst>), // `Value_` necessary to prevent `Function` / `PrimFunction` clone
        Call3,
        Return,
    }
    impl FrameCont {
        pub fn formal(&self) -> Option<FormalFrameCont> {
            use FormalFrameCont::*;
            Some(match self {
                FrameCont::Call1(..) => Call1,
                FrameCont::Call2(v, ..) => match &**v {
                    Value::Function(..) => Call2,
                    Value::PrimFunction(..) => Call2Prim,
                    Value::Dynamic(..) => Call2Dyn,
                    _ => None?,
                },
                _ => todo!(),
            })
        }
    }
    // Quick mockup of `FormalFrameCont` (to be generated by Ott)
    pub enum FormalFrameCont {
        Call1,
        Call2,
        Call2Prim,
        Call2Dyn,
        Call3,
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
        pub val: Value_,
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
pub type Env = HashMap<Identifier, Value_>;

/// Store holds mutable variables, mutable arrays and mutable
/// records.
pub type Store = HashMap<Pointer, Value_>;

/// Counts. Some ideas of how we could count and limit what the VM
/// does, to interject some "slow interactivity" into its execution.
#[derive(Clone, Debug, Serialize, Deserialize, Default)]
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
    pub next_pointer: usize,
    pub debug_print_out: Vector<crate::value::Text>,
    pub counts: Counts,
}

/// An active evaluation context provides mutable access to several components.
pub trait Active {
    fn cont<'a>(&'a mut self) -> &'a mut Cont;
    fn cont_source<'a>(&'a mut self) -> &'a mut Source;
    fn cont_prim_type<'a>(&'a mut self) -> &'a mut Option<PrimType>;
    fn env<'a>(&'a mut self) -> &'a mut Env;
    fn stack<'a>(&'a mut self) -> &'a mut Stack;
    fn store<'a>(&'a mut self) -> &'a mut Store;
    fn next_pointer<'a>(&'a mut self) -> &'a mut usize;
    fn debug_print_out<'a>(&'a mut self) -> &'a mut Vector<crate::value::Text>;
    fn counts<'a>(&'a mut self) -> &'a mut Counts;

    fn alloc(&mut self, value: impl Into<Value_>) -> Pointer;
}

impl Active for Core {
    fn cont<'a>(&'a mut self) -> &'a mut Cont {
        &mut self.cont
    }
    fn cont_source<'a>(&'a mut self) -> &'a mut Source {
        &mut self.cont_source
    }
    fn cont_prim_type<'a>(&'a mut self) -> &'a mut Option<PrimType> {
        &mut self.cont_prim_type
    }
    fn env<'a>(&'a mut self) -> &'a mut Env {
        &mut self.env
    }
    fn stack<'a>(&'a mut self) -> &'a mut Stack {
        &mut self.stack
    }
    fn store<'a>(&'a mut self) -> &'a mut Store {
        &mut self.store
    }
    fn next_pointer<'a>(&'a mut self) -> &'a mut usize {
        &mut self.next_pointer
    }
    fn debug_print_out<'a>(&'a mut self) -> &'a mut Vector<crate::value::Text> {
        &mut self.debug_print_out
    }
    fn counts<'a>(&'a mut self) -> &'a mut Counts {
        &mut self.counts
    }
    fn alloc(&mut self, value: impl Into<Value_>) -> Pointer {
        let value = value.into();
        let ptr = Pointer(self.next_pointer);
        self.next_pointer = self.next_pointer.checked_add(1).expect("Out of pointers");
        self.store.insert(ptr.clone(), value);
        ptr
    }
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
    Done(Value_),
    Breakpoint(Breakpoint),
    Dangling(Pointer),
    TypeMismatch,
    NoMatchingCase,
    #[cfg(feature = "parser")]
    SyntaxError(SyntaxError),
    ValueError(ValueError),
    EvalInitError(EvalInitError),
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
    Other(String),
}

impl From<SyntaxError> for Interruption {
    fn from(err: SyntaxError) -> Self {
        Interruption::SyntaxError(err)
    }
}

impl From<ValueError> for Interruption {
    fn from(err: ValueError) -> Self {
        Interruption::ValueError(err)
    }
}

impl From<EvalInitError> for Interruption {
    fn from(err: EvalInitError) -> Self {
        Interruption::EvalInitError(err)
    }
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
    Done(Value_),
    Interruption(Interruption),
}

pub type Breakpoint = Span;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Error {
    ICAgentError,
    // etc
}
