use im_rc::{HashMap, Vector};
use num_traits::ToPrimitive;
use serde::{Deserialize, Serialize};

use crate::ast::Mut;
#[cfg(feature = "parser")]
use crate::parser_types::SyntaxError;
use crate::shared::FastClone;
use crate::value::ValueError;
use crate::{
    ast::{Dec_, Exp_, Id, Id_, PrimType, Source, Span},
    value::Value_,
};
use crate::{Share, Value};

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
        Actor(Actor),
        Function(Function),
        Value(crate::value::Value_),
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Module {
        parent: CtxId,
        fields: CtxId,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Actor {
        context: CtxId,
        fields: CtxId,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Function {
        context: CtxId,
        function: crate::ast::Function,
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct NumericPointer(pub usize);

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct NamedPointer(pub Id);

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum Pointer {
    Numeric(NumericPointer),
    Named(NamedPointer),
}

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
pub type Env = HashMap<Id, Value_>;

/// Store holds mutable variables, mutable arrays and mutable records.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Store {
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    map: HashMap<Pointer, Value_>,
    next_pointer: usize,
}
impl Store {
    pub fn new() -> Self {
        Self::default()
    }

    fn alloc(&mut self, value: impl Into<Value_>) -> Pointer {
        let value = value.into();
        let ptr = Pointer::Numeric(NumericPointer(self.next_pointer));
        self.next_pointer = self.next_pointer.checked_add(1).expect("Out of pointers");
        self.map.insert(ptr.clone(), value);
        ptr
    }

    pub fn dealloc(&mut self, pointer: &Pointer) -> Option<Value_> {
        self.map.remove(pointer)
    }

    pub fn get(&self, pointer: &Pointer) -> Option<&Value_> {
        self.map.get(pointer)
    }

    pub fn get_mut(&mut self, pointer: &Pointer) -> Option<&mut Value_> {
        self.map.get_mut(pointer)
    }

    pub fn mutate(&mut self, pointer: Pointer, value: Value_) -> Result<(), Interruption> {
        // it is an error to mutate an unallocated pointer.
        match self.map.get_mut(&pointer) {
            None => return Err(Interruption::Dangling(pointer)),
            Some(v) => *v = value,
        };
        Ok(())
    }

    pub fn mutate_index(
        &mut self,
        pointer: Pointer,
        index: Value_,
        value: Value_,
    ) -> Result<(), Interruption> {
        // it is an error to mutate an unallocated pointer.
        let pointer_ref = self
            .get_mut(&pointer)
            .ok_or(Interruption::Dangling(pointer))?;

        match &**pointer_ref {
            Value::Array(Mut::Var, a) => {
                let i = match &*index {
                    Value::Nat(n) => n
                        .to_usize()
                        .ok_or(Interruption::ValueError(crate::value::ValueError::BigInt))?,
                    _ => Err(Interruption::TypeMismatch)?,
                };
                let mut a = a.clone();
                if i < a.len() {
                    a.set(i, value);
                    *pointer_ref = Value::Array(Mut::Var, a).share();
                    Ok(())
                } else {
                    Err(Interruption::IndexOutOfBounds)
                }
            }
            Value::Dynamic(d) => {
                d.fast_clone().dynamic_mut().set_index(self, index, value)?;
                Ok(())
            }
            _ => Err(Interruption::TypeMismatch),
        }
    }
}

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

/// A Motoko Agent interacts with zero or more Motoko actors.
///
/// The cost of copying this state is O(1).
///
/// An Agent removes some aspects of an Actor, but can still execute
/// Motoko code.  Unlike an actor, an agent lacks a public API with
/// entry points. Hence, it has no way to be activated, and it awaits
/// at most one response at a time.  Actors are more complex, in that
/// they have a public API, and can be awaiting many responses as they
/// service one.
///
/// The Agent captures several distinct use cases:
///  - ingress message queue (as a Motoko program that sends messages to actors).
///  - single-Actor unit test scripts.
///  - multi-Actor integration test scripts.
///
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Agent {
    pub store: Store,
    pub counts: Counts,
    pub active: Activation,
}

/// Components for a single activated thread of control.
///
/// Omits shared components like a store, counts, etc.
///
/// See also: `Active` trait.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Activation {
    pub cont: Cont,
    pub cont_source: Source,
    /// `Some(t)` when evaluating under an annotation of type `t`.
    /// (`e : Nat8`  makes `Nat8` the `cont_prim_type` for `e`)
    pub cont_prim_type: Option<PrimType>,
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    pub env: Env,
    pub stack: Stack,
}

impl Activation {
    pub fn new() -> Self {
        let cont_prim_type: Option<PrimType> = None;
        Activation {
            stack: Vector::new(),
            env: HashMap::new(),
            cont: Cont::Value_(Value::Unit.share()),
            cont_source: Source::CoreInit, // todo
            cont_prim_type,
        }
    }
}

/// A Motoko Actor.
///
/// The cost of copying this state is O(1).
///
/// An actors has a public API, and can be awaiting many responses as
/// it services one.  In these ways, an Actor is more complex than the
/// Motoko Agent that activates it.
///
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Actor {
    pub store: Store,
    pub counts: Counts,
    pub active: Option<Activation>,
    pub awaiting: HashMap<RespId, Activation>,
    // to do -- private/public fields/methods.
}

/// Unique response Id, for coordinating message responses from the
/// actor replying to the actor or agent receiving the reply.
/// Generally, an actor is awaiting multiple replies at once, from
/// other actors.
#[derive(Clone, Hash, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RespId(pub usize);

/// A Core encompasses VM system state, including its actors.
///
/// The cost of copying this state is O(1).
///
/// A VM Core permits an Agent to interact with zero or more Actors.
///
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Core {
    pub schedule_choice: ScheduleChoice,
    pub agent: Agent,
    pub actors: Actors,
    pub next_resp_id: usize,
    pub debug_print_out: Vector<DebugPrintLine>,
}

/// The current/last/next schedule choice, depending on context.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ScheduleChoice {
    Agent,
    Actor(Id),
}

/// The Actors in a Core system.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Actors {
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    map: HashMap<Id, Actor>,
}

/// A line of output emitted by prim "debugPrint".
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DebugPrintLine {
    /// Who emitted this output?
    schedule_choice: ScheduleChoice,
    /// The emitted text.
    text: crate::value::Text,
}

/// Exclusive write access to the "active" components of the VM.
pub trait Active: ActiveBorrow {
    fn cont<'a>(&'a mut self) -> &'a mut Cont;
    fn cont_source<'a>(&'a mut self) -> &'a mut Source;
    fn cont_prim_type<'a>(&'a mut self) -> &'a mut Option<PrimType>;
    fn env<'a>(&'a mut self) -> &'a mut Env;
    fn stack<'a>(&'a mut self) -> &'a mut Stack;
    fn store<'a>(&'a mut self) -> &'a mut Store;
    fn debug_print_out<'a>(&'a mut self) -> &'a mut Vector<crate::value::Text>;
    fn counts<'a>(&'a mut self) -> &'a mut Counts;

    fn alloc(&mut self, value: impl Into<Value_>) -> Pointer {
        self.store().alloc(value)
    }

    fn create(&mut self, _name: Option<Id>, _actor: Actor) -> Result<Value_, Interruption> {
        todo!()
    }
}

/// Non-exclusive read access to the "active" components of the VM.
pub trait ActiveBorrow {
    fn cont<'a>(&'a self) -> &'a Cont;
    fn cont_source<'a>(&'a self) -> &'a Source;
    fn cont_prim_type<'a>(&'a self) -> &'a Option<PrimType>;
    fn env<'a>(&'a self) -> &'a Env;
    fn stack<'a>(&'a self) -> &'a Stack;
    fn store<'a>(&'a self) -> &'a Store;
    fn debug_print_out<'a>(&'a self) -> &'a Vector<crate::value::Text>;
    fn counts<'a>(&'a self) -> &'a Counts;
    fn deref(&self, pointer: &Pointer) -> Result<Value_, Interruption> {
        self.store()
            .get(pointer)
            .ok_or_else(|| Interruption::Dangling(pointer.clone()))
            .map(|v| v.fast_clone())
    }
    fn deref_value(&mut self, value: impl Into<Value_>) -> Result<Value_, Interruption> {
        let value = value.into();
        match &*value {
            crate::value::Value::Pointer(p) => self.deref(p),
            _ => Ok(value),
        }
    }
}

impl Active for Agent {
    fn cont<'a>(&'a mut self) -> &'a mut Cont {
        &mut self.active.cont
    }
    fn cont_source<'a>(&'a mut self) -> &'a mut Source {
        &mut self.active.cont_source
    }
    fn cont_prim_type<'a>(&'a mut self) -> &'a mut Option<PrimType> {
        &mut self.active.cont_prim_type
    }
    fn env<'a>(&'a mut self) -> &'a mut Env {
        &mut self.active.env
    }
    fn stack<'a>(&'a mut self) -> &'a mut Stack {
        &mut self.active.stack
    }
    fn store<'a>(&'a mut self) -> &'a mut Store {
        &mut self.store
    }
    fn debug_print_out<'a>(&'a mut self) -> &'a mut Vector<crate::value::Text> {
        todo!()
    }
    fn counts<'a>(&'a mut self) -> &'a mut Counts {
        &mut self.counts
    }
}

impl ActiveBorrow for Agent {
    fn cont<'a>(&'a self) -> &'a Cont {
        &self.active.cont
    }
    fn cont_source<'a>(&'a self) -> &'a Source {
        &self.active.cont_source
    }
    fn cont_prim_type<'a>(&'a self) -> &'a Option<PrimType> {
        &self.active.cont_prim_type
    }
    fn env<'a>(&'a self) -> &'a Env {
        &self.active.env
    }
    fn stack<'a>(&'a self) -> &'a Stack {
        &self.active.stack
    }
    fn store<'a>(&'a self) -> &'a Store {
        &self.store
    }
    fn debug_print_out<'a>(&'a self) -> &'a Vector<crate::value::Text> {
        todo!()
    }
    fn counts<'a>(&'a self) -> &'a Counts {
        &self.counts
    }
}

impl Active for Actor {
    fn cont<'a>(&'a mut self) -> &'a mut Cont {
        &mut self.active.as_mut().unwrap().cont
    }
    fn cont_source<'a>(&'a mut self) -> &'a mut Source {
        &mut self.active.as_mut().unwrap().cont_source
    }
    fn cont_prim_type<'a>(&'a mut self) -> &'a mut Option<PrimType> {
        &mut self.active.as_mut().unwrap().cont_prim_type
    }
    fn env<'a>(&'a mut self) -> &'a mut Env {
        &mut self.active.as_mut().unwrap().env
    }
    fn stack<'a>(&'a mut self) -> &'a mut Stack {
        &mut self.active.as_mut().unwrap().stack
    }
    fn store<'a>(&'a mut self) -> &'a mut Store {
        &mut self.store
    }
    fn debug_print_out<'a>(&'a mut self) -> &'a mut Vector<crate::value::Text> {
        todo!()
    }
    fn counts<'a>(&'a mut self) -> &'a mut Counts {
        &mut self.counts
    }
}

impl ActiveBorrow for Actor {
    fn cont<'a>(&'a self) -> &'a Cont {
        &self.active.as_ref().unwrap().cont
    }
    fn cont_source<'a>(&'a self) -> &'a Source {
        &self.active.as_ref().unwrap().cont_source
    }
    fn cont_prim_type<'a>(&'a self) -> &'a Option<PrimType> {
        &self.active.as_ref().unwrap().cont_prim_type
    }
    fn env<'a>(&'a self) -> &'a Env {
        &self.active.as_ref().unwrap().env
    }
    fn stack<'a>(&'a self) -> &'a Stack {
        &self.active.as_ref().unwrap().stack
    }
    fn store<'a>(&'a self) -> &'a Store {
        &self.store
    }
    fn debug_print_out<'a>(&'a self) -> &'a Vector<crate::value::Text> {
        todo!()
    }
    fn counts<'a>(&'a self) -> &'a Counts {
        &self.counts
    }
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
    UnboundIdentifer(Id),
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
