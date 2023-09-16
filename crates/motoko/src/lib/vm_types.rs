use im_rc::{HashMap, Vector};
use num_traits::ToPrimitive;
use serde::{Deserialize, Serialize};

use crate::ast::{Inst, Mut};
#[cfg(feature = "parser")]
use crate::parser_types::SyntaxError as SyntaxErrorCode;
use crate::shared::FastClone;
use crate::value::{ActorId, ActorMethod, ValueError};
use crate::{
    ast::{Dec_, Exp_, Id, Id_, PrimType, Source, Span},
    value::Value_,
};
use crate::{Share, Value};

pub type Result<T = Value_, E = Interruption> = std::result::Result<T, E>;

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct SyntaxError {
    pub package_name: Option<String>,
    pub local_path: String,
    pub code: SyntaxErrorCode,
}

#[macro_export]
macro_rules! type_mismatch_ {
    () => {
        Interruption::TypeMismatch($crate::vm_types::OptionCoreSource(None))
    };
    ($file:expr, $line:expr) => {
        Interruption::TypeMismatch($crate::vm_types::OptionCoreSource(Some(
            $crate::vm_types::CoreSource {
                name: None,
                description: None,
                file: $file.to_string(),
                line: $line,
            },
        )))
    };
    ($file:expr, $line:expr, $name:expr) => {
        Interruption::TypeMismatch($crate::vm_types::OptionCoreSource(Some(
            $crate::vm_types::CoreSource {
                name: Some($name.to_string()),
                description: None,
                file: $file.to_string(),
                line: $line,
            },
        )))
    };
    ($file:expr, $line:expr, $name:expr, $description:expr) => {
        Interruption::TypeMismatch($crate::vm_types::OptionCoreSource(Some(
            $crate::vm_types::CoreSource {
                name: Some($name.to_string()),
                description: Some($description.to_string()),
                file: $file.to_string(),
                line: $line,
            },
        )))
    };
}

#[macro_export]
macro_rules! type_mismatch {
    ($file:expr, $line:expr) => {
        return Err($crate::type_mismatch_!($file, $line))
    };
}

#[macro_export]
macro_rules! nyi {
    ($line:expr) => {
        Err(Interruption::NotYetImplemented($crate::vm_types::CoreSource {
            name: None,
            description: Some("Not yet implemented in current VM logic".to_string()),
            file: file!().to_string(), // to do -- take as arg
            line: $line,
        }, None))
    };
    ($line:expr, $($mesg:tt)+) => {
        Err(Interruption::NotYetImplemented($crate::vm_types::CoreSource {
            name: None,
            description: Some("Not yet implemented in current VM logic".to_string()),
            file: file!().to_string(), // to do -- take as arg
            line: $line,
        }, Some(format!($($mesg)+)) ))
    };
}

pub mod def {
    use crate::ast::{Id, Source, Stab_, Vis_};
    use im_rc::HashMap;
    use serde::{Deserialize, Serialize};

    /// Definition database.
    ///
    /// Each entry is a context of mutually-recursive definitions.
    /// Definitions may consist of actors, actor classes, classes, modules, functions and constant values.
    /// Actors, actor classes, (object) classes and modules each introduce a new, child context for nested definitions.
    /// Except for the root context(s), every context has one parent context.
    /// Hence, contexts in the database form a forest, with one tree per root.
    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub struct Defs {
        pub map: HashMap<CtxId, Ctx>,
        pub active_ctx: CtxId,
        pub next_ctx_id: usize,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub struct CtxId(pub usize);

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub struct Ctx {
        pub parent: Option<CtxId>,
        pub fields: HashMap<Id, Field>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub struct Field {
        pub def: Def,
        pub vis: Option<Vis_>,
        pub stab: Option<Stab_>,
        pub source: Source,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub enum Def {
        Module(Module),
        Actor(Actor),
        Func(Function),
        Var(Var),
        /// We represent "static values" as expressions.
        /// (to permit variables that mention other static values.)
        StaticValue(CtxId, super::Exp_),
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub struct Var {
        pub owner: super::ScheduleChoice,
        pub name: Id,
        pub init: crate::value::Value_,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub struct Module {
        pub context: CtxId,
        pub fields: CtxId,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub struct Actor {
        pub context: CtxId,
        pub fields: CtxId,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
    pub struct Function {
        pub context: CtxId,
        pub function: crate::ast::Function,
        pub rec_value: crate::value::Value_,
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct NumericPointer(pub usize);

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct NamedPointer(pub Id);

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct Pointer {
    pub owner: ScheduleChoice,
    pub local: LocalPointer,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum LocalPointer {
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

pub fn source_from_cont(cont: &Cont) -> Source {
    use Cont::*;
    match cont {
        Taken => {
            unreachable!("no source for Taken continuation. This signals a VM bug.  Please report.")
        }
        Decs(decs) => crate::ast::source_from_decs(decs),
        Exp_(exp_, decs) => {
            if decs.is_empty() {
                exp_.1.clone()
            } else {
                exp_.1.expand(&decs.back().unwrap().1)
            }
        }
        LetVarRet(s, _) => s.clone(),
        Value_(_v) => Source::Evaluation,
    }
}

pub mod stack {
    use super::{def::CtxId, Cont, Env, Pointer, RespTarget, Vector};
    use crate::ast::{
        BinOp, Cases, Dec_, ExpField_, Exp_, Id_, Inst, Mut, Pat_, PrimType, ProjIndex, RelOp,
        Source, Type_, UnOp,
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
        BinAssign1(BinOp, Exp_),
        BinAssign2(Value_, BinOp),
        Proj(ProjIndex),
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
        Respond(RespTarget),
        Fast(FastInfo),
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FastInfo {
        // True to indicate that we should not dereference pointers implicitly; they are to be produced as values, to become the targets of a surrounding assignment.
        pub assignment_lhs: bool,
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
        pub context: CtxId,
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
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Store {
    owner: ScheduleChoice,
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    map: HashMap<LocalPointer, Value_>,
    next_pointer: usize,
}
impl Store {
    pub fn new(owner: ScheduleChoice) -> Self {
        Store {
            owner,
            map: HashMap::new(),
            next_pointer: 0,
        }
    }

    fn alloc(&mut self, value: impl Into<Value_>) -> Pointer {
        let value = value.into();
        let ptr = LocalPointer::Numeric(NumericPointer(self.next_pointer));
        self.next_pointer = self.next_pointer.checked_add(1).expect("Out of pointers");
        self.map.insert(ptr.clone(), value);
        Pointer {
            owner: self.owner.clone(),
            local: ptr,
        }
    }

    pub fn alloc_named(&mut self, name: Id, value: impl Into<Value_>) -> Pointer {
        let value = value.into();
        let ptr = LocalPointer::Named(NamedPointer(name));
        let prev = self.map.insert(ptr.clone(), value);
        assert_eq!(prev, None);
        Pointer {
            owner: self.owner.clone(),
            local: ptr,
        }
    }

    pub fn dealloc(&mut self, pointer: &LocalPointer) -> Option<Value_> {
        self.map.remove(pointer)
    }

    pub fn get(&self, pointer: &LocalPointer) -> Option<&Value_> {
        self.map.get(pointer)
    }

    pub fn get_mut(&mut self, pointer: &LocalPointer) -> Option<&mut Value_> {
        self.map.get_mut(pointer)
    }

    pub fn mutate(&mut self, pointer: Pointer, value: Value_) -> Result<(), Interruption> {
        if &pointer.owner != &self.owner {
            return Err(Interruption::NotOwner(pointer));
        };
        // it is an error to mutate an unallocated pointer.
        match self.map.get_mut(&pointer.local) {
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
        if &pointer.owner != &self.owner {
            return Err(Interruption::NotOwner(pointer));
        };
        // it is an error to mutate an unallocated pointer.
        let pointer_ref = self
            .get_mut(&pointer.local)
            .ok_or(Interruption::Dangling(pointer))?;

        match &**pointer_ref {
            Value::Array(Mut::Var, a) => {
                let i = match &*index {
                    Value::Nat(n) => n
                        .to_usize()
                        .ok_or(Interruption::ValueError(crate::value::ValueError::BigInt))?,
                    _ => type_mismatch!(file!(), line!()),
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
            _ => type_mismatch!(file!(), line!()),
        }
    }
}

/// Counts. Some ideas of how we could count and limit what the VM
/// does, to interject some "slow interactivity" into its execution.
#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct Counts {
    pub step: usize,
    pub redex: usize,
    pub send: usize,
    /*
    pub call: usize,
    pub alloc: usize,
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
    pub package: Option<String>,
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
            package: None,
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
    pub path: String,
    pub def: def::Actor,
    pub env: Env,
    pub store: Store,
    pub counts: Counts,
    pub active: Option<Activation>,
    pub awaiting: HashMap<RespId, Activation>,
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
    pub defs: def::Defs,
    pub schedule_choice: ScheduleChoice,
    pub agent: Agent,
    pub actors: Actors,
    pub next_resp_id: usize,
    pub debug_print_out: Vector<DebugPrintLine>,
    pub module_files: ModuleFiles,
}

/// The current/last/next schedule choice, depending on context.
#[derive(Clone, Debug, Hash, Serialize, Deserialize, PartialEq, Eq)]
pub enum ScheduleChoice {
    Agent,
    Actor(ActorId),
}

/// The Actors in a Core system.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Actors {
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    pub map: HashMap<ActorId, Actor>,
}

/// The ModuleFiles in a Core system (a virtual filesystem).
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ModuleFiles {
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    pub map: HashMap<ModulePath, ModuleFileState>,
    pub import_stack: Vector<ModulePath>,
}

/// A Path should adhere to certain rules, not enforced by this type.
#[derive(Clone, Debug, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct ModulePath {
    pub package_name: Option<String>,
    pub local_path: String,
}

/// The file representation for a module.
///
/// This representation is a status that ensures that the module was
/// imported and defined successfully.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ModuleFile {
    pub file_content: String,
    /// Each file is a root in the definition database forest.
    pub context: def::CtxId,
    /// Within the file's root context, the module is defined (after all imports).
    pub module: def::CtxId,
    // /// Module definition.
    // pub def: def::Module,
}

impl ModuleFile {
    pub fn def(&self) -> def::Module {
        def::Module {
            context: self.context.clone(),
            fields: self.module.clone(),
        }
    }
    pub fn value(&self) -> Value {
        Value::Module(self.def())
    }
}

/// The initialization-phase file representation for a module, before
/// being imported.
///
/// The field file_content is the original string.
/// The other fields come from parsing it and matching the AST.
///
/// This representation is a status which ensures that once imported, the
/// module will have the correct AST form of a module.  But, it may
/// define fields incorrectly or import incorrectly, resulting in
/// still-latent Interruptions.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ModuleFileInit {
    pub file_content: String,
    pub outer_decs: Vector<Dec_>,
    pub id: Option<Id_>,
    pub fields: crate::ast::DecFields,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ModuleFileState {
    Init(ModuleFileInit),
    Defined(ModuleFile),
}

/// A line of output emitted by prim "debugPrint".
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DebugPrintLine {
    /// Who emitted this output?
    pub schedule_choice: ScheduleChoice,
    /// The emitted text.
    pub text: crate::value::Text,
}

/// Exclusive write access to the "active" components of the VM.
pub trait Active: ActiveBorrow {
    fn defs<'a>(&'a mut self) -> &'a mut def::Defs;
    fn module_files<'a>(&'a mut self) -> &'a mut ModuleFiles;
    fn ctx_id<'a>(&'a mut self) -> &'a mut def::CtxId;
    //fn schedule_choice<'a>(&'a self) -> &'a ScheduleChoice;
    fn cont<'a>(&'a mut self) -> &'a mut Cont;
    fn cont_source<'a>(&'a mut self) -> &'a mut Source;
    fn cont_prim_type<'a>(&'a mut self) -> &'a mut Option<PrimType>;
    fn env<'a>(&'a mut self) -> &'a mut Env;
    fn stack<'a>(&'a mut self) -> &'a mut Stack;
    fn store<'a>(&'a mut self) -> &'a mut Store;
    fn package<'a>(&'a mut self) -> &'a mut Option<String>;
    fn debug_print_out<'a>(&'a mut self) -> &'a mut Vector<DebugPrintLine>;
    fn counts<'a>(&'a mut self) -> &'a mut Counts;

    fn alloc(&mut self, value: impl Into<Value_>) -> Pointer {
        self.store().alloc(value)
    }

    fn create(
        &mut self,
        path: String,
        id: ActorId,
        actor: def::Actor,
    ) -> Result<Value_, Interruption>;
    fn upgrade(
        &mut self,
        path: String,
        id: ActorId,
        actor: def::Actor,
    ) -> Result<Value_, Interruption>;

    fn create_module(
        &mut self,
        path: ModulePath,
        id: Option<Id>,
        module: def::Module,
    ) -> Result<Value_, Interruption>;
    fn upgrade_module(
        &mut self,
        path: ModulePath,
        id: Option<Id>,
        module: def::Module,
    ) -> Result<Value_, Interruption>;
}

/// Non-exclusive read access to the "active" components of the VM.
pub trait ActiveBorrow {
    fn defs<'a>(&'a self) -> &'a def::Defs;
    fn ctx_id<'a>(&'a self) -> &'a def::CtxId;
    fn schedule_choice<'a>(&'a self) -> &'a ScheduleChoice;
    fn cont<'a>(&'a self) -> &'a Cont;
    fn cont_source<'a>(&'a self) -> &'a Source;
    fn cont_prim_type<'a>(&'a self) -> &'a Option<PrimType>;
    fn env<'a>(&'a self) -> &'a Env;
    fn stack<'a>(&'a self) -> &'a Stack;
    fn store<'a>(&'a self) -> &'a Store;
    fn debug_print_out<'a>(&'a self) -> &'a Vector<DebugPrintLine>;
    fn counts<'a>(&'a self) -> &'a Counts;
    fn deref(&self, pointer: &Pointer) -> Result<Value_, Interruption> {
        if &pointer.owner != self.schedule_choice() {
            return Err(Interruption::NotOwner(pointer.clone()));
        };
        self.store()
            .get(&pointer.local)
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

// Some ideas of how we could count and limit what the VM does,
// to interject some "slow interactivity" into its execution.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Limits {
    pub breakpoints: Vec<Breakpoint>,

    pub step: Option<usize>,
    pub redex: Option<usize>,
    pub send: Option<usize>,
    /*
    pub stack: Option<usize>,
    pub call: Option<usize>,
    pub alloc: Option<usize>,
     */
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum Limit {
    Step,
    Redex,
    Send,
}

// to do Q -- how much detail to provide about stepping?
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Step {
    // - new context ID?
    // - log of lexical regions of steps?
    // - log of kind of steps (expression kinds)?
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Response {
    pub target: RespTarget,
    /* to do -- RespId */
    pub value: Value_,
}

pub type RespTarget = ScheduleChoice;

#[derive(Clone, Debug, Serialize, Deserialize, Eq)]
pub struct OptionCoreSource(pub Option<CoreSource>);

// interruptions are events that prevent steppping from progressing.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "interruption_type", content = "value")]
pub enum Interruption {
    Done(Value_),
    Send(ActorMethod, Option<Inst>, Value_),
    Response(Response),
    Breakpoint(Breakpoint),
    Dangling(Pointer),
    NotOwner(Pointer),
    ImportCycle(Vector<ModulePath>),
    ModuleFileNotFound(ModulePath),
    ModuleNotStatic(Source, Option<String>),
    ModuleFieldNotPublic(Id),
    TypeMismatch(OptionCoreSource),
    NonLiteralInit(Source),
    NoMatchingCase,
    #[cfg(feature = "parser")]
    SyntaxError(SyntaxError),
    ValueError(ValueError),
    EvalInitError(EvalInitError),
    UnboundIdentifer(Id),
    MissingActorDefinition,
    NotAnActorDefinition,
    NotAModuleDefinition,
    MissingModuleDefinition,
    AmbiguousActorId(ActorId),
    ActorIdNotFound(ActorId),
    ActorFieldNotFound(ActorId, Id),
    ActorFieldNotPublic(ActorId, Id),
    AmbiguousIdentifer(Id, Source, Source),
    UnrecognizedPrim(String),
    Limit(Limit),
    DivideByZero,
    AmbiguousOperation,
    AssertionFailure,
    IndexOutOfBounds,
    NoDoQuestBangNull,
    MisplacedReturn,
    NotYetImplemented(CoreSource, Option<String>),
    Unknown,
    Impossible,
    Other(String),
}

impl From<()> for Interruption {
    // try to avoid this conversion, except in temp code.
    fn from(_x: ()) -> Interruption {
        Interruption::Unknown
    }
}

impl Interruption {
    pub fn is_recoverable(&self) -> bool {
        match self {
            Interruption::Send(..) => true,
            Interruption::Response(..) => true,
            Interruption::Breakpoint(..) => true,
            Interruption::Limit(..) => true,
            _ => false,
        }
    }
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

impl PartialEq for OptionCoreSource {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (None, _) => true,
            (_, None) => true,
            (Some(x), Some(y)) => x == y,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "evaliniterror_type", content = "value")]
pub enum EvalInitError {
    NonEmptyStack,
    NonValueCont,
    AgentNotScheduled,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Deserialize)]
pub struct CoreSource {
    pub name: Option<String>,
    pub description: Option<String>,
    pub file: String,
    pub line: u32,
}

pub type Breakpoint = Span;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Error {
    ICAgentError,
    // etc
}
