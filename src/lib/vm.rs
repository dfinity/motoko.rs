use crate::ast::Prog;
use crate::vm_types::{Core, Local};

pub fn core_init(_prog: Prog) -> Core {
    unimplemented!()
}

// to do Q -- how much detail to provide about stepping?
pub struct Step {
    // - new context ID?
    // - lexical region of step?
    // - kind of step (expression kind)?
}

// interruptions are events that prevent steppping from progressing.
pub enum Interruption {
    TypeMismatch,
    BlockedAwaiting,
}

// To advance the core Motoko state by a single step.
pub fn core_step(_core: &mut Core) -> Result<Step, Interruption> {
    unimplemented!()
}

// For core Motoko state initializing local VM state.
pub fn local_init(_core: Core) -> Local {
    unimplemented!()
}

// Some ideas of how we could count and limit what the VM does,
// to interject some "slow interactivity" into its execution.
pub struct Limits {
    pub step: Option<usize>,
    pub stack: Option<usize>,
    pub call: Option<usize>,
    pub alloc: Option<usize>,
    pub send: Option<usize>,
}

pub enum Limit {
    Step,
    Stack,
    Call,
    Alloc,
    Send,
}

pub enum Signal {
    ReachedLimit(Limit),
}

pub enum Error {
    ICAgentError,
    // etc
}

// use ic-agent to do function calls.
pub fn local_run(_local: &mut Local, _limits: Limits) -> Result<Signal, Error> {
    unimplemented!()
}
