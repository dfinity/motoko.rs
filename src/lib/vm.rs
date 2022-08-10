use crate::ast::Prog;
use crate::value::Value;
use crate::vm_types::{Core, Counts, Local};
use im_rc::{HashMap, Vector};

// Some ideas of how we could count and limit what the VM does,
// to interject some "slow interactivity" into its execution.
#[derive(Clone, Debug)]
pub struct Limits {
    pub step: Option<usize>,
    pub stack: Option<usize>,
    pub call: Option<usize>,
    pub alloc: Option<usize>,
    pub send: Option<usize>,
}

#[derive(Clone, Debug)]
pub enum Limit {
    Step,
    Stack,
    Call,
    Alloc,
    Send,
}

pub fn core_init(prog: Prog) -> Core {
    Core {
        //store: HashMap::new(),
        stack: Vector::new(),
        env: HashMap::new(),
        cont: prog.vec.into(),
        counts: Counts {
            step: 0,
            stack: 0,
            call: 0,
            alloc: 0,
            send: 0,
        },
    }
}

// to do Q -- how much detail to provide about stepping?
pub struct Step {
    // - new context ID?
    // - log of lexical regions of steps?
    // - log of kind of steps (expression kinds)?
}

// interruptions are events that prevent steppping from progressing.
pub enum Interruption {
    TypeMismatch,
    BlockedAwaiting,
    Limit(Limit),
}

// To advance the core Motoko state by a single step.
pub fn core_step(core: &mut Core, limits: &Limits) -> Result<Step, Interruption> {
    println!("{:?}", core);
    unimplemented!()
}

// For core Motoko state initializing local VM state.
pub fn local_init(active: Core) -> Local {
    Local { active }
}

#[derive(Debug)]
pub enum Signal {
    Ok(Value),
    ReachedLimit(Limit),
}

#[derive(Clone, Debug)]
pub enum Error {
    ICAgentError,
    // etc
}

// use ic-agent to do function calls.
pub fn local_run(local: &mut Local, limits: &Limits) -> Result<Signal, Error> {
    loop {
        match core_step(&mut local.active, limits) {
            Ok(_step) => { /* to do */ }
            Err(Interruption::Limit(limit)) => return Ok(Signal::ReachedLimit(limit)),
            _ => unimplemented!(),
        }
    }
}
