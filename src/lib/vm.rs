use crate::ast::{BinOp, Dec, Exp, Id as Identifier, Prog};
use crate::value::Value;
use crate::vm_types::{
    stack::{Frame, FrameCont},
    Cont, Core, Counts, Local,
};
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
        cont: Cont::Decs(prog.vec.into()),
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
    UnboundIdentifer(Identifier),
    BlockedAwaiting,
    Limit(Limit),
    DivideByZero,
}

fn binop(binop: BinOp, v1: Value, v2: Value) -> Result<Value, Interruption> {
    use BinOp::*;
    use Value::*;
    match binop {
        Add => match (v1, v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 + n2)),
            /*
                        (Int(i1), Int(i2)) => Ok(Int(i1 + i2)),
                        (Int(i1), Nat(n2)) => Ok(Int(i1 + n2)),
                        (Nat(n1), Int(i2)) => Ok(Int(n1 + i2)),
            */
            _ => unimplemented!(),
        },
    }
}

fn exp_step(core: &mut Core, exp: Exp, limits: &Limits) -> Result<Step, Interruption> {
    use Exp::*;
    match exp {
        Var(x) => {
            core.cont = Cont::Value(
                core.env
                    .get(&x)
                    .or_else(Err(Interruption::UnboundIdentifer(x.clone()))),
            );
            Ok(Step {})
        }
        Bin(e1, binop, e2) => {
            core.stack.push(FrameCont::BinOp1(binop, e2));
            core.cont = Cont::Exp(e1);
            Ok(Step {})
        }
    }
}

// To advance the core Motoko state by a single step.
pub fn core_step(core: &mut Core, limits: &Limits) -> Result<Step, Interruption> {
    println!("{:?}", core);
    match core.cont {
        Cont::Value(v) => {
            // to do -- pop stack and match the frame to decide what to do.
            unimplemented!()
        }
        Cont::Decs(decs) => {}

        _ => unimplemented!(),
    }
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
