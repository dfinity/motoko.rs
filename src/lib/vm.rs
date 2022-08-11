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
        store: HashMap::new(),
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
    ParseError,
    UnboundIdentifer(Identifier),
    BlockedAwaiting,
    Limit(Limit),
    DivideByZero,
    Done(Value),
}

/*
fn pattern(env: Env, pat: &Pat, val: Value) -> Result<Env, MatchError> {

}
*/

fn binop(binop: BinOp, v1: Value, v2: Value) -> Result<Value, Interruption> {
    use BinOp::*;
    use Value::*;
    match binop {
        Add => match (v1, v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 + n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 + i2)),
            /*
                       (Int(i1), Nat(n2)) => Ok(Int(i1 + n2)),
                       (Nat(n1), Int(i2)) => Ok(Int(n1 + i2)),
            */
            _ => todo!(),
        },
        _ => todo!(),
    }
}

fn exp_step(core: &mut Core, exp: Exp, limits: &Limits) -> Result<Step, Interruption> {
    use Exp::*;
    match exp {
        Literal(l) => {
            core.cont = Cont::Value(Value::from_literal(l).map_err(|_| Interruption::ParseError)?);
            Ok(Step {})
        }
        Var(x) => match core.env.get(&x) {
            None => Err(Interruption::UnboundIdentifer(x.clone())),
            Some(v) => {
                core.cont = Cont::Value(v.clone());
                Ok(Step {})
            }
        },
        Bin(e1, binop, e2) => {
            core.stack.push_back(Frame {
                env: core.env.clone(),
                cont: FrameCont::BinOp1(binop, e2),
            });
            core.cont = Cont::Exp_(e1);
            Ok(Step {})
        }
        _ => todo!(),
    }
}

// To advance the core Motoko state by a single step.
pub fn core_step(core: &mut Core, limits: &Limits) -> Result<Step, Interruption> {
    println!("{:?}", core);
    let cont = std::mem::replace(&mut core.cont, Cont::Taken);
    match cont {
        Cont::Exp_(e) => {
            core.cont = Cont::Taken;
            exp_step(core, *e, limits)
        }
        Cont::Value(v) => {
            if core.stack.len() == 0 {
                Err(Interruption::Done(v.clone()))
            } else {
                use FrameCont::*;
                match core.stack.pop_back().unwrap().cont {
                    BinOp1(binop, e2) => {
                        core.stack.push_back(Frame {
                            env: core.env.clone(),
                            cont: BinOp2(v, binop),
                        });
                        core.cont = Cont::Exp_(e2);
                        Ok(Step {})
                    }
                    BinOp2(v1, bop) => {
                        core.cont = Cont::Value(binop(bop, v1, v)?);
                        Ok(Step {})
                    }
                    _ => todo!(),
                }
            }
        }
        Cont::Decs(mut decs) => {
            if decs.len() == 0 {
                core.cont = Cont::Value(Value::Unit);
                Ok(Step {})
            } else {
                match decs.pop_front().unwrap() {
                    Dec::Exp(e) => {
                        core.cont = Cont::Exp_(Box::new(e));
                        Ok(Step {})
                    }
                    Dec::Let(p, e) => {
                        todo!()
                    }
                    _ => todo!(),
                }
            }
        }
        _ => unimplemented!(),
    }
}

// For core Motoko state initializing local VM state.
pub fn local_init(active: Core) -> Local {
    Local { active }
}

#[derive(Debug)]
pub enum Signal {
    Done(Value),
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
            Err(Interruption::Done(v)) => return Ok(Signal::Done(v)),
            Err(Interruption::Limit(limit)) => return Ok(Signal::ReachedLimit(limit)),
            _ => unimplemented!(),
        }
    }
}
