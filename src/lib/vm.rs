use crate::ast::{BinOp, Dec, Exp, Id as Identifier, Id_, Pat, Prog};
use crate::value::Value;
use crate::vm_types::{
    stack::{Frame, FrameCont},
    Canister, Cont, Core, Counts, Error, Interruption, Limits, Local, Signal, Step,
};
use im_rc::{HashMap, Vector};
use serde::{Deserialize, Serialize};

impl Limits {
    fn none() -> Limits {
        Limits {
            step: None,
            stack: None,
            call: None,
            alloc: None,
            send: None,
        }
    }
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
        Sub => match (v1, v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 - n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 - i2)),
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
        Paren(e) => {
            core.stack.push_back(Frame {
                env: core.env.clone(),
                cont: FrameCont::Paren,
            });
            core.cont = Cont::Exp_(e);
            Ok(Step {})
        }
        Variant(id, None) => {
            core.cont = Cont::Value(Value::Variant(id, None));
            Ok(Step {})
        }
        Variant(id, Some(e)) => {
            core.stack.push_back(Frame {
                env: core.env.clone(),
                cont: FrameCont::Variant(id),
            });
            core.cont = Cont::Exp_(e);
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
                    Let(Pat::Var(x), cont) => {
                        core.env.insert(x, v);
                        core.cont = cont;
                        Ok(Step {})
                    }
                    Paren => {
                        core.cont = Cont::Value(v);
                        Ok(Step {})
                    }
                    Variant(i) => {
                        core.cont = Cont::Value(Value::Variant(i, Some(Box::new(v))));
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
                        core.stack.push_back(Frame {
                            cont: FrameCont::Let(p, Cont::Decs(decs)),
                            env: core.env.clone(),
                        });
                        core.cont = Cont::Exp_(Box::new(e));
                        Ok(Step {})
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

/// Used for tests in check module.
pub fn eval(prog: &str) -> Result<Value, ()> {
    let p = crate::check::parse(&prog)?;
    let c = core_init(p);
    let mut l = local_init(c);
    let s = local_run(&mut l, &Limits::none()).map_err(|_| ())?;
    println!("final signal: {:?}", s);
    use crate::vm_types::Signal::*;
    match s {
        ReachedLimit(_) => unreachable!(),
        Done(result) => Ok(result),
    }
}
