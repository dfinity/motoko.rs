use crate::ast::{BinOp, Cases, Dec, Exp, Id as Identifier, Id_, Pat, Prog, UnOp, Type, PrimType};
use crate::value::Value;
use crate::vm_types::{
    stack::{Frame, FrameCont},
    Canister, Cont, Core, Counts, Env, Error, Interruption, Limit, Limits, Local, Signal, Step,
};
use im_rc::{HashMap, Vector};
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};

impl From<()> for Interruption {
    // try to avoid this conversion, except in temp code.
    fn from(_x: ()) -> Interruption {
        Interruption::Unknown
    }
}

impl Limits {
    pub fn none() -> Limits {
        Limits {
            step: None,
            stack: None,
            call: None,
            alloc: None,
            send: None,
        }
    }
    pub fn step(&mut self, s: usize) {
        self.step = Some(s);
    }
}

pub fn core_init(prog: Prog) -> Core {
    Core {
        store: HashMap::new(),
        stack: Vector::new(),
        env: HashMap::new(),
        cont: Cont::Decs(prog.vec.into()),
        cont_prim_type: None : Option<PrimType>,
        counts: Counts {
            step: 0,
            stack: 0,
            call: 0,
            alloc: 0,
            send: 0,
        },
    }
}

fn unop(un: UnOp, v: Value) -> Result<Value, Interruption> {
    match (un, v) {
        (UnOp::Neg, Value::Nat(n)) => Ok(Value::Int(-BigInt::from(n))),
        _ => todo!(),
    }
}

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
            (Nat(n1), Nat(n2)) => {
                if n2 > n1 {
                    Ok(Int(BigInt::from(n1) - BigInt::from(n2)))
                } else {
                    Ok(Nat(n1 - n2))
                }
            }
            (Int(i1), Int(i2)) => Ok(Int(i1 - i2)),
            (Int(i1), Nat(n2)) => Ok(Int(i1 - BigInt::from(n2))),
            /*
                       (Int(i1), Nat(n2)) => Ok(Int(i1 + n2)),
                       (Nat(n1), Int(i2)) => Ok(Int(n1 + i2)),
            */
            _ => todo!(),
        },
        _ => todo!(),
    }
}

fn exp_conts_(core: &mut Core, frame_cont: FrameCont, next_cont: Cont) -> Result<Step, Interruption> {
    core.stack.push_back(Frame {
        env: core.env.clone(),
        cont: frame_cont,
        cont_prim_type: core.cont_prim_type,
    });
    core.cont = next_cont;
    Ok(Step {})
}

fn exp_conts(core: &mut Core, frame_cont: FrameCont, next_cont: _Exp) -> Result<Step, Interruption> {
    exp_conts_(core, frame_cont, Cont::Exp_(next_cont))
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
            exp_conts(core, 
                      FrameCont::BinOp1(binop, e2),
                      e1)
        }
        Un(un, e) => {
            exp_conts(core, FrameCont::UnOp(un),
                      e)
        }
        Paren(e) => {
            exp_conts(FrameCont::Paren, e)
        }
        Variant(id, None) => {
            core.cont = Cont::Value(Value::Variant(id, None));
            Ok(Step {})
        }
        Variant(id, Some(e)) => {
            exp_conts(FrameCont::Variant(id), e)
        }
        Switch(e1, cases) => {
            exp_conts(FrameCont::Switch(cases),e1)
        }
        Block(decs) => {
            exp_conts_(FrameCont::Block, Cont::Decs(decs.vec.into())
        }
        Do(e) => {
            exp_conts(FrameCont::Do, e)
        }
        Tuple(es) => {
            let mut es: Vector<_> = es.vec.into();
            match es.pop_front() {
                None => {
                    core.cont = Cont::Value(Value::Unit);
                    Ok(Step {})
                }
                Some(e1) => {
                    exp_conts(FrameCont::Tuple(Vector::new(), es),
                              Box::new(e1))
                }
            }
        }
        Annot(e, t) => {
            match *t {
                Type::PrimType(pt) => {
                    core.cont_prim_type = Some(pt.clone())
                }
                _ => {}
            };
            exp_conts(FrameCont::Annot(t), e)
        }
        _ => todo!(),
    }
}

fn pattern_matches(env: &Env, pat: &Pat, v: &Value) -> Option<Env> {
    match (pat, v) {
        (Pat::Paren(p), v) => pattern_matches(env, &*p, v),
        (Pat::Var(x), v) => {
            let mut env = env.clone();
            env.insert(x.clone(), v.clone());
            Some(env)
        }
        (Pat::Variant(id1, None), Value::Variant(id2, None)) => {
            if *id1 != **id2 {
                return None;
            };
            Some(env.clone())
        }
        (Pat::Variant(id1, Some(pat_)), Value::Variant(id2, Some(v_))) => {
            if *id1 != **id2 {
                return None;
            };
            pattern_matches(env, &*pat_, &*v_)
        }
        _ => None,
    }
}

fn switch(core: &mut Core, limits: &Limits, v: Value, cases: Cases) -> Result<Step, Interruption> {
    for case in cases.vec.into_iter() {
        if let Some(env) = pattern_matches(&core.env, &case.pat, &v) {
            core.env = env;
            core.cont = Cont::Exp_(Box::new(case.exp));
            return Ok(Step {});
        }
    }
    Err(Interruption::NoMatchingCase)
}

// continue execution using the top-most stack frame, if any.
fn stack_cont(core: &mut Core, limits: &Limits, v: Value) -> Result<Step, Interruption> {
    if core.stack.len() == 0 {
        Err(Interruption::Done(v.clone()))
    } else {
        use FrameCont::*;
        let frame = core.stack.pop_back().unwrap();
        core.env = frame.env;
        core.cont_prim_type = frame.cont_prim_type;
        match frame.cont {
            UnOp(un) => {
                core.cont = Cont::Value(unop(un, v)?);
                Ok(Step {})
            }
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
            Switch(cases) => switch(core, limits, v, cases),
            Block => {
                core.cont = Cont::Value(v);
                Ok(Step {})
            }
            Do => {
                core.cont = Cont::Value(v);
                Ok(Step {})
            }
            Tuple(mut done, mut rest) => {
                done.push_back(v);
                match rest.pop_front() {
                    None => {
                        core.cont = Cont::Value(Value::Tuple(done));
                        Ok(Step {})
                    }
                    Some(next) => {
                        exp_conts(core, Tuple(done, rest), Cont::Exp_(Box::new(next)))
                    }
                }
            }
            _ => todo!(),
        }
    }
}

// To advance the core Motoko state by a single step.
pub fn core_step(core: &mut Core, limits: &Limits) -> Result<Step, Interruption> {
    println!("# step {}", core.counts.step);
    println!(" - cont = {:?}", core.cont);
    println!(" - env = {:?}", core.env);
    println!(" - stack = {:?}", core.stack);
    core.counts.step += 1;
    if let Some(step_limit) = limits.step {
        if core.counts.step > step_limit {
            return Err(Interruption::Limit(Limit::Step));
        }
    }
    let cont = core.cont.clone(); // to do -- avoid clone here.
    core.cont = Cont::Taken;
    match cont {
        Cont::Exp_(e) => exp_step(core, *e, limits),
        Cont::Value(v) => stack_cont(core, limits, v),
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
            Err(other_interruption) => return Ok(Signal::Interruption(other_interruption)),
        }
    }
}

/// Used for tests in check module.
pub fn eval_limit(prog: &str, limits: &Limits) -> Result<Value, Interruption> {
    let p = crate::check::parse(&prog)?;
    let c = core_init(p);
    let mut l = local_init(c);
    let s = local_run(&mut l, limits).map_err(|_| ())?;
    println!("final signal: {:?}", s);
    use crate::vm_types::Signal::*;
    match s {
        Done(result) => Ok(result),
        Interruption(i) => Err(i),
    }
}


/// Used for tests in check module.
pub fn eval(prog: &str) -> Result<Value, ()> {
    eval_limit(prog, &Limits::none()).map_err(|_| ())
}
