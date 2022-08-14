use crate::ast::{BinOp, Cases, Dec, Exp, Loc, Pat, Prog, UnOp};
use crate::value::Value;
use crate::vm_types::{
    stack::{Frame, FrameCont},
    Cont, Core, Counts, Env, Error, Interruption, Limit, Limits, Local, Signal, Step,
};
use im_rc::{HashMap, Vector};
use num_bigint::BigInt;

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

fn exp_step(core: &mut Core, exp: Exp, limits: &Limits) -> Result<Step, Interruption> {
    use Exp::*;
    match exp {
        Literal(l) => {
            core.cont =
                Cont::Value(Value::from_literal(*l.0).map_err(|_| Interruption::ParseError)?);
            Ok(Step {})
        }
        Var(x) => match core.env.get(&*x.0) {
            None => Err(Interruption::UnboundIdentifer((*x.0).clone())),
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
        Un(un, e) => {
            core.stack.push_back(Frame {
                env: core.env.clone(),
                cont: FrameCont::UnOp(un),
            });
            core.cont = Cont::Exp_(e);
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
        Switch(e1, cases) => {
            core.stack.push_back(Frame {
                env: core.env.clone(),
                cont: FrameCont::Switch(cases),
            });
            core.cont = Cont::Exp_(e1);
            Ok(Step {})
        }
        Block(decs) => {
            core.stack.push_back(Frame {
                env: core.env.clone(),
                cont: FrameCont::Block,
            });
            core.cont = Cont::Decs(decs.vec.into());
            Ok(Step {})
        }
        Do(e) => {
            core.stack.push_back(Frame {
                env: core.env.clone(),
                cont: FrameCont::Do,
            });
            core.cont = Cont::Exp_(e);
            Ok(Step {})
        }
        Tuple(es) => {
            let mut es: Vector<_> = es.vec.into();
            match es.pop_front() {
                None => {
                    core.cont = Cont::Value(Value::Unit);
                    Ok(Step {})
                }
                Some(e1) => {
                    core.stack.push_back(Frame {
                        env: core.env.clone(),
                        cont: FrameCont::Tuple(Vector::new(), es),
                    });
                    core.cont = Cont::Exp_(e1);
                    Ok(Step {})
                }
            }
        }
        _ => todo!(),
    }
}

fn pattern_matches(env: &Env, pat: &Pat, v: &Value) -> Option<Env> {
    match (pat, v) {
        (Pat::Paren(p), v) => pattern_matches(env, &*p.0, v),
        (Pat::Var(x), v) => {
            let mut env = env.clone();
            env.insert((*x.0).clone(), v.clone());
            Some(env)
        }
        (Pat::Variant(id1, None), Value::Variant(id2, None)) => {
            if *id1 != **id2.0 {
                return None;
            };
            Some(env.clone())
        }
        (Pat::Variant(id1, Some(pat_)), Value::Variant(id2, Some(v_))) => {
            if *id1 != **id2.0 {
                return None;
            };
            pattern_matches(env, &*pat_.0, &*v_)
        }
        _ => None,
    }
}

fn switch(core: &mut Core, limits: &Limits, v: Value, cases: Cases) -> Result<Step, Interruption> {
    for case in cases.vec.into_iter() {
        if let Some(env) = pattern_matches(&core.env, &*case.0.pat.0, &v) {
            core.env = env;
            core.cont = Cont::Exp_(case.0.exp);
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
                core.env.insert(*x.0, v);
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
                        core.stack.push_back(Frame {
                            env: core.env.clone(),
                            cont: Tuple(done, rest),
                        });
                        core.cont = Cont::Exp_(next);
                        Ok(Step {})
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
        Cont::Exp_(e) => exp_step(core, *e.0, limits),
        Cont::Value(v) => stack_cont(core, limits, v),
        Cont::Decs(mut decs) => {
            if decs.len() == 0 {
                core.cont = Cont::Value(Value::Unit);
                Ok(Step {})
            } else {
                let dec_ = decs.pop_front().unwrap();
                match *dec_.0 {
                    Dec::Exp(e) => {
                        core.cont = Cont::Exp_(Loc(Box::new(e), dec_.1));
                        Ok(Step {})
                    }
                    Dec::Let(p, e) => {
                        core.stack.push_back(Frame {
                            cont: FrameCont::Let(*p.0, Cont::Decs(decs)),
                            env: core.env.clone(),
                        });
                        core.cont = Cont::Exp_(e);
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
