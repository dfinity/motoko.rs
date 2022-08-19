use crate::ast::{BinOp, Cases, Dec, Exp, Exp_, Pat, PrimType, Prog, Source, Type, UnOp};
use crate::ast_traversal::ToNode;
use crate::value::Value;
use crate::vm_types::{
    stack::{Frame, FrameCont},
    Breakpoint, Cont, Core, Counts, Env, Error, Interruption, Limit, Limits, Local, Signal, Step,
};
use im_rc::{HashMap, Vector};
use num_bigint::{BigInt, BigUint};

impl From<()> for Interruption {
    // try to avoid this conversion, except in temp code.
    fn from(_x: ()) -> Interruption {
        Interruption::Unknown
    }
}

impl Limits {
    /// No limits.
    pub fn none() -> Limits {
        Limits {
            breakpoints: vec![],
            step: None,
        }
    }
    /// Set step limit.
    pub fn step(&mut self, s: usize) {
        self.step = Some(s);
    }
}

fn core_init(prog: Prog) -> Core {
    let cont_prim_type: Option<PrimType> = None;
    Core {
        store: HashMap::new(),
        stack: Vector::new(),
        env: HashMap::new(),
        cont: Cont::Decs(prog.vec.into()),
        cont_source: Source::CoreInit, // special source -- or get the "full span" (but then what line or column number would be helpful here?  line 1 column 0?)
        cont_prim_type,
        counts: Counts {
            step: 0,
            /*
            stack: 0,
            call: 0,
            alloc: 0,
            send: 0,
             */
        },
    }
}

fn unop(un: UnOp, v: Value) -> Result<Value, Interruption> {
    match (un, v) {
        (UnOp::Neg, Value::Nat(n)) => Ok(Value::Int(-BigInt::from(n))),
        _ => todo!(),
    }
}

fn binop(
    cont_prim_type: &Option<PrimType>,
    binop: BinOp,
    v1: Value,
    v2: Value,
) -> Result<Value, Interruption> {
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
        WAdd => match (cont_prim_type, v1, v2) {
            (None, _, _) => Err(Interruption::AmbiguousOperation),
            (Some(t), Value::Nat(n1), Value::Nat(n2)) => match t {
                PrimType::Nat => Ok(Value::Nat(n1 + n2)),
                PrimType::Nat8 => Ok(Value::Nat(
                    (n1 + n2) % BigUint::parse_bytes(b"256", 10).unwrap(),
                )),
                _ => todo!(),
            },
            _ => todo!(),
        },
        _ => todo!(),
    }
}

fn exp_conts_(
    core: &mut Core,
    source: Source,
    frame_cont: FrameCont,
    cont: Cont,
    cont_source: Source,
) -> Result<Step, Interruption> {
    core.stack.push_front(Frame {
        env: core.env.clone(),
        cont: frame_cont,
        cont_prim_type: core.cont_prim_type.clone(),
        source,
    });
    core.cont = cont;
    core.cont_source = cont_source;
    Ok(Step {})
}

fn exp_conts(core: &mut Core, frame_cont: FrameCont, cont: Exp_) -> Result<Step, Interruption> {
    let cont_source = cont.1.clone();
    exp_conts_(
        core,
        core.cont_source.clone(),
        frame_cont,
        Cont::Exp_(cont),
        cont_source,
    )
}

fn exp_step(core: &mut Core, exp: Exp_, _limits: &Limits) -> Result<Step, Interruption> {
    use Exp::*;
    let source = exp.1.clone();
    match *exp.0 {
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
        Bin(e1, binop, e2) => exp_conts(core, FrameCont::BinOp1(binop, e2), e1),
        Un(un, e) => exp_conts(core, FrameCont::UnOp(un), e),
        Paren(e) => exp_conts(core, FrameCont::Paren, e),
        Variant(id, None) => {
            core.cont = Cont::Value(Value::Variant(id, None));
            Ok(Step {})
        }
        Variant(id, Some(e)) => exp_conts(core, FrameCont::Variant(id), e),
        Switch(e1, cases) => exp_conts(core, FrameCont::Switch(cases), e1),
        Block(decs) => exp_conts_(
            core,
            source.clone(),
            FrameCont::Block,
            Cont::Decs(decs.vec.into()),
            source,
        ),
        Do(e) => exp_conts(core, FrameCont::Do, e),
        Tuple(es) => {
            let mut es: Vector<_> = es.vec.into();
            match es.pop_front() {
                None => {
                    core.cont = Cont::Value(Value::Unit);
                    Ok(Step {})
                }
                Some(e1) => exp_conts(core, FrameCont::Tuple(Vector::new(), es), e1),
            }
        }
        Annot(e, t) => {
            match &*t.0 {
                Type::Prim(pt) => core.cont_prim_type = Some(pt.clone()),
                _ => {}
            };
            exp_conts(core, FrameCont::Annot(t), e)
        }
        _ => todo!(),
    }
}

fn pattern_matches(env: &Env, pat: &Pat, v: &Value) -> Option<Env> {
    match (pat, v) {
        (Pat::Paren(p), v) => pattern_matches(env, &*p.0, v),
        (Pat::Var(x), v) => {
            let mut env = env.clone();
            env.insert(x.clone(), v.clone());
            Some(env)
        }
        (Pat::Variant(id1, None), Value::Variant(id2, None)) => {
            if **id1.0 != **id2.0 {
                return None;
            };
            Some(env.clone())
        }
        (Pat::Variant(id1, Some(pat_)), Value::Variant(id2, Some(v_))) => {
            if **id1.0 != **id2.0 {
                return None;
            };
            pattern_matches(env, &*pat_.0, &*v_)
        }
        _ => None,
    }
}

fn switch(core: &mut Core, _limits: &Limits, v: Value, cases: Cases) -> Result<Step, Interruption> {
    for case in cases.vec.into_iter() {
        if let Some(env) = pattern_matches(&core.env, &*case.0.pat.0, &v) {
            core.env = env;
            core.cont_source = case.0.exp.1.clone();
            core.cont = Cont::Exp_(case.0.exp);
            return Ok(Step {});
        }
    }
    Err(Interruption::NoMatchingCase)
}

fn source_from_cont<'a>(cont: &'a Cont) -> Source {
    use Cont::*;
    match cont {
        Taken => {
            unreachable!("no source for Taken continuation. This signals a VM bug.  Please report.")
        }
        Decs(decs) => decs.front().unwrap().1.expand(&decs.back().unwrap().1),
        Exp_(exp_) => exp_.1.clone(),
        Value(_v) => Source::Evaluation,
    }
}

// continue execution using the top-most stack frame, if any.
fn stack_cont(core: &mut Core, limits: &Limits, v: Value) -> Result<Step, Interruption> {
    if core.stack.len() == 0 {
        Err(Interruption::Done(v.clone()))
    } else {
        use FrameCont::*;
        let frame = core.stack.pop_front().unwrap();
        core.env = frame.env;
        core.cont_prim_type = frame.cont_prim_type;
        core.cont_source = frame.source;
        match frame.cont {
            UnOp(un) => {
                core.cont = Cont::Value(unop(un, v)?);
                Ok(Step {})
            }
            BinOp1(binop, e2) => exp_conts(core, BinOp2(v, binop), e2),
            BinOp2(v1, bop) => {
                core.cont = Cont::Value(binop(&core.cont_prim_type, bop, v1, v)?);
                Ok(Step {})
            }
            Let(Pat::Var(x), cont) => {
                core.env.insert(x, v);
                core.cont_source = source_from_cont(&cont);
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
                    Some(next) => exp_conts(core, Tuple(done, rest), next),
                }
            }
            Annot(_t) => {
                core.cont = Cont::Value(v);
                Ok(Step {})
            }
            _ => todo!(),
        }
    }
}

// To advance the core Motoko state by a single step.
fn core_step(core: &mut Core, limits: &Limits) -> Result<Step, Interruption> {
    println!("# step {}", core.counts.step);
    println!(" - cont_source = {:?}", core.cont_source);
    println!(" - cont = {:?}", core.cont);
    println!(" - env = {:?}", core.env);
    println!(" - stack = {:#?}", core.stack);
    core.counts.step += 1;
    if let Some(step_limit) = limits.step {
        if core.counts.step > step_limit {
            return Err(Interruption::Limit(Limit::Step));
        }
    }
    let cont = core.cont.clone(); // to do -- avoid clone here.
    core.cont = Cont::Taken;
    match cont {
        Cont::Exp_(e) => exp_step(core, e, limits),
        Cont::Value(v) => stack_cont(core, limits, v),
        Cont::Decs(mut decs) => {
            if decs.len() == 0 {
                core.cont = Cont::Value(Value::Unit);
                core.cont_source = Source::Evaluation;
                Ok(Step {})
            } else {
                let dec_ = decs.pop_front().unwrap();
                match *dec_.0 {
                    Dec::Exp(e) => {
                        core.cont_source = dec_.1.clone();
                        core.cont = Cont::Exp_(e.node(dec_.1));
                        Ok(Step {})
                    }
                    Dec::Let(p, e) => exp_conts(core, FrameCont::Let(*p.0, Cont::Decs(decs)), e),
                    _ => todo!(),
                }
            }
        }
        _ => unimplemented!(),
    }
}

impl Core {
    /// New VM core for a given program.
    pub fn new(prog: Prog) -> Self {
        core_init(prog)
    }

    /// Step VM core, under some limits.
    pub fn step(&mut self, limits: &Limits) -> Result<Step, Interruption> {
        core_step(self, limits)
    }
}

// For core Motoko state initializing local VM state.
fn local_init(active: Core) -> Local {
    Local { active }
}

// Returns `Some(span)` if the limits include the breakpoint.
fn check_for_breakpoint(local: &mut Local, limits: &Limits) -> Option<Breakpoint> {
    let cont_span = &local.active.cont_source.span();
    if let Some(span) = cont_span {
        if limits.breakpoints.contains(span) {
            Some(span.clone())
        } else {
            None
        }
    } else {
        None
    }
}

// use ic-agent to do function calls.
fn local_run(local: &mut Local, limits: &Limits) -> Result<Signal, Error> {
    loop {
        if let Some(break_span) = check_for_breakpoint(local, limits) {
            return Ok(Signal::Breakpoint(break_span));
        }
        match core_step(&mut local.active, limits) {
            Ok(_step) => { /* to do */ }
            Err(Interruption::Done(v)) => return Ok(Signal::Done(v)),
            Err(other_interruption) => return Ok(Signal::Interruption(other_interruption)),
        }
    }
}

impl Local {
    /// New local VM instance for given `Core`.
    pub fn new(core: Core) -> Self {
        local_init(core)
    }
    /// Run the local VM (under some possible limits).
    pub fn run(&mut self, limits: &Limits) -> Result<Signal, Error> {
        local_run(self, limits)
    }
}

/// Used for tests in check module.
pub fn eval_limit(prog: &str, limits: &Limits) -> Result<Value, Interruption> {
    let p = crate::check::parse(&prog)?;
    let mut l = Local::new(Core::new(p));
    let s = l.run(limits).map_err(|_| ())?;
    println!("final signal: {:?}", s);
    use crate::vm_types::Signal::*;
    match s {
        Done(result) => Ok(result),
        Interruption(i) => Err(i),
        Breakpoint(_) => todo!(),
    }
}

/// Used for tests in check module.
pub fn eval(prog: &str) -> Result<Value, ()> {
    eval_limit(prog, &Limits::none()).map_err(|_| ())
}

/// Used for tests in check module.
pub fn eval_(prog: &str) -> Result<Value, Interruption> {
    eval_limit(prog, &Limits::none())
}
