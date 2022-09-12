use crate::ast::{
    BinOp, Cases, Dec, Dec_, Exp, Exp_, Inst, Mut, Pat, PrimType, Prog, RelOp, Source, Type, UnOp,
};
use crate::ast_traversal::ToNode;
use crate::value::{Closed, ClosedFunction, PrimFunction, Value};
use crate::vm_types::{
    stack::{FieldContext, FieldValue, Frame, FrameCont},
    Breakpoint, Cont, Core, Counts, Env, Error, Interruption, Limit, Limits, Local, Pointer,
    Signal, Step, NYI,
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

macro_rules! nyi {
    ($line:expr) => {
        Err(Interruption::NotYetImplemented(NYI {
            file: file!().to_string(),
            line: $line,
        }))
    };
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
        debug_print_out: Vector::new(),
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
        _ => nyi!(line!()),
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
    if let Unit = v1 {
        return Err(Interruption::TypeMismatch);
    };
    if let Unit = v2 {
        return Err(Interruption::TypeMismatch);
    };
    match binop {
        Add => match (v1, v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 + n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 + i2)),
            _ => nyi!(line!()),
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
            _ => nyi!(line!()),
        },
        Mul => match (v1, v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 * n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 * i2)),
            _ => nyi!(line!()),
        },
        WAdd => match (cont_prim_type, v1, v2) {
            (None, _, _) => Err(Interruption::AmbiguousOperation),
            (Some(t), Value::Nat(n1), Value::Nat(n2)) => match t {
                PrimType::Nat => Ok(Value::Nat(n1 + n2)),
                PrimType::Nat8 => Ok(Value::Nat(
                    (n1 + n2) % BigUint::parse_bytes(b"256", 10).unwrap(),
                )),
                _ => nyi!(line!()),
            },
            _ => nyi!(line!()),
        },
        _ => nyi!(line!()),
    }
}

fn relop(
    _cont_prim_type: &Option<PrimType>,
    relop: RelOp,
    v1: Value,
    v2: Value,
) -> Result<Value, Interruption> {
    use RelOp::*;
    use Value::*;
    match relop {
        Eq => match (v1, v2) {
            (Nat(n1), Nat(n2)) => Ok(Bool(n1 == n2)),
            (Int(i1), Int(i2)) => Ok(Bool(i1 == i2)),
            _ => nyi!(line!()),
        },
        Neq => match (v1, v2) {
            (Nat(n1), Nat(n2)) => Ok(Bool(n1 != n2)),
            (Int(i1), Int(i2)) => Ok(Bool(i1 != i2)),
            _ => nyi!(line!()),
        },
        _ => nyi!(line!()),
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
        Cont::Exp_(cont, Vector::new()),
        cont_source,
    )
}

fn call_prim_function(
    core: &mut Core,
    pf: PrimFunction,
    _targs: Option<Inst>,
    args: Value,
) -> Result<Step, Interruption> {
    use PrimFunction::*;
    match pf {
        DebugPrint => match args {
            Value::Text(s) => {
                log::info!("DebugPrint: {}: {:?}", core.cont_source, s);
                core.debug_print_out.push_back(s);
                core.cont = Cont::Value(Value::Unit);
                Ok(Step {})
            }
            _ => Err(Interruption::TypeMismatch),
        },
    }
}

fn call_function(
    core: &mut Core,
    cf: ClosedFunction,
    _targs: Option<Inst>,
    args: Value,
) -> Result<Step, Interruption> {
    if let Some(env_) = pattern_matches(&cf.0.env, &cf.0.content.3 .0, &args) {
        let source = core.cont_source.clone();
        let env_saved = core.env.clone();
        core.env = env_;
        cf.0.content
            .0
            .clone()
            .map(|f| core.env.insert(*f.0, Value::Function(cf.clone())));
        core.cont = Cont::Exp_(cf.0.content.6.clone(), Vector::new());
        core.stack.push_front(Frame {
            source,
            env: env_saved,
            cont: FrameCont::Call3,
            cont_prim_type: None, /* to do */
        }); // to match with Return, if any.
        Ok(Step {})
    } else {
        Err(Interruption::TypeMismatch)
    }
}

fn call_dot_next(core: &mut Core, exp: Exp_) -> Exp_ {
    use crate::ast::Literal::Unit;
    use crate::ast::Loc;
    use Exp::*;

    let s = Source::ExpStep {
        source: Box::new(core.cont_source.clone()),
    };
    Loc(
        Box::new(Call(
            Loc(
                Box::new(Dot(exp, Loc(Box::new("next".to_string()), s.clone()))),
                s.clone(),
            ),
            None,
            Loc(Box::new(Literal(Unit)), s.clone()),
        )),
        s,
    )
}

fn exp_step(core: &mut Core, exp: Exp_, _limits: &Limits) -> Result<Step, Interruption> {
    use Exp::*;
    let source = exp.1.clone();
    match *exp.0 {
        Literal(l) => {
            core.cont = Cont::Value(Value::from_literal(l).map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        Function(f) => {
            core.cont = Cont::Value(Value::Function(ClosedFunction(Closed {
                env: core.env.clone(),
                content: f,
            })));
            Ok(Step {})
        }
        Call(e1, inst, e2) => exp_conts(core, FrameCont::Call1(inst, e2), e1),
        Return(None) => return_(core, Value::Unit),
        Return(Some(e)) => exp_conts(core, FrameCont::Return, e),
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
        Assert(e) => exp_conts(core, FrameCont::Assert, e),
        Object(fs) => {
            let mut fs: Vector<_> = fs.vec.into();
            match fs.pop_front() {
                None => {
                    core.cont = Cont::Value(Value::Object(HashMap::new()));
                    Ok(Step {})
                }
                Some(f1) => {
                    let f1 = f1.0;
                    let fc = FieldContext {
                        mut_: f1.mut_,
                        id: f1.id,
                        typ: f1.typ,
                    };
                    exp_conts(core, FrameCont::Object(Vector::new(), fc, fs), f1.exp)
                }
            }
        }
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
        Array(mut_, es) => {
            let mut es: Vector<_> = es.vec.into();
            match es.pop_front() {
                None => {
                    core.cont = Cont::Value(Value::Array(mut_, Vector::new()));
                    Ok(Step {})
                }
                Some(e1) => exp_conts(core, FrameCont::Array(mut_, Vector::new(), es), e1),
            }
        }
        Idx(e1, e2) => exp_conts(core, FrameCont::Idx1(e2), e1),
        Annot(e, t) => {
            match &*t.0 {
                Type::Prim(pt) => core.cont_prim_type = Some(pt.clone()),
                _ => {}
            };
            exp_conts(core, FrameCont::Annot(t), e)
        }
        Assign(e1, e2) => exp_conts(core, FrameCont::Assign1(e2), e1),
        Proj(e1, i) => exp_conts(core, FrameCont::Proj(i), e1),
        Dot(e1, f) => exp_conts(core, FrameCont::Dot(f), e1),
        If(e1, e2, e3) => exp_conts(core, FrameCont::If(e2, e3), e1),
        Rel(e1, relop, e2) => exp_conts(core, FrameCont::RelOp1(relop, e2), e1),
        While(e1, e2) => exp_conts(core, FrameCont::While1(e1.clone(), e2), e1),
        For(p, e1, e2) => {
            let next = call_dot_next(core, e1);
            exp_conts(core, FrameCont::For1(p, next.clone(), e2), next)
        }
        And(e1, e2) => exp_conts(core, FrameCont::And1(e2), e1),
        Or(e1, e2) => exp_conts(core, FrameCont::Or1(e2), e1),
        Not(e) => exp_conts(core, FrameCont::Not, e),
        Opt(e) => exp_conts(core, FrameCont::Opt, e),
        DoOpt(e) => exp_conts(core, FrameCont::DoOpt, e),
        Bang(e) => exp_conts(core, FrameCont::Bang, e),
        Ignore(e) => exp_conts(core, FrameCont::Ignore, e),
        Debug(e) => exp_conts(core, FrameCont::Debug, e),
        Prim(s) => match &s.as_str() {
            &"\"debugPrint\"" => {
                core.cont = Cont::Value(Value::PrimFunction(PrimFunction::DebugPrint));
                Ok(Step {})
            }
            _ => nyi!(line!()),
        },
        _ => nyi!(line!()),
    }
}

fn pattern_matches(env: &Env, pat: &Pat, v: &Value) -> Option<Env> {
    use crate::ast::Literal;
    match (pat, v) {
        (Pat::Wild, _) => Some(env.clone()),
        (Pat::Literal(Literal::Unit), Value::Unit) => Some(env.clone()),
        (Pat::Paren(p), v) => pattern_matches(env, &*p.0, v),
        (Pat::Annot(p, _), v) => pattern_matches(env, &*p.0, v),
        (Pat::Var(x), v) => {
            let mut env = env.clone();
            env.insert(*x.0.clone(), v.clone());
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
            core.cont = Cont::Exp_(case.0.exp, Vector::new());
            return Ok(Step {});
        }
    }
    Err(Interruption::NoMatchingCase)
}

fn bang_null(core: &mut Core) -> Result<Step, Interruption> {
    let mut stack = core.stack.clone();
    loop {
        if let Some(fr) = stack.pop_front() {
            match fr.cont {
                FrameCont::DoOpt => {
                    core.stack = stack;
                    core.cont = Cont::Value(Value::Null);
                    return Ok(Step {});
                }
                _ => {}
            }
        } else {
            return Err(Interruption::NoDoQuestBangNull);
        }
    }
}

fn return_(core: &mut Core, v: Value) -> Result<Step, Interruption> {
    let mut stack = core.stack.clone();
    loop {
        if let Some(fr) = stack.pop_front() {
            match fr.cont {
                FrameCont::Call3 => {
                    core.stack = stack;
                    core.env = fr.env;
                    core.cont = Cont::Value(v);
                    return Ok(Step {});
                }
                _ => {}
            }
        } else {
            return Err(Interruption::MisplacedReturn);
        }
    }
}

fn source_from_decs(decs: &Vector<Dec_>) -> Source {
    if decs.len() == 0 {
        Source::Unknown
    } else {
        let first = decs.front().unwrap().1.clone();
        match decs.back() {
            None => first,
            Some(back) => first.expand(&back.1),
        }
    }
}

fn source_from_cont(cont: &Cont) -> Source {
    use Cont::*;
    match cont {
        Taken => {
            unreachable!("no source for Taken continuation. This signals a VM bug.  Please report.")
        }
        Decs(decs) => source_from_decs(decs),
        Exp_(exp_, decs) => {
            if decs.len() == 0 {
                exp_.1.clone()
            } else {
                exp_.1.expand(&decs.back().unwrap().1)
            }
        }
        LetVarRet(s, _) => s.clone(),
        Value(_v) => Source::Evaluation,
    }
}

mod store {
    use super::{Core, Interruption, Mut, Pointer, Value};

    pub fn alloc(core: &mut Core, v: Value) -> Pointer {
        let ptr = core.store.len();
        core.store.insert(Pointer(ptr), v);
        Pointer(ptr)
    }

    pub fn deref(core: &mut Core, p: &Pointer) -> Option<Value> {
        match core.store.get(p) {
            None => None,
            Some(v) => Some(v.clone()),
        }
    }

    pub fn mutate(core: &mut Core, p: Pointer, v: Value) -> Result<(), Interruption> {
        // it is an error to mutate an unallocated pointer.
        match core.store.get(&p) {
            None => return Err(Interruption::Dangling(p)),
            Some(_) => (),
        };
        core.store.insert(p, v);
        Ok(())
    }

    pub fn mutate_array(
        core: &mut Core,
        p: Pointer,
        i: usize,
        v: Value,
    ) -> Result<(), Interruption> {
        // it is an error to mutate an unallocated pointer.
        match core.store.get_mut(&p) {
            None => Err(Interruption::Dangling(p)),
            Some(Value::Array(Mut::Var, a)) => {
                if i < a.len() {
                    drop(a.set(i, v));
                    return Ok(());
                } else {
                    Err(Interruption::IndexOutOfBounds)
                }
            }
            _ => Err(Interruption::TypeMismatch),
        }
    }
}

fn usize_from_biguint(n: BigUint, max: Option<usize>) -> Result<usize, Interruption> {
    let digits = n.to_u64_digits();
    if digits.len() == 0 {
        return Ok(0);
    } else if digits.len() > 1 {
        Err(Interruption::IndexOutOfBounds)
    } else {
        if let Some(m) = max {
            if (digits[0] as usize) < m {
                Ok(digits[0] as usize)
            } else {
                Err(Interruption::IndexOutOfBounds)
            }
        } else {
            Ok(digits[0] as usize)
        }
    }
}

// continue execution using the top-most stack frame, if any.
fn stack_cont(core: &mut Core, limits: &Limits, v: Value) -> Result<Step, Interruption> {
    if core.stack.len() == 0 {
        Err(Interruption::Done(v.clone()))
    } else {
        use FrameCont::*;
        let frame = core.stack.pop_front().unwrap();
        match &frame.cont {
            Decs(_) => { /* decs in same block share an environment. */ }
            _ => {
                core.env = frame.env;
            }
        }
        core.cont_prim_type = frame.cont_prim_type;
        core.cont_source = frame.source;
        match frame.cont {
            UnOp(un) => {
                core.cont = Cont::Value(unop(un, v)?);
                Ok(Step {})
            }
            RelOp1(relop, e2) => exp_conts(core, RelOp2(v, relop), e2),
            RelOp2(v1, rel) => {
                core.cont = Cont::Value(relop(&core.cont_prim_type, rel, v1, v)?);
                Ok(Step {})
            }
            BinOp1(binop, e2) => exp_conts(core, BinOp2(v, binop), e2),
            BinOp2(v1, bop) => {
                core.cont = Cont::Value(binop(&core.cont_prim_type, bop, v1, v)?);
                Ok(Step {})
            }
            Assign1(e2) => match v {
                Value::Pointer(p) => exp_conts(core, Assign2(Value::Pointer(p)), e2),
                Value::ArrayOffset(p, i) => exp_conts(core, Assign2(Value::ArrayOffset(p, i)), e2),
                _ => Err(Interruption::TypeMismatch),
            },
            Assign2(Value::Pointer(p)) => {
                store::mutate(core, p, v)?;
                core.cont = Cont::Value(Value::Unit);
                Ok(Step {})
            }
            Assign2(Value::ArrayOffset(p, i)) => {
                store::mutate_array(core, p, i, v)?;
                core.cont = Cont::Value(Value::Unit);
                Ok(Step {})
            }
            Idx1(e2) => exp_conts(core, Idx2(v), e2),
            Idx2(v1) => {
                if let Some(Frame {
                    cont: FrameCont::Assign1(_), // still need to evaluate RHS of assignment.
                    ..
                }) = core.stack.get(0)
                {
                    match (v1, v) {
                        (Value::Pointer(p), Value::Nat(i)) => {
                            // save array pointer and offset until after RHS is evaluated.
                            let i = usize_from_biguint(i, None)?;
                            core.cont = Cont::Value(Value::ArrayOffset(p, i));
                            Ok(Step {})
                        }
                        _ => Err(Interruption::TypeMismatch),
                    }
                } else {
                    match (v1, v) {
                        (Value::Array(_mut_, a), Value::Nat(i)) => {
                            let i = usize_from_biguint(i, Some(a.len()))?;
                            core.cont = Cont::Value(a.get(i.into()).unwrap().clone());
                            Ok(Step {})
                        }
                        (Value::Pointer(p), Value::Nat(i)) => match store::deref(core, &p) {
                            None => Err(Interruption::Dangling(p.clone())),
                            Some(Value::Array(_mut_, a)) => {
                                let i = usize_from_biguint(i, Some(a.len()))?;
                                core.cont = Cont::Value(a.get(i).unwrap().clone());
                                Ok(Step {})
                            }
                            _ => Err(Interruption::TypeMismatch),
                        },
                        _ => Err(Interruption::TypeMismatch),
                    }
                }
            }
            Let(p, cont) => {
                if let Some(env) = pattern_matches(&core.env, &p, &v) {
                    core.env = env;
                    core.cont_source = source_from_cont(&cont);
                    core.cont = cont;
                    Ok(Step {})
                } else {
                    Err(Interruption::TypeMismatch)
                }
            }
            Var(x, cont) => {
                let ptr = store::alloc(core, v);
                core.env.insert(x, Value::Pointer(ptr));
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
            Decs(decs) => {
                match decs.front() {
                    None => {
                        // return final value from block.
                        core.cont = Cont::Value(v);
                        return Ok(Step {});
                    }
                    Some(_) => {
                        core.cont = Cont::Decs(decs);
                        Ok(Step {})
                    }
                }
            }
            Do => {
                core.cont = Cont::Value(v);
                Ok(Step {})
            }
            Assert => match v {
                Value::Bool(true) => {
                    core.cont = Cont::Value(Value::Unit);
                    Ok(Step {})
                }
                Value::Bool(false) => Err(Interruption::AssertionFailure),
                _ => Err(Interruption::TypeMismatch),
            },
            Ignore => {
                core.cont = Cont::Value(Value::Unit);
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
            Array(mut_, mut done, mut rest) => {
                done.push_back(v);
                match rest.pop_front() {
                    None => {
                        if let Mut::Const = mut_ {
                            core.cont = Cont::Value(Value::Array(mut_, done));
                            Ok(Step {})
                        } else {
                            let arr = Value::Array(mut_, done);
                            let ptr = store::alloc(core, arr);
                            core.cont = Cont::Value(Value::Pointer(ptr));
                            Ok(Step {})
                        }
                    }
                    Some(next) => exp_conts(core, Array(mut_, done, rest), next),
                }
            }
            Object(mut done, ctx, mut rest) => {
                done.push_back(FieldValue {
                    mut_: ctx.mut_,
                    id: ctx.id,
                    typ: ctx.typ,
                    val: v,
                });
                match rest.pop_front() {
                    None => {
                        let mut hm = HashMap::new();
                        for f in done.into_iter() {
                            let id = *f.id.0; // to do -- avoid cloning strings. Use Rc.
                            let val = match f.mut_ {
                                Mut::Const => f.val,
                                Mut::Var => Value::Pointer(store::alloc(core, f.val)),
                            };
                            hm.insert(
                                id.clone(),
                                crate::value::FieldValue {
                                    mut_: f.mut_,
                                    id: id,
                                    val: val,
                                },
                            );
                        }
                        core.cont = Cont::Value(Value::Object(hm));
                        Ok(Step {})
                    }
                    Some(next) => exp_conts(
                        core,
                        Object(
                            done,
                            FieldContext {
                                mut_: next.0.mut_,
                                id: next.0.id,
                                typ: next.0.typ,
                            },
                            rest,
                        ),
                        next.0.exp,
                    ),
                }
            }
            Annot(_t) => {
                core.cont = Cont::Value(v);
                Ok(Step {})
            }
            Proj(i) => match v {
                Value::Tuple(vs) => {
                    if i < vs.len() {
                        let vi = vs.get(i).unwrap();
                        core.cont = Cont::Value(vi.clone());
                        Ok(Step {})
                    } else {
                        Err(Interruption::TypeMismatch)
                    }
                }
                _ => Err(Interruption::TypeMismatch),
            },
            Dot(f) => match v {
                Value::Object(fs) => {
                    if let Some(f) = fs.get(&*f.0) {
                        core.cont = Cont::Value(f.val.clone());
                        Ok(Step {})
                    } else {
                        Err(Interruption::TypeMismatch)
                    }
                }
                _ => Err(Interruption::TypeMismatch),
            },
            Debug => match v {
                Value::Unit => {
                    core.cont = Cont::Value(v);
                    Ok(Step {})
                }
                _ => Err(Interruption::TypeMismatch),
            },
            If(e2, e3) => match v {
                Value::Bool(b) => {
                    core.cont = if b {
                        Cont::Exp_(e2, Vector::new())
                    } else {
                        match e3 {
                            Some(e3) => Cont::Exp_(e3, Vector::new()),
                            None => Cont::Value(Value::Unit),
                        }
                    };
                    Ok(Step {})
                }
                _ => Err(Interruption::TypeMismatch),
            },
            While1(e1, e2) => match v {
                Value::Bool(b) => {
                    if b {
                        exp_conts(core, FrameCont::While2(e1, e2.clone()), e2)
                    } else {
                        core.cont = Cont::Value(Value::Unit);
                        Ok(Step {})
                    }
                }
                _ => Err(Interruption::TypeMismatch),
            },
            While2(e1, e2) => match v {
                Value::Unit => exp_conts(core, FrameCont::While1(e1.clone(), e2), e1),
                _ => Err(Interruption::TypeMismatch),
            },
            For1(p, e1, e2) => match v {
                Value::Null => {
                    core.cont = Cont::Value(Value::Unit);
                    Ok(Step {})
                }
                Value::Option(v_) => {
                    if let Some(env) = pattern_matches(&core.env, &*p.0, &v_) {
                        core.env = env;
                        exp_conts(core, FrameCont::For2(p, e1, e2.clone()), e2)
                    } else {
                        Err(Interruption::TypeMismatch)
                    }
                }
                _ => Err(Interruption::TypeMismatch),
            },
            For2(p, e1, e2) => match v {
                Value::Unit => exp_conts(core, FrameCont::For1(p, e1.clone(), e2), e1),
                _ => Err(Interruption::TypeMismatch),
            },
            And1(e2) => match v {
                Value::Bool(b) => {
                    if b {
                        exp_conts(core, FrameCont::And2, e2)
                    } else {
                        core.cont = Cont::Value(Value::Bool(false));
                        Ok(Step {})
                    }
                }
                _ => Err(Interruption::TypeMismatch),
            },
            And2 => match v {
                Value::Bool(b) => {
                    core.cont = Cont::Value(Value::Bool(b));
                    Ok(Step {})
                }
                _ => Err(Interruption::TypeMismatch),
            },
            Or1(e2) => match v {
                Value::Bool(b) => {
                    if b {
                        core.cont = Cont::Value(Value::Bool(true));
                        Ok(Step {})
                    } else {
                        exp_conts(core, FrameCont::Or2, e2)
                    }
                }
                _ => Err(Interruption::TypeMismatch),
            },
            Or2 => match v {
                Value::Bool(b) => {
                    core.cont = Cont::Value(Value::Bool(b));
                    Ok(Step {})
                }
                _ => Err(Interruption::TypeMismatch),
            },
            Not => match v {
                Value::Bool(b) => {
                    core.cont = Cont::Value(Value::Bool(!b));
                    Ok(Step {})
                }
                _ => Err(Interruption::TypeMismatch),
            },
            Opt => {
                core.cont = Cont::Value(Value::Option(Box::new(v)));
                Ok(Step {})
            }
            DoOpt => {
                core.cont = Cont::Value(Value::Option(Box::new(v)));
                Ok(Step {})
            }
            Bang => match v {
                Value::Option(v) => {
                    core.cont = Cont::Value(*v);
                    Ok(Step {})
                }
                Value::Null => bang_null(core),
                _ => Err(Interruption::TypeMismatch),
            },
            Call1(inst, e2) => match v {
                Value::Function(cf) => exp_conts(core, FrameCont::Call2(cf, inst), e2),
                Value::PrimFunction(pf) => exp_conts(core, FrameCont::Call2Prim(pf, inst), e2),
                _ => Err(Interruption::TypeMismatch),
            },
            Call2(cf, inst) => call_function(core, cf, inst, v),
            Call2Prim(pf, inst) => call_prim_function(core, pf, inst, v),
            Call3 => {
                core.cont = Cont::Value(v);
                Ok(Step {})
            }
            Return => return_(core, v),
            _ => nyi!(line!()),
        }
    }
}

// To advance the core Motoko state by a single step.
fn core_step(core: &mut Core, limits: &Limits) -> Result<Step, Interruption> {
    use log::trace;
    trace!("# step {}", core.counts.step);
    trace!(" - cont = {:?}", core.cont);
    trace!("   - cont_source = {:?}", core.cont_source);
    trace!("   - env = {:?}", core.env);
    trace!(" - stack = {:#?}", core.stack);
    trace!(" - store = {:#?}", core.store);
    core.counts.step += 1;
    if let Some(step_limit) = limits.step {
        if core.counts.step > step_limit {
            return Err(Interruption::Limit(Limit::Step));
        }
    }
    let cont = core.cont.clone(); // to do -- avoid clone here.
    core.cont = Cont::Taken;
    match cont {
        Cont::Taken => unreachable!("The VM's logic currently has an internal issue."),
        Cont::Exp_(e, decs) => {
            if decs.len() == 0 {
                core.cont_prim_type = core.cont_prim_type.clone();
                exp_step(core, e, limits)
            } else {
                let source = source_from_decs(&decs);
                core.stack.push_front(Frame {
                    env: core.env.clone(),
                    cont: FrameCont::Decs(decs),
                    source,
                    cont_prim_type: None,
                });
                exp_step(core, e, limits)
            }
        }
        Cont::LetVarRet(_, i) => {
            match i {
                Some(i) => {
                    core.cont =
                        Cont::Value(core.env.get(&*i.0).ok_or(Interruption::Impossible)?.clone())
                }
                None => core.cont = Cont::Value(Value::Unit),
            };
            Ok(Step {})
        }
        Cont::Value(Value::Pointer(p)) => {
            // Are we assigning to this pointer?
            // If not, we are implicitly dereferencing it here.
            match &core.stack.front() {
                // Case: Let-binding the pointer.
                Some(Frame {
                    cont: FrameCont::Let(_, _),
                    ..
                }) => return stack_cont(core, limits, Value::Pointer(p)),
                // Case: Assignment to a pointer.
                Some(Frame {
                    cont: FrameCont::Assign1(_),
                    ..
                }) => return stack_cont(core, limits, Value::Pointer(p)),
                // Case: Array-indexing with a pointer.
                Some(Frame {
                    cont: FrameCont::Idx1(_),
                    ..
                }) => return stack_cont(core, limits, Value::Pointer(p)),
                _ => (),
            };
            // Final case: Implicit dereferencing of pointer:
            let v = store::deref(core, &p).ok_or_else(|| Interruption::Dangling(p.clone()))?;
            core.cont = Cont::Value(v);
            Ok(Step {})
        }
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
                        core.cont = Cont::Exp_(e.node(dec_.1), decs);
                        Ok(Step {})
                    }
                    Dec::Let(p, e) => {
                        if decs.len() == 0 {
                            let i = match &*p.0 {
                                Pat::Var(i) => Some(i.clone()),
                                _ => None,
                            };
                            exp_conts(
                                core,
                                FrameCont::Let(*p.0, Cont::LetVarRet(core.cont_source.clone(), i)),
                                e,
                            )
                        } else {
                            exp_conts(core, FrameCont::Let(*p.0, Cont::Decs(decs)), e)
                        }
                    }
                    Dec::Var(p, e) => match *p.0 {
                        Pat::Var(x) => exp_conts(core, FrameCont::Var(*x.0, Cont::Decs(decs)), e),
                        _ => nyi!(line!()),
                    },
                    Dec::Func(f) => {
                        let id = f.0.clone();
                        let v = Value::Function(ClosedFunction(Closed {
                            env: core.env.clone(),
                            content: f,
                        }));
                        if decs.len() == 0 {
                            core.cont = Cont::Value(v);
                            Ok(Step {})
                        } else {
                            if let Some(i) = id {
                                core.env.insert(*i.0, v);
                            };
                            core.cont = Cont::Decs(decs);
                            Ok(Step {})
                        }
                    }
                    _ => nyi!(line!()),
                }
            }
        } //_ => unimplemented!(),
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
    info!("eval_limit:");
    info!("  - prog = {}", prog);
    info!("  - limits = {:#?}", limits);
    use crate::vm_types::Interruption::SyntaxError;
    let p = crate::check::parse(&prog).map_err(SyntaxError)?;
    info!("eval_limit: parsed.");
    let mut l = Local::new(Core::new(p));
    let s = l.run(limits).map_err(|_| ())?;
    use log::info;
    info!("eval_limit: final signal: {:#?}", s);
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
