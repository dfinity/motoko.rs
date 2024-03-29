use crate::ast::{Cases, Dec, Exp, Exp_, Inst, Literal, Mut, Pat, Pat_, ProjIndex, Source, Type};
use crate::shared::{FastClone, Share};
use crate::value::{
    ActorId, ActorMethod, Closed, ClosedFunction, CollectionFunction, FastRandIter,
    FastRandIterFunction, HashMapFunction, PrimFunction, Value, Value_,
};
use crate::vm_types::{
    def::{Def, Field as FieldDef, Function as FunctionDef},
    stack::{FieldContext, FieldValue, Frame, FrameCont},
    Active, ActiveBorrow, Breakpoint, Cont, DebugPrintLine, Env, Interruption, Limit, Limits,
    ModulePath, Pointer, Response, Step,
};
use im_rc::{HashMap, Vector};

use crate::{nyi, type_mismatch, type_mismatch_};

fn exp_step<A: Active>(active: &mut A, exp: Exp_) -> Result<Step, Interruption> {
    use Exp::*;
    let source = exp.1.clone();
    match &exp.0 {
        Value_(v) => {
            *active.cont() = cont_value((**v).clone());
            Ok(Step {})
        }
        Literal(l) => {
            // TODO: partial evaluation would now be highly efficient due to value sharing
            *active.cont() = cont_value(Value::from_literal(l).map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        Function(f) => {
            let env = active.env().fast_clone();
            *active.cont() = cont_value(Value::Function(ClosedFunction(Closed {
                ctx: active.defs().active_ctx.clone(),
                env,
                content: f.clone(), // TODO: `Shared<Function>`?
            })));
            Ok(Step {})
        }
        Call(e1, inst, e2) => {
            exp_conts(active, FrameCont::Call1(inst.clone(), e2.fast_clone()), e1)
        }
        Return(None) => return_(active, Value::Unit.share()),
        Return(Some(e)) => exp_conts(active, FrameCont::Return, e),
        Var(x) => match active.env().get(x) {
            None => {
                if x.string.starts_with("@") {
                    let f = crate::value::PrimFunction::AtSignVar(x.to_string());
                    let v = Value::PrimFunction(f).share();
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                } else {
                    let ctx = active.defs().active_ctx.clone();
                    let fd = crate::vm_def::resolve_def(active.defs(), &ctx, false, x)?;
                    let v = crate::vm_def::def_as_value(active.defs(), x, &fd.def)?;
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                }
            }
            Some(v) => {
                *active.cont() = Cont::Value_(v.fast_clone());
                Ok(Step {})
            }
        },
        Bin(e1, binop, e2) => exp_conts(
            active,
            FrameCont::BinOp1(binop.clone(), e2.fast_clone()),
            e1,
        ),
        Un(un, e) => exp_conts(active, FrameCont::UnOp(un.clone()), e),
        Paren(e) => exp_conts(active, FrameCont::Paren, e),
        Variant(id, None) => {
            // TODO: cache and share variants?
            *active.cont() = cont_value(Value::Variant(id.0.clone(), None));
            Ok(Step {})
        }
        Variant(id, Some(e)) => exp_conts(active, FrameCont::Variant(id.fast_clone()), e),
        Switch(e1, cases) => exp_conts(active, FrameCont::Switch(cases.clone()), e1),
        Block(decs) => exp_conts_(
            active,
            source.clone(),
            FrameCont::Block,
            Cont::Decs(decs.vec.clone()),
            source,
        ),
        Do(e) => exp_conts(active, FrameCont::Do, e),
        Assert(e) => exp_conts(active, FrameCont::Assert, e),
        Object(bases, fields) => {
            if let Some(_bases) = bases {
                return nyi!(line!());
            };
            if let Some(fields) = fields {
                let mut fs: Vector<_> = fields.vec.fast_clone();
                match fs.pop_front() {
                    None => {
                        *active.cont() = cont_value(Value::Object(HashMap::new()));
                        Ok(Step {})
                    }
                    Some(f1) => {
                        let fc = FieldContext {
                            mut_: f1.0.mut_.clone(),
                            id: f1.0.id.fast_clone(),
                            typ: f1.0.typ.fast_clone(),
                        };
                        exp_conts(
                            active,
                            FrameCont::Object(Vector::new(), fc, fs),
                            &f1.0.exp_(),
                        )
                    }
                }
            } else {
                *active.cont() = cont_value(Value::Object(HashMap::new()));
                Ok(Step {})
            }
        }
        Tuple(es) => {
            let mut es: Vector<_> = es.vec.fast_clone();
            match es.pop_front() {
                None => {
                    // TODO: globally share (), true, false, null, etc.
                    *active.cont() = cont_value(Value::Unit);
                    Ok(Step {})
                }
                Some(e1) => exp_conts(active, FrameCont::Tuple(Vector::new(), es), &e1),
            }
        }
        Array(mut_, es) => {
            let mut es: Vector<_> = es.vec.fast_clone();
            match es.pop_front() {
                None => {
                    *active.cont() = cont_value(Value::Array(mut_.clone(), Vector::new()));
                    Ok(Step {})
                }
                Some(e1) => exp_conts(
                    active,
                    FrameCont::Array(mut_.clone(), Vector::new(), es),
                    &e1,
                ),
            }
        }
        Index(e1, e2) => exp_conts(active, FrameCont::Idx1(e2.fast_clone()), e1),
        Annot(_, e, t) => {
            match &t.0 {
                Type::Prim(pt) => *active.cont_prim_type() = Some(pt.clone()),
                _ => {}
            };
            exp_conts(active, FrameCont::Annot(t.fast_clone()), e)
        }
        Assign(e1, e2) => exp_conts(active, FrameCont::Assign1(e2.fast_clone()), e1),
        BinAssign(e1, b, e2) => exp_conts(
            active,
            FrameCont::BinAssign1(b.clone(), e2.fast_clone()),
            e1,
        ),
        Proj(e1, i) => exp_conts(active, FrameCont::Proj(i.clone()), e1),
        Dot(e1, f) => exp_conts(active, FrameCont::Dot(f.fast_clone()), e1),
        If(e1, e2, e3) => exp_conts(active, FrameCont::If(e2.fast_clone(), e3.fast_clone()), e1),
        Rel(e1, relop, e2) => exp_conts(
            active,
            FrameCont::RelOp1(relop.clone(), e2.fast_clone()),
            e1,
        ),
        While(e1, e2) => exp_conts(
            active,
            FrameCont::While1(e1.fast_clone(), e2.fast_clone()),
            e1,
        ),
        For(p, e1, e2) => exp_conts(active, FrameCont::For1(p.fast_clone(), e2.fast_clone()), e1),
        And(e1, e2) => exp_conts(active, FrameCont::And1(e2.fast_clone()), e1),
        Or(e1, e2) => exp_conts(active, FrameCont::Or1(e2.fast_clone()), e1),
        Not(e) => exp_conts(active, FrameCont::Not, e),
        Opt(e) => exp_conts(active, FrameCont::Opt, e),
        DoOpt(e) => exp_conts(active, FrameCont::DoOpt, e),
        Bang(e) => exp_conts(active, FrameCont::Bang, e),
        Ignore(e) => exp_conts(active, FrameCont::Ignore, e),
        Debug(e) => exp_conts(active, FrameCont::Debug, e),
        Prim(p) => {
            *active.cont() = cont_value(Value::PrimFunction(
                p.clone()
                    .map_err(|s| Interruption::UnrecognizedPrim(s.to_string()))?,
            ));
            Ok(Step {})
        }
        e => nyi!(line!(), "{:?}", e),
    }
}

// To advance the active Motoko state by a single step, after all limits are checked.
fn active_step_<A: Active>(active: &mut A) -> Result<Step, Interruption> {
    active_trace(active);
    let mut cont = Cont::Taken;
    std::mem::swap(active.cont(), &mut cont);
    match cont {
        Cont::Taken => unreachable!("The VM's logic currently has an internal issue."),
        Cont::Exp_(e, decs) => {
            if decs.is_empty() {
                exp_step(active, e)
            } else {
                let source = crate::ast::source_from_decs(&decs);
                let env = active.env().fast_clone();
                let context = active.defs().active_ctx.clone();

                active.stack().push_front(Frame {
                    context,
                    env,
                    cont: FrameCont::Decs(decs),
                    source,
                    cont_prim_type: None,
                });
                exp_step(active, e)
            }
        }
        Cont::LetVarRet(_, i) => {
            match i {
                Some(i) => {
                    *active.cont() = Cont::Value_(
                        active
                            .env()
                            .get(&i.0)
                            .ok_or(Interruption::Impossible)?
                            .fast_clone(),
                    )
                }
                None => *active.cont() = cont_value(Value::Unit),
            };
            Ok(Step {})
        }
        Cont::Value_(v) => {
            match &*v {
                Value::Pointer(p) => {
                    // Are we assigning to this pointer?
                    // If not, we are implicitly dereferencing it here.
                    match &active.stack().front() {
                        // Case: Let-binding the pointer.
                        Some(Frame {
                            cont: FrameCont::Let(_, _),
                            ..
                        }) => return stack_cont(active, v),
                        // Case: Assignment to a pointer.
                        Some(Frame {
                            cont: FrameCont::Assign1(_),
                            ..
                        }) => return stack_cont(active, v),
                        // Case: Binary-op + Assignment to a pointer.
                        Some(Frame {
                            cont: FrameCont::BinAssign1(..),
                            ..
                        }) => return stack_cont(active, v),
                        // Case: Array-indexing with a pointer.
                        Some(Frame {
                            cont: FrameCont::Idx1(_),
                            ..
                        }) => return stack_cont(active, v),
                        _ => (),
                    };
                    // Final case: Implicit dereferencing of pointer:
                    let v = active.deref(p)?;
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                }
                _ => stack_cont(active, v),
            }
        }
        Cont::Decs(mut decs) => {
            if decs.is_empty() {
                *active.cont() = cont_value(Value::Unit);
                *active.cont_source() = Source::Evaluation;
                Ok(Step {})
            } else {
                let dec_ = decs.pop_front().unwrap();
                match &dec_.0 {
                    Dec::Type(..) => {
                        *active.cont() = Cont::Decs(decs);
                        Ok(Step {})
                    }
                    Dec::Exp(e) => {
                        *active.cont_source() = dec_.1.clone();
                        *active.cont() = Cont::Exp_(e.fast_clone(), decs);
                        Ok(Step {})
                    }
                    Dec::Let(p, e) => {
                        if decs.is_empty() {
                            let i = match &p.0 {
                                Pat::Var(i) => Some(i.fast_clone()),
                                _ => None,
                            };
                            let source = active.cont_source().clone();
                            exp_conts(
                                active,
                                FrameCont::Let(p.fast_clone(), Cont::LetVarRet(source, i)),
                                e,
                            )
                        } else {
                            exp_conts(active, FrameCont::Let(p.fast_clone(), Cont::Decs(decs)), e)
                        }
                    }
                    Dec::LetActor(i, _, dfs) => {
                        let v = match i {
                            /* Are we upgrading a local Actor? */
                            None => todo!(),
                            Some(local_name) => {
                                let ctx_id = active.defs().active_ctx.clone();
                                let old_def =
                                    ctx_id.get_field(active, &local_name.0).map(|x| x.clone());

                                let id = ActorId::Local(local_name.0.clone());
                                match old_def {
                                    None => crate::vm_def::def::actor(
                                        active,
                                        format!("<anonymous@{}>", dec_.1),
                                        &id,
                                        dec_.1.clone(),
                                        None,
                                        None,
                                        dfs,
                                    )?,
                                    Some(FieldDef {
                                        def: Def::Actor(old_def),
                                        ..
                                    }) => crate::vm_def::def::actor_upgrade(
                                        active,
                                        format!("<anonymous@{}>", dec_.1),
                                        &id,
                                        dec_.1.clone(),
                                        None,
                                        None,
                                        dfs,
                                        &old_def,
                                    )?,
                                    _ => unreachable!(),
                                }
                            }
                        };
                        match i {
                            None => (),
                            Some(i) => {
                                active.env().insert(i.0.clone(), v);
                            }
                        };
                        *active.cont() = Cont::Decs(decs);
                        Ok(Step {})
                    }
                    Dec::LetObject(_id, _, _dfs) => {
                        nyi!(line!())
                    }
                    Dec::LetModule(id, _, dfs) => {
                        let v = crate::vm_def::def::module(
                            active,
                            ModulePath {
                                package_name: None,
                                local_path: format!("<anonymous@{}>", dec_.1),
                            },
                            &id,
                            dec_.1.clone(),
                            None,
                            None,
                            &dfs,
                            None,
                        )?;
                        match id {
                            None => (),
                            Some(i) => {
                                active.env().insert(i.0.clone(), v);
                            }
                        };
                        *active.cont() = Cont::Decs(decs);
                        Ok(Step {})
                    }
                    Dec::LetImport(pattern, _, path) => {
                        let m = crate::vm_def::def::import(active, path)?;
                        let fields = crate::vm_def::module_project(active.defs(), &m, &pattern.0)?;
                        for (x, def) in fields {
                            let val = crate::vm_def::def_as_value(active.defs(), &x.0, &def)?;
                            active.env().insert(x.0.clone(), val);
                        }
                        *active.cont() = Cont::Decs(decs);
                        Ok(Step {})
                    }
                    Dec::Var(p, e) => {
                        if let Some(x) = crate::vm_match::get_pat_var(&p.0) {
                            exp_conts(active, FrameCont::Var(x.fast_clone(), Cont::Decs(decs)), e)
                        } else {
                            nyi!(line!(), "Dec::Var({:?}, _)", p)
                        }
                    }
                    Dec::Func(f) => {
                        let id = f.name.clone();
                        let v = Value::Function(ClosedFunction(Closed {
                            ctx: active.defs().active_ctx.clone(),
                            env: active.env().fast_clone(),
                            content: f.clone(),
                        }))
                        .share();
                        if decs.is_empty() {
                            *active.cont() = Cont::Value_(v);
                            Ok(Step {})
                        } else {
                            if let Some(i) = id {
                                active.env().insert(i.as_ref().data_ref().clone(), v);
                            };
                            *active.cont() = Cont::Decs(decs);
                            Ok(Step {})
                        }
                    }
                    d => nyi!(line!(), "{:?}", d),
                }
            }
        } //_ => unimplemented!(),
    }
}

fn switch<A: Active>(active: &mut A, v: Value_, cases: Cases) -> Result<Step, Interruption> {
    for case in cases.vec.into_iter() {
        if let Some(env) = crate::vm_match::pattern_matches(
            active.env().fast_clone(),
            &case.0.pat.0,
            v.fast_clone(),
        ) {
            *active.env() = env;
            *active.cont_source() = case.0.exp.1.clone();
            *active.cont() = Cont::Exp_(case.0.exp.fast_clone(), Vector::new());
            return Ok(Step {});
        }
    }
    Err(Interruption::NoMatchingCase)
}

fn bang_null<A: Active>(active: &mut A) -> Result<Step, Interruption> {
    let mut stack = active.stack().clone();
    loop {
        if let Some(fr) = stack.pop_front() {
            match fr.cont {
                FrameCont::DoOpt => {
                    *active.stack() = stack;
                    *active.cont() = cont_value(Value::Null);
                    return Ok(Step {});
                }
                _ => {}
            }
        } else {
            return Err(Interruption::NoDoQuestBangNull);
        }
    }
}

// TODO: possibly refactor to `Cont::Value(Value)` and `Cont::Value_(Value_)`
#[inline(always)]
fn cont_value(value: Value) -> Cont {
    // TODO: memoize (), true, false, null, variants, etc.
    Cont::Value_(value.share())
}

fn return_<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    let mut stack = active.stack().fast_clone();
    loop {
        if let Some(fr) = stack.pop_front() {
            match fr.cont {
                FrameCont::Call3 => {
                    active.defs().active_ctx = fr.context;
                    *active.env() = fr.env;
                    *active.stack() = stack;
                    *active.cont() = Cont::Value_(v);
                    return Ok(Step {});
                }
                _ => {}
            }
        } else {
            return Err(Interruption::MisplacedReturn);
        }
    }
}

fn stack_cont_has_redex<A: ActiveBorrow>(active: &A, v: &Value) -> Result<bool, Interruption> {
    if active.stack().is_empty() {
        Ok(false)
    } else {
        use FrameCont::*;
        let frame = active.stack().front().unwrap();
        let r = match &frame.cont {
            Respond(_) => true,
            UnOp(_) => true,
            RelOp1(_, _) => false,
            RelOp2(_, _) => true,
            BinOp1(_, _) => false,
            BinOp2(_, _) => true,
            Assign1(_) => false,
            Assign2(_) => true,
            BinAssign1(..) => false,
            BinAssign2(..) => true,
            Idx1(_) => false,
            Idx2(_) => true,
            Let(_, _) => true,
            Var(_, _) => true,
            Paren => false,
            Variant(_) => false,
            Switch(_) => true,
            Block => false,
            Decs(_) => false,
            Do => true,
            Assert => true,
            Ignore => true,
            Tuple(_, _) => false,
            Array(..) => false,
            Object(..) => false,
            Annot(..) => false,
            Proj(..) => true,
            Dot(..) => true,
            Debug => false,
            If(_, _) => true,
            While1(_, _) => true,
            While2(_, _) => false,
            For1(_, _) => false,
            For2(_, _, _) => true,
            For3(_, _, _) => false,
            ForOpaqueIter(_, _, _) => true,
            And1(_) => false,
            And2 => true,
            Or1(_) => match v {
                Value::Bool(b) => *b,
                _ => true,
            },
            Or2 => true,
            Not => true,
            Opt => false,
            DoOpt => false,
            Bang => true,
            Call1(..) => false,
            Call2(..) => true,
            Call3 => false,
            Return => true,
            //_ => return nyi!(line!()),
        };
        Ok(r)
    }
}

// continue execution using the top-most stack frame, if any.
fn stack_cont<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    if active.stack().is_empty() {
        *active.cont() = Cont::Value_(v.fast_clone());
        Err(Interruption::Done(v))
    } else if let Some(&Frame {
        cont: FrameCont::ForOpaqueIter(ref pat, ref ptr, ref body),
        ..
    }) = active.stack().front()
    {
        let pat = pat.fast_clone();
        let ptr = ptr.fast_clone();
        let body = body.fast_clone();
        let env = active.stack().front().unwrap().env.fast_clone();
        /* fast-path: avoid popping top stack frame. */
        match &*v {
            Value::Unit => (),
            _ => type_mismatch!(file!(), line!()),
        };
        match opaque_iter_next(active, &ptr)? {
            None => {
                active.stack().pop_front();
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            Some(v_) => {
                if let Some(env) = crate::vm_match::pattern_matches(env, &pat.0, v_.fast_clone()) {
                    *active.env() = env;
                    exp_cont(active, &body)
                } else {
                    type_mismatch!(file!(), line!())
                }
            }
        }
    } else {
        // common cases: need to pop top stack frame, then pattern-match it.
        nonempty_stack_cont(active, v)
    }
}

fn nonempty_stack_cont<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    use FrameCont::*;
    let frame = active.stack().pop_front().unwrap();
    match &frame.cont {
        Decs(_) => { /* decs in same block share an environment. */ }
        _ => {
            *active.env() = frame.env;
        }
    }
    active.defs().active_ctx = frame.context;
    *active.cont_prim_type() = frame.cont_prim_type;
    *active.cont_source() = frame.source;
    match frame.cont {
        ForOpaqueIter(..) => unreachable!(),
        Respond(target) => Err(Interruption::Response(Response { target, value: v })),
        UnOp(un) => {
            *active.cont() = cont_value(crate::vm_ops::unop(un, v)?);
            Ok(Step {})
        }
        RelOp1(relop, e2) => exp_conts(active, RelOp2(v, relop), &e2),
        RelOp2(v1, rel) => {
            let v = crate::vm_ops::relop(&active.cont_prim_type(), rel, v1, v)?;
            *active.cont() = cont_value(v);
            Ok(Step {})
        }
        BinOp1(binop, e2) => exp_conts(active, BinOp2(v, binop), &e2),
        BinOp2(v1, bop) => {
            let v = crate::vm_ops::binop(&active.cont_prim_type(), bop, v1, v)?;
            *active.cont() = cont_value(v);
            Ok(Step {})
        }
        Assign1(e2) => exp_conts(active, Assign2(v), &e2),
        BinAssign1(b, e2) => exp_conts(active, BinAssign2(v, b), &e2),
        Assign2(v1) => match &*v1 {
            Value::Pointer(p) => {
                active.store().mutate(p.clone(), v)?;
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            Value::Index(p, i) => {
                active.store().mutate_index(p.clone(), i.fast_clone(), v)?;
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        BinAssign2(v1, bop) => {
            let v1d = match &*v1 {
                Value::Pointer(p) => active.deref(p)?,
                x => {
                    return nyi!(
                        line!(),
                        "BinAssign2: expected Value::Pointer, but got {:?}",
                        x
                    )
                }
            };
            let v3 = crate::vm_ops::binop(&active.cont_prim_type(), bop, v1d.clone(), v.clone())?;
            match &*v1 {
                Value::Pointer(p) => {
                    active.store().mutate(p.clone(), v3.share())?;
                    *active.cont() = cont_value(Value::Unit);
                    Ok(Step {})
                }
                _ => return nyi!(line!()),
            }
        }
        Idx1(e2) => exp_conts(active, Idx2(v), &e2),
        Idx2(v1) => {
            if let Some(Frame {
                cont: FrameCont::Assign1(_), // still need to evaluate RHS of assignment.
                ..
            }) = active.stack().get(0)
            {
                match &*v1 {
                    Value::Pointer(p) => {
                        // save array pointer and offset until after RHS is evaluated.
                        *active.cont() = cont_value(Value::Index(p.clone(), v.fast_clone()));
                        Ok(Step {})
                    }
                    Value::Dynamic(_) => Err(Interruption::Other(
                        "Dynamic Rust value without a pointer".to_string(),
                    )),
                    _ => type_mismatch!(file!(), line!()),
                }
            } else {
                let v1 = active.deref_value(v1)?;
                match (&*v1, &*v) {
                    (Value::Array(_mut, a), Value::Nat(i)) => {
                        let i = crate::value::usize_from_biguint(i)?;
                        *active.cont() =
                            cont_value((**a.get(i).ok_or(Interruption::IndexOutOfBounds)?).clone());
                        Ok(Step {})
                    }
                    (Value::Dynamic(d), _) => {
                        *active.cont() =
                            cont_value((*d.dynamic().get_index(active.store(), v)?).clone());
                        Ok(Step {})
                    }
                    _ => type_mismatch!(file!(), line!()),
                }
            }
        }
        Let(p, cont) => {
            if let Some(env) =
                crate::vm_match::pattern_matches(active.env().clone(), p.as_ref().data_ref(), v)
            {
                *active.env() = env;
                *active.cont_source() = crate::vm_types::source_from_cont(&cont);
                *active.cont() = cont;
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        Var(x, cont) => {
            let ptr = active.alloc(v);
            active
                .env()
                .insert(x.as_ref().data_ref().clone(), Value::Pointer(ptr).share());
            *active.cont_source() = crate::vm_types::source_from_cont(&cont);
            *active.cont() = cont;
            Ok(Step {})
        }
        Paren => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Variant(i) => {
            *active.cont() = cont_value(Value::Variant(i.0.clone(), Some(v)));
            Ok(Step {})
        }
        Switch(cases) => switch(active, v, cases),
        Block => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Decs(decs) => {
            match decs.front() {
                None => {
                    // return final value from block.
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                }
                Some(_) => {
                    *active.cont() = Cont::Decs(decs);
                    Ok(Step {})
                }
            }
        }
        Do => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Assert => match &*v {
            Value::Bool(true) => {
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            Value::Bool(false) => Err(Interruption::AssertionFailure),
            _ => type_mismatch!(file!(), line!()),
        },
        Ignore => {
            *active.cont() = cont_value(Value::Unit);
            Ok(Step {})
        }
        Tuple(mut done, mut rest) => {
            done.push_back(v);
            match rest.pop_front() {
                None => {
                    *active.cont() = cont_value(Value::Tuple(done));
                    Ok(Step {})
                }
                Some(next) => exp_conts(active, Tuple(done, rest), &next),
            }
        }
        Array(mut_, mut done, mut rest) => {
            done.push_back(v);
            match rest.pop_front() {
                None => {
                    if let Mut::Const = mut_ {
                        *active.cont() = cont_value(Value::Array(mut_, done));
                        Ok(Step {})
                    } else {
                        let arr = Value::Array(mut_, done);
                        let ptr = active.alloc(arr.share());
                        *active.cont() = cont_value(Value::Pointer(ptr));
                        Ok(Step {})
                    }
                }
                Some(next) => exp_conts(active, Array(mut_, done, rest), &next),
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
                        let id = f.id.0.clone();
                        let val = match f.mut_ {
                            Mut::Const => f.val,
                            Mut::Var => Value::Pointer(active.alloc(f.val)).share(),
                        };
                        hm.insert(id, crate::value::FieldValue { mut_: f.mut_, val });
                    }
                    *active.cont() = cont_value(Value::Object(hm));
                    Ok(Step {})
                }
                Some(next) => exp_conts(
                    active,
                    Object(
                        done,
                        FieldContext {
                            mut_: next.0.mut_.clone(),
                            id: next.0.id.fast_clone(),
                            typ: next.0.typ.fast_clone(),
                        },
                        rest,
                    ),
                    &next.0.exp_(),
                ),
            }
        }
        Annot(_t) => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Proj(i) => match &*v {
            Value::Tuple(vs) => {
                if let ProjIndex::Usize(i) = i {
                    if let Some(vi) = vs.get(i) {
                        *active.cont() = Cont::Value_(vi.fast_clone());
                        Ok(Step {})
                    } else {
                        type_mismatch!(file!(), line!())
                    }
                } else {
                    nyi!(line!())
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Dot(f) => match &*v {
            Value::Object(fs) => {
                if let Some(f) = fs.get(&f.0) {
                    *active.cont() = Cont::Value_(f.val.fast_clone());
                    Ok(Step {})
                } else {
                    type_mismatch!(file!(), line!())
                }
            }
            Value::Module(m) => {
                let fd = crate::vm_def::resolve_def(active.defs(), &m.fields, true, &f.0)?;
                let v = crate::vm_def::def_as_value(active.defs(), &f.0, &fd.def)?;
                *active.cont() = Cont::Value_(v);
                Ok(Step {})
            }
            Value::Dynamic(d) => {
                let f = d.dynamic().get_field(active.store(), f.0.as_str())?;
                *active.cont() = Cont::Value_(f);
                Ok(Step {})
            }
            Value::Actor(a) => {
                // to do -- get defs from actor n
                // look up definition for f
                // is it a public function?
                // if not public, give error.
                // if not available, type mismatch.
                *active.cont() = Cont::Value_(
                    Value::ActorMethod(ActorMethod {
                        actor: a.id.clone(),
                        method: f.0.clone(),
                    })
                    .share(),
                );
                // do projection, representing function with special value.
                Ok(Step {})
            }
            v => Err(type_mismatch_!(
                file!(),
                line!(),
                "dot-operator-is-matching-operand",
                format!("{:?} @ {}", v, active.cont_source())
            )),
        },
        Debug => match &*v {
            Value::Unit => {
                *active.cont() = Cont::Value_(v);
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        If(e2, e3) => match &*v {
            Value::Bool(b) => {
                *active.cont() = if *b {
                    Cont::Exp_(e2, Vector::new())
                } else {
                    match e3 {
                        Some(e3) => Cont::Exp_(e3, Vector::new()),
                        None => cont_value(Value::Unit),
                    }
                };
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        While1(e1, e2) => match &*v {
            Value::Bool(b) => {
                if *b {
                    exp_conts(active, FrameCont::While2(e1, e2.fast_clone()), &e2)
                } else {
                    *active.cont() = cont_value(Value::Unit);
                    Ok(Step {})
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        While2(e1, e2) => match &*v {
            Value::Unit => exp_conts(active, FrameCont::While1(e1.fast_clone(), e2), &e1),
            _ => type_mismatch!(file!(), line!()),
        },
        For1(p, body) => {
            /* for-loop state: iterator object is value v */
            match &*v {
                Value::Opaque(ptr) => {
                    /* enter ForOpaqueIter loop (a "fast path"):
                    no need to evaluate general Motoko code for iterator. */
                    let env = active.env().fast_clone();
                    let source = active.cont_source().clone();
                    let context = active.defs().active_ctx.clone();

                    active.stack().push_front(Frame {
                        context,
                        env,
                        cont: FrameCont::ForOpaqueIter(p, ptr.clone(), body),
                        cont_prim_type: None,
                        source,
                    });
                    *active.cont() = cont_value(Value::Unit);
                    Ok(Step {})
                }
                _ => cont_for_call_dot_next(active, p, v, body),
            }
        }
        For2(p, v_iter, body) => match &*v {
            Value::Null => {
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            Value::Option(v_) => {
                if let Some(env) = crate::vm_match::pattern_matches(
                    active.env().fast_clone(),
                    &p.0,
                    v_.fast_clone(),
                ) {
                    *active.env() = env;
                    exp_conts(active, FrameCont::For3(p, v_iter, body.fast_clone()), &body)
                } else {
                    type_mismatch!(file!(), line!())
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        For3(p, v_iter, body) => match &*v {
            Value::Unit => cont_for_call_dot_next(active, p, v_iter, body),
            _ => type_mismatch!(file!(), line!()),
        },
        And1(e2) => match &*v {
            Value::Bool(b) => {
                if *b {
                    exp_conts(active, FrameCont::And2, &e2)
                } else {
                    *active.cont() = cont_value(Value::Bool(false));
                    Ok(Step {})
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        And2 => match &*v {
            Value::Bool(b) => {
                *active.cont() = cont_value(Value::Bool(*b));
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Or1(e2) => match &*v {
            Value::Bool(b) => {
                if *b {
                    *active.cont() = cont_value(Value::Bool(true));
                    Ok(Step {})
                } else {
                    exp_conts(active, FrameCont::Or2, &e2)
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Or2 => match &*v {
            Value::Bool(b) => {
                *active.cont() = cont_value(Value::Bool(*b));
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Not => match &*v {
            Value::Bool(b) => {
                *active.cont() = cont_value(Value::Bool(!b));
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Opt => {
            *active.cont() = cont_value(Value::Option(v));
            Ok(Step {})
        }
        DoOpt => {
            *active.cont() = cont_value(Value::Option(v));
            Ok(Step {})
        }
        Bang => match &*v {
            Value::Option(v) => {
                *active.cont() = Cont::Value_(v.fast_clone());
                Ok(Step {})
            }
            Value::Null => bang_null(active),
            _ => type_mismatch!(file!(), line!()),
        },
        Call1(inst, e2) => exp_conts(active, FrameCont::Call2(v, inst), &e2),
        Call2(f, inst) => call_cont(active, f, inst, v),
        Call3 => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Return => return_(active, v),
    }
}

// Returns `Some(span)` if the limits include the breakpoint.
fn check_for_breakpoint<A: ActiveBorrow>(active: &A, limits: &Limits) -> Option<Breakpoint> {
    let cont_span = &active.cont_source().span();
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

fn check_for_redex<A: ActiveBorrow>(active: &A, limits: &Limits) -> Result<usize, Interruption> {
    let mut redex_bump = 0;
    if let Cont::Value_(ref v) = active.cont() {
        if stack_cont_has_redex(active, v)? {
            redex_bump = 1;
            if let Some(redex_limit) = limits.redex {
                if active.counts().redex >= redex_limit {
                    // if =, adding 1 will exceed limit, so do not.
                    return Err(Interruption::Limit(Limit::Redex));
                }
            }
        }
    }
    Ok(redex_bump)
}

pub fn active_step<A: Active>(active: &mut A, limits: &Limits) -> Result<Step, Interruption> {
    /* to do -- check for pending send. */
    if let Some(break_span) = check_for_breakpoint(active, limits) {
        return Err(Interruption::Breakpoint(break_span));
    }
    if let Some(step_limit) = limits.step {
        if active.counts().step >= step_limit {
            return Err(Interruption::Limit(Limit::Step));
        }
    }
    let redex_bump = check_for_redex(active, limits)?;
    let ret = active_step_(active)?;
    active.counts().step += 1;
    active.counts().redex += redex_bump;
    Ok(ret)
}

fn active_trace<A: ActiveBorrow>(active: &A) {
    use log::trace;
    trace!(
        "# {:?} step {} (redex {})",
        active.schedule_choice(),
        active.counts().step,
        active.counts().redex
    );
    trace!(" - cont = {:?}", active.cont());
    trace!("   - cont_source = {:?}", active.cont_source());
    trace!("   - env = {:?}", active.env());
    trace!(" - stack = {:#?}", active.stack());
    trace!(" - store = {:#?}", active.store());
    trace!(" - defs  = {:#?}", active.defs());
}

mod collection {
    pub mod fastranditer {
        use super::super::*;
        use crate::{shared::Share, value::Collection};

        pub fn new<A: Active>(
            active: &mut A,
            _targs: Option<Inst>,
            v: Value_,
        ) -> Result<Step, Interruption> {
            if let Some(args) =
                crate::vm_match::pattern_matches_temps(&crate::vm_match::pattern::temps(2), v)
            {
                let size: Option<u32> = crate::vm_match::assert_value_is_option_u32(&args[0])?;
                let seed: u32 = crate::vm_match::assert_value_is_u32(&args[1])?;
                let ptr = active.alloc(
                    Value::Collection(Collection::FastRandIter(FastRandIter::new(size, seed)))
                        .share(),
                );
                *active.cont() = cont_value(Value::Opaque(ptr));
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }

        pub fn next<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            let ptr = crate::vm_match::assert_value_is_opaque_pointer(&v)?;
            match &*active.deref(&ptr)? {
                Value::Collection(Collection::FastRandIter(fri)) => {
                    let mut fri = fri.clone();
                    let n = match fri.next() {
                        Some(n) => Value::Option(n.share()),
                        None => Value::Null,
                    };
                    let i = Value::Collection(Collection::FastRandIter(fri));
                    active.store().mutate(ptr, i.share())?;
                    *active.cont() = cont_value(n);
                    Ok(Step {})
                }
                _ => type_mismatch!(file!(), line!()),
            }
        }
    }

    pub mod hashmap {
        use super::super::*;
        use crate::value::Collection;
        use im_rc::vector;

        pub fn new<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(_) = crate::vm_match::pattern_matches_temps(&Pat::Literal(Literal::Unit), v)
            {
                *active.cont() = cont_value(Value::Collection(Collection::HashMap(HashMap::new())));
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        pub fn put<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(args) =
                crate::vm_match::pattern_matches_temps(&crate::vm_match::pattern::temps(3), v)
            {
                let hm = &args[0];
                let k = &args[1];
                let v = &args[2];
                let (hm, old) = {
                    if let Value::Collection(Collection::HashMap(mut hm)) = hm.get() {
                        match hm.insert(k.fast_clone(), v.fast_clone()) {
                            None => (hm, Value::Null),
                            Some(old) => (hm, Value::Option(old)),
                        }
                    } else {
                        type_mismatch!(file!(), line!());
                    }
                };
                // Note for later: immutable map updates are adding extra overhead here
                // We could probably just tolerate this and use `Dynamic` values for performance-critical situations
                let hm = Value::Collection(Collection::HashMap(hm));
                let ret = Value::Tuple(vector![hm.share(), old.share()]);
                *active.cont() = cont_value(ret);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        pub fn get<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(args) =
                crate::vm_match::pattern_matches_temps(&crate::vm_match::pattern::temps(2), v)
            {
                let hm = &args[0];
                let k = &args[1];
                let ret = {
                    if let Value::Collection(Collection::HashMap(hm)) = hm.get() {
                        match hm.get(k) {
                            None => Value::Null,
                            Some(v) => Value::Option(v.fast_clone()),
                        }
                    } else {
                        type_mismatch!(file!(), line!());
                    }
                };
                *active.cont() = cont_value(ret);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        pub fn remove<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(args) =
                crate::vm_match::pattern_matches_temps(&crate::vm_match::pattern::temps(2), v)
            {
                let hm = &args[0];
                let k = &args[1];
                let (hm, old) = {
                    if let Value::Collection(Collection::HashMap(mut hm)) = hm.get() {
                        match hm.remove(k) {
                            None => (hm, Value::Null),
                            Some(v) => (hm, Value::Option(v)),
                        }
                    } else {
                        type_mismatch!(file!(), line!());
                    }
                };
                let hm = Value::Collection(Collection::HashMap(hm));
                let ret = Value::Tuple(vector![hm.share(), old.share()]);
                *active.cont() = cont_value(ret);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
    }
}

fn opaque_iter_next<A: Active>(
    active: &mut A,
    p: &Pointer,
) -> Result<Option<Value_>, Interruption> {
    use crate::value::Collection;
    let iter_value = active.deref(p)?;
    // dispatch based on iterator value (as opposed to primitive function being given in source).
    // one case for each inbuilt iterator value.
    // to do -- integrate "dynamic" iterators too
    match &*iter_value {
        Value::Collection(Collection::FastRandIter(fri)) => {
            let mut fri = fri.clone();
            let n = fri.next();
            active.store().mutate(
                p.clone(),
                Value::Collection(Collection::FastRandIter(fri)).share(),
            )?;
            Ok(n.map(|v| v.share()))
        }
        _ => type_mismatch!(file!(), line!()),
    }
}

fn cont_for_call_dot_next<A: Active>(
    active: &mut A,
    p: Pat_,
    v: Value_,
    body: Exp_,
) -> Result<Step, Interruption> {
    let deref_v = active.deref_value(v.fast_clone())?; // Only used for `Dynamic` case
    match &*deref_v {
        Value::Dynamic(d) => {
            let env = active.env().fast_clone();
            let source = active.cont_source().clone();
            let context = active.defs().active_ctx.clone();
            active.stack().push_front(Frame {
                context,
                env,
                cont: FrameCont::For2(p, v, body),
                cont_prim_type: None,
                source,
            });
            *active.cont() = Cont::Value_(d.dynamic_mut().iter_next(active.store())?);
            Ok(Step {})
        }
        _ => {
            let v_next_func = v.get_field_or("next", type_mismatch_!(file!(), line!()))?;
            let env = active.env().fast_clone();
            let source = active.cont_source().clone();
            let context = active.defs().active_ctx.clone();
            active.stack().push_front(Frame {
                context,
                env,
                cont: FrameCont::For2(p, v, body),
                cont_prim_type: None,
                source,
            });
            call_cont(active, v_next_func, None, Value::Unit.share())
        }
    }
}

fn call_prim_function<A: Active>(
    active: &mut A,
    pf: &PrimFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use PrimFunction::*;
    match pf {
        AtSignVar(v) => nyi!(line!(), "call_prim_function({})", v),
        DebugPrint => match &*args {
            Value::Text(s) => {
                let schedule_choice = active.schedule_choice().clone();
                log::info!(
                    "DebugPrint: {:?}, {}: {:?}",
                    schedule_choice,
                    active.cont_source(),
                    s
                );

                active.debug_print_out().push_back(DebugPrintLine {
                    text: s.clone(),
                    schedule_choice,
                });
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            v => {
                let txt = crate::value::string_from_value(v)?;
                let schedule_choice = active.schedule_choice().clone();
                log::info!(
                    "DebugPrint: {:?}: {}: {:?}",
                    schedule_choice,
                    active.cont_source(),
                    txt
                );
                let schedule_choice = active.schedule_choice().clone();
                active.debug_print_out().push_back(DebugPrintLine {
                    text: crate::value::Text::from(txt),
                    schedule_choice,
                });
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
        },
        NatToText => match &*args {
            Value::Nat(n) => {
                *active.cont() = cont_value(Value::Text(format!("{}", n).into()));
                Ok(Step {})
            }
            v => {
                *active.cont() = cont_value(Value::Text(format!("{:?}", v).into()));
                Ok(Step {})
            }
        },
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "value-reflection")]
        ReifyValue => {
            use crate::value::ToMotoko;
            *active.cont() = cont_value(args.to_motoko().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "value-reflection")]
        ReflectValue => {
            // active.cont = cont_value(args.to_rust::<Value>().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "active-reflection")]
        ReifyActive => {
            use crate::value::ToMotoko;
            *active.cont() = cont_value(active.to_motoko().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "active-reflection")]
        ReflectActive => {
            *active = args.to_rust::<Active>().map_err(Interruption::ValueError)?;
            Ok(Step {})
        }
        Collection(cf) => call_collection_function(active, cf, targs, args),
    }
}

fn call_collection_function<A: Active>(
    active: &mut A,
    cf: &CollectionFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use CollectionFunction::*;
    match cf {
        HashMap(hmf) => call_hashmap_function(active, hmf, targs, args),
        FastRandIter(frif) => call_fastranditer_function(active, frif, targs, args),
    }
}

fn call_fastranditer_function<A: Active>(
    active: &mut A,
    frif: &FastRandIterFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use FastRandIterFunction::*;
    match frif {
        New => collection::fastranditer::new(active, targs, args),
        Next => collection::fastranditer::next(active, args),
    }
}

fn call_hashmap_function<A: Active>(
    active: &mut A,
    hmf: &HashMapFunction,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use HashMapFunction::*;
    match hmf {
        New => collection::hashmap::new(active, args),
        Put => collection::hashmap::put(active, args),
        Get => collection::hashmap::get(active, args),
        Remove => collection::hashmap::remove(active, args),
    }
}

pub fn call_function_def<A: Active>(
    active: &mut A,
    actor_env: Env,
    fndef: &FunctionDef,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    if let Some(env_) = crate::vm_match::pattern_matches(actor_env, &fndef.function.input.0, args) {
        let source = active.cont_source().clone();
        let env_saved = active.env().fast_clone();
        *active.env() = env_;
        fndef.function.name.fast_clone().map(|f| {
            active
                .env()
                .insert(f.0.clone(), fndef.rec_value.fast_clone())
        });
        active.defs().active_ctx = fndef.context.clone();
        *active.cont() = Cont::Exp_(fndef.function.exp.fast_clone(), Vector::new());
        let context = active.defs().active_ctx.clone();
        active.stack().push_front(Frame {
            context,
            source,
            env: env_saved,
            cont: FrameCont::Call3,
            cont_prim_type: None, /* to do */
        }); // to match with Return, if any.
        Ok(Step {})
    } else {
        type_mismatch!(file!(), line!())
    }
}

fn call_function<A: Active>(
    active: &mut A,
    value: Value_,
    cf: &ClosedFunction,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    if let Some(env_) =
        crate::vm_match::pattern_matches(cf.0.env.fast_clone(), &cf.0.content.input.0, args)
    {
        let source = active.cont_source().clone();
        let env_saved = active.env().fast_clone();
        *active.env() = env_;
        cf.0.content
            .name
            .fast_clone()
            .map(|f| active.env().insert(f.0.clone(), value));
        *active.cont() = Cont::Exp_(cf.0.content.exp.fast_clone(), Vector::new());
        let context = active.defs().active_ctx.clone();
        active.defs().active_ctx = cf.0.ctx.clone();
        active.stack().push_front(Frame {
            context,
            source,
            env: env_saved,
            cont: FrameCont::Call3,
            cont_prim_type: None, /* to do */
        }); // to match with Return, if any.
        Ok(Step {})
    } else {
        type_mismatch!(file!(), line!())
    }
}

fn call_cont<A: Active>(
    active: &mut A,
    func_value: Value_,
    inst: Option<Inst>,
    args_value: Value_,
) -> Result<Step, Interruption> {
    match &*func_value {
        Value::Function(cf) => call_function(active, func_value.fast_clone(), cf, inst, args_value),
        Value::PrimFunction(pf) => call_prim_function(active, pf, inst, args_value),
        _ => {
            let func_value = active.deref_value(func_value)?; // Account for dynamic value pointers
            match &*func_value {
                Value::Dynamic(d) => {
                    let result =
                        d.dynamic_mut()
                            .call(active.store(), &inst, args_value.fast_clone())?;
                    *active.cont() = Cont::Value_(result);
                    Ok(Step {})
                }
                Value::ActorMethod(am) => Err(Interruption::Send(am.clone(), inst, args_value)),
                _ => type_mismatch!(file!(), line!()),
            }
        }
    }
}

pub fn exp_conts_<A: Active>(
    active: &mut A,
    source: Source,
    frame_cont: FrameCont,
    cont: Cont,
    cont_source: Source,
) -> Result<Step, Interruption> {
    let env = active.env().fast_clone();
    let cont_prim_type = active.cont_prim_type().clone();
    let context = active.defs().active_ctx.clone();
    active.stack().push_front(Frame {
        context,
        env,
        cont: frame_cont,
        cont_prim_type,
        source,
    });
    *active.cont() = cont;
    *active.cont_source() = cont_source;
    Ok(Step {})
}

/* continuation separates into stack frame cont and immediate cont. */
pub fn exp_conts<A: Active>(
    active: &mut A,
    frame_cont: FrameCont,
    cont: &Exp_,
) -> Result<Step, Interruption> {
    let cont_source = cont.1.clone();
    exp_conts_(
        active,
        cont_source.clone(),
        frame_cont,
        Cont::Exp_(cont.fast_clone(), Vector::new()),
        cont_source,
    )
}

/* continuation uses same stack frame. */
fn exp_cont<A: Active>(active: &mut A, cont: &Exp_) -> Result<Step, Interruption> {
    *active.cont_source() = cont.1.clone();
    *active.cont() = Cont::Exp_(cont.fast_clone(), Vector::new());
    Ok(Step {})
}
