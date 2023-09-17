use crate::ast::{
    Cases, Dec, Dec_, Decs, Exp, Exp_, Inst, Literal, Mut, Pat, Pat_, Prog, ProjIndex, Source, Type,
};
use crate::shared::{FastClone, Share};
use crate::value::{
    ActorId, ActorMethod, Closed, ClosedFunction, CollectionFunction, FastRandIter,
    FastRandIterFunction, HashMapFunction, PrimFunction, Value, Value_,
};
use crate::vm_types::{
    def::{Def, Field as FieldDef, Function as FunctionDef},
    stack::{FastInfo, FieldContext, FieldValue, Frame, FrameCont},
    Active, ActiveBorrow, Breakpoint, Cont, DebugPrintLine, Env, Interruption, Limit, Limits,
    ModulePath, Pointer, Response, Step,
};
use im_rc::{HashMap, Vector};

use crate::{nyi, type_mismatch, type_mismatch_};

fn begin<A: Active>(active: &mut A, assignment_lhs: bool) {
    let env = active.env().fast_clone();
    let source = active.cont_source().clone();
    let context = active.defs().active_ctx.clone();
    // to do -- Set current cont to Hole? (connote no meaningful expression in that slot)
    active.stack().push_front(Frame {
        context,
        env,
        cont: FrameCont::Fast(FastInfo { assignment_lhs }),
        cont_prim_type: None,
        source,
    })
}

fn end<A: Active>(active: &mut A) {
    match active.stack().pop_front() {
        Some(Frame {
            cont: FrameCont::Fast(_),
            ..
        }) => (),
        _ => unreachable!(),
    }
}

fn info<'a, A: Active>(active: &'a A) -> &'a FastInfo {
    match active.stack().front() {
        Some(Frame {
            cont: FrameCont::Fast(info),
            ..
        }) => info,
        _ => unreachable!(),
    }
}

fn assign<A: Active>(active: &mut A, v1: &Value_, v2: &Value_) -> Result<Value_, Interruption> {
    let r = match &**v1 {
        Value::Pointer(p) => active.store().mutate(p.clone(), v2.fast_clone()),
        Value::Index(p, i) => {
            active
                .store()
                .mutate_index(p.clone(), i.fast_clone(), v2.fast_clone())
        }
        _ => type_mismatch!(file!(), line!()),
    };
    r?;
    Ok(Value::Unit.share())
}

fn deref<A: Active>(active: &mut A, v: &Value_) -> Result<Value_, Interruption> {
    match &**v {
        Value::Pointer(p) => active.deref(p),
        _ => type_mismatch!(file!(), line!()),
    }
}

fn call<A: Active>(
    active: &mut A,
    v1: &Value_,
    inst: &Option<Inst>,
    v2: &Value_,
) -> Result<Value_, Interruption> {
    match &**v1 {
        Value::Function(cf) => {
            if let Some(env_) = crate::vm_match::pattern_matches(
                cf.0.env.fast_clone(),
                &cf.0.content.input.0,
                v2.fast_clone(),
            ) {
                // Push stack.
                begin(active, false);
                // Bind variables.
                *active.env() = env_;
                // Do recursive function binding.
                cf.0.content
                    .name
                    .fast_clone()
                    .map(|f| active.env().insert(f.0.clone(), v1.fast_clone()));
                // Use static bindings from the lexical scope of the function def.
                active.defs().active_ctx = cf.0.ctx.clone();
                // Run the body of the function.
                let res = eval_exp_(active, &cf.0.content.exp)?;
                // Pop stack.
                end(active);
                Ok(res)
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        Value::PrimFunction(pf) => nyi!(line!()),
        _ => {
            let func_value = active.deref_value(v1.fast_clone())?; // Account for dynamic value pointers
            match &*func_value {
                Value::Dynamic(d) => d.dynamic_mut().call(active.store(), inst, v2.fast_clone()),
                _ => type_mismatch!(file!(), line!()),
            }
        }
    }
}

fn eval_value_<A: Active>(active: &mut A, v: &Value_) -> Result<Value_, Interruption> {
    // We implicitly deref store pointers when they are evaluated
    // outside the context of an assignment LHS.  When on the LHS of
    // an assignment, their evaluation retains their pointer identity,
    // in preparation for being re-assigned a new value.
    match &**v {
        Value::Pointer(p) => {
            if info(active).assignment_lhs {
                Ok(v.fast_clone())
            } else {
                active.deref(p)
            }
        }
        _ => Ok(v.fast_clone()),
    }
}

pub fn eval_decs<A: Active>(active: &mut A, decs: &Decs) -> Result<Value_, Interruption> {
    begin(active, false);
    let r = eval_decs_(active, decs);
    end(active);
    r
}

pub fn eval_decs_<A: Active>(active: &mut A, decs: &Decs) -> Result<Value_, Interruption> {
    let mut ret = Value::Unit.into();
    for dec in decs.vec.iter() {
        ret = eval_dec_(active, dec)?;
    }
    Ok(ret)
}

pub fn eval_exp<A: Active>(active: &mut A, exp: &Exp_) -> Result<Value_, Interruption> {
    begin(active, false);
    let r = eval_exp_(active, exp);
    end(active);
    r
}

fn eval_exp_<A: Active>(active: &mut A, exp: &Exp_) -> Result<Value_, Interruption> {
    use Exp::*;
    match &exp.0 {
        Value_(v) => Ok(v.fast_clone()),
        Literal(l) => Ok(Value::from_literal(l)
            .map_err(Interruption::ValueError)?
            .into()),
        Var(x) => {
            let vo = active.env().get(x).map(|v| v.clone());
            match vo {
                Some(v) => eval_value_(active, &v),
                None => {
                    if x.string.starts_with("@") {
                        let f = crate::value::PrimFunction::AtSignVar(x.to_string());
                        Ok(Value::PrimFunction(f).share())
                    } else {
                        let ctx = active.defs().active_ctx.clone();
                        let fd = crate::vm_def::resolve_def(active.defs(), &ctx, false, x)?;
                        crate::vm_def::def_as_value(active.defs(), x, &fd.def)
                    }
                }
            }
        }
        Tuple(es) => {
            let mut vs = Vector::new();
            for e in es.vec.iter() {
                vs.push_back(eval_exp_(active, e)?)
            }
            Ok(Value::Tuple(vs).into())
        }
        Paren(e) => eval_exp_(active, e),
        Ignore(e) => {
            drop(eval_exp_(active, e)?);
            Ok(Value::Unit.into())
        }
        Bin(e1, binop, e2) => {
            let v1 = eval_exp_(active, e1)?;
            let v2 = eval_exp_(active, e2)?;
            Ok(crate::vm_ops::binop(&None, binop.clone(), v1, v2)?.into())
        }
        Rel(e1, relop, e2) => {
            let v1 = eval_exp_(active, e1)?;
            let v2 = eval_exp_(active, e2)?;
            Ok(crate::vm_ops::relop(&None, relop.clone(), v1, v2)?.into())
        }
        While(e1, e2) => {
            while match *eval_exp_(active, e1)? {
                Value::Bool(true) => true,
                _ => false,
            } {
                drop(eval_exp_(active, e2)?)
            }
            Ok(Value::Unit.into())
        }
        BinAssign(e1, binop, e2) => {
            begin(active, true);
            let v1 = eval_exp_(active, e1)?;
            end(active);
            let v2 = eval_exp_(active, e2)?;
            let v3 = deref(active, &v1)?;
            let v4 = crate::vm_ops::binop(&None, binop.clone(), v3, v2)?.into();
            assign(active, &v1, &v4)
        }
        Assign(e1, e2) => {
            begin(active, true);
            let v1 = eval_exp_(active, e1)?;
            end(active);
            let v2 = eval_exp_(active, e2)?;
            assign(active, &v1, &v2)
        }
        Proj(e1, pi) => {
            let v = eval_exp_(active, e1)?;
            match &*v {
                Value::Tuple(vs) => {
                    if let ProjIndex::Usize(i) = pi {
                        if let Some(v) = vs.get(*i).map(|v| (*v).clone().into()) {
                            Ok(v)
                        } else {
                            type_mismatch!(file!(), line!())
                        }
                    } else {
                        nyi!(line!())
                    }
                }
                _ => type_mismatch!(file!(), line!()),
            }
        }
        Call(e1, inst, e2) => {
            let v1 = eval_exp_(active, e1)?;
            let v2 = eval_exp_(active, e2)?;
            call(active, &v1, inst, &v2)
        }
        Block(decs) => {
            begin(active, false);
            let r = eval_decs_(active, decs);
            end(active);
            r
        }
        Dot(e, f) => {
            let v = eval_exp_(active, e)?;
            match &*v {
                Value::Object(fs) => todo!(),
                Value::Module(m) => todo!(),
                Value::Dynamic(d) => d.dynamic().get_field(active.store(), f.0.as_str()),
                v => Err(type_mismatch_!(
                    file!(),
                    line!(),
                    "dot-operator-is-matching-operand",
                    format!("{:?} @ {}", v, active.cont_source())
                )),
            }
        }
        _ => nyi!(line!()),
    }
}

fn eval_dec_<A: Active>(active: &mut A, dec: &Dec_) -> Result<Value_, Interruption> {
    use Dec::*;
    match &dec.0 {
        Exp(e) => eval_exp_(active, e),
        Var(p, e) => {
            let v = eval_exp_(active, e)?;
            if let Some(x) = crate::vm_match::get_pat_var(&p.0) {
                let ptr = active.alloc(v);
                let v = Value::Pointer(ptr).share();
                active
                    .env()
                    .insert(x.as_ref().data_ref().clone(), v.fast_clone());
                Ok(eval_value_(active, &v)?)
            } else {
                nyi!(line!(), "Dec::Var({:?}, _)", p)
            }
        }
        Let(p, e) => {
            let v = eval_exp_(active, e)?;
            if let Some(env) = crate::vm_match::pattern_matches(
                active.env().clone(), /* to do -- avoid clones here. */
                p.as_ref().data_ref(),
                v.fast_clone(),
            ) {
                *active.env() = env;
                Ok(v)
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        Func(f) => {
            let id = f.name.clone();
            let v = Value::Function(ClosedFunction(Closed {
                ctx: active.defs().active_ctx.clone(),
                env: active.env().fast_clone(),
                content: f.clone(),
            }))
            .share();
            if let Some(i) = id {
                active
                    .env()
                    .insert(i.as_ref().data_ref().clone(), v.fast_clone());
            };
            Ok(v)
        }
        _ => nyi!(line!()),
    }
}

pub fn eval(prog: &str) -> Result<Value_, Interruption> {
    let decs = crate::check::parse(prog).unwrap();
    eval_decs(&mut crate::vm_types::Core::empty(), &decs)
}
