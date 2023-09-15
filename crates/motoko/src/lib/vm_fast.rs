use crate::ast::{
    Cases, Dec_, Decs, Exp, Exp_, Inst, Literal, Mut, Pat, Pat_, ProjIndex, Source, Type,
};
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

fn eval_exp_<A: Active>(active: &mut A, exp: &Exp_) -> Result<Value_, Interruption> {
    use Exp::*;
    match &exp.0 {
        Literal(l) => Ok(Value::from_literal(l)
            .map_err(Interruption::ValueError)?
            .into()),
        Var(x) => {
            todo!()
        }
        Paren(e) => eval_exp_(active, e),
        Ignore(e) => {
            todo!()
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
            todo!()
        }
        Assign(e1, e2) => {
            todo!()
        }
        Proj(e1, pi) => {
            todo!()
        }
        Call(e1, _, e2) => {
            todo!()
        }
        Block(decs) => eval_decs(active, decs),
        _ => nyi!(line!()),
    }
}

fn eval_decs<A: Active>(active: &mut A, decs: &Decs) -> Result<Value_, Interruption> {
    todo!()
}

fn eval_dec_<A: Active>(active: &mut A, dec: &Dec_) -> Result<Value_, Interruption> {
    todo!()
}
