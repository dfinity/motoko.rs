use crate::ast::{Id_, Literal, Pat, Source};
use crate::shared::{FastClone, Share};
use crate::value::{Value, Value_};
use crate::vm_types::{Env, Interruption, Pointer};
use std::vec::Vec;

use crate::type_mismatch;

// to do -- simplify (elim this nested mod)
pub mod pattern {
    use super::*;
    use crate::ast::{Delim, NodeData};

    pub fn temps(num: u16) -> Pat {
        let mut vars = vec![];
        for i in 0..num {
            vars.push(NodeData::new(Pat::TempVar(i as u16), Source::Evaluation).share())
        }
        Pat::Tuple(Delim::from(vars))
    }
}

pub fn assert_value_is_optional<'a>(v: &'a Value) -> Result<Option<&'a Value_>, Interruption> {
    match v {
        Value::Option(v) => Ok(Some(v)),
        Value::Null => Ok(None),
        _ => type_mismatch!(file!(), line!()),
    }
}

pub fn assert_value_is_u32<'a>(v: &'a Value) -> Result<u32, Interruption> {
    v.to_rust().map_err(Interruption::ValueError)
}

pub fn assert_value_is_f32<'a>(v: &'a Value) -> Result<f32, Interruption> {
    v.to_rust().map_err(Interruption::ValueError)
}

pub fn assert_value_is_f64<'a>(v: &'a Value) -> Result<f64, Interruption> {
    match v {
        Value::Float(f) => Ok((*f).into()),
        v => v.to_rust().map_err(Interruption::ValueError),
    }
}

pub fn assert_value_is_string<'a>(v: &'a Value) -> Result<String, Interruption> {
    v.to_rust().map_err(Interruption::ValueError)
}

pub fn assert_value_is_option_u32<'a>(v: &'a Value) -> Result<Option<u32>, Interruption> {
    match assert_value_is_optional(v)? {
        None => Ok(None),
        Some(v) => Ok(Some(assert_value_is_u32(v)?)),
    }
}

pub fn assert_value_is_opaque_pointer(v: &Value) -> Result<Pointer, Interruption> {
    match v {
        Value::Opaque(p) => Ok(p.clone()),
        _ => type_mismatch!(file!(), line!()),
    }
}

pub fn match_tuple(size: u16, v: Value_) -> Result<Vec<Value_>, Interruption> {
    if size == 0 && &*v == &Value::Unit {
        return Ok(vec![]);
    } else {
        match pattern_matches_temps(&pattern::temps(size), v) {
            Some(v) => Ok(v),
            None => type_mismatch!(file!(), line!()),
        }
    }
}

pub fn pattern_matches_temps(pat: &Pat, v: Value_) -> Option<Vec<Value_>> {
    pattern_matches_temps_(pat, v, vec![])
}

// TODO: see whether it's possible to return something like `&'a Option<Env>` to reduce cloning
// (since this has more of a performance impact than `fast_clone()`)
fn pattern_matches_temps_(pat: &Pat, v: Value_, mut out: Vec<Value_>) -> Option<Vec<Value_>> {
    match (pat, &*v) {
        (Pat::Wild, _) => Some(out),
        (Pat::Annot(_), _) => Some(out),
        (Pat::Literal(Literal::Unit), Value::Unit) => Some(out),
        (Pat::Paren(p), _) => pattern_matches_temps_(&p.0, v, out),
        (Pat::AnnotPat(p, _), _) => pattern_matches_temps_(&p.0, v, out),
        (Pat::Var(_x), _) => {
            unreachable!()
        }
        (Pat::TempVar(n), _) => {
            assert_eq!(out.len() as u16, *n);
            out.push(v.fast_clone());
            Some(out)
        }
        (Pat::Variant(id1, None), Value::Variant(id2, None)) => {
            if &id1.0 != id2 {
                return None;
            };
            Some(out)
        }
        (Pat::Variant(id1, Some(pat_)), Value::Variant(id2, Some(v_))) => {
            if &id1.0 != id2 {
                return None;
            };
            pattern_matches_temps_(&pat_.0, v_.fast_clone(), out)
        }
        (Pat::Tuple(ps), Value::Tuple(vs)) => {
            if ps.vec.len() != vs.len() {
                None
            } else {
                let mut out = out;
                for i in 0..ps.vec.len() {
                    if let Some(out_) = pattern_matches_temps_(
                        &ps.vec.get(i).unwrap().0,
                        vs.get(i).unwrap().fast_clone(),
                        out,
                    ) {
                        out = out_
                    } else {
                        return None;
                    }
                }
                Some(out)
            }
        }
        _ => None,
    }
}

pub fn get_pat_var(p: &Pat) -> Option<Id_> {
    match p {
        Pat::Var(x) => Some(x.clone()),
        Pat::AnnotPat(p, _) => get_pat_var(&p.0),
        Pat::Paren(p) => get_pat_var(&p.0),
        _ => None,
    }
}

// TODO: see whether it's possible to return something like `&'a Option<Env>` to reduce cloning
// (since this has more of a performance impact than `fast_clone()`)
pub fn pattern_matches(env: Env, pat: &Pat, v: Value_) -> Option<Env> {
    match (pat, &*v) {
        (Pat::Wild, _) => Some(env),
        (Pat::Literal(Literal::Unit), Value::Unit) => Some(env),
        (Pat::Paren(p), _) => pattern_matches(env, &p.0, v),
        (Pat::Annot(_), _) => Some(env),
        (Pat::AnnotPat(p, _), _) => pattern_matches(env, &p.0, v),
        (Pat::Var(x), _) => {
            let mut env = env;
            env.insert(x.0.clone(), v);
            Some(env)
        }
        (Pat::Variant(id1, None), Value::Variant(id2, None)) => {
            if &id1.0 != id2 {
                return None;
            };
            Some(env)
        }
        (Pat::Variant(id1, Some(pat_)), Value::Variant(id2, Some(v_))) => {
            if &id1.0 != id2 {
                return None;
            };
            pattern_matches(env, &pat_.0, v_.fast_clone())
        }
        (Pat::Tuple(ps), Value::Tuple(vs)) => {
            if ps.vec.len() != vs.len() {
                None
            } else {
                let mut env = env;
                for i in 0..ps.vec.len() {
                    if let Some(env_) = pattern_matches(
                        env,
                        &ps.vec.get(i).unwrap().0,
                        vs.get(i).unwrap().fast_clone(),
                    ) {
                        env = env_
                    } else {
                        return None;
                    }
                }
                Some(env)
            }
        }
        _ => None,
    }
}
