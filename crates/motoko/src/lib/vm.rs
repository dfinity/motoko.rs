use crate::ast::{
    BinOp, Cases, Dec, Dec_, Exp, Exp_, Inst, Literal, Mut, Pat, PrimType, Prog, RelOp, Source,
    Type, UnOp,
};
use crate::ast_traversal::ToNode;
use crate::shared::Share;
use crate::value::{
    Closed, ClosedFunction, CollectionFunction, FastRandIter, FastRandIterFunction,
    HashMapFunction, PrimFunction, Value, ValueError, Value_,
};
use crate::vm_types::EvalInitError;
use crate::vm_types::{
    stack::{FieldContext, FieldValue, Frame, FrameCont},
    Breakpoint, Cont, Core, Counts, Env, Error, Interruption, Limit, Limits, Local, Pointer,
    Signal, Step, NYI,
};
use im_rc::{HashMap, Vector};
use num_bigint::{BigInt, BigUint, ToBigInt};
use num_traits::ToPrimitive;
use std::rc::Rc;
use std::vec::Vec;

impl From<()> for Interruption {
    // try to avoid this conversion, except in temp code.
    fn from(_x: ()) -> Interruption {
        Interruption::Unknown
    }
}

impl Limits {
    pub fn default() -> Self {
        Self::none()
    }

    /// No limits.
    pub fn none() -> Limits {
        Limits {
            breakpoints: vec![],
            step: None,
            redex: None,
        }
    }
    /// Set step limit.
    pub fn step(mut self, s: usize) -> Self {
        self.step = Some(s);
        self
    }
    /// Set redex limit.
    pub fn redex(mut self, s: usize) -> Self {
        self.redex = Some(s);
        self
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
        next_pointer: 0,
        debug_print_out: Vector::new(),
        counts: Counts::default(),
    }
}

fn unop(un: UnOp, v: Value_) -> Result<Value, Interruption> {
    match (un, &*v) {
        (UnOp::Neg, Value::Nat(n)) => Ok(Value::Int(-n.to_bigint().unwrap())),
        _ => nyi!(line!()),
    }
}

fn binop(
    cont_prim_type: &Option<PrimType>,
    binop: BinOp,
    v1: Value_,
    v2: Value_,
) -> Result<Value, Interruption> {
    use BinOp::*;
    use Value::*;
    if let Unit = &*v1 {
        return Err(Interruption::TypeMismatch);
    };
    if let Unit = &*v2 {
        return Err(Interruption::TypeMismatch);
    };
    match binop {
        Add => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 + n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 + i2)),
            // _ => nyi!(line!()),
            (v1, v2) => unimplemented!("{:?} + {:?}", v1, v2),
        },
        Sub => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => {
                if n2 > n1 {
                    Ok(Int(n1.to_bigint().unwrap() - n2.to_bigint().unwrap()))
                } else {
                    Ok(Nat(n1 - n2))
                }
            }
            (Int(i1), Int(i2)) => Ok(Int(i1 - i2)),
            (Int(i1), Nat(n2)) => Ok(Int(i1 - n2.to_bigint().unwrap())),
            (Nat(n1), Int(i2)) => Ok(Int(n1.to_bigint().unwrap() - i2)),
            // _ => nyi!(line!()),
            (v1, v2) => unimplemented!("{:?} - {:?}", v1, v2),
        },
        Mul => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 * n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 * i2)),
            // _ => nyi!(line!()),
            (v1, v2) => unimplemented!("{:?} * {:?}", v1, v2),
        },
        WAdd => match (cont_prim_type, &*v1, &*v2) {
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
    Ok(Bool(match relop {
        Eq => match (v1, v2) {
            (Bool(b1), Bool(b2)) => b1 == b2,
            (Text(t1), Text(t2)) => t1.to_string() == t2.to_string(),
            (Nat(n1), Nat(n2)) => n1 == n2,
            (Int(i1), Int(i2)) => i1 == i2,
            _ => nyi!(line!())?,
        },
        Neq => match (v1, v2) {
            (Bool(b1), Bool(b2)) => b1 != b2,
            (Text(t1), Text(t2)) => t1.to_string() == t2.to_string(),
            (Nat(n1), Nat(n2)) => n1 != n2,
            (Int(i1), Int(i2)) => i1 != i2,
            _ => nyi!(line!())?,
        },
        _ => nyi!(line!())?,
    }))
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

fn string_from_value(v: &Value) -> Result<String, Interruption> {
    //Ok(crate::format::format_one_line(v))
    Ok(format!("{:?}", v))
}

fn call_prim_function(
    core: &mut Core,
    pf: &PrimFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use PrimFunction::*;
    match pf {
        DebugPrint => match &*args {
            Value::Text(s) => {
                log::info!("DebugPrint: {}: {:?}", core.cont_source, s);
                core.debug_print_out.push_back(s.clone()); // TODO: store debug output as `Value_`?
                core.cont = cont_value(Value::Unit);
                Ok(Step {})
            }
            v => {
                let txt = string_from_value(&v)?;
                log::info!("DebugPrint: {}: {:?}", core.cont_source, txt);
                core.debug_print_out
                    .push_back(crate::value::Text::from(txt));
                core.cont = cont_value(Value::Unit);
                Ok(Step {})
            }
        },
        NatToText => match &*args {
            Value::Nat(n) => {
                core.cont = cont_value(Value::Text(format!("{}", n).into()));
                Ok(Step {})
            }
            v => {
                core.cont = cont_value(Value::Text(format!("{:?}", v).into()));
                Ok(Step {})
            }
        },
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "value-reflection")]
        ReifyValue => {
            use crate::value::ToMotoko;
            core.cont = cont_value(args.to_motoko().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "value-reflection")]
        ReflectValue => {
            // core.cont = cont_value(args.to_rust::<Value>().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "core-reflection")]
        ReifyCore => {
            use crate::value::ToMotoko;
            core.cont = cont_value(core.to_motoko().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "core-reflection")]
        ReflectCore => {
            *core = args.to_rust::<Core>().map_err(Interruption::ValueError)?;
            Ok(Step {})
        }
        Collection(cf) => call_collection_function(core, cf, targs, args),
    }
}

fn call_collection_function(
    core: &mut Core,
    cf: &CollectionFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use CollectionFunction::*;
    match cf {
        HashMap(hmf) => call_hashmap_function(core, hmf, targs, args),
        FastRandIter(frif) => call_fastranditer_function(core, frif, targs, args),
    }
}

fn call_fastranditer_function(
    core: &mut Core,
    frif: &FastRandIterFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use FastRandIterFunction::*;
    match frif {
        New => collection::fastranditer::new(core, targs, args),
        Next => collection::fastranditer::next(core, args),
    }
}

fn call_hashmap_function(
    core: &mut Core,
    hmf: &HashMapFunction,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use HashMapFunction::*;
    match hmf {
        New => collection::hashmap::new(core, args),
        Put => collection::hashmap::put(core, args),
        Get => collection::hashmap::get(core, args),
        Remove => collection::hashmap::remove(core, args),
    }
}

fn call_function(
    core: &mut Core,
    value: Value_,
    cf: &ClosedFunction,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    if let Some(env_) = pattern_matches(cf.0.env, &cf.0.content.input.0, args) {
        // TODO: any subtle optimizations here will probably make a huge difference
        let source = core.cont_source.clone();
        let env_saved = core.env.clone();
        core.env = env_;
        cf.0.content
            .name
            .clone()
            .map(|f| core.env.insert(*f.0, value));
        core.cont = Cont::Exp_(cf.0.content.exp.clone(), Vector::new());
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

mod pattern {
    use super::*;
    use crate::ast::{Delim, Loc, Node, Pat_};

    pub fn node<X>(core: &Core, x: X) -> Node<X> {
        let s = Source::ExpStep {
            source: Box::new(core.cont_source.clone()),
        };
        Loc(Box::new(x), s)
    }

    pub fn var_(core: &Core, id: &str) -> Pat_ {
        node(core, Pat::Var(node(core, id.to_string())))
    }

    pub fn vars(core: &Core, ids: Vector<&str>) -> Pat {
        let vars: Vec<_> = ids.into_iter().map(|i| var_(core, i)).collect();
        Pat::Tuple(Delim::from(vars))
    }
}

fn assert_value_is_optional<'a>(v: &'a Value) -> Result<Option<&'a Value_>, Interruption> {
    match v {
        Value::Option(v) => Ok(Some(v)),
        Value::Null => Ok(None),
        _ => Err(Interruption::TypeMismatch),
    }
}

fn assert_value_is_u32<'a>(v: &'a Value) -> Result<u32, Interruption> {
    v.to_rust().map_err(Interruption::ValueError)
}

fn assert_value_is_option_u32<'a>(v: &'a Value) -> Result<Option<u32>, Interruption> {
    match assert_value_is_optional(v)? {
        None => Ok(None),
        Some(v) => Ok(Some(assert_value_is_u32(v)?)),
    }
}

mod collection {
    pub mod fastranditer {
        use super::super::*;
        use crate::{shared::Share, value::Collection};
        use im_rc::vector;

        pub fn new(core: &mut Core, _targs: Option<Inst>, v: Value_) -> Result<Step, Interruption> {
            if let Some(env) = pattern_matches(
                HashMap::new(),
                &pattern::vars(core, vector!["size", "seed"]),
                v,
            ) {
                let seed: u32 = assert_value_is_u32(&*env.get("seed").unwrap())?;
                let size: Option<u32> = assert_value_is_option_u32(&*env.get("size").unwrap())?;

                core.cont = cont_value(
                    Value::Collection(Collection::FastRandIter(FastRandIter::new(size, seed)))
                );
                Ok(Step {})
            } else {
                Err(Interruption::TypeMismatch)
            }
        }

        pub fn next(core: &mut Core, v: Value_) -> Result<Step, Interruption> {
            match &*v {
                Value::Collection(Collection::FastRandIter(mut fri)) => {
                    let n = match fri.next() {
                        // to do -- systematic Option<_> ~> ?<_> conversion.
                        Some(n) => Value::Option(n.share()),
                        None => Value::Null,
                    };
                    // let i = Value::Collection(Collection::FastRandIter(fri));
                    core.cont = cont_value(Value::Tuple(vector![n.share(), v]));
                    Ok(Step {})
                }
                _ => Err(Interruption::TypeMismatch),
            }
        }
    }

    pub mod hashmap {
        use super::super::*;
        use crate::value::Collection;
        use im_rc::vector;

        pub fn new(core: &mut Core, v: Value_) -> Result<Step, Interruption> {
            if let Some(_) = pattern_matches(core.env.clone(), &Pat::Literal(Literal::Unit), v) {
                core.cont =
                cont_value(Value::Collection(Collection::HashMap(HashMap::new())));
                Ok(Step {})
            } else {
                Err(Interruption::TypeMismatch)
            }
        }
        pub fn put(core: &mut Core, v: Value_) -> Result<Step, Interruption> {
            if let Some(env) = pattern_matches(
                HashMap::new(),
                &pattern::vars(core, vector!["hm", "k", "v"]),
                v,
            ) {
                let hm = env.get("hm").unwrap();
                let k = env.get("k").unwrap();
                let v = env.get("v").unwrap();
                let (hm, old) = {
                    if let Value::Collection(Collection::HashMap(mut hm)) = &**hm {
                        match hm.insert(k.fast_clone(), v.fast_clone()) {
                            None => (hm, Value::Null),
                            Some(old) => (hm, Value::Option(old)),
                        }
                    } else {
                        return Err(Interruption::TypeMismatch);
                    }
                };
                // Note for later: immutable map updates are adding extra overhead here
                // We could probably just tolerate this and use `Dynamic` values for performance-critical situations
                let hm = Value::Collection(Collection::HashMap(hm));
                let ret = Value::Tuple(vector![hm.share(), old.share()]);
                core.cont = cont_value(ret);
                Ok(Step {})
            } else {
                Err(Interruption::TypeMismatch)
            }
        }
        pub fn get(core: &mut Core, v: Value_) -> Result<Step, Interruption> {
            if let Some(env) =
                pattern_matches(HashMap::new(), &pattern::vars(core, vector!["hm", "k"]), v)
            {
                let hm = env.get("hm").unwrap();
                let k = env.get("k").unwrap();
                let ret = {
                    if let Value::Collection(Collection::HashMap(hm)) = &**hm {
                        match hm.get(k) {
                            None => Value::Null,
                            Some(v) => Value::Option(v.fast_clone()),
                        }
                    } else {
                        return Err(Interruption::TypeMismatch);
                    }
                };
                core.cont = cont_value(ret);
                Ok(Step {})
            } else {
                Err(Interruption::TypeMismatch)
            }
        }
        pub fn remove(core: &mut Core, v: Value_) -> Result<Step, Interruption> {
            if let Some(env) =
                pattern_matches(HashMap::new(), &pattern::vars(core, vector!["hm", "k"]), v)
            {
                let hm = env.get("hm").unwrap();
                let k = env.get("k").unwrap();
                let (hm, old) = {
                    if let Value::Collection(Collection::HashMap(mut hm)) = &**hm {
                        match hm.remove(k) {
                            None => (hm, Value::Null),
                            Some(v) => (hm, Value::Option(v)),
                        }
                    } else {
                        return Err(Interruption::TypeMismatch);
                    }
                };
                let hm = Value::Collection(Collection::HashMap(hm));
                let ret = Value::Tuple(vector![hm.share(), old.share()]);
                core.cont = cont_value(ret);
                Ok(Step {})
            } else {
                Err(Interruption::TypeMismatch)
            }
        }
    }
}

fn prim_value(name: &str) -> Result<Value, Interruption> {
    //use crate::value::{CollectionFunction, HashMapFunction, PrimFunction};
    use CollectionFunction::*;
    use PrimFunction::*;
    if let Some(pf) = match name {
        "\"debugPrint\"" => Some(DebugPrint),
        "\"natToText\"" => Some(NatToText),
        "\"hashMapNew\"" => Some(Collection(HashMap(HashMapFunction::New))),
        "\"hashMapPut\"" => Some(Collection(HashMap(HashMapFunction::Put))),
        "\"hashMapGet\"" => Some(Collection(HashMap(HashMapFunction::Get))),
        "\"hashMapRemove\"" => Some(Collection(HashMap(HashMapFunction::Remove))),
        "\"fastRandIterNew\"" => Some(Collection(FastRandIter(FastRandIterFunction::New))),
        "\"fastRandIterNext\"" => Some(Collection(FastRandIter(FastRandIterFunction::Next))),
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "value-reflection")]
        "\"reifyValue\"" => Some(ReifyValue),
        #[cfg(feature = "value-reflection")]
        "\"reflectValue\"" => Some(ReflectValue),
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "core-reflection")]
        "\"reifyCore\"" => Some(ReifyCore),
        #[cfg(feature = "core-reflection")]
        "\"reflectCore\"" => Some(ReflectCore),
        _ => None,
    } {
        Ok(Value::PrimFunction(pf))
    } else {
        Err(Interruption::UnrecognizedPrim(name.to_string()))
    }
}

fn exp_step(core: &mut Core, exp: Exp_) -> Result<Step, Interruption> {
    use Exp::*;
    let source = exp.1.clone();
    match *exp.0 {
        Literal(l) => {
            // TODO: partial evaluation would now be highly efficient due to value sharing
            core.cont = cont_value(
                Value::from_literal(l)
                    .map_err(Interruption::ValueError)?
            );
            Ok(Step {})
        }
        Function(f) => {
            core.cont = cont_value(
                Value::Function(ClosedFunction(Closed {
                    env: core.env.clone(),
                    content: f,
                }))
            );
            Ok(Step {})
        }
        Call(e1, inst, e2) => exp_conts(core, FrameCont::Call1(inst, e2), e1),
        Return(None) => return_(core, Value::Unit.share()),
        Return(Some(e)) => exp_conts(core, FrameCont::Return, e),
        Var(x) => match core.env.get(&x) {
            None => Err(Interruption::UnboundIdentifer(x.clone())),
            Some(v) => {
                core.cont = _(v.fast_clone());
                Ok(Step {})
            }
        },
        Bin(e1, binop, e2) => exp_conts(core, FrameCont::BinOp1(binop, e2), e1),
        Un(un, e) => exp_conts(core, FrameCont::UnOp(un), e),
        Paren(e) => exp_conts(core, FrameCont::Paren, e),
        Variant(id, None) => {
            // TODO: cache and share variants?
            core.cont =cont_value(Value::Variant(*id.0, None));
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
                    core.cont = cont_value(Value::Object(HashMap::new()));
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
                    // TODO: globally share (), true, false, null, etc.
                    core.cont = cont_value(Value::Unit);
                    Ok(Step {})
                }
                Some(e1) => exp_conts(core, FrameCont::Tuple(Vector::new(), es), e1),
            }
        }
        Array(mut_, es) => {
            let mut es: Vector<_> = es.vec.into();
            match es.pop_front() {
                None => {
                    core.cont = cont_value(Value::Array(mut_, Vector::new()));
                    Ok(Step {})
                }
                Some(e1) => exp_conts(core, FrameCont::Array(mut_, Vector::new(), es), e1),
            }
        }
        Index(e1, e2) => exp_conts(core, FrameCont::Idx1(e2), e1),
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
        Prim(s) => {
            core.cont = cont_value(prim_value(&s)?);
            Ok(Step {})
        }
        _ => nyi!(line!()),
    }
}

fn pattern_matches(env: Env, pat: &Pat, v: Value_) -> Option<Env> {
    match (pat, &*v) {
        (Pat::Wild, _) => Some(env),
        (Pat::Literal(Literal::Unit), Value::Unit) => Some(env),
        (Pat::Paren(p), _) => pattern_matches(env, &*p.0, v),
        (Pat::Annot(p, _), _) => pattern_matches(env, &*p.0, v),
        (Pat::Var(x), _) => {
            let mut env = env;
            env.insert(*x.0.clone() /* TODO: remove clone() */, v);
            Some(env)
        }
        (Pat::Variant(id1, None), Value::Variant(id2, None)) => {
            if **id1.0 != *id2 {
                return None;
            };
            Some(env)
        }
        (Pat::Variant(id1, Some(pat_)), Value::Variant(id2, Some(v_))) => {
            if **id1.0 != *id2 {
                return None;
            };
            pattern_matches(env, &*pat_.0, v_.fast_clone())
        }
        (Pat::Tuple(ps), Value::Tuple(vs)) => {
            if ps.vec.len() != vs.len() {
                None
            } else {
                for i in 0..ps.vec.len() {
                    if let Some(env_) = pattern_matches(
                        env,
                        &*ps.vec.get(i).unwrap().0,
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

fn switch(core: &mut Core, v: Value_, cases: Cases) -> Result<Step, Interruption> {
    for case in cases.vec.into_iter() {
        if let Some(env) = pattern_matches(core.env.clone(), &*case.0.pat.0, v) {
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
                    core.cont = cont_value(Value::Null);
                    return Ok(Step {});
                }
                _ => {}
            }
        } else {
            return Err(Interruption::NoDoQuestBangNull);
        }
    }
}

fn return_(core: &mut Core, v: Value_) -> Result<Step, Interruption> {
    let mut stack = core.stack.clone();
    loop {
        if let Some(fr) = stack.pop_front() {
            match fr.cont {
                FrameCont::Call3 => {
                    core.env = fr.env;
                    core.stack = stack;
                    core.cont = cont_value(v);
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
        Value_(_v) => Source::Evaluation,
    }
}

mod store {
    use num_traits::ToPrimitive;

    use crate::{shared::Share, value::Value_};

    use super::{Core, Interruption, Mut, Pointer, Value};

    pub fn mutate(core: &mut Core, p: Pointer, v: Value_) -> Result<(), Interruption> {
        // it is an error to mutate an unallocated pointer.
        match core.store.get(&p) {
            None => return Err(Interruption::Dangling(p)),
            Some(_) => (),
        };
        core.store.insert(p, v);
        Ok(())
    }

    pub fn mutate_index(
        core: &mut Core,
        p: Pointer,
        i: Value_,
        v: Value_,
    ) -> Result<(), Interruption> {
        // it is an error to mutate an unallocated pointer.
        let pointer_ref = core
            .store
            .get_mut(&p)
            .ok_or_else(|| Interruption::Dangling(p))?;

        match &**pointer_ref {
            Value::Array(Mut::Var, a) => {
                let i = match &*i {
                    Value::Nat(n) => n.to_usize().ok_or_else(|| {
                        Interruption::ValueError(crate::value::ValueError::BigInt)
                    })?,
                    _ => Err(Interruption::TypeMismatch)?,
                };
                let mut a = a.clone();
                if i < a.len() {
                    a.set(i, v);
                    *pointer_ref = Value::Array(Mut::Var, a).share();
                    Ok(())
                } else {
                    Err(Interruption::IndexOutOfBounds)
                }
            }
            Value::Dynamic(d) => {
                d.dynamic_mut().set_index(i, v)?;
                Ok(())
            }
            _ => Err(Interruption::TypeMismatch),
        }
    }
}

fn usize_from_biguint(n: BigUint) -> Result<usize, Interruption> {
    n.to_usize()
        .ok_or_else(|| Interruption::ValueError(ValueError::BigInt))
}

fn stack_cont_has_redex(core: &Core, v: &Value) -> Result<bool, Interruption> {
    if core.stack.len() == 0 {
        Ok(false)
    } else {
        use FrameCont::*;
        let frame = core.stack.front().unwrap();
        let r = match &frame.cont {
            UnOp(_) => true,
            RelOp1(_, _) => false,
            RelOp2(_, _) => true,
            BinOp1(_, _) => false,
            BinOp2(_, _) => true,
            Assign1(_) => false,
            Assign2(_) => true,
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
            For1(_, _, _) => true,
            For2(_, _, _) => false,
            And1(_) => false,
            And2 => true,
            Or1(_) => match v {
                Value::Bool(b) => b.clone(),
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
fn stack_cont(core: &mut Core, v: Value_) -> Result<Step, Interruption> {
    if core.stack.len() == 0 {
        core.cont = cont_value(v);
        Err(Interruption::Done(v))
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
                core.cont = cont_value(unop(un, v)?);
                Ok(Step {})
            }
            RelOp1(relop, e2) => exp_conts(core, RelOp2(v, relop), e2),
            RelOp2(v1, rel) => {
                core.cont = cont_value(relop(&core.cont_prim_type, rel, v1, v)?);
                Ok(Step {})
            }
            BinOp1(binop, e2) => exp_conts(core, BinOp2(v, binop), e2),
            BinOp2(v1, bop) => {
                core.cont = cont_value(binop(&core.cont_prim_type, bop, v1, v)?);
                Ok(Step {})
            }
            // Updating for consistency with the `Call1` / `Call2` optimization
            // Assign1(e2) => match &*v {
            //     Value::Pointer(p) => exp_conts(core, Assign2(v), e2),
            //     Value::Index(p, i) => exp_conts(core, Assign2(v), e2),
            //     _ => Err(Interruption::TypeMismatch),
            // },
            Assign1(e2) => exp_conts(core, Assign2(v), e2),
            Assign2(v1) => match &*v1 {
                Value::Pointer(p) => {
                    store::mutate(core, p.clone(), v)?;
                    core.cont = cont_value(Value::Unit);
                    Ok(Step {})
                }
                Value::Index(p, i) => {
                    println!("Assign2 index {:?}[{:?}] := {:?}", p, i, v); ////
                    store::mutate_index(core, p.clone(), i.fast_clone(), v)?;
                    core.cont = cont_value(Value::Unit);
                    Ok(Step {})
                }
                _ => Err(Interruption::TypeMismatch),
            },
            Idx1(e2) => exp_conts(core, Idx2(v), e2),
            Idx2(v1) => {
                if let Some(Frame {
                    cont: FrameCont::Assign1(_), // still need to evaluate RHS of assignment.
                    ..
                }) = core.stack.get(0)
                {
                    match &*v1 {
                        Value::Pointer(p) => {
                            // save array pointer and offset until after RHS is evaluated.
                            core.cont =
                            cont_value(Value::Index(p.clone(), v.fast_clone()));
                            Ok(Step {})
                        }
                        Value::Dynamic(_) => Err(Interruption::Other(
                            "Dynamic Rust value without a pointer".to_string(),
                        )),
                        _ => Err(Interruption::TypeMismatch),
                    }
                } else {
                    let v1 = core.deref_value(v1)?;
                    match (&*v1, v) {
                        (Value::Array(_mut, a), Value::Nat(i)) => {
                            let i = usize_from_biguint(i)?;
                            core.cont = cont_value(
                                (**a.get(i).ok_or(Interruption::IndexOutOfBounds)?).clone(),
                            );
                            Ok(Step {})
                        }
                        (Value::Dynamic(d), v) => {
                            core.cont = cont_value((*d.dynamic().get_index(&v)?).clone());
                            Ok(Step {})
                        }
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
                let ptr = core.alloc(v);
                core.env.insert(x, Value::Pointer(ptr).share());
                core.cont_source = source_from_cont(&cont);
                core.cont = cont;
                Ok(Step {})
            }
            Paren => {
                core.cont = Cont::Value_(v);
                Ok(Step {})
            }
            Variant(i) => {
                core.cont = cont_value(Value::Variant(*i.0, Some(v)));
                Ok(Step {})
            }
            Switch(cases) => switch(core, v, cases),
            Block => {
                core.cont = Cont::Value_(v);
                Ok(Step {})
            }
            Decs(decs) => {
                match decs.front() {
                    None => {
                        // return final value from block.
                        core.cont = Cont::Value_(v);
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
                done.push_back(Rc::new(v));
                match rest.pop_front() {
                    None => {
                        if let Mut::Const = mut_ {
                            core.cont = Cont::Value(Value::Array(mut_, done));
                            Ok(Step {})
                        } else {
                            let arr = Value::Array(mut_, done);
                            let ptr = core.alloc(Rc::new(arr));
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
                                Mut::Var => Value::Pointer(core.alloc(Rc::new(f.val))),
                            };
                            hm.insert(
                                id.clone(),
                                crate::value::FieldValue {
                                    mut_: f.mut_,
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
                Value::Dynamic(d) => {
                    let f = d.dynamic().get_field(&*f.0)?;
                    core.cont = Cont::Value((*f).clone());
                    Ok(Step {})
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
                core.cont = Cont::Value(Value::Option(Rc::new(v)));
                Ok(Step {})
            }
            DoOpt => {
                core.cont = Cont::Value(Value::Option(Rc::new(v)));
                Ok(Step {})
            }
            Bang => match v {
                Value::Option(v) => {
                    core.cont = Cont::Value((*v).clone());
                    Ok(Step {})
                }
                Value::Null => bang_null(core),
                _ => Err(Interruption::TypeMismatch),
            },
            Call1(inst, e2) => {
                exp_conts(core, FrameCont::Call2(v, inst), e2)
                // match v {
                //     Value::Function(cf) => exp_conts(core, FrameCont::Call2(cf, inst), e2),
                //     Value::PrimFunction(pf) => exp_conts(core, FrameCont::Call2Prim(pf, inst), e2),
                //     Value::Dynamic(d) => exp_conts(core, FrameCont::Call2Dyn(d, inst), e2),
                //     _ => Err(Interruption::TypeMismatch),
                // }
            }
            Call2(f, inst) => match &*f {
                Value::Function(cf) => call_function(core, f.clone(), cf, inst, v),
                Value::PrimFunction(pf) => call_prim_function(core, pf, inst, v),
                Value::Dynamic(d) => {
                    let result = d.dynamic().call(&inst, Rc::new(v))?;
                    core.cont = Cont::Value((*result).clone());
                    Ok(Step {})
                }
                _ => Err(Interruption::TypeMismatch),
            },
            Call3 => {
                core.cont = Cont::Value(v);
                Ok(Step {})
            }
            Return => return_(core, v),
            _ => nyi!(line!()),
        }
    }
}

// Returns `Some(span)` if the limits include the breakpoint.
fn check_for_breakpoint(core: &Core, limits: &Limits) -> Option<Breakpoint> {
    let cont_span = &core.cont_source.span();
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

fn core_step(core: &mut Core, limits: &Limits) -> Result<Step, Interruption> {
    if let Some(break_span) = check_for_breakpoint(core, limits) {
        return Err(Interruption::Breakpoint(break_span));
    }
    let mut redex_bump = 0;
    if let Some(step_limit) = limits.step {
        if core.counts.step >= step_limit {
            return Err(Interruption::Limit(Limit::Step));
        }
    }
    if let Cont::Value(ref v) = core.cont {
        if stack_cont_has_redex(core, v)? {
            redex_bump = 1;
            if let Some(redex_limit) = limits.redex {
                if core.counts.redex >= redex_limit {
                    // if =, adding 1 will exceed limit, so do not.
                    return Err(Interruption::Limit(Limit::Redex));
                }
            }
        }
    }
    let ret = core_step_(core)?;
    core.counts.step += 1;
    core.counts.redex += redex_bump;
    Ok(ret)
}

// To advance the core Motoko state by a single step, after all limits are checked.
fn core_step_(core: &mut Core) -> Result<Step, Interruption> {
    use log::trace;
    trace!("# step {} (redex {})", core.counts.step, core.counts.redex);
    trace!(" - cont = {:?}", core.cont);
    trace!("   - cont_source = {:?}", core.cont_source);
    trace!("   - env = {:?}", core.env);
    trace!(" - stack = {:#?}", core.stack);
    trace!(" - store = {:#?}", core.store);
    let mut cont = Cont::Taken;
    std::mem::swap(&mut core.cont, &mut cont);
    match cont {
        Cont::Taken => unreachable!("The VM's logic currently has an internal issue."),
        Cont::Exp_(e, decs) => {
            if decs.len() == 0 {
                core.cont_prim_type = core.cont_prim_type.clone();
                exp_step(core, e)
            } else {
                let source = source_from_decs(&decs);
                core.stack.push_front(Frame {
                    env: core.env.clone(),
                    cont: FrameCont::Decs(decs),
                    source,
                    cont_prim_type: None,
                });
                exp_step(core, e)
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
                }) => return stack_cont(core, Value::Pointer(p)),
                // Case: Assignment to a pointer.
                Some(Frame {
                    cont: FrameCont::Assign1(_),
                    ..
                }) => return stack_cont(core, Value::Pointer(p)),
                // Case: Array-indexing with a pointer.
                Some(Frame {
                    cont: FrameCont::Idx1(_),
                    ..
                }) => return stack_cont(core, Value::Pointer(p)),
                _ => (),
            };
            // Final case: Implicit dereferencing of pointer:
            let v = core.deref(&p)?;
            core.cont = Cont::Value((*v).clone());
            Ok(Step {})
        }
        Cont::Value(v) => stack_cont(core, v),
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
                    Dec::LetModule(_i, _, _dfs) => {
                        nyi!(line!())
                    }
                    Dec::Var(p, e) => match *p.0 {
                        Pat::Var(x) => exp_conts(core, FrameCont::Var(*x.0, Cont::Decs(decs)), e),
                        _ => nyi!(line!()),
                    },
                    Dec::Func(f) => {
                        let id = f.name.clone();
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

    /// New VM core without any program.
    pub fn empty() -> Self {
        let mut core = core_init(crate::ast::Delim::new());
        // core.eval_(None, &Limits::none()).expect("empty");
        core.continue_(&Limits::none()).expect("empty");
        core
    }

    /// New VM core from a given program string, to be parsed during Core construction.
    #[cfg(feature = "parser")]
    pub fn from_str(s: &str) -> Result<Self, crate::parser_types::SyntaxError> {
        Ok(core_init(crate::check::parse(s)?))
    }

    /// Step VM core, under some limits.
    pub fn step(&mut self, limits: &Limits) -> Result<Step, Interruption> {
        core_step(self, limits)
    }

    /// Evaluate a new program fragment, assuming `Core` is in a
    /// well-defined "done" state.
    pub fn eval_prog(&mut self, prog: Prog) -> Result<Value, Interruption> {
        self.assert_idle().map_err(Interruption::EvalInitError)?;
        self.cont = Cont::Decs(Vector::from(prog.vec));
        self.continue_(&Limits::none())
    }

    /// Evaluate a new program fragment, assuming `Core` is in a
    /// well-defined "done" state.  The block may refer to variables
    /// bound as arguments, and then forgotten after evaluation.
    pub fn eval_open_block(
        &mut self,
        value_bindings: Vec<(&str, Value)>,
        prog: Prog,
    ) -> Result<Value, Interruption> {
        let source = self.cont_source.clone(); // to do -- use prog source
        self.assert_idle().map_err(Interruption::EvalInitError)?;
        exp_conts_(
            self,
            source.clone(),
            FrameCont::Block,
            Cont::Decs(prog.vec.into()),
            source.clone(),
        )?;
        for (x, v) in value_bindings.into_iter() {
            let _ = self.env.insert(x.to_string(), v);
        }
        self.continue_(&Limits::none())
    }

    /// Evaluate a new program fragment, assuming `Core` is in a
    /// well-defined "done" state.
    #[cfg(feature = "parser")]
    pub fn eval(&mut self, new_prog_frag: &str) -> Result<Value, Interruption> {
        self.eval_(Some(new_prog_frag), &Limits::none())
    }

    pub fn assert_idle(&self) -> Result<(), EvalInitError> {
        if self.stack.len() > 0 {
            return Err(EvalInitError::NonEmptyStack);
        }
        match self.cont {
            Cont::Value(_) => {}
            _ => return Err(EvalInitError::NonValueCont),
        };
        Ok(())
    }

    /// Continue evaluation, with given limits.
    pub fn continue_(&mut self, limits: &Limits) -> Result<Value, Interruption> {
        loop {
            match self.step(limits) {
                Ok(_step) => {}
                Err(Interruption::Done(v)) => return Ok(v),
                Err(other_interruption) => return Err(other_interruption),
            }
        }
    }

    /// Evaluate current continuation, or optionally a new program
    /// fragment, assuming `Core` is in a well-defined "done" state.
    #[cfg(feature = "parser")]
    pub fn eval_(
        &mut self,
        new_prog_frag: Option<&str>,
        limits: &Limits,
    ) -> Result<Value, Interruption> {
        if let Some(new_prog_frag) = new_prog_frag {
            self.assert_idle().map_err(Interruption::EvalInitError)?;
            let p = crate::check::parse(&new_prog_frag).map_err(Interruption::SyntaxError)?;
            self.cont = Cont::Decs(Vector::from(p.vec));
        };
        self.continue_(limits)
    }

    pub fn alloc(&mut self, value: Value_) -> Pointer {
        let ptr = Pointer(self.next_pointer);
        self.next_pointer = self.next_pointer.checked_add(1).expect("Out of pointers");
        self.store.insert(ptr.clone(), value);
        ptr
    }

    pub fn dealloc(&mut self, pointer: &Pointer) -> Option<Value_> {
        self.store.remove(pointer)
    }

    pub fn deref(&mut self, pointer: &Pointer) -> Result<Value_, Interruption> {
        self.store
            .get(pointer)
            .ok_or_else(|| Interruption::Dangling(pointer.clone()))
            .map(|v| v.fast_clone())
    }

    pub fn deref_value(&mut self, value: Value_) -> Result<Value_, Interruption> {
        match &*value {
            Value::Pointer(p) => self.deref(p),
            _ => Ok(value),
        }
    }
}

// For core Motoko state initializing local VM state.
fn local_init(active: Core) -> Local {
    Local { active }
}

// use ic-agent to do function calls.
fn local_run(local: &mut Local, limits: &Limits) -> Result<Signal, Error> {
    loop {
        match core_step(&mut local.active, limits) {
            Ok(_step) => {}
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

#[cfg(feature = "parser")]
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
    }
}

/// Used for tests in check module.
#[cfg(feature = "parser")]
pub fn eval(prog: &str) -> Result<Value, Interruption> {
    eval_limit(prog, &Limits::none())
}

#[cfg(feature = "parser")]
pub fn eval_into<T: serde::de::DeserializeOwned>(prog: &str) -> Result<T, Interruption> {
    eval(prog)?.to_rust().map_err(Interruption::ValueError)
}

// TODO: possibly refactor to `Cont::Value(Value)` and `Cont::Value_(Value_)`
#[inline(always)]
fn cont_value(value: Value) -> Cont {
    Cont::Value_(value.share())
}
