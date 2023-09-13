use crate::ast::{BinOp, PrimType, RelOp, UnOp};
use crate::vm_types::Interruption;
//use crate::ast_traversal::ToNode;
//use crate::shared::{FastClone, Share};
use crate::value::{
    //    ActorId, ActorMethod, Closed, ClosedFunction, CollectionFunction, FastRandIter,
    //    FastRandIterFunction, HashMapFunction, PrimFunction,
    Value,
    //    ValueError,
    Value_,
};
use num_bigint::{BigUint, ToBigInt};
//use num_traits::ToPrimitive;

use crate::{nyi, type_mismatch};

pub fn unop(un: UnOp, v: Value_) -> Result<Value, Interruption> {
    match (un, &*v) {
        (UnOp::Neg, Value::Nat(n)) => Ok(Value::Int(-n.to_bigint().unwrap())),
        _ => crate::nyi!(line!()),
    }
}

pub fn binop(
    cont_prim_type: &Option<PrimType>,
    binop: BinOp,
    v1: Value_,
    v2: Value_,
) -> Result<Value, Interruption> {
    use BinOp::*;
    use Value::*;
    if let Unit = &*v1 {
        type_mismatch!(file!(), line!());
    };
    if let Unit = &*v2 {
        type_mismatch!(file!(), line!());
    };
    match binop {
        Add => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 + n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 + i2)),
            (Float(f1), Float(f2)) => Ok(Float(*f1 + *f2)),
            // _ => nyi!(line!()),
            (v1, v2) => unimplemented!("{:?} + {:?}", v1, v2),
        },
        Div => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 / n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 / i2)),
            (Float(f1), Float(f2)) => Ok(Float(*f1 / *f2)),
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
            (Float(f1), Float(f2)) => Ok(Float(*f1 - *f2)),
            // _ => nyi!(line!()),
            (v1, v2) => unimplemented!("{:?} - {:?}", v1, v2),
        },
        Mul => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 * n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 * i2)),
            (Float(f1), Float(f2)) => Ok(Float(*f1 * *f2)),
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
        _ => nyi!(line!(), "binop({:?}. {:?}, {:?})", binop, v1, v2),
    }
}

pub fn relop(
    _cont_prim_xotype: &Option<PrimType>,
    relop: RelOp,
    v1: Value_,
    v2: Value_,
) -> Result<Value, Interruption> {
    use RelOp::*;
    use Value::*;
    Ok(Bool(match relop {
        Eq => match (&*v1, &*v2) {
            (Unit, Unit) => true,
            (Bool(b1), Bool(b2)) => b1 == b2,
            (Text(t1), Text(t2)) => t1 == t2,
            (Nat(n1), Nat(n2)) => n1 == n2,
            (Int(i1), Int(i2)) => i1 == i2,
            (v1, v2) => v1 == v2, //            _ => nyi!(line!(), "{:?} == {:?}", v1, v2)?,
        },
        Neq => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Bool(b1), Bool(b2)) => b1 != b2,
            (Text(t1), Text(t2)) => t1 == t2,
            (Nat(n1), Nat(n2)) => n1 != n2,
            (Int(i1), Int(i2)) => i1 != i2,
            (v1, v2) => v1 != v2, //            _ => nyi!(line!(), "{:?} == {:?}", v1, v2)?,
        },
        Lt => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Nat(n1), Nat(n2)) => n1 < n2,
            (Int(i1), Int(i2)) => i1 < i2,
            _ => nyi!(line!(), "{:?} < {:?}", v1, v2)?,
        },
        Le => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Nat(n1), Nat(n2)) => n1 <= n2,
            (Int(i1), Int(i2)) => i1 <= i2,
            _ => nyi!(line!(), "{:?} <= {:?}", v1, v2)?,
        },
        Gt => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Nat(n1), Nat(n2)) => n1 > n2,
            (Int(i1), Int(i2)) => i1 > i2,
            _ => nyi!(line!(), "{:?} > {:?}", v1, v2)?,
        },
        Ge => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Nat(n1), Nat(n2)) => n1 >= n2,
            (Int(i1), Int(i2)) => i1 >= i2,
            _ => nyi!(line!(), "{:?} >= {:?}", v1, v2)?,
        },
        //        _ => nyi!(line!(), "relop({:?}, {:?}, {:?})", relop, v1, v2)?,
    }))
}
