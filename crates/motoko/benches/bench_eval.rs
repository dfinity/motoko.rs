#![feature(test)]

use motoko::{
    shared::Share,
    value::Value,
    vm_types::{Core, Limits},
};
use num_bigint::ToBigUint;
use test::Bencher;
extern crate test;

#[bench]
fn eval_smaller(b: &mut Bencher) {
    let prog = motoko::check::parse("let x = 0; x").unwrap();
    b.iter(|| {
        assert_eq!(
            Core::new(prog.clone()).continue_(&Limits::default()),
            Ok(Value::Nat(0.to_biguint().unwrap()).share())
        )
    })
}

#[bench]
fn eval_larger(b: &mut Bencher) {
    let prog = motoko::check::parse(
        r#"
            let Debug = { print = prim "debugPrint"};
            var x = 0;
            let Iter = { range = func(end){
            { next = func() {
            if (x == end) {
                null
            } else {
                let x_ = x;
                x := x_ + 1;
                ?x_
            }}}}};
            let i = Iter.range(100);
            var sum = 0;
            for (y in i) {
                sum := sum + 1;
                Debug.print sum
            }
        "#,
    )
    .unwrap();
    b.iter(|| {
        assert_eq!(
            Core::new(prog.clone()).continue_(&Limits::default()),
            Ok(Value::Unit.share())
        )
    })
}
