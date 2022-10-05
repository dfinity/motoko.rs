#![feature(test)]

use motoko::vm_types::{Core, Limits};
use test::Bencher;
extern crate test;

#[bench]
fn core_clone_smaller(b: &mut Bencher) {
    let mut core = Core::from_str("let x = 0; x").unwrap();
    core.continue_(&Limits::default()).unwrap();

    b.iter(|| core.clone())
}

#[bench]
fn core_clone_larger(b: &mut Bencher) {
    let mut core = Core::from_str(
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
        let i = Iter.range(3);
        var sum = 0;
        for (y in i) {
            sum := sum + 1;
            Debug.print sum
        }
    "#,
    )
    .unwrap();
    core.continue_(&Limits::default()).unwrap();

    b.iter(|| core.clone())
}
