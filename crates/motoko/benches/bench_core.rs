#![feature(test)]

use motoko::vm_types::Core;
use test::Bencher;
extern crate test;

#[bench]
fn core_clone(b: &mut Bencher) {
    let core = Core::from_str("
        let x = 0;
        let y = x;
        y
    ");

    b.iter(|| core.clone())
}
