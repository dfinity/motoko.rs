#![feature(test)]
extern crate test;
use test::Bencher;

use motoko::check::assert_vm_eval as assert_;

fn recursion() {
    let prog = r#"
var x = 0;
func f() {
  if (x != 666) {
    x := x + 1;
    f()
  } else {
    x
  }
};
f()
"#;
    assert_(prog, "666");
}


#[bench]
fn bench_recursion(b: &mut Bencher) {
    b.iter(|| recursion());
}
