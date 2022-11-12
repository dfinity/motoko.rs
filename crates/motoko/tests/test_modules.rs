use motoko::check::assert_vm_eval as assert_;
//use motoko::check::assert_vm_interruption as assert_x;

use test_log::test; // enable logging output for tests by default.

#[test]
fn module() {
    let p = "module X { public let x = y; let y = (f 1, 2); func f () { } }";
    assert_(p, p)
}

#[test]
fn import_nothing() {
    let p = "import _ = \"Module\"";
    assert_(p, "()")
}

#[test]
fn import_something() {
    let p = "import M \"Module\"";
    assert_(p, "()")
}
