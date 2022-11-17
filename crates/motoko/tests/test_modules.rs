use motoko::check::assert_vm_eval as assert_;
//use motoko::check::assert_vm_interruption as assert_x;

use test_log::test; // enable logging output for tests by default.

#[test]
fn module() {
    let p = "
    module M {
      public let x1 = y;
      let x2 = (1, 2);
      func x3 () { };
      let _ = 1 + 2 - 4;
      1 + 2 - 4;
      public 1 + 2 - 4;
      public let x4 = #foo(x6);
      public let x5 = #foo(1 + 2);
      public let x6 = #foo(x5);
      let x7 = [1, 2];
    };
    M.x1;
    M.x2;
    M.x3;
    M.x4;
    M.x5;
    M.x6;
    M.x7;";
    assert_(p, p)
}

#[ignore]
#[test]
fn import_nothing() {
    let p = "import _ = \"Module\"";
    assert_(p, "()")
}

#[ignore]
#[test]
fn import_something() {
    let p = "import M \"Module\"";
    assert_(p, "()")
}
