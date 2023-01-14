use motoko::check::assert_vm_eval as assert_;
//use motoko::check::assert_vm_interruption as assert_x;

use test_log::test; // enable logging output for tests by default.

#[ignore]
#[test]
fn class() {
    let p = "
    class C (w) {
      public let x1 = x2;
      public let x2 = (1, 2);
      public func x3 () { };
      let _ = 1 + 2 - 4;
      1 + 2 - 4;
      public 1 + 2 - 4;
      public let x4 = #foo(x6);
      public let x5 = #foo(1 + 2);
      public let x6 = #foo(x5);
      public let x7 = [1, 2];
      public let x8 = z;
      let z = 0;
      public let x9 = w + 1;
    };
    let o = C(42);
    assert o.x1.0 == 1;
    assert o.x2.0 == 1;
    assert o.x3 () == ();
    assert o.x4 == #foo(#foo(#foo(3)));
    assert o.x5 == #foo(3);
    assert o.x6 == #foo(#foo(3));
    assert o.x7[0] == 1;
    assert o.x8 == 0;
    assert o.x9 == 43;";
    assert_(p, p)
}
