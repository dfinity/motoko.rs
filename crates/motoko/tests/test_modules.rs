use motoko::check::assert_vm_eval as assert_;
//use motoko::check::assert_vm_interruption as assert_x;

use test_log::test; // enable logging output for tests by default.

#[test]
fn module() {
    let p = "
    module M {
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
    };
    assert M.x1.0 == 1;
    assert M.x2.0 == 1;
    assert M.x3 () == ();
    assert M.x4 == #foo(#foo(#foo(3)));
    assert M.x5 == #foo(3);
    assert M.x6 == #foo(#foo(3));
    assert M.x7[0] == 1;
    assert M.x8 == 0;";
    assert_(p, p)
}

#[test]
fn nested_module() {
    let p = "
    module M {
      public module M {
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
      };
    };
    assert M.M.x1.0 == 1;
    assert M.M.x2.0 == 1;
    assert M.M.x3 () == ();
    assert M.M.x4 == #foo(#foo(#foo(3)));
    assert M.M.x5 == #foo(3);
    assert M.M.x6 == #foo(#foo(3));
    assert M.M.x7[0] == 1;
    assert M.M.x8 == 0;";
    assert_(p, p)
}
