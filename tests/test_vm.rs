use motoko::check::assert_vm_eval as assert_;
use motoko::check::assert_vm_interruption as assert_x;
use motoko::vm_types::Interruption;

use test_log::test; // enable logging output for tests by default.

fn assert_is_value(v: &str) {
    assert_(v, v)
}

#[test]
fn vm_literals() {
    assert_is_value("1");
    assert_is_value("1.");
    assert_is_value("1_000");
    assert_is_value("1e3");
    assert_is_value("1_2.3_4e5_6");
    assert_is_value("0x123abcDEF");
    assert_("0xff", "255");
    // TODO: equality between different numeric types
    // assert_("1", "1.");
    // assert_("1_000", "1000");
    // assert_("1e3", "1000");
    // assert_("1.1e3", "1100");

    assert_is_value("#apple");
    assert_is_value("#apple(1)");
}

#[test]
fn vm_let() {
    assert_("let x = 1; x", "1");
    assert_("let x = 1; let y = do {let x = 666; 42}; x + y", "43");
}

#[test]
fn vm_binop() {
    assert_("1 + 1", "2");
    assert_("1 - 1", "0");
    assert_("3 - 2 - 1", "0");
    assert_("(3 - 2) - 1", "0");
    assert_("3 - (2 - 1)", "2");
}

#[test]
fn vm_switch() {
    assert_("switch (#apple) { case (#apple) { 42 } }", "42");
    assert_("switch (#apple(42)) { case (#apple(x)) { x } }", "42")
}

#[test]
fn vm_tuples() {
    assert_("(1, 2, 3)", "(1, 2, 3)");
    assert_("(1 + 1, 2 + 2, 3 + 3)", "(2, 4, 6)");
}

#[test]
fn vm_prim_ops() {
    assert_("255 + 1 : Nat", "256");
    assert_("255 +% 1 : Nat8", "0");
    assert_("(255 +% 1) +% (255 +% 1) : Nat8", "0");
    assert_x("255 +% 1", &Interruption::AmbiguousOperation);
}

#[test]
fn vm_vars() {
    assert_("var x = 1", "()");
    assert_("var x = 1; x", "1");
    assert_("var x = 1; x := 2; x", "2");
    assert_x("1 := 1", &Interruption::TypeMismatch);
}

#[test]
fn vm_tuple_proj() {
    assert_("(1, 2).0", "1");
    assert_("(1, 2).1", "2");
    assert_x("(1, 2).2", &Interruption::TypeMismatch);
}

#[test]
fn vm_if_then_else() {
    assert_("if true 1 else 2", "1");
    assert_("if false 1 else 2", "2");
    assert_x("if 1 2 else 3", &Interruption::TypeMismatch);
}

#[test]
fn vm_if_then_no_else() {
    assert_("var x = 0; if true { x := 1 } \\no_else; x", "1");
}

#[test]
fn vm_equals() {
    assert_("1 == 1", "true");
    assert_("1 == 2", "false");
    assert_("1 + 1 == 2", "true");
}

#[test]
fn vm_not_equals() {
    assert_("1 != 1", "false");
    assert_("1 != 2", "true");
    assert_("1 + 1 != 2", "false");
    assert_("1 + 2 != 2", "true");
    assert_("1 != 2 - 1", "false");
    assert_("1 != 2 + 1", "true");
}

#[test]
fn vm_assert() {
    assert_("assert true", "()");
    assert_x("assert false", &Interruption::AssertionFailure);
    assert_x("assert 0", &Interruption::TypeMismatch);
    assert_x("assert 1", &Interruption::TypeMismatch);
}

#[test]
fn vm_while() {
    assert_("var x = 0; while (x != 1) { x := 1 }; x", "1");
    assert_(
        "var x = 0; var y = 1; while (x != 100) { x := (x + 1); y := (y * 2) }; y",
        "1267650600228229401496703205376",
    );
    assert_(
        "var x = 0; var y = 1; while (x != 100) { x := x + 1; y := y * 2 }; y",
        "1267650600228229401496703205376",
    );
    assert_x("while 1 { }", &Interruption::TypeMismatch);
    assert_x("while true { 1 }", &Interruption::TypeMismatch);
}

#[test]
fn vm_array() {
    assert_("[0, 1, 2]", "[0, 1, 2]");
    assert_("[0, 1, 2][2]", "2");
    assert_x("[0, 1, 2][3]", &Interruption::IndexOutOfBounds);
    assert_("[var 0, 1, 2]", "[var 0, 1, 2]");
    assert_("[var 0, 1, 2][2]", "2");
    assert_("[var 0, 1, 2][2] := 11", "()");
    assert_("let x = [var 0, 1, 2]; x[1] := 3; x[1]", "3");
    assert_x(
        "let x = [var 0, 1, 2]; x[3] := 3",
        &Interruption::IndexOutOfBounds,
    );
}

#[test]
fn vm_records() {
    assert_("{ x = 3; y = 5; z = 8 }", "{ x = 3; y = 5; z = 8 }");
    assert_("{ x = 3; y = 5; z = 8 }", "{ z = 8; y = 5; x = 3 }");

    assert_("{ x = 3 }.x", "3");
    assert_("let x = { x = 3 }; x.x", "3");

    assert_("{ var x = 0 }.x := 1", "()");
    assert_("let r = { var r = 0 }; r.r := 1; r.r", "1");

    // to do -- need to traverse pointers for equality check to works
    if false {
        assert_("{ var z = 8; var y = 5 }", "{ var y = 5; var z = 8 }");
    }

    // to do -- equality that is quotiented by type annotations that narrow the type.
    if false {
        assert_(
            "let r1 : { x : Nat } = { x = 3; y = 5}; r1 == { x = 3}",
            "true",
        );
    }
}

#[test]
fn vm_boolean_ops() {
    assert_("false or true", "true");
    assert_("true or (do { while true { } ; false })", "true");
    assert_x("false or 1", &Interruption::TypeMismatch);
    assert_x("1 or true", &Interruption::TypeMismatch);

    assert_("true and false", "false");
    assert_("false and (do { while true { } ; false })", "false");
    assert_x("true and 1", &Interruption::TypeMismatch);
    assert_x("1 and true", &Interruption::TypeMismatch);

    assert_("not false", "true");
    assert_("not true", "false");
}

#[test]
fn vm_option_monad() {
    assert_("?(3)", "? 3");
    assert_("do ? { 3 }", "?3");
    assert_("do ? { (?3)! }", "?3");
    assert_("do ? { null! }", "null");
    assert_("do ? { null! ; while true { } }", "null");
    assert_x("do ? { 3! }", &Interruption::TypeMismatch);
    assert_x("null!", &Interruption::NoDoQuestBangNull);
}

#[test]
fn function_call() {
    assert_("func f (x: Nat) : Nat { x }; f 3", "3");
    assert_("func f ( x ) { x }; f 3", "3");
    assert_("let y = 3; func f ( x ) { y }; f 4", "3");
}

#[test]
fn function_rec_call() {
    assert_(
        "(func f (x) { if (x == 0) { 123 } else { f (x - 1) } }) 1",
        "123",
    );
}

#[test]
fn return_() {
    assert_x("return 3", &Interruption::MisplacedReturn);
    assert_("func f ( x ) { return x }; f 3", "3");
    assert_("func f ( x ) { return x; while true { } }; f 3", "3");
    assert_("let y = 3; func f ( x ) { return y }; f 4", "3");
}

#[test]
fn ignore() {
    assert_("ignore 3", "()");
    assert_("ignore 1 + 1", "()");
}

#[test]
fn debug() {
    assert_("debug { () }", "()");
    assert_x("debug { 3 }", &Interruption::TypeMismatch);
}

#[test]
fn for_() {
    assert_("for (i in { next = func () { null } }) { }", "()");
    assert_(
        "for (i in { next = func () { null } }) { while true { } }",
        "()",
    );
    assert_(
        "var x = 13; var c = 0; let i = { next = func () { if (x == 0) { null } else { x := x - 1; c := c + 1; ?x } } }; for (j in i) { let _ = j; }; c", "13");
}

#[test]
fn module() {
    assert_("module X { public let x = 5; let y = (1, 2); func f () { } }",
            "module X { public let x = 5; let y = (1, 2); func f () { } }");
}
