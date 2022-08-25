use motoko::check::assert_vm_eval as assert_;
use motoko::check::assert_vm_interruption as assert_x;
use motoko::vm_types::Interruption;

fn assert_is_value(v: &str) {
    assert_(v, v)
}

#[test]
fn vm_literals() {
    assert_is_value("1");
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
