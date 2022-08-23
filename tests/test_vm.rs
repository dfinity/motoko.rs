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
fn vm_if() {
    assert_("if true 1 else 2", "1");
    assert_("if false 1 else 2", "2");
    assert_x("if 1 2 else 3", &Interruption::TypeMismatch);
}
