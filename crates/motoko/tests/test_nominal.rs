use motoko::check::assert_vm_eval as assert_;
//use motoko::check::assert_vm_interruption as assert_x;

use test_log::test; // enable logging output for tests by default.

#[test]
fn memo_13_plus_13() {
    // depdency graph of one node, named `13 + 13`, holding 26.
    assert_("@(memo {13 + 13})", "26")
}

#[test]
fn force_memo_thunk_13_plus_13() {
    // depdency graph of one node, named `13 + 13`, holding 26.
    assert_("force (memo {thunk {13 + 13 }})", "26")
}

#[test]
fn force_thunk_13_plus_13() {
    // no depdency graph here:
    assert_("force (thunk { 13 + 13 })", "26")
}

#[test]
fn force_thunk_return_26() {
    // no depdency graph here:
    assert_("force (thunk { return 26; return \"wrong\" })", "26")
}

#[test]
fn post_force_thunk_restores_prior_env() {
    // no depdency graph here:
    assert_("let t = thunk { 0 }; let x = 26; force t; x", "26")
}

#[test]
fn double_force_thunk_13_plus_13() {
    // no depdency graph here:
    assert_(
        "let t = memo{thunk{13 + 13}}; (force t, force t)",
        "(26, 26)",
    )
}

#[test]
fn force_thunk_ptr_13_plus_13() {
    // depdency graph of one node, named $1.
    assert_("force(@1 := (thunk { 13 + 13 }))", "26")
}

#[test]
fn do_at_1_13_plus_13() {
    // depdency graph of one node, named $1.
    assert_("do @1 { 13 + 13 }", "26")
}

#[test]
fn get_at_1_put_26() {
    // depdency graph of one node, named $1.
    // holds value 26.
    // to do -- elim need for parens around 13 + 13.
    assert_("@(@1 := (13 + 13))", "26")
}

#[test]
fn get_at_x_put_26() {
    // depdency graph of one node, named $x.
    // holds value 26.
    // to do -- elim need for parens around 13 + 13.
    assert_("@(@$x := (13 + 13))", "26")
}

#[test]
fn sym_literal_num() {
    assert_("$(1)", "$1")
}

#[test]
fn sym_literal_id() {
    // parens are optional for $foo
    assert_("$(foo)", "$foo")
}

#[test]
fn sym_literal_foo_dot_1() {
    // need parens for dot to work.
    assert_("$(foo.1)", "$(foo.1)")
}

#[test]
fn sym_literal_foo_() {
    // parens are optional for $foo_
    assert_("$(foo_)", "$foo_")
}

#[test]
fn sym_literal_foo_bar() {
    // parens are optional for $foo_bar
    assert_("$(foo_bar)", "$foo_bar")
}

#[test]
fn sym_literal_big() {
    // 3_11 is parsed as a single number, 311.
    assert_("$(foo - goo_ . 3_11)", "$(foo-goo_.311)")
}

#[test]
fn sym_dash_composition_operator() {
    assert_("$foo - $bar", "$(foo-bar)")
}

#[test]
fn sym_dash_composition_operator_twice() {
    assert_("$foo - $bar - $baz", "$(foo-bar-baz)");
}

#[test]
fn sym_dash_composition_operator_associates_left() {
    // '-' associates to the left.
    assert_("( $foo - $bar ) - $baz", "$(foo-bar-baz)")
}

#[test]
fn sym_dot_composition_operator() {
    assert_("$foo . $bar", "$(foo.bar)")
}

#[test]
fn sym_dot_composition_operator_twice() {
    assert_("$foo . $bar . $baz", "$(foo.bar.baz)");
}

#[test]
fn sym_dot_composition_operator_associates_left() {
    // '.' associates to the left.
    assert_("( $foo . $bar ) . $baz", "$(foo.bar.baz)")
}

#[test]
fn sym_dot_dash_composition_precedence() {
    // dot binds tighter than dash.
    // dash binds looser than dot.
    assert_(
        "$foo . $bar - $baz . $goo",
        "( $foo . $bar )  - ( $baz  . $goo )",
    )
}
