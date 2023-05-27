use motoko::check::assert_vm_eval as assert_;
//use motoko::check::assert_vm_interruption as assert_x;

use test_log::test; // enable logging output for tests by default.

#[test]
fn memo_13_plus_13() {
    // depdency graph of one node, named `13 + 13`, holding 26.
    assert_("memo {13 + 13}", "26")
}

#[test]
fn force_thunk_13_plus_13() {
    // no depdency graph here:
    assert_("force (thunk { 13 + 13 })", "26")
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
fn sym_literal_foo_space_under() {
    // space works with parens, but is a binary composition (not single Id).
    assert_("$(foo _)", "$(foo _)")
}

#[test]
fn sym_literal_foo_space_under_bar() {
    // under as ternary composition (not single Id).
    assert_("$(foo _ bar)", "$(foo _ bar)")
}

#[test]
fn sym_literal_foo_space_under_vs_no_space() {
    // $foo_ is not the same as $(foo _)
    // assert_not_equal("$(foo _)", "$(foo_)")
}

#[test]
fn sym_literal_big() {
    // 3_11 is parsed as a single number, 311.
    assert_("$(foo - goo . 3 _ 11)", "$(foo-goo.3_ 11)")
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
