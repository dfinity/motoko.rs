use motoko::check::assert_vm_eval as assert_;
//use motoko::check::assert_vm_interruption as assert_x;

use test_log::test; // enable logging output for tests by default.

#[test]
fn sanity_1_plus_2_equals_3() {
    assert_("1 + 2", "3")
}

#[test]
fn force_thunk_2() {
    assert_("force (thunk { 2 })", "2")
}

#[test]
fn memo_137_plus_137() {
    assert_("memo {137 + 137}", "274")
}

#[test]
fn get_at_1_put_4() {
    assert_("@(@1 := 4)", "4")
}

#[test]
fn get_at_x_put_4() {
    assert_("@(@$x := 4)", "4")
}

#[test]
fn do_at_1_2_plus_3() {
    assert_("do @1 { 2 + 3 }", "5")
}

#[test]
fn force_thunk_ptr() {
    assert_("force(@1 := (thunk { 13 + 13 }))", "26")
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
