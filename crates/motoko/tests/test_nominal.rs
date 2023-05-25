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
fn get_put_4() {
    assert_("@(@1 := 4)", "4")
}

#[test]
fn do_at() {
    assert_("do @1 { 2 + 3 }", "5")
}

#[test]
fn force_thunk_ptr() {
    assert_("force(@1 := (thunk { 13 + 13 }))", "26")
}

#[test]
fn sym_literal_num() {
    assert_("$(1)", "$(1)")
}

#[test]
fn sym_literal_id() {
    assert_("$(foo)", "$(foo)")
}

#[test]
fn sym_literal_foo_dot_1() {
    assert_("$(foo.1)", "$(foo.1)")
}

#[test]
fn sym_literal_foo_() {
    assert_("$(foo_)", "$(foo_)")
}

#[test]
fn sym_literal_foo_bar() {
    assert_("$(foo_bar)", "$(foo_bar)")
}

#[test]
fn sym_literal_big() {
    // 3_11 is parsed as a single number, 311.
    assert_("$(foo - goo . 3 _ 11)", "$(foo-goo.3_ 11)")
}
