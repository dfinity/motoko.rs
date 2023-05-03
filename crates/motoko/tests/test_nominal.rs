use motoko::check::assert_vm_eval as assert_;
//use motoko::check::assert_vm_interruption as assert_x;

use test_log::test; // enable logging output for tests by default.

#[test]
fn sanity_1_plus_2_equals_3() {
    assert_("1 + 2", "3")    
}


#[test]
fn force_thunk_2() {
    assert_("force (thunk 2)", "2")
}

#[test]
fn memo_137_plus_137() {
    assert_("memo (137 + 137)", "274")
}

#[test]
fn get_put_4() {
    assert_("@(@1 := 4)", "4")
}
