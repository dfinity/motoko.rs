use motoko::check::assert_vm_run as assert_;

fn assert_is_value(v: &str) {
    assert_(v, v)
}

#[test]
fn test_literals() {
    assert_is_value("1");
    assert_is_value("#apple");
    assert_is_value("#apple(1)");
}

#[test]
fn test_let() {
    assert_("let x = 1; x", "1");
}
