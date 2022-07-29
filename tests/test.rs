use motoko::check::assert_parse;

#[test]
fn test_let() {
    assert_parse("let a = 0; a;", "TODO");
}
