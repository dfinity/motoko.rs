use motoko::check::assert_lex_roundtrip as assert_;

#[test]
fn test_formatter() {
    assert_("hello world 123!");
}
