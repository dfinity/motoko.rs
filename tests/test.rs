use motoko::check::assert_roundtrip;

#[test]
fn test_examples() {
    assert_roundtrip("0");
    assert_roundtrip("123");
    assert_roundtrip("return 0");
}
