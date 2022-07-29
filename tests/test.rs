use motoko::check::assert_roundtrip;

#[test]
fn test_examples() {
    assert_roundtrip("return 0");
}
