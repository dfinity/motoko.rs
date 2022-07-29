use motoko::check::assert_roundtrip;

#[test]
fn test_numbers() {
    assert_roundtrip("0");
    assert_roundtrip("123");
}

#[test]
fn test_bools() {
    assert_roundtrip("true");
    assert_roundtrip("false");
}

#[test]
fn test_operators() {
    assert_roundtrip("+0");
    assert_roundtrip("-0");
    assert_roundtrip("not true");

    assert_roundtrip("0 + 0");
    assert_roundtrip("0 - 0");
    assert_roundtrip("0 / 0");
    assert_roundtrip("0 % 0");
    assert_roundtrip("0 ** 0");
    assert_roundtrip("0 ^ 0"); // Xor or Cat ???
    assert_roundtrip("0 << 0");
    assert_roundtrip("0 >> 0");
    assert_roundtrip("0 <<> 0");
    assert_roundtrip("0 <>> 0");
    assert_roundtrip("0 +% 0");
    assert_roundtrip("0 *% 0");
    assert_roundtrip("0 **% 0");
    assert_roundtrip("0 = 0");
    assert_roundtrip("0 != 0");
    assert_roundtrip("0 < 0");
    assert_roundtrip("0 > 0");
    assert_roundtrip("0 >= 0");
    assert_roundtrip("0 <= 0");
}

#[test]
fn test_return() {
    assert_roundtrip("return 0");
}
