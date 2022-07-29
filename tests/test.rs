use motoko::check::assert_roundtrip;

#[test]
fn test_chars() {
    assert_roundtrip("'h'");
}

#[test]
fn test_strings() {
    assert_roundtrip("\"hello\"");
}

#[test]
fn test_unit() {
    assert_roundtrip("()");
}

#[test]
fn test_nats() {
    assert_roundtrip("0");
    assert_roundtrip("123");
}

#[test]
fn test_ints() {
    assert_roundtrip("-0");
    assert_roundtrip("-123");
}

#[test]
fn test_floats() {
    assert_roundtrip("-0.0");
    assert_roundtrip("-123.123");
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
