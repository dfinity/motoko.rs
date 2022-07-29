use motoko::check::assert_roundtrip as assert_;

#[test]
fn test_chars() {
    assert_("'h'");
}

#[test]
fn test_strings() {
    assert_("\"hello\"");
}

#[test]
fn test_tuples() {
    assert_("()");
    assert_("(1,)");
    assert_("(1, 2)");
    assert_("(1, 2, 3)");
}

// to do -- test single-tuple function-type interaction in parsing:
//((x:Nat,)) -> ((y:Nat,))

#[test]
fn test_nats() {
    assert_("0");
    assert_("123");
}

#[test]
fn test_ints() {
    assert_("-0");
    assert_("-123");
}

#[test]
fn test_floats() {
    assert_("-0.0");
    assert_("-123.123");
}

#[test]
fn test_bools() {
    assert_("true");
    assert_("false");
}

#[test]
fn test_operators() {
    assert_("+0");
    assert_("-0");
    assert_("not true");

    assert_("0 + 0");
    assert_("0 - 0");
    assert_("0 / 0");
    assert_("0 % 0");
    assert_("0 ** 0");
    assert_("0 ^ 0"); // Xor or Cat ???
    assert_("0 << 0");
    assert_("0 >> 0");
    assert_("0 <<> 0");
    assert_("0 <>> 0");
    assert_("0 +% 0");
    assert_("0 *% 0");
    assert_("0 **% 0");
    assert_("0 = 0");
    assert_("0 != 0");
    assert_("0 < 0");
    assert_("0 > 0");
    assert_("0 >= 0");
    assert_("0 <= 0");
}

#[test]
fn test_return() {
    assert_("return 0");
}

#[test]
fn test_array() {
    assert_("[]");
    assert_("[1]");
    assert_("[1, 2]");
}

#[test]
fn test_let() {
    assert_("let x = 0; x");
}

#[test]
fn test_let_var() {
    assert_("let var x = 0; x");
}

#[test]
fn test_nested_block() {
    assert_("{ let x = 0; x }");
    assert_("let y = { let x = 0; x }; y");
}

#[test]
fn test_variant() {
    assert_("#banana")
    assert_("#banana(0)")
    assert_("#banana(#apple)")
}

#[test]
fn test_record() {
    assert_("{ }");
    assert_("{ foo = 3; }");
    assert_("{ foo = 3; bar = #apple }");
}

#[test]
fn test_assign() {
    assert_("x := 3");
}

#[test]
fn test_if() {
    assert_("if true { 1 } else { 2 }");
    assert_("if true { () };");
}

#[test]
fn test_ignore() {
    assert_("ignore 1");
}

#[test]
fn test_switch() {
    assert_("switch #apple { case (#apple) 1 }");
    //assert_("switch (#apple) { case (#apple) 1 }");
}

#[test]
fn test_record_proj() {
    assert_("x.foo");
}

#[test]
fn test_tuple_proj() {
    assert_("x.0");
}

#[test]
fn test_array_index() {
    assert_("x[0]");
}


