use motoko::check::{assert_parse as assert_to, assert_roundtrip as assert_};

#[test]
fn test_ids() {
    assert_("x");
    assert_("X");
    assert_("xxx_");
    assert_("xXx_01");
    assert_("xxx_01_xxX");

    // 'let' as a variable -- this is not a legal program, but we want a good parse error, so parse it as a variable.
    // currently results in a parse error
    // assert_("let");
}

#[test]
fn test_chars() {
    assert_("'h'");
}

#[test]
fn test_strings() {
    assert_("\"hello\"");
}

#[test]
fn test_paren() {
    assert_("(1)");
    assert_("(\"hello\")");
}

#[test]
fn test_tuples() {
    assert_("()");
    assert_("(1,)");
    assert_("(1, 2)");
    assert_("(1, 2,)");

    assert_("(1, 2, 3)");
    assert_("(1, 2, 3,)");

    // current trailing delimiter behavior
    assert_to("(1, )", "(1,)");
}

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
    assert_("0.");
    assert_("-0.0");
    assert_("-123.123");
}

#[test]
fn test_bools() {
    assert_("true");
    assert_("false");
}

#[test]
fn test_unary_operators() {
    assert_("-0");
    assert_("+0");
    assert_("not true");
}

#[test]
fn test_binary_operators() {
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
    assert_("return (0, 1)");
}

#[test]
fn test_array() {
    assert_("[]");
    assert_("[1]");
    assert_("[1, 2]");
    assert_("[1, 2,]");
}

#[test]
fn test_let_var() {
    assert_("let x = 0; x");
    assert_("let x : Int = 0; x");
    assert_("var x = 0; x");
    assert_("var x : Int = 0; x");
}

#[test]
fn test_nested_block() {
    assert_("{ let x = 0; x }");
    assert_("let y = { let x = 0; x }; y");
}

#[test]
fn test_variant() {
    assert_("#banana");
    assert_("#banana(0)");
    assert_("#banana(#apple)");
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
fn test_seq() {
    assert_("ignore 0; ignore 1; 0");
    assert_("ignore 0; ignore 1; 0;");
}

#[test]
fn test_ignore() {
    assert_("ignore 1");
}

#[test]
fn test_switch() {
    assert_("switch 0 { }");
    assert_("switch 0 { case _ 0 }");
    assert_("switch 0 { case _ 0; }");
    assert_("switch 0 { case (_) 0 }");
    assert_("switch 0 { case (_,) 0 }");
    assert_("switch 0 { case (_, _) 0 }");
    assert_("switch 0 { case (_, _,) 0 }");
    //assert_("switch (#apple) { case (#apple) 1 }");
    //assert_("switch (#apple) { case (#apple) 1; }");
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
