use motoko::check::assert_vm_eval as assert_;
use motoko::check::assert_vm_interruption as assert_x;
use motoko::vm_types::Interruption;

use test_log::test; // enable logging output for tests by default.

fn assert_is_value(v: &str) {
    assert_(v, v)
}

#[test]
fn vm_literals() {
    assert_is_value("1");
    assert_is_value("1_000");
    assert_is_value("0x123abcDEF");
    assert_("0xff", "255");
    // TODO: equality between different numeric types
    // assert_("1", "1.");
    // assert_("1_000", "1000");
    // assert_("1e3", "1000");
    // assert_("1.1e3", "1100");

    assert_is_value("#apple");
    assert_is_value("#apple(1)");

    if false {
        assert_is_value("1e3");
        assert_is_value("1.");
        assert_is_value("1_2.3_4e5_6");
    }
}

#[test]
fn vm_let() {
    assert_("let x = 1; x", "1");
    assert_("let x = 1; let y = do {let x = 666; 42}; x + y", "43");
}

#[test]
fn vm_binop() {
    assert_("1 + 1", "2");
    assert_("1 - 1", "0");
    assert_("3 - 2 - 1", "0");
    assert_("(3 - 2) - 1", "0");
    assert_("3 - (2 - 1)", "2");
}

#[test]
fn vm_switch() {
    assert_("switch (#apple) { case (#apple) { 42 } }", "42");
    assert_("switch (#apple(42)) { case (#apple(x)) { x } }", "42")
}

#[test]
fn vm_tuples() {
    assert_("(1, 2, 3)", "(1, 2, 3)");
    assert_("(1 + 1, 2 + 2, 3 + 3)", "(2, 4, 6)");
}

#[test]
fn vm_prim_ops() {
    assert_("255 + 1 : Nat", "256");
    assert_("255 +% 1 : Nat8", "0");
    assert_("(255 +% 1) +% (255 +% 1) : Nat8", "0");
    assert_x("255 +% 1", &Interruption::AmbiguousOperation);
}

#[test]
fn vm_vars() {
    assert_("var x = 1", "()");
    assert_("var x = 1; x", "1");
    assert_("var x = 1; x := 2; x", "2");
    assert_x("1 := 1", &Interruption::TypeMismatch);
}

#[test]
fn vm_tuple_proj() {
    assert_("(1, 2).0", "1");
    assert_("(1, 2).1", "2");
    assert_x("(1, 2).2", &Interruption::TypeMismatch);
}

#[test]
fn vm_if_then_else() {
    assert_("if true 1 else 2", "1");
    assert_("if false 1 else 2", "2");
    assert_x("if 1 2 else 3", &Interruption::TypeMismatch);
}

#[test]
fn vm_if_then_no_else() {
    assert_("var x = 0; if true { x := 1 } \\no_else; x", "1");
}

#[test]
fn vm_equals() {
    assert_("1 == 1", "true");
    assert_("1 == 2", "false");
    assert_("1 + 1 == 2", "true");
}

#[test]
fn vm_not_equals() {
    assert_("1 != 1", "false");
    assert_("1 != 2", "true");
    assert_("1 + 1 != 2", "false");
    assert_("1 + 2 != 2", "true");
    assert_("1 != 2 - 1", "false");
    assert_("1 != 2 + 1", "true");
}

#[test]
fn vm_assert() {
    assert_("assert true", "()");
    assert_x("assert false", &Interruption::AssertionFailure);
    assert_x("assert 0", &Interruption::TypeMismatch);
    assert_x("assert 1", &Interruption::TypeMismatch);
}

#[test]
fn vm_while() {
    assert_("var x = 0; while (x != 1) { x := 1 }; x", "1");
    assert_(
        "var x = 0; var y = 1; while (x != 100) { x := (x + 1); y := (y * 2) }; y",
        "1267650600228229401496703205376",
    );
    assert_(
        "var x = 0; var y = 1; while (x != 100) { x := x + 1; y := y * 2 }; y",
        "1267650600228229401496703205376",
    );
    assert_x("while 1 { }", &Interruption::TypeMismatch);
    assert_x("while true { 1 }", &Interruption::TypeMismatch);
}

#[test]
fn vm_array() {
    assert_("[0, 1, 2]", "[0, 1, 2]");
    assert_("[0, 1, 2][2]", "2");
    assert_x("[0, 1, 2][3]", &Interruption::IndexOutOfBounds);
    assert_("[var 0, 1, 2]", "[var 0, 1, 2]");
    assert_("[var 0, 1, 2][2]", "2");
    assert_("[var 0, 1, 2][2] := 11", "()");
    assert_("let x = [var 0, 1, 2]; x[1] := 3; x[1]", "3");
    assert_x(
        "let x = [var 0, 1, 2]; x[3] := 3",
        &Interruption::IndexOutOfBounds,
    );
}

#[test]
fn vm_records() {
    assert_("{ x = 3; y = 5; z = 8 }", "{ x = 3; y = 5; z = 8 }");
    assert_("{ x = 3; y = 5; z = 8 }", "{ z = 8; y = 5; x = 3 }");

    assert_("{ x = 3 }.x", "3");
    assert_("let x = { x = 3 }; x.x", "3");

    assert_("{ var x = 0 }.x := 1", "()");
    assert_("let r = { var r = 0 }; r.r := 1; r.r", "1");

    // to do -- need to traverse pointers for equality check to works
    if false {
        assert_("{ var z = 8; var y = 5 }", "{ var y = 5; var z = 8 }");
    }

    // to do -- equality that is quotiented by type annotations that narrow the type.
    if false {
        assert_(
            "let r1 : { x : Nat } = { x = 3; y = 5}; r1 == { x = 3}",
            "true",
        );
    }
}

#[test]
fn vm_boolean_ops() {
    assert_("false or true", "true");
    assert_("true or (do { while true { } ; false })", "true");
    assert_x("false or 1", &Interruption::TypeMismatch);
    assert_x("1 or true", &Interruption::TypeMismatch);

    assert_("true and false", "false");
    assert_("false and (do { while true { } ; false })", "false");
    assert_x("true and 1", &Interruption::TypeMismatch);
    assert_x("1 and true", &Interruption::TypeMismatch);

    assert_("not false", "true");
    assert_("not true", "false");
}

#[test]
fn vm_option_monad() {
    assert_("?(3)", "? 3");
    assert_("do ? { 3 }", "?3");
    assert_("do ? { (?3)! }", "?3");
    assert_("do ? { null! }", "null");
    assert_("do ? { null! ; while true { } }", "null");
    assert_x("do ? { 3! }", &Interruption::TypeMismatch);
    assert_x("null!", &Interruption::NoDoQuestBangNull);
}

#[test]
fn function_call() {
    assert_("func f (x: Nat) : Nat { x }; f 3", "3");
    assert_("func f ( x ) { x }; f 3", "3");
    assert_("let y = 3; func f ( x ) { y }; f 4", "3");
}

#[test]
fn function_rec_call() {
    assert_(
        "(func f (x) { if (x == 0) { 123 } else { f (x - 1) } }) 1",
        "123",
    );
}

#[test]
fn return_() {
    assert_x("return 3", &Interruption::MisplacedReturn);
    assert_("func f ( x ) { return x }; f 3", "3");
    assert_("func f ( x ) { return x; while true { } }; f 3", "3");
    assert_("let y = 3; func f ( x ) { return y }; f 4", "3");
}

#[test]
fn ignore() {
    assert_("ignore 3", "()");
    assert_("ignore 1 + 1", "()");
}

#[test]
fn debug() {
    assert_("debug { () }", "()");
    assert_x("debug { 3 }", &Interruption::TypeMismatch);
}

#[test]
fn for_() {
    assert_("for (i in { next = func () { null } }) { }", "()");
    assert_(
        "for (i in { next = func () { null } }) { while true { } }",
        "()",
    );
    assert_(
        "var x = 13; var c = 0; let i = { next = func () { if (x == 0) { null } else { x := x - 1; c := c + 1; ?x } } }; for (j in i) { let _ = j; }; c", "13");
}

#[ignore]
#[test]
fn module() {
    let p = "module X { public let x = 5; let y = (1, 2); func f () { } }";
    assert_(p, p)
}

#[ignore]
#[test]
fn actor() {
    let p = "actor A { public func f () { } }";
    assert_(p, p);
}

#[test]
fn actor_upgrade_demo_with_counter_inc() {
    let p = "actor Counter = {
               var x = 0;
               public func get() /*: async Nat*/ { x };
               public func inc() { x := x + 1 };
             };
             assert (Counter.get() == 0);
             Counter.inc();
             assert (Counter.get() == 1);
             actor Counter {
               var x = 0;
               public func get() /*: async Nat*/ { x };
               public func inc() { x := x + 2 };
             };
             assert (Counter.get() == 1);
             Counter.inc();
             assert (Counter.get() == 3);
             #ok
";
    assert_(p, "#ok");
}

#[test]
fn prim_debug_print() {
    assert_("prim \"debugPrint\" \"hello, world\"", "()");
    assert_(
        "let Debug = { print = prim \"debugPrint\" }; Debug.print \"hello, world\"",
        "()",
    );
}

#[test]
fn test_nat_to_text() {
    assert_("prim \"natToText\" 666", "\"666\"");
}

#[test]
#[cfg(feature = "to-motoko")]
#[cfg(feature = "value-reflection")]
fn prim_reify_value() {
    assert_("prim \"reifyValue\" (#abc)", "#Variant(\"abc\", null)");
    assert_(
        "prim \"reifyValue\" (#abc 123)",
        "#Variant(\"abc\", ?(#Nat 123))",
    );
}

#[test]
#[cfg(feature = "value-reflection")]
fn prim_reflect_value() {
    assert_("prim \"reflectValue\" (#Text \"hello\")", "\"hello\"");
    assert_("prim \"reflectValue\" (#Function { env = (prim \"hashMapNew\" ()); content = { input = (#Wild, { source_type = \"Known\"; span = { start = 5; end = 6 }; line = 1; col = 6 }); exp = (#Literal(#Unit), { source_type = \"Known\"; span = { start = 9; end = 11 }; line = 1; col = 10 }); sugar = true } })", "func _ = ()");
}

#[test]
#[cfg(feature = "to-motoko")]
#[cfg(feature = "agent-reflection")]
fn prim_reify_agent() {
    // assert_("let x = 0; prim \"hashMapGet\" ((prim \"reifyAgent\" ()).env, \"x\")", "?#Nat(0)");
    assert_(
        "let x = 0; let agent = (prim \"reifyAgent\" ()); agent.env",
        "[var (\"x\", #Nat(0))]",
    );
}

#[test]
#[cfg(feature = "agent-reflection")]
fn prim_reflect_agent() {
    // assert_("var x = 0; let agent = prim \"reifyAgent\" (); x := 1; prim \"reflectAgent\" (agent); x", "0");
    assert_(
        r#"
            prim "reflectAgent" ({
                cont_source = { source_type = "Unknown" };
                cont = { cont_type = "Value"; value = #Nat(123) };
                env = [("x", #Nat(0))];
                stack = [];
                store = (prim "hashMapNew" ());
                debug_print_out = [];
                counts = {
                    step = 0;
                    redex = 0;
                };
            });
          "#,
        "123",
    );
}

#[test]
fn prim_collection_hashmap() {
    let p = "let hm = prim \"hashMapNew\" (); let hm2 = prim \"hashMapPut\" (hm, 1, 2); let hm3 = prim \"hashMapPut\" (hm2.0, 2, 3); (hm, hm2, hm3)";
    assert_(p, p);

    assert_(
        "let (hm, old) = prim \"hashMapPut\" (prim \"hashMapNew\" (), 1, 2); old",
        "null",
    );

    assert_("let (hm, _) = prim \"hashMapPut\" (prim \"hashMapNew\" (), 1, 2); prim \"hashMapGet\" (hm, 1)",
           "?2");

    assert_("let (hm, _) = prim \"hashMapPut\" (prim \"hashMapNew\" (), 1, 2); (prim \"hashMapPut\" (hm, 1, 3)).1",
           "?2");

    assert_("var _hm = (prim \"hashMapNew\") (); let hm = { put = func (k_, v_) { let (hm__, v) = prim \"hashMapPut\" (_hm, k_, v_);  _hm := hm__; };  get = func k { prim \"hashMapGet\" (_hm, k) } }; let _ = hm . put (1, 2) ; let _ = hm.get(1) ; let _ = hm.put(2, 3) ; (hm.get(1), hm.get(2))", "(?2, ?3)")
}

#[test]
fn fastranditer() {
    assert_(
        "var i = prim \"fastRandIterNew\" (null, 33); let x = (prim \"fastRandIterNext\" i); (x, (prim \"fastRandIterNext\" i))",
        "(?1592943, ?1731023874)",
    );
}

#[test]
fn fastranditer_fastfor() {
    assert_("for (x in prim \"fastRandIterNew\" (?3, 3)) { }", "()");
}

#[test]
fn function_call_return_restores_env() {
    assert_("func f() { }; let x = 0; x", "0");
    assert_("func f() { }; let x = 0; f(); x", "0");
}

#[test]
fn demo_redex_stepping() {
    let prog = r#"
let a = 1;
(prim "debugPrint") "Hello, VM 1!";
(prim "debugPrint") "Hello, VM 2!";
(prim "debugPrint") "Hello, VM 3!";
(prim "debugPrint") "Hello, VM 4!";
var y = 666;
(prim "debugPrint") "Hello, VM 5!";
(prim "debugPrint") "Hello, VM 6!";
(prim "debugPrint") "Hello, VM 7!";
var x = y + a;
(prim "debugPrint") "Hello, VM 8!";
(prim "debugPrint") "Hello, VM 9!";
x + 1;
"#;
    assert_(prog, "668");
}

#[test]
fn demo_simple_recursion() {
    let prog = r#"
var x = true;
func f() {
  if x {
    x := false;
    f()
  } else {
    x
  }
};
f()
"#;
    assert_(prog, "false");
}

#[test]
fn demo_for_() {
    let prog = r#"
let Debug = { print = prim "debugPrint"};
var x = 0;
let Iter = { range = func(end){
  { next = func() {
  if (x == end) {
    null
  } else {
    let x_ = x;
    x := x_ + 1;
    ?x_
  }}}}};
let i = Iter.range(3);
var sum = 0;
for (y in i) {
  sum := sum + 1;
  Debug.print sum
}"#;
    assert_(prog, "()");
}

#[test]
fn demo_more_recursion() {
    let prog = r#"
var x = 0;
func f() {
  if (x != 666) {
    x := x + 1;
    f()
  } else {
    x
  }
};
f()
"#;
    assert_(prog, "666");
}

#[test]
fn test_agent_eval() {
    let mut agent = motoko::vm_types::Agent::empty();
    agent.eval("var x = 1").expect("oops");
    agent.eval("var y = x + 1").expect("oops");
    let y = agent.eval("y").expect("oops");
    assert_eq!(
        &*y,
        &motoko::value::Value::Nat(num_bigint::BigUint::from(2_u32))
    )
}
