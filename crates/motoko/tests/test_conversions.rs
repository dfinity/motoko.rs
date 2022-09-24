use std::collections::HashMap;
use std::fmt::Debug;

use motoko::value::ToMotoko;
use serde::{de::DeserializeOwned, Deserialize, Serialize};

fn assert<T: Debug + Eq + Serialize + DeserializeOwned>(input: &str, value: T, debug_str: &str) {
    println!("Evaluating: {}", input);
    let result: T = motoko::vm::eval_into(input).unwrap();
    assert_eq!(result, value);
    assert_eq!(format!("{:?}", result.to_motoko().unwrap()), debug_str);
}

#[test]
fn convert_struct() {
    #[derive(Debug, PartialEq, Deserialize)]
    struct Item {
        a: usize,
        b: (String, Option<String>),
        c: HashMap<String, u8>,
        d: Vec<isize>,
    }
    let expected = Item {
        a: 5,
        b: ("abc".to_string(), None),
        c: vec![("x".to_string(), 0)].into_iter().collect(),
        d: vec![1, 2, 3],
    };
    let item: Item =
        motoko::vm::eval_into(r#"{ a = 5; b = ("abc", null); c = { x = 0 }; d = [var 1, 2, 3] }"#)
            .unwrap();
    assert_eq!(expected, item);
}

#[test]
fn roundtrip_struct_enum() {
    #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
    enum Enum {
        A,
        B(u8, isize),
        C { c: Box<Enum> },
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
    struct Struct {
        e: Enum,
    }

    assert("?#A", Enum::A, r#"Variant("A", None)"#);
    assert(
        "#B (5, -5)",
        Some(Enum::B(5, -5)),
        "Option(Variant(\"B\", Some(Tuple([Nat(5), Int(-5)]))))",
    );
    assert(
        "#C { c = #A }",
        Enum::C {
            c: Box::new(Enum::A),
        },
        "Variant(\"C\", Some(Object({\"c\": FieldValue { mut_: Var, val: Variant(\"A\", None) }})))",
    );
    assert(
        "{ e = #A }",
        Struct { e: Enum::A },
        "Object({\"e\": FieldValue { mut_: Var, val: Variant(\"A\", None) }})",
    );
}

#[test]
fn roundtrip_value() {
    assert(
        "#Text \"abc\"",
        "abc".to_motoko().unwrap(),
        "Variant(\"Text\", Some(Text(String(\"abc\"))))",
    );
    assert(
        "#Tuple([#Nat 123, #Text \"abc\"])",
        (123_usize, "abc").to_motoko().unwrap(),
        "Variant(\"Tuple\", Some(Array(Var, [Variant(\"Nat\", Some(Nat(123))), Variant(\"Text\", Some(Text(String(\"abc\"))))])))",
    );
}
