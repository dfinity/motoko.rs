#![cfg(feature = "to-motoko")]

use std::collections::HashMap;
use std::fmt::Debug;

use motoko::shared::Share;
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
    assert("#Unit", ().to_motoko().unwrap(), "Variant(\"Unit\", None)");
    assert(
        "#Null",
        None::<()>.to_motoko().unwrap(),
        "Variant(\"Null\", None)",
    );
    assert(
        "#Bool(true)",
        true.to_motoko().unwrap(),
        "Variant(\"Bool\", Some(Bool(true)))",
    );
    assert(
        "#Nat(5)",
        5_usize.to_motoko().unwrap(),
        "Variant(\"Nat\", Some(Nat(5)))",
    );
    assert(
        "#Int(-5)",
        (-5).to_motoko().unwrap(),
        "Variant(\"Int\", Some(Int(-5)))",
    );
    assert(
        "#Float(0.1)",
        0.1.to_motoko().unwrap(),
        "Variant(\"Float\", Some(Float(OrderedFloat(0.1))))",
    );
    assert(
        "#Char('a')",
        'a'.to_motoko().unwrap(),
        "Variant(\"Char\", Some(Char('a')))",
    );
    assert(
        "#Text(\"abc\")",
        "abc".to_motoko().unwrap(),
        "Variant(\"Text\", Some(Text(String(\"abc\"))))",
    );
    // TODO: Blob
    assert(
        "#Array(#Var, [])",
        Vec::<()>::new().to_motoko().unwrap(),
        "Variant(\"Array\", Some(Tuple([Variant(\"Var\", None), Array(Var, [])])))",
    );
    assert(
        "#Tuple([#Nat 123, #Text \"abc\"])",
        (123_usize, "abc").to_motoko().unwrap(),
        "Variant(\"Tuple\", Some(Array(Var, [Variant(\"Nat\", Some(Nat(123))), Variant(\"Text\", Some(Text(String(\"abc\"))))])))",
    );
    assert(
        // "#Object { x = { mut = #Var; val = #Int(0) } }",
        "#Object {}",
        {
            #[derive(Serialize, Deserialize)]
            struct Struct {
                // x:isize,
            }
            Struct {
                // x:0,
            }
        }
        .to_motoko()
        .unwrap(),
        // "Variant(\"Object\", Some(Collection(HashMap({Text(String(\"x\")): Object({\"mut\": FieldValue { mut_: Var, val: Variant(\"Var\", None) }, \"val\": FieldValue { mut_: Var, val: Variant(\"Int\", Some(Int(0))) }})}))))",
        "Variant(\"Object\", Some(Collection(HashMap({}))))",
    );
    assert(
        "#Option(#Null)",
        Some(None::<()>).to_motoko().unwrap(),
        "Variant(\"Option\", Some(Variant(\"Null\", None)))",
    );
    assert(
        "#Variant(\"Abc\", null)",
        {
            #[derive(Serialize, Deserialize)]
            enum Enum {
                Abc,
            }
            Enum::Abc
        }
        .to_motoko()
        .unwrap(),
        "Variant(\"Variant\", Some(Tuple([Text(String(\"Abc\")), Null])))",
    );
    // assert(
    //     "#Pointer(123)",
    //     motoko::value::Value::Pointer(motoko::vm_types::Pointer(123)),
    //     "Variant(\"Pointer\", Some(Pointer(Pointer(123))))",
    // );
    assert(
        "#Index(123, #Nat 1)",
        // (123_usize, 1_usize).to_motoko().unwrap(),
        motoko::value::Value::Index(
            motoko::vm_types::Pointer {
                owner: motoko::vm_types::ScheduleChoice::Agent,
                local: motoko::vm_types::LocalPointer::Numeric(motoko::vm_types::NumericPointer(
                    123,
                )),
            },
            1_usize.to_motoko().unwrap().share(),
        ),
        "Variant(\"Index\", Some(Tuple([Nat(123), Variant(\"Nat\", Some(Nat(1)))])))",
    );
    // assert(
    //     "#Function { env = {}; content = { input = (#Wild, { source_type = \"Unknown\" }); exp = (#Literal(#Unit), { source_type = \"Unknown\" }) } }",
    //     (motoko::value::Closed {
    //         env: im_rc::HashMap::new(),
    //         content: motoko::ast::Function {
    //             name: None,
    //             shared: None,
    //             binds: None,
    //             input: motoko::ast::Node::without_source(motoko::ast::Pat::Wild),
    //             output:None,
    //             sugar: Default::default(),
    //             exp: motoko::ast::Node::without_source(motoko::ast::Exp::Literal(motoko::ast::Literal::Unit)),
    //         },
    //     })
    //     .to_motoko()
    //     .unwrap(),
    //     "",
    // );
}
