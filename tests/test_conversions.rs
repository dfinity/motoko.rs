use std::collections::HashMap;
use std::fmt::Debug;

use motoko::value::Value;
use serde::{de::DeserializeOwned, Deserialize, Serialize};

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

    // use motoko::value::Value;
    // assert_eq!(
    //     motoko::vm::eval_into::<Value>("[0]").unwrap(),
    //     Value::Tuple(im_rc::Vector::from(vec![Value::Nat(0_usize.into())])),
    // );
}

#[test]
fn roundtrip() {
    fn assert<T: Debug + Eq + Serialize + DeserializeOwned>(
        input: &str,
        value: T,
        debug_str: &str,
    ) {
        println!("Evaluating: {}", input);

        let result: T = motoko::vm::eval_into(input).unwrap();

        assert_eq!(result, value);

        assert_eq!(format!("{:?}", Value::from_rust(value).unwrap()), debug_str);
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
    enum Enum {
        A,
        B(u8, isize),
        C { c: Box<Enum> },
    }

    assert("#A", Enum::A, r#"Variant("A", None)"#);
    assert(
        "#B (5, -5)",
        Enum::B(5, -5),
        r#"Variant("B", Some((-5, 5)))"#,
    );
    assert(
        "#C { c = #A }",
        Enum::C {
            c: Box::new(Enum::A),
        },
        r#"Variant("C", Some({c: #A}))"#,
    );
}
