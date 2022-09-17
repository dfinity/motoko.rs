use std::collections::HashMap;

use serde::Deserialize;

#[test]
fn test_convert_value() {
    #[derive(Debug, PartialEq, Deserialize)]
    struct Item {
        a: usize,
        b: (String, Option<String>),
        c: HashMap<String, u8>,
        d: Vec<isize>,
    }
    assert_eq!(
        motoko::vm::eval(r#"{ a = 5; b = ("abc", null); c = { x = 0 }; d = [var 1, 2, 3] }"#)
            .unwrap()
            .convert::<Item>()
            .unwrap(),
        Item {
            a: 5,
            b: ("abc".to_string(), None),
            c: vec![("x".to_string(), 0)].into_iter().collect(),
            d: vec![1, 2, 3],
        }
    );
}
