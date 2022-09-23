use motoko::value::Value;
use motoko_proc_macro::parse_static;

#[test]
fn primitive() {
    let x = 567;
    let exp: Value = parse_static!(
        "
            123
        "
    );
    println!("{:?}", exp);
    // assert_eq!(value, Value::Nat(123.into()));
}

// #[test]
// fn function() {
//     let value: Value = eval!(
//         "
//             func() {
//                 123
//             }
//         "
//     );
//     assert_eq!(value, );
// }
