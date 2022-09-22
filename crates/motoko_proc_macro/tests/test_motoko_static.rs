use motoko::{value::Value};
use motoko_proc_macro::eval;

#[test]
fn example() {
    let value: Value = eval!(
        "
            123
        "
    );
    assert_eq!(value, Value::Nat(123.into()));
}
