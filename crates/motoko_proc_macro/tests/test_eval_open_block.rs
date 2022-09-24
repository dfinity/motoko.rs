use motoko::ast::Prog;
use motoko::value::Value;
use motoko::vm_types::{Core, Limits};
use motoko_proc_macro::parse_static;
use num_bigint::BigUint;

#[test]
fn test_eval_open_block() {
    let mut core = Core::new(parse_static!("let x = 666; let y = 777;").clone());
    core.continue_(&Limits::none()).unwrap();
    core.eval_open_block(
        vec![
            ("x", Value::Nat(BigUint::from(1 as u32))),
            ("y", Value::Nat(BigUint::from(2 as u32))),
        ],
        parse_static!("var z = x + y").clone(),
    );
    let r = core.eval_prog(parse_static!("x + y").clone()).unwrap();
    assert_eq!(r, Value::Nat(BigUint::from(666 + 777 as u32)));
}
