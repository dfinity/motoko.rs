use motoko::shared::Share;
use motoko::value::Value;
use motoko::vm_types::{Core, Limits};
use motoko_proc_macro::parse_static;
use num_bigint::BigUint;

use test_log::test; // enable logging output for tests by default.

#[test]
fn test_eval_open_block() {
    let mut core = Core::new(parse_static!("let x = 666; let y = 777;").clone());
    core.continue_(&Limits::none()).unwrap();
    core.eval_open_block(
        vec![
            ("x", Value::Nat(BigUint::from(1_u32)).share()),
            ("y", Value::Nat(BigUint::from(2_u32)).share()),
        ],
        parse_static!("var z = x + y").clone(),
    )
    .unwrap();
    let r = core.eval_prog(parse_static!("x + y").clone()).unwrap();
    assert_eq!(r, Value::Nat(BigUint::from(666 + 777_u32)).share());
}

#[test]
fn test_hashmap_performance_steps() {
    // init core with empty hashmap.
    let mut core = Core::new(
        parse_static!(
            "
      var map = prim \"hashMapNew\" ();
   "
        )
        .clone(),
    );
    core.continue_(&Limits::none()).unwrap();

    // generate initial data / batch random put.
    let size = 10;
    core.eval_open_block(
        vec![("size", Value::Nat(BigUint::from(size as u32)).share())],
        parse_static!(
            "
      var i = prim \"fastRandIterNew\" (?size, 1);
      var j = {
        next = func () {
          let (n, i_) = prim \"fastRandIterNext\" i;
          i := i_;
          n
        }
      };
      for (x in j) {
        let s = prim \"natToText\" x;
        let (m, _) = prim \"hashMapPut\" (map, x, s);
        map := m;
      }
    "
        )
        .clone(),
    )
    .unwrap();

    // batch get.
    let size = 10;
    core.eval_open_block(
        vec![("size", Value::Nat(BigUint::from(size as u32)).share())],
        parse_static!(
            "
      var i = prim \"fastRandIterNew\" (?size, 1);
      var j = {
        next = func () {
          let (n, i_) = prim \"fastRandIterNext\" i;
          i := i_;
          n
        }
      };
      for (x in j) {
        let _ = prim \"hashMapGet\" (map, x);
      }
    "
        )
        .clone(),
    )
    .unwrap();

    // batch remove.
    let size = 10;
    core.eval_open_block(
        vec![("size", Value::Nat(BigUint::from(size as u32)).share())],
        parse_static!(
            "
      var i = prim \"fastRandIterNew\" (?size, 1);
      var j = {
        next = func () {
          let (n, i_) = prim \"fastRandIterNext\" i;
          i := i_;
          n
        }
      };
      for (x in j) {
        let (m, _) = prim \"hashMapRemove\" (map, x);
        map := m;
      }
    "
        )
        .clone(),
    )
    .unwrap();
}
