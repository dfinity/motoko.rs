use motoko::shared::Share;
use motoko::value::Value;
use motoko::vm_types::{Core, Limits};
use motoko_proc_macro::parse_static;
use num_bigint::BigUint;

use test_log::test; // enable logging output for tests by default.

#[test]
fn test_hashmap_randiter_intergration() {
    // init with empty hashmap.s
    let mut core = Core::new(
        parse_static!(
            "
            var map = prim \"hashMapNew\" ();
            var rand_ = prim \"fastRandIterNew\" (null, 42);
            let rands = func(count){
              var c = 0;
              { next = func() {
                 if (c == count) {
                   null
                 } else {
                   c := c + 1;
                   prim \"fastRandIterNext\" rand_;
                 }
                }
              }
            };
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
      let j = rands(size);
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
      let j = rands(size);
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
      let j = rands(size);
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
