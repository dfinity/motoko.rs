use num_bigint::ToBigUint;
use std::{collections::HashMap, rc::Rc};

use motoko::value::Text;
use motoko::value::{Dynamic, DynamicValue, Value};

#[test]
fn get_index() {
    #[derive(Debug, Clone)]
    struct Struct {
        pub map: HashMap<Value, Rc<Value>>,
    }

    impl Dynamic for Struct {
        fn get_index(&self, index: &Value) -> Option<Rc<Value>> {
            println!("Index: {:?}", index);
            self.map.get(index).map(Clone::clone)
        }
    }

    let expected = Rc::new(Value::Text(Text::from("expected")));
    let value = Value::Dynamic(DynamicValue(Box::new({
        let mut s = Struct {
            map: HashMap::new(),
        };
        s.map
            .insert(Value::Nat(5.to_biguint().unwrap()), expected.clone());
        s
    })));

    let mut core = motoko::vm_types::Core::empty();
    core.env.insert("value".to_string(), value);

    assert_eq!(
        core.eval_prog(motoko::check::parse("value[5]").unwrap()),
        Ok((*expected).clone())
    );
}
