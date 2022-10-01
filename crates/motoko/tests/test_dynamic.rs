use motoko::vm_types::{Interruption, Pointer};
use motoko::{dynamic::Dynamic, value::Value_};

use motoko::value::{DynamicValue, Value};

#[test]
fn dyn_struct() {
    #[derive(Clone, Debug, Hash, Default)]
    struct Struct {
        pub map: im_rc::HashMap<Value_, Value_>,
        pub x: Option<Value_>,
    }

    impl Dynamic for Struct {
        fn get_index(&self, index: &Value) -> motoko::dynamic::Result {
            self.map
                .get(index)
                .map(Clone::clone)
                .ok_or_else(|| Interruption::IndexOutOfBounds)
        }

        fn set_index(&mut self, key: Value_, value: Value_) -> motoko::dynamic::Result<()> {
            self.map.insert(key, value);
            Ok(())
        }

        fn get_field(&self, name: &str) -> motoko::dynamic::Result {
            match name {
                "x" => Ok(self.x.clone().expect("`x` not defined")),
                _ => Err(Interruption::UnboundIdentifer(name.to_string())),
            }
        }

        fn set_field(&mut self, name: &str, value: Value_) -> motoko::dynamic::Result<()> {
            match name {
                "x" => {
                    self.x = Some(value);
                    Ok(())
                }
                _ => Err(Interruption::UnboundIdentifer(name.to_string())),
            }
        }

        fn call(
            &mut self,
            _inst: &Option<motoko::ast::Inst>,
            args: Value_,
        ) -> motoko::dynamic::Result {
            Ok(args)
        }
    }

    let value = Value::Dynamic(DynamicValue(Box::new(Struct::default())));

    let mut core = motoko::vm_types::Core::empty();

    // TODO: use `alloc()`
    let pointer = Pointer(12345);
    core.store.insert(pointer, value);
    core.env
        .insert("value".to_string(), Value::Pointer(pointer));

    assert_eq!(
        core.eval_prog(motoko::check::parse("value[5] := 'a'; value[5]").unwrap()),
        Ok(Value::Char('a'))
    );
    assert_eq!(
        core.eval_prog(motoko::check::parse("value.x := 'b'; value.x").unwrap()),
        Ok(Value::Char('b'))
    );
    assert_eq!(
        core.eval_prog(motoko::check::parse("value('c')").unwrap()),
        Ok(Value::Char('c'))
    );
}
