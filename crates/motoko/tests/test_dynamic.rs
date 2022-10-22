use motoko::ast::ToId;
use motoko::shared::{FastClone, Share};
use motoko::value::Value;
use motoko::vm_types::{Interruption, Store};
use motoko::{dynamic::Dynamic, value::Value_};

#[test]
fn dyn_struct() {
    #[derive(Clone, Debug, Hash, Default)]
    struct Struct {
        pub map: im_rc::HashMap<Value_, Value_>,
        pub x: Option<Value_>,
    }

    impl Dynamic for Struct {
        fn get_index(&self, _store: &Store, index: Value_) -> motoko::dynamic::Result {
            self.map
                .get(&index)
                .map(FastClone::fast_clone)
                .ok_or(Interruption::IndexOutOfBounds)
        }

        fn set_index(
            &mut self,
            _store: &mut Store,
            key: Value_,
            value: Value_,
        ) -> motoko::dynamic::Result<()> {
            self.map.insert(key, value);
            Ok(())
        }

        // fn get_field(&self, name: &str) -> motoko::dynamic::Result {
        //     match name {
        //         "x" => Ok(self.x.clone().expect("`x` not defined")),
        //         _ => Err(Interruption::UnboundIdentifer(name.to_string())),
        //     }
        // }

        // fn set_field(&mut self, name: &str, value: Value_) -> motoko::dynamic::Result<()> {
        //     match name {
        //         "x" => {
        //             self.x = Some(value);
        //             Ok(())
        //         }
        //         _ => Err(Interruption::UnboundIdentifer(name.to_string())),
        //     }
        // }

        fn call(
            &mut self,
            _store: &mut Store,
            _inst: &Option<motoko::ast::Inst>,
            args: Value_,
        ) -> motoko::dynamic::Result {
            Ok(args)
        }

        fn iter_next(&mut self, _store: &mut Store) -> motoko::dynamic::Result {
            Ok(Value::Null.share())
        }
    }

    let mut core = motoko::vm_types::Core::empty();

    core.assign_alloc("value".to_id(), Struct::default().into_value());

    assert_eq!(
        core.eval_prog(motoko::check::parse("value[5] := 'a'; value[5]").unwrap())
            .unwrap()
            .get(),
        Value::Char('a')
    );
    // assert_eq!(
    //     core.eval_prog(motoko::check::parse("value.x := 'b'; value.x").unwrap()),
    //     Ok(Value::Char('b'))
    // );
    assert_eq!(
        core.eval_prog(motoko::check::parse("value('c')").unwrap())
            .unwrap()
            .get(),
        Value::Char('c')
    );
    assert_eq!(
        core.eval_prog(
            motoko::check::parse("var x = true; for (_ in value) { x := false }; x").unwrap()
        )
        .unwrap()
        .get(),
        Value::Bool(true)
    );
}
