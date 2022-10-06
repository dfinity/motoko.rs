use motoko::ast::ToId;
use motoko::shared::Share;
use motoko::value::Value;
use motoko::vm_types::Interruption;
use motoko::{dynamic::Dynamic, value::Value_};

#[test]
fn dyn_struct() {
    #[derive(Clone, Debug, Hash, Default)]
    struct Struct {
        pub map: im_rc::HashMap<Value_, Value_>,
        pub x: Option<Value_>,
    }

    impl Dynamic for Struct {
        fn get_index(&self, index: Value_) -> motoko::dynamic::Result {
            self.map
                .get(&index)
                .map(Clone::clone)
                .ok_or(Interruption::IndexOutOfBounds)
        }

        fn set_index(&mut self, key: Value_, value: Value_) -> motoko::dynamic::Result<()> {
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
            _inst: &Option<motoko::ast::Inst>,
            args: Value_,
        ) -> motoko::dynamic::Result {
            Ok(args)
        }

        fn next(&mut self) -> motoko::dynamic::Result {
            Ok(Value::Null.share())
        }
    }

    let value = Struct::default().into_value().share();

    let mut core = motoko::vm_types::Core::empty();
    let pointer = core.alloc(value);

    core.env
        .insert("value".to_id(), Value::Pointer(pointer).share());

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
