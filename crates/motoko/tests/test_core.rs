use motoko::ast::ToId;
use motoko::eval;
use motoko::shared::Share;
use motoko::value::ActorId;
use motoko::vm_types::{Core, Limits};
use motoko::ToMotoko;

#[test]
fn actor_upgrade_demo_with_counter_inc() {
    let mut core = Core::empty();
    let id = ActorId::Alias("Counter".to_id());

    core.set_actor(
        id.clone(),
        "
      actor {
        var x = 0;
        public func get() : async Nat { x };
        public func inc() { x := x + 1 };
    }",
    )
    .expect("create");

    assert_eq!(
        core.call(
            &id,
            &"get".to_id(),
            ().to_motoko().unwrap().share(),
            &Limits::none()
        ),
        eval("0")
    );

    assert_eq!(
        core.call(
            &id,
            &"inc".to_id(),
            ().to_motoko().unwrap().share(),
            &Limits::none()
        ),
        eval("()")
    );

    assert_eq!(
        core.call(
            &id,
            &"get".to_id(),
            ().to_motoko().unwrap().share(),
            &Limits::none()
        ),
        eval("1")
    );

    core.set_actor(
        id.clone(),
        "
      actor {
        var x = 0;
        public func get() : async Nat { x };
        public func inc() { x := x + 2 };
    }",
    )
    .expect("create");

    assert_eq!(
        core.call(
            &id,
            &"get".to_id(),
            ().to_motoko().unwrap().share(),
            &Limits::none()
        ),
        eval("1")
    );

    assert_eq!(
        core.call(
            &id,
            &"inc".to_id(),
            ().to_motoko().unwrap().share(),
            &Limits::none()
        ),
        eval("()")
    );

    assert_eq!(
        core.call(
            &id,
            &"get".to_id(),
            ().to_motoko().unwrap().share(),
            &Limits::none()
        ),
        eval("3")
    );
}
