use im_rc::vector;
use motoko::ast::ToId;
//use motoko::check::assert_vm_eval as assert_;
use motoko::eval;
use motoko::shared::Share;
use motoko::value::ActorId;
use motoko::vm_types::{Core, Interruption, Limits};
use motoko::ToMotoko;
use test_log::test; // enable logging output for tests by default.

#[test]
fn import_cycle() {
    let mut core = Core::empty();

    core.set_module(None, "M".to_string(), "import M \"M\"; module { }")
        .expect("set_module");

    let id = ActorId::Alias("A".to_id());

    let r = core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "import M \"M\"; actor { }",
    );

    let stack = vector!["M".to_string(), "M".to_string()];
    assert_eq!(r, Err(Interruption::ImportCycle(stack)))
}

#[test]
fn import_dag_size2() {
    let mut core = Core::empty();

    core.set_module(
        None,
        "M".to_string(),
        "import N \"N\"; module { public func f () { N.f() } }",
    )
    .expect("set_module M");

    core.set_module(None, "N".to_string(), "module { public func f () { #ok } }")
        .expect("set_module N");

    let id = ActorId::Alias("A".to_id());

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "import M \"M\"; actor { public func f () { M.f() } }",
    )
    .expect("set_actor");

    assert_eq!(
        core.call(
            &id,
            &"f".to_id(),
            ().to_motoko().unwrap().share(),
            &Limits::none()
        ),
        eval("#ok")
    );
}

#[test]
fn import_package_module() {
    let mut core = Core::empty();

    core.set_module(
        Some("p".to_string()),
        "M".to_string(),
        "module { public func f () { #ok } }",
    )
    .expect("set_module");

    let id = ActorId::Alias("A".to_id());

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "import M \"mo:p/M\"; actor { public func f () { M.f() } }",
    )
    .expect("set_actor");

    assert_eq!(
        core.call(
            &id,
            &"f".to_id(),
            ().to_motoko().unwrap().share(),
            &Limits::none()
        ),
        eval("#ok")
    );
}
