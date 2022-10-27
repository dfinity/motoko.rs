use motoko::ast::ToId;
use motoko::check::assert_vm_eval as assert_;
use motoko::check::assert_vm_interruption as assert_x;
use motoko::value::ActorId;
use motoko::vm_types::{Interruption, NumericPointer, Pointer};

use test_log::test; // enable logging output for tests by default.

#[test]
fn actor_A_public_func_f() {
    let p = "
    actor A { public func f () { } };
    A.f()";
    assert_(p, "()");
}

#[test]
fn actor_A_public_func_f_137() {
    let p = "
    let x = 137;
    actor A { public func f () { x } };
    A.f()";
    assert_(p, "137");
}

#[test]
fn actor_A_public_func_f_dangling() {
    let i = Interruption::Dangling(Pointer::Numeric(NumericPointer(0)));
    let p = "
    var x = 137;
    actor A { public func f () { x } };
    A.f()";
    assert_x(p, &i)
}

#[test]
fn actor_A_public_func_f_g() {
    let p = "
    let x = 137;
    actor A { public func f () { x };
              public func g () { f() } };
    A.g()";
    assert_(p, "137");
}

#[test]
fn actor_A_private_func_f_fail() {
    let i = Interruption::ActorFieldNotPublic(ActorId::Local("A".to_id()), "f".to_id());
    let p = "
    actor A { func f () { } };
    A.f()";
    assert_x(p, &i);
}

#[test]
fn actors_A_missing_func_f_fail() {
    let i = Interruption::ActorFieldNotFound(ActorId::Local("A".to_id()), "f".to_id());
    let p = "
    actor A { };
    A.f()";
    assert_x(p, &i);
}

#[test]
fn counter_inc_twice() {
    let p = "let y = 999;
             let x = 666;
             actor Counter = {
               var x = 0;
               public func get() /*: async Nat*/ { x };
               public func inc() { x := x + 1 };
             };
             assert (Counter.get() == 0);
             Counter.inc();
             assert (Counter.get() == 1);
             Counter.inc();
             assert (Counter.get() == 2);
             assert (y == 999);
             assert (x == 666);
             #ok
";
    assert_(p, "#ok");
}

#[test]
fn actor_upgrade_demo_with_counter_inc() {
    let p = "actor Counter = {
               var x = 0;
               public func get() /*: async Nat*/ { x };
               public func inc() { x := x + 1 };
             };
             assert (Counter.get() == 0);
             Counter.inc();
             assert (Counter.get() == 1);
             actor Counter {
               var x = 0;
               public func get() /*: async Nat*/ { x };
               public func inc() { x := x + 2 };
             };
             assert (Counter.get() == 1);
             Counter.inc();
             assert (Counter.get() == 3);
             #ok
";
    assert_(p, "#ok");
}

#[test]
fn actors_A_B_public_func_f_g() {
    // Actor A is forward-declared,
    // then B is defined using A's future API,
    // then A is defined, exposing API used by B.
    let p = "
actor A { };
actor B { public func f() { A.g() } };
actor A { public func g() { #ok } };
B.f()";
    assert_(p, "#ok");
}

#[test]
fn actors_A_B_public_func_f_g_fail() {
    let i = Interruption::UnboundIdentifer("A".to_id());

    // Actor A is not defined at all.
    let p = "
actor B { public func f() { A.g() } };
B.f()";
    assert_x(p, &i);

    // Actor A is defined too late, after Actor B.
    let p = "
actor B { public func f() { A.g() } };
actor A { };
B.f()";
    assert_x(p, &i);
}
