use motoko::ast::ToId;
use motoko::check::assert_vm_eval as assert_;
use motoko::check::assert_vm_interruption as assert_x;
use motoko::value::ActorId;
use motoko::vm_types::Interruption;

use test_log::test; // enable logging output for tests by default.

#[test]
fn actor_A_public_func_f() {
    let p = "actor A { public func f () { } }; A.f()";
    assert_(p, "()");
}

#[test]
fn actor_A_public_func_f_137() {
    let p = "let x = 137; actor A { public func f () { x } }; A.f()";
    assert_(p, "137");
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
fn actors_A_public_func_f_fail() {
    let i = Interruption::ActorFieldNotFound(ActorId::Local("A".to_id()), "f".to_id());

    let p = "
    actor A { };
    A.f()";
    assert_x(p, &i);
}

#[ignore]
#[test]
fn actors_A_B_public_func_f_g_h_success() {
    let p = "
    actor B { };
    actor A { public func f () { B.g() } };
    actor B { public func g () { #ok }; public func h() { A.f() } };
    B.h()";
    assert_(p, "#ok");
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
