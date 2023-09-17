use motoko::check::assert_vm_fast_eval;

#[test]
fn tuple_proj() {
    assert_vm_fast_eval("3", r#"(1, 2, 3, 4).2"#);
}

#[test]
fn binop() {
    assert_vm_fast_eval("4", "2 + 2")
}

#[test]
fn let_() {
    assert_vm_fast_eval(
        "2",
        r#"
          let x = 2;
          x
        "#,
    );

    assert_vm_fast_eval(
        "2",
        r#"
          let x = 2;
        "#,
    );
}

#[test]
fn var_() {
    assert_vm_fast_eval(
        "2",
        r#"
          var x = 2;
          x
        "#,
    );

    assert_vm_fast_eval(
        "2",
        r#"
          var x = 2;
        "#,
    );
}

#[test]
fn let_var_binop() {
    assert_vm_fast_eval(
        "4",
        r#"
      let x = 1 + 1;
      var y = 2;
      y + x
    "#,
    );
}

#[test]
fn assignment() {
    assert_vm_fast_eval(
        "()",
        r#"
      var x = 1;
      x := 2;
    "#,
    );

    assert_vm_fast_eval(
        "2",
        r#"
      var x = 1;
      x := 2;
      x
    "#,
    );
}

#[test]
fn binop_assignment() {
    assert_vm_fast_eval(
        "()",
        r#"
      var x = 1;
      x += 1;
    "#,
    );

    assert_vm_fast_eval(
        "2",
        r#"
      var x = 1;
      x += 1;
      x
    "#,
    );
}

#[test]
fn while_() {
    assert_vm_fast_eval(
        "10",
        r#"
      var x = 0;
      while(x < 10) {
        x := x + 1
      };
      x
    "#,
    );
}

#[test]
fn nested_while_() {
    assert_vm_fast_eval(
        "()",
        r#"
var i = 0;
var y = 0.0;
var j = 0;
var x = 0.0;
i := 0;
y := 0.0;
while(i < 30) {
 j := 0;
 x := 0.0;
 while(j < 20) {
   x := x + 16.0;
   j := j + 1;
 };
 i := i + 1;
 y := y + 16.0;
};
"#,
    );
}

#[test]
fn func_def_call() {
    assert_vm_fast_eval(
        "2",
        r#"
func f(x) { x + 1 };
f(1)
"#,
    );
}
