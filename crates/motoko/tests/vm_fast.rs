use motoko::check::assert_vm_fast_eval;

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
fn binop() {
    assert_vm_fast_eval("4", "2 + 2")
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
