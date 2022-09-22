use motoko::{
    ast_traversal::{get_breakpoint_span_from_line as get_span, ToTree},
    check::parse,
};

#[test]
fn test_breakpoint_line() {
    let prog = parse("do {\nlet x = 0;\n\n x \n}").unwrap();
    let ast = prog.vec.into_iter().nth(0).unwrap();
    let tree = ast.tree();

    assert_eq!(get_span(&tree, 0), None, "line 0");
    assert_eq!(get_span(&tree, 1), Some(0..22), "line 1");
    assert_eq!(get_span(&tree, 2), Some(5..14), "line 2");
    assert_eq!(get_span(&tree, 3), None, "line 3");
    assert_eq!(get_span(&tree, 4), Some(18..19), "line 4");
    assert_eq!(get_span(&tree, 5), None, "line 5");
}
