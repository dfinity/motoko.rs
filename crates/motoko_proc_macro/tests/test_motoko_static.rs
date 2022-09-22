use motoko::ast::Prog;
use motoko_proc_macro::eval;

#[test]
fn example() {
    let prog: Prog = eval!(
        "
            123
        "
    );
    assert_eq!(prog,);
}
