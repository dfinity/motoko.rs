use motoko::ast::Prog;
use motoko_proc_macro::parse_static;

#[test]
fn primitive() {
    let x = 567;
    let prog: Prog = parse_static!(
        "
            123
        "
    );

    assert_eq!(
        format!("{:?}", prog),
        "Delim { vec: [<Exp(Literal(Nat(\"123\")))@13..16 @ 2:13>], has_trailing: false }"
    )
}
