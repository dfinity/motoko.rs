use motoko_proc_macro::parse_static;

#[test]
fn literal_123() {
    let prog = parse_static!(
        "
            123
        "
    );

    assert_eq!(
        format!("{:?}", prog),
        "Delim { vec: [<Exp(Literal(Nat(\"123\")))@13..16 @ 2:13>], has_trailing: false }"
    )
}

#[test]
fn forever_while_loop() {
    let prog = parse_static!(
        "
            while true { }
        "
    );

    assert_eq!(
        format!("{:?}", prog),
        "Delim { vec: [<Exp(While(<Literal(Bool(true))@19..23 @ 2:19>, <Block(Delim { vec: [], has_trailing: false })@24..27 @ 2:24>))@13..27 @ 2:13>], has_trailing: false }"
    )
}
