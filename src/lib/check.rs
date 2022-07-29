use crate::ast::Exp_ as Exp;

pub fn parse_exp(input: &str) -> Result<Exp, ()> {
    crate::parser::ExpParser::new().parse(input).map_err(|_| ())
}

// Feel free to completely change how this works again (using this in `tests/` directory for now)

pub fn assert_parse(input: &str, expected: &str) -> Exp {
    let expr = parse_exp(input).unwrap();
    assert_eq!(&format!("{:?}", expr), expected);
    expr
}
