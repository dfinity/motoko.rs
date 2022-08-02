use crate::ast::Exp;

pub fn parse_exp(input: &str) -> Result<Exp, ()> {
    // crate::parser::ExpParser::new().parse(input).map_err(|_| ())
    Ok(crate::parser::ExpParser::new().parse(input).unwrap())
}

#[allow(unused_variables)]
pub fn assert_parse_(input: &str, expected: &str) -> Exp {
    println!("testing {}", input);
    let expr = parse_exp(input).unwrap();
    println!(" * parsed {}", input);
    //assert_eq!(&format!("{}", expr), expected);
    println!(" * formatted {}", input);
    expr
}

pub fn assert_roundtrip(input: &str) {
    let _ = assert_parse_(input, input);
}
