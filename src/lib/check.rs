use crate::ast::Prog;
use crate::format::format;

pub fn parse(input: &str) -> Result<Prog, ()> {
    // crate::parser::ExpParser::new().parse(input).map_err(|_| ())
    Ok(crate::parser::ProgParser::new().parse(input).unwrap())
}

#[allow(unused_variables)]
pub fn assert_parse_(input: &str, expected: &str) -> Prog {
    println!("testing {}", input);
    let prog = parse(input).unwrap();
    println!(" * parsed {}", input);
    //assert_eq!(&format!("{}", expr), expected);
    println!(" * formatted {}", format(&prog));
    prog
}

pub fn assert_roundtrip(input: &str) {
    let _ = assert_parse_(input, input);
}
