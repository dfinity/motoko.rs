use crate::ast::Prog;
use crate::format::format;

pub fn parse(input: &str) -> Result<Prog, ()> {
    // crate::parser::ExpParser::new().parse(input).map_err(|_| ())
    Ok(crate::parser::ProgParser::new().parse(input).unwrap())
}

#[allow(unused_variables)]
pub fn assert_parse(input: &str, expected: &str) -> Prog {
    println!("testing {}", input);
    let prog = parse(input).unwrap();
    println!(" * parsed {}", input);
    let formatted = format(&prog);
    println!(" * formatted {}", formatted);
    assert_eq!(&formatted, expected);
    prog
}

pub fn assert_roundtrip(input: &str) {
    let _ = assert_parse(input, input);
}
