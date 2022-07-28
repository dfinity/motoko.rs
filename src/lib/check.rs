use crate::ast::Exp;

pub fn parse(input: &str, parse_ast: Option<&str>) -> Result<Exp, ()> {
    let expr = crate::parser::ExpParser::new().parse(input).unwrap();
    match parse_ast {
        None => (),
        Some(a) => {
            assert_eq!(&format!("{:?}", expr), a);
        }
    };
    Ok(expr)
}
