use crate::value::Value_;
use crate::vm_types::{Core, Interruption, Limits, SyntaxError};

#[cfg(feature = "parser")]
/// Used for tests in check module.
pub fn eval_limit(prog: &str, limits: &Limits) -> Result<Value_, Interruption> {
    info!("eval_limit:");
    info!("  - prog = {}", prog);
    info!("  - limits = {:#?}", limits);
    //use crate::vm_types::Interruption::SyntaxError;
    let package_name = None;
    let local_path = "<anonymous>".to_string();
    let p = crate::check::parse(prog).map_err(|code| {
        Interruption::SyntaxError(SyntaxError {
            code,
            local_path,
            package_name,
        })
    })?;
    info!("eval_limit: parsed.");
    let mut c = Core::new(p);
    let r = c.run(limits);
    use log::info;
    info!("eval_limit: result: {:#?}", r);
    r
}

/// Used for tests in check module.
#[cfg(feature = "parser")]
pub fn eval(prog: &str) -> Result<Value_, Interruption> {
    eval_limit(prog, &Limits::none())
}

#[cfg(feature = "parser")]
pub fn eval_into<T: serde::de::DeserializeOwned>(prog: &str) -> Result<T, Interruption> {
    eval(prog)?.to_rust().map_err(Interruption::ValueError)
}
