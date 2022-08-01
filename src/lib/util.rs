// TODO: move this into the lalrpop file?

use crate::ast::Delim;

pub fn get_one<T>(d: Delim<T>) -> Result<T, Delim<T>> {
    if d.vec.len() == 1 && !d.has_trailing {
        Ok(d.vec.into_iter().nth(0).unwrap())
    } else {
        Err(d)
    }
}
