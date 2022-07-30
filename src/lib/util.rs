// TODO: move this into the lalrpop file?

pub fn get_one<T>(vec: Vec<T>) -> Result<T, Vec<T>> {
    if vec.len() == 1 {
        Ok(vec.into_iter().nth(0).unwrap())
    } else {
        Err(vec)
    }
}
