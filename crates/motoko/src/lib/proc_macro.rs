use ::serde::{Deserialize, Serialize};

pub fn serialize<T: Sized + Serialize>(value: T) -> Result<Vec<u8>, ()> {
    serde_json::to_vec(&value).map_err(|_| ())
}

pub fn deserialize<'de, T: Sized + Deserialize<'de>>(slice: &'de [u8]) -> Result<T, ()> {
    serde_json::from_slice(slice).map_err(|_| ())
}
