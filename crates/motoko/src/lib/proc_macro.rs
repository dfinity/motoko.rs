use ::serde::{Deserialize, Serialize};

pub fn serialize<T: Sized + Serialize>(value: T) -> Result<Vec<u8>, ()> {
    serde_json::to_vec(&value).map_err(|_| ())
}

pub fn deserialize<'de, T: Sized + Deserialize<'de>>(slice: &'de [u8]) -> Result<T, ()> {
    serde_json::from_slice(slice).map_err(|_| ())
}

// pub fn serialize<T: Sized + Serialize>(value: T) -> Result<Vec<u8>, ()> {
//     Ok(ron::ser::to_string(&value).map_err(|_| ())?.as_bytes().to_vec())
// }

// pub fn deserialize<'de, T: Sized + Deserialize<'de>>(slice: &'de [u8]) -> Result<T, ()> {
//     let input_str = std::str::from_utf8(slice).map_err(|_| ())?;
//     ron::de::from_str(input_str).map_err(|_| ())
// }
