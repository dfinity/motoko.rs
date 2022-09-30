pub mod biguint {
    use num_bigint::BigUint;
    use num_traits::ToPrimitive;
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(n: &num_bigint::BigUint, ser: S) -> Result<S::Ok, S::Error> {
        match n.to_u64() {
            Some(n) => serde::Serialize::serialize(&n, ser),
            None => serde::Serialize::serialize(n, ser), // [u64], TODO: consistent format
        }
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(des: D) -> Result<BigUint, D::Error> {
        let n: u64 = serde::Deserialize::deserialize(des)?;
        Ok(n.into())
    }
}
pub mod bigint {
    use num_bigint::BigInt;
    use num_traits::ToPrimitive;
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(n: &num_bigint::BigInt, ser: S) -> Result<S::Ok, S::Error> {
        match n.to_i64() {
            Some(n) => serde::Serialize::serialize(&n, ser),
            None => serde::Serialize::serialize(n, ser), // [i64], TODO: consistent format
        }
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(des: D) -> Result<BigInt, D::Error> {
        let n: i64 = serde::Deserialize::deserialize(des)?;
        Ok(n.into())
    }
}

pub mod im_rc_hashmap {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::hash::Hash;

    pub fn serialize<K: Serialize + Clone + Eq + Hash, V: Serialize + Clone, S: Serializer>(
        map: &im_rc::HashMap<K, V>,
        ser: S,
    ) -> Result<S::Ok, S::Error> {
        let map: Vec<(&K, &V)> = map.iter().collect();
        serde::Serialize::serialize(&map, ser)
    }

    pub fn deserialize<
        'de,
        K: Deserialize<'de> + Clone + Eq + Hash,
        V: Deserialize<'de> + Clone,
        D: Deserializer<'de>,
    >(
        des: D,
    ) -> Result<im_rc::HashMap<K, V>, D::Error> {
        let attr: Vec<(K, V)> = serde::Deserialize::deserialize(des)?;
        Ok(attr.into_iter().collect())
    }
}

pub mod box_dynamic {
    // use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use crate::value::Dynamic;
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(map: &Box<dyn Dynamic>, ser: S) -> Result<S::Ok, S::Error> {
        todo!()
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(des: D) -> Result<Box<dyn Dynamic>, D::Error> {
        todo!()
    }
}
