pub mod im_rc_hashmap {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::hash::Hash;

    pub fn serialize<K: Serialize + Clone + Eq + Hash, V: Serialize + Clone, S: Serializer>(
        map: &im_rc::HashMap<K, V>,
        ser: S,
    ) -> Result<S::Ok, S::Error> {
        let map: Vec<_> = map.iter().collect();
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
        let attr: Vec<_> = serde::Deserialize::deserialize(des)?;
        Ok(attr.into_iter().collect())
    }
}
