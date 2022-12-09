use std::collections::HashMap;

use serde::{Deserialize, Serialize};

/// Temporary: get base library from static file
pub fn get_base_library() -> Package {
    serde_json::from_str(include_str!("../packages/base.json")).unwrap()
}

/// Temporary: get base library tests from static file
pub fn get_base_library_tests() -> Package {
    serde_json::from_str(include_str!("../packages/base_test.json")).unwrap()
}

/// Temporary: get primitive definitions from static file
pub fn get_prim_library() -> Package {
    let mut files = HashMap::new();
    let content = include_str!("../packages/prim.mo").to_string();
    files.insert("🚫.mo".to_string(), PackageFile { content });
    Package {
        name: "🚫".to_string(),
        version: "".to_string(),
        files,
    }
}

type PackageFiles = HashMap<String, PackageFile>;

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    pub files: PackageFiles,
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PackageFile {
    pub content: String,
}
