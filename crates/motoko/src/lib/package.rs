use std::collections::HashMap;

use serde::{Deserialize, Serialize};

/// Temporary: get base library from static file
pub fn get_base_library() -> Package {
    let mut base_package: Package =
        serde_json::from_str(include_str!("../packages/base.json")).unwrap();
    // to do -- fix this by making the import semantics understand URIs and package names better.
    let prim_content = include_str!("../packages/prim.mo").to_string();
    base_package.files.insert(
        "mo:⛔.mo".to_string(),
        PackageFile {
            content: prim_content,
        },
    );
    base_package
}

/// Temporary: get base library tests from static file
pub fn get_base_library_tests() -> Package {
    serde_json::from_str(include_str!("../packages/base_test.json")).unwrap()
}

/// Temporary: get primitive definitions from static file
pub fn get_prim_library() -> Package {
    let mut files = HashMap::new();
    let content = include_str!("../packages/prim.mo").to_string();
    files.insert("lib.mo".to_string(), PackageFile { content });
    Package {
        name: "⛔".to_string(),
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
