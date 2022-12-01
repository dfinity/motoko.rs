use std::collections::HashMap;

use serde::{Deserialize, Serialize};

/// Temporary: get base library from static file
pub fn get_base_library() -> Package {
    let package: Package = serde_json::from_str(include_str!("../utils/mo_base.json")).unwrap();
    package
}

/// Temporary: get base library tests from static file
pub fn get_base_library_tests() -> Package {
    let package: Package = serde_json::from_str(include_str!("../utils/mo_base_test.json")).unwrap();
    package
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
