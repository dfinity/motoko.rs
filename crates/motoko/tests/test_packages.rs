use motoko::{
    package::{get_base_library, get_base_library_tests, get_prim_library, Package},
    vm_types::Core,
};

use test_log::test;

fn new_core_with_base() -> Core {
    let mut core = Core::empty();

    let prim = get_prim_library();
    for (path, file) in prim.files.into_iter() {
        // remove '.mo' from suffix of the filename to produce the path
        let path = format!("{}", &path[0..path.len() - 3]);
        core.set_module(Some("⛔".to_string()), path.clone(), &file.content)
            .expect("load prim");
    }

    let base = get_base_library();
    for (path, file) in base.files.into_iter() {
        // remove '.mo' from suffix of the filename to produce the path
        let path = format!("{}", &path[0..path.len() - 3]);
        core.set_module(Some("base".to_string()), path.clone(), &file.content)
            .expect("load base");
    }

    core
}

#[test]
fn import_and_eval_debug_print() {
    let print_hello_world = r##"
 import Debug "mo:base/Debug";
 Debug.print "hello world"
 "##;
    let mut core = new_core_with_base();
    core.eval(&print_hello_world)
        .expect("eval print hello world");
}

#[test]
fn import_all_your_base() {
    let import_all = r##"
 import Array "mo:base/Array";
 import AssocList "mo:base/AssocList";
 import Blob "mo:base/Blob";
 import Bool "mo:base/Bool";
 import Buffer "mo:base/Buffer";
 import CertifiedData "mo:base/CertifiedData";
 import Char "mo:base/Char";
 import Debug "mo:base/Debug";
 import Deque "mo:base/Deque";
 import Error "mo:base/Error";
 import ExperimentalCycles "mo:base/ExperimentalCycles";
 import ExperimentalInternetComputer "mo:base/ExperimentalInternetComputer";
 import ExperimentalStableMemory "mo:base/ExperimentalStableMemory";
 import Float "mo:base/Float";
 import Func "mo:base/Func";
 import Hash "mo:base/Hash";
 import HashMap "mo:base/HashMap";
 import Heap "mo:base/Heap";
 import Int "mo:base/Int";
 import Int16 "mo:base/Int16";
 import Int32 "mo:base/Int32";
 import Int64 "mo:base/Int64";
 import Int8 "mo:base/Int8";
 import Iter "mo:base/Iter";
 import IterType "mo:base/IterType";
 import List "mo:base/List";
 import Nat "mo:base/Nat";
 import Nat16 "mo:base/Nat16";
 import Nat32 "mo:base/Nat32";
 import Nat64 "mo:base/Nat64";
 import Nat8 "mo:base/Nat8";
 import None "mo:base/None";
 import Option "mo:base/Option";
 import Order "mo:base/Order";
 import Prelude "mo:base/Prelude";
 import Principal "mo:base/Principal";
 import RBTree "mo:base/RBTree";
 import Stack "mo:base/Stack";
 import Random "mo:base/Random";
 import Text "mo:base/Text";
 import Time "mo:base/Time";
 import Trie "mo:base/Trie";
 import TrieMap "mo:base/TrieMap";
 import TrieSet "mo:base/TrieSet";
   "##;

    let mut core = new_core_with_base();
    core.eval(&import_all).expect("eval import all");
}

fn assert_parse_packages(package: Package) {
    println!(
        "Parsing package: {}, with {} files.",
        package.name,
        package.files.len()
    );
    //assert!(!package.files.is_empty());
    let mut files = package.files.into_iter().collect::<Vec<_>>();
    files.sort_by_cached_key(|(path, _)| path.clone());
    let total = files.len();
    let mut count = 0;
    let mut error_count = 0;
    for (path, file) in files {
        count += 1;
        match motoko::check::parse(&file.content) {
            Ok(_) => println!(" {}. ✅ {}", count, path),
            Err(i) => {
                error_count += 1;
                println!(" {}. ❌ {} : {:?}", count, path, i)
            }
        }
    }
    println!("Attempted to parse {} modules.", total);
    if error_count > 0 {
        println!("  But, found {} modules did not parse.", error_count);
    } else {
        println!("  Success!");
    }
    assert_eq!(total - error_count, total);
}

fn assert_eval_packages(main_package: Package, dependencies: Vec<Package>) {
    println!("Evaluating package: {}", main_package.name);
    let packages = vec![vec![main_package], dependencies].concat();
    //assert!(!packages.iter().all(|p| !p.files.is_empty()));
    let mut core = Core::empty();
    let mut files = packages
        .into_iter()
        .flat_map(|p| {
            let package_name = p.name.clone();
            p.files
                .into_iter()
                .map(move |(path, file)| (package_name.clone(), path, file))
        })
        .collect::<Vec<_>>();
    files.sort_by_cached_key(|(package_name, path, _)| (package_name.clone(), path.clone()));
    let total = files.len();
    let mut total_loaded = 0;
    let mut parse_count = 0;
    let mut error_count = 0;
    println!(
        "Attempting to load {} modules via set_module (parsing only; is order-independent).",
        total
    );
    for (package_name, path, file) in files.iter() {
        parse_count += 1;
        // drop .mo from the path (will not be there for the imports)
        let path = format!("{}", &path[0..path.len() - 3]);
        if Core::is_module_def(&file.content) {
            match core.set_module(Some(package_name.to_string()), path.clone(), &file.content) {
                Ok(_) => {
                    total_loaded += 1;
                    println!(" set_module {}. ✅ {}/{}", parse_count, package_name, path)
                }
                Err(i) => {
                    error_count += 1;
                    println!(
                        " set_module {}. ❌ {}/{} : {:?}",
                        parse_count, package_name, path, i
                    )
                }
            }
        } else {
            println!(
                " skipping non-module {}. ✅ {}/{}",
                parse_count, package_name, path
            )
        }
    }
    println!("Attempted to parse {} files", total);
    if error_count > 0 {
        println!("  But, found {} set_module/eval errors.", error_count);
    } else {
        println!("  Success!\n  Loaded all {} modules.", total_loaded);
    }
    println!(
        "Attempting to evaluate {} modules (process all imports and definitions).",
        total
    );
    let mut eval_count = 0;
    let mut error_count = 0;
    for (package_name, path, file) in files {
        eval_count += 1;
        let mut core2 = core.clone();
        core2
            .set_ambient_package_name(Some(package_name.clone()))
            .expect("");
        match core2.eval(&file.content) {
            Ok(_) => println!(" eval {}. ✅ {}/{}", eval_count, package_name, path),
            Err(i) => {
                error_count += 1;
                println!(
                    " eval {}. ❌ {}/{} : {:?}",
                    eval_count, package_name, path, i
                )
            }
        }
    }
    println!("Attempted to evaluate {} modules.", total);
    if error_count > 0 {
        println!("  But, found {} set_module/eval errors.", error_count);
        assert!(false)
    } else {
        println!("  Success!");
    }
}

#[test]
fn parse_prim_library() {
    assert_parse_packages(get_prim_library());
}

#[test]
fn parse_base_library() {
    assert_parse_packages(get_base_library());
}

#[test]
fn parse_base_library_tests() {
    assert_parse_packages(get_base_library_tests())
}

#[test]
fn eval_prim_library() {
    assert_eval_packages(get_prim_library(), vec![]);
}

#[test]
fn eval_base_library() {
    assert_eval_packages(get_base_library(), vec![get_prim_library()]);
}

#[ignore]
#[test]
fn eval_base_library_tests() {
    assert_eval_packages(
        get_base_library_tests(),
        vec![get_base_library(), get_prim_library()],
    );
}
