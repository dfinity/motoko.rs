use motoko::{
    package::{get_base_library, get_base_library_tests, Package},
    vm_types::Core,
};

use test_log::test;

fn assert_parse_package(package: Package) {
    println!("Parsing package: {}", package.name);
    assert!(!package.files.is_empty());

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

fn assert_eval_package(package: Package) {
    println!("Evaluating package: {}", package.name);
    assert!(!package.files.is_empty());
    let mut core = Core::empty();
    let mut files = package.files.into_iter().collect::<Vec<_>>();
    files.sort_by_cached_key(|(path, _)| path.clone());
    let total = files.len();
    let mut parse_count = 0;
    let mut error_count = 0;
    println!(
        "Attempting to load {} modules via set_module (parsing only; is order-independent).",
        total
    );
    for (path, file) in files.iter() {
        parse_count += 1;
        // drop .mo from the path (will not be there for the imports)
        let path = format!("{}", &path[0..path.len() - 3]);
        match core.set_module(path.clone(), &file.content) {
            Ok(_) => println!(" set_module {}. ✅ {}", parse_count, path),
            Err(i) => {
                error_count += 1;
                println!(" set_module {}. ❌ {} : {:?}", parse_count, path, i)
            }
        }
    }
    println!("Attempted to load {} modules.", total);
    if error_count > 0 {
        println!("  But, found {} set_module/eval errors.", error_count);
    } else {
        println!("  Success!");
    }
    println!(
        "Attempting to evaluate {} modules (process all imports and definitions).",
        total
    );
    let mut eval_count = 0;
    let mut error_count = 0;
    for (path, file) in files {
        eval_count += 1;
        let mut core2 = core.clone();
        match core2.eval(&file.content) {
            Ok(_) => println!(" eval {}. ✅ {}", eval_count, path),
            Err(i) => {
                error_count += 1;
                println!(" eval {}. ❌ {} : {:?}", eval_count, path, i)
            }
        }
    }
    println!("Attempted to evaluate {} modules.", total);
    if error_count > 0 {
        println!("  But, found {} set_module/eval errors.", error_count);
    } else {
        println!("  Success!");
    }

    assert!(false);
}

#[test]
fn parse_base_library() {
    assert_parse_package(get_base_library());
}

#[test]
fn parse_base_library_tests() {
    assert_parse_package(get_base_library_tests())
}

#[ignore]
#[test]
fn eval_base_library() {
    assert_eval_package(get_base_library());
}

#[ignore]
#[test]
fn eval_base_library_tests() {
    assert_eval_package(get_base_library_tests());
}
