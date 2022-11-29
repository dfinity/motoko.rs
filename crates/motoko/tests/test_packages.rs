use motoko::package::get_base_library;

use test_log::test;

#[test]
fn parse_base_library() {
    let base = get_base_library();
    assert!(base.files.len() > 10);

    let mut files = base.files.into_iter().collect::<Vec<_>>();
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

// #[test]
// fn load_base_library() {
//     let base = get_base_library();
//     assert!(base.files.len() > 10);

//     let mut core = Core::empty();
//     let mut files = base.files.into_iter().collect::<Vec<_>>();
//     files.sort_by_cached_key(|(path, _)| path.clone());
//     let mut success = true;
//     for (path, file) in files {
//         match core.set_module(path.clone(), &file.content) {
//             Ok(_) => println!("✅ {}", path),
//             Err(i) => {
//                 success = false;
//                 println!("❌ {} : {:?}", path, i)
//             }
//         }
//     }
//     assert!(success);
// }
