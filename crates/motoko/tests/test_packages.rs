use motoko::{package::get_base_library, vm_types::Core};

use test_log::test;

#[test]
fn base_library() {
    let base = get_base_library();
    assert!(base.files.len() > 10);

    let mut core = Core::empty();
    let mut files = base.files.into_iter().collect::<Vec<_>>();
    files.sort_by_cached_key(|(path, _)| path.clone());
    let mut success = true;
    for (path, file) in files {
        match core.set_module(path.clone(), &file.content) {
            Ok(_) => println!("✅ {}", path),
            Err(i) => {
                success = false;
                println!("❌ {} : {:?}", path, i)
            }
        }
    }
    assert!(success);
}
