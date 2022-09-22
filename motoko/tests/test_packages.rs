use motoko::package::get_base_library;

#[test]
fn base_library() {
    let base = get_base_library();

    assert!(base.files.len() > 10);
}
