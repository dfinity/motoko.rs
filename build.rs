extern crate lalrpop;

fn main() {
    println!("hello");
    lalrpop::Configuration::new()
        .always_use_colors()
        .process_file(&"src/lib/parser.lalrpop")
        .unwrap();
}
