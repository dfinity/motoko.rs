// use std::fs;

// use motoko::check::assert_lex;
// use regex::Regex;

// #[test]
// fn test_formatter() {
//     for file in fs::read_dir("./tests/formatter").unwrap() {
//         let path = file.unwrap().path();
//         println!("\nFile: {}", path.as_os_str().to_string_lossy());

//         let script = fs::read_to_string(path).unwrap();

//         let case_splitter = Regex::new(r"\n====+ *").unwrap();
//         let line_width = Regex::new(r"^[0-9]+\n").unwrap();

//         let mut split = case_splitter.split(&script);

//         let input = split.next().expect("Splitter not found in file");

//         while let Some(case) = split.next() {
//             let (width, case) = if line_width.is_match(case) {
//                 let (n, case) = case.split_once('\n').unwrap();
//                 (
//                     Some(
//                         n.parse::<usize>()
//                             .expect("Line width should be a natural number"),
//                     ),
//                     case,
//                 )
//             } else {
//                 (None, case)
//             };

//             let expected = case.trim();
//             assert_lex(input, expected, width);
//         }
//     }

//     // TODO: generate test cases once the formatter works mostly as intended
// }
