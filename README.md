# motoko.rs

Motoko concrete syntax parser and dynamic evaluator (VM) in Rust.

## Exploring a more dynamic way for Motoko.

The VM runs programs that parse without type-checking them, and it
issues dynamic type errors when execution fails to progress.

VM executes source syntax trees directly, and it eschews additional
intermediate forms or bytecode compilation in favor of greater
simplicity and greater transparency.

## Out of scope

 - No complex, high-performance VM techniques, like just-in-time compilation.
 - Not [a compiler from Motoko to Wasm](https://github.com/dfinity/motoko).
 - No static type system here. [See compiler for static typing.](https://github.com/dfinity/motoko).
