# motoko.rs

Motoko support in Rust.

[Online demo.](https://mo-vm.netlify.app/)

## Motoko VM

Motoko VM explores a more dynamic way to run Motoko.

## Priorities (WIP)

 - Relaxed semantics for programs with type errors, permitting limited execution.
 - Faithfully cover full Motoko language. Support base library.
 - Permit versioning, snapshots, transactions and rollback.

## Out of scope

 - No complex, high-performance VM techniques, like just-in-time compilation.
 - Not [a compiler from Motoko to Wasm](https://github.com/dfinity/motoko).
 - No static type system here. [See compiler for static typing.](https://github.com/dfinity/motoko).
