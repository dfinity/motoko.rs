# motoko.rs

Motoko concrete syntax parser and dynamic evaluator (VM) in Rust.

## Motoko VM

The Motoko VM explores a more dynamic way for Motoko to execute.

The VM runs programs that parse without type-checking them, and it
issues dynamic type errors when execution fails to progress.

The VM executes source syntax trees directly, for greater simplicity
and greater transparency over more advanced, compilation-based techniques.

## Priorities (WIP)

 - Relaxed semantics for programs with type errors, permitting limited execution.
 - Faithfully cover full Motoko language. Support base library.
 - Permit versioning, snapshots, transactions and rollback.

## Out of scope

 - No complex, high-performance VM techniques, like just-in-time compilation.
 - Not [a compiler from Motoko to Wasm](https://github.com/dfinity/motoko).
 - No static type system here. [See compiler for static typing.](https://github.com/dfinity/motoko).
