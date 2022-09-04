### Viewing `env_logger` output in failing tests.

Because logs go to standard error, not standard output, that behavior
is misaligned with the setup of `cargo test -- --nocapture`, which
only looks at `stdout`, and ignores `stderr`.

To fix this undesirable default, we use this package:

https://github.com/d-e-s-o/test-log

It redirects logging to `stdout`, which is what we want for tests run with `cargo test`.

And this `use test_log::test;` appears in the tests that use it.

To see the full output of logging, invoke `cargo test` this way:

`RUST_LOG=trace cargo test -- --nocapture`

Note two important feature of this CLI invocation:

 - Using `RUST_LOG=trace` to set the log level to `trace`, giving maximal output for failing tests.
 - Using `-- --nocapture` to suppress the default test behavior of capturing and omitting `stdout`
