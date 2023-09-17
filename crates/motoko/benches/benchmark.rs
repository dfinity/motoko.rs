use criterion::{criterion_group, criterion_main, Criterion};
use motoko::{vm_types::Core, vm_types::Limits, Share, Value};

static nested_loops: &str = r#"
var i = 0;
var y = 0.0;
var j = 0;
var x = 0.0;
i := 0;
y := 0.0;
while(i < 300) {
 j := 0;
 x := 0.0;
 while(j < 200) {
   x := x + 16.0;
   j := j + 1;
 };
 i := i + 1;
 y := y + 16.0;
};
"#;

fn nested_loops_step(c: &mut Criterion) {
    let mut group = c.benchmark_group("Examples");
    let prog = motoko::check::parse(nested_loops).unwrap();

    // TODO: load examples from a directory
    group.bench_function("Nested loops, step", |b| {
        b.iter(|| {
            assert_eq!(
                Core::new(prog.clone()).run(&Limits::default()),
                Ok(Value::Unit.share())
            )
        })
    });
    group.finish();
}

fn nested_loops_fast(c: &mut Criterion) {
    let mut group = c.benchmark_group("Examples");
    let prog = motoko::check::parse(nested_loops).unwrap();

    // TODO: load examples from a directory
    group.bench_function("Nested loops, fast", |b| {
        b.iter(|| assert_eq!(Core::new(prog.clone()).run_fast(), Ok(Value::Unit.share())))
    });
    group.finish();
}

criterion_group!(benches, nested_loops_step, nested_loops_fast);
criterion_main!(benches);
