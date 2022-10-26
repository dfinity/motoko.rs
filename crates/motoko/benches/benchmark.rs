use criterion::{criterion_group, criterion_main, Criterion};
use motoko::{vm_types::Core, vm_types::Limits, Share, Value};

fn bench_example(c: &mut Criterion) {
    let mut group = c.benchmark_group("Examples");

    // TODO: load examples from a directory
    group.bench_function("Basic example", |b| {
        let prog = motoko::check::parse(
            r#"
                let Debug = { print = prim "debugPrint"};
                var x = 0;
                let Iter = { range = func(end){
                { next = func() {
                if (x == end) {
                    null
                } else {
                    let x_ = x;
                    x := x_ + 1;
                    ?x_
                }}}}};
                let i = Iter.range(100);
                var sum = 0;
                for (y in i) {
                    sum := sum + 1;
                    Debug.print sum
                }
            "#,
        )
        .unwrap();
        b.iter(|| {
            assert_eq!(
                Core::new(prog.clone()).run(&Limits::default()),
                Ok(Value::Unit.share())
            )
        })
    });
    group.finish();
}

criterion_group!(benches, bench_example);
criterion_main!(benches);
