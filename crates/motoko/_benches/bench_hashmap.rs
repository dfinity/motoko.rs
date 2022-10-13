#![feature(test)]
extern crate test;

macro_rules! hashmap_clone_n {
    ($n:literal, $id:ident) => {
        mod $id {
            use motoko::shared::{Share, Shared};
            use test::Bencher;

            use motoko::shared::FastClone;
            use motoko::value::{ToMotoko, Value};
            use std::collections::HashMap;
            use std::time::SystemTime;

            #[bench]
            fn hashmap_clone(b: &mut Bencher) {
                let mut i = 0;
                let map: HashMap<Shared<Value>, Shared<Value>> = (0..$n)
                    .into_iter()
                    .map(|_| {
                        (
                            (i += 1).to_motoko().unwrap().share(),
                            SystemTime::now().to_motoko().unwrap().share(),
                        )
                    })
                    .collect();

                b.iter(|| map.clone())
            }

            #[bench]
            fn shared_hashmap_clone(b: &mut Bencher) {
                let mut i = 0;
                let map: Shared<HashMap<Shared<Value>, Shared<Value>>> = Shared::new(
                    (0..$n)
                        .into_iter()
                        .map(|_| {
                            (
                                (i += 1).to_motoko().unwrap().share(),
                                SystemTime::now().to_motoko().unwrap().share(),
                            )
                        })
                        .collect::<HashMap<_, _>>(),
                );

                b.iter(|| map.fast_clone())
            }

            #[bench]
            fn imrc_hashmap_clone(b: &mut Bencher) {
                let mut i = 0;
                let map: im_rc::HashMap<Shared<Value>, Shared<Value>> = (0..$n)
                    .into_iter()
                    .map(|_| {
                        (
                            (i += 1).to_motoko().unwrap().share(),
                            SystemTime::now().to_motoko().unwrap().share(),
                        )
                    })
                    .collect();

                b.iter(|| map.clone())
            }
        }
    };
}

hashmap_clone_n!(10, n_10);
hashmap_clone_n!(100, n_100);
