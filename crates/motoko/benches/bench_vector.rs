#![feature(test)]
extern crate test;

macro_rules! vec_clone_n {
    ($n:literal,$id:ident) => {
        mod $id {
            use motoko::shared::{Share, Shared};
            use test::Bencher;

            use motoko::shared::FastClone;
            use motoko::value::{ToMotoko, Value};
            use std::time::SystemTime;

            #[bench]
            fn vec_clone(b: &mut Bencher) {
                let vec: Vec<Shared<Value>> = (0..$n)
                    .into_iter()
                    .map(|_| SystemTime::now().to_motoko().unwrap().share())
                    .collect::<Vec<_>>();

                b.iter(|| vec.clone())
            }

            #[bench]
            fn shared_vec_clone(b: &mut Bencher) {
                let vec: Shared<Vec<Shared<Value>>> = Shared::new(
                    (0..$n)
                        .into_iter()
                        .map(|_| SystemTime::now().to_motoko().unwrap().share())
                        .collect::<Vec<_>>(),
                );

                b.iter(|| vec.fast_clone())
            }

            #[bench]
            fn vector_clone(b: &mut Bencher) {
                let vec: im_rc::Vector<Shared<Value>> = (0..$n)
                    .into_iter()
                    .map(|_| SystemTime::now().to_motoko().unwrap().share())
                    .collect();

                b.iter(|| vec.clone())
            }
        }
    };
}

vec_clone_n!(10, n_10);
vec_clone_n!(100, n_100);
// vec_clone_n!(1000, n_1000);
