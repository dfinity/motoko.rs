#![feature(test)]
extern crate test;
use num_bigint::ToBigUint;
use test::Bencher;

use motoko::value::Value;
use num_traits::ToPrimitive;
use std::cell::RefCell;

struct Random {
    state: u32,
    size: Option<u32>,
    ind: u32,
}
impl Random {
    pub fn new(size: Option<u32>, seed: u32) -> Self {
        Random {
            state: seed,
            size,
            ind: 0,
        }
    }
}
impl Iterator for Random {
    type Item = u32;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(size) = self.size {
            self.ind += 1;
            if self.ind > size {
                return None;
            }
        }
        self.state = self.state * 48271 % 0x7fffffff;
        Some(self.state)
    }
}

thread_local! {
    static RAND: RefCell<Random>  = RefCell::new(Random::new(None, 42));
}

#[bench]
fn match_u32(b: &mut Bencher) {
    b.iter(|| {
        RAND.with(|rand| {
            let mut rand = rand.borrow_mut();

            let value = Value::Nat(rand.next().unwrap().to_biguint().unwrap());

            match value {
                Value::Nat(n) => Some(n),
                Value::Int(n) => n.to_biguint(),
                _ => None,
            }
            .unwrap()
            .to_u32()
            .unwrap()
        });
    })
}

#[bench]
fn match_u32_clone(b: &mut Bencher) {
    b.iter(|| {
        RAND.with(|rand| {
            let mut rand = rand.borrow_mut();

            let value = &Value::Nat(rand.next().unwrap().to_biguint().unwrap());

            match value.clone() {
                Value::Nat(n) => Some(n),
                Value::Int(n) => n.to_biguint(),
                _ => None,
            }
            .unwrap()
            .to_u32()
            .unwrap()
        });
    })
}

#[bench]
fn convert_u32(b: &mut Bencher) {
    b.iter(|| {
        RAND.with(|rand| {
            let mut rand = rand.borrow_mut();

            let value = &Value::Nat(rand.next().unwrap().to_biguint().unwrap());

            value.to_rust::<u32>().unwrap()
        });
    })
}
