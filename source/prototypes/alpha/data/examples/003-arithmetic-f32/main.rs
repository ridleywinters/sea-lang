#![allow(unused_parens)]
#![allow(dead_code)]

fn println_val<T: std::fmt::Display>(value: T) {
    println!("{}", value);
}

fn print_val<T: std::fmt::Display>(value: T) {
    print!("{}", value);
}

fn expect_eq<T: PartialEq + std::fmt::Debug>(a: T, b: T) {
    if a != b {
        panic!("Expected {:?} === {:?}", a, b);
    }
}

fn expect_ne<T: PartialEq + std::fmt::Debug, U: PartialEq + std::fmt::Debug>(_a: T, _b: U) {
    // Different types are always not equal
}

fn expect_ne_same<T: PartialEq + std::fmt::Debug>(a: T, b: T) {
    if a == b {
        panic!("Expected {:?} !== {:?}", a, b);
    }
}

fn sea_main() {
    let mut x: f32 = (4.0_f32 + (3.0_f32 * 2.0_f32));
    println_val(x);
}

fn main() {
    sea_main();
}
