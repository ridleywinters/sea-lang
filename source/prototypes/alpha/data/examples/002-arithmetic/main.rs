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
    let mut x: i32 = (4_i32.wrapping_add((3_i32.wrapping_mul(2_i32))));
    println_val("Hello, world!");
    println_val(x);
    let mut y0: i8 = 100_i8;
    let mut y1: i8 = 50_i8;
    let mut y: i8 = (y0.wrapping_add(y1));
    expect_eq(y, (106_i8.wrapping_neg()));
    println_val(y);
    let mut a: i16 = (10_i16.wrapping_add(8_i16));
    let mut b: i16 = (6_i16.wrapping_add(12_i16));
    expect_eq(a, b);
    let mut c: i8 = (10_i8.wrapping_add(8_i8));
    expect_ne(a, c);
}

fn main() {
    sea_main();
}
