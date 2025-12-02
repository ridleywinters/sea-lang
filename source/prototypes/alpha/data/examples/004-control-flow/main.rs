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

fn add(a: i32, b: i32) -> i32 {
    return (a.wrapping_add(b));
}

fn sub(a: i32, b: i32) -> i32 {
    return (a.wrapping_sub(b));
}

fn abs(a: i32) -> i32 {
    if (a < 0_i32) {
        return (a.wrapping_neg());
    }
    return a;
}

fn mul(a: i32, b: i32) -> i32 {
    let mut count: i32 = abs(b);
    let mut sum: i32 = 0_i32;
    let mut i: i32 = 0_i32;
    while (i < count) {
        sum += a;
        i += 1_i32
    }
    if (b < 0_i32) {
        sum *= (1_i32.wrapping_neg());
    }
    return sum;
}

fn sea_main() {
    let mut x: i32 = 2_i32;
    let mut y: i32 = 3_i32;
    expect_eq(add(x, y), 5);
    expect_eq(add(2, 3), 5);
    expect_eq(sub(3, 2), 1);
    expect_ne(sub(3, 2), 2);
    let mut a: i32 = (3_i32.wrapping_neg());
    expect_eq(abs(a), 3);
    expect_eq(mul(x, y), 6);
    expect_eq(mul(x, a), (-6));
    expect_eq((x.wrapping_mul(a)), (6_i32.wrapping_neg()));
    let mut j: i32 = 0_i32;
    while (j < 32_i32) {
        let mut k: i32 = 0_i32;
        while (k < 32_i32) {
            expect_eq(mul(j, k), (j.wrapping_mul(k)));
            k += 1_i32
        }
        j += 1_i32
    }
}

fn main() {
    sea_main();
}
