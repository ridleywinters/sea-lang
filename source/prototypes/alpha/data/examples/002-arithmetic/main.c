#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void sea_main(void);

void println_str(const char* value) {
    printf("%s\n", value);
}

void println_int(int64_t value) {
    printf("%lld\n", (long long)value);
}

void println_double(double value) {
    printf("%g\n", value);
}

void print_str(const char* value) {
    printf("%s", value);
}

void print_int(int64_t value) {
    printf("%lld", (long long)value);
}

void print_double(double value) {
    printf("%g", value);
}

void expect_eq_int(int64_t a, int64_t b) {
    if (a != b) {
        fprintf(stderr, "Expected %lld === %lld\n", (long long)a, (long long)b);
        exit(1);
    }
}

void expect_eq_double(double a, double b) {
    if (a != b) {
        fprintf(stderr, "Expected %g === %g\n", a, b);
        exit(1);
    }
}

void expect_ne_typed(const char* type_a, int64_t a, const char* type_b, int64_t b) {
    if (strcmp(type_a, type_b) != 0) return;
    if (a == b) {
        fprintf(stderr, "Expected %lld !== %lld\n", (long long)a, (long long)b);
        exit(1);
    }
}

void expect_ne_int(int64_t a, int64_t b) {
    if (a == b) {
        fprintf(stderr, "Expected %lld !== %lld\n", (long long)a, (long long)b);
        exit(1);
    }
}

void expect_ne_double(double a, double b) {
    if (a == b) {
        fprintf(stderr, "Expected %g !== %g\n", a, b);
        exit(1);
    }
}

void sea_main(void) {
    int32_t x = (4 + (3 * 2));
    println_str("Hello, world!");
    println_int(x);
    int8_t y0 = 100;
    int8_t y1 = 50;
    int8_t y = (y0 + y1);
    expect_eq_int(y, (-106));
    println_int(y);
    int16_t a = (10 + 8);
    int16_t b = (6 + 12);
    expect_eq_int(a, b);
    int8_t c = (10 + 8);
    expect_ne_typed("i16", a, "i8", c);
}

int main(void) {
    sea_main();
    return 0;
}
