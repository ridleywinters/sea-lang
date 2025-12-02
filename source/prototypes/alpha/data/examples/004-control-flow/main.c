#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int32_t add(int32_t a, int32_t b);
int32_t sub(int32_t a, int32_t b);
int32_t abs(int32_t a);
int32_t mul(int32_t a, int32_t b);
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

int32_t add(int32_t a, int32_t b) {
    return (a + b);
}

int32_t sub(int32_t a, int32_t b) {
    return (a - b);
}

int32_t abs(int32_t a) {
    if ((a < 0)) {
        return (-a);
    }
    return a;
}

int32_t mul(int32_t a, int32_t b) {
    int32_t count = abs(b);
    int32_t sum = 0;
    for (int i = 0; (i < count); i += 1) {
        sum += a;
    }
    if ((b < 0)) {
        sum *= (-1);
    }
    return sum;
}

void sea_main(void) {
    int32_t x = 2;
    int32_t y = 3;
    expect_eq_int(add(x, y), 5);
    expect_eq_int(add(2, 3), 5);
    expect_eq_int(sub(3, 2), 1);
    expect_ne_typed("unknown", sub(3, 2), "i64", 2);
    int32_t a = (-3);
    expect_eq_int(abs(a), 3);
    expect_eq_int(mul(x, y), 6);
    expect_eq_int(mul(x, a), (-6));
    expect_eq_int((x * a), (-6));
    for (int j = 0; (j < 32); j += 1) {
        for (int k = 0; (k < 32); k += 1) {
            expect_eq_int(mul(j, k), (j * k));
        }
    }
}

int main(void) {
    sea_main();
    return 0;
}
