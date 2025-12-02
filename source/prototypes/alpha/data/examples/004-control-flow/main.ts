// Generated TypeScript code

class TypedValue {
    constructor(public type: string, public value: number | string | boolean) {}
    toString(): string { return String(this.value); }
    valueOf(): number | string | boolean { return this.value; }
}

function wrap_i8(n: number): TypedValue {
    n = n | 0;
    if (n > 127) n = n - 256;
    else if (n < -128) n = n + 256;
    return new TypedValue('i8', n);
}

function wrap_i16(n: number): TypedValue {
    n = n | 0;
    if (n > 32767) n = n - 65536;
    else if (n < -32768) n = n + 65536;
    return new TypedValue('i16', n);
}

function wrap_i32(n: number): TypedValue {
    return new TypedValue('i32', n | 0);
}

function wrap_u8(n: number): TypedValue {
    return new TypedValue('u8', ((n % 256) + 256) % 256);
}

function wrap_u16(n: number): TypedValue {
    return new TypedValue('u16', ((n % 65536) + 65536) % 65536);
}

function wrap_u32(n: number): TypedValue {
    return new TypedValue('u32', n >>> 0);
}

function wrap_f32(n: number): TypedValue {
    return new TypedValue('f32', Math.fround(n));
}

function wrap_f64(n: number): TypedValue {
    return new TypedValue('f64', n);
}

function val(v: TypedValue | number | string | boolean): number | string | boolean {
    return v instanceof TypedValue ? v.value : v;
}

function num(v: TypedValue | number): number {
    return (v instanceof TypedValue ? v.value : v) as number;
}

function rewrap(tv: TypedValue, n: number): TypedValue {
    switch (tv.type) {
        case 'i8': return wrap_i8(n);
        case 'i16': return wrap_i16(n);
        case 'i32': return wrap_i32(n);
        case 'u8': return wrap_u8(n);
        case 'u16': return wrap_u16(n);
        case 'u32': return wrap_u32(n);
        case 'f32': return wrap_f32(n);
        case 'f64': return wrap_f64(n);
        default: return new TypedValue(tv.type, n);
    }
}

function println(value: unknown): void {
    console.log(val(value as any));
}

function print(value: unknown): void {
    Deno.stdout.writeSync(new TextEncoder().encode(String(val(value as any))));
}

function sameType(a: unknown, b: unknown): boolean {
    const typeA = a instanceof TypedValue ? a.type : typeof a;
    const typeB = b instanceof TypedValue ? b.type : typeof b;
    return typeA === typeB;
}

function expect_eq(a: unknown, b: unknown): void {
    const va = val(a as any), vb = val(b as any);
    if (va !== vb) throw new Error(`Expected ${va} === ${vb}`);
}

function expect_ne(a: unknown, b: unknown): void {
    if (!sameType(a, b)) return;
    const va = val(a as any), vb = val(b as any);
    if (va === vb) throw new Error(`Expected ${va} !== ${vb}`);
}

function add(a: number, b: number): number {
    return (num(a) + num(b));
}

function sub(a: number, b: number): number {
    return (num(a) - num(b));
}

function abs(a: number): number {
    if ((num(a) < num(0))) {
        return (-num(a));
    }
    return num(a);
}

function mul(a: number, b: number): number {
    let count: TypedValue = wrap_i32(abs(num(b)));
    let sum: TypedValue = wrap_i32(0);
    for (let i = 0; (num(i) < num(count)); i += 1) {
        sum = rewrap(sum, num(sum) + num(a));
    }
    if ((num(b) < num(0))) {
        sum = rewrap(sum, num(sum) * num((-num(1))));
    }
    return num(sum);
}

export function main(): void {
    let x: TypedValue = wrap_i32(2);
    let y: TypedValue = wrap_i32(3);
    expect_eq(add(num(x), num(y)), 5);
    expect_eq(add(2, 3), 5);
    expect_eq(sub(3, 2), 1);
    expect_ne(sub(3, 2), 2);
    let a: TypedValue = wrap_i32((-num(3)));
    expect_eq(abs(num(a)), 3);
    expect_eq(mul(num(x), num(y)), 6);
    expect_eq(mul(num(x), num(a)), (-num(6)));
    expect_eq((num(x) * num(a)), (-num(6)));
    for (let j = 0; (num(j) < num(32)); j += 1) {
        for (let k = 0; (num(k) < num(32)); k += 1) {
            expect_eq(mul(num(j), num(k)), (num(j) * num(k)));
        }
    }
}

main();
