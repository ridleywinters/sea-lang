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

export function main(): void {
    let x: TypedValue = wrap_i32((num(4) + num((num(3) * num(2)))));
    println("Hello, world!");
    println(num(x));
    let y0: TypedValue = wrap_i8(100);
    let y1: TypedValue = wrap_i8(50);
    let y: TypedValue = wrap_i8((num(y0) + num(y1)));
    expect_eq(y, (-num(106)));
    println(num(y));
    let a: TypedValue = wrap_i16((num(10) + num(8)));
    let b: TypedValue = wrap_i16((num(6) + num(12)));
    expect_eq(a, b);
    let c: TypedValue = wrap_i8((num(10) + num(8)));
    expect_ne(a, c);
}

main();
