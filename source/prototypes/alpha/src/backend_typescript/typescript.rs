use crate::parser::{
    AssignOp, BinaryOperator, Block, Expr, FunctionDef, Literal, ModuleAST, Statement, TypeExpr,
    UnaryOperator,
};

pub fn generate_typescript(module: &ModuleAST) -> String {
    let mut output = String::new();

    output.push_str("// Generated TypeScript code\n\n");

    // Typed value class for tracking types at runtime
    output.push_str("class TypedValue {\n");
    output.push_str(
        "    constructor(public type: string, public value: number | string | boolean) {}\n",
    );
    output.push_str("    toString(): string { return String(this.value); }\n");
    output.push_str("    valueOf(): number | string | boolean { return this.value; }\n");
    output.push_str("}\n\n");

    // Integer overflow helper functions that return TypedValue
    output.push_str("function wrap_i8(n: number): TypedValue {\n");
    output.push_str("    n = n | 0;\n");
    output.push_str("    if (n > 127) n = n - 256;\n");
    output.push_str("    else if (n < -128) n = n + 256;\n");
    output.push_str("    return new TypedValue('i8', n);\n");
    output.push_str("}\n\n");

    output.push_str("function wrap_i16(n: number): TypedValue {\n");
    output.push_str("    n = n | 0;\n");
    output.push_str("    if (n > 32767) n = n - 65536;\n");
    output.push_str("    else if (n < -32768) n = n + 65536;\n");
    output.push_str("    return new TypedValue('i16', n);\n");
    output.push_str("}\n\n");

    output.push_str("function wrap_i32(n: number): TypedValue {\n");
    output.push_str("    return new TypedValue('i32', n | 0);\n");
    output.push_str("}\n\n");

    output.push_str("function wrap_u8(n: number): TypedValue {\n");
    output.push_str("    return new TypedValue('u8', ((n % 256) + 256) % 256);\n");
    output.push_str("}\n\n");

    output.push_str("function wrap_u16(n: number): TypedValue {\n");
    output.push_str("    return new TypedValue('u16', ((n % 65536) + 65536) % 65536);\n");
    output.push_str("}\n\n");

    output.push_str("function wrap_u32(n: number): TypedValue {\n");
    output.push_str("    return new TypedValue('u32', n >>> 0);\n");
    output.push_str("}\n\n");

    output.push_str("function wrap_f32(n: number): TypedValue {\n");
    output.push_str("    return new TypedValue('f32', Math.fround(n));\n");
    output.push_str("}\n\n");

    output.push_str("function wrap_f64(n: number): TypedValue {\n");
    output.push_str("    return new TypedValue('f64', n);\n");
    output.push_str("}\n\n");

    output.push_str(
        "function val(v: TypedValue | number | string | boolean): number | string | boolean {\n",
    );
    output.push_str("    return v instanceof TypedValue ? v.value : v;\n");
    output.push_str("}\n\n");

    output.push_str("function num(v: TypedValue | number): number {\n");
    output.push_str("    return (v instanceof TypedValue ? v.value : v) as number;\n");
    output.push_str("}\n\n");

    output.push_str("function rewrap(tv: TypedValue, n: number): TypedValue {\n");
    output.push_str("    switch (tv.type) {\n");
    output.push_str("        case 'i8': return wrap_i8(n);\n");
    output.push_str("        case 'i16': return wrap_i16(n);\n");
    output.push_str("        case 'i32': return wrap_i32(n);\n");
    output.push_str("        case 'u8': return wrap_u8(n);\n");
    output.push_str("        case 'u16': return wrap_u16(n);\n");
    output.push_str("        case 'u32': return wrap_u32(n);\n");
    output.push_str("        case 'f32': return wrap_f32(n);\n");
    output.push_str("        case 'f64': return wrap_f64(n);\n");
    output.push_str("        default: return new TypedValue(tv.type, n);\n");
    output.push_str("    }\n");
    output.push_str("}\n\n");

    output.push_str("function println(value: unknown): void {\n");
    output.push_str("    console.log(val(value as any));\n");
    output.push_str("}\n\n");
    output.push_str("function print(value: unknown): void {\n");
    output.push_str(
        "    Deno.stdout.writeSync(new TextEncoder().encode(String(val(value as any))));\n",
    );
    output.push_str("}\n\n");

    output.push_str("function sameType(a: unknown, b: unknown): boolean {\n");
    output.push_str("    const typeA = a instanceof TypedValue ? a.type : typeof a;\n");
    output.push_str("    const typeB = b instanceof TypedValue ? b.type : typeof b;\n");
    output.push_str("    return typeA === typeB;\n");
    output.push_str("}\n\n");

    output.push_str("function expect_eq(a: unknown, b: unknown): void {\n");
    output.push_str("    const va = val(a as any), vb = val(b as any);\n");
    output.push_str("    if (va !== vb) throw new Error(`Expected ${va} === ${vb}`);\n");
    output.push_str("}\n\n");

    output.push_str("function expect_ne(a: unknown, b: unknown): void {\n");
    output.push_str("    if (!sameType(a, b)) return;\n");
    output.push_str("    const va = val(a as any), vb = val(b as any);\n");
    output.push_str("    if (va === vb) throw new Error(`Expected ${va} !== ${vb}`);\n");
    output.push_str("}\n\n");

    for func in &module.functions {
        output.push_str(&generate_function(func));
        output.push('\n');
    }

    output.push_str("main();\n");

    output
}

fn generate_function(func: &FunctionDef) -> String {
    let mut output = String::new();

    let params: Vec<String> = func
        .params
        .iter()
        .map(|p| format!("{}: {}", p.name, type_to_ts(&p.type_expr)))
        .collect();

    let return_type = func
        .return_type
        .as_ref()
        .map(|t| type_to_ts(t))
        .unwrap_or_else(|| "void".to_string());

    let export_prefix = if func.is_exported { "export " } else { "" };

    output.push_str(&format!(
        "{}function {}({}): {} {{\n",
        export_prefix,
        func.name,
        params.join(", "),
        return_type
    ));

    output.push_str(&generate_block(&func.body, 1));

    output.push_str("}\n");
    output
}

fn generate_block(block: &Block, indent: usize) -> String {
    let mut output = String::new();
    let indent_str = "    ".repeat(indent);

    for stmt in &block.statements {
        output.push_str(&indent_str);
        output.push_str(&generate_statement(stmt, indent));
        output.push('\n');
    }

    output
}

fn generate_statement(stmt: &Statement, indent: usize) -> String {
    match stmt {
        Statement::VariableDecl {
            name,
            type_expr,
            initializer,
        } => {
            let wrapped = wrap_expr_for_type(type_expr, &generate_expr(initializer));
            format!(
                "let {}: {} = {};",
                name,
                type_to_ts_for_var(type_expr),
                wrapped
            )
        }
        Statement::Assignment { name, op, value } => match op {
            AssignOp::Assign => {
                format!("{} = {};", name, generate_expr(value))
            }
            AssignOp::AddAssign => {
                format!(
                    "{} = rewrap({}, num({}) + num({}));",
                    name,
                    name,
                    name,
                    generate_expr_raw(value)
                )
            }
            AssignOp::SubAssign => {
                format!(
                    "{} = rewrap({}, num({}) - num({}));",
                    name,
                    name,
                    name,
                    generate_expr_raw(value)
                )
            }
            AssignOp::MulAssign => {
                format!(
                    "{} = rewrap({}, num({}) * num({}));",
                    name,
                    name,
                    name,
                    generate_expr_raw(value)
                )
            }
            AssignOp::DivAssign => {
                format!(
                    "{} = rewrap({}, num({}) / num({}));",
                    name,
                    name,
                    name,
                    generate_expr_raw(value)
                )
            }
        },
        Statement::Expression(expr) => {
            format!("{};", generate_expr(expr))
        }
        Statement::Return(expr) => match expr {
            Some(e) => format!("return {};", generate_expr(e)),
            None => "return;".to_string(),
        },
        Statement::If {
            condition,
            then_block,
            else_block,
        } => {
            let mut output = format!("if ({}) {{\n", generate_expr(condition));
            output.push_str(&generate_block(then_block, indent + 1));
            output.push_str(&"    ".repeat(indent));
            output.push('}');

            if let Some(else_blk) = else_block {
                output.push_str(" else {\n");
                output.push_str(&generate_block(else_blk, indent + 1));
                output.push_str(&"    ".repeat(indent));
                output.push('}');
            }

            output
        }
        Statement::For {
            init,
            condition,
            update,
            body,
        } => {
            let init_str = generate_for_init(init);
            let update_str = generate_for_update(update);

            let mut output = format!(
                "for ({}; {}; {}) {{\n",
                init_str,
                generate_expr(condition),
                update_str
            );
            output.push_str(&generate_block(body, indent + 1));
            output.push_str(&"    ".repeat(indent));
            output.push('}');
            output
        }
    }
}

fn generate_for_init(stmt: &Statement) -> String {
    match stmt {
        Statement::Assignment {
            name,
            op: AssignOp::Assign,
            value,
        } => {
            format!("let {} = {}", name, generate_expr(value))
        }
        other => generate_statement(other, 0)
            .trim_end_matches(';')
            .to_string(),
    }
}

fn generate_for_update(stmt: &Statement) -> String {
    match stmt {
        Statement::Assignment { name, op, value } => {
            let op_str = match op {
                AssignOp::Assign => "=",
                AssignOp::AddAssign => "+=",
                AssignOp::SubAssign => "-=",
                AssignOp::MulAssign => "*=",
                AssignOp::DivAssign => "/=",
            };
            format!("{} {} {}", name, op_str, generate_expr(value))
        }
        other => generate_statement(other, 0)
            .trim_end_matches(';')
            .to_string(),
    }
}

fn generate_expr(expr: &Expr) -> String {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Integer(s) => s.clone(),
            Literal::Decimal(s) => s.clone(),
            Literal::String(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
            Literal::Bool(b) => b.to_string(),
        },
        Expr::Identifier(name) => format!("num({})", name),
        Expr::Call {
            function,
            arguments,
        } => {
            let func_name = generate_expr_raw(function);
            // For expect_eq/expect_ne, pass TypedValue objects directly for type comparison
            if func_name == "expect_eq" || func_name == "expect_ne" {
                let args: Vec<String> = arguments.iter().map(generate_expr_raw).collect();
                format!("{}({})", func_name, args.join(", "))
            } else {
                let args: Vec<String> = arguments.iter().map(generate_expr).collect();
                format!("{}({})", func_name, args.join(", "))
            }
        }
        Expr::BinaryOp { left, op, right } => {
            let is_arithmetic = matches!(
                op,
                BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::Mul
                    | BinaryOperator::Div
            );
            let is_comparison = matches!(
                op,
                BinaryOperator::Lt | BinaryOperator::Gt | BinaryOperator::Le | BinaryOperator::Ge
            );
            let op_str = match op {
                BinaryOperator::Add => "+",
                BinaryOperator::Sub => "-",
                BinaryOperator::Mul => "*",
                BinaryOperator::Div => "/",
                BinaryOperator::Eq => "===",
                BinaryOperator::Ne => "!==",
                BinaryOperator::Lt => "<",
                BinaryOperator::Gt => ">",
                BinaryOperator::Le => "<=",
                BinaryOperator::Ge => ">=",
            };
            if is_arithmetic || is_comparison {
                format!(
                    "(num({}) {} num({}))",
                    generate_expr_raw(left),
                    op_str,
                    generate_expr_raw(right)
                )
            } else {
                format!(
                    "({} {} {})",
                    generate_expr(left),
                    op_str,
                    generate_expr(right)
                )
            }
        }
        Expr::UnaryOp { op, operand } => {
            let op_str = match op {
                UnaryOperator::Neg => "-",
            };
            format!("({}num({}))", op_str, generate_expr_raw(operand))
        }
    }
}

fn generate_expr_raw(expr: &Expr) -> String {
    match expr {
        Expr::Identifier(name) => name.clone(),
        _ => generate_expr(expr),
    }
}

fn type_to_ts(type_expr: &TypeExpr) -> String {
    type_to_ts_inner(type_expr, false)
}

fn type_to_ts_for_var(type_expr: &TypeExpr) -> String {
    type_to_ts_inner(type_expr, true)
}

fn type_to_ts_inner(type_expr: &TypeExpr, for_variable: bool) -> String {
    match type_expr {
        TypeExpr::Named(name) => match name.as_str() {
            "i8" | "i16" | "i32" | "u8" | "u16" | "u32" | "f32" | "f64" => {
                if for_variable {
                    "TypedValue".to_string()
                } else {
                    "number".to_string()
                }
            }
            "i64" | "i128" | "u64" | "u128" => "bigint".to_string(),
            "string" => "string".to_string(),
            "bool" => "boolean".to_string(),
            _ => name.clone(),
        },
    }
}

fn wrap_expr_for_type(type_expr: &TypeExpr, expr: &str) -> String {
    match type_expr {
        TypeExpr::Named(name) => match name.as_str() {
            "i8" => format!("wrap_i8({})", expr),
            "i16" => format!("wrap_i16({})", expr),
            "i32" => format!("wrap_i32({})", expr),
            "u8" => format!("wrap_u8({})", expr),
            "u16" => format!("wrap_u16({})", expr),
            "u32" => format!("wrap_u32({})", expr),
            "f32" => format!("wrap_f32({})", expr),
            "f64" => format!("wrap_f64({})", expr),
            _ => expr.to_string(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::parse_from_path;
    use std::path::PathBuf;

    fn generate_example(name: &str) -> String {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("data/examples")
            .join(name);

        let module_ast = parse_from_path(&path, "Testing", false).unwrap_or_else(|err| {
            eprintln!("Failed to parse example '{}':", name);
            eprintln!("{}", err);
            panic!("Example parsing failed")
        });

        generate_typescript(&module_ast)
    }

    #[test]
    fn test_hello_world_generates_typescript() {
        let output = generate_example("001-hello-world");
        assert!(
            output.contains("export function main()"),
            "Should have exported main function"
        );
        assert!(
            output.contains("println(\"Hello, world!\")"),
            "Should call println"
        );
        assert!(output.contains("main();"), "Should call main at end");
    }

    #[test]
    fn test_arithmetic_generates_typescript() {
        let output = generate_example("002-arithmetic");
        assert!(
            output.contains("let x: TypedValue"),
            "Should declare typed variables"
        );
        assert!(output.contains("expect_eq("), "Should have expect_eq calls");
    }

    #[test]
    fn test_control_flow_generates_typescript() {
        let output = generate_example("004-control-flow");
        assert!(output.contains("if ("), "Should have if statements");
        assert!(output.contains("for ("), "Should have for loops");
        assert!(output.contains("return"), "Should have return statements");
    }
}
