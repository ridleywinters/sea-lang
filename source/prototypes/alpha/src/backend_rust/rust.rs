use std::collections::HashMap;

use crate::parser::{
    AssignOp, BinaryOperator, Block, Expr, FunctionDef, Literal, ModuleAST, Statement, TypeExpr,
    UnaryOperator,
};

struct CodeGenContext {
    var_types: HashMap<String, String>,
}

impl CodeGenContext {
    fn new() -> Self {
        Self {
            var_types: HashMap::new(),
        }
    }

    fn declare_var(&mut self, name: &str, type_name: &str) {
        self.var_types
            .insert(name.to_string(), type_name.to_string());
    }

    fn get_var_type(&self, name: &str) -> Option<&String> {
        self.var_types.get(name)
    }
}

pub fn generate_rust(module: &ModuleAST) -> String {
    let mut output = String::new();

    output.push_str("#![allow(unused_parens)]\n");
    output.push_str("#![allow(dead_code)]\n\n");

    // Builtin functions
    output.push_str("fn println_val<T: std::fmt::Display>(value: T) {\n");
    output.push_str("    println!(\"{}\", value);\n");
    output.push_str("}\n\n");

    output.push_str("fn print_val<T: std::fmt::Display>(value: T) {\n");
    output.push_str("    print!(\"{}\", value);\n");
    output.push_str("}\n\n");

    output.push_str("fn expect_eq<T: PartialEq + std::fmt::Debug>(a: T, b: T) {\n");
    output.push_str("    if a != b {\n");
    output.push_str("        panic!(\"Expected {:?} === {:?}\", a, b);\n");
    output.push_str("    }\n");
    output.push_str("}\n\n");

    output.push_str("fn expect_ne<T: PartialEq + std::fmt::Debug, U: PartialEq + std::fmt::Debug>(_a: T, _b: U) {\n");
    output.push_str("    // Different types are always not equal\n");
    output.push_str("}\n\n");

    output.push_str("fn expect_ne_same<T: PartialEq + std::fmt::Debug>(a: T, b: T) {\n");
    output.push_str("    if a == b {\n");
    output.push_str("        panic!(\"Expected {:?} !== {:?}\", a, b);\n");
    output.push_str("    }\n");
    output.push_str("}\n\n");

    // User functions
    for func in &module.functions {
        output.push_str(&generate_function(func));
        output.push('\n');
    }

    // Entry point
    output.push_str("fn main() {\n");
    output.push_str("    sea_main();\n");
    output.push_str("}\n");

    output
}

fn generate_function(func: &FunctionDef) -> String {
    let mut ctx = CodeGenContext::new();
    let mut output = String::new();

    // Register parameters in context
    for p in &func.params {
        ctx.declare_var(&p.name, &type_to_rust(&p.type_expr));
    }

    let params: Vec<String> = func
        .params
        .iter()
        .map(|p| format!("{}: {}", p.name, type_to_rust(&p.type_expr)))
        .collect();

    let return_type = func
        .return_type
        .as_ref()
        .map(|t| format!(" -> {}", type_to_rust(t)))
        .unwrap_or_default();

    let func_name = if func.name == "main" {
        "sea_main"
    } else {
        &func.name
    };

    output.push_str(&format!(
        "fn {}({}){} {{\n",
        func_name,
        params.join(", "),
        return_type
    ));

    output.push_str(&generate_block(&func.body, 1, &mut ctx));

    output.push_str("}\n");
    output
}

fn generate_block(block: &Block, indent: usize, ctx: &mut CodeGenContext) -> String {
    let mut output = String::new();
    let indent_str = "    ".repeat(indent);

    for stmt in &block.statements {
        output.push_str(&indent_str);
        output.push_str(&generate_statement(stmt, indent, ctx));
        output.push('\n');
    }

    output
}

fn generate_statement(stmt: &Statement, indent: usize, ctx: &mut CodeGenContext) -> String {
    match stmt {
        Statement::VariableDecl {
            name,
            type_expr,
            initializer,
        } => {
            let rust_type = type_to_rust(type_expr);
            ctx.declare_var(name, &rust_type);
            format!(
                "let mut {}: {} = {};",
                name,
                rust_type,
                generate_expr(initializer, Some(&rust_type), ctx)
            )
        }
        Statement::Assignment { name, op, value } => {
            let target_type = ctx.get_var_type(name).cloned();
            let op_str = match op {
                AssignOp::Assign => "=",
                AssignOp::AddAssign => "+=",
                AssignOp::SubAssign => "-=",
                AssignOp::MulAssign => "*=",
                AssignOp::DivAssign => "/=",
            };
            format!(
                "{} {} {};",
                name,
                op_str,
                generate_expr(value, target_type.as_deref(), ctx)
            )
        }
        Statement::Expression(expr) => {
            format!("{};", generate_expr(expr, None, ctx))
        }
        Statement::Return(expr) => match expr {
            Some(e) => format!("return {};", generate_expr(e, None, ctx)),
            None => "return;".to_string(),
        },
        Statement::If {
            condition,
            then_block,
            else_block,
        } => {
            let mut output = format!("if {} {{\n", generate_expr(condition, None, ctx));
            output.push_str(&generate_block(then_block, indent + 1, ctx));
            output.push_str(&"    ".repeat(indent));
            output.push('}');

            if let Some(else_blk) = else_block {
                output.push_str(" else {\n");
                output.push_str(&generate_block(else_blk, indent + 1, ctx));
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
            let init_str = generate_for_init(init, ctx);
            let mut output = format!("{}\n", init_str);
            output.push_str(&"    ".repeat(indent));
            output.push_str(&format!(
                "while {} {{\n",
                generate_expr(condition, None, ctx)
            ));
            output.push_str(&generate_block(body, indent + 1, ctx));
            output.push_str(&"    ".repeat(indent + 1));
            output.push_str(&generate_for_update(update, ctx));
            output.push('\n');
            output.push_str(&"    ".repeat(indent));
            output.push('}');
            output
        }
    }
}

fn generate_for_init(stmt: &Statement, ctx: &mut CodeGenContext) -> String {
    match stmt {
        Statement::Assignment {
            name,
            op: AssignOp::Assign,
            value,
        } => {
            ctx.declare_var(name, "i32");
            format!(
                "let mut {}: i32 = {};",
                name,
                generate_expr(value, Some("i32"), ctx)
            )
        }
        Statement::VariableDecl {
            name,
            type_expr,
            initializer,
        } => {
            let rust_type = type_to_rust(type_expr);
            ctx.declare_var(name, &rust_type);
            format!(
                "let mut {}: {} = {};",
                name,
                rust_type,
                generate_expr(initializer, Some(&rust_type), ctx)
            )
        }
        _ => generate_statement(stmt, 0, ctx),
    }
}

fn generate_for_update(stmt: &Statement, ctx: &CodeGenContext) -> String {
    match stmt {
        Statement::Assignment { name, op, value } => {
            let target_type = ctx.get_var_type(name).cloned();
            let op_str = match op {
                AssignOp::Assign => "=",
                AssignOp::AddAssign => "+=",
                AssignOp::SubAssign => "-=",
                AssignOp::MulAssign => "*=",
                AssignOp::DivAssign => "/=",
            };
            format!(
                "{} {} {}",
                name,
                op_str,
                generate_expr(value, target_type.as_deref(), ctx)
            )
        }
        _ => panic!("Unexpected statement type in for loop update"),
    }
}

fn generate_expr(expr: &Expr, target_type: Option<&str>, ctx: &CodeGenContext) -> String {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Integer(s) => {
                if let Some(ty) = target_type {
                    if is_integer_type(ty) {
                        return format!("{}_{}", s, ty);
                    }
                }
                s.clone()
            }
            Literal::Decimal(s) => {
                if let Some(ty) = target_type {
                    if ty == "f32" || ty == "f64" {
                        return format!("{}_{}", s, ty);
                    }
                }
                if s.contains('.') {
                    format!("{}_f64", s)
                } else {
                    s.clone()
                }
            }
            Literal::String(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
            Literal::Bool(b) => b.to_string(),
        },
        Expr::Identifier(name) => name.clone(),
        Expr::Call {
            function,
            arguments,
        } => {
            let func_name = generate_expr(function, None, ctx);

            match func_name.as_str() {
                "println" => {
                    let args: Vec<String> = arguments
                        .iter()
                        .map(|a| generate_expr(a, None, ctx))
                        .collect();
                    format!("println_val({})", args.join(", "))
                }
                "print" => {
                    let args: Vec<String> = arguments
                        .iter()
                        .map(|a| generate_expr(a, None, ctx))
                        .collect();
                    format!("print_val({})", args.join(", "))
                }
                "expect_eq" | "expect_ne" => {
                    // Infer type from first argument and apply to second
                    let first_type = if !arguments.is_empty() {
                        infer_expr_type(&arguments[0], None, ctx)
                    } else {
                        None
                    };
                    let args: Vec<String> = arguments
                        .iter()
                        .map(|a| generate_expr(a, first_type.as_deref(), ctx))
                        .collect();
                    format!("{}({})", func_name, args.join(", "))
                }
                "main" => {
                    let args: Vec<String> = arguments
                        .iter()
                        .map(|a| generate_expr(a, None, ctx))
                        .collect();
                    format!("sea_main({})", args.join(", "))
                }
                _ => {
                    let args: Vec<String> = arguments
                        .iter()
                        .map(|a| generate_expr(a, None, ctx))
                        .collect();
                    format!("{}({})", func_name, args.join(", "))
                }
            }
        }
        Expr::BinaryOp { left, op, right } => {
            let inferred_type = infer_expr_type(expr, target_type, ctx);
            let left_expr = generate_expr(left, inferred_type.as_deref(), ctx);
            let right_expr = generate_expr(right, inferred_type.as_deref(), ctx);
            let use_wrapping = inferred_type
                .as_ref()
                .map(|t| is_integer_type(t))
                .unwrap_or(false);
            match op {
                BinaryOperator::Add => {
                    if use_wrapping {
                        format!("({}.wrapping_add({}))", left_expr, right_expr)
                    } else {
                        format!("({} + {})", left_expr, right_expr)
                    }
                }
                BinaryOperator::Sub => {
                    if use_wrapping {
                        format!("({}.wrapping_sub({}))", left_expr, right_expr)
                    } else {
                        format!("({} - {})", left_expr, right_expr)
                    }
                }
                BinaryOperator::Mul => {
                    if use_wrapping {
                        format!("({}.wrapping_mul({}))", left_expr, right_expr)
                    } else {
                        format!("({} * {})", left_expr, right_expr)
                    }
                }
                BinaryOperator::Div => format!("({} / {})", left_expr, right_expr),
                BinaryOperator::Eq => format!("({} == {})", left_expr, right_expr),
                BinaryOperator::Ne => format!("({} != {})", left_expr, right_expr),
                BinaryOperator::Lt => format!("({} < {})", left_expr, right_expr),
                BinaryOperator::Gt => format!("({} > {})", left_expr, right_expr),
                BinaryOperator::Le => format!("({} <= {})", left_expr, right_expr),
                BinaryOperator::Ge => format!("({} >= {})", left_expr, right_expr),
            }
        }
        Expr::UnaryOp { op, operand } => {
            let inferred_type = infer_expr_type(expr, target_type, ctx);
            let use_wrapping = inferred_type
                .as_ref()
                .map(|t| is_integer_type(t))
                .unwrap_or(false);
            match op {
                UnaryOperator::Neg => {
                    if use_wrapping {
                        format!(
                            "({}.wrapping_neg())",
                            generate_expr(operand, inferred_type.as_deref(), ctx)
                        )
                    } else {
                        format!(
                            "(-{})",
                            generate_expr(operand, inferred_type.as_deref(), ctx)
                        )
                    }
                }
            }
        }
    }
}

fn is_integer_type(ty: &str) -> bool {
    matches!(
        ty,
        "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64"
    )
}

fn infer_expr_type(expr: &Expr, target_type: Option<&str>, ctx: &CodeGenContext) -> Option<String> {
    if let Some(ty) = target_type {
        return Some(ty.to_string());
    }

    match expr {
        Expr::Identifier(name) => ctx.get_var_type(name).cloned(),
        Expr::BinaryOp { left, right, .. } => {
            infer_expr_type(left, None, ctx).or_else(|| infer_expr_type(right, None, ctx))
        }
        Expr::UnaryOp { operand, .. } => infer_expr_type(operand, None, ctx),
        _ => None,
    }
}

fn type_to_rust(type_expr: &TypeExpr) -> String {
    match type_expr {
        TypeExpr::Named(name) => match name.as_str() {
            "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64"
            | "bool" => name.clone(),
            "string" => "&str".to_string(),
            _ => name.clone(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::parse_from_path;
    use std::path::PathBuf;
    use std::process::Command;

    fn generate_example(name: &str) -> String {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("data/examples")
            .join(name);

        let module_ast = parse_from_path(&path, "Testing", false).unwrap();
        generate_rust(&module_ast)
    }

    #[test]
    fn test_hello_world_generates_rust() {
        let output = generate_example("001-hello-world");
        assert!(
            output.contains("fn sea_main()"),
            "Should have main function"
        );
        assert!(
            output.contains("println_val(\"Hello, world!\")"),
            "Should call println"
        );
    }

    #[test]
    fn test_arithmetic_generates_rust() {
        let output = generate_example("002-arithmetic");
        assert!(output.contains("let mut x: i32"), "Should use i32 type");
        assert!(output.contains("expect_eq("), "Should have expect_eq calls");
    }

    #[test]
    fn test_control_flow_generates_rust() {
        let output = generate_example("004-control-flow");
        assert!(output.contains("if "), "Should have if statements");
        assert!(output.contains("while "), "Should have while loops");
        assert!(output.contains("return"), "Should have return statements");
    }

    #[test]
    fn test_rust_examples() {
        let examples = [
            "001-hello-world",
            "002-arithmetic",
            "003-arithmetic-f32",
            "004-control-flow",
        ];

        for example in examples {
            println!("Testing Rust backend for: {}", example);

            let rust_code = generate_example(example);

            let temp_dir = std::env::temp_dir();
            let source_path = temp_dir.join(format!("sea_test_{}.rs", example));
            let binary_path = temp_dir.join(format!("sea_test_{}_bin", example));

            std::fs::write(&source_path, &rust_code).expect("Failed to write Rust source");

            let compile_result = Command::new("rustc")
                .args([
                    "-o",
                    binary_path.to_str().unwrap(),
                    source_path.to_str().unwrap(),
                ])
                .output()
                .expect("Failed to run rustc");

            if !compile_result.status.success() {
                eprintln!("Rust compilation failed for '{}':", example);
                eprintln!("Generated Rust:\n{}", rust_code);
                eprintln!(
                    "stderr: {}",
                    String::from_utf8_lossy(&compile_result.stderr)
                );
                panic!("Rust compilation failed for '{}'", example);
            }

            let run_result = Command::new(&binary_path)
                .output()
                .expect("Failed to run compiled binary");

            let _ = std::fs::remove_file(&source_path);
            let _ = std::fs::remove_file(&binary_path);

            if !run_result.status.success() {
                eprintln!("Rust execution failed for '{}':", example);
                eprintln!("Generated Rust:\n{}", rust_code);
                eprintln!("stdout: {}", String::from_utf8_lossy(&run_result.stdout));
                eprintln!("stderr: {}", String::from_utf8_lossy(&run_result.stderr));
                panic!("Rust execution failed for '{}'", example);
            }

            println!("  ✓ {} passed", example);
        }
    }
}
