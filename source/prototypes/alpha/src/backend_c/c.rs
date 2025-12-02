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

pub fn generate_c(module: &ModuleAST) -> String {
    let mut output = String::new();

    output.push_str("#include <stdio.h>\n");
    output.push_str("#include <stdint.h>\n");
    output.push_str("#include <stdbool.h>\n");
    output.push_str("#include <stdlib.h>\n");
    output.push_str("#include <string.h>\n");
    output.push_str("#include <math.h>\n\n");

    // Forward declarations for all user functions
    for func in &module.functions {
        output.push_str(&generate_forward_decl(func));
        output.push('\n');
    }
    output.push('\n');

    // Builtin functions
    output.push_str("void println_str(const char* value) {\n");
    output.push_str("    printf(\"%s\\n\", value);\n");
    output.push_str("}\n\n");

    output.push_str("void println_int(int64_t value) {\n");
    output.push_str("    printf(\"%lld\\n\", (long long)value);\n");
    output.push_str("}\n\n");

    output.push_str("void println_double(double value) {\n");
    output.push_str("    printf(\"%g\\n\", value);\n");
    output.push_str("}\n\n");

    output.push_str("void print_str(const char* value) {\n");
    output.push_str("    printf(\"%s\", value);\n");
    output.push_str("}\n\n");

    output.push_str("void print_int(int64_t value) {\n");
    output.push_str("    printf(\"%lld\", (long long)value);\n");
    output.push_str("}\n\n");

    output.push_str("void print_double(double value) {\n");
    output.push_str("    printf(\"%g\", value);\n");
    output.push_str("}\n\n");

    output.push_str("void expect_eq_int(int64_t a, int64_t b) {\n");
    output.push_str("    if (a != b) {\n");
    output.push_str(
        "        fprintf(stderr, \"Expected %lld === %lld\\n\", (long long)a, (long long)b);\n",
    );
    output.push_str("        exit(1);\n");
    output.push_str("    }\n");
    output.push_str("}\n\n");

    output.push_str("void expect_eq_double(double a, double b) {\n");
    output.push_str("    if (a != b) {\n");
    output.push_str("        fprintf(stderr, \"Expected %g === %g\\n\", a, b);\n");
    output.push_str("        exit(1);\n");
    output.push_str("    }\n");
    output.push_str("}\n\n");

    output.push_str(
        "void expect_ne_typed(const char* type_a, int64_t a, const char* type_b, int64_t b) {\n",
    );
    output.push_str("    if (strcmp(type_a, type_b) != 0) return;\n");
    output.push_str("    if (a == b) {\n");
    output.push_str(
        "        fprintf(stderr, \"Expected %lld !== %lld\\n\", (long long)a, (long long)b);\n",
    );
    output.push_str("        exit(1);\n");
    output.push_str("    }\n");
    output.push_str("}\n\n");

    output.push_str("void expect_ne_int(int64_t a, int64_t b) {\n");
    output.push_str("    if (a == b) {\n");
    output.push_str(
        "        fprintf(stderr, \"Expected %lld !== %lld\\n\", (long long)a, (long long)b);\n",
    );
    output.push_str("        exit(1);\n");
    output.push_str("    }\n");
    output.push_str("}\n\n");

    output.push_str("void expect_ne_double(double a, double b) {\n");
    output.push_str("    if (a == b) {\n");
    output.push_str("        fprintf(stderr, \"Expected %g !== %g\\n\", a, b);\n");
    output.push_str("        exit(1);\n");
    output.push_str("    }\n");
    output.push_str("}\n\n");

    // User functions
    for func in &module.functions {
        output.push_str(&generate_function(func));
        output.push('\n');
    }

    // Entry point
    output.push_str("int main(void) {\n");
    output.push_str("    sea_main();\n");
    output.push_str("    return 0;\n");
    output.push_str("}\n");

    output
}

fn generate_forward_decl(func: &FunctionDef) -> String {
    let params: Vec<String> = if func.params.is_empty() {
        vec!["void".to_string()]
    } else {
        func.params
            .iter()
            .map(|p| format!("{} {}", type_to_c(&p.type_expr), p.name))
            .collect()
    };

    let return_type = func
        .return_type
        .as_ref()
        .map(|t| type_to_c(t))
        .unwrap_or_else(|| "void".to_string());

    let func_name = if func.name == "main" {
        "sea_main".to_string()
    } else {
        func.name.clone()
    };

    format!("{} {}({});", return_type, func_name, params.join(", "))
}

fn generate_function(func: &FunctionDef) -> String {
    let mut output = String::new();
    let mut ctx = CodeGenContext::new();

    // Register function parameters in context
    for param in &func.params {
        let TypeExpr::Named(type_name) = &param.type_expr;
        ctx.declare_var(&param.name, type_name);
    }

    let params: Vec<String> = if func.params.is_empty() {
        vec!["void".to_string()]
    } else {
        func.params
            .iter()
            .map(|p| format!("{} {}", type_to_c(&p.type_expr), p.name))
            .collect()
    };

    let return_type = func
        .return_type
        .as_ref()
        .map(|t| type_to_c(t))
        .unwrap_or_else(|| "void".to_string());

    let func_name = if func.name == "main" {
        "sea_main".to_string()
    } else {
        func.name.clone()
    };

    output.push_str(&format!(
        "{} {}({}) {{\n",
        return_type,
        func_name,
        params.join(", ")
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
            let TypeExpr::Named(type_name) = type_expr;
            ctx.declare_var(name, type_name);
            format!(
                "{} {} = {};",
                type_to_c(type_expr),
                name,
                generate_expr(initializer, ctx)
            )
        }
        Statement::Assignment { name, op, value } => {
            let op_str = match op {
                AssignOp::Assign => "=",
                AssignOp::AddAssign => "+=",
                AssignOp::SubAssign => "-=",
                AssignOp::MulAssign => "*=",
                AssignOp::DivAssign => "/=",
            };
            format!("{} {} {};", name, op_str, generate_expr(value, ctx))
        }
        Statement::Expression(expr) => {
            format!("{};", generate_expr(expr, ctx))
        }
        Statement::Return(expr) => match expr {
            Some(e) => format!("return {};", generate_expr(e, ctx)),
            None => "return;".to_string(),
        },
        Statement::If {
            condition,
            then_block,
            else_block,
        } => {
            let mut output = format!("if ({}) {{\n", generate_expr(condition, ctx));
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
            let update_str = generate_for_update(update, ctx);

            let mut output = format!(
                "for ({}; {}; {}) {{\n",
                init_str,
                generate_expr(condition, ctx),
                update_str
            );
            output.push_str(&generate_block(body, indent + 1, ctx));
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
            ctx.declare_var(name, "int");
            format!("int {} = {}", name, generate_expr(value, ctx))
        }
        Statement::VariableDecl {
            name,
            type_expr,
            initializer,
        } => {
            let TypeExpr::Named(type_name) = type_expr;
            ctx.declare_var(name, type_name);
            format!(
                "{} {} = {}",
                type_to_c(type_expr),
                name,
                generate_expr(initializer, ctx)
            )
        }
        other => generate_statement(other, 0, ctx)
            .trim_end_matches(';')
            .to_string(),
    }
}

fn generate_for_update(stmt: &Statement, ctx: &mut CodeGenContext) -> String {
    match stmt {
        Statement::Assignment { name, op, value } => {
            let op_str = match op {
                AssignOp::Assign => "=",
                AssignOp::AddAssign => "+=",
                AssignOp::SubAssign => "-=",
                AssignOp::MulAssign => "*=",
                AssignOp::DivAssign => "/=",
            };
            format!("{} {} {}", name, op_str, generate_expr(value, ctx))
        }
        other => generate_statement(other, 0, ctx)
            .trim_end_matches(';')
            .to_string(),
    }
}

fn generate_expr(expr: &Expr, ctx: &CodeGenContext) -> String {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Integer(s) => s.clone(),
            Literal::Decimal(s) => s.clone(),
            Literal::String(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
            Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        },
        Expr::Identifier(name) => name.clone(),
        Expr::Call {
            function,
            arguments,
        } => {
            let func_name = generate_expr(function, ctx);
            let args: Vec<String> = arguments.iter().map(|a| generate_expr(a, ctx)).collect();

            match func_name.as_str() {
                "println" => {
                    if let Some(arg) = arguments.first() {
                        let suffix = get_type_suffix(arg, ctx);
                        format!("println_{}({})", suffix, args.join(", "))
                    } else {
                        "println_str(\"\")".to_string()
                    }
                }
                "print" => {
                    if let Some(arg) = arguments.first() {
                        let suffix = get_type_suffix(arg, ctx);
                        format!("print_{}({})", suffix, args.join(", "))
                    } else {
                        "print_str(\"\")".to_string()
                    }
                }
                "expect_eq" => {
                    if let Some(arg) = arguments.first() {
                        let suffix = get_type_suffix(arg, ctx);
                        format!("expect_eq_{}({})", suffix, args.join(", "))
                    } else {
                        format!("expect_eq_int({})", args.join(", "))
                    }
                }
                "expect_ne" => {
                    if arguments.len() >= 2 {
                        let type_a = get_sea_type(&arguments[0], ctx);
                        let type_b = get_sea_type(&arguments[1], ctx);
                        format!(
                            "expect_ne_typed(\"{}\", {}, \"{}\", {})",
                            type_a, args[0], type_b, args[1]
                        )
                    } else {
                        format!("expect_ne_int({})", args.join(", "))
                    }
                }
                "main" => format!("sea_main({})", args.join(", ")),
                _ => format!("{}({})", func_name, args.join(", ")),
            }
        }
        Expr::BinaryOp { left, op, right } => {
            let op_str = match op {
                BinaryOperator::Add => "+",
                BinaryOperator::Sub => "-",
                BinaryOperator::Mul => "*",
                BinaryOperator::Div => "/",
                BinaryOperator::Eq => "==",
                BinaryOperator::Ne => "!=",
                BinaryOperator::Lt => "<",
                BinaryOperator::Gt => ">",
                BinaryOperator::Le => "<=",
                BinaryOperator::Ge => ">=",
            };
            format!(
                "({} {} {})",
                generate_expr(left, ctx),
                op_str,
                generate_expr(right, ctx)
            )
        }
        Expr::UnaryOp { op, operand } => {
            let op_str = match op {
                UnaryOperator::Neg => "-",
            };
            format!("({}{})", op_str, generate_expr(operand, ctx))
        }
    }
}

fn get_sea_type(expr: &Expr, ctx: &CodeGenContext) -> String {
    match expr {
        Expr::Literal(Literal::String(_)) => "string".to_string(),
        Expr::Literal(Literal::Decimal(_)) => "f64".to_string(),
        Expr::Literal(Literal::Integer(_)) => "i64".to_string(),
        Expr::Literal(Literal::Bool(_)) => "bool".to_string(),
        Expr::Identifier(name) => ctx
            .get_var_type(name)
            .cloned()
            .unwrap_or_else(|| "unknown".to_string()),
        _ => "unknown".to_string(),
    }
}

fn get_type_suffix(expr: &Expr, ctx: &CodeGenContext) -> &'static str {
    match expr {
        Expr::Literal(Literal::String(_)) => "str",
        Expr::Literal(Literal::Decimal(_)) => "double",
        Expr::Literal(Literal::Integer(_)) => "int",
        Expr::Literal(Literal::Bool(_)) => "int",
        Expr::Identifier(name) => {
            if let Some(type_name) = ctx.get_var_type(name) {
                match type_name.as_str() {
                    "f32" | "f64" => "double",
                    "string" => "str",
                    _ => "int",
                }
            } else {
                "int"
            }
        }
        _ => "int",
    }
}

fn type_to_c(type_expr: &TypeExpr) -> String {
    match type_expr {
        TypeExpr::Named(name) => match name.as_str() {
            "i8" => "int8_t".to_string(),
            "i16" => "int16_t".to_string(),
            "i32" => "int32_t".to_string(),
            "i64" => "int64_t".to_string(),
            "u8" => "uint8_t".to_string(),
            "u16" => "uint16_t".to_string(),
            "u32" => "uint32_t".to_string(),
            "u64" => "uint64_t".to_string(),
            "f32" => "float".to_string(),
            "f64" => "double".to_string(),
            "string" => "const char*".to_string(),
            "bool" => "bool".to_string(),
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
        generate_c(&module_ast)
    }

    #[test]
    fn test_hello_world_generates_c() {
        let output = generate_example("001-hello-world");
        assert!(
            output.contains("#include <stdio.h>"),
            "Should have includes"
        );
        assert!(
            output.contains("void sea_main(void);"),
            "Should have forward declaration"
        );
        assert!(
            output.contains("void sea_main(void) {"),
            "Should have main function"
        );
        assert!(
            output.contains("println_str(\"Hello, world!\")"),
            "Should call println"
        );
    }

    #[test]
    fn test_arithmetic_generates_c() {
        let output = generate_example("002-arithmetic");
        assert!(output.contains("int8_t"), "Should use int8_t for i8");
        assert!(
            output.contains("expect_eq_int("),
            "Should have expect_eq calls"
        );
    }

    #[test]
    fn test_control_flow_generates_c() {
        let output = generate_example("004-control-flow");
        assert!(output.contains("if ("), "Should have if statements");
        assert!(output.contains("for ("), "Should have for loops");
        assert!(output.contains("return"), "Should have return statements");
    }

    #[test]
    fn test_c_examples() {
        let examples = [
            "001-hello-world",
            "002-arithmetic",
            "003-arithmetic-f32",
            "004-control-flow",
        ];

        for example in examples {
            println!("Testing C backend for: {}", example);

            let c_code = generate_example(example);

            let temp_dir = std::env::temp_dir();
            let source_path = temp_dir.join(format!("sea_test_{}.c", example));
            let binary_path = temp_dir.join(format!("sea_test_{}", example));

            std::fs::write(&source_path, &c_code).expect("Failed to write C source");

            let compile_result = Command::new("gcc")
                .args([
                    "-Wall",
                    "-Wextra",
                    "-o",
                    binary_path.to_str().unwrap(),
                    source_path.to_str().unwrap(),
                ])
                .output()
                .expect("Failed to run gcc");

            if !compile_result.status.success() {
                eprintln!("C compilation failed for '{}':", example);
                eprintln!("Generated C:\n{}", c_code);
                eprintln!(
                    "stderr: {}",
                    String::from_utf8_lossy(&compile_result.stderr)
                );
                panic!("C compilation failed for '{}'", example);
            }

            let run_result = Command::new(&binary_path)
                .output()
                .expect("Failed to run compiled binary");

            let _ = std::fs::remove_file(&source_path);
            let _ = std::fs::remove_file(&binary_path);

            if !run_result.status.success() {
                eprintln!("C execution failed for '{}':", example);
                eprintln!("Generated C:\n{}", c_code);
                eprintln!("stdout: {}", String::from_utf8_lossy(&run_result.stdout));
                eprintln!("stderr: {}", String::from_utf8_lossy(&run_result.stderr));
                panic!("C execution failed for '{}'", example);
            }

            println!("  ✓ {} passed", example);
        }
    }
}
