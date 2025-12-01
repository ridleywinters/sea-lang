use super::{OutputWriter, RuntimeError, Value};

pub fn call_builtin(
    name: &str,
    args: Vec<Value>,
    output: &OutputWriter,
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "println" => Some(builtin_println(args, output)),
        "expect_eq" => Some(builtin_expect_eq(args)),
        "expect_ne" => Some(builtin_expect_ne(args)),
        _ => None,
    }
}

fn builtin_println(args: Vec<Value>, output: &OutputWriter) -> Result<Value, RuntimeError> {
    if let Some(value) = args.first() {
        output.write_line(&value.to_string());
    } else {
        output.write_empty_line();
    }
    Ok(Value::Unit)
}

fn builtin_expect_eq(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError {
            message: format!("expect_eq expects 2 arguments, got {}", args.len()),
        });
    }
    let left = &args[0];
    let right = &args[1];
    if !left.same_type(right) {
        return Err(RuntimeError {
            message: format!(
                "expect_eq failed: type mismatch: {} != {}",
                left.type_name(),
                right.type_name()
            ),
        });
    }
    if !left.eq_value(right) {
        return Err(RuntimeError {
            message: format!("expect_eq failed: {} != {}", left, right),
        });
    }
    Ok(Value::Unit)
}

fn builtin_expect_ne(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError {
            message: format!("expect_ne expects 2 arguments, got {}", args.len()),
        });
    }
    let left = &args[0];
    let right = &args[1];
    if left.same_type(right) && left.eq_value(right) {
        return Err(RuntimeError {
            message: format!("expect_ne failed: {} == {}", left, right),
        });
    }
    Ok(Value::Unit)
}
