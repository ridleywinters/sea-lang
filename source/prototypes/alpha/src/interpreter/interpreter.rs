use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use crate::parser::{
    BinaryOperator, Block, Expr, FunctionDef, Literal, ModuleAST, Statement, TypeExpr,
    UnaryOperator,
};

use super::builtins;

#[derive(Debug, Clone)]
pub enum Value {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F32(f32),
    F64(f64),
    String(String),
    Bool(bool),
    Unit,
    Integer(String),
    Decimal(String),
}

impl Value {
    pub(super) fn type_name(&self) -> &'static str {
        match self {
            Value::I8(_) => "i8",
            Value::I16(_) => "i16",
            Value::I32(_) => "i32",
            Value::I64(_) => "i64",
            Value::I128(_) => "i128",
            Value::U8(_) => "u8",
            Value::U16(_) => "u16",
            Value::U32(_) => "u32",
            Value::U64(_) => "u64",
            Value::U128(_) => "u128",
            Value::F32(_) => "f32",
            Value::F64(_) => "f64",
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Unit => "unit",
            Value::Integer(_) => "integer",
            Value::Decimal(_) => "decimal",
        }
    }

    fn is_integer_type(&self) -> bool {
        matches!(
            self,
            Value::I8(_)
                | Value::I16(_)
                | Value::I32(_)
                | Value::I64(_)
                | Value::I128(_)
                | Value::U8(_)
                | Value::U16(_)
                | Value::U32(_)
                | Value::U64(_)
                | Value::U128(_)
                | Value::Integer(_)
        )
    }

    fn is_float_type(&self) -> bool {
        matches!(self, Value::F32(_) | Value::F64(_) | Value::Decimal(_))
    }

    pub(super) fn same_type(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Integer(_), other) => other.is_integer_type(),
            (other, Value::Integer(_)) => other.is_integer_type(),
            (Value::Decimal(_), other) => other.is_float_type(),
            (other, Value::Decimal(_)) => other.is_float_type(),
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }

    pub(super) fn eq_value(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::I8(a), Value::I8(b)) => a == b,
            (Value::I16(a), Value::I16(b)) => a == b,
            (Value::I32(a), Value::I32(b)) => a == b,
            (Value::I64(a), Value::I64(b)) => a == b,
            (Value::I128(a), Value::I128(b)) => a == b,
            (Value::U8(a), Value::U8(b)) => a == b,
            (Value::U16(a), Value::U16(b)) => a == b,
            (Value::U32(a), Value::U32(b)) => a == b,
            (Value::U64(a), Value::U64(b)) => a == b,
            (Value::U128(a), Value::U128(b)) => a == b,
            (Value::F32(a), Value::F32(b)) => a == b,
            (Value::F64(a), Value::F64(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Decimal(a), Value::Decimal(b)) => a == b,
            (Value::Integer(s), typed) => {
                if let Ok(converted) = Value::Integer(s.clone()).convert_to_match(typed) {
                    converted.eq_value(other)
                } else {
                    false
                }
            }
            (typed, Value::Integer(s)) => {
                if let Ok(converted) = Value::Integer(s.clone()).convert_to_match(typed) {
                    self.eq_value(&converted)
                } else {
                    false
                }
            }
            (Value::Decimal(s), typed) => {
                if let Ok(converted) = Value::Decimal(s.clone()).convert_to_match(typed) {
                    converted.eq_value(other)
                } else {
                    false
                }
            }
            (typed, Value::Decimal(s)) => {
                if let Ok(converted) = Value::Decimal(s.clone()).convert_to_match(typed) {
                    self.eq_value(&converted)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn to_i8(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as integer", s),
                })?;
                if n < i8::MIN as i128 || n > i8::MAX as i128 {
                    return Err(RuntimeError {
                        message: format!(
                            "Value {} does not fit in i8 (range {} to {})",
                            n,
                            i8::MIN,
                            i8::MAX
                        ),
                    });
                }
                Ok(Value::I8(n as i8))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i8", self.type_name()),
            }),
        }
    }

    pub fn to_i16(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as integer", s),
                })?;
                if n < i16::MIN as i128 || n > i16::MAX as i128 {
                    return Err(RuntimeError {
                        message: format!(
                            "Value {} does not fit in i16 (range {} to {})",
                            n,
                            i16::MIN,
                            i16::MAX
                        ),
                    });
                }
                Ok(Value::I16(n as i16))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i16", self.type_name()),
            }),
        }
    }

    pub fn to_i32(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as integer", s),
                })?;
                if n < i32::MIN as i128 || n > i32::MAX as i128 {
                    return Err(RuntimeError {
                        message: format!(
                            "Value {} does not fit in i32 (range {} to {})",
                            n,
                            i32::MIN,
                            i32::MAX
                        ),
                    });
                }
                Ok(Value::I32(n as i32))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i32", self.type_name()),
            }),
        }
    }

    pub fn to_i64(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as integer", s),
                })?;
                if n < i64::MIN as i128 || n > i64::MAX as i128 {
                    return Err(RuntimeError {
                        message: format!(
                            "Value {} does not fit in i64 (range {} to {})",
                            n,
                            i64::MIN,
                            i64::MAX
                        ),
                    });
                }
                Ok(Value::I64(n as i64))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i64", self.type_name()),
            }),
        }
    }

    pub fn to_i128(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as i128", s),
                })?;
                Ok(Value::I128(n))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i128", self.type_name()),
            }),
        }
    }

    pub fn to_u8(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as integer", s),
                })?;
                if n < 0 || n > u8::MAX as i128 {
                    return Err(RuntimeError {
                        message: format!("Value {} does not fit in u8 (range 0 to {})", n, u8::MAX),
                    });
                }
                Ok(Value::U8(n as u8))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to u8", self.type_name()),
            }),
        }
    }

    pub fn to_u16(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as integer", s),
                })?;
                if n < 0 || n > u16::MAX as i128 {
                    return Err(RuntimeError {
                        message: format!(
                            "Value {} does not fit in u16 (range 0 to {})",
                            n,
                            u16::MAX
                        ),
                    });
                }
                Ok(Value::U16(n as u16))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to u16", self.type_name()),
            }),
        }
    }

    pub fn to_u32(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as integer", s),
                })?;
                if n < 0 || n > u32::MAX as i128 {
                    return Err(RuntimeError {
                        message: format!(
                            "Value {} does not fit in u32 (range 0 to {})",
                            n,
                            u32::MAX
                        ),
                    });
                }
                Ok(Value::U32(n as u32))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to u32", self.type_name()),
            }),
        }
    }

    pub fn to_u64(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as integer", s),
                })?;
                if n < 0 || n > u64::MAX as i128 {
                    return Err(RuntimeError {
                        message: format!(
                            "Value {} does not fit in u64 (range 0 to {})",
                            n,
                            u64::MAX
                        ),
                    });
                }
                Ok(Value::U64(n as u64))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to u64", self.type_name()),
            }),
        }
    }

    pub fn to_u128(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n: i128 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as integer", s),
                })?;
                if n < 0 {
                    return Err(RuntimeError {
                        message: format!("Value {} does not fit in u128 (must be non-negative)", n),
                    });
                }
                Ok(Value::U128(n as u128))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to u128", self.type_name()),
            }),
        }
    }

    pub fn to_f32(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) | Value::Decimal(s) => {
                let n: f64 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as float", s),
                })?;
                Ok(Value::F32(n as f32))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to f32", self.type_name()),
            }),
        }
    }

    pub fn to_f64(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) | Value::Decimal(s) => {
                let n: f64 = s.parse().map_err(|_| RuntimeError {
                    message: format!("Cannot parse '{}' as float", s),
                })?;
                Ok(Value::F64(n))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to f64", self.type_name()),
            }),
        }
    }

    pub fn convert_to_match(&self, target: &Value) -> Result<Value, RuntimeError> {
        match target {
            Value::I8(_) => self.to_i8(),
            Value::I16(_) => self.to_i16(),
            Value::I32(_) => self.to_i32(),
            Value::I64(_) => self.to_i64(),
            Value::I128(_) => self.to_i128(),
            Value::U8(_) => self.to_u8(),
            Value::U16(_) => self.to_u16(),
            Value::U32(_) => self.to_u32(),
            Value::U64(_) => self.to_u64(),
            Value::U128(_) => self.to_u128(),
            Value::F32(_) => self.to_f32(),
            Value::F64(_) => self.to_f64(),
            _ => Err(RuntimeError {
                message: format!(
                    "Cannot convert {} to {}",
                    self.type_name(),
                    target.type_name()
                ),
            }),
        }
    }

    pub fn convert_to_type_name(&self, type_name: &str) -> Result<Value, RuntimeError> {
        match type_name {
            "i8" => self.to_i8(),
            "i16" => self.to_i16(),
            "i32" => self.to_i32(),
            "i64" => self.to_i64(),
            "i128" => self.to_i128(),
            "u8" => self.to_u8(),
            "u16" => self.to_u16(),
            "u32" => self.to_u32(),
            "u64" => self.to_u64(),
            "u128" => self.to_u128(),
            "f32" => self.to_f32(),
            "f64" => self.to_f64(),
            _ => Err(RuntimeError {
                message: format!("Unknown type '{}'", type_name),
            }),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I8(n) => write!(f, "{}", n),
            Value::I16(n) => write!(f, "{}", n),
            Value::I32(n) => write!(f, "{}", n),
            Value::I64(n) => write!(f, "{}", n),
            Value::I128(n) => write!(f, "{}", n),
            Value::U8(n) => write!(f, "{}", n),
            Value::U16(n) => write!(f, "{}", n),
            Value::U32(n) => write!(f, "{}", n),
            Value::U64(n) => write!(f, "{}", n),
            Value::U128(n) => write!(f, "{}", n),
            Value::F32(n) => write!(f, "{}", n),
            Value::F64(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
            Value::Integer(s) => write!(f, "{}", s),
            Value::Decimal(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Runtime error: {}", self.message)
    }
}

pub(super) enum OutputWriter {
    Stdout,
    Captured(Rc<RefCell<Vec<u8>>>),
}

impl OutputWriter {
    pub fn write_line(&self, s: &str) {
        match self {
            OutputWriter::Stdout => println!("{}", s),
            OutputWriter::Captured(buffer) => {
                let mut buf = buffer.borrow_mut();
                writeln!(buf, "{}", s).unwrap();
            }
        }
    }

    pub fn write_empty_line(&self) {
        match self {
            OutputWriter::Stdout => println!(),
            OutputWriter::Captured(buffer) => {
                let mut buf = buffer.borrow_mut();
                writeln!(buf).unwrap();
            }
        }
    }
}

pub struct InterpreterOutput {
    pub value: Value,
    pub captured_output: Option<String>,
}

struct Interpreter<'a> {
    module: &'a ModuleAST,
    variables: HashMap<String, Value>,
    output: OutputWriter,
}

impl<'a> Interpreter<'a> {
    fn new(module: &'a ModuleAST, output: OutputWriter) -> Self {
        Interpreter {
            module,
            variables: HashMap::new(),
            output,
        }
    }

    fn find_function(&self, name: &str) -> Option<&FunctionDef> {
        self.module.functions.iter().find(|f| f.name == name)
    }

    fn convert_to_type(&self, value: Value, type_expr: &TypeExpr) -> Result<Value, RuntimeError> {
        let TypeExpr::Named(type_name) = type_expr;

        match &value {
            Value::Integer(_) | Value::Decimal(_) => value.convert_to_type_name(type_name),
            _ => {
                if value.type_name() == type_name {
                    Ok(value)
                } else {
                    Err(RuntimeError {
                        message: format!(
                            "Cannot convert {} to type '{}'",
                            value.type_name(),
                            type_name
                        ),
                    })
                }
            }
        }
    }

    fn call_function(&mut self, name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if let Some(result) = builtins::call_builtin(name, args.clone(), &self.output) {
            return result;
        }

        let func = self
            .find_function(name)
            .cloned()
            .ok_or_else(|| RuntimeError {
                message: format!("Undefined function: {}", name),
            })?;

        if args.len() != func.params.len() {
            return Err(RuntimeError {
                message: format!(
                    "Function '{}' expects {} arguments, got {}",
                    name,
                    func.params.len(),
                    args.len()
                ),
            });
        }

        let converted_args: Result<Vec<Value>, RuntimeError> = func
            .params
            .iter()
            .zip(args)
            .map(|(param, arg)| self.convert_to_type(arg, &param.type_expr))
            .collect();
        let converted_args = converted_args?;

        let old_variables = std::mem::take(&mut self.variables);

        for (param, value) in func.params.iter().zip(converted_args) {
            self.variables.insert(param.name.clone(), value);
        }

        let result = self.execute_block(&func.body);

        self.variables = old_variables;

        result
    }

    fn execute_block(&mut self, block: &Block) -> Result<Value, RuntimeError> {
        let mut result = Value::Unit;

        for statement in &block.statements {
            match statement {
                Statement::VariableDecl {
                    name,
                    type_expr,
                    initializer,
                } => {
                    let value = self.evaluate(initializer)?;
                    let value = self.convert_to_type(value, type_expr)?;
                    self.variables.insert(name.clone(), value);
                    result = Value::Unit;
                }
                Statement::Expression(expr) => {
                    result = self.evaluate(expr)?;
                }
                Statement::Return(expr) => {
                    return match expr {
                        Some(e) => self.evaluate(e),
                        None => Ok(Value::Unit),
                    };
                }
            }
        }

        Ok(result)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(lit) => Ok(match lit {
                Literal::Integer(s) => Value::Integer(s.clone()),
                Literal::Decimal(s) => Value::Decimal(s.clone()),
                Literal::String(s) => Value::String(s.clone()),
                Literal::Bool(b) => Value::Bool(*b),
            }),

            Expr::Identifier(name) => {
                self.variables
                    .get(name)
                    .cloned()
                    .ok_or_else(|| RuntimeError {
                        message: format!("Undefined variable: {}", name),
                    })
            }

            Expr::Call {
                function,
                arguments,
            } => {
                let func_name = match function.as_ref() {
                    Expr::Identifier(name) => name.clone(),
                    _ => {
                        return Err(RuntimeError {
                            message: "Cannot call non-identifier as function".to_string(),
                        });
                    }
                };

                let args: Result<Vec<Value>, RuntimeError> =
                    arguments.iter().map(|arg| self.evaluate(arg)).collect();

                self.call_function(&func_name, args?)
            }

            Expr::BinaryOp { left, op, right } => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;
                self.apply_binary_op(op, left_val, right_val)
            }

            Expr::UnaryOp { op, operand } => {
                let val = self.evaluate(operand)?;
                self.apply_unary_op(op, val)
            }
        }
    }

    fn apply_unary_op(&self, op: &UnaryOperator, val: Value) -> Result<Value, RuntimeError> {
        match op {
            UnaryOperator::Neg => match val {
                Value::I8(n) => Ok(Value::I8(-n)),
                Value::I16(n) => Ok(Value::I16(-n)),
                Value::I32(n) => Ok(Value::I32(-n)),
                Value::I64(n) => Ok(Value::I64(-n)),
                Value::I128(n) => Ok(Value::I128(-n)),
                Value::F32(n) => Ok(Value::F32(-n)),
                Value::F64(n) => Ok(Value::F64(-n)),
                Value::Integer(s) => {
                    let n: i128 = s.parse().map_err(|_| RuntimeError {
                        message: format!("Cannot parse '{}' as integer", s),
                    })?;
                    Ok(Value::Integer((-n).to_string()))
                }
                Value::Decimal(s) => {
                    let n: f64 = s.parse().map_err(|_| RuntimeError {
                        message: format!("Cannot parse '{}' as decimal", s),
                    })?;
                    Ok(Value::Decimal((-n).to_string()))
                }
                _ => Err(RuntimeError {
                    message: format!("Cannot negate {}", val.type_name()),
                }),
            },
        }
    }

    fn apply_binary_op(
        &self,
        op: &BinaryOperator,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match (&left, &right) {
            (Value::Integer(_), Value::Decimal(_)) | (Value::Decimal(_), Value::Integer(_)) => {
                return Err(RuntimeError {
                    message: "Cannot mix integer and decimal literals in binary operation"
                        .to_string(),
                });
            }
            _ => {}
        }

        match (&left, &right) {
            (Value::Integer(a), Value::Integer(b)) => self.apply_integer_literal_op(op, a, b),
            (Value::Decimal(a), Value::Decimal(b)) => self.apply_decimal_literal_op(op, a, b),
            (Value::Integer(_), typed) | (typed, Value::Integer(_)) => {
                let int_val = if matches!(left, Value::Integer(_)) {
                    &left
                } else {
                    &right
                };
                let converted = int_val.convert_to_match(typed)?;
                if matches!(left, Value::Integer(_)) {
                    self.apply_binary_op(op, converted, right)
                } else {
                    self.apply_binary_op(op, left, converted)
                }
            }
            (Value::Decimal(_), typed) | (typed, Value::Decimal(_)) => {
                let dec_val = if matches!(left, Value::Decimal(_)) {
                    &left
                } else {
                    &right
                };
                let converted = dec_val.convert_to_match(typed)?;
                if matches!(left, Value::Decimal(_)) {
                    self.apply_binary_op(op, converted, right)
                } else {
                    self.apply_binary_op(op, left, converted)
                }
            }
            _ => {
                if !left.same_type(&right) {
                    return Err(RuntimeError {
                        message: format!(
                            "Type mismatch in binary operation: {} and {}",
                            left.type_name(),
                            right.type_name()
                        ),
                    });
                }
                self.apply_typed_binary_op(op, left, right)
            }
        }
    }

    fn apply_typed_binary_op(
        &self,
        op: &BinaryOperator,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match (&left, &right) {
            (Value::I8(a), Value::I8(b)) => self.apply_i8_op(op, *a, *b),
            (Value::I16(a), Value::I16(b)) => self.apply_i16_op(op, *a, *b),
            (Value::I32(a), Value::I32(b)) => self.apply_i32_op(op, *a, *b),
            (Value::I64(a), Value::I64(b)) => self.apply_i64_op(op, *a, *b),
            (Value::I128(a), Value::I128(b)) => self.apply_i128_op(op, *a, *b),
            (Value::U8(a), Value::U8(b)) => self.apply_u8_op(op, *a, *b),
            (Value::U16(a), Value::U16(b)) => self.apply_u16_op(op, *a, *b),
            (Value::U32(a), Value::U32(b)) => self.apply_u32_op(op, *a, *b),
            (Value::U64(a), Value::U64(b)) => self.apply_u64_op(op, *a, *b),
            (Value::U128(a), Value::U128(b)) => self.apply_u128_op(op, *a, *b),
            (Value::F32(a), Value::F32(b)) => self.apply_f32_op(op, *a, *b),
            (Value::F64(a), Value::F64(b)) => self.apply_f64_op(op, *a, *b),
            (Value::String(a), Value::String(b)) => match op {
                BinaryOperator::Add => Ok(Value::String(a.clone() + b)),
                BinaryOperator::Eq => Ok(Value::Bool(a == b)),
                BinaryOperator::Ne => Ok(Value::Bool(a != b)),
                _ => Err(RuntimeError {
                    message: format!("Unsupported operation {:?} on strings", op),
                }),
            },
            (Value::Bool(a), Value::Bool(b)) => match op {
                BinaryOperator::Eq => Ok(Value::Bool(a == b)),
                BinaryOperator::Ne => Ok(Value::Bool(a != b)),
                _ => Err(RuntimeError {
                    message: format!("Unsupported operation {:?} on booleans", op),
                }),
            },
            _ => Err(RuntimeError {
                message: format!("Unsupported operation on {}", left.type_name()),
            }),
        }
    }

    fn apply_integer_literal_op(
        &self,
        op: &BinaryOperator,
        a: &str,
        b: &str,
    ) -> Result<Value, RuntimeError> {
        let a: i128 = a.parse().map_err(|_| RuntimeError {
            message: format!("Cannot parse '{}' as integer", a),
        })?;
        let b: i128 = b.parse().map_err(|_| RuntimeError {
            message: format!("Cannot parse '{}' as integer", b),
        })?;

        match op {
            BinaryOperator::Add => {
                let result = a.checked_add(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} + {}", a, b),
                })?;
                Ok(Value::Integer(result.to_string()))
            }
            BinaryOperator::Sub => {
                let result = a.checked_sub(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} - {}", a, b),
                })?;
                Ok(Value::Integer(result.to_string()))
            }
            BinaryOperator::Mul => {
                let result = a.checked_mul(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} * {}", a, b),
                })?;
                Ok(Value::Integer(result.to_string()))
            }
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                let result = a.checked_div(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} / {}", a, b),
                })?;
                Ok(Value::Integer(result.to_string()))
            }
            BinaryOperator::Eq => Ok(Value::Bool(a == b)),
            BinaryOperator::Ne => Ok(Value::Bool(a != b)),
            BinaryOperator::Lt => Ok(Value::Bool(a < b)),
            BinaryOperator::Gt => Ok(Value::Bool(a > b)),
            BinaryOperator::Le => Ok(Value::Bool(a <= b)),
            BinaryOperator::Ge => Ok(Value::Bool(a >= b)),
        }
    }

    fn apply_decimal_literal_op(
        &self,
        op: &BinaryOperator,
        a: &str,
        b: &str,
    ) -> Result<Value, RuntimeError> {
        let a: f64 = a.parse().map_err(|_| RuntimeError {
            message: format!("Cannot parse '{}' as decimal", a),
        })?;
        let b: f64 = b.parse().map_err(|_| RuntimeError {
            message: format!("Cannot parse '{}' as decimal", b),
        })?;

        match op {
            BinaryOperator::Add => Ok(Value::Decimal((a + b).to_string())),
            BinaryOperator::Sub => Ok(Value::Decimal((a - b).to_string())),
            BinaryOperator::Mul => Ok(Value::Decimal((a * b).to_string())),
            BinaryOperator::Div => Ok(Value::Decimal((a / b).to_string())),
            BinaryOperator::Eq => Ok(Value::Bool(a == b)),
            BinaryOperator::Ne => Ok(Value::Bool(a != b)),
            BinaryOperator::Lt => Ok(Value::Bool(a < b)),
            BinaryOperator::Gt => Ok(Value::Bool(a > b)),
            BinaryOperator::Le => Ok(Value::Bool(a <= b)),
            BinaryOperator::Ge => Ok(Value::Bool(a >= b)),
        }
    }

    fn apply_i128_op(&self, op: &BinaryOperator, a: i128, b: i128) -> Result<Value, RuntimeError> {
        match op {
            BinaryOperator::Add => {
                let result = a.checked_add(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} + {}", a, b),
                })?;
                Ok(Value::I128(result))
            }
            BinaryOperator::Sub => {
                let result = a.checked_sub(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} - {}", a, b),
                })?;
                Ok(Value::I128(result))
            }
            BinaryOperator::Mul => {
                let result = a.checked_mul(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} * {}", a, b),
                })?;
                Ok(Value::I128(result))
            }
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                let result = a.checked_div(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} / {}", a, b),
                })?;
                Ok(Value::I128(result))
            }
            BinaryOperator::Eq => Ok(Value::Bool(a == b)),
            BinaryOperator::Ne => Ok(Value::Bool(a != b)),
            BinaryOperator::Lt => Ok(Value::Bool(a < b)),
            BinaryOperator::Gt => Ok(Value::Bool(a > b)),
            BinaryOperator::Le => Ok(Value::Bool(a <= b)),
            BinaryOperator::Ge => Ok(Value::Bool(a >= b)),
        }
    }

    fn apply_u128_op(&self, op: &BinaryOperator, a: u128, b: u128) -> Result<Value, RuntimeError> {
        match op {
            BinaryOperator::Add => {
                let result = a.checked_add(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} + {}", a, b),
                })?;
                Ok(Value::U128(result))
            }
            BinaryOperator::Sub => {
                let result = a.checked_sub(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} - {}", a, b),
                })?;
                Ok(Value::U128(result))
            }
            BinaryOperator::Mul => {
                let result = a.checked_mul(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} * {}", a, b),
                })?;
                Ok(Value::U128(result))
            }
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                let result = a.checked_div(b).ok_or_else(|| RuntimeError {
                    message: format!("Integer overflow: {} / {}", a, b),
                })?;
                Ok(Value::U128(result))
            }
            BinaryOperator::Eq => Ok(Value::Bool(a == b)),
            BinaryOperator::Ne => Ok(Value::Bool(a != b)),
            BinaryOperator::Lt => Ok(Value::Bool(a < b)),
            BinaryOperator::Gt => Ok(Value::Bool(a > b)),
            BinaryOperator::Le => Ok(Value::Bool(a <= b)),
            BinaryOperator::Ge => Ok(Value::Bool(a >= b)),
        }
    }

    fn apply_i8_op(&self, op: &BinaryOperator, a: i8, b: i8) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::I8(a.wrapping_add(b)),
            BinaryOperator::Sub => Value::I8(a.wrapping_sub(b)),
            BinaryOperator::Mul => Value::I8(a.wrapping_mul(b)),
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                Value::I8(a.wrapping_div(b))
            }
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_i16_op(&self, op: &BinaryOperator, a: i16, b: i16) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::I16(a.wrapping_add(b)),
            BinaryOperator::Sub => Value::I16(a.wrapping_sub(b)),
            BinaryOperator::Mul => Value::I16(a.wrapping_mul(b)),
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                Value::I16(a.wrapping_div(b))
            }
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_i32_op(&self, op: &BinaryOperator, a: i32, b: i32) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::I32(a.wrapping_add(b)),
            BinaryOperator::Sub => Value::I32(a.wrapping_sub(b)),
            BinaryOperator::Mul => Value::I32(a.wrapping_mul(b)),
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                Value::I32(a.wrapping_div(b))
            }
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_i64_op(&self, op: &BinaryOperator, a: i64, b: i64) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::I64(a.wrapping_add(b)),
            BinaryOperator::Sub => Value::I64(a.wrapping_sub(b)),
            BinaryOperator::Mul => Value::I64(a.wrapping_mul(b)),
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                Value::I64(a.wrapping_div(b))
            }
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_u8_op(&self, op: &BinaryOperator, a: u8, b: u8) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::U8(a.wrapping_add(b)),
            BinaryOperator::Sub => Value::U8(a.wrapping_sub(b)),
            BinaryOperator::Mul => Value::U8(a.wrapping_mul(b)),
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                Value::U8(a.wrapping_div(b))
            }
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_u16_op(&self, op: &BinaryOperator, a: u16, b: u16) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::U16(a.wrapping_add(b)),
            BinaryOperator::Sub => Value::U16(a.wrapping_sub(b)),
            BinaryOperator::Mul => Value::U16(a.wrapping_mul(b)),
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                Value::U16(a.wrapping_div(b))
            }
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_u32_op(&self, op: &BinaryOperator, a: u32, b: u32) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::U32(a.wrapping_add(b)),
            BinaryOperator::Sub => Value::U32(a.wrapping_sub(b)),
            BinaryOperator::Mul => Value::U32(a.wrapping_mul(b)),
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                Value::U32(a.wrapping_div(b))
            }
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_u64_op(&self, op: &BinaryOperator, a: u64, b: u64) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::U64(a.wrapping_add(b)),
            BinaryOperator::Sub => Value::U64(a.wrapping_sub(b)),
            BinaryOperator::Mul => Value::U64(a.wrapping_mul(b)),
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                Value::U64(a.wrapping_div(b))
            }
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_f32_op(&self, op: &BinaryOperator, a: f32, b: f32) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::F32(a + b),
            BinaryOperator::Sub => Value::F32(a - b),
            BinaryOperator::Mul => Value::F32(a * b),
            BinaryOperator::Div => Value::F32(a / b),
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_f64_op(&self, op: &BinaryOperator, a: f64, b: f64) -> Result<Value, RuntimeError> {
        Ok(match op {
            BinaryOperator::Add => Value::F64(a + b),
            BinaryOperator::Sub => Value::F64(a - b),
            BinaryOperator::Mul => Value::F64(a * b),
            BinaryOperator::Div => Value::F64(a / b),
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }
}

pub fn interpret(
    module: &ModuleAST,
    capture_output: bool,
) -> Result<InterpreterOutput, RuntimeError> {
    let output_buffer = if capture_output {
        Some(Rc::new(RefCell::new(Vec::new())))
    } else {
        None
    };

    let output_writer = match &output_buffer {
        Some(buf) => OutputWriter::Captured(Rc::clone(buf)),
        None => OutputWriter::Stdout,
    };

    let mut interpreter = Interpreter::new(module, output_writer);

    let main_func = interpreter
        .find_function("main")
        .ok_or_else(|| RuntimeError {
            message: "No 'main' function found".to_string(),
        })?;

    if !main_func.params.is_empty() {
        return Err(RuntimeError {
            message: "'main' function should not have parameters".to_string(),
        });
    }

    let value = interpreter.call_function("main", vec![])?;

    let captured_output = output_buffer.map(|buf| {
        let bytes = buf.borrow();
        String::from_utf8_lossy(&bytes).to_string()
    });

    Ok(InterpreterOutput {
        value,
        captured_output,
    })
}
