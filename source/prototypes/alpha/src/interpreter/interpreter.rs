use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use crate::parser::{
    AssignOp, BinaryOperator, Block, Expr, FunctionDef, Literal, ModuleAST, Statement, TypeExpr,
    UnaryOperator,
};

use super::builtins;
use super::value::{RuntimeError, Value};

trait WrappingOps: Sized + Copy + PartialEq + PartialOrd + std::fmt::Display {
    fn wrapping_add(self, rhs: Self) -> Self;
    fn wrapping_sub(self, rhs: Self) -> Self;
    fn wrapping_mul(self, rhs: Self) -> Self;
    fn wrapping_div(self, rhs: Self) -> Self;
    fn is_zero(&self) -> bool;
}

trait CheckedOps: Sized + Copy + PartialEq + PartialOrd + std::fmt::Display {
    fn checked_add(self, rhs: Self) -> Option<Self>;
    fn checked_sub(self, rhs: Self) -> Option<Self>;
    fn checked_mul(self, rhs: Self) -> Option<Self>;
    fn checked_div(self, rhs: Self) -> Option<Self>;
    fn is_zero(&self) -> bool;
}

trait FloatOps: Sized + Copy + PartialEq + PartialOrd {
    fn add(self, rhs: Self) -> Self;
    fn sub(self, rhs: Self) -> Self;
    fn mul(self, rhs: Self) -> Self;
    fn div(self, rhs: Self) -> Self;
}

fn apply_wrapping_int_op<T: WrappingOps>(
    op: &BinaryOperator,
    a: T,
    b: T,
    wrap: fn(T) -> Value,
) -> Result<Value, RuntimeError> {
    Ok(match op {
        BinaryOperator::Add => wrap(a.wrapping_add(b)),
        BinaryOperator::Sub => wrap(a.wrapping_sub(b)),
        BinaryOperator::Mul => wrap(a.wrapping_mul(b)),
        BinaryOperator::Div => {
            if b.is_zero() {
                return Err(RuntimeError {
                    message: "Division by zero".to_string(),
                });
            }
            wrap(a.wrapping_div(b))
        }
        BinaryOperator::Eq => Value::Bool(a == b),
        BinaryOperator::Ne => Value::Bool(a != b),
        BinaryOperator::Lt => Value::Bool(a < b),
        BinaryOperator::Gt => Value::Bool(a > b),
        BinaryOperator::Le => Value::Bool(a <= b),
        BinaryOperator::Ge => Value::Bool(a >= b),
    })
}

fn apply_checked_int_op<T: CheckedOps>(
    op: &BinaryOperator,
    a: T,
    b: T,
    wrap: fn(T) -> Value,
) -> Result<Value, RuntimeError> {
    match op {
        BinaryOperator::Add => {
            let result = a.checked_add(b).ok_or_else(|| RuntimeError {
                message: format!("Integer overflow: {} + {}", a, b),
            })?;
            Ok(wrap(result))
        }
        BinaryOperator::Sub => {
            let result = a.checked_sub(b).ok_or_else(|| RuntimeError {
                message: format!("Integer overflow: {} - {}", a, b),
            })?;
            Ok(wrap(result))
        }
        BinaryOperator::Mul => {
            let result = a.checked_mul(b).ok_or_else(|| RuntimeError {
                message: format!("Integer overflow: {} * {}", a, b),
            })?;
            Ok(wrap(result))
        }
        BinaryOperator::Div => {
            if b.is_zero() {
                return Err(RuntimeError {
                    message: "Division by zero".to_string(),
                });
            }
            let result = a.checked_div(b).ok_or_else(|| RuntimeError {
                message: format!("Integer overflow: {} / {}", a, b),
            })?;
            Ok(wrap(result))
        }
        BinaryOperator::Eq => Ok(Value::Bool(a == b)),
        BinaryOperator::Ne => Ok(Value::Bool(a != b)),
        BinaryOperator::Lt => Ok(Value::Bool(a < b)),
        BinaryOperator::Gt => Ok(Value::Bool(a > b)),
        BinaryOperator::Le => Ok(Value::Bool(a <= b)),
        BinaryOperator::Ge => Ok(Value::Bool(a >= b)),
    }
}

fn apply_float_op<T: FloatOps>(
    op: &BinaryOperator,
    a: T,
    b: T,
    wrap: fn(T) -> Value,
) -> Result<Value, RuntimeError> {
    Ok(match op {
        BinaryOperator::Add => wrap(a.add(b)),
        BinaryOperator::Sub => wrap(a.sub(b)),
        BinaryOperator::Mul => wrap(a.mul(b)),
        BinaryOperator::Div => wrap(a.div(b)),
        BinaryOperator::Eq => Value::Bool(a == b),
        BinaryOperator::Ne => Value::Bool(a != b),
        BinaryOperator::Lt => Value::Bool(a < b),
        BinaryOperator::Gt => Value::Bool(a > b),
        BinaryOperator::Le => Value::Bool(a <= b),
        BinaryOperator::Ge => Value::Bool(a >= b),
    })
}

impl WrappingOps for i8 {
    fn wrapping_add(self, rhs: Self) -> Self {
        self.wrapping_add(rhs)
    }
    fn wrapping_sub(self, rhs: Self) -> Self {
        self.wrapping_sub(rhs)
    }
    fn wrapping_mul(self, rhs: Self) -> Self {
        self.wrapping_mul(rhs)
    }
    fn wrapping_div(self, rhs: Self) -> Self {
        self.wrapping_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl WrappingOps for i16 {
    fn wrapping_add(self, rhs: Self) -> Self {
        self.wrapping_add(rhs)
    }
    fn wrapping_sub(self, rhs: Self) -> Self {
        self.wrapping_sub(rhs)
    }
    fn wrapping_mul(self, rhs: Self) -> Self {
        self.wrapping_mul(rhs)
    }
    fn wrapping_div(self, rhs: Self) -> Self {
        self.wrapping_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl WrappingOps for i32 {
    fn wrapping_add(self, rhs: Self) -> Self {
        self.wrapping_add(rhs)
    }
    fn wrapping_sub(self, rhs: Self) -> Self {
        self.wrapping_sub(rhs)
    }
    fn wrapping_mul(self, rhs: Self) -> Self {
        self.wrapping_mul(rhs)
    }
    fn wrapping_div(self, rhs: Self) -> Self {
        self.wrapping_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl WrappingOps for i64 {
    fn wrapping_add(self, rhs: Self) -> Self {
        self.wrapping_add(rhs)
    }
    fn wrapping_sub(self, rhs: Self) -> Self {
        self.wrapping_sub(rhs)
    }
    fn wrapping_mul(self, rhs: Self) -> Self {
        self.wrapping_mul(rhs)
    }
    fn wrapping_div(self, rhs: Self) -> Self {
        self.wrapping_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl WrappingOps for u8 {
    fn wrapping_add(self, rhs: Self) -> Self {
        self.wrapping_add(rhs)
    }
    fn wrapping_sub(self, rhs: Self) -> Self {
        self.wrapping_sub(rhs)
    }
    fn wrapping_mul(self, rhs: Self) -> Self {
        self.wrapping_mul(rhs)
    }
    fn wrapping_div(self, rhs: Self) -> Self {
        self.wrapping_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl WrappingOps for u16 {
    fn wrapping_add(self, rhs: Self) -> Self {
        self.wrapping_add(rhs)
    }
    fn wrapping_sub(self, rhs: Self) -> Self {
        self.wrapping_sub(rhs)
    }
    fn wrapping_mul(self, rhs: Self) -> Self {
        self.wrapping_mul(rhs)
    }
    fn wrapping_div(self, rhs: Self) -> Self {
        self.wrapping_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl WrappingOps for u32 {
    fn wrapping_add(self, rhs: Self) -> Self {
        self.wrapping_add(rhs)
    }
    fn wrapping_sub(self, rhs: Self) -> Self {
        self.wrapping_sub(rhs)
    }
    fn wrapping_mul(self, rhs: Self) -> Self {
        self.wrapping_mul(rhs)
    }
    fn wrapping_div(self, rhs: Self) -> Self {
        self.wrapping_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl WrappingOps for u64 {
    fn wrapping_add(self, rhs: Self) -> Self {
        self.wrapping_add(rhs)
    }
    fn wrapping_sub(self, rhs: Self) -> Self {
        self.wrapping_sub(rhs)
    }
    fn wrapping_mul(self, rhs: Self) -> Self {
        self.wrapping_mul(rhs)
    }
    fn wrapping_div(self, rhs: Self) -> Self {
        self.wrapping_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl CheckedOps for i128 {
    fn checked_add(self, rhs: Self) -> Option<Self> {
        self.checked_add(rhs)
    }
    fn checked_sub(self, rhs: Self) -> Option<Self> {
        self.checked_sub(rhs)
    }
    fn checked_mul(self, rhs: Self) -> Option<Self> {
        self.checked_mul(rhs)
    }
    fn checked_div(self, rhs: Self) -> Option<Self> {
        self.checked_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl CheckedOps for u128 {
    fn checked_add(self, rhs: Self) -> Option<Self> {
        self.checked_add(rhs)
    }
    fn checked_sub(self, rhs: Self) -> Option<Self> {
        self.checked_sub(rhs)
    }
    fn checked_mul(self, rhs: Self) -> Option<Self> {
        self.checked_mul(rhs)
    }
    fn checked_div(self, rhs: Self) -> Option<Self> {
        self.checked_div(rhs)
    }
    fn is_zero(&self) -> bool {
        *self == 0
    }
}

impl FloatOps for f32 {
    fn add(self, rhs: Self) -> Self {
        self + rhs
    }
    fn sub(self, rhs: Self) -> Self {
        self - rhs
    }
    fn mul(self, rhs: Self) -> Self {
        self * rhs
    }
    fn div(self, rhs: Self) -> Self {
        self / rhs
    }
}

impl FloatOps for f64 {
    fn add(self, rhs: Self) -> Self {
        self + rhs
    }
    fn sub(self, rhs: Self) -> Self {
        self - rhs
    }
    fn mul(self, rhs: Self) -> Self {
        self * rhs
    }
    fn div(self, rhs: Self) -> Self {
        self / rhs
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
        let mut result = Value::Void;

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
                    result = Value::Void;
                }
                Statement::Assignment { name, op, value } => {
                    let new_value = self.evaluate(value)?;
                    let final_value = match op {
                        AssignOp::Assign => new_value,
                        AssignOp::AddAssign
                        | AssignOp::SubAssign
                        | AssignOp::MulAssign
                        | AssignOp::DivAssign => {
                            let current =
                                self.variables
                                    .get(name)
                                    .cloned()
                                    .ok_or_else(|| RuntimeError {
                                        message: format!("Undefined variable: {}", name),
                                    })?;
                            let bin_op = match op {
                                AssignOp::AddAssign => BinaryOperator::Add,
                                AssignOp::SubAssign => BinaryOperator::Sub,
                                AssignOp::MulAssign => BinaryOperator::Mul,
                                AssignOp::DivAssign => BinaryOperator::Div,
                                AssignOp::Assign => unreachable!(),
                            };
                            self.apply_binary_op(&bin_op, current, new_value)?
                        }
                    };
                    self.variables.insert(name.clone(), final_value);
                    result = Value::Void;
                }
                Statement::Expression(expr) => {
                    result = self.evaluate(expr)?;
                }
                Statement::Return(expr) => {
                    return match expr {
                        Some(e) => self.evaluate(e),
                        None => Ok(Value::Void),
                    };
                }
                Statement::If {
                    condition,
                    then_block,
                    else_block,
                } => {
                    let cond_val = self.evaluate(condition)?;
                    let cond_bool = match cond_val {
                        Value::Bool(b) => b,
                        _ => {
                            return Err(RuntimeError {
                                message: format!(
                                    "Condition must be a boolean, got {}",
                                    cond_val.type_name()
                                ),
                            });
                        }
                    };

                    if cond_bool {
                        let block_result = self.execute_block(then_block)?;
                        if matches!(then_block.statements.last(), Some(Statement::Return(_))) {
                            return Ok(block_result);
                        }
                        result = block_result;
                    } else if let Some(else_blk) = else_block {
                        let block_result = self.execute_block(else_blk)?;
                        if matches!(else_blk.statements.last(), Some(Statement::Return(_))) {
                            return Ok(block_result);
                        }
                        result = block_result;
                    }
                }
                Statement::For {
                    init,
                    condition,
                    update,
                    body,
                } => {
                    self.execute_statement(init)?;

                    loop {
                        let cond_val = self.evaluate(condition)?;
                        let cond_bool = match cond_val {
                            Value::Bool(b) => b,
                            _ => {
                                return Err(RuntimeError {
                                    message: format!(
                                        "For loop condition must be a boolean, got {}",
                                        cond_val.type_name()
                                    ),
                                });
                            }
                        };

                        if !cond_bool {
                            break;
                        }

                        let block_result = self.execute_block(body)?;
                        if matches!(body.statements.last(), Some(Statement::Return(_))) {
                            return Ok(block_result);
                        }

                        self.execute_statement(update)?;
                    }
                    result = Value::Void;
                }
            }
        }

        Ok(result)
    }

    fn execute_statement(&mut self, statement: &Statement) -> Result<Value, RuntimeError> {
        let block = Block {
            statements: vec![statement.clone()],
        };
        self.execute_block(&block)
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
            (Value::Integer(a), Value::Integer(b)) => apply_integer_literal_op(op, a, b),
            (Value::Decimal(a), Value::Decimal(b)) => apply_decimal_literal_op(op, a, b),
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
            (Value::I8(a), Value::I8(b)) => apply_wrapping_int_op(op, *a, *b, Value::I8),
            (Value::I16(a), Value::I16(b)) => apply_wrapping_int_op(op, *a, *b, Value::I16),
            (Value::I32(a), Value::I32(b)) => apply_wrapping_int_op(op, *a, *b, Value::I32),
            (Value::I64(a), Value::I64(b)) => apply_wrapping_int_op(op, *a, *b, Value::I64),
            (Value::I128(a), Value::I128(b)) => apply_checked_int_op(op, *a, *b, Value::I128),
            (Value::U8(a), Value::U8(b)) => apply_wrapping_int_op(op, *a, *b, Value::U8),
            (Value::U16(a), Value::U16(b)) => apply_wrapping_int_op(op, *a, *b, Value::U16),
            (Value::U32(a), Value::U32(b)) => apply_wrapping_int_op(op, *a, *b, Value::U32),
            (Value::U64(a), Value::U64(b)) => apply_wrapping_int_op(op, *a, *b, Value::U64),
            (Value::U128(a), Value::U128(b)) => apply_checked_int_op(op, *a, *b, Value::U128),
            (Value::F32(a), Value::F32(b)) => apply_float_op(op, *a, *b, Value::F32),
            (Value::F64(a), Value::F64(b)) => apply_float_op(op, *a, *b, Value::F64),
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
}

fn apply_integer_literal_op(op: &BinaryOperator, a: &str, b: &str) -> Result<Value, RuntimeError> {
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

fn apply_decimal_literal_op(op: &BinaryOperator, a: &str, b: &str) -> Result<Value, RuntimeError> {
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

    if !main_func.is_exported {
        return Err(RuntimeError {
            message: "'main' function must be exported".to_string(),
        });
    }

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
