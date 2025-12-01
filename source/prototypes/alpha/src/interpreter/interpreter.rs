use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use crate::parser::{
    BinaryOperator, Block, Expr, FunctionDef, Literal, ModuleAST, Statement, TypeExpr,
};

#[derive(Debug, Clone)]
pub enum Value {
    I8(i64),
    I16(i64),
    I32(i64),
    I64(i64),
    U8(i64),
    U16(i64),
    U32(i64),
    U64(i64),
    F32(f64),
    F64(f64),
    String(String),
    Bool(bool),
    Unit,
}

impl Value {
    fn as_i64(&self) -> Option<i64> {
        match self {
            Value::I8(n) => Some(*n),
            Value::I16(n) => Some(*n),
            Value::I32(n) => Some(*n),
            Value::I64(n) => Some(*n),
            Value::U8(n) => Some(*n),
            Value::U16(n) => Some(*n),
            Value::U32(n) => Some(*n),
            Value::U64(n) => Some(*n),
            _ => None,
        }
    }

    fn as_f64(&self) -> Option<f64> {
        match self {
            Value::F32(n) => Some(*n),
            Value::F64(n) => Some(*n),
            _ => None,
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::I8(_) => "i8",
            Value::I16(_) => "i16",
            Value::I32(_) => "i32",
            Value::I64(_) => "i64",
            Value::U8(_) => "u8",
            Value::U16(_) => "u16",
            Value::U32(_) => "u32",
            Value::U64(_) => "u64",
            Value::F32(_) => "f32",
            Value::F64(_) => "f64",
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Unit => "unit",
        }
    }

    fn same_type(&self, other: &Value) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I8(n) => write!(f, "{}", n),
            Value::I16(n) => write!(f, "{}", n),
            Value::I32(n) => write!(f, "{}", n),
            Value::I64(n) => write!(f, "{}", n),
            Value::U8(n) => write!(f, "{}", n),
            Value::U16(n) => write!(f, "{}", n),
            Value::U32(n) => write!(f, "{}", n),
            Value::U64(n) => write!(f, "{}", n),
            Value::F32(n) => write!(f, "{}", n),
            Value::F64(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
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

pub enum OutputWriter {
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

    fn coerce_to_type(&self, value: Value, type_expr: &TypeExpr) -> Result<Value, RuntimeError> {
        let TypeExpr::Named(type_name) = type_expr;

        if let Some(n) = value.as_i64() {
            let coerced = match type_name.as_str() {
                "i8" => Value::I8(n as i8 as i64),
                "i16" => Value::I16(n as i16 as i64),
                "i32" => Value::I32(n as i32 as i64),
                "i64" => Value::I64(n),
                "u8" => Value::U8(n as u8 as i64),
                "u16" => Value::U16(n as u16 as i64),
                "u32" => Value::U32(n as u32 as i64),
                "u64" => Value::U64(n as u64 as i64),
                _ => {
                    return Err(RuntimeError {
                        message: format!("Cannot coerce integer to type '{}'", type_name),
                    });
                }
            };
            return Ok(coerced);
        }

        if let Some(n) = value.as_f64() {
            let coerced = match type_name.as_str() {
                "f32" => Value::F32(n as f32 as f64),
                "f64" => Value::F64(n),
                _ => {
                    return Err(RuntimeError {
                        message: format!("Cannot coerce float to type '{}'", type_name),
                    });
                }
            };
            return Ok(coerced);
        }

        Ok(value)
    }

    fn call_builtin(&mut self, name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match name {
            "println" => {
                if let Some(value) = args.first() {
                    self.output.write_line(&value.to_string());
                } else {
                    self.output.write_empty_line();
                }
                Ok(Value::Unit)
            }
            _ => Err(RuntimeError {
                message: format!("Unknown builtin function: {}", name),
            }),
        }
    }

    fn call_function(&mut self, name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if name == "println" {
            return self.call_builtin(name, args);
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

        let old_variables = std::mem::take(&mut self.variables);

        for (param, value) in func.params.iter().zip(args) {
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
                    let value = self.coerce_to_type(value, type_expr)?;
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
                Literal::Integer(n) => Value::I64(*n),
                Literal::Float(f) => Value::F64(*f),
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
        }
    }

    fn apply_binary_op(
        &self,
        op: &BinaryOperator,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if !left.same_type(&right) {
            return Err(RuntimeError {
                message: format!(
                    "Type mismatch in binary operation: {} and {}",
                    left.type_name(),
                    right.type_name()
                ),
            });
        }

        match (&left, &right) {
            (Value::I8(a), Value::I8(b)) => self.apply_int_op(op, *a, *b, |n| Value::I8(n)),
            (Value::I16(a), Value::I16(b)) => self.apply_int_op(op, *a, *b, |n| Value::I16(n)),
            (Value::I32(a), Value::I32(b)) => self.apply_int_op(op, *a, *b, |n| Value::I32(n)),
            (Value::I64(a), Value::I64(b)) => self.apply_int_op(op, *a, *b, |n| Value::I64(n)),
            (Value::U8(a), Value::U8(b)) => self.apply_int_op(op, *a, *b, |n| Value::U8(n)),
            (Value::U16(a), Value::U16(b)) => self.apply_int_op(op, *a, *b, |n| Value::U16(n)),
            (Value::U32(a), Value::U32(b)) => self.apply_int_op(op, *a, *b, |n| Value::U32(n)),
            (Value::U64(a), Value::U64(b)) => self.apply_int_op(op, *a, *b, |n| Value::U64(n)),
            (Value::F32(a), Value::F32(b)) => self.apply_float_op(op, *a, *b, |n| Value::F32(n)),
            (Value::F64(a), Value::F64(b)) => self.apply_float_op(op, *a, *b, |n| Value::F64(n)),
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

    fn apply_int_op<F>(
        &self,
        op: &BinaryOperator,
        a: i64,
        b: i64,
        wrap: F,
    ) -> Result<Value, RuntimeError>
    where
        F: Fn(i64) -> Value,
    {
        Ok(match op {
            BinaryOperator::Add => wrap(a + b),
            BinaryOperator::Sub => wrap(a - b),
            BinaryOperator::Mul => wrap(a * b),
            BinaryOperator::Div => {
                if b == 0 {
                    return Err(RuntimeError {
                        message: "Division by zero".to_string(),
                    });
                }
                wrap(a / b)
            }
            BinaryOperator::Eq => Value::Bool(a == b),
            BinaryOperator::Ne => Value::Bool(a != b),
            BinaryOperator::Lt => Value::Bool(a < b),
            BinaryOperator::Gt => Value::Bool(a > b),
            BinaryOperator::Le => Value::Bool(a <= b),
            BinaryOperator::Ge => Value::Bool(a >= b),
        })
    }

    fn apply_float_op<F>(
        &self,
        op: &BinaryOperator,
        a: f64,
        b: f64,
        wrap: F,
    ) -> Result<Value, RuntimeError>
    where
        F: Fn(f64) -> Value,
    {
        Ok(match op {
            BinaryOperator::Add => wrap(a + b),
            BinaryOperator::Sub => wrap(a - b),
            BinaryOperator::Mul => wrap(a * b),
            BinaryOperator::Div => wrap(a / b),
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
