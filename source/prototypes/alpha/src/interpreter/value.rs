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
    Void,
    Integer(String),
    Decimal(String),
}

fn parse_integer_literal(s: &str) -> Result<i128, RuntimeError> {
    s.parse().map_err(|_| RuntimeError {
        message: format!("Cannot parse '{}' as integer", s),
    })
}

fn convert_to_signed<T>(
    s: &str,
    type_name: &str,
    min: i128,
    max: i128,
    wrap: fn(T) -> Value,
) -> Result<Value, RuntimeError>
where
    T: TryFrom<i128>,
{
    let n = parse_integer_literal(s)?;
    if n < min || n > max {
        return Err(RuntimeError {
            message: format!(
                "Value {} does not fit in {} (range {} to {})",
                n, type_name, min, max
            ),
        });
    }
    Ok(wrap(T::try_from(n).ok().unwrap()))
}

fn convert_to_unsigned<T>(
    s: &str,
    type_name: &str,
    max: i128,
    wrap: fn(T) -> Value,
) -> Result<Value, RuntimeError>
where
    T: TryFrom<i128>,
{
    let n = parse_integer_literal(s)?;
    if n < 0 || n > max {
        return Err(RuntimeError {
            message: format!(
                "Value {} does not fit in {} (range 0 to {})",
                n, type_name, max
            ),
        });
    }
    Ok(wrap(T::try_from(n).ok().unwrap()))
}

impl Value {
    pub fn type_name(&self) -> &'static str {
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
            Value::Void => "unit",
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

    pub fn same_type(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Integer(_), other) => other.is_integer_type(),
            (other, Value::Integer(_)) => other.is_integer_type(),
            (Value::Decimal(_), other) => other.is_float_type(),
            (other, Value::Decimal(_)) => other.is_float_type(),
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }

    pub fn eq_value(&self, other: &Value) -> bool {
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
            (Value::Void, Value::Void) => true,
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
                convert_to_signed(s, "i8", i8::MIN as i128, i8::MAX as i128, Value::I8)
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i8", self.type_name()),
            }),
        }
    }

    pub fn to_i16(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                convert_to_signed(s, "i16", i16::MIN as i128, i16::MAX as i128, Value::I16)
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i16", self.type_name()),
            }),
        }
    }

    pub fn to_i32(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                convert_to_signed(s, "i32", i32::MIN as i128, i32::MAX as i128, Value::I32)
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i32", self.type_name()),
            }),
        }
    }

    pub fn to_i64(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                convert_to_signed(s, "i64", i64::MIN as i128, i64::MAX as i128, Value::I64)
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i64", self.type_name()),
            }),
        }
    }

    pub fn to_i128(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n = parse_integer_literal(s)?;
                Ok(Value::I128(n))
            }
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to i128", self.type_name()),
            }),
        }
    }

    pub fn to_u8(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => convert_to_unsigned(s, "u8", u8::MAX as i128, Value::U8),
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to u8", self.type_name()),
            }),
        }
    }

    pub fn to_u16(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => convert_to_unsigned(s, "u16", u16::MAX as i128, Value::U16),
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to u16", self.type_name()),
            }),
        }
    }

    pub fn to_u32(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => convert_to_unsigned(s, "u32", u32::MAX as i128, Value::U32),
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to u32", self.type_name()),
            }),
        }
    }

    pub fn to_u64(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => convert_to_unsigned(s, "u64", u64::MAX as i128, Value::U64),
            _ => Err(RuntimeError {
                message: format!("Cannot convert {} to u64", self.type_name()),
            }),
        }
    }

    pub fn to_u128(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Integer(s) => {
                let n = parse_integer_literal(s)?;
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
            Value::Void => write!(f, "()"),
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
