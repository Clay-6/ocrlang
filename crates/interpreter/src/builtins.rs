use std::io::{BufRead, Read, Write};

use crate::{InterpretError, Interpreter, Value};

pub(crate) fn int_cast(val: &Value) -> Result<Value, InterpretError> {
    match *val {
        Value::Bool(b) => Ok(Value::Int(b.into())),
        #[allow(clippy::cast_possible_truncation)]
        Value::Float(f) => Ok(Value::Int(f as i64)),
        Value::Char(c) => Ok(Value::Int(u32::from(c).into())),
        Value::String(ref s) => {
            Ok(Value::Int(s.trim().parse().map_err(|_| {
                InterpretError::CastFailure {
                    value: val.clone(),
                    target: "int",
                }
            })?))
        }
        Value::Int(_) => Ok(val.clone()),
        _ => Err(InterpretError::InvalidCast {
            from: val.type_str(),
            to: "int",
        }),
    }
}

pub(crate) fn str_cast(val: &Value) -> Value {
    Value::String(val.to_string().into())
}

pub(crate) fn bool_cast(val: &Value) -> Result<Value, InterpretError> {
    match *val {
        Value::Int(i) => Ok(Value::Bool(match i {
            1 => true,
            0 => false,
            _ => Err(InterpretError::CastFailure {
                value: val.clone(),
                target: "boolean",
            })?,
        })),
        Value::Float(f) => Ok(Value::Bool(if (f - 1.0).abs() < f64::EPSILON {
            true
        } else if f == 0.0 {
            false
        } else {
            Err(InterpretError::CastFailure {
                value: val.clone(),
                target: "boolean",
            })?
        })),
        Value::String(ref s) => {
            Ok(Value::Bool(match s.to_lowercase().trim() {
                "true" => true,
                "false" => false,
                _ => Err(InterpretError::CastFailure {
                    value: val.clone(),
                    target: "boolean",
                })?,
            }))
        }
        Value::Bool(_) => Ok(val.clone()),
        _ => Err(InterpretError::InvalidCast {
            from: val.type_str(),
            to: "boolean",
        }),
    }
}

pub(crate) fn float_cast(val: &Value) -> Result<Value, InterpretError> {
    match *val {
        #[allow(clippy::cast_precision_loss)]
        Value::Int(i) => Ok(Value::Float(i as f64)),
        Value::Float(_) => Ok(val.clone()),
        Value::Char(c) => Ok(Value::Float(u32::from(c).into())),
        Value::String(ref s) => {
            Ok(Value::Float(s.trim().parse().map_err(|_| {
                InterpretError::CastFailure {
                    value: val.clone(),
                    target: "float",
                }
            })?))
        }
        Value::Bool(b) => Ok(Value::Float(b.into())),
        _ => Err(InterpretError::InvalidCast {
            from: val.type_str(),
            to: "float",
        }),
    }
}

pub(crate) fn asc(args: &Value) -> Result<Value, InterpretError> {
    let Value::Char(c) = args else {
        return Err(InterpretError::MismatchedTypes {
            expected: vec!["char"],
            found: args.type_str(),
        });
    };
    Ok(Value::Int(u32::from(*c).into()))
}

pub(crate) fn chr(args: &Value) -> Result<Value, InterpretError> {
    let Value::Int(n) = args else {
        return Err(InterpretError::MismatchedTypes {
            expected: vec!["int"],
            found: args.type_str(),
        });
    };
    let Ok(n) = u8::try_from(*n) else {
        return Err(InterpretError::IntegerTooLarge);
    };
    Ok(Value::Char(n.into()))
}

pub(crate) fn random(
    upper: &Value,
    lower: &Value,
) -> Result<Value, InterpretError> {
    use rand::Rng;
    let mut rng = rand::thread_rng();

    if let (Value::Int(lower), Value::Int(upper)) = (upper, lower) {
        Ok(Value::Int(rng.gen_range(*lower..=*upper)))
    } else if let (Value::Float(lower), Value::Float(upper)) = (upper, lower) {
        Ok(Value::Float(rng.gen_range(*lower..=*upper)))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["float", "int"],
            found: lower.type_str(),
        })
    }
}

pub(crate) fn open(filename: &Value) -> Result<Value, InterpretError> {
    if let Value::String(path) = filename {
        let file = std::fs::OpenOptions::new()
            .read(true)
            .append(true)
            .open(path.as_str())?;
        Ok(Value::File(file.into()))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["string"],
            found: filename.type_str(),
        })
    }
}

pub(crate) fn new_file(filename: &Value) -> Result<Value, InterpretError> {
    if let Value::String(path) = filename {
        std::fs::File::create(path.as_str())?;
        Ok(Value::Unit)
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["string"],
            found: filename.type_str(),
        })
    }
}

pub(crate) fn print<I, O: Write>(
    interpreter: &mut Interpreter<I, O>,
    val: &Value,
) -> Result<Value, InterpretError> {
    writeln!(interpreter.output, "{val}")
        .map(|()| Value::Unit)
        .map_err(Into::into)
}

pub(crate) fn input<I: Read, O: Write>(
    interpreter: &mut Interpreter<I, O>,
    args: &Value,
) -> Result<Value, InterpretError> {
    if let Value::String(prompt) = args {
        write!(interpreter.output, "{prompt}").unwrap();
        interpreter.output.flush().unwrap();
        let mut buf = String::new();
        interpreter.input.read_line(&mut buf).unwrap();
        Ok(Value::String(buf.into()))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["string"],
            found: args.type_str(),
        })
    }
}
