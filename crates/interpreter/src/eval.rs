use smol_str::SmolStr;

use crate::{InterpretError, Value};

pub(crate) fn eval_binary_op(
    op: &hir::BinaryOp,
    lhs: Value,
    rhs: Value,
) -> Result<Value, InterpretError> {
    match op {
        hir::BinaryOp::Add => {
            if let (Value::String(s1), Value::String(s2)) = (&lhs, &rhs) {
                Ok(Value::String(format!("{s1}{s2}").into()))
            } else if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Int(i1 + i2))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Float(f1 + f2))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Float(*i as f64 + *f))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Float(*f + *i as f64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["string", "int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::Sub => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Int(i1 - i2))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Float(f1 - f2))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Float(*i as f64 - f))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Float(f - *i as f64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::Mul => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Int(i1 * i2))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Float(f1 * f2))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Float(*i as f64 * f))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Float(f * *i as f64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::Div => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Float(*i1 as f64 / *i2 as f64))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Float(f1 / f2))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Float(*i as f64 / f))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Float(f / *i as f64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::Mod => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Int(i1 % i2))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Int((f1 % f2) as i64))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Int((*i as f64 % f) as i64))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Int((f % *i as f64) as i64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::Quot => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Int(i1.div_euclid(*i2)))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Int(f1.div_euclid(*f2) as i64))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Int((*i as f64).div_euclid(*f) as i64))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Int(f.div_euclid(*i as f64) as i64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::Pow => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Int((*i1 as f64).powf(*i2 as f64) as i64))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Float(f1.powf(*f2)))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Float((*i as f64).powf(*f)))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Float(f.powf(*i as _)))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::And => {
            if let (Value::Bool(b1), Value::Bool(b2)) = (&lhs, rhs) {
                Ok(Value::Bool(*b1 && b2))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["boolean"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::Or => {
            if let (Value::Bool(b1), Value::Bool(b2)) = (&lhs, rhs) {
                Ok(Value::Bool(*b1 || b2))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["boolean"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::Equals => Ok(Value::Bool(lhs == rhs)),
        hir::BinaryOp::NotEquals => Ok(Value::Bool(lhs != rhs)),
        hir::BinaryOp::LessThan => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Bool(i1 < i2))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Bool(f1 < f2))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Bool((*i as f64) < *f))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Bool(*f < *i as f64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::LessEquals => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Bool(i1 <= i2))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Bool(f1 <= f2))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Bool((*i as f64) <= *f))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Bool(*f <= *i as f64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::GreaterThan => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Bool(i1 > i2))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Bool(f1 > f2))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Bool((*i as f64) > *f))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Bool(*f > *i as f64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::GreaterEquals => {
            if let (Value::Int(i1), Value::Int(i2)) = (&lhs, &rhs) {
                Ok(Value::Bool(i1 >= i2))
            } else if let (Value::Float(f1), Value::Float(f2)) = (&lhs, &rhs) {
                Ok(Value::Bool(f1 >= f2))
            } else if let (Value::Int(i), Value::Float(f)) = (&lhs, &rhs) {
                Ok(Value::Bool((*i as f64) >= *f))
            } else if let (Value::Float(f), Value::Int(i)) = (&lhs, &rhs) {
                Ok(Value::Bool(*f >= *i as f64))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: lhs.type_str(),
                })
            }
        }
        hir::BinaryOp::SubScript => {
            if let (Value::Array(arr), Value::Int(i)) = (&lhs, &rhs) {
                Ok(arr
                    .get(*i as usize)
                    .cloned()
                    .ok_or(InterpretError::IndexOutOfRange)?)
            } else if !matches!(lhs, Value::Array(_)) {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["array"],
                    found: lhs.type_str(),
                })
            } else if !matches!(rhs, Value::Int(_)) {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int"],
                    found: lhs.type_str(),
                })
            } else {
                // The only way we don't hit the first
                // block is if `lhs` isn't an array
                // and/or `rhs` isn't an int,
                // which we've already handled
                unreachable!();
            }
        }
        hir::BinaryOp::Dot => todo!(),
    }
}

pub(crate) fn eval_string_attrs(
    op: hir::BinaryOp,
    lhs: &Value,
    name: &SmolStr,
) -> Option<Result<Value, InterpretError>> {
    if matches!(op, hir::BinaryOp::Dot) {
        if let Value::String(s) = lhs {
            if name == "length" {
                return Some(Ok(Value::Int(s.len() as _)));
            }
            if name == "lower" {
                return Some(Ok(Value::String(s.to_string().to_lowercase().into())));
            }
            if name == "upper" {
                return Some(Ok(Value::String(s.to_string().to_uppercase().into())));
            }
        } else {
            return Some(Err(InterpretError::MismatchedTypes {
                expected: vec!["string"],
                found: lhs.type_str(),
            }));
        }
    }
    None
}

pub(crate) fn eval_unary_op(operand: Value, op: hir::UnaryOp) -> Result<Value, InterpretError> {
    match op {
        hir::UnaryOp::Neg => {
            if let Value::Int(i) = operand {
                Ok(Value::Int(-i))
            } else if let Value::Float(f) = operand {
                Ok(Value::Float(-f))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["int", "float"],
                    found: operand.type_str(),
                })
            }
        }
        hir::UnaryOp::Not => {
            if let Value::Bool(b) = operand {
                Ok(Value::Bool(!b))
            } else {
                Err(InterpretError::MismatchedTypes {
                    expected: vec!["bool"],
                    found: operand.type_str(),
                })
            }
        }
    }
}
