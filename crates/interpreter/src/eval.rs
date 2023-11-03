use crate::{InterpretError, InterpretResult, Value};

pub(crate) fn eval_binary_op(
    op: hir::BinaryOp,
    lhs: &Value,
    rhs: &Value,
) -> InterpretResult<Value> {
    match op {
        hir::BinaryOp::Add => eval_add(lhs, rhs),
        hir::BinaryOp::Sub => eval_sub(lhs, rhs),
        hir::BinaryOp::Mul => eval_mul(lhs, rhs),
        hir::BinaryOp::Div => eval_div(lhs, rhs),
        hir::BinaryOp::Mod => eval_mod(lhs, rhs),
        hir::BinaryOp::Quot => eval_quot(lhs, rhs),
        hir::BinaryOp::Pow => eval_pow(lhs, rhs),
        hir::BinaryOp::And => eval_logic_and(lhs, rhs),
        hir::BinaryOp::Or => eval_logic_or(lhs, rhs),
        hir::BinaryOp::Equals => eval_eq(lhs, rhs),
        hir::BinaryOp::NotEquals => eval_eq(lhs, rhs).map(|res| {
            let Value::Bool(res) = res else {
                unreachable!()
            };
            Value::Bool(!res)
        }),
        hir::BinaryOp::LessThan => eval_lt(lhs, rhs),
        hir::BinaryOp::LessEquals => eval_le(lhs, rhs),
        hir::BinaryOp::GreaterThan => eval_gt(lhs, rhs),
        hir::BinaryOp::GreaterEquals => eval_ge(lhs, rhs),
        hir::BinaryOp::SubScript => eval_subscript(lhs, rhs),
        hir::BinaryOp::Dot => {
            unreachable!(
                "`Interpreter::eval` passes this to `eval_string_attrs`, so we never recieve a dot expr"
            )
        }
    }
}

pub(crate) fn eval_string_attrs(
    op: hir::BinaryOp,
    lhs: &Value,
    name: &str,
) -> Option<InterpretResult<Value>> {
    if matches!(op, hir::BinaryOp::Dot) {
        let Value::String(s) = lhs else {
            return Some(Err(InterpretError::MismatchedTypes {
                expected: vec!["string"],
                found: lhs.type_str(),
            }));
        };

        return match name {
            "length" => {
                let Ok(len) = s.len().try_into() else {
                    return Some(Err(InterpretError::IntegerTooLarge));
                };
                Some(Ok(Value::Int(len)))
            }
            "lower" => {
                Some(Ok(Value::String(s.to_string().to_lowercase().into())))
            }
            "upper" => {
                Some(Ok(Value::String(s.to_string().to_uppercase().into())))
            }
            _ => Some(Err(InterpretError::InvalidDotTarget {
                name: name.into(),
            })),
        };
    }

    None
}

pub(crate) fn eval_unary_op(
    operand: &Value,
    op: hir::UnaryOp,
) -> InterpretResult<Value> {
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

fn eval_eq(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if lhs.same_type(rhs) {
        Ok(Value::Bool(lhs == rhs))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec![lhs.type_str()],
            found: rhs.type_str(),
        })
    }
}

fn eval_subscript(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Array(arr), Value::Int(i)) = (lhs, rhs) {
        Ok(arr
            .get(
                usize::try_from(*i)
                    .map_err(|_| InterpretError::IntegerTooLarge)?,
            )
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

#[allow(clippy::cast_precision_loss)]
fn eval_ge(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Bool(i1 >= i2))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Bool(f1 >= f2))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Bool((*i as f64) >= *f))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Bool(*f >= *i as f64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_precision_loss)]
fn eval_le(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Bool(i1 <= i2))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Bool(f1 <= f2))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Bool((*i as f64) <= *f))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Bool(*f <= *i as f64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_precision_loss)]
fn eval_lt(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Bool(i1 < i2))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Bool(f1 < f2))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Bool((*i as f64) < *f))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Bool(*f < *i as f64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

fn eval_logic_or(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Bool(b1), Value::Bool(b2)) = (lhs, rhs) {
        Ok(Value::Bool(*b1 || *b2))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["boolean"],
            found: lhs.type_str(),
        })
    }
}

fn eval_logic_and(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Bool(b1), Value::Bool(b2)) = (lhs, rhs) {
        Ok(Value::Bool(*b1 && *b2))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["boolean"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_precision_loss)]
fn eval_pow(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Int(
            i1.pow(
                (*i2)
                    .try_into()
                    .map_err(|_| InterpretError::IntegerTooLarge)?,
            ),
        ))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Float(f1.powf(*f2)))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Float((*i as f64).powf(*f)))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Float(f.powf(*i as f64)))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_possible_truncation, clippy::cast_precision_loss)]
fn eval_quot(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Int(i1.div_euclid(*i2)))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Int(f1.div_euclid(*f2) as i64))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Int((*i as f64).div_euclid(*f) as i64))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Int(f.div_euclid(*i as f64) as i64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_possible_truncation, clippy::cast_precision_loss)]
fn eval_mod(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Int(i1 % i2))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Int((f1 % f2) as i64))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Int((*i as f64 % f) as i64))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Int((f % *i as f64) as i64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_precision_loss)]
fn eval_div(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Float(*i1 as f64 / *i2 as f64))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Float(f1 / f2))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Float(*i as f64 / f))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Float(f / *i as f64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_precision_loss)]
fn eval_gt(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Bool(i1 > i2))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Bool(f1 > f2))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Bool((*i as f64) > *f))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Bool(*f > *i as f64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_precision_loss)]
fn eval_mul(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Int(i1 * i2))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Float(f1 * f2))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Float(*i as f64 * f))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Float(f * *i as f64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_precision_loss)]
fn eval_sub(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Int(i1 - i2))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Float(f1 - f2))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Float(*i as f64 - f))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Float(f - *i as f64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["int", "float"],
            found: lhs.type_str(),
        })
    }
}

#[allow(clippy::cast_precision_loss)]
fn eval_add(lhs: &Value, rhs: &Value) -> InterpretResult<Value> {
    if let (Value::String(s1), Value::String(s2)) = (lhs, rhs) {
        Ok(Value::String(format!("{s1}{s2}").into()))
    } else if let (Value::Int(i1), Value::Int(i2)) = (lhs, rhs) {
        Ok(Value::Int(i1 + i2))
    } else if let (Value::Float(f1), Value::Float(f2)) = (lhs, rhs) {
        Ok(Value::Float(f1 + f2))
    } else if let (Value::Int(i), Value::Float(f)) = (lhs, rhs) {
        Ok(Value::Float(*i as f64 + *f))
    } else if let (Value::Float(f), Value::Int(i)) = (lhs, rhs) {
        Ok(Value::Float(*f + *i as f64))
    } else {
        Err(InterpretError::MismatchedTypes {
            expected: vec!["string", "int", "float"],
            found: lhs.type_str(),
        })
    }
}
