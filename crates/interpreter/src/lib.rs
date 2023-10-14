mod env;

use env::{Binding, Env, SubprogKind, Subprogram};
use hir::{Database, Stmt};
use smol_str::SmolStr;

pub type IResult<T> = Result<T, InterpretError>;

#[derive(Debug)]
pub struct Interpreter {
    env: Env,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Default::default(),
        }
    }

    pub fn run(&mut self, src: &str) -> IResult<()> {
        let (db, stmts) = hir::lower(ast::Root::cast(parser::parse(src).syntax()).unwrap());

        self.execute(&stmts, &db)
    }

    pub fn execute(&mut self, stmts: &[Stmt], db: &Database) -> IResult<()> {
        for stmt in stmts {
            self.exec_stmt(stmt, db)?;
        }

        Ok(())
    }

    fn exec_stmt(&mut self, stmt: &Stmt, db: &Database) -> IResult<Value> {
        match stmt {
            Stmt::Expr(e) => self.eval(e, db),
            Stmt::VarDef { name, kind, value } => {
                let value = self.eval(value, db)?;
                match kind {
                    hir::VarDefKind::Constant => self
                        .env
                        .insert_constant(name.clone(), value)
                        .map_err(|_| InterpretError::ReassignedConstant)
                        .map(|_| Value::Unit),
                    hir::VarDefKind::Global => todo!(),
                    hir::VarDefKind::Standard => {
                        self.env.insert(name.clone(), Binding::Var(value));
                        Ok(Value::Unit)
                    }
                }
            }
            Stmt::ArrayDef {
                name,
                kind,
                subscript,
                dimensions,
                value,
            } => todo!(),
            Stmt::SubprogramDef {
                kind,
                name,
                params,
                body,
            } => {
                let kind = match kind {
                    hir::SubprogramKind::Function => SubprogKind::Function,
                    hir::SubprogramKind::Procedure => SubprogKind::Procedure,
                };
                self.env.insert(
                    name.clone(),
                    Binding::Func(Subprogram {
                        kind,
                        params: params.clone(),
                        body: body.clone(),
                    }),
                );
                Ok(Value::Unit)
            }
            Stmt::ReturnStmt { value } => self.eval(db.get(*value), db),
            Stmt::IfElse {
                condition,
                body,
                elseifs,
                else_body,
            } => todo!(),
            Stmt::SwitchCase {
                scrutinee,
                cases,
                case_bodies,
                default_body,
            } => todo!(),
            Stmt::ForLoop {
                start,
                end,
                step,
                body,
            } => todo!(),
            Stmt::WhileLoop { condition, body } => todo!(),
            Stmt::DoUntilLoop { condition, body } => todo!(),
        }
    }

    fn eval(&mut self, expr: &hir::Expr, db: &Database) -> IResult<Value> {
        match expr {
            hir::Expr::Literal { value } => Ok(match value {
                hir::Literal::Int(i) => Value::Int(*i),
                hir::Literal::Float(f) => Value::Float(*f),
                hir::Literal::Char(c) => Value::Char(*c),
                hir::Literal::String(s) => Value::String(s.clone()),
                hir::Literal::Bool(b) => Value::Bool(*b),
                hir::Literal::Array(range) => {
                    let exprs = db
                        .get_range(range.clone())
                        .iter()
                        .map(|e| self.eval(e, db))
                        .collect::<IResult<Vec<_>>>()?;
                    let mut chunks = exprs.chunks_exact(2);
                    if !chunks.all(|chunk| chunk[0].same_type(&chunk[1])) {
                        return Err(InterpretError::HeterogeneousArray);
                    }
                    if !chunks.remainder().is_empty()
                        && exprs
                            .last()
                            .is_some_and(|e| e.same_type(&exprs[exprs.len() - 2]))
                    {
                        return Err(InterpretError::HeterogeneousArray);
                    }

                    Value::Array(exprs)
                }
            }),
            hir::Expr::Binary { op, lhs, rhs } => {
                let lhs = self.eval(db.get(*lhs), db)?;
                let rhs = self.eval(db.get(*rhs), db)?;
                if !lhs.same_type(&rhs)
                    && !matches!((&lhs, &rhs), (&Value::Int(_), &Value::Float(_)))
                    && !matches!((&lhs, &rhs), (&Value::Float(_), &Value::Int(_)))
                {
                    return Err(InterpretError::MismatchedTypes {
                        expected: vec![lhs.type_str()],
                        found: rhs.type_str(),
                    });
                }

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
                    hir::BinaryOp::Mod => todo!(),
                    hir::BinaryOp::Quot => todo!(),
                    hir::BinaryOp::Pow => todo!(),
                    hir::BinaryOp::And => todo!(),
                    hir::BinaryOp::Or => todo!(),
                    hir::BinaryOp::Equals => todo!(),
                    hir::BinaryOp::NotEquals => todo!(),
                    hir::BinaryOp::LessThan => todo!(),
                    hir::BinaryOp::LessEquals => todo!(),
                    hir::BinaryOp::GreaterThan => todo!(),
                    hir::BinaryOp::GreaterEquals => todo!(),
                    hir::BinaryOp::SubScript => todo!(),
                    hir::BinaryOp::Dot => todo!(),
                }
            }
            hir::Expr::Unary { op, opand } => {
                let operand = self.eval(db.get(*opand), db)?;
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
            hir::Expr::NameRef { name } => {
                let resolved = self.env.get_var(name);
                if let Some(v) = resolved {
                    Ok(v)
                } else {
                    Err(InterpretError::UnresolvedVariable)
                }
            }
            hir::Expr::Call { callee, args } => {
                let subprogram = self.env.get_subgrogram(callee);
                if let Some(sp) = subprogram {
                    if args.len() != sp.params.len() {
                        return Err(InterpretError::InvalidArgumentCount {
                            expected: sp.params.len(),
                            got: args.len(),
                        });
                    }
                    let inner_db = db.clone();
                    for (param, arg) in sp.params.iter().zip(inner_db.get_range(args.clone())) {
                        let val = self.eval(arg, &inner_db)?;
                        self.env.insert(param.clone(), Binding::Var(val));
                    }
                    self.call_subprog(sp.body, sp.kind, inner_db)
                } else {
                    Err(InterpretError::UnresolvedSubprogram)
                }
            }
            hir::Expr::Missing => Ok(Value::Unit),
        }
    }

    fn call_subprog(
        &mut self,
        body: Vec<Stmt>,
        kind: SubprogKind,
        db: Database,
    ) -> Result<Value, InterpretError> {
        match kind {
            SubprogKind::Function => match body.len().cmp(&1) {
                std::cmp::Ordering::Equal => {
                    self.execute(&body[..body.len() - 1], &db)?;
                    self.exec_stmt(body.last().unwrap(), &db)
                }
                std::cmp::Ordering::Greater => self.exec_stmt(&body[0], &db),
                std::cmp::Ordering::Less => Ok(Value::Unit),
            },
            SubprogKind::Procedure => self.execute(&body, &db).map(|_| Value::Unit),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Char(char),
    String(SmolStr),
    Bool(bool),
    Array(Vec<Value>),
    Unit,
}

impl Value {
    fn same_type(&self, b: &Value) -> bool {
        match self {
            Value::Int(_) => matches!(b, Value::Int(_)),
            Value::Float(_) => matches!(b, Value::Float(_)),
            Value::Char(_) => matches!(b, Value::Char(_)),
            Value::String(_) => matches!(b, Value::String(_)),
            Value::Bool(_) => matches!(b, Value::Bool(_)),
            Value::Array(_) => matches!(b, Value::Array(_)),
            Value::Unit => matches!(b, Value::Unit),
        }
    }

    fn type_str(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Char(_) => "character",
            Value::String(_) => "string",
            Value::Bool(_) => "boolean",
            Value::Array(_) => "array",
            Value::Unit => "unit",
        }
    }
}

#[derive(Debug)]
pub enum InterpretError {
    ReassignedConstant,
    HeterogeneousArray,
    MismatchedTypes {
        expected: Vec<&'static str>,
        found: &'static str,
    },
    UnresolvedVariable,
    UnresolvedSubprogram,
    InvalidArgumentCount {
        expected: usize,
        got: usize,
    },
}
#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn lower(src: &str) -> (Database, Vec<Stmt>) {
        hir::lower(ast::Root::cast(parser::parse(src).syntax()).unwrap())
    }

    fn check_eval(expr: &str, expected: Value) {
        let (db, stmts) = lower(expr);
        let evaled = Interpreter::new().exec_stmt(&stmts[0], &db).unwrap();
        assert_eq!(evaled, expected)
    }

    #[test]
    fn eval_array_literal() {
        check_eval(
            "[1, 2, 3, 4]",
            Value::Array(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
                Value::Int(4),
            ]),
        );
    }

    #[test]
    fn eval_string_literal() {
        check_eval("\"Hello World!\"", Value::String("Hello World!".into()));
    }

    #[test]
    fn eval_bool_literals() {
        check_eval("true", Value::Bool(true));
        check_eval("false", Value::Bool(false));
    }

    #[test]
    fn eval_number_literals() {
        check_eval("42", Value::Int(42));
        check_eval("6.9", Value::Float(6.9));
    }

    #[test]
    fn eval_char_literal() {
        check_eval("'c'", Value::Char('c'));
    }

    #[test]
    fn eval_unary() {
        check_eval("-5", Value::Int(-5));
        check_eval("-4.2", Value::Float(-4.2));
        check_eval("NOT false", Value::Bool(true));
    }

    #[test]
    fn eval_name_ref() {
        let (db, stmts) = lower("x = 5\nx");
        let mut interpreter = Interpreter::new();
        interpreter.exec_stmt(&stmts[0], &db).unwrap();
        let evaled = interpreter.exec_stmt(&stmts[1], &db).unwrap();
        assert_eq!(evaled, Value::Int(5));
    }

    #[test]
    fn eval_return_stmt() {
        check_eval("return 17", Value::Int(17));
    }

    #[test]
    fn eval_func_call() {
        let (db, stmts) = lower("function neg(x)\nreturn -x\nendfunction\nneg(3)");
        let mut interpreter = Interpreter::new();
        interpreter.exec_stmt(&stmts[0], &db).unwrap();
        let evaled = interpreter.exec_stmt(&stmts[1], &db).unwrap();
        assert_eq!(evaled, Value::Int(-3));
    }

    #[test]
    fn eval_arithmetic() {
        check_eval("1 + 1", Value::Int(2));
        check_eval("2 - 1", Value::Int(1));
        check_eval("5 * 3", Value::Int(15));
        check_eval("3 / 2", Value::Float(3.0 / 2.0));

        check_eval("1.3 + 2", Value::Float(3.3))
    }
}
