mod env;
mod eval;

use core::fmt;

use env::{Binding, Env, SubprogKind, Subprogram};
use eval::*;
use hir::{Database, Stmt};
use smol_str::SmolStr;

pub type IResult<T> = Result<T, InterpretError>;

#[derive(Debug)]
pub struct Interpreter<O> {
    env: Env,
    root_env: Env,
    output: O,
}

impl<O> Interpreter<O>
where
    O: std::io::Write,
{
    pub fn new(output: O) -> Self {
        Self {
            root_env: Default::default(),
            env: Default::default(),
            output,
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
                    hir::VarDefKind::Global => {
                        self.root_env.insert(name.clone(), Binding::Var(value));
                        Ok(Value::Unit)
                    }
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
            } => {
                if let (hir::Expr::Missing, hir::Expr::Missing) = subscript {
                    if let (hir::Expr::Missing, hir::Expr::Missing) = dimensions {
                        if matches!(value, hir::Expr::Missing) {
                            return Err(InterpretError::InvalidArrayDeclaration);
                        }
                        let value = self.eval(value, db)?;
                        if let Value::Array(arr) = value {
                            match kind {
                                hir::VarDefKind::Constant => self
                                    .env
                                    .insert_constant(name.clone(), Value::Array(arr))
                                    .map_err(|_| InterpretError::ReassignedConstant)?,
                                hir::VarDefKind::Global => self
                                    .root_env
                                    .insert(name.clone(), Binding::Var(Value::Array(arr))),
                                hir::VarDefKind::Standard => self
                                    .env
                                    .insert(name.clone(), Binding::Var(Value::Array(arr))),
                            }
                            return Ok(Value::Unit);
                        }
                    }
                    let (i, j) = (self.eval(&dimensions.0, db)?, self.eval(&dimensions.1, db)?);
                    if !matches!(i, Value::Int(_) | Value::Unit) {
                        return Err(InterpretError::MismatchedTypes {
                            expected: vec!["int"],
                            found: i.type_str(),
                        });
                    }
                    if let Value::Int(i) = i {
                        if i < 0 {
                            return Err(InterpretError::IllegalNegative);
                        }
                    }
                    if let Value::Int(j) = j {
                        if j < 0 {
                            return Err(InterpretError::IllegalNegative);
                        }
                    }

                    let Value::Int(outer_len) = i else {
                        unreachable!()
                    };

                    if let Value::Int(inner_len) = j {
                        let mut arr = Vec::with_capacity(outer_len as usize);

                        let mut inner = Vec::with_capacity(inner_len as usize);
                        inner.resize(inner_len as usize, Value::Unit);
                        arr.resize(outer_len as usize, Value::Array(inner));
                        match kind {
                            hir::VarDefKind::Constant => {
                                self.env
                                    .insert_constant(name.clone(), Value::Array(arr))
                                    .map_err(|_| InterpretError::ReassignedConstant)?;
                            }
                            hir::VarDefKind::Global => self
                                .root_env
                                .insert(name.clone(), Binding::Var(Value::Array(arr))),
                            hir::VarDefKind::Standard => self
                                .env
                                .insert(name.clone(), Binding::Var(Value::Array(arr))),
                        }
                        Ok(Value::Unit)
                    } else {
                        let mut arr = Vec::with_capacity(outer_len as usize);
                        arr.resize(outer_len as usize, Value::Unit);
                        match kind {
                            hir::VarDefKind::Constant => {
                                self.env
                                    .insert_constant(name.clone(), Value::Array(arr))
                                    .map_err(|_| InterpretError::ReassignedConstant)?;
                            }
                            hir::VarDefKind::Global => self
                                .root_env
                                .insert(name.clone(), Binding::Var(Value::Array(arr))),
                            hir::VarDefKind::Standard => self
                                .env
                                .insert(name.clone(), Binding::Var(Value::Array(arr))),
                        }
                        Ok(Value::Unit)
                    }
                } else {
                    if !matches!(kind, hir::VarDefKind::Standard) {
                        return Err(InterpretError::DisallowedVariableQualifier);
                    }
                    let i1 = self.eval(&subscript.0, db)?;
                    if !matches!(i1, Value::Int(_)) {
                        return Err(InterpretError::MismatchedTypes {
                            expected: vec!["int"],
                            found: i1.type_str(),
                        });
                    }
                    let Value::Int(i1) = i1 else { unreachable!() };

                    let i2 = self.eval(&subscript.1, db)?;
                    let Some(arr) = self.env.get_var(name) else {
                        return Err(InterpretError::UnresolvedVariable { name: name.clone() });
                    };
                    let Value::Array(mut arr) = arr else {
                        return Err(InterpretError::MismatchedTypes {
                            expected: vec!["array"],
                            found: arr.type_str(),
                        });
                    };
                    let value = self.eval(value, db)?;
                    if matches!(i2, Value::Unit) {
                        arr[i1 as usize] = value;
                    } else {
                        let Value::Int(i2) = i2 else {
                            return Err(InterpretError::MismatchedTypes {
                                expected: vec!["int"],
                                found: i2.type_str(),
                            });
                        };
                        let Value::Array(subarr) = &mut arr[i1 as usize] else {
                            return Err(InterpretError::MismatchedTypes {
                                expected: vec!["array"],
                                found: arr[i1 as usize].type_str(),
                            });
                        };
                        subarr[i2 as usize] = value;
                    }
                    self.env
                        .insert(name.clone(), Binding::Var(Value::Array(arr)));
                    Ok(Value::Unit)
                }
            }
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
            } => {
                if self.eval(db.get(*condition), db)? == Value::Bool(true) {
                    self.execute(body, db).map(|_| Value::Unit)
                } else {
                    let mut ran_elseif = false;
                    for (cond, body) in elseifs {
                        if self.eval(db.get(*cond), db)? == Value::Bool(true) {
                            self.execute(body, db)?;
                            ran_elseif = true;
                            break;
                        }
                    }
                    if !ran_elseif {
                        self.execute(else_body, db)?;
                    }
                    Ok(Value::Unit)
                }
            }
            Stmt::SwitchCase {
                scrutinee,
                cases,
                case_bodies,
                default_body,
            } => {
                let scrutinee = self.eval(db.get(*scrutinee), db)?;
                let cases = db
                    .get_range(cases.clone())
                    .iter()
                    .map(|i| self.eval(i, db))
                    .collect::<IResult<Vec<_>>>()?;
                if let Some(case) = cases.iter().find(|c| !c.same_type(&scrutinee)) {
                    return Err(InterpretError::MismatchedTypes {
                        expected: vec![scrutinee.type_str()],
                        found: case.type_str(),
                    });
                }

                let mut ran_case = false;
                for (i, case) in cases.iter().enumerate() {
                    if &scrutinee == case {
                        self.execute(&case_bodies[i], db)?;
                        ran_case = true;
                    }
                }
                if !ran_case {
                    self.execute(default_body, db)?;
                }

                Ok(Value::Unit)
            }
            Stmt::ForLoop {
                loop_var,
                start,
                end,
                step,
                body,
            } => {
                if loop_var.is_none() {
                    return Err(InterpretError::ForLoopWithoutVariable);
                }
                let loop_var = loop_var.as_ref().unwrap();
                let prev_env = self.env.clone();
                let start = self.eval(db.get(*start), db)?;
                if !matches!(start, Value::Int(_)) {
                    return Err(InterpretError::MismatchedTypes {
                        expected: vec!["int"],
                        found: start.type_str(),
                    });
                }

                self.env.insert(loop_var.clone(), Binding::Var(start));
                let end = self.eval(db.get(*end), db)?;
                if !matches!(end, Value::Int(_)) {
                    return Err(InterpretError::MismatchedTypes {
                        expected: vec!["int"],
                        found: end.type_str(),
                    });
                }

                let step = {
                    let e = db.get(*step);
                    if let hir::Expr::Missing = e {
                        1
                    } else {
                        let val = self.eval(e, db)?;
                        if let Value::Int(v) = val {
                            v
                        } else {
                            return Err(InterpretError::MismatchedTypes {
                                expected: vec!["int"],
                                found: val.type_str(),
                            });
                        }
                    }
                };

                loop {
                    let current_i = self
                        .env
                        .get_var(loop_var)
                        .expect("There's definitely a loop var by now");
                    let Value::Int(i) = current_i else {
                        unreachable!()
                    };
                    let Value::Int(fin) = end else { unreachable!() };
                    if (step.is_positive() && i > fin) || (step.is_negative() && i < fin) {
                        break;
                    }

                    self.execute(body, db)?;
                    let Some(Value::Int(old)) = self.env.get_var(loop_var) else {
                        unreachable!()
                    };
                    self.env
                        .insert(loop_var.clone(), Binding::Var(Value::Int(old + step)))
                }

                self.env = prev_env;
                Ok(Value::Unit)
            }
            Stmt::WhileLoop { condition, body } => {
                let cond = db.get(*condition);
                while self.eval(cond, db)? == Value::Bool(true) {
                    self.execute(body, db)?;
                }
                Ok(Value::Unit)
            }
            Stmt::DoUntilLoop { condition, body } => {
                let cond = db.get(*condition);
                loop {
                    self.execute(body, db)?;
                    if self.eval(cond, db)? == Value::Bool(true) {
                        break;
                    }
                }
                Ok(Value::Unit)
            }
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

                    Value::Array(exprs)
                }
            }),
            hir::Expr::Binary { op, lhs, rhs } => {
                let lhs = self.eval(db.get(*lhs), db)?;
                if let hir::Expr::NameRef { name, .. } = db.get(*rhs) {
                    if let Some(value) = eval_string_attrs(op, &lhs, name) {
                        return value;
                    }
                }

                let rhs = self.eval(db.get(*rhs), db)?;
                if !lhs.same_type(&rhs)
                    && !matches!((&lhs, &rhs), (&Value::Int(_), &Value::Float(_)))
                    && !matches!((&lhs, &rhs), (&Value::Float(_), &Value::Int(_)))
                    && !matches!((&lhs, &rhs), (&Value::Array(_), Value::Int(_)))
                {
                    return Err(InterpretError::MismatchedTypes {
                        expected: vec![lhs.type_str()],
                        found: rhs.type_str(),
                    });
                }

                eval_binary_op(op, lhs, rhs)
            }
            hir::Expr::Unary { op, opand } => eval_unary_op(self.eval(db.get(*opand), db)?, op),
            hir::Expr::NameRef { name } => {
                let resolved = self.env.get_var(name);
                if let Some(v) = resolved {
                    Ok(v)
                } else if let Some(v) = self.root_env.get_var(name) {
                    Ok(v)
                } else {
                    Err(InterpretError::UnresolvedVariable { name: name.clone() })
                }
            }
            hir::Expr::Call { callee, args } => {
                let subprogram = self.env.get_subgrogram(callee);
                let args = db
                    .get_range(args.clone())
                    .iter()
                    .map(|e| self.eval(e, db))
                    .collect::<IResult<Vec<_>>>()?;
                if let Some(sp) = subprogram {
                    if args.len() != sp.params.len() {
                        return Err(InterpretError::InvalidArgumentCount {
                            expected: sp.params.len(),
                            got: args.len(),
                        });
                    }
                    let prev_env = self.env.clone();
                    for (param, arg) in sp.params.iter().zip(args) {
                        self.env.insert(param.clone(), Binding::Var(arg));
                    }
                    let res = self.call_subprog(sp.body, sp.kind, db);
                    self.env = prev_env;
                    res
                } else if callee == "print" {
                    if args.len() != 1 {
                        Err(InterpretError::InvalidArgumentCount {
                            expected: 1,
                            got: args.len(),
                        })
                    } else {
                        writeln!(self.output, "{}", args[0]).unwrap();
                        Ok(Value::Unit)
                    }
                } else {
                    Err(InterpretError::UnresolvedSubprogram {
                        name: callee.clone(),
                    })
                }
            }
            hir::Expr::Missing => Ok(Value::Unit),
        }
    }

    fn call_subprog(
        &mut self,
        body: Vec<Stmt>,
        kind: SubprogKind,
        db: &Database,
    ) -> Result<Value, InterpretError> {
        match kind {
            SubprogKind::Function => match body.len().cmp(&1) {
                std::cmp::Ordering::Equal => self.exec_stmt(&body[0], db),
                std::cmp::Ordering::Greater => {
                    self.execute(&body[..body.len() - 1], db)?;
                    self.exec_stmt(body.last().unwrap(), db)
                }
                std::cmp::Ordering::Less => Ok(Value::Unit),
            },
            SubprogKind::Procedure => self.execute(&body, db).map(|_| Value::Unit),
        }
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
            Value::Unit => true,
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => i.fmt(f),
            Value::Float(fl) => fl.fmt(f),
            Value::Char(c) => c.fmt(f),
            Value::String(s) => s.fmt(f),
            Value::Bool(b) => b.fmt(f),
            Value::Array(arr) => {
                write!(f, "[")?;
                if !arr.is_empty() {
                    for v in &arr[..arr.len() - 1] {
                        write!(f, "{}, ", v)?;
                    }
                    write!(f, "{}", arr[arr.len() - 1])?;
                }
                write!(f, "]")
            }
            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug)]
pub enum InterpretError {
    ReassignedConstant,
    MismatchedTypes {
        expected: Vec<&'static str>,
        found: &'static str,
    },
    UnresolvedVariable {
        name: SmolStr,
    },
    UnresolvedSubprogram {
        name: SmolStr,
    },
    InvalidArgumentCount {
        expected: usize,
        got: usize,
    },
    IndexOutOfRange,
    ForLoopWithoutVariable,
    DisallowedVariableQualifier,
    InvalidArrayDeclaration,
    IllegalNegative,
    IncorrectArrayLength {
        expected: usize,
        found: usize,
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
        let evaled = Interpreter::new(vec![]).exec_stmt(&stmts[0], &db).unwrap();
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
        let mut interpreter = Interpreter::new(vec![]);
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
        let mut interpreter = Interpreter::new(vec![]);
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

    #[test]
    fn eval_concat() {
        check_eval(
            "\"Hello\" + \" World\"",
            Value::String("Hello World".into()),
        );
    }

    #[test]
    fn eval_div_mod() {
        check_eval("5 MOD 3", Value::Int(2));
        check_eval("5 DIV 3", Value::Int(1));
        check_eval("3.1 MOD 4.2", Value::Int((3.1 % 4.2) as _));
        check_eval("4.2 DIV 3.1", Value::Int(4.2_f64.div_euclid(3.1) as _))
    }

    #[test]
    fn eval_pow() {
        check_eval("2^5", Value::Int(32));
        check_eval("4.2^6.9", Value::Float(4.2_f64.powf(6.9)));
        check_eval("4.2^6", Value::Float(4.2_f64.powf(6 as _)));
        check_eval("6^4.2", Value::Float(6_f64.powf(4.2)));
    }

    #[test]
    fn eval_and_or() {
        check_eval("true AND false", Value::Bool(false));
        check_eval("true OR false", Value::Bool(true));
    }

    #[test]
    fn eval_equality() {
        check_eval("5 == 5", Value::Bool(true));
        check_eval("3 != 3.1415926", Value::Bool(true));
        check_eval("\"string\" == \"string\"", Value::Bool(true));
    }

    #[test]
    fn eval_subscript() {
        let (db, stmts) = lower("arr[1]");
        let mut env = Env::default();
        env.insert(
            "arr".into(),
            Binding::Var(Value::Array(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
            ])),
        );
        let mut interpreter = Interpreter {
            output: vec![],
            env,
            root_env: Default::default(),
        };

        let evaled = interpreter.exec_stmt(&stmts[0], &db).unwrap();
        assert_eq!(evaled, Value::Int(2));
    }

    #[test]
    fn eval_basic_string_manip() {
        check_eval("\"string\".length", Value::Int(6));
        check_eval("\"sTrInG\".upper", Value::String("STRING".into()));
        check_eval("\"StRiNg\".lower", Value::String("string".into()));
    }

    #[test]
    fn eval_comparison() {
        check_eval("1 < 2", Value::Bool(true));
        check_eval("1 > 2", Value::Bool(false));
        check_eval("3 >= 3", Value::Bool(true));
        check_eval("4 <= 3", Value::Bool(false));
    }
}
