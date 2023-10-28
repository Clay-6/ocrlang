mod env;
mod eval;

use core::fmt;
use std::io::{self, BufRead};

use env::{Binding, Env, SubprogKind, Subprogram};
use eval::{eval_binary_op, eval_string_attrs, eval_unary_op};
use hir::{Database, ExprIdx, ExprRange, Stmt};
use smol_str::SmolStr;

pub type IResult<T> = Result<T, InterpretError>;

#[derive(Debug)]
pub struct Interpreter<I, O> {
    envs: (Env, Vec<Env>),
    output: O,
    input: io::BufReader<I>,
}

impl<I, O> Interpreter<I, O>
where
    O: io::Write,
    I: io::Read,
{
    pub fn new(input: I, output: O) -> Self {
        Self {
            envs: (Env::default(), Vec::new()),
            output,
            input: io::BufReader::new(input),
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

    fn exec_block(&mut self, block: &[Stmt], db: &Database) -> IResult<Value> {
        let mut res = Value::Unit;
        for stmt in block {
            res = self.exec_stmt(stmt, db)?;
        }
        Ok(res)
    }

    fn env(&self) -> &Env {
        self.envs.1.last().unwrap_or(&self.envs.0)
    }

    fn env_mut(&mut self) -> &mut Env {
        self.envs.1.last_mut().unwrap_or(&mut self.envs.0)
    }

    fn root_env(&mut self) -> &mut Env {
        &mut self.envs.0
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        self.envs
            .1
            .iter()
            .rev()
            .find_map(|e| e.get_var(name))
            .or_else(|| self.envs.0.get_var(name))
    }

    fn get_subprogram(&self, name: &str) -> Option<Subprogram> {
        self.envs
            .1
            .iter()
            .rev()
            .find_map(|e| e.get_subprogram(name))
            .or_else(|| self.envs.0.get_subprogram(name))
    }

    fn push_env(&mut self) {
        self.envs.1.push(Env::default());
    }

    fn pop_env(&mut self) {
        self.envs.1.pop();
    }

    fn exec_stmt(&mut self, stmt: &Stmt, db: &Database) -> IResult<Value> {
        match stmt {
            Stmt::Expr(e) => self.eval(e, db),
            Stmt::VarDef { name, kind, value } => self.exec_var_def(value, db, kind, name),
            Stmt::ArrayDef {
                name,
                kind,
                subscript,
                dimensions,
                value,
            } => self.exec_array_def(subscript, dimensions, value, db, *kind, name),
            Stmt::SubprogramDef {
                kind,
                name,
                params,
                body,
            } => Ok(self.exec_subprogram_def(*kind, name, params, body)),
            Stmt::ReturnStmt { value } => self.eval(value, db),
            Stmt::IfElse {
                condition,
                body,
                elseifs,
                else_body,
            } => self.exec_if_else(db, *condition, body, elseifs, else_body),
            Stmt::SwitchCase {
                scrutinee,
                cases,
                case_bodies,
                default_body,
            } => self.exec_switch_case(db, *scrutinee, cases, case_bodies, default_body),
            Stmt::ForLoop {
                loop_var,
                start,
                end,
                step,
                body,
            } => self.exec_for_loop(loop_var, db, *start, *end, *step, body),
            Stmt::WhileLoop { condition, body } => self.exec_while_loop(db, *condition, body),
            Stmt::DoUntilLoop { condition, body } => self.exec_do_until(db, *condition, body),
        }
    }

    fn exec_do_until(
        &mut self,
        db: &Database,
        condition: ExprIdx,
        body: &[Stmt],
    ) -> Result<Value, InterpretError> {
        let cond = db.get(condition);
        loop {
            self.execute(body, db)?;
            if self.eval(cond, db)? == Value::Bool(true) {
                break;
            }
        }
        Ok(Value::Unit)
    }

    fn exec_while_loop(
        &mut self,
        db: &Database,
        condition: ExprIdx,
        body: &[Stmt],
    ) -> Result<Value, InterpretError> {
        let cond = db.get(condition);
        while self.eval(cond, db)? == Value::Bool(true) {
            self.execute(body, db)?;
        }
        Ok(Value::Unit)
    }

    fn exec_for_loop(
        &mut self,
        loop_var: &Option<SmolStr>,
        db: &Database,
        start: ExprIdx,
        end: ExprIdx,
        step: ExprIdx,
        body: &[Stmt],
    ) -> Result<Value, InterpretError> {
        if loop_var.is_none() {
            return Err(InterpretError::ForLoopWithoutVariable);
        }
        let loop_var = loop_var.as_ref().unwrap();
        self.push_env();
        let start = self.eval(db.get(start), db)?;
        if !matches!(start, Value::Int(_)) {
            return Err(InterpretError::MismatchedTypes {
                expected: vec!["int"],
                found: start.type_str(),
            });
        }
        self.env_mut().insert(loop_var.clone(), Binding::Var(start));
        let end = self.eval(db.get(end), db)?;
        if !matches!(end, Value::Int(_)) {
            return Err(InterpretError::MismatchedTypes {
                expected: vec!["int"],
                found: end.type_str(),
            });
        }
        let step = {
            let e = db.get(step);
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
                .env()
                .get_var(loop_var)
                .expect("There's definitely a loop var by now");
            let Value::Int(i) = current_i else {
                unreachable!()
            };
            let Value::Int(fin) = end else { unreachable!() };
            if (step.is_positive() && i >= fin) || (step.is_negative() && i <= fin) {
                break;
            }

            self.execute(body, db)?;
            let Some(Value::Int(old)) = self.get_var(loop_var) else {
                unreachable!()
            };
            self.env_mut()
                .insert(loop_var.clone(), Binding::Var(Value::Int(old + step)));
        }
        self.pop_env();
        Ok(Value::Unit)
    }

    fn exec_switch_case(
        &mut self,
        db: &Database,
        scrutinee: ExprIdx,
        cases: &ExprRange,
        case_bodies: &[Vec<Stmt>],
        default_body: &[Stmt],
    ) -> Result<Value, InterpretError> {
        let scrutinee = self.eval(db.get(scrutinee), db)?;
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
        for (i, case) in cases.iter().enumerate() {
            if &scrutinee == case {
                return self.exec_block(&case_bodies[i], db);
            }
        }
        self.exec_block(default_body, db)
    }

    fn exec_if_else(
        &mut self,
        db: &Database,
        condition: ExprIdx,
        body: &[Stmt],
        elseifs: &[(ExprIdx, Vec<Stmt>)],
        else_body: &[Stmt],
    ) -> Result<Value, InterpretError> {
        if self.eval(db.get(condition), db)? == Value::Bool(true) {
            self.exec_block(body, db)
        } else {
            for (cond, body) in elseifs {
                if self.eval(db.get(*cond), db)? == Value::Bool(true) {
                    return self.exec_block(body, db);
                }
            }
            self.exec_block(else_body, db)
        }
    }

    fn exec_subprogram_def(
        &mut self,
        kind: hir::SubprogramKind,
        name: &SmolStr,
        params: &[SmolStr],
        body: &[Stmt],
    ) -> Value {
        let kind = match kind {
            hir::SubprogramKind::Function => SubprogKind::Function,
            hir::SubprogramKind::Procedure => SubprogKind::Procedure,
        };
        self.env_mut().insert(
            name.clone(),
            Binding::Func(Subprogram {
                kind,
                params: params.to_vec(),
                body: body.to_vec(),
            }),
        );

        Value::Unit
    }

    fn exec_array_def(
        &mut self,
        subscript: &(hir::Expr, hir::Expr),
        dimensions: &(hir::Expr, hir::Expr),
        value: &hir::Expr,
        db: &Database,
        kind: hir::VarDefKind,
        name: &SmolStr,
    ) -> Result<Value, InterpretError> {
        if let (hir::Expr::Missing, hir::Expr::Missing) = subscript {
            if let (hir::Expr::Missing, hir::Expr::Missing) = dimensions {
                if matches!(value, hir::Expr::Missing) {
                    return Err(InterpretError::InvalidArrayDeclaration);
                }
                let value = self.eval(value, db)?;
                if let Value::Array(arr) = value {
                    match kind {
                        hir::VarDefKind::Constant => self
                            .env_mut()
                            .insert_constant(name.clone(), Value::Array(arr))
                            .map_err(|()| InterpretError::ReassignedConstant)?,
                        hir::VarDefKind::Global => self
                            .root_env()
                            .insert(name.clone(), Binding::Var(Value::Array(arr))),
                        hir::VarDefKind::Standard => self
                            .env_mut()
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
                        self.env_mut()
                            .insert_constant(name.clone(), Value::Array(arr))
                            .map_err(|()| InterpretError::ReassignedConstant)?;
                    }
                    hir::VarDefKind::Global => self
                        .root_env()
                        .insert(name.clone(), Binding::Var(Value::Array(arr))),
                    hir::VarDefKind::Standard => self
                        .env_mut()
                        .insert(name.clone(), Binding::Var(Value::Array(arr))),
                }
                Ok(Value::Unit)
            } else {
                let mut arr = Vec::with_capacity(outer_len as usize);
                arr.resize(outer_len as usize, Value::Unit);
                match kind {
                    hir::VarDefKind::Constant => {
                        self.env_mut()
                            .insert_constant(name.clone(), Value::Array(arr))
                            .map_err(|()| InterpretError::ReassignedConstant)?;
                    }
                    hir::VarDefKind::Global => self
                        .root_env()
                        .insert(name.clone(), Binding::Var(Value::Array(arr))),
                    hir::VarDefKind::Standard => self
                        .env_mut()
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
            let Some(arr) = self.get_var(name) else {
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
            self.env_mut()
                .insert(name.clone(), Binding::Var(Value::Array(arr)));
            Ok(Value::Unit)
        }
    }

    fn exec_var_def(
        &mut self,
        value: &hir::Expr,
        db: &Database,
        kind: &hir::VarDefKind,
        name: &SmolStr,
    ) -> Result<Value, InterpretError> {
        let value = self.eval(value, db)?;
        match kind {
            hir::VarDefKind::Constant => self
                .env_mut()
                .insert_constant(name.clone(), value)
                .map_err(|()| InterpretError::ReassignedConstant)
                .map(|()| Value::Unit),
            hir::VarDefKind::Global => {
                self.root_env().insert(name.clone(), Binding::Var(value));
                Ok(Value::Unit)
            }
            hir::VarDefKind::Standard => {
                self.env_mut().insert(name.clone(), Binding::Var(value));
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
                let rhs = db.get(*rhs);
                if let hir::Expr::NameRef { name, .. } = rhs {
                    if let Some(value) = eval_string_attrs(*op, &lhs, name) {
                        return value;
                    }
                } else if let hir::Expr::Call { callee, args } = rhs {
                    if let Some(value) =
                        self.eval_string_methods(*op, &lhs, callee, args.clone(), db)
                    {
                        return value;
                    }
                }

                let rhs = self.eval(rhs, db)?;
                if !lhs.same_type(&rhs)
                    && !matches!(
                        (&lhs, &rhs),
                        (&Value::Int(_), &Value::Float(_))
                            | (&Value::Float(_), &Value::Int(_))
                            | (&Value::Array(_), Value::Int(_))
                    )
                {
                    return Err(InterpretError::MismatchedTypes {
                        expected: vec![lhs.type_str()],
                        found: rhs.type_str(),
                    });
                }

                eval_binary_op(op, lhs, rhs)
            }
            hir::Expr::Unary { op, opand } => eval_unary_op(self.eval(db.get(*opand), db)?, *op),
            hir::Expr::NameRef { name } => self
                .get_var(name)
                .ok_or_else(|| InterpretError::UnresolvedVariable { name: name.clone() }),
            hir::Expr::Call { callee, args } => {
                let Some(subprog) = self.get_subprogram(callee) else {
                    return self.builtin_subprog_call(callee, args, db);
                };

                let args = db
                    .get_range(args.clone())
                    .iter()
                    .map(|e| self.eval(e, db))
                    .collect::<IResult<Vec<_>>>()?;

                if args.len() != subprog.params.len() {
                    return Err(InterpretError::InvalidArgumentCount {
                        expected: subprog.params.len(),
                        got: args.len(),
                    });
                }

                self.push_env();
                for (arg, name) in args.iter().zip(subprog.params) {
                    self.env_mut().insert(name, Binding::Var(arg.clone()));
                }
                let result = self.call_subprog(&subprog.body, subprog.kind, db);
                self.pop_env();

                result
            }
            hir::Expr::Missing => Ok(Value::Unit),
        }
    }

    fn eval_string_methods(
        &mut self,
        op: hir::BinaryOp,
        lhs: &Value,
        callee: &str,
        args: ExprRange,
        db: &Database,
    ) -> Option<IResult<Value>> {
        if matches!(op, hir::BinaryOp::Dot) {
            let Value::String(lhs) = lhs else {
                return Some(Err(InterpretError::MismatchedTypes {
                    expected: vec!["string"],
                    found: lhs.type_str(),
                }));
            };

            let args = match db
                .get_range(args)
                .iter()
                .map(|e| self.eval(e, db))
                .collect::<IResult<Vec<_>>>()
            {
                Ok(args) => args,
                Err(e) => return Some(Err(e)),
            };
            if let Some(a) = args.iter().find(|a| !matches!(a, &&Value::Int(_))) {
                return Some(Err(InterpretError::MismatchedTypes {
                    expected: vec!["int"],
                    found: a.type_str(),
                }));
            }

            match callee {
                f @ ("left" | "right") => {
                    if args.len() != 1 {
                        return Some(Err(InterpretError::InvalidArgumentCount {
                            expected: 1,
                            got: args.len(),
                        }));
                    }
                    let Value::Int(n) = args[0] else {
                        unreachable!();
                    };
                    let Ok(n) = n.try_into() else {
                        return Some(Err(InterpretError::IntegerTooLarge));
                    };

                    if f == "left" {
                        return Some(Ok(Value::String(lhs.chars().take(n).collect())));
                    } else if f == "right" {
                        return Some(Ok(Value::String(lhs.chars().skip(lhs.len() - n).collect())));
                    }
                }
                "substring" => {
                    if args.len() != 2 {
                        return Some(Err(InterpretError::InvalidArgumentCount {
                            expected: 2,
                            got: args.len(),
                        }));
                    }
                    if let Some(a) = args.iter().find(|a| !matches!(a, Value::Int(_))) {
                        return Some(Err(InterpretError::MismatchedTypes {
                            expected: vec!["int"],
                            found: a.type_str(),
                        }));
                    }
                    let (Value::Int(skip), Value::Int(len)) = (&args[0], &args[1]) else {
                        unreachable!()
                    };
                    if *skip < 0 || *len < 0 {
                        return Some(Err(InterpretError::IllegalNegative));
                    }
                    let (Ok(skip), Ok(len)) = (usize::try_from(*skip), usize::try_from(*len))
                    else {
                        return Some(Err(InterpretError::IntegerTooLarge));
                    };

                    return Some(Ok(Value::String(
                        lhs.chars().skip(skip).take(len).collect(),
                    )));
                }
                _ => {
                    return Some(Err(InterpretError::InvalidDotTarget {
                        name: callee.into(),
                    }))
                }
            }
        }

        None
    }

    fn call_subprog(
        &mut self,
        body: &[Stmt],
        kind: SubprogKind,
        db: &Database,
    ) -> Result<Value, InterpretError> {
        match kind {
            SubprogKind::Function => self.exec_block(body, db),
            SubprogKind::Procedure => self.execute(body, db).map(|()| Value::Unit),
        }
    }

    fn builtin_subprog_call(
        &mut self,
        callee: &str,
        args: &ExprRange,
        db: &Database,
    ) -> Result<Value, InterpretError> {
        let args = db
            .get_range(args.clone())
            .iter()
            .map(|e| self.eval(e, db))
            .collect::<IResult<Vec<_>>>()?;
        if args.len() != 1 {
            return Err(InterpretError::InvalidArgumentCount {
                expected: 1,
                got: args.len(),
            });
        }

        match callee {
            "print" => {
                writeln!(self.output, "{}", args[0]).unwrap();
                Ok(Value::Unit)
            }
            "input" => {
                if let Value::String(ref prompt) = args[0] {
                    write!(self.output, "{prompt}").unwrap();
                    self.output.flush().unwrap();
                    let mut buf = String::new();
                    self.input.read_line(&mut buf).unwrap();
                    Ok(Value::String(buf.into()))
                } else {
                    Err(InterpretError::MismatchedTypes {
                        expected: vec!["string"],
                        found: args[0].type_str(),
                    })
                }
            }
            "ASC" => {
                let Value::Char(ref c) = args[0] else {
                    return Err(InterpretError::MismatchedTypes {
                        expected: vec!["char"],
                        found: args[0].type_str(),
                    });
                };
                Ok(Value::Int(u32::from(*c).into()))
            }
            "CHR" => {
                let Value::Int(ref n) = args[0] else {
                    return Err(InterpretError::MismatchedTypes {
                        expected: vec!["int"],
                        found: args[0].type_str(),
                    });
                };
                let Ok(n) = u8::try_from(*n) else {
                    return Err(InterpretError::IntegerTooLarge);
                };
                Ok(Value::Char(n.into()))
            }
            _ => Err(InterpretError::UnresolvedSubprogram {
                name: callee.into(),
            }),
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
                        write!(f, "{v}, ")?;
                    }
                    write!(f, "{}", arr[arr.len() - 1])?;
                }
                write!(f, "]")
            }
            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    IntegerTooLarge,
    InvalidDotTarget {
        name: SmolStr,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashMap;
    use std::io::{empty, BufReader};

    use hir::{Expr, Literal};
    use pretty_assertions::assert_eq;

    fn lower(src: &str) -> (Database, Vec<Stmt>) {
        hir::lower(ast::Root::cast(parser::parse(src).syntax()).unwrap())
    }

    fn check_eval(expr: &str, expected: Value) {
        let (db, stmts) = lower(expr);
        let evaled = Interpreter::new(std::io::empty(), vec![])
            .exec_stmt(&stmts[0], &db)
            .unwrap();
        assert_eq!(evaled, expected);
    }

    fn check_output(code: &str, expected: &str) {
        let mut output = Vec::new();
        let mut interpreter = Interpreter::new(empty(), &mut output);
        interpreter.run(code).unwrap();

        assert_eq!(std::str::from_utf8(&output).unwrap(), expected);
    }

    fn check_env(code: &str, expected: Env) {
        let mut interpreter = Interpreter::new(empty(), empty());
        interpreter.run(code).unwrap();

        assert_eq!(interpreter.env(), &expected);
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
        let mut interpreter = Interpreter::new(empty(), empty());
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
        let mut interpreter = Interpreter::new(empty(), empty());
        interpreter.exec_stmt(&stmts[0], &db).unwrap();
        let evaled = interpreter.exec_stmt(&stmts[1], &db).unwrap();
        assert_eq!(evaled, Value::Int(-3));
    }

    #[test]
    fn eval_builtin_ascii_conversions() {
        check_eval("CHR(97)", Value::Char('a'));
        check_eval("ASC('A')", Value::Int(65));
    }

    #[test]
    fn eval_arithmetic() {
        check_eval("1 + 1", Value::Int(2));
        check_eval("2 - 1", Value::Int(1));
        check_eval("5 * 3", Value::Int(15));
        check_eval("3 / 2", Value::Float(3.0 / 2.0));

        check_eval("1.3 + 2", Value::Float(3.3));
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
        check_eval("4.2 DIV 3.1", Value::Int(4.2_f64.div_euclid(3.1) as _));
    }

    #[test]
    fn eval_pow() {
        check_eval("2^5", Value::Int(32));
        check_eval("4.2^6.9", Value::Float(4.2_f64.powf(6.9)));
        check_eval("4.2^6", Value::Float(4.2_f64.powf(f64::from(6))));
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
            input: BufReader::new(empty()),
            output: empty(),
            envs: (env, Vec::default()),
        };

        let evaled = interpreter.exec_stmt(&stmts[0], &db).unwrap();
        assert_eq!(evaled, Value::Int(2));
    }

    #[test]
    fn eval_string_attributes() {
        check_eval("\"string\".length", Value::Int(6));
        check_eval("\"sTrInG\".upper", Value::String("STRING".into()));
        check_eval("\"StRiNg\".lower", Value::String("string".into()));
    }

    #[test]
    fn eval_string_methods() {
        check_eval(r#""ComputerScience".left(4)"#, Value::String("Comp".into()));
        check_eval(r#""ComputerScience".right(3)"#, Value::String("nce".into()));
        check_eval(
            r#""ComputerScience".substring(3, 5)"#,
            Value::String("puter".into()),
        );
    }

    #[test]
    fn invalid_dot_target_errors() {
        {
            let (db, stmts) = lower(r#""string".ballsack"#);
            let res = Interpreter::new(std::io::empty(), vec![]).exec_stmt(&stmts[0], &db);

            assert_eq!(
                res,
                Err(InterpretError::InvalidDotTarget {
                    name: "ballsack".into()
                })
            );
        };
    }

    #[test]
    fn eval_comparison() {
        check_eval("1 < 2", Value::Bool(true));
        check_eval("1 > 2", Value::Bool(false));
        check_eval("3 >= 3", Value::Bool(true));
        check_eval("3 <= 3", Value::Bool(true));
        check_eval("4 <= 3", Value::Bool(false));
    }

    #[test]
    fn exec_var_def() {
        check_output(
            r#"
            x = 69
            print(x)"#,
            "69\n",
        );
        check_output(
            r#"const C = "Const defined"
            print(C)"#,
            "Const defined\n",
        );
    }

    #[test]
    fn exec_array_def() {
        check_env(
            r#"array nums[5]"#,
            Env {
                bindings: HashMap::from([(
                    "nums".into(),
                    Binding::Var(Value::Array(vec![Value::Unit; 5])),
                )]),
            },
        );

        check_output(
            "
            array nums = [1, 2, 3, 4, 5]
            print(nums)",
            "[1, 2, 3, 4, 5]\n",
        );

        check_env(
            "array nums[5, 3]",
            Env {
                bindings: HashMap::from([(
                    "nums".into(),
                    Binding::Var(Value::Array(vec![Value::Array(vec![Value::Unit; 3]); 5])),
                )]),
            },
        );
    }

    #[test]
    fn exec_array_assign() {
        check_output(
            r#"
        array nums = [1, 2, 3]
        nums[2] = 5
        print(nums[2])"#,
            "5\n",
        );

        check_output(
            "
            array nums = [[1, 2], [3, 4], [5, 6]]
            nums[2, 1] = 69
            print(nums[2, 1])",
            "[5, 69]\n",
        );
    }

    #[test]
    fn exec_return_stmt() {
        check_output(
            r#"
            function f(x)
                return x
            endfunction
            
            print(f(5))"#,
            "5\n",
        );
    }

    #[test]
    fn exec_subprog_def() {
        check_env(
            r#"
            function f()
                return 0
            endfunction"#,
            Env {
                bindings: HashMap::from([(
                    "f".into(),
                    Binding::Func(Subprogram {
                        kind: SubprogKind::Function,
                        params: vec![],
                        body: vec![Stmt::ReturnStmt {
                            value: Expr::Literal {
                                value: Literal::Int(0),
                            },
                        }],
                    }),
                )]),
            },
        );
        check_env(
            r#"
            procedure p()
            endprocedure"#,
            Env {
                bindings: HashMap::from([(
                    "p".into(),
                    Binding::Func(Subprogram {
                        kind: SubprogKind::Procedure,
                        params: vec![],
                        body: vec![],
                    }),
                )]),
            },
        );
    }

    #[test]
    fn exec_simple_if_else() {
        check_output(
            r#"
        if true then
            print(true)
        else
            print(false)
        endif"#,
            "true\n",
        );
        check_output(
            r#"
        if false then
            print(true)
        else
            print(false)
        endif"#,
            "false\n",
        );
    }

    #[test]
    fn exec_multi_branch_if_else() {
        check_output(
            r#"
            if 5 < 3 then
                print("no")
            elseif 3 == 3 then
                print("Hooray!")
            elseif 5 == 5 then
                print("Shit")
            else
                print("NO")
            endif"#,
            "Hooray!\n",
        );
    }

    #[test]
    fn exec_switch_case() {
        check_output(
            r#"
            switch 'c':
                case 'c':
                    print("Yes")
                case 'd':
                    print("No")
                default:
                    print("Also no")
            endswitch"#,
            "Yes\n",
        );

        check_output(
            r#"
            switch 'd':
                case 'c':
                    print("No")
                case 'd':
                    print("Yes")
                default:
                    print("Also no")
            endswitch"#,
            "Yes\n",
        );

        check_output(
            r#"
            switch 'x':
                case 'c':
                    print("No")
                case 'd':
                    print("Also no")
                default:
                    print("Yes")
            endswitch"#,
            "Yes\n",
        );
    }

    #[test]
    fn exec_for_loop() {
        check_output(
            r#"
            for i = 0 to 10
                print("Ran")
            next i"#,
            &"Ran\n".repeat(10),
        );
    }

    #[test]
    fn exec_do_until() {
        check_output(
            r#"
            i = 1
            do
                print("Ran")
                i = i +1
            until i > 10"#,
            &"Ran\n".repeat(10),
        );
    }

    #[test]
    fn exec_while_loop() {
        check_output(
            r#"
            i = 1
            while i < 10
                print("Ran")
                i = i+1
            endwhile"#,
            &"Ran\n".repeat(9),
        );
    }
}
