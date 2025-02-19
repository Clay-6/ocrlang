mod builtins;
mod env;
mod eval;

use core::fmt;
use std::{
    fs::File,
    io::{self, BufReader, Seek, SeekFrom, Write},
};

use env::{Binding, Env, Subprogram};
use eval::{eval_binary_op, eval_string_attrs, eval_unary_op};
use hir::{Database, ExprIdx, ExprRange, Stmt, StmtKind};
use smol_str::SmolStr;
use text_size::{TextRange, TextSize};

pub type InterpretResult<T> = Result<T, InterpretError>;

#[derive(Debug)]
pub struct Interpreter<I = io::Stdin, O = io::Stdout> {
    envs: (Env, Vec<Env>),
    output: O,
    input: BufReader<I>,
    call_depth: usize,
    db: Database,
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
            input: BufReader::new(input),
            call_depth: 0,
            db: Database::default(),
        }
    }

    pub fn run(
        &mut self,
        src: &str,
    ) -> Result<Value, (TextRange, InterpretError)> {
        let parse_tree = parser::parse(src).map_err(|e| {
            (
                TextRange::new(
                    TextSize::new(e.range.start as u32),
                    TextSize::new(e.range.end as u32),
                ),
                InterpretError::from(e),
            )
        })?;
        if parse_tree.errors().is_empty() {
            let  stmts = hir::lower_with(&mut self.db,&ast::Root::cast(parse_tree.syntax()).expect(
                "`parer::parse` tree contains a `Root` so conversion should always succeed",
            ));
            self.execute(&stmts)
        } else {
            Err((
                TextRange::new(
                    parse_tree.errors()[0]
                        .text_range()
                        .map(|r| r.start())
                        .unwrap_or_default(),
                    parse_tree
                        .errors()
                        .last()
                        .and_then(|e| e.text_range().map(|r| r.start()))
                        .unwrap_or_default(),
                ),
                InterpretError::ParseErrors {
                    errors: parse_tree.errors().to_owned(),
                },
            ))
        }
    }

    fn execute(
        &mut self,
        block: &[Stmt],
    ) -> Result<Value, (TextRange, InterpretError)> {
        let mut res = Value::Unit;
        for stmt in block {
            let prev_depth = self.call_depth;
            res = self.exec_stmt(stmt)?;
            if prev_depth > self.call_depth {
                break;
            }
        }
        Ok(res)
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

    fn exec_stmt(
        &mut self,
        stmt: &Stmt,
    ) -> Result<Value, (TextRange, InterpretError)> {
        match &stmt.kind {
            StmtKind::Expr(e) => self.eval(e),
            StmtKind::VarDef { name, kind, value } => {
                self.exec_var_def(value, *kind, name, stmt.range)
            }
            StmtKind::ArrayDef {
                name,
                kind,
                subscript,
                dimensions,
                value,
            } => self.exec_array_def(
                subscript, dimensions, value, *kind, name, stmt.range,
            ),
            StmtKind::SubprogramDef { name, params, body } => {
                Ok(self.exec_subprogram_def(name, params, body))
            }
            StmtKind::ReturnStmt { value } => {
                if self.call_depth > 0 {
                    let res = self.eval(value);
                    self.call_depth -= 1;
                    res
                } else {
                    Err((stmt.range, InterpretError::ReturnOutsideFunction))
                }
            }
            StmtKind::IfElse {
                condition,
                body,
                elseifs,
                else_body,
            } => self.exec_if_else(*condition, body, elseifs, else_body),
            StmtKind::SwitchCase {
                scrutinee,
                cases,
                case_bodies,
                default_body,
            } => self.exec_switch_case(
                *scrutinee,
                cases,
                case_bodies,
                default_body,
                stmt.range,
            ),
            StmtKind::ForLoop {
                loop_var,
                start,
                end,
                step,
                body,
            } => self.exec_for_loop(
                loop_var,
                (*start, *end, *step),
                body,
                stmt.range,
            ),
            StmtKind::WhileLoop { condition, body } => {
                self.exec_while_loop(*condition, body)
            }
            StmtKind::DoUntilLoop { condition, body } => {
                self.exec_do_until(*condition, body)
            }
        }
    }

    fn exec_do_until(
        &mut self,
        condition: ExprIdx,
        body: &[Stmt],
    ) -> Result<Value, (TextRange, InterpretError)> {
        let cond = self.db.get(condition).clone();
        loop {
            self.execute(body)?;
            if self.eval(&cond)? == Value::Bool(true) {
                break;
            }
        }
        Ok(Value::Unit)
    }

    fn exec_while_loop(
        &mut self,
        condition: ExprIdx,
        body: &[Stmt],
    ) -> Result<Value, (TextRange, InterpretError)> {
        let cond = self.db.get(condition).clone();
        while self.eval(&cond)? == Value::Bool(true) {
            self.execute(body)?;
        }
        Ok(Value::Unit)
    }

    fn exec_for_loop(
        &mut self,
        loop_var: &Option<SmolStr>,
        (start, end, step): (ExprIdx, ExprIdx, ExprIdx),
        body: &[Stmt],
        range: TextRange,
    ) -> Result<Value, (TextRange, InterpretError)> {
        let Some(loop_var) = loop_var.as_ref() else {
            return Err((range, InterpretError::ForLoopWithoutVariable));
        };

        let start = self.eval(&self.db.get(start).clone())?;
        let end = self.eval(&self.db.get(end).clone())?;
        let step = self.eval(&self.db.get(step).clone())?;

        let Value::Int(start) = start else {
            return Err((
                range,
                InterpretError::MismatchedTypes {
                    expected: vec!["int"],
                    found: start.type_str(),
                },
            ));
        };
        let Value::Int(end) = end else {
            return Err((
                range,
                InterpretError::MismatchedTypes {
                    expected: vec!["int"],
                    found: end.type_str(),
                },
            ));
        };
        let step = if let Value::Int(i) = step {
            i
        } else if let Value::Unit = step {
            1
        } else {
            return Err((
                range,
                InterpretError::MismatchedTypes {
                    expected: vec!["int"],
                    found: step.type_str(),
                },
            ));
        };

        self.push_env();
        self.env_mut()
            .insert(loop_var.clone(), Binding::Var(Value::Int(start)));
        let mut loop_idx = start;
        loop {
            if (step.is_positive() && loop_idx > end)
                || (step.is_negative() && loop_idx < end)
            {
                break;
            }
            self.execute(body)?;
            loop_idx += step;
            self.env_mut()
                .insert(loop_var.clone(), Binding::Var(Value::Int(loop_idx)));
        }
        self.pop_env();

        Ok(Value::Unit)
    }

    fn exec_switch_case(
        &mut self,
        scrutinee: ExprIdx,
        cases: &ExprRange,
        case_bodies: &[Vec<Stmt>],
        default_body: &[Stmt],
        range: TextRange,
    ) -> Result<Value, (TextRange, InterpretError)> {
        let scrutinee = self.eval(&self.db.get(scrutinee).clone())?;
        let cases = self
            .db
            .get_range(cases.clone())
            .to_owned()
            .iter()
            .map(|i| self.eval(i))
            .collect::<Result<Vec<_>, (TextRange, InterpretError)>>()?;
        if let Some(case) = cases.iter().find(|c| !c.same_type(&scrutinee)) {
            return Err((
                range,
                InterpretError::MismatchedTypes {
                    expected: vec![scrutinee.type_str()],
                    found: case.type_str(),
                },
            ));
        }
        for (i, case) in cases.iter().enumerate() {
            if &scrutinee == case {
                return self.execute(&case_bodies[i]);
            }
        }
        self.execute(default_body)
    }

    fn exec_if_else(
        &mut self,
        condition: ExprIdx,
        body: &[Stmt],
        elseifs: &[(ExprIdx, Vec<Stmt>)],
        else_body: &[Stmt],
    ) -> Result<Value, (TextRange, InterpretError)> {
        if self.eval(&self.db.get(condition).clone())? == Value::Bool(true) {
            self.execute(body)
        } else {
            for (cond, body) in elseifs {
                if self.eval(&self.db.get(*cond).clone())? == Value::Bool(true)
                {
                    return self.execute(body);
                }
            }
            self.execute(else_body)
        }
    }

    fn exec_subprogram_def(
        &mut self,
        name: &SmolStr,
        params: &[SmolStr],
        body: &[Stmt],
    ) -> Value {
        self.env_mut().insert(
            name.clone(),
            Binding::Func(Subprogram {
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
        kind: hir::VarDefKind,
        name: &SmolStr,
        range: TextRange,
    ) -> Result<Value, (TextRange, InterpretError)> {
        if matches!(
            (&subscript.0.kind, &subscript.1.kind),
            (&hir::ExprKind::Missing, &hir::ExprKind::Missing)
        ) {
            if matches!(
                (&dimensions.0.kind, &dimensions.1.kind),
                (&hir::ExprKind::Missing, &hir::ExprKind::Missing)
            ) {
                self.array_define(value, kind, name, range)
            } else {
                self.array_decl(dimensions, kind, name, range)
            }
        } else {
            self.array_subscript_assign(kind, subscript, name, value, range)
        }
    }

    fn array_define(
        &mut self,
        expr: &hir::Expr,
        kind: hir::VarDefKind,
        name: &SmolStr,
        range: TextRange,
    ) -> Result<Value, (TextRange, InterpretError)> {
        if matches!(expr.kind, hir::ExprKind::Missing) {
            return Err((range, InterpretError::InvalidArrayDeclaration));
        }
        let value = self.eval(expr)?;
        let Value::Array(arr) = value else {
            return Err((
                expr.range,
                InterpretError::MismatchedTypes {
                    expected: vec!["array"],
                    found: value.type_str(),
                },
            ));
        };
        match kind {
            hir::VarDefKind::Constant => self
                .env_mut()
                .insert_constant(name.clone(), Value::Array(arr))
                .map_err(|()| {
                    (expr.range, InterpretError::ReassignedConstant)
                })?,
            hir::VarDefKind::Global => self
                .root_env()
                .insert(name.clone(), Binding::Var(Value::Array(arr))),
            hir::VarDefKind::Standard => self
                .env_mut()
                .insert(name.clone(), Binding::Var(Value::Array(arr))),
        }
        Ok(Value::Unit)
    }

    fn array_decl(
        &mut self,
        dimensions: &(hir::Expr, hir::Expr),
        kind: hir::VarDefKind,
        name: &SmolStr,
        range: TextRange,
    ) -> Result<Value, (TextRange, InterpretError)> {
        let (i, j) = (self.eval(&dimensions.0)?, self.eval(&dimensions.1)?);
        if !matches!(i, Value::Int(_) | Value::Unit) {
            return Err((
                range,
                InterpretError::MismatchedTypes {
                    expected: vec!["int"],
                    found: i.type_str(),
                },
            ));
        }
        if let Value::Int(i) = i {
            if i < 0 {
                return Err((range, InterpretError::IllegalNegative));
            }
        }
        if let Value::Int(j) = j {
            if j < 0 {
                return Err((range, InterpretError::IllegalNegative));
            }
        }

        let Value::Int(outer_len) = i else {
            unreachable!()
        };

        if let Value::Int(inner_len) = j {
            let mut arr = Vec::with_capacity(
                outer_len
                    .try_into()
                    .map_err(|_| (range, InterpretError::IntegerTooLarge))?,
            );

            let mut inner = Vec::with_capacity(
                inner_len
                    .try_into()
                    .map_err(|_| (range, InterpretError::IntegerTooLarge))?,
            );
            inner.resize(
                inner_len
                    .try_into()
                    .map_err(|_| (range, InterpretError::IntegerTooLarge))?,
                Value::Unit,
            );
            arr.resize(
                outer_len
                    .try_into()
                    .map_err(|_| (range, InterpretError::IntegerTooLarge))?,
                Value::Array(inner),
            );
            match kind {
                hir::VarDefKind::Constant => {
                    self.env_mut()
                        .insert_constant(name.clone(), Value::Array(arr))
                        .map_err(|()| {
                            (range, InterpretError::ReassignedConstant)
                        })?;
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
            let mut arr = Vec::with_capacity(
                outer_len
                    .try_into()
                    .map_err(|_| (range, InterpretError::IntegerTooLarge))?,
            );
            arr.resize(
                outer_len
                    .try_into()
                    .map_err(|_| (range, InterpretError::IntegerTooLarge))?,
                Value::Unit,
            );
            match kind {
                hir::VarDefKind::Constant => {
                    self.env_mut()
                        .insert_constant(name.clone(), Value::Array(arr))
                        .map_err(|()| {
                            (range, InterpretError::ReassignedConstant)
                        })?;
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
    }

    fn array_subscript_assign(
        &mut self,
        kind: hir::VarDefKind,
        subscript: &(hir::Expr, hir::Expr),
        name: &SmolStr,
        value: &hir::Expr,
        range: TextRange,
    ) -> Result<Value, (TextRange, InterpretError)> {
        if !matches!(kind, hir::VarDefKind::Standard) {
            return Err((range, InterpretError::DisallowedVariableQualifier));
        }
        let i1 = self.eval(&subscript.0)?;
        if !matches!(i1, Value::Int(_)) {
            return Err((
                range,
                InterpretError::MismatchedTypes {
                    expected: vec!["int"],
                    found: i1.type_str(),
                },
            ));
        }
        let Value::Int(i1) = i1 else { unreachable!() };

        let i2 = self.eval(&subscript.1)?;
        let Some(arr) = self.get_var(name) else {
            return Err((
                range,
                InterpretError::UnresolvedVariable { name: name.clone() },
            ));
        };
        let Value::Array(mut arr) = arr else {
            return Err((
                range,
                InterpretError::MismatchedTypes {
                    expected: vec!["array"],
                    found: arr.type_str(),
                },
            ));
        };
        let value = self.eval(value)?;
        if matches!(i2, Value::Unit) {
            arr[usize::try_from(i1)
                .map_err(|_| (range, InterpretError::IntegerTooLarge))?] =
                value;
        } else {
            let Value::Int(i2) = i2 else {
                return Err((
                    range,
                    InterpretError::MismatchedTypes {
                        expected: vec!["int"],
                        found: i2.type_str(),
                    },
                ));
            };
            let Value::Array(subarr) = &mut arr[usize::try_from(i1)
                .map_err(|_| (range, InterpretError::IntegerTooLarge))?]
            else {
                return Err((
                    range,
                    InterpretError::MismatchedTypes {
                        expected: vec!["array"],
                        found: arr[usize::try_from(i1).map_err(|_| {
                            (range, InterpretError::IntegerTooLarge)
                        })?]
                        .type_str(),
                    },
                ));
            };
            subarr[usize::try_from(i2)
                .map_err(|_| (range, InterpretError::IntegerTooLarge))?] =
                value;
        }
        self.env_mut()
            .insert(name.clone(), Binding::Var(Value::Array(arr)));
        Ok(Value::Unit)
    }

    fn exec_var_def(
        &mut self,
        value: &hir::Expr,
        kind: hir::VarDefKind,
        name: &SmolStr,
        range: TextRange,
    ) -> Result<Value, (TextRange, InterpretError)> {
        let value = self.eval(value)?;
        match kind {
            hir::VarDefKind::Constant => self
                .env_mut()
                .insert_constant(name.clone(), value)
                .map_err(|()| (range, InterpretError::ReassignedConstant))
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

    fn eval(
        &mut self,
        expr: &hir::Expr,
    ) -> Result<Value, (TextRange, InterpretError)> {
        match &expr.kind {
            hir::ExprKind::Literal { value } => Ok(match value {
                hir::Literal::Int(i) => Value::Int(*i),
                hir::Literal::Float(f) => Value::Float(*f),
                hir::Literal::Char(c) => Value::Char(*c),
                hir::Literal::String(s) => Value::String(s.clone()),
                hir::Literal::Bool(b) => Value::Bool(*b),
                hir::Literal::Array(range) => {
                    let exprs = self
                        .db
                        .get_range(range.clone())
                        .to_owned()
                        .iter()
                        .map(|e| self.eval(e))
                        .collect::<Result<Vec<_>, (TextRange, InterpretError)>>(
                        )?;

                    Value::Array(exprs)
                }
            }),
            hir::ExprKind::Binary {
                op,
                lhs: lhs_idx,
                rhs: rhs_idx,
            } => {
                let mut lhs = self.eval(&self.db.get(*lhs_idx).clone())?;
                let rhs = &self.db.get(*rhs_idx).clone();
                if let hir::ExprKind::NameRef { name, .. } = &rhs.kind {
                    if let Some(value) = eval_string_attrs(*op, &lhs, name) {
                        return value
                            .map_err(|e| (self.db.get(*lhs_idx).range, e));
                    }
                } else if let hir::ExprKind::Call { callee, args } = &rhs.kind {
                    if let Some(value) =
                        self.eval_method(*op, &mut lhs, callee, args.clone())
                    {
                        return value
                            .map_err(|e| (self.db.get(*lhs_idx).range, e));
                    }
                }
                let rhs = self.eval(rhs)?;
                if !lhs.same_type(&rhs)
                    && !matches!(
                        (&lhs, &rhs),
                        (&Value::Int(_), &Value::Float(_))
                            | (&Value::Float(_), &Value::Int(_))
                            | (&Value::Array(_), Value::Int(_))
                    )
                {
                    return Err((
                        self.db.get(*rhs_idx).range,
                        InterpretError::MismatchedTypes {
                            expected: vec![lhs.type_str()],
                            found: rhs.type_str(),
                        },
                    ));
                }

                eval_binary_op(*op, &lhs, &rhs)
                    .map_err(|e| (self.db.get(*lhs_idx).range, e))
            }
            hir::ExprKind::Unary { op, opand } => {
                eval_unary_op(&self.eval(&self.db.get(*opand).clone())?, *op)
                    .map_err(|e| (self.db.get(*opand).range, e))
            }
            hir::ExprKind::NameRef { name } => {
                self.get_var(name).ok_or_else(|| {
                    (
                        expr.range,
                        InterpretError::UnresolvedVariable {
                            name: name.clone(),
                        },
                    )
                })
            }
            hir::ExprKind::Call { callee, args } => {
                let Some(subprog) = self.get_subprogram(callee) else {
                    return self
                        .builtin_subprog_call(callee, args)
                        .map_err(|e| (expr.range, e));
                };

                let args = self
                    .db
                    .get_range(args.clone())
                    .to_owned()
                    .iter()
                    .map(|e| self.eval(e))
                    .collect::<Result<Vec<_>, (TextRange, InterpretError)>>()?;

                if args.len() != subprog.params.len() {
                    return Err((
                        expr.range,
                        InterpretError::InvalidArgumentCount {
                            expected: subprog.params.len(),
                            got: args.len(),
                        },
                    ));
                }

                self.push_env();
                for (arg, name) in args.iter().zip(subprog.params) {
                    self.env_mut().insert(name, Binding::Var(arg.clone()));
                }
                self.call_depth += 1;
                let result = self.call_subprog(&subprog.body);
                self.pop_env();

                result
            }
            hir::ExprKind::Missing => Ok(Value::Unit),
        }
    }

    fn eval_string_methods(
        &mut self,
        op: hir::BinaryOp,
        lhs: &Value,
        callee: &str,
        args: ExprRange,
    ) -> Option<InterpretResult<Value>> {
        if matches!(op, hir::BinaryOp::Dot) {
            let Value::String(lhs) = lhs else {
                return Some(Err(InterpretError::MismatchedTypes {
                    expected: vec!["string"],
                    found: lhs.type_str(),
                }));
            };

            let args = match self
                .db
                .get_range(args)
                .to_owned()
                .iter()
                .map(|e| self.eval(e))
                .collect::<Result<Vec<_>, (TextRange, InterpretError)>>()
            {
                Ok(args) => args,
                Err(e) => return Some(Err(e.1)),
            };
            if let Some(a) = args.iter().find(|a| !matches!(a, &&Value::Int(_)))
            {
                return Some(Err(InterpretError::MismatchedTypes {
                    expected: vec!["int"],
                    found: a.type_str(),
                }));
            }

            return match callee {
                f @ ("left" | "right") => {
                    if args.len() != 1 {
                        return Some(Err(
                            InterpretError::InvalidArgumentCount {
                                expected: 1,
                                got: args.len(),
                            },
                        ));
                    }
                    let Value::Int(n) = args[0] else {
                        unreachable!();
                    };
                    let Ok(n) = n.try_into() else {
                        return Some(Err(InterpretError::IntegerTooLarge));
                    };

                    if f == "left" {
                        Some(Ok(Value::String(lhs.chars().take(n).collect())))
                    } else {
                        Some(Ok(Value::String(
                            lhs.chars().skip(lhs.len() - n).collect(),
                        )))
                    }
                }
                "substring" => {
                    if args.len() != 2 {
                        return Some(Err(
                            InterpretError::InvalidArgumentCount {
                                expected: 2,
                                got: args.len(),
                            },
                        ));
                    }
                    if let Some(a) =
                        args.iter().find(|a| !matches!(a, Value::Int(_)))
                    {
                        return Some(Err(InterpretError::MismatchedTypes {
                            expected: vec!["int"],
                            found: a.type_str(),
                        }));
                    }
                    let (Value::Int(skip), Value::Int(len)) =
                        (&args[0], &args[1])
                    else {
                        unreachable!()
                    };
                    if *skip < 0 || *len < 0 {
                        return Some(Err(InterpretError::IllegalNegative));
                    }
                    let (Ok(skip), Ok(len)) =
                        (usize::try_from(*skip), usize::try_from(*len))
                    else {
                        return Some(Err(InterpretError::IntegerTooLarge));
                    };

                    Some(Ok(Value::String(
                        lhs.chars().skip(skip).take(len).collect(),
                    )))
                }
                _ => Some(Err(InterpretError::InvalidDotTarget {
                    name: callee.into(),
                })),
            };
        }

        None
    }

    fn eval_file_methods(
        &mut self,
        op: hir::BinaryOp,
        lhs: &mut Value,
        callee: &str,
        args: ExprRange,
    ) -> Option<InterpretResult<Value>> {
        use io::Read;

        if !matches!(op, hir::BinaryOp::Dot) {
            return None;
        }

        let Value::File(FileInner(file)) = lhs else {
            return Some(Err(InterpretError::MismatchedTypes {
                expected: vec!["file"],
                found: lhs.type_str(),
            }));
        };
        let args = match self
            .db
            .get_range(args)
            .to_owned()
            .iter()
            .map(|a| self.eval(a).map_err(|e| e.1))
            .collect::<InterpretResult<Vec<_>>>()
        {
            Ok(args) => args,
            Err(e) => return Some(Err(e)),
        };
        if !args.is_empty() && callee != "writeLine" {
            return Some(Err(InterpretError::InvalidArgumentCount {
                expected: 0,
                got: args.len(),
            }));
        }

        Some(match callee {
            "close" => {
                // Reassign to drop the `File` & close it in its `Drop` impl
                *lhs = Value::Unit;
                Ok(Value::Unit)
            }
            "readLine" => {
                let mut line = Vec::new();
                let mut buf = [0];
                while let Ok(()) = file.read_exact(&mut buf) {
                    line.push(buf[0]);
                    if line.ends_with(b"\n") {
                        break;
                    }
                    buf = [0];
                }
                Ok(Value::String(String::from_utf8_lossy(&line).into()))
            }
            "endOfFile" => {
                let mut buf = [0];
                let res = file.read_exact(&mut buf);

                match file.stream_position() {
                    Ok(pos) => {
                        if pos > 0 {
                            if let Err(e) = file.seek(SeekFrom::Current(-1)) {
                                return Some(Err(e.into()));
                            }
                        }
                    }
                    Err(e) => return Some(Err(e.into())),
                }
                Ok(match res {
                    Ok(()) => Value::Bool(false),
                    Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => {
                        Value::Bool(true)
                    }
                    Err(e) => return Some(Err(e.into())),
                })
            }
            "writeLine" => {
                if args.len() != 1 {
                    return Some(Err(InterpretError::InvalidArgumentCount {
                        expected: 1,
                        got: args.len(),
                    }));
                }
                let Value::String(ref line) = args[0] else {
                    return Some(Err(InterpretError::MismatchedTypes {
                        expected: vec!["string"],
                        found: args[0].type_str(),
                    }));
                };
                if let Err(e) = writeln!(file, "{line}") {
                    return Some(Err(e.into()));
                };

                Ok(Value::Unit)
            }
            _ => Err(InterpretError::InvalidDotTarget {
                name: callee.into(),
            }),
        })
    }

    fn call_subprog(
        &mut self,
        body: &[Stmt],
    ) -> Result<Value, (TextRange, InterpretError)> {
        self.execute(body)
    }

    fn builtin_subprog_call(
        &mut self,
        callee: &str,
        args: &ExprRange,
    ) -> InterpretResult<Value> {
        let args = self
            .db
            .get_range(args.clone())
            .to_owned()
            .iter()
            .map(|e| self.eval(e).map_err(|e| e.1))
            .collect::<InterpretResult<Vec<_>>>()?;
        if callee == "random" {
            return if args.len() == 2 {
                builtins::random(&args[0], &args[1])
            } else {
                Err(InterpretError::InvalidArgumentCount {
                    expected: 2,
                    got: args.len(),
                })
            };
        }

        if args.len() != 1 {
            return Err(InterpretError::InvalidArgumentCount {
                expected: 1,
                got: args.len(),
            });
        }

        match callee {
            "print" => builtins::print(self, &args[0]),
            "input" => builtins::input(self, &args[0]),
            "ASC" => builtins::asc(&args[0]),
            "CHR" => builtins::chr(&args[0]),
            "str" => Ok(builtins::str_cast(&args[0])),
            "int" => builtins::int_cast(&args[0]),
            "float" | "real" => builtins::float_cast(&args[0]),
            "bool" => builtins::bool_cast(&args[0]),
            "open" => builtins::open(&args[0]),
            "newFile" => builtins::new_file(&args[0]),
            _ => Err(InterpretError::UnresolvedSubprogram {
                name: callee.into(),
            }),
        }
    }

    fn eval_method(
        &mut self,
        op: hir::BinaryOp,
        lhs: &mut Value,
        callee: &str,
        args: ExprRange,
    ) -> Option<Result<Value, InterpretError>> {
        match lhs {
            Value::String(_) => self.eval_string_methods(op, lhs, callee, args),
            Value::File(_) => self.eval_file_methods(op, lhs, callee, args),
            _ => None,
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            envs: Default::default(),
            output: io::stdout(),
            input: BufReader::new(io::stdin()),
            call_depth: 0,
            db: Database::default(),
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
    File(FileInner),
    Unit,
}

#[derive(Debug)]
pub struct FileInner(File);

impl From<File> for FileInner {
    fn from(value: File) -> Self {
        Self(value)
    }
}

impl PartialEq for FileInner {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Clone for FileInner {
    fn clone(&self) -> Self {
        Self(self.0.try_clone().unwrap())
    }
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
            Value::File(_) => matches!(b, Value::File(_)),
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
            Value::File(_) => "file",
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
            Value::File(_) => write!(f, "[file]"),
            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InterpretError {
    LexError {
        text: SmolStr,
        // range: Range<usize>,
    },
    ParseErrors {
        errors: Vec<parser::ParseError>,
    },
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
    ReturnOutsideFunction,
    CastFailure {
        value: Value,
        target: &'static str,
    },
    InvalidCast {
        from: &'static str,
        to: &'static str,
    },
    IoError(#[from] std::io::Error),
}

impl fmt::Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            InterpretError::ParseErrors { errors } => {
                errors.iter().try_fold(String::new(), |mut output, e| {
                    use std::fmt::Write;
                    writeln!(output, "{e}").map(|()| output)
                })?
            }
            InterpretError::ReassignedConstant => "reassigned constant".into(),
            InterpretError::MismatchedTypes { expected, found } => {
                format!("mismatched types. Expected one of {expected:?}, but found {found}")
            }
            InterpretError::UnresolvedVariable { name } => {
                format!("unresolved variable '{name}'")
            }
            InterpretError::UnresolvedSubprogram { name } => {
                format!("unresolved subprogram '{name}'")
            }
            InterpretError::InvalidArgumentCount { expected, got } => {
                format!("invalid argument count. Expected {expected}, but found {got}")
            }
            InterpretError::IndexOutOfRange => "index out of range".to_string(),
            InterpretError::ForLoopWithoutVariable => {
                "for loop without variable".to_string()
            }
            InterpretError::DisallowedVariableQualifier => {
                "disallowed variable qualifier".into()
            }
            InterpretError::InvalidArrayDeclaration => {
                "invalid array declaration".to_string()
            }
            InterpretError::IllegalNegative => {
                "negative value not allowed".to_string()
            }
            InterpretError::IncorrectArrayLength { expected, found } => {
                format!("incorrect array length; expected {expected}, but found {found}")
            }
            InterpretError::IntegerTooLarge => "integer too large".to_string(),
            InterpretError::InvalidDotTarget { name } => {
                format!("invalid dot expression target '{name}'")
            }
            InterpretError::ReturnOutsideFunction => {
                "return statement outside of function".to_string()
            }
            InterpretError::LexError { text } => {
                format!("unrecognised token '{text}'")
            }
            InterpretError::InvalidCast { from, to } => {
                format!("casting {from} to {to} is invalid")
            }
            InterpretError::CastFailure { value, target } => {
                format!("failed to cast {value} to type {target}")
            }
            InterpretError::IoError(e) => e.to_string(),
        };

        write!(f, "{s}")
    }
}

impl From<parser::LexError<'_>> for InterpretError {
    fn from(err: parser::LexError) -> Self {
        Self::LexError {
            text: err.text.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashMap;
    use std::fs;
    use std::io::{empty, BufReader};

    use hir::{Expr, ExprKind, Literal, StmtKind};
    use pretty_assertions::assert_eq;
    use text_size::{TextRange, TextSize};

    fn lower(src: &str) -> (Database, Vec<Stmt>) {
        hir::lower(
            &ast::Root::cast(parser::parse(src).unwrap().syntax()).unwrap(),
        )
    }

    fn check_eval(expr: &str, expected: Value) {
        let (db, stmts) = lower(expr);
        let evaled = Interpreter {
            input: BufReader::new(std::io::empty()),
            output: vec![],
            db,
            envs: Default::default(),
            call_depth: 0,
        }
        .exec_stmt(&stmts[0])
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

        assert_eq!(&interpreter.envs.0, &expected);
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
        let mut interpreter = Interpreter {
            input: BufReader::new(empty()),
            output: empty(),
            db,
            envs: Default::default(),
            call_depth: 0,
        };
        interpreter.exec_stmt(&stmts[0]).unwrap();
        let evaled = interpreter.exec_stmt(&stmts[1]).unwrap();
        assert_eq!(evaled, Value::Int(5));
    }

    #[test]
    fn eval_return_stmt() {
        {
            let expected = Value::Int(17);
            let (db, stmts) = lower("return 17");
            let evaled = Interpreter {
                input: BufReader::new(std::io::empty()),
                output: vec![],
                envs: (Env::default(), Vec::default()),
                call_depth: 1, // Pretend we're actually inside a function
                db,
            }
            .exec_stmt(&stmts[0])
            .unwrap();
            assert_eq!(evaled, expected);
        };
    }

    #[test]
    fn eval_func_call() {
        let (db, stmts) =
            lower("function neg(x)\nreturn -x\nendfunction\nneg(3)");
        let mut interpreter = Interpreter {
            db,
            envs: Default::default(),
            input: BufReader::new(empty()),
            output: empty(),
            call_depth: 0,
        };
        interpreter.exec_stmt(&stmts[0]).unwrap();
        let evaled = interpreter.exec_stmt(&stmts[1]).unwrap();
        assert_eq!(evaled, Value::Int(-3));
    }

    #[test]
    fn eval_builtin_ascii_conversions() {
        check_eval("CHR(97)", Value::Char('a'));
        check_eval("ASC('A')", Value::Int(65));
    }

    #[test]
    fn eval_type_casts() {
        check_eval("str(345)", Value::String("345".into()));
        check_eval("int(\"3\")", Value::Int(3));
        check_eval("float(\"4.52\")", Value::Float(4.52));
        check_eval("real(\"4.52\")", Value::Float(4.52));
        check_eval("bool(\"True\")", Value::Bool(true));
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
            call_depth: 0,
            db,
        };

        let evaled = interpreter.exec_stmt(&stmts[0]).unwrap();
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
        check_eval(
            r#""ComputerScience".left(4)"#,
            Value::String("Comp".into()),
        );
        check_eval(
            r#""ComputerScience".right(3)"#,
            Value::String("nce".into()),
        );
        check_eval(
            r#""ComputerScience".substring(3, 5)"#,
            Value::String("puter".into()),
        );
    }

    #[test]
    fn invalid_dot_target_errors() {
        {
            let (db, stmts) = lower(r#""string".ballsack"#);
            let res = Interpreter {
                input: BufReader::new(std::io::empty()),
                output: vec![],
                db,
                envs: Default::default(),
                call_depth: 0,
            }
            .exec_stmt(&stmts[0]);

            let err = res.unwrap_err();
            let InterpretError::InvalidDotTarget { name } = err.1 else {
                panic!("Wrong error type")
            };
            assert_eq!(name, "ballsack");
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
                    Binding::Var(Value::Array(vec![
                        Value::Array(
                            vec![Value::Unit; 3]
                        );
                        5
                    ])),
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
                        params: vec![],
                        body: vec![Stmt {
                            range: TextRange::new(
                                TextSize::new(42),
                                TextSize::new(63),
                            ),
                            kind: StmtKind::ReturnStmt {
                                value: Expr {
                                    kind: ExprKind::Literal {
                                        value: Literal::Int(0),
                                    },
                                    range: TextRange::new(
                                        TextSize::new(49),
                                        TextSize::new(63),
                                    ),
                                },
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
                        params: vec![],
                        body: vec![Stmt {
                            range: TextRange::default(),
                            kind: StmtKind::ReturnStmt {
                                value: Expr::default(),
                            },
                        }],
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
            for i = 0 to 9
                print("Loop")
            next i"#,
            &"Loop\n".repeat(10),
        );
    }

    #[test]
    fn exec_for_loop_with_step() {
        check_output(
            r#"
        for i = 2 to 10 step 2
            print(i)
        next i"#,
            "2\n4\n6\n8\n10\n",
        );
    }

    #[test]
    fn exec_for_loop_with_negative_step() {
        check_output(
            r#"
            for i=10 to 0 step -1
                print(i)
            next i"#,
            "10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n0\n",
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

    #[test]
    fn file_create_open() {
        check_output(
            r#"
            newFile("file_create_open.txt")
            print("created")
            file = open("file_create_open.txt")
            print("opened")"#,
            "created\nopened\n",
        );
        fs::remove_file("file_create_open.txt").unwrap();
    }

    #[test]
    fn file_close() {
        File::create("file_close.txt").unwrap();
        check_output(
            r#"
            f = open("file_close.txt")
            print("opened")
            f.close()
            print("closed")"#,
            "opened\nclosed\n",
        );
        fs::remove_file("file_close.txt").unwrap();
    }

    #[test]
    fn file_read_line() {
        use io::Write;
        let mut f = File::create("file_read_line.txt").unwrap();
        writeln!(f, "Some line").unwrap();
        writeln!(f, "Some other line").unwrap();
        check_output(
            r#"
            f = open("file_read_line.txt")
            line = f.readLine()
            print(line)
            line2 = f.readLine()
            print(line2)
            f.close()
            "#,
            "Some line\n\nSome other line\n\n",
        );
        drop(f);
        fs::remove_file("file_read_line.txt").unwrap();
    }

    #[test]
    fn file_end_of_file() {
        let f = File::create("file_end_of_file.txt").unwrap();
        check_output(
            r#"
            f = open("file_end_of_file.txt")
            print(f.endOfFile())
            f.close()"#,
            "true\n",
        );
        drop(f);
        fs::remove_file("file_end_of_file.txt").unwrap();
    }

    #[test]
    fn file_write_line() {
        check_output(
            r#"
            newFile("file_write_line.txt")
            f = open("file_write_line.txt")
            f.writeLine("Some line")
            f.close()

            f = open("file_write_line.txt")
            line = f.readLine()
            print(line)"#,
            "Some line\n\n",
        );
        fs::remove_file("file_write_line.txt").unwrap();
    }

    #[test]
    fn multiple_runs() {
        let mut interpreter = Interpreter::default();
        interpreter
            .run("function double(x) return x*2 endfunction")
            .unwrap();
        interpreter.run("print(double(2))").unwrap();
    }
}
