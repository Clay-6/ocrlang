mod env;

use env::{Binding, Env, SubprogKind};
use hir::{Database, Stmt};
use smol_str::SmolStr;

pub type IResult<T> = Result<T, InterpretError>;

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

        self.execute(&stmts, db)
    }

    pub fn execute(&mut self, stmts: &[Stmt], db: Database) -> IResult<()> {
        for stmt in stmts {
            self.exec_stmt(stmt, &db)?;
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
                    Binding::Func {
                        kind,
                        params: params.clone(),
                        body: body.clone(),
                    },
                );
                Ok(Value::Unit)
            }
            Stmt::ReturnStmt { value } => todo!(),
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
            hir::Expr::Binary { op, lhs, rhs } => todo!(),
            hir::Expr::Unary { op, opand } => todo!(),
            hir::Expr::NameRef { name } => todo!(),
            hir::Expr::Call { callee, args } => todo!(),
            hir::Expr::Missing => Ok(Value::Unit),
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
}

#[derive(Debug)]
pub enum InterpretError {
    ReassignedConstant,
    HeterogeneousArray,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_literal() {}
}
