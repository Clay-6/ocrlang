mod env;

use env::{Binding, Env};
use hir::{Database, Stmt};
use smol_str::SmolStr;

pub type IResult<T> = Result<T, InterpretResult>;

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
                        .map_err(|_| InterpretResult::ReassignedConstant)
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
            } => todo!(),
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
        todo!()
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
pub enum Value {
    Int(i64),
    Float(f64),
    Char(char),
    String(SmolStr),
    Bool(bool),
    Unit,
}

pub enum InterpretResult {
    ReassignedConstant,
}
