mod database;

pub use database::Database;

use la_arena::{Idx, IdxRange};
use smol_str::SmolStr;

type ExprIdx = Idx<Expr>;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDef {
        name: SmolStr,
        value: Expr,
    },
    SubprogramDef {
        name: SmolStr,
        params: Vec<SmolStr>,
        body: Vec<Stmt>,
    },
    ReturnStmt {
        value: ExprIdx,
    },
    IfElse {
        condition: ExprIdx,
        body: Vec<Stmt>,
        else_body: Vec<Stmt>,
    },
    SwitchCase {
        scrutinee: ExprIdx,
        cases: IdxRange<Expr>,
        case_bodies: Vec<Vec<Stmt>>,
        default_body: Vec<Stmt>,
    },
    ForLoop {
        start: ExprIdx,
        end: ExprIdx,
        step: ExprIdx,
        body: Vec<Stmt>,
    },
    WhileLoop {
        condition: ExprIdx,
        body: Vec<Stmt>,
    },
    DoUntilLoop {
        condition: ExprIdx,
        body: Vec<Stmt>,
    },
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Missing,
    Binary {
        op: BinaryOp,
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
    Unary {
        op: UnaryOp,
        opand: ExprIdx,
    },
    NameRef {
        name: SmolStr,
    },
    Call {
        callee: SmolStr,
        args: IdxRange<Expr>,
    },
    Literal {
        value: Value,
    },
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Quot,
    And,
    Or,
    SubScript,
    Dot,
    Pow,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Char(char),
    String(SmolStr),
    Bool(bool),
    Array(IdxRange<Expr>),
}

pub fn lower(ast: ast::Root) -> (Database, Vec<Stmt>) {
    let mut db = Database::default();
    let stmts = ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect();

    (db, stmts)
}
