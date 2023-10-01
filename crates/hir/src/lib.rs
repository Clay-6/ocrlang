mod database;

pub use database::Database;

use la_arena::{Idx, IdxRange};
use smol_str::SmolStr;

type ExprIdx = Idx<Expr>;
type ExprRange = IdxRange<Expr>;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDef {
        name: SmolStr,
        kind: VarDefKind,
        value: Expr,
    },
    ArrayDef {
        name: SmolStr,
        kind: VarDefKind,
        subscript: (Expr, Expr),
        dimensions: (Expr, Expr),
        value: Expr,
    },
    SubprogramDef {
        kind: SubprogramKind,
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
        elseifs: Vec<(ExprIdx, Vec<Stmt>)>,
        else_body: Vec<Stmt>,
    },
    SwitchCase {
        scrutinee: ExprIdx,
        cases: ExprRange,
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
pub enum SubprogramKind {
    Function,
    Procedure,
}

#[derive(Debug, PartialEq)]
pub enum VarDefKind {
    Constant,
    Global,
    Standard,
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
    Equals,
    NotEquals,
    LessThan,
    LessEquals,
    GreaterThan,
    GreaterEquals,
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
