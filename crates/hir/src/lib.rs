mod database;

pub use database::Database;

use la_arena::{Idx, IdxRange};
use smol_str::SmolStr;
use text_size::TextRange;

pub type ExprIdx = Idx<Expr>;
pub type ExprRange = IdxRange<Expr>;

#[derive(Debug, PartialEq, Clone)]
pub struct Stmt {
    pub range: TextRange,
    pub kind: StmtKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StmtKind {
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
        name: SmolStr,
        params: Vec<SmolStr>,
        body: Vec<Stmt>,
    },
    ReturnStmt {
        value: Expr,
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
        loop_var: Option<SmolStr>,
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
    ImportStmt {
        path: SmolStr,
    },
    Expr(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub range: TextRange,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
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
        args: ExprRange,
    },
    Literal {
        value: Literal,
    },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VarDefKind {
    Constant,
    Global,
    Standard,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
    String(SmolStr),
    Bool(bool),
    Array(ExprRange),
}

#[must_use]
pub fn lower(ast: &ast::Root) -> (Database, Vec<Stmt>) {
    let mut db = Database::default();
    let stmts = ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect();

    (db, stmts)
}

#[must_use]
pub fn lower_with(db: &mut Database, ast: &ast::Root) -> Vec<Stmt> {
    ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect()
}

impl Default for Expr {
    fn default() -> Self {
        Self {
            range: Default::default(),
            kind: ExprKind::Missing,
        }
    }
}
