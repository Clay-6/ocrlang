mod database;

pub use database::Database;

use la_arena::{Idx, IdxRange};
use smol_str::SmolStr;

type ExprIdx = Idx<Expr>;

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
    String(String),
    Bool(bool),
    Array(IdxRange<Expr>),
}
