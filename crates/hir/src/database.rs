use la_arena::Arena;
use syntax::SyntaxKind;

use crate::{BinaryOp, Expr, UnaryOp, Value};

pub struct Database {
    exprs: Arena<Expr>,
}

impl Database {
    pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
        if let Some(ast) = ast {
            match ast {
                ast::Expr::Binary(ast) => self.lower_binary(ast),
                ast::Expr::Unary(ast) => self.lower_unary(ast),
                ast::Expr::Literal(ast) => self.lower_literal(ast),
                ast::Expr::ArrayLiteral(ast) => self.lower_array_literal(ast),
                ast::Expr::Paren(ast) => self.lower_expr(ast.expr()),
                ast::Expr::NameRef(ast) => self.lower_name_ref(ast),
                ast::Expr::Call(ast) => self.lower_call(ast),
            }
        } else {
            Expr::Missing
        }
    }

    fn lower_binary(&mut self, ast: ast::BinaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Minus => BinaryOp::Sub,
            SyntaxKind::Star => BinaryOp::Mul,
            SyntaxKind::Slash => BinaryOp::Div,
            SyntaxKind::Mod => BinaryOp::Mod,
            SyntaxKind::Div => BinaryOp::Quot,
            SyntaxKind::And => BinaryOp::And,
            SyntaxKind::Or => BinaryOp::Or,
            SyntaxKind::Caret => BinaryOp::Pow,
            SyntaxKind::Dot => BinaryOp::Dot,
            _ => unreachable!(),
        };

        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());

        Expr::Binary {
            lhs: self.exprs.alloc(lhs),
            rhs: self.exprs.alloc(rhs),
            op,
        }
    }

    fn lower_unary(&mut self, ast: ast::UnaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Not => UnaryOp::Not,
            SyntaxKind::Minus => UnaryOp::Neg,
            _ => unreachable!(),
        };

        let opand = self.lower_expr(ast.expr());

        Expr::Unary {
            op,
            opand: self.exprs.alloc(opand),
        }
    }

    fn lower_literal(&mut self, ast: ast::Literal) -> Expr {
        match ast.parse() {
            Some(v) => match v {
                ast::Val::Int(i) => Expr::Literal {
                    value: Value::Int(i),
                },
                ast::Val::Float(f) => Expr::Literal {
                    value: Value::Float(f),
                },
                ast::Val::Char(c) => Expr::Literal {
                    value: Value::Char(c),
                },
                ast::Val::String(s) => Expr::Literal {
                    value: Value::String(s.into()),
                },
                ast::Val::Bool(b) => Expr::Literal {
                    value: Value::Bool(b),
                },
            },
            None => Expr::Missing,
        }
    }

    fn lower_array_literal(&mut self, ast: ast::ArrayLiteral) -> Expr {
        match ast.items() {
            Some(v) => {
                let vals = v
                    .into_iter()
                    .map(|i| self.lower_expr(Some(i)))
                    .collect::<Vec<_>>();
                Expr::Literal {
                    value: Value::Array(self.exprs.alloc_many(vals)),
                }
            }
            None => Expr::Missing,
        }
    }

    fn lower_name_ref(&mut self, ast: ast::NameRef) -> Expr {
        Expr::NameRef {
            name: ast.name().unwrap().text().into(),
        }
    }

    fn lower_call(&mut self, ast: ast::SubprogCall) -> Expr {
        let callee = ast.callee().unwrap().text().into();
        let args = ast
            .args()
            .map(|e| self.lower_expr(Some(e)))
            .collect::<Vec<_>>();

        Expr::Call {
            callee,
            args: self.exprs.alloc_many(args),
        }
    }
}
