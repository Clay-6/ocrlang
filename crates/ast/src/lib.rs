use std::borrow::Cow;

use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal(Literal),
    Paren(ParenExpr),
    VarRef(VariableRef),
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct UnaryExpr(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct Literal(SyntaxNode);

pub enum Val {
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
pub struct ParenExpr(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct VariableRef(SyntaxNode);

impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| {
                matches!(
                    token.kind(),
                    SyntaxKind::Plus
                        | SyntaxKind::Minus
                        | SyntaxKind::Star
                        | SyntaxKind::Slash
                        | SyntaxKind::Mod
                        | SyntaxKind::Div
                        | SyntaxKind::And
                        | SyntaxKind::Or
                        | SyntaxKind::Dot
                )
            })
    }
}

impl UnaryExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| matches!(token.kind(), SyntaxKind::Minus | SyntaxKind::Not))
    }
}

impl Literal {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if matches!(
            node.kind(),
            SyntaxKind::Number | SyntaxKind::String | SyntaxKind::True | SyntaxKind::False
        ) {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn parse(&self) -> Option<Val> {
        let tok = self.0.first_token().unwrap();

        match tok.kind() {
            SyntaxKind::True => Some(Val::Bool(true)),
            SyntaxKind::False => Some(Val::Bool(false)),
            SyntaxKind::String => Some(Val::String(tok.text().to_string())),
            SyntaxKind::Number => {
                let txt = tok.text();
                if txt.contains('.') {
                    Some(Val::Float(txt.parse().unwrap()))
                } else {
                    Some(Val::Int(txt.parse().unwrap()))
                }
            }
            _ => None,
        }
    }
}

impl ParenExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl VariableRef {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }
}
