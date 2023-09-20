use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

pub struct Root(SyntaxNode);

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDef(VarDef),
    SubprogDef(SubprogDef),
    ForLoop(ForLoop),
    WhileLoop(WhileLoop),
    DoUntil(DoUntil),
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal(Literal),
    ArrayLiteral(ArrayLiteral),
    Paren(ParenExpr),
    NameRef(NameRef),
}

impl Root {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Root {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }
}

impl Stmt {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        Some(match node.kind() {
            SyntaxKind::VarDef => todo!(),
            _ => Self::Expr(Expr::cast(node)?),
        })
    }
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::BinaryExpr => Self::Binary(BinaryExpr(node)),
            SyntaxKind::UnaryExpr => Self::Unary(UnaryExpr(node)),
            SyntaxKind::ParenExpr => Self::Paren(ParenExpr(node)),
            SyntaxKind::NameRef => Self::NameRef(NameRef(node)),
            SyntaxKind::Literal => Self::Literal(Literal(node)),
            SyntaxKind::ArrayLiteral => Self::ArrayLiteral(ArrayLiteral(node)),
            _ => return None,
        };

        Some(result)
    }
}

#[derive(Debug, PartialEq)]
pub struct VarDef(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct SubprogDef(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct ForLoop(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct WhileLoop(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct DoUntil(SyntaxNode);

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
pub struct ArrayLiteral(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct ParenExpr(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct NameRef(SyntaxNode);

impl VarDef {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| matches!(token.kind(), SyntaxKind::Ident | SyntaxKind::IdentSubscript))
    }

    pub fn value(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl SubprogDef {
    pub fn kind(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|tok| matches!(tok.kind(), SyntaxKind::Function | SyntaxKind::Procedure))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|tok| tok.kind() == SyntaxKind::Ident)
    }

    pub fn params(&self) -> impl Iterator<Item = SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|tok| tok.kind() == SyntaxKind::Ident)
            .skip(1)
    }

    pub fn body(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }
}

impl ForLoop {
    pub fn body(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }

    pub fn bounds(&self) -> Option<(Expr, Expr)> {
        let mut iter = self.0.children().filter_map(Expr::cast);
        Some((iter.next()?, iter.next()?))
    }

    pub fn step(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(2)
    }
}

impl WhileLoop {
    pub fn condition(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn body(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }
}

impl DoUntil {
    pub fn condition(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).last()
    }

    pub fn body(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }
}

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
                    Some(Val::Float(txt.parse().ok()?))
                } else {
                    Some(Val::Int(txt.parse().ok()?))
                }
            }
            _ => None,
        }
    }
}

impl ArrayLiteral {
    pub fn items(&self) -> Option<Vec<Expr>> {
        Some(
            self.0
                .children_with_tokens()
                .filter_map(SyntaxElement::into_node)
                .filter_map(Expr::cast)
                .collect(),
        )
    }
}

impl ParenExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl NameRef {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }
}
