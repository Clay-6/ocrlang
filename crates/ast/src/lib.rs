use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

pub struct Root(SyntaxNode);

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDef(VarDef),
    SubprogDef(SubprogDef),
    RetStmt(RetStmt),
    IfElse(IfElse),
    SwitchCase(SwitchCase),
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
    Call(SubprogCall),
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
            SyntaxKind::VarDef => Self::VarDef(VarDef(node)),
            SyntaxKind::SubProgramDef => Self::SubprogDef(SubprogDef(node)),
            SyntaxKind::RetStmt => Self::RetStmt(RetStmt(node)),
            SyntaxKind::IfStmt => Self::IfElse(IfElse(node)),
            SyntaxKind::SwitchStmt => Self::SwitchCase(SwitchCase(node)),
            SyntaxKind::ForLoop => Self::ForLoop(ForLoop(node)),
            SyntaxKind::WhileLoop => Self::WhileLoop(WhileLoop(node)),
            SyntaxKind::DoUntil => Self::DoUntil(DoUntil(node)),
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
            SyntaxKind::SubprogCall => Self::Call(SubprogCall(node)),
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
pub struct RetStmt(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct IfElse(SyntaxNode);

#[derive(Debug, PartialEq)]
pub struct SwitchCase(SyntaxNode);

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

#[derive(Debug, PartialEq)]
pub struct SubprogCall(SyntaxNode);

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

impl RetStmt {
    pub fn value(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl IfElse {
    pub fn condition(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn body(&self) -> Option<impl Iterator<Item = Stmt>> {
        self.0
            .children()
            .find(|t| t.kind() == SyntaxKind::ConditionalBody)
            .map(|b| b.children().filter_map(Stmt::cast))
    }

    pub fn elseif_conditions(&self) -> impl Iterator<Item = Option<Expr>> {
        let mut v = vec![];
        for i in self
            .0
            .children()
            .enumerate()
            .filter(|(_, t)| t.kind() == SyntaxKind::Elseif)
            .map(|(i, _)| i)
        {
            v.push(self.0.children().skip(i).find_map(Expr::cast))
        }
        v.into_iter()
    }

    pub fn elseif_bodies(&self) -> impl Iterator<Item = impl Iterator<Item = Stmt>> {
        let mut bodies = self
            .0
            .children()
            .filter(|t| t.kind() == SyntaxKind::ConditionalBody)
            .skip(1)
            .collect::<Vec<_>>();
        if self
            .0
            .children_with_tokens()
            .any(|t| t.kind() == SyntaxKind::Else)
        {
            bodies.pop();
        }

        bodies
            .into_iter()
            .map(|b| b.children().filter_map(Stmt::cast))
    }

    pub fn else_body(&self) -> Option<impl Iterator<Item = Stmt>> {
        if self
            .0
            .children_with_tokens()
            .any(|t| t.kind() == SyntaxKind::Else)
        {
            self.0
                .children()
                .filter(|t| t.kind() == SyntaxKind::Else)
                .last()
                .map(|b| b.children().filter_map(Stmt::cast))
        } else {
            None
        }
    }
}

impl SwitchCase {
    pub fn scrutinee(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn cases(&self) -> impl Iterator<Item = Expr> {
        let idxs = self
            .0
            .children()
            .enumerate()
            .filter(|(_, t)| t.kind() == SyntaxKind::Case)
            .map(|(i, _)| i);
        let mut v = vec![];
        for i in idxs {
            v.push(self.0.children().skip(i).find_map(Expr::cast))
        }
        v.into_iter().flatten()
    }

    pub fn case_bodies(&self) -> impl Iterator<Item = impl Iterator<Item = Stmt>> {
        let mut bodies = self
            .0
            .children()
            .filter(|t| t.kind() == SyntaxKind::ConditionalBody)
            .collect::<Vec<_>>();
        if self.0.children().any(|t| t.kind() == SyntaxKind::Default) {
            bodies.pop();
        }

        bodies
            .into_iter()
            .map(|b| b.children().filter_map(Stmt::cast))
    }

    pub fn default_body(&self) -> Option<impl Iterator<Item = Stmt>> {
        self.0
            .children()
            .skip_while(|t| t.kind() != SyntaxKind::Default)
            .skip_while(|t| t.kind() != SyntaxKind::ConditionalBody)
            .nth(1)
            .map(|t| t.children().filter_map(Stmt::cast))
    }
}

impl ForLoop {
    pub fn body(&self) -> Option<impl Iterator<Item = Stmt>> {
        self.0
            .children()
            .find(|t| t.kind() == SyntaxKind::LoopBody)
            .map(|t| t.children().filter_map(Stmt::cast))
    }

    pub fn bounds(&self) -> Option<(Expr, Expr)> {
        let mut iter = self.0.children().filter_map(Expr::cast);
        Some((iter.next()?, iter.next()?))
    }

    pub fn step(&self) -> Option<Expr> {
        self.0
            .children()
            .skip_while(|t| t.kind() != SyntaxKind::Step)
            .find_map(Expr::cast)
    }
}

impl WhileLoop {
    pub fn condition(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn body(&self) -> Option<impl Iterator<Item = Stmt>> {
        self.0
            .children()
            .find(|t| t.kind() == SyntaxKind::LoopBody)
            .map(|t| t.children().filter_map(Stmt::cast))
    }
}

impl DoUntil {
    pub fn condition(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).last()
    }

    pub fn body(&self) -> Option<impl Iterator<Item = Stmt>> {
        self.0
            .children()
            .find(|t| t.kind() == SyntaxKind::LoopBody)
            .map(|t| t.children().filter_map(Stmt::cast))
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
                        | SyntaxKind::EqualEqual
                        | SyntaxKind::BangEqual
                        | SyntaxKind::Greater
                        | SyntaxKind::GreaterEqual
                        | SyntaxKind::Less
                        | SyntaxKind::LessEqual
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
            SyntaxKind::String => Some(Val::String(tok.text().into())),
            SyntaxKind::Char => Some(Val::Char(tok.text().chars().find(|&c| c != '\'').unwrap())),
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

impl SubprogCall {
    pub fn callee(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }

    pub fn args(&self) -> impl Iterator<Item = Expr> {
        self.0
            .children()
            .skip(1)
            .filter(|t| t.kind() != SyntaxKind::Comma)
            .take_while(|t| t.kind() != SyntaxKind::RParen)
            .filter_map(Expr::cast)
    }
}
