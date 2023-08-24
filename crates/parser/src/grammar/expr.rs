use lexer::TokenKind;

use super::*;

pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_bp(p, 0)
}

fn expr_bp(p: &mut Parser, bp: u8) -> Option<CompletedMarker> {
    let lhs = lhs(p)?;

    todo!();
}

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Number) {
        literal(p)
    } else if p.at(TokenKind::LParen) {
        paren_expr(p)
    } else if p.at(TokenKind::Minus) {
        prefix_expr(p)
    } else if p.at(TokenKind::Ident) {
        name_ref(p)
    } else {
        p.error();
        return None;
    };

    Some(cm)
}

fn literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Number));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::Literal)
}

fn paren_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));

    let m = p.start();
    p.bump(); // Left paren
    expr_bp(p, 0);
    p.expect(TokenKind::RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

fn prefix_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Minus));

    let m = p.start();

    let op = PrefixOp::Neg;
    let ((), bp) = op.bp();

    p.bump();

    expr_bp(p, bp);

    m.complete(p, SyntaxKind::PrefixExpr)
}

fn name_ref(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::NameRef)
}

enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Quot,
    Pow,
}

enum PrefixOp {
    Neg,
}

impl InfixOp {
    fn bp(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div | Self::Mod | Self::Quot => (3, 4),
            Self::Pow => (7, 6),
        }
    }
}

impl PrefixOp {
    fn bp(&self) -> ((), u8) {
        match self {
            PrefixOp::Neg => ((), 5),
        }
    }
}
