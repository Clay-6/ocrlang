use lexer::TokenKind;

use super::*;

pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_bp(p, 0)
}

fn expr_bp(p: &mut Parser, min_bp: u8) -> Option<CompletedMarker> {
    let mut lhs = lhs(p)?;

    loop {
        let op = if p.at(TokenKind::Plus) {
            InfixOp::Add
        } else if p.at(TokenKind::Minus) {
            InfixOp::Sub
        } else if p.at(TokenKind::Star) {
            InfixOp::Mul
        } else if p.at(TokenKind::Slash) {
            InfixOp::Div
        } else if p.at(TokenKind::Caret) {
            InfixOp::Pow
        } else if p.at(TokenKind::Mod) {
            InfixOp::Mod
        } else if p.at(TokenKind::Div) {
            InfixOp::Quot
        } else if p.at(TokenKind::And) {
            InfixOp::And
        } else if p.at(TokenKind::Or) {
            InfixOp::Or
        } else {
            break;
        };

        let (l_bp, r_bp) = op.bp();

        if l_bp < min_bp {
            break;
        }

        p.bump(); // Operator's token

        let m = lhs.precede(p);
        let parsed_rhs = expr_bp(p, r_bp).is_some();
        lhs = m.complete(p, SyntaxKind::BinaryExpr);

        if !parsed_rhs {
            break;
        }
    }

    Some(lhs)
}

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Number) || p.at(TokenKind::True) || p.at(TokenKind::False) {
        literal(p)
    } else if p.at(TokenKind::LParen) {
        paren_expr(p)
    } else if p.at(TokenKind::Minus) || p.at(TokenKind::Not) {
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
    let m = p.start();

    let op = if p.at(TokenKind::Minus) {
        PrefixOp::Neg
    } else if p.at(TokenKind::Not) {
        PrefixOp::Not
    } else {
        unreachable!()
    };

    let ((), bp) = op.bp();

    p.bump();

    expr_bp(p, bp);

    m.complete(p, SyntaxKind::UnaryExpr)
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
    And,
    Or,
}

enum PrefixOp {
    Neg,
    Not,
}

impl InfixOp {
    fn bp(&self) -> (u8, u8) {
        match self {
            Self::Or => (1, 2),
            Self::And => (3, 4),
            Self::Add | Self::Sub => (5, 6),
            Self::Mul | Self::Div | Self::Mod | Self::Quot => (7, 8),
            Self::Pow => (9, 10),
        }
    }
}

impl PrefixOp {
    fn bp(&self) -> ((), u8) {
        match self {
            PrefixOp::Not => ((), 3),
            PrefixOp::Neg => ((), 6),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_bin_expr() {
        check(
            "1+2",
            expect![[r#"
                Root@0..3
                  BinaryExpr@0..3
                    Literal@0..1
                      Number@0..1 "1"
                    Plus@1..2 "+"
                    Literal@2..3
                      Number@2..3 "2""#]],
        );
    }

    #[test]
    fn parse_bin_expr_with_nesting() {
        check(
            "1+2*3",
            expect![[r#"
                Root@0..5
                  BinaryExpr@0..5
                    Literal@0..1
                      Number@0..1 "1"
                    Plus@1..2 "+"
                    BinaryExpr@2..5
                      Literal@2..3
                        Number@2..3 "2"
                      Star@3..4 "*"
                      Literal@4..5
                        Number@4..5 "3""#]],
        );
    }

    #[test]
    fn parse_mixed_binary_logic_expr() {
        check(
            "true AND true OR false",
            expect![[r#"
            Root@0..22
              BinaryExpr@0..22
                BinaryExpr@0..14
                  Literal@0..5
                    True@0..4 "true"
                    Whitespace@4..5 " "
                  And@5..8 "AND"
                  Whitespace@8..9 " "
                  Literal@9..14
                    True@9..13 "true"
                    Whitespace@13..14 " "
                Or@14..16 "OR"
                Whitespace@16..17 " "
                Literal@17..22
                  False@17..22 "false""#]],
        );
    }

    #[test]
    fn parse_unary_expr() {
        check(
            "-15",
            expect![[r#"
            Root@0..3
              UnaryExpr@0..3
                Minus@0..1 "-"
                Literal@1..3
                  Number@1..3 "15""#]],
        );
    }

    #[test]
    fn parse_logic_unary_expr() {
        check("NOT false", expect![[r#"
            Root@0..9
              UnaryExpr@0..9
                Not@0..3 "NOT"
                Whitespace@3..4 " "
                Literal@4..9
                  False@4..9 "false""#]]);
    }

    #[test]
    fn parse_bin_and_unary_expr() {
        check(
            "-1+2",
            expect![[r#"
            Root@0..4
              BinaryExpr@0..4
                UnaryExpr@0..2
                  Minus@0..1 "-"
                  Literal@1..2
                    Number@1..2 "1"
                Plus@2..3 "+"
                Literal@3..4
                  Number@3..4 "2""#]],
        );
    }

    #[test]
    fn parse_paren_expr() {
        check(
            "(1+2)",
            expect![[r#"
            Root@0..5
              ParenExpr@0..5
                LParen@0..1 "("
                BinaryExpr@1..4
                  Literal@1..2
                    Number@1..2 "1"
                  Plus@2..3 "+"
                  Literal@3..4
                    Number@3..4 "2"
                RParen@4..5 ")""#]],
        );
    }

    #[test]
    fn parse_nested_paren_expr() {
        check(
            "(1+(3-1))",
            expect![[r#"
            Root@0..9
              ParenExpr@0..9
                LParen@0..1 "("
                BinaryExpr@1..8
                  Literal@1..2
                    Number@1..2 "1"
                  Plus@2..3 "+"
                  ParenExpr@3..8
                    LParen@3..4 "("
                    BinaryExpr@4..7
                      Literal@4..5
                        Number@4..5 "3"
                      Minus@5..6 "-"
                      Literal@6..7
                        Number@6..7 "1"
                    RParen@7..8 ")"
                RParen@8..9 ")""#]],
        );
    }
}
