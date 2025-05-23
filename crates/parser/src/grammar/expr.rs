use super::{CompletedMarker, Parser, SyntaxKind, TokenKind, expr, stmt};

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
        } else if p.at(TokenKind::EqualEqual) {
            InfixOp::Equal
        } else if p.at(TokenKind::BangEqual) {
            InfixOp::NotEqual
        } else if p.at(TokenKind::Greater) {
            InfixOp::Greater
        } else if p.at(TokenKind::GreaterEqual) {
            InfixOp::GreaterEqual
        } else if p.at(TokenKind::Less) {
            InfixOp::Less
        } else if p.at(TokenKind::LessEqual) {
            InfixOp::LessEqual
        } else if p.at(TokenKind::Dot) {
            InfixOp::Dot
        } else if p.at(TokenKind::LBracket) {
            InfixOp::Subscript
        } else {
            break;
        };

        let (l_bp, r_bp) = op.bp();

        if l_bp < min_bp {
            break;
        }

        p.bump(); // Operator's token

        let m = lhs.precede(p);
        let mut parsed_rhs = expr_bp(p, r_bp).is_some();
        if matches!(op, InfixOp::Subscript) {
            if p.at(TokenKind::Comma) {
                p.bump(); // Comma token
                parsed_rhs = expr_bp(p, r_bp).is_some();
            }
            p.expect(TokenKind::RBracket);
            if p.at(TokenKind::Equal) {
                let cm = m.complete(p, SyntaxKind::IdentSubscript);
                return Some(stmt::array_assigment_fallback(p, cm));
            }
        }

        lhs = m.complete(p, SyntaxKind::BinaryExpr);

        if !parsed_rhs {
            break;
        }
    }

    Some(lhs)
}

const LITERALS: [TokenKind; 5] = [
    TokenKind::Number,
    TokenKind::String,
    TokenKind::Char,
    TokenKind::True,
    TokenKind::False,
];

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = if p.at_set(&LITERALS) {
        literal(p)
    } else if p.at(TokenKind::LParen) {
        paren_expr(p)
    } else if p.at(TokenKind::Minus) || p.at(TokenKind::Not) {
        prefix_expr(p)
    } else if p.at(TokenKind::Ident) {
        name_ref(p)
    } else if p.at(TokenKind::LBracket) {
        array_literal(p)
    } else {
        p.error();
        return None;
    };

    Some(cm)
}

pub(crate) fn array_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LBracket));
    let m = p.start();
    p.bump();

    while !p.at(TokenKind::RBracket) && !p.at_end() {
        expr::expr(p);
        if p.at(TokenKind::Comma) {
            p.bump();
        }
    }
    p.expect(TokenKind::RBracket);
    m.complete(p, SyntaxKind::ArrayLiteral)
}

pub(crate) fn literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_set(&LITERALS));

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
        unreachable!(
            "The only 2 prefix operators are `-` and `NOT`, so it must be one of those"
        )
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

    let mut lhs = m.complete(p, SyntaxKind::NameRef);

    // Handle subprog calls here so that they can be the lhs of a full expression
    if p.at(TokenKind::LParen) {
        let m = lhs.precede(p);
        p.bump();
        while !p.at(TokenKind::RParen) {
            expr(p); // An arg
            if p.at(TokenKind::Comma) {
                p.bump();
            }
        }
        p.expect(TokenKind::RParen);
        lhs = m.complete(p, SyntaxKind::SubprogCall);
    }

    lhs
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
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Dot,
    Subscript,
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
            Self::Equal
            | Self::NotEqual
            | Self::Greater
            | Self::GreaterEqual
            | Self::Less
            | Self::LessEqual => (7, 8),
            Self::Add | Self::Sub => (9, 10),
            Self::Mul | Self::Div | Self::Mod | Self::Quot => (11, 12),
            Self::Pow => (16, 15),
            Self::Subscript => (17, 18),
            Self::Dot => (19, 20),
        }
    }
}

impl PrefixOp {
    fn bp(&self) -> ((), u8) {
        match self {
            PrefixOp::Not => ((), 5),
            PrefixOp::Neg => ((), 13),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_name_ref_expr() {
        check(
            "x",
            expect![[r#"
        Root@0..1
          NameRef@0..1
            Ident@0..1 "x""#]],
        );
    }

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
    fn parse_string_concat_expr() {
        check(
            r#""Hello "+"World!""#,
            expect![[r#"
            Root@0..17
              BinaryExpr@0..17
                Literal@0..8
                  String@0..8 "\"Hello \""
                Plus@8..9 "+"
                Literal@9..17
                  String@9..17 "\"World!\"""#]],
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
    fn parse_nested_pow_expr() {
        check(
            "2^3^4",
            expect![[r#"
            Root@0..5
              BinaryExpr@0..5
                Literal@0..1
                  Number@0..1 "2"
                Caret@1..2 "^"
                BinaryExpr@2..5
                  Literal@2..3
                    Number@2..3 "3"
                  Caret@3..4 "^"
                  Literal@4..5
                    Number@4..5 "4""#]],
        );
    }

    #[test]
    fn parse_mixed_logic_binary_expr() {
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
    fn parse_mixed_comparison_expr() {
        check(
            "1<2!=3>4==5>=6!=7<=8",
            expect![[r#"
                Root@0..20
                  BinaryExpr@0..20
                    BinaryExpr@0..17
                      BinaryExpr@0..14
                        BinaryExpr@0..11
                          BinaryExpr@0..8
                            BinaryExpr@0..6
                              BinaryExpr@0..3
                                Literal@0..1
                                  Number@0..1 "1"
                                Less@1..2 "<"
                                Literal@2..3
                                  Number@2..3 "2"
                              BangEqual@3..5 "!="
                              Literal@5..6
                                Number@5..6 "3"
                            Greater@6..7 ">"
                            Literal@7..8
                              Number@7..8 "4"
                          EqualEqual@8..10 "=="
                          Literal@10..11
                            Number@10..11 "5"
                        GreaterEqual@11..13 ">="
                        Literal@13..14
                          Number@13..14 "6"
                      BangEqual@14..16 "!="
                      Literal@16..17
                        Number@16..17 "7"
                    LessEqual@17..19 "<="
                    Literal@19..20
                      Number@19..20 "8""#]],
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
        check(
            "NOT false",
            expect![[r#"
            Root@0..9
              UnaryExpr@0..9
                Not@0..3 "NOT"
                Whitespace@3..4 " "
                Literal@4..9
                  False@4..9 "false""#]],
        );
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

    #[test]
    fn parse_dot_expr() {
        check(
            "string.upper",
            expect![[r#"
            Root@0..12
              BinaryExpr@0..12
                NameRef@0..6
                  Ident@0..6 "string"
                Dot@6..7 "."
                NameRef@7..12
                  Ident@7..12 "upper""#]],
        );
    }

    #[test]
    fn parse_nested_dot_exprs() {
        check(
            "string.upper.lower",
            expect![[r#"
            Root@0..18
              BinaryExpr@0..18
                BinaryExpr@0..12
                  NameRef@0..6
                    Ident@0..6 "string"
                  Dot@6..7 "."
                  NameRef@7..12
                    Ident@7..12 "upper"
                Dot@12..13 "."
                NameRef@13..18
                  Ident@13..18 "lower""#]],
        );
    }

    #[test]
    fn parse_subscript_expr() {
        check(
            "arr[0]",
            expect![[r#"
            Root@0..6
              BinaryExpr@0..6
                NameRef@0..3
                  Ident@0..3 "arr"
                LBracket@3..4 "["
                Literal@4..5
                  Number@4..5 "0"
                RBracket@5..6 "]""#]],
        );
    }

    #[test]
    fn parse_repeated_subscript_expr() {
        check(
            "arr[0][0]",
            expect![[r#"
            Root@0..9
              BinaryExpr@0..9
                BinaryExpr@0..6
                  NameRef@0..3
                    Ident@0..3 "arr"
                  LBracket@3..4 "["
                  Literal@4..5
                    Number@4..5 "0"
                  RBracket@5..6 "]"
                LBracket@6..7 "["
                Literal@7..8
                  Number@7..8 "0"
                RBracket@8..9 "]""#]],
        );
    }

    #[test]
    fn parse_comma_subscript_expr() {
        check(
            "table[0,0]",
            expect![[r#"
            Root@0..10
              BinaryExpr@0..10
                NameRef@0..5
                  Ident@0..5 "table"
                LBracket@5..6 "["
                Literal@6..7
                  Number@6..7 "0"
                Comma@7..8 ","
                Literal@8..9
                  Number@8..9 "0"
                RBracket@9..10 "]""#]],
        );
    }

    #[test]
    fn parse_array_expr() {
        check(
            "[1,2,3]",
            expect![[r#"
            Root@0..7
              ArrayLiteral@0..7
                LBracket@0..1 "["
                Literal@1..2
                  Number@1..2 "1"
                Comma@2..3 ","
                Literal@3..4
                  Number@3..4 "2"
                Comma@4..5 ","
                Literal@5..6
                  Number@5..6 "3"
                RBracket@6..7 "]""#]],
        );
    }

    #[test]
    fn parse_empty_subprog_call() {
        check(
            "f()",
            expect![[r#"
                Root@0..3
                  SubprogCall@0..3
                    NameRef@0..1
                      Ident@0..1 "f"
                    LParen@1..2 "("
                    RParen@2..3 ")""#]],
        );
    }

    #[test]
    fn parse_one_arg_subprog_call() {
        check(
            "f(x)",
            expect![[r#"
                Root@0..4
                  SubprogCall@0..4
                    NameRef@0..1
                      Ident@0..1 "f"
                    LParen@1..2 "("
                    NameRef@2..3
                      Ident@2..3 "x"
                    RParen@3..4 ")""#]],
        );
    }

    #[test]
    fn parse_multi_arg_subprog_call() {
        check(
            "f(x,y,z)",
            expect![[r#"
                Root@0..8
                  SubprogCall@0..8
                    NameRef@0..1
                      Ident@0..1 "f"
                    LParen@1..2 "("
                    NameRef@2..3
                      Ident@2..3 "x"
                    Comma@3..4 ","
                    NameRef@4..5
                      Ident@4..5 "y"
                    Comma@5..6 ","
                    NameRef@6..7
                      Ident@6..7 "z"
                    RParen@7..8 ")""#]],
        );
    }

    #[test]
    fn parse_subscript_call() {
        check(
            "string.left(4)",
            expect![[r#"
            Root@0..14
              BinaryExpr@0..14
                NameRef@0..6
                  Ident@0..6 "string"
                Dot@6..7 "."
                SubprogCall@7..14
                  NameRef@7..11
                    Ident@7..11 "left"
                  LParen@11..12 "("
                  Literal@12..13
                    Number@12..13 "4"
                  RParen@13..14 ")""#]],
        );
    }

    #[test]
    fn parse_call_in_lhs() {
        check(
            "f(x-1) * x",
            expect![[r#"
            Root@0..10
              BinaryExpr@0..10
                SubprogCall@0..7
                  NameRef@0..1
                    Ident@0..1 "f"
                  LParen@1..2 "("
                  BinaryExpr@2..5
                    NameRef@2..3
                      Ident@2..3 "x"
                    Minus@3..4 "-"
                    Literal@4..5
                      Number@4..5 "1"
                  RParen@5..6 ")"
                  Whitespace@6..7 " "
                Star@7..8 "*"
                Whitespace@8..9 " "
                NameRef@9..10
                  Ident@9..10 "x""#]],
        );
    }

    #[test]
    fn parse_call_in_lhs_in_call() {
        check(
            "f(f(x) - 1)",
            expect![[r#"
            Root@0..11
              SubprogCall@0..11
                NameRef@0..1
                  Ident@0..1 "f"
                LParen@1..2 "("
                BinaryExpr@2..10
                  SubprogCall@2..7
                    NameRef@2..3
                      Ident@2..3 "f"
                    LParen@3..4 "("
                    NameRef@4..5
                      Ident@4..5 "x"
                    RParen@5..6 ")"
                    Whitespace@6..7 " "
                  Minus@7..8 "-"
                  Whitespace@8..9 " "
                  Literal@9..10
                    Number@9..10 "1"
                RParen@10..11 ")""#]],
        );
    }

    #[test]
    fn parse_nested_calls_in_lhs() {
        check(
            "f(5*f(x) - 1) * x",
            expect![[r#"
                Root@0..17
                  BinaryExpr@0..17
                    SubprogCall@0..14
                      NameRef@0..1
                        Ident@0..1 "f"
                      LParen@1..2 "("
                      BinaryExpr@2..12
                        BinaryExpr@2..9
                          Literal@2..3
                            Number@2..3 "5"
                          Star@3..4 "*"
                          SubprogCall@4..9
                            NameRef@4..5
                              Ident@4..5 "f"
                            LParen@5..6 "("
                            NameRef@6..7
                              Ident@6..7 "x"
                            RParen@7..8 ")"
                            Whitespace@8..9 " "
                        Minus@9..10 "-"
                        Whitespace@10..11 " "
                        Literal@11..12
                          Number@11..12 "1"
                      RParen@12..13 ")"
                      Whitespace@13..14 " "
                    Star@14..15 "*"
                    Whitespace@15..16 " "
                    NameRef@16..17
                      Ident@16..17 "x""#]],
        );
    }

    #[test]
    fn parse_array_index_in_rhs() {
        check(
            "1 + bs[1]",
            expect![[r#"
            Root@0..9
              BinaryExpr@0..9
                Literal@0..2
                  Number@0..1 "1"
                  Whitespace@1..2 " "
                Plus@2..3 "+"
                Whitespace@3..4 " "
                BinaryExpr@4..9
                  NameRef@4..6
                    Ident@4..6 "bs"
                  LBracket@6..7 "["
                  Literal@7..8
                    Number@7..8 "1"
                  RBracket@8..9 "]""#]],
        )
    }
}
