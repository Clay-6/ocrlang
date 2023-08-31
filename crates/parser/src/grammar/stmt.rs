use super::*;

const VAR_DEF_START: [TokenKind; 3] = [
    // TokenKind::Ident,
    TokenKind::Array,
    TokenKind::Const,
    TokenKind::Global,
];

pub(crate) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at_set(&VAR_DEF_START) {
        Some(var_def(p))
    } else {
        expr::expr(p)
    }
}

// TODO: Allow implicit declarations
fn var_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_set(&VAR_DEF_START));
    let m = p.start();

    p.bump(); // First token

    if p.at(TokenKind::Ident) {
        // That wasn't the ident, bump it now
        p.bump();
    }

    p.expect(TokenKind::Equal);

    expr::expr(p);

    m.complete(p, SyntaxKind::VarDef)
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    // #[test]
    // fn parse_var_def() {
    //     check(
    //         "x=15",
    //         expect![[r#"
    //         Root@0..4
    //           VarDef@0..4
    //             Ident@0..1 "x"
    //             Equal@1..2 "="
    //             Literal@2..4
    //               Number@2..4 "15""#]],
    //     )
    // }

    #[test]
    fn parse_const_def() {
        check(
            "const PI=3.14",
            expect![[r#"
            Root@0..13
              VarDef@0..13
                Const@0..5 "const"
                Whitespace@5..6 " "
                Ident@6..8 "PI"
                Equal@8..9 "="
                Literal@9..13
                  Number@9..13 "3.14""#]],
        )
    }
}
