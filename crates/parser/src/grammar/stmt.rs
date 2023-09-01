use super::*;

const VAR_DEF_START: [TokenKind; 3] = [
    // TokenKind::Ident,
    TokenKind::Array,
    TokenKind::Const,
    TokenKind::Global,
];
const SUBPROG_START: [TokenKind; 2] = [TokenKind::Function, TokenKind::Procedure];
const SUBPROG_END: [TokenKind; 2] = [TokenKind::Endfunction, TokenKind::Endprocedure];

pub(crate) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at_set(&VAR_DEF_START) {
        Some(var_def(p))
    } else if p.at_set(&SUBPROG_START) {
        Some(subprog_def(p))
    } else if p.at(TokenKind::Return) {
        Some(ret(p))
    } else {
        expr::expr(p)
    }
}

fn ret(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Return));
    let m = p.start();
    p.bump(); // `return` kw
    expr::expr(p);

    m.complete(p, SyntaxKind::RetStmt)
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

fn subprog_def(p: &mut Parser) -> CompletedMarker {
    enum FuncKind {
        Func,
        Proc,
    }
    assert!(p.at_set(&SUBPROG_START));
    let kind = if p.at(TokenKind::Function) {
        FuncKind::Func
    } else {
        FuncKind::Proc
    };

    let m = p.start();
    p.bump(); // `function`/`procedure`

    p.expect(TokenKind::Ident); // name

    p.expect(TokenKind::LParen);
    while p.at(TokenKind::Ident) {
        p.bump(); // param name
        if p.at(TokenKind::Comma) {
            p.bump()
        }
    }

    p.expect(TokenKind::RParen);

    while !p.at_set(&SUBPROG_END) {
        stmt(p);
    }

    match kind {
        FuncKind::Func => p.expect(TokenKind::Endfunction),
        FuncKind::Proc => p.expect(TokenKind::Endprocedure),
    }

    m.complete(p, SyntaxKind::SubProgramDef)
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

    #[test]
    fn parse_proc_no_body() {
        check(
            "procedure f() endprocedure",
            expect![[r#"
            Root@0..26
              SubProgramDef@0..26
                Procedure@0..9 "procedure"
                Whitespace@9..10 " "
                Ident@10..11 "f"
                LParen@11..12 "("
                RParen@12..13 ")"
                Whitespace@13..14 " "
                Endprocedure@14..26 "endprocedure""#]],
        )
    }

    #[test]
    fn parse_return_stmt() {
        check(
            "return 69",
            expect![[r#"
            Root@0..9
              RetStmt@0..9
                Return@0..6 "return"
                Whitespace@6..7 " "
                Literal@7..9
                  Number@7..9 "69""#]],
        )
    }

    #[test]
    fn parse_func_one_stmt() {
        check(
            "function f()
return 42
endfunction",
            expect![[r#"
                Root@0..34
                  SubProgramDef@0..34
                    Function@0..8 "function"
                    Whitespace@8..9 " "
                    Ident@9..10 "f"
                    LParen@10..11 "("
                    RParen@11..12 ")"
                    Newline@12..13 "\n"
                    RetStmt@13..23
                      Return@13..19 "return"
                      Whitespace@19..20 " "
                      Literal@20..23
                        Number@20..22 "42"
                        Newline@22..23 "\n"
                    Endfunction@23..34 "endfunction""#]],
        )
    }
}
