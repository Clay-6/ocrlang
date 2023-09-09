use super::*;

const VAR_DEF_START: [TokenKind; 4] = [
    TokenKind::Ident,
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
    } else if p.at(TokenKind::If) {
        Some(if_else(p))
    } else {
        expr::expr(p)
    }
}

fn if_else(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::If));
    let m = p.start();
    p.bump(); // `if` token
    expr::expr(p); // Condition

    p.expect(TokenKind::Then);

    while !(p.at(TokenKind::Elseif) || p.at(TokenKind::Else) || p.at(TokenKind::Endif)) {
        stmt(p); // The body
    }

    // Parse the elseif & else clauses
    while !p.at(TokenKind::Endif) {
        if p.at(TokenKind::Elseif) {
            p.bump();
            expr::expr(p); // Condition again
            p.expect(TokenKind::Then);
            while !(p.at(TokenKind::Elseif) || p.at(TokenKind::Else) || p.at(TokenKind::Endif)) {
                stmt(p); // The body
            }
        }

        if p.at(TokenKind::Else) {
            p.bump();
            while !p.at(TokenKind::Endif) {
                stmt(p); // The body again, again
            }
        }
    }

    p.expect(TokenKind::Endif);

    m.complete(p, SyntaxKind::IfStmt)
}

fn ret(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Return));
    let m = p.start();
    p.bump(); // `return` kw
    expr::expr(p);

    m.complete(p, SyntaxKind::RetStmt)
}

fn var_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_set(&VAR_DEF_START));

    if p.at(TokenKind::Ident)
    // All the things that could mean we're not actually in a variable def
        && (
            p.peek_next() == Some(TokenKind::Dot) 
            || p.peek_next() == Some(TokenKind::LBracket) 
            || p.peek_next() == Some(TokenKind::LParen)
        )
    {
        // No attrs for you, young one
        return expr::expr(p).expect("This'll never be none, the ident we're at is a valid lhs");
    }

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

    #[test]
    fn parse_var_def() {
        check(
            "x=15",
            expect![[r#"
            Root@0..4
              VarDef@0..4
                Ident@0..1 "x"
                Equal@1..2 "="
                Literal@2..4
                  Number@2..4 "15""#]],
        )
    }

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

    #[test]
    fn parse_proc_many_stmts() {
        check(
            "procedure p()\n1 + 1\n2 + 2\nendprocedure",
            expect![[r#"
            Root@0..38
              SubProgramDef@0..38
                Procedure@0..9 "procedure"
                Whitespace@9..10 " "
                Ident@10..11 "p"
                LParen@11..12 "("
                RParen@12..13 ")"
                Newline@13..14 "\n"
                BinaryExpr@14..20
                  Literal@14..16
                    Number@14..15 "1"
                    Whitespace@15..16 " "
                  Plus@16..17 "+"
                  Whitespace@17..18 " "
                  Literal@18..20
                    Number@18..19 "1"
                    Newline@19..20 "\n"
                BinaryExpr@20..26
                  Literal@20..22
                    Number@20..21 "2"
                    Whitespace@21..22 " "
                  Plus@22..23 "+"
                  Whitespace@23..24 " "
                  Literal@24..26
                    Number@24..25 "2"
                    Newline@25..26 "\n"
                Endprocedure@26..38 "endprocedure""#]],
        )
    }

    #[test]
    fn parse_func_one_param() {
        check(
            "function id(x)\nreturn x\nendfunction",
            expect![[r#"
            Root@0..35
              SubProgramDef@0..35
                Function@0..8 "function"
                Whitespace@8..9 " "
                Ident@9..11 "id"
                LParen@11..12 "("
                Ident@12..13 "x"
                RParen@13..14 ")"
                Newline@14..15 "\n"
                RetStmt@15..24
                  Return@15..21 "return"
                  Whitespace@21..22 " "
                  NameRef@22..24
                    Ident@22..23 "x"
                    Newline@23..24 "\n"
                Endfunction@24..35 "endfunction""#]],
        );
    }

    #[test]
    fn parse_func_many_params() {
        check(
            "function add(x, y, z)\nreturn x + y + z\nendfunction",
            expect![[r#"
                Root@0..50
                  SubProgramDef@0..50
                    Function@0..8 "function"
                    Whitespace@8..9 " "
                    Ident@9..12 "add"
                    LParen@12..13 "("
                    Ident@13..14 "x"
                    Comma@14..15 ","
                    Whitespace@15..16 " "
                    Ident@16..17 "y"
                    Comma@17..18 ","
                    Whitespace@18..19 " "
                    Ident@19..20 "z"
                    RParen@20..21 ")"
                    Newline@21..22 "\n"
                    RetStmt@22..39
                      Return@22..28 "return"
                      Whitespace@28..29 " "
                      BinaryExpr@29..39
                        BinaryExpr@29..35
                          NameRef@29..31
                            Ident@29..30 "x"
                            Whitespace@30..31 " "
                          Plus@31..32 "+"
                          Whitespace@32..33 " "
                          NameRef@33..35
                            Ident@33..34 "y"
                            Whitespace@34..35 " "
                        Plus@35..36 "+"
                        Whitespace@36..37 " "
                        NameRef@37..39
                          Ident@37..38 "z"
                          Newline@38..39 "\n"
                    Endfunction@39..50 "endfunction""#]],
        );
    }

    #[test]
    fn parse_if_elif_else() {
        check(
            r#"if answer == "Yes" then
    print("Correct")
elseif answer == "No" then
    print("Wrong")
else
    print("Error")
endif"#,
            expect![[r#"
                Root@0..120
                  IfStmt@0..120
                    If@0..2 "if"
                    Whitespace@2..3 " "
                    BinaryExpr@3..19
                      NameRef@3..10
                        Ident@3..9 "answer"
                        Whitespace@9..10 " "
                      EqualEqual@10..12 "=="
                      Whitespace@12..13 " "
                      Literal@13..19
                        String@13..18 "\"Yes\""
                        Whitespace@18..19 " "
                    Then@19..23 "then"
                    Newline@23..24 "\n"
                    Whitespace@24..28 "    "
                    SubprogCall@28..45
                      NameRef@28..33
                        Ident@28..33 "print"
                      LParen@33..34 "("
                      Literal@34..43
                        String@34..43 "\"Correct\""
                      RParen@43..44 ")"
                      Newline@44..45 "\n"
                    Elseif@45..51 "elseif"
                    Whitespace@51..52 " "
                    BinaryExpr@52..67
                      NameRef@52..59
                        Ident@52..58 "answer"
                        Whitespace@58..59 " "
                      EqualEqual@59..61 "=="
                      Whitespace@61..62 " "
                      Literal@62..67
                        String@62..66 "\"No\""
                        Whitespace@66..67 " "
                    Then@67..71 "then"
                    Newline@71..72 "\n"
                    Whitespace@72..76 "    "
                    SubprogCall@76..91
                      NameRef@76..81
                        Ident@76..81 "print"
                      LParen@81..82 "("
                      Literal@82..89
                        String@82..89 "\"Wrong\""
                      RParen@89..90 ")"
                      Newline@90..91 "\n"
                    Else@91..95 "else"
                    Newline@95..96 "\n"
                    Whitespace@96..100 "    "
                    SubprogCall@100..115
                      NameRef@100..105
                        Ident@100..105 "print"
                      LParen@105..106 "("
                      Literal@106..113
                        String@106..113 "\"Error\""
                      RParen@113..114 ")"
                      Newline@114..115 "\n"
                    Endif@115..120 "endif""#]],
        )
    }
}
