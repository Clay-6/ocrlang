use super::{CompletedMarker, Parser, SyntaxKind, TokenKind, expr};

const VAR_DEF_START: [TokenKind; 4] = [
    TokenKind::Ident,
    TokenKind::Const,
    TokenKind::Global,
    TokenKind::Array,
];
const SUBPROG_START: [TokenKind; 2] =
    [TokenKind::Function, TokenKind::Procedure];
const SUBPROG_END: [TokenKind; 2] =
    [TokenKind::Endfunction, TokenKind::Endprocedure];
const CASE_ENDINGS: [TokenKind; 3] =
    [TokenKind::Case, TokenKind::Default, TokenKind::Endswitch];

pub(crate) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at_set(&VAR_DEF_START) {
        Some(var_def(p))
    } else if p.at_set(&SUBPROG_START) {
        Some(subprog_def(p))
    } else if p.at(TokenKind::Return) {
        Some(ret(p))
    } else if p.at(TokenKind::If) {
        Some(if_else(p))
    } else if p.at(TokenKind::For) {
        Some(for_loop(p))
    } else if p.at(TokenKind::While) {
        Some(while_loop(p))
    } else if p.at(TokenKind::Do) {
        Some(do_until(p))
    } else if p.at(TokenKind::Switch) {
        Some(switch_stmt(p))
    } else if p.at(TokenKind::Import) {
        Some(import_stmt(p))
    } else {
        expr::expr(p)
    }
}

fn import_stmt(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Import));
    let m = p.start();
    p.bump(); // `import`

    p.expect(TokenKind::String); // File path

    m.complete(p, SyntaxKind::Import)
}

fn switch_stmt(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Switch));
    let m = p.start();
    p.bump();

    expr::expr(p);
    p.expect(TokenKind::Colon);

    while !p.at_end() && !p.at_set(&[TokenKind::Endswitch, TokenKind::Default])
    {
        if p.at(TokenKind::Case) {
            p.bump();
            let cond = p.start();
            expr::literal(p);
            cond.complete(p, SyntaxKind::ConditionExpr);
            p.expect(TokenKind::Colon);
            let body = p.start();
            while !p.at_set(&CASE_ENDINGS) {
                stmt(p);
            }
            body.complete(p, SyntaxKind::ConditionalBody);
        }
    }
    if p.at(TokenKind::Default) {
        p.bump();
        p.expect(TokenKind::Colon);
        let body = p.start();
        while !p.at_end() && !p.at_set(&CASE_ENDINGS) {
            stmt(p);
        }
        body.complete(p, SyntaxKind::OtherwiseBody);
    }

    p.expect(TokenKind::Endswitch);
    m.complete(p, SyntaxKind::SwitchStmt)
}

fn do_until(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Do));
    let m = p.start();
    p.bump();

    let bm = p.start();
    while !p.at_end() && !p.at(TokenKind::Until) {
        stmt(p);
    }
    bm.complete(p, SyntaxKind::LoopBody);
    p.expect(TokenKind::Until);

    expr::expr(p);
    m.complete(p, SyntaxKind::DoUntil)
}

fn while_loop(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::While));
    let m = p.start();
    p.bump(); // `while`

    expr::expr(p); // Condition

    let bm = p.start();

    while !p.at_end() && !p.at(TokenKind::Endwhile) {
        stmt(p);
    }
    bm.complete(p, SyntaxKind::LoopBody);
    p.expect(TokenKind::Endwhile);

    m.complete(p, SyntaxKind::WhileLoop)
}

fn for_loop(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::For));
    let m = p.start();
    p.bump(); // `for`

    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Equal);
    expr::expr(p); // Start val
    p.expect(TokenKind::To);
    expr::expr(p); // End val

    if p.at(TokenKind::Step) {
        p.bump();
        expr::expr(p);
    }
    let bm = p.start();
    while !p.at_end() && !p.at(TokenKind::Next) {
        stmt(p);
    }

    p.expect(TokenKind::Next);
    p.expect(TokenKind::Ident);

    bm.complete(p, SyntaxKind::LoopBody);
    m.complete(p, SyntaxKind::ForLoop)
}

fn if_else(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::If));
    let m = p.start();
    p.bump(); // `if` token
    expr::expr(p); // Condition

    p.expect(TokenKind::Then);

    let main_body = p.start();
    while !p.at_end()
        && !p.at_set(&[TokenKind::Elseif, TokenKind::Else, TokenKind::Endif])
    {
        stmt(p); // The body
    }
    main_body.complete(p, SyntaxKind::PrimaryBody);

    // Parse the elseif clauses
    while !p.at_end() && !p.at_set(&[TokenKind::Endif, TokenKind::Else]) {
        if p.at(TokenKind::Elseif) {
            p.bump();
            let cond = p.start();
            expr::expr(p); // Condition again
            cond.complete(p, SyntaxKind::ConditionExpr);
            p.expect(TokenKind::Then);
            let inner_body = p.start();
            while !p.at_end()
                && !p.at_set(&[
                    TokenKind::Elseif,
                    TokenKind::Else,
                    TokenKind::Endif,
                ])
            {
                stmt(p); // The body
            }
            inner_body.complete(p, SyntaxKind::ConditionalBody);
        }
    }

    // (Maybe) Parse the else clause
    if p.at(TokenKind::Else) {
        p.bump();
        let else_body = p.start();
        while !p.at_end() && !p.at(TokenKind::Endif) {
            stmt(p); // The body again, again
        }
        else_body.complete(p, SyntaxKind::OtherwiseBody);
    }

    p.expect(TokenKind::Endif);

    m.complete(p, SyntaxKind::IfStmt)
}

fn ret(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Return));
    let m = p.start();
    p.bump(); // `return` kw
    if !p.at_newline() {
        expr::expr(p);
    }

    m.complete(p, SyntaxKind::RetStmt)
}

fn var_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_set(&VAR_DEF_START));

    if p.at(TokenKind::Ident)
        && !matches!(p.peek_next(), Some(TokenKind::Equal))
    {
        // No attrs for you, young one
        return expr::expr(p).expect(
            "This'll never be none, the ident we're at is a valid lhs",
        );
    }

    let m = p.start();

    if p.at(TokenKind::Array) {
        array_decl(p);
        return m.complete(p, SyntaxKind::ArrayDef);
    }

    p.bump(); // First token

    /*  Weird, huh? Well actually no.
     * `const array xs[n]` is allowed syntax, so we
     * need to check for `array` as the second token
     * _as well as_ the first
     */
    if p.at(TokenKind::Array) {
        array_decl(p);
        return m.complete(p, SyntaxKind::ArrayDef);
    }

    if p.at(TokenKind::Ident) {
        // That wasn't the ident, bump it now
        p.bump();
    }

    p.expect(TokenKind::Equal);

    expr::expr(p);

    m.complete(p, SyntaxKind::VarDef)
}

fn array_decl(p: &mut Parser) {
    assert!(p.at(TokenKind::Array));
    p.bump(); // Get rid of that `array` token
    p.expect(TokenKind::Ident); // Arrays have a name
    let mut seen_dimensions = false;

    if p.at(TokenKind::LBracket) {
        seen_dimensions = true;
        p.bump();
        p.expect(TokenKind::Number);
        if p.at(TokenKind::Comma) {
            p.bump();
            p.expect(TokenKind::Number);
        }
        p.expect(TokenKind::RBracket);
    }

    if !seen_dimensions && p.at(TokenKind::Equal) {
        p.bump(); // `=` token
        expr::array_literal(p);
    }
}

pub(crate) fn array_assigment_fallback(
    p: &mut Parser,
    cm: CompletedMarker,
) -> CompletedMarker {
    let m = cm.precede(p);
    p.expect(TokenKind::Equal);
    expr::expr(p);
    m.complete(p, SyntaxKind::ArrayDef)
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
            p.bump();
        }
    }

    p.expect(TokenKind::RParen);

    while !p.at_end() && !p.at_set(&SUBPROG_END) {
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
            "x = 3",
            expect![[r#"
            Root@0..5
              VarDef@0..5
                Ident@0..1 "x"
                Whitespace@1..2 " "
                Equal@2..3 "="
                Whitespace@3..4 " "
                Literal@4..5
                  Number@4..5 "3""#]],
        );
    }

    #[test]
    fn parse_var_def_string() {
        check(
            r#"name = "Louise""#,
            expect![[r#"
            Root@0..15
              VarDef@0..15
                Ident@0..4 "name"
                Whitespace@4..5 " "
                Equal@5..6 "="
                Whitespace@6..7 " "
                Literal@7..15
                  String@7..15 "\"Louise\"""#]],
        );
    }

    #[test]
    fn parse_var_def_name_ref() {
        check(
            "x = y",
            expect![[r#"
        Root@0..5
          VarDef@0..5
            Ident@0..1 "x"
            Whitespace@1..2 " "
            Equal@2..3 "="
            Whitespace@3..4 " "
            NameRef@4..5
              Ident@4..5 "y""#]],
        );
    }

    #[test]
    fn parse_const_def() {
        check(
            "const vat = 0.2",
            expect![[r#"
            Root@0..15
              VarDef@0..15
                Const@0..5 "const"
                Whitespace@5..6 " "
                Ident@6..9 "vat"
                Whitespace@9..10 " "
                Equal@10..11 "="
                Whitespace@11..12 " "
                Literal@12..15
                  Number@12..15 "0.2""#]],
        );
    }

    #[test]
    fn parse_global_def() {
        check(
            r#"global userID = "Cust001""#,
            expect![[r#"
            Root@0..25
              VarDef@0..25
                Global@0..6 "global"
                Whitespace@6..7 " "
                Ident@7..13 "userID"
                Whitespace@13..14 " "
                Equal@14..15 "="
                Whitespace@15..16 " "
                Literal@16..25
                  String@16..25 "\"Cust001\"""#]],
        );
    }

    #[test]
    fn parse_array_decl() {
        check(
            "array colours[5]",
            expect![[r#"
                Root@0..16
                  ArrayDef@0..16
                    Array@0..5 "array"
                    Whitespace@5..6 " "
                    Ident@6..13 "colours"
                    LBracket@13..14 "["
                    Number@14..15 "5"
                    RBracket@15..16 "]""#]],
        );
    }

    #[test]
    fn parse_global_array_decl() {
        check(
            "global array colours[5]",
            expect![[r#"
            Root@0..23
              ArrayDef@0..23
                Global@0..6 "global"
                Whitespace@6..7 " "
                Array@7..12 "array"
                Whitespace@12..13 " "
                Ident@13..20 "colours"
                LBracket@20..21 "["
                Number@21..22 "5"
                RBracket@22..23 "]""#]],
        );
    }

    #[test]
    fn parse_const_array_decl() {
        check(
            "const array colours[5]",
            expect![[r#"
            Root@0..22
              ArrayDef@0..22
                Const@0..5 "const"
                Whitespace@5..6 " "
                Array@6..11 "array"
                Whitespace@11..12 " "
                Ident@12..19 "colours"
                LBracket@19..20 "["
                Number@20..21 "5"
                RBracket@21..22 "]""#]],
        );
    }

    #[test]
    fn parse_2d_array_decl() {
        check(
            "array gameboard[8,8]",
            expect![[r#"
                Root@0..20
                  ArrayDef@0..20
                    Array@0..5 "array"
                    Whitespace@5..6 " "
                    Ident@6..15 "gameboard"
                    LBracket@15..16 "["
                    Number@16..17 "8"
                    Comma@17..18 ","
                    Number@18..19 "8"
                    RBracket@19..20 "]""#]],
        );
    }

    #[test]
    fn parse_global_2d_array_decl() {
        check(
            "global array gameboard[8,8]",
            expect![[r#"
            Root@0..27
              ArrayDef@0..27
                Global@0..6 "global"
                Whitespace@6..7 " "
                Array@7..12 "array"
                Whitespace@12..13 " "
                Ident@13..22 "gameboard"
                LBracket@22..23 "["
                Number@23..24 "8"
                Comma@24..25 ","
                Number@25..26 "8"
                RBracket@26..27 "]""#]],
        );
    }

    #[test]
    fn parse_const_2d_array_decl() {
        check(
            "const array gameboard[8,8]",
            expect![[r#"
            Root@0..26
              ArrayDef@0..26
                Const@0..5 "const"
                Whitespace@5..6 " "
                Array@6..11 "array"
                Whitespace@11..12 " "
                Ident@12..21 "gameboard"
                LBracket@21..22 "["
                Number@22..23 "8"
                Comma@23..24 ","
                Number@24..25 "8"
                RBracket@25..26 "]""#]],
        );
    }

    #[test]
    fn parse_array_assignment() {
        check(
            r#"names[3] = "Noni""#,
            expect![[r#"
                Root@0..17
                  ArrayDef@0..17
                    IdentSubscript@0..9
                      NameRef@0..5
                        Ident@0..5 "names"
                      LBracket@5..6 "["
                      Literal@6..7
                        Number@6..7 "3"
                      RBracket@7..8 "]"
                      Whitespace@8..9 " "
                    Equal@9..10 "="
                    Whitespace@10..11 " "
                    Literal@11..17
                      String@11..17 "\"Noni\"""#]],
        );
    }

    #[test]
    fn parse_2d_array_assign() {
        check(
            r#"gameboard[1,0] = "Pawn""#,
            expect![[r#"
                Root@0..23
                  ArrayDef@0..23
                    IdentSubscript@0..15
                      NameRef@0..9
                        Ident@0..9 "gameboard"
                      LBracket@9..10 "["
                      Literal@10..11
                        Number@10..11 "1"
                      Comma@11..12 ","
                      Literal@12..13
                        Number@12..13 "0"
                      RBracket@13..14 "]"
                      Whitespace@14..15 " "
                    Equal@15..16 "="
                    Whitespace@16..17 " "
                    Literal@17..23
                      String@17..23 "\"Pawn\"""#]],
        );
    }

    #[test]
    fn parse_multi_index_array_assign() {
        check(
            "xs[1][0] = 42",
            expect![[r#"
            Root@0..13
              ArrayDef@0..13
                IdentSubscript@0..9
                  BinaryExpr@0..5
                    NameRef@0..2
                      Ident@0..2 "xs"
                    LBracket@2..3 "["
                    Literal@3..4
                      Number@3..4 "1"
                    RBracket@4..5 "]"
                  LBracket@5..6 "["
                  Literal@6..7
                    Number@6..7 "0"
                  RBracket@7..8 "]"
                  Whitespace@8..9 " "
                Equal@9..10 "="
                Whitespace@10..11 " "
                Literal@11..13
                  Number@11..13 "42""#]],
        );
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
        );
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
        );
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
        );
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
        );
    }

    #[test]
    fn parse_proc_early_return() {
        check(
            "procedure p()\nreturn\nendprocedure",
            expect![[r#"
            Root@0..33
              SubProgramDef@0..33
                Procedure@0..9 "procedure"
                Whitespace@9..10 " "
                Ident@10..11 "p"
                LParen@11..12 "("
                RParen@12..13 ")"
                Newline@13..14 "\n"
                RetStmt@14..21
                  Return@14..20 "return"
                  Newline@20..21 "\n"
                Endprocedure@21..33 "endprocedure""#]],
        );
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
                    PrimaryBody@28..45
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
                    ConditionExpr@52..67
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
                    ConditionalBody@76..91
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
                    OtherwiseBody@100..115
                      SubprogCall@100..115
                        NameRef@100..105
                          Ident@100..105 "print"
                        LParen@105..106 "("
                        Literal@106..113
                          String@106..113 "\"Error\""
                        RParen@113..114 ")"
                        Newline@114..115 "\n"
                    Endif@115..120 "endif""#]],
        );
    }

    #[test]
    fn parse_for() {
        check(
            r#"for i=0 to 9
    print("Loop")
next i"#,
            expect![[r#"
                Root@0..37
                  ForLoop@0..37
                    For@0..3 "for"
                    Whitespace@3..4 " "
                    Ident@4..5 "i"
                    Equal@5..6 "="
                    Literal@6..8
                      Number@6..7 "0"
                      Whitespace@7..8 " "
                    To@8..10 "to"
                    Whitespace@10..11 " "
                    Literal@11..17
                      Number@11..12 "9"
                      Newline@12..13 "\n"
                      Whitespace@13..17 "    "
                    LoopBody@17..37
                      SubprogCall@17..31
                        NameRef@17..22
                          Ident@17..22 "print"
                        LParen@22..23 "("
                        Literal@23..29
                          String@23..29 "\"Loop\""
                        RParen@29..30 ")"
                        Newline@30..31 "\n"
                      Next@31..35 "next"
                      Whitespace@35..36 " "
                      Ident@36..37 "i""#]],
        );
    }

    #[test]
    fn parse_for_with_step() {
        check(
            r#"for i=2 to 10 step 2
    print(i)
next i"#,
            expect![[r#"
                Root@0..40
                  ForLoop@0..40
                    For@0..3 "for"
                    Whitespace@3..4 " "
                    Ident@4..5 "i"
                    Equal@5..6 "="
                    Literal@6..8
                      Number@6..7 "2"
                      Whitespace@7..8 " "
                    To@8..10 "to"
                    Whitespace@10..11 " "
                    Literal@11..14
                      Number@11..13 "10"
                      Whitespace@13..14 " "
                    Step@14..18 "step"
                    Whitespace@18..19 " "
                    Literal@19..25
                      Number@19..20 "2"
                      Newline@20..21 "\n"
                      Whitespace@21..25 "    "
                    LoopBody@25..40
                      SubprogCall@25..34
                        NameRef@25..30
                          Ident@25..30 "print"
                        LParen@30..31 "("
                        NameRef@31..32
                          Ident@31..32 "i"
                        RParen@32..33 ")"
                        Newline@33..34 "\n"
                      Next@34..38 "next"
                      Whitespace@38..39 " "
                      Ident@39..40 "i""#]],
        );
    }

    #[test]
    fn parse_while_loop() {
        check(
            r#"while answer != "Correct"
    answer = input("New answer")
endwhile"#,
            expect![[r#"
                Root@0..67
                  WhileLoop@0..67
                    While@0..5 "while"
                    Whitespace@5..6 " "
                    BinaryExpr@6..30
                      NameRef@6..13
                        Ident@6..12 "answer"
                        Whitespace@12..13 " "
                      BangEqual@13..15 "!="
                      Whitespace@15..16 " "
                      Literal@16..30
                        String@16..25 "\"Correct\""
                        Newline@25..26 "\n"
                        Whitespace@26..30 "    "
                    LoopBody@30..59
                      VarDef@30..59
                        Ident@30..36 "answer"
                        Whitespace@36..37 " "
                        Equal@37..38 "="
                        Whitespace@38..39 " "
                        SubprogCall@39..59
                          NameRef@39..44
                            Ident@39..44 "input"
                          LParen@44..45 "("
                          Literal@45..57
                            String@45..57 "\"New answer\""
                          RParen@57..58 ")"
                          Newline@58..59 "\n"
                    Endwhile@59..67 "endwhile""#]],
        );
    }

    #[test]
    fn parse_do_until() {
        check(
            r#"do
    answer = input("New answer")
until answer == "Correct""#,
            expect![[r#"
                Root@0..61
                  DoUntil@0..61
                    Do@0..2 "do"
                    Newline@2..3 "\n"
                    Whitespace@3..7 "    "
                    LoopBody@7..36
                      VarDef@7..36
                        Ident@7..13 "answer"
                        Whitespace@13..14 " "
                        Equal@14..15 "="
                        Whitespace@15..16 " "
                        SubprogCall@16..36
                          NameRef@16..21
                            Ident@16..21 "input"
                          LParen@21..22 "("
                          Literal@22..34
                            String@22..34 "\"New answer\""
                          RParen@34..35 ")"
                          Newline@35..36 "\n"
                    Until@36..41 "until"
                    Whitespace@41..42 " "
                    BinaryExpr@42..61
                      NameRef@42..49
                        Ident@42..48 "answer"
                        Whitespace@48..49 " "
                      EqualEqual@49..51 "=="
                      Whitespace@51..52 " "
                      Literal@52..61
                        String@52..61 "\"Correct\"""#]],
        );
    }

    #[test]
    fn parse_switch_stmt() {
        check(
            r#"switch day:
    case "Sat":
        print("Saturday")
    case "Sun":
        print("Sunday")
    default:
        print("Weekday")
endswitch"#,
            expect![[r#"
                Root@0..141
                  SwitchStmt@0..141
                    Switch@0..6 "switch"
                    Whitespace@6..7 " "
                    NameRef@7..10
                      Ident@7..10 "day"
                    Colon@10..11 ":"
                    Newline@11..12 "\n"
                    Whitespace@12..16 "    "
                    Case@16..20 "case"
                    Whitespace@20..21 " "
                    ConditionExpr@21..26
                      Literal@21..26
                        String@21..26 "\"Sat\""
                    Colon@26..27 ":"
                    Newline@27..28 "\n"
                    Whitespace@28..36 "        "
                    ConditionalBody@36..58
                      SubprogCall@36..58
                        NameRef@36..41
                          Ident@36..41 "print"
                        LParen@41..42 "("
                        Literal@42..52
                          String@42..52 "\"Saturday\""
                        RParen@52..53 ")"
                        Newline@53..54 "\n"
                        Whitespace@54..58 "    "
                    Case@58..62 "case"
                    Whitespace@62..63 " "
                    ConditionExpr@63..68
                      Literal@63..68
                        String@63..68 "\"Sun\""
                    Colon@68..69 ":"
                    Newline@69..70 "\n"
                    Whitespace@70..78 "        "
                    ConditionalBody@78..98
                      SubprogCall@78..98
                        NameRef@78..83
                          Ident@78..83 "print"
                        LParen@83..84 "("
                        Literal@84..92
                          String@84..92 "\"Sunday\""
                        RParen@92..93 ")"
                        Newline@93..94 "\n"
                        Whitespace@94..98 "    "
                    Default@98..105 "default"
                    Colon@105..106 ":"
                    Newline@106..107 "\n"
                    Whitespace@107..115 "        "
                    OtherwiseBody@115..132
                      SubprogCall@115..132
                        NameRef@115..120
                          Ident@115..120 "print"
                        LParen@120..121 "("
                        Literal@121..130
                          String@121..130 "\"Weekday\""
                        RParen@130..131 ")"
                        Newline@131..132 "\n"
                    Endswitch@132..141 "endswitch""#]],
        );
    }

    #[test]
    fn parse_import_stmt() {
        check(
            "import \"file.ocr\"",
            expect![[r#"
                Root@0..17
                  Import@0..17
                    Import@0..6 "import"
                    Whitespace@6..7 " "
                    String@7..17 "\"file.ocr\"""#]],
        );
    }

    #[test]
    fn parse_many_import_stmt() {
        check(
            r#"import"foo"
            import"bar""#,
            expect![[r#"
                Root@0..35
                  Import@0..24
                    Import@0..6 "import"
                    String@6..11 "\"foo\""
                    Newline@11..12 "\n"
                    Whitespace@12..24 "            "
                  Import@24..35
                    Import@24..30 "import"
                    String@30..35 "\"bar\"""#]],
        );
    }
}
