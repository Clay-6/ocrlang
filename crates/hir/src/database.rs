use std::iter;

use la_arena::Arena;
use syntax::SyntaxKind;
use text_size::TextRange;

use crate::{
    BinaryOp, Expr, ExprIdx, ExprKind, ExprRange, Literal, Stmt, StmtKind,
    UnaryOp, VarDefKind,
};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Database {
    exprs: Arena<Expr>,
}

impl Database {
    pub(crate) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Stmt> {
        Some(Stmt {
            range: ast.text_range(),
            kind: match ast {
                ast::Stmt::VarDef(ast) => self.lower_var_def(ast)?,
                ast::Stmt::ArrayDef(ast) => self.lower_array_def(ast)?,
                ast::Stmt::SubprogDef(ast) => self.lower_subprogram_def(ast)?,
                ast::Stmt::RetStmt(ast) => self.lower_return_stmt(ast),
                ast::Stmt::IfElse(ast) => self.lower_if_else(ast),
                ast::Stmt::SwitchCase(ast) => self.lower_switch_case(ast),
                ast::Stmt::ForLoop(ast) => self.lower_for_loop(ast),
                ast::Stmt::WhileLoop(ast) => self.lower_while_loop(ast),
                ast::Stmt::DoUntil(ast) => self.lower_do_until(ast),
                ast::Stmt::Expr(ast) => {
                    StmtKind::Expr(self.lower_expr(Some(ast)))
                }
            },
        })
    }

    pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
        ast.map_or_else(Expr::default, |ast| Expr {
            range: ast.text_range(),
            kind: match ast {
                ast::Expr::Binary(ast) => self.lower_binary(ast),
                ast::Expr::Unary(ast) => self.lower_unary(ast),
                ast::Expr::Literal(ast) => Self::lower_literal(ast),
                ast::Expr::ArrayLiteral(ast) => self.lower_array_literal(ast),
                ast::Expr::Paren(ast) => return self.lower_expr(ast.expr()),
                ast::Expr::NameRef(ast) => Self::lower_name_ref(ast),
                ast::Expr::Call(ast) => self.lower_call(ast),
            },
        })
    }

    #[must_use]
    pub fn get(&self, idx: ExprIdx) -> &Expr {
        &self.exprs[idx]
    }

    #[must_use]
    pub fn get_range(&self, range: ExprRange) -> &[Expr] {
        &self.exprs[range]
    }

    fn lower_do_until(&mut self, ast: ast::DoUntil) -> StmtKind {
        let body = ast
            .body()
            .map(|b| b.filter_map(|ast| self.lower_stmt(ast)).collect())
            .unwrap_or_default();
        let condition = self.lower_expr(ast.condition());
        StmtKind::DoUntilLoop {
            condition: self.exprs.alloc(condition),
            body,
        }
    }

    fn lower_while_loop(&mut self, ast: ast::WhileLoop) -> StmtKind {
        let condition = {
            let expr = self.lower_expr(ast.condition());
            self.exprs.alloc(expr)
        };
        let body = ast
            .body()
            .map(|b| b.filter_map(|ast| self.lower_stmt(ast)).collect())
            .unwrap_or_default();
        StmtKind::WhileLoop { condition, body }
    }

    fn lower_for_loop(&mut self, ast: ast::ForLoop) -> StmtKind {
        let loop_var = ast.loop_var().map(|i| i.text().into());
        let (start, end) = {
            let tmp = ast.bounds().map_or(Default::default(), |(s, e)| {
                (self.lower_expr(Some(s)), self.lower_expr(Some(e)))
            });
            (self.exprs.alloc(tmp.0), self.exprs.alloc(tmp.1))
        };
        let step = {
            let tmp = ast
                .step()
                .map_or(Expr::default(), |ast| self.lower_expr(Some(ast)));
            self.exprs.alloc(tmp)
        };
        let body = ast
            .body()
            .map(|b| b.filter_map(|ast| self.lower_stmt(ast)).collect())
            .unwrap_or_default();
        StmtKind::ForLoop {
            loop_var,
            start,
            end,
            step,
            body,
        }
    }

    fn lower_switch_case(&mut self, ast: ast::SwitchCase) -> StmtKind {
        let scrutinee = {
            let e = self.lower_expr(ast.scrutinee());
            self.exprs.alloc(e)
        };
        let cases = {
            let es = ast
                .cases()
                .map(|ast| self.lower_expr(ast))
                .collect::<Vec<_>>();
            self.exprs.alloc_many(es)
        };
        let case_bodies = ast
            .case_bodies()
            .map(|case| {
                case.filter_map(|ast| self.lower_stmt(ast))
                    .collect::<Vec<_>>()
            })
            .collect();
        let default_body = ast
            .default_body()
            .map(|b| {
                b.filter_map(|ast| self.lower_stmt(ast)).collect::<Vec<_>>()
            })
            .unwrap_or_default();
        StmtKind::SwitchCase {
            scrutinee,
            cases,
            case_bodies,
            default_body,
        }
    }

    fn lower_if_else(&mut self, ast: ast::IfElse) -> StmtKind {
        let condition = {
            let e = self.lower_expr(ast.condition());
            self.exprs.alloc(e)
        };
        let body = ast
            .body()
            .map(|b| b.filter_map(|ast| self.lower_stmt(ast)).collect())
            .unwrap_or_default();
        let elseifs = {
            let elseif_conds = ast
                .elseif_conditions()
                .map(|ast| self.lower_expr(ast))
                .collect::<Vec<_>>();
            let elseif_bodies = ast
                .elseif_bodies()
                .map(|body| {
                    body.filter_map(|ast| self.lower_stmt(ast)).collect()
                })
                .collect::<Vec<Vec<_>>>();
            elseif_conds
                .into_iter()
                .map(|e| self.exprs.alloc(e))
                .zip(elseif_bodies)
                .collect::<Vec<_>>()
        };
        let else_body = ast
            .else_body()
            .map(|b| b.filter_map(|ast| self.lower_stmt(ast)).collect())
            .unwrap_or_default();

        StmtKind::IfElse {
            condition,
            body,
            elseifs,
            else_body,
        }
    }

    fn lower_return_stmt(&mut self, ast: ast::RetStmt) -> StmtKind {
        let value = self.lower_expr(ast.value());
        StmtKind::ReturnStmt { value }
    }

    fn lower_subprogram_def(
        &mut self,
        ast: ast::SubprogDef,
    ) -> Option<StmtKind> {
        let body = {
            match ast.kind()?.kind() {
                SyntaxKind::Function => {
                    ast.body().filter_map(|ast| self.lower_stmt(ast)).collect()
                }
                SyntaxKind::Procedure => ast
                    .body()
                    .filter_map(|ast| self.lower_stmt(ast))
                    .chain(iter::once(Stmt {
                        range: TextRange::default(),
                        kind: StmtKind::ReturnStmt {
                            value: Expr::default(),
                        },
                    }))
                    .collect(),
                _ => unreachable!(),
            }
        };
        Some(StmtKind::SubprogramDef {
            name: ast.name()?.text().into(),
            params: ast.params().map(|ast| ast.text().into()).collect(),
            body,
        })
    }

    fn lower_var_def(&mut self, ast: ast::VarDef) -> Option<StmtKind> {
        Some(StmtKind::VarDef {
            name: ast.name()?.text().into(),
            kind: ast.kind().map_or(VarDefKind::Standard, |k| match k.kind() {
                SyntaxKind::Const => VarDefKind::Constant,
                SyntaxKind::Global => VarDefKind::Global,
                _ => unreachable!(),
            }),
            value: self.lower_expr(ast.value()),
        })
    }

    fn lower_array_def(&mut self, ast: ast::ArrayDef) -> Option<StmtKind> {
        let subscript = {
            if let Some(mut subs) = ast.subscript() {
                let first = self.lower_expr(subs.next());
                let last = self.lower_expr(subs.next());
                (first, last)
            } else {
                (Expr::default(), Expr::default())
            }
        };
        let dimensions = {
            let mut dims = ast.dimensions();
            let outer = self.lower_expr(dims.next());
            let inner = self.lower_expr(dims.next());

            (outer, inner)
        };

        Some(StmtKind::ArrayDef {
            name: ast.name()?.text().into(),
            kind: ast.kind().map_or(VarDefKind::Standard, |k| match k.kind() {
                SyntaxKind::Const => VarDefKind::Constant,
                SyntaxKind::Global => VarDefKind::Global,
                _ => unreachable!(),
            }),
            subscript,
            dimensions,
            value: self.lower_expr(ast.value()),
        })
    }

    fn lower_binary(&mut self, ast: ast::BinaryExpr) -> ExprKind {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Minus => BinaryOp::Sub,
            SyntaxKind::Star => BinaryOp::Mul,
            SyntaxKind::Slash => BinaryOp::Div,
            SyntaxKind::Mod => BinaryOp::Mod,
            SyntaxKind::Div => BinaryOp::Quot,
            SyntaxKind::And => BinaryOp::And,
            SyntaxKind::Or => BinaryOp::Or,
            SyntaxKind::Caret => BinaryOp::Pow,
            SyntaxKind::Dot => BinaryOp::Dot,
            SyntaxKind::EqualEqual => BinaryOp::Equals,
            SyntaxKind::BangEqual => BinaryOp::NotEquals,
            SyntaxKind::Greater => BinaryOp::GreaterThan,
            SyntaxKind::GreaterEqual => BinaryOp::GreaterEquals,
            SyntaxKind::Less => BinaryOp::LessThan,
            SyntaxKind::LessEqual => BinaryOp::LessEquals,
            SyntaxKind::LBracket => BinaryOp::SubScript,
            _ => unreachable!(),
        };

        let lhs = {
            let e = self.lower_expr(ast.lhs());
            self.exprs.alloc(e)
        };
        let rhs = {
            let e = self.lower_expr(ast.rhs());
            self.exprs.alloc(e)
        };

        ExprKind::Binary { lhs, rhs, op }
    }

    fn lower_unary(&mut self, ast: ast::UnaryExpr) -> ExprKind {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Not => UnaryOp::Not,
            SyntaxKind::Minus => UnaryOp::Neg,
            _ => unreachable!(),
        };

        let opand = self.lower_expr(ast.expr());

        ExprKind::Unary {
            op,
            opand: self.exprs.alloc(opand),
        }
    }

    fn lower_literal(ast: ast::Literal) -> ExprKind {
        match ast.parse() {
            Some(v) => match v {
                ast::Val::Int(i) => ExprKind::Literal {
                    value: Literal::Int(i),
                },
                ast::Val::Float(f) => ExprKind::Literal {
                    value: Literal::Float(f),
                },
                ast::Val::Char(c) => ExprKind::Literal {
                    value: Literal::Char(c),
                },
                ast::Val::String(s) => ExprKind::Literal {
                    value: Literal::String(s.trim_matches('"').into()),
                },
                ast::Val::Bool(b) => ExprKind::Literal {
                    value: Literal::Bool(b),
                },
            },
            None => ExprKind::Missing,
        }
    }

    fn lower_array_literal(&mut self, ast: ast::ArrayLiteral) -> ExprKind {
        match ast.items() {
            Some(v) => {
                let vals = v
                    .into_iter()
                    .map(|i| self.lower_expr(Some(i)))
                    .collect::<Vec<_>>();
                ExprKind::Literal {
                    value: Literal::Array(self.exprs.alloc_many(vals)),
                }
            }
            None => ExprKind::Missing,
        }
    }

    fn lower_name_ref(ast: ast::NameRef) -> ExprKind {
        ExprKind::NameRef {
            name: ast.name().unwrap().text().into(),
        }
    }

    fn lower_call(&mut self, ast: ast::SubprogCall) -> ExprKind {
        let callee = ast.callee().unwrap().text().into();
        let args = ast
            .args()
            .map(|e| self.lower_expr(Some(e)))
            .collect::<Vec<_>>();

        ExprKind::Call {
            callee,
            args: self.exprs.alloc_many(args),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use text_size::TextSize;

    fn parse(input: &str) -> ast::Root {
        ast::Root::cast(parser::parse(input).unwrap().syntax()).unwrap()
    }

    fn check_stmt(input: &str, expected_hir: StmtKind) {
        let root = parse(input);
        let ast = root.stmts().next().unwrap();
        let hir = Database::default().lower_stmt(ast).unwrap();

        assert_eq!(hir.kind, expected_hir);
    }

    fn check_expr(
        input: &str,
        expected_hir: ExprKind,
        expected_database: Database,
    ) {
        let root = parse(input);
        let first_stmt = root.stmts().next().unwrap();
        let ast::Stmt::Expr(ast) = first_stmt else {
            unreachable!()
        };
        let mut database = Database::default();
        let hir = database.lower_expr(Some(ast));

        assert_eq!(hir.kind, expected_hir);
        assert_eq!(database, expected_database);
    }

    #[test]
    fn lower_variable_def() {
        check_stmt(
            "foo = bar",
            StmtKind::VarDef {
                name: "foo".into(),
                kind: VarDefKind::Standard,
                value: Expr {
                    kind: ExprKind::NameRef { name: "bar".into() },
                    range: TextRange::new(TextSize::new(6), TextSize::new(9)),
                },
            },
        );
    }

    #[test]
    fn lower_variable_def_without_value() {
        check_stmt(
            "a = ",
            StmtKind::VarDef {
                name: "a".into(),
                kind: VarDefKind::Standard,
                value: Expr {
                    kind: ExprKind::Missing,
                    range: TextRange::default(),
                },
            },
        );
    }

    #[test]
    fn lower_const_variable_def() {
        check_stmt(
            "const a = 15",
            StmtKind::VarDef {
                name: "a".into(),
                kind: VarDefKind::Constant,
                value: Expr {
                    kind: ExprKind::Literal {
                        value: Literal::Int(15),
                    },
                    range: TextRange::new(TextSize::new(10), TextSize::new(12)),
                },
            },
        );
    }

    #[test]
    fn lower_global_variable_def() {
        check_stmt(
            "global COUNTER = 0",
            StmtKind::VarDef {
                name: "COUNTER".into(),
                kind: VarDefKind::Global,
                value: Expr {
                    kind: ExprKind::Literal {
                        value: Literal::Int(0),
                    },
                    range: TextRange::new(TextSize::new(17), TextSize::new(18)),
                },
            },
        );
    }

    #[test]
    fn lower_array_decl() {
        check_stmt(
            "const array nums[5]",
            StmtKind::ArrayDef {
                name: "nums".into(),
                kind: VarDefKind::Constant,
                subscript: (Expr::default(), Expr::default()),
                dimensions: (
                    Expr {
                        kind: ExprKind::Literal {
                            value: Literal::Int(5),
                        },
                        range: TextRange::new(
                            TextSize::new(17),
                            TextSize::new(18),
                        ),
                    },
                    Expr::default(),
                ),
                value: Expr::default(),
            },
        );
    }

    #[test]
    fn lower_array_def() {
        let mut exprs = Arena::new();
        let nums = exprs.alloc_many([
            Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(1),
                },
                range: TextRange::default(),
            },
            Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(2),
                },
                range: TextRange::default(),
            },
            Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(3),
                },
                range: TextRange::default(),
            },
        ]);
        check_stmt(
            "array nums = [1, 2, 3]",
            StmtKind::ArrayDef {
                name: "nums".into(),
                kind: VarDefKind::Standard,
                subscript: (Expr::default(), Expr::default()),
                dimensions: (Expr::default(), Expr::default()),
                value: Expr {
                    kind: ExprKind::Literal {
                        value: Literal::Array(nums),
                    },
                    range: TextRange::new(TextSize::new(13), TextSize::new(22)),
                },
            },
        );
    }

    #[test]
    fn lower_2d_array_decl() {
        check_stmt(
            "global array board[8,8]",
            StmtKind::ArrayDef {
                name: "board".into(),
                kind: VarDefKind::Global,
                subscript: (Expr::default(), Expr::default()),
                dimensions: (
                    Expr {
                        kind: ExprKind::Literal {
                            value: Literal::Int(8),
                        },
                        range: TextRange::new(
                            TextSize::new(19),
                            TextSize::new(20),
                        ),
                    },
                    Expr {
                        kind: ExprKind::Literal {
                            value: Literal::Int(8),
                        },
                        range: TextRange::new(
                            TextSize::new(21),
                            TextSize::new(22),
                        ),
                    },
                ),
                value: Expr::default(),
            },
        );
    }

    #[test]
    fn lower_array_subscript_assign() {
        check_stmt(
            "arr[5] = 5",
            StmtKind::ArrayDef {
                name: "arr".into(),
                kind: VarDefKind::Standard,
                subscript: (
                    Expr {
                        kind: ExprKind::Literal {
                            value: Literal::Int(5),
                        },
                        range: TextRange::new(
                            TextSize::new(4),
                            TextSize::new(5),
                        ),
                    },
                    Expr::default(),
                ),
                dimensions: (Expr::default(), Expr::default()),
                value: Expr {
                    kind: ExprKind::Literal {
                        value: Literal::Int(5),
                    },
                    range: TextRange::new(TextSize::new(9), TextSize::new(10)),
                },
            },
        );
    }

    #[test]
    fn lower_2d_array_subscript_assign() {
        check_stmt(
            "arr[3,4] = 5",
            StmtKind::ArrayDef {
                name: "arr".into(),
                kind: VarDefKind::Standard,
                subscript: (
                    Expr {
                        kind: ExprKind::Literal {
                            value: Literal::Int(3),
                        },
                        range: TextRange::new(
                            TextSize::new(4),
                            TextSize::new(5),
                        ),
                    },
                    Expr {
                        kind: ExprKind::Literal {
                            value: Literal::Int(4),
                        },
                        range: TextRange::new(
                            TextSize::new(6),
                            TextSize::new(7),
                        ),
                    },
                ),
                dimensions: (Expr::default(), Expr::default()),
                value: Expr {
                    kind: ExprKind::Literal {
                        value: Literal::Int(5),
                    },
                    range: TextRange::new(TextSize::new(11), TextSize::new(12)),
                },
            },
        );
    }

    #[test]
    fn lower_expr_stmt() {
        check_stmt(
            "123",
            StmtKind::Expr(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(123),
                },
                range: TextRange::new(TextSize::new(0), TextSize::new(3)),
            }),
        );
    }

    #[test]
    fn lower_binary_expr() {
        let mut exprs = Arena::new();
        let lhs = exprs.alloc(Expr {
            kind: ExprKind::Literal {
                value: Literal::Int(5),
            },
            range: TextRange::new(TextSize::new(0), TextSize::new(2)),
        });
        let rhs = exprs.alloc(Expr {
            kind: ExprKind::Literal {
                value: Literal::Int(10),
            },
            range: TextRange::new(TextSize::new(4), TextSize::new(6)),
        });

        check_expr(
            "5 + 10",
            ExprKind::Binary {
                op: BinaryOp::Add,
                lhs,
                rhs,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_binary_expr_without_rhs() {
        let mut exprs = Arena::new();
        let lhs = exprs.alloc(Expr {
            kind: ExprKind::Literal {
                value: Literal::Float(17.0),
            },
            range: TextRange::new(TextSize::new(0), TextSize::new(5)),
        });
        let rhs = exprs.alloc(Expr::default());

        check_expr(
            "17.0 * ",
            ExprKind::Binary {
                op: BinaryOp::Mul,
                lhs,
                rhs,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_num_literal() {
        check_expr(
            "99",
            ExprKind::Literal {
                value: Literal::Int(99),
            },
            Database::default(),
        );
        check_expr(
            "1.5",
            ExprKind::Literal {
                value: Literal::Float(1.5),
            },
            Database::default(),
        );
    }

    #[test]
    fn lower_string_literal() {
        check_expr(
            r#""hello""#,
            ExprKind::Literal {
                value: Literal::String("hello".into()),
            },
            Database::default(),
        );
    }

    #[test]
    fn lower_char_literal() {
        check_expr(
            "'c'",
            ExprKind::Literal {
                value: Literal::Char('c'),
            },
            Database::default(),
        );
    }

    #[test]
    fn lower_bool_literal() {
        check_expr(
            "true",
            ExprKind::Literal {
                value: Literal::Bool(true),
            },
            Database::default(),
        );
        check_expr(
            "false",
            ExprKind::Literal {
                value: Literal::Bool(false),
            },
            Database::default(),
        );
    }

    #[test]
    fn lower_array_literal() {
        let mut exprs = Arena::new();
        let elems = [
            ExprKind::Literal {
                value: Literal::Int(1),
            },
            ExprKind::Literal {
                value: Literal::Int(2),
            },
            ExprKind::Literal {
                value: Literal::Int(3),
            },
            ExprKind::Literal {
                value: Literal::Int(4),
            },
            ExprKind::Literal {
                value: Literal::Int(5),
            },
        ];
        check_expr(
            "[1, 2, 3, 4, 5]",
            ExprKind::Literal {
                value: Literal::Array(exprs.alloc_many(
                    elems.into_iter().enumerate().map(|(i, e)| Expr {
                        kind: e,
                        range: TextRange::new(
                            TextSize::new(3 * i as u32 + 1),
                            TextSize::new(3 * i as u32 + 2),
                        ),
                    }),
                )),
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_paren_expr() {
        check_expr(
            "(((((x)))))",
            ExprKind::NameRef { name: "x".into() },
            Database::default(),
        );
    }

    #[test]
    fn lower_unary_expr() {
        let mut exprs = Arena::new();
        let five = exprs.alloc(Expr {
            kind: ExprKind::Literal {
                value: Literal::Int(5),
            },
            range: TextRange::new(TextSize::new(1), TextSize::new(2)),
        });
        check_expr(
            "-5",
            ExprKind::Unary {
                op: UnaryOp::Neg,
                opand: five,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_unary_expr_without_expr() {
        let mut exprs = Arena::new();
        let opand = exprs.alloc(Expr::default());

        check_expr(
            "NOT ",
            ExprKind::Unary {
                op: UnaryOp::Not,
                opand,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_name_ref() {
        check_expr(
            "x",
            ExprKind::NameRef { name: "x".into() },
            Database::default(),
        );
    }

    #[test]
    fn lower_subprog_call() {
        let mut exprs = Arena::default();
        let arg = exprs.alloc_many([Expr {
            kind: ExprKind::NameRef { name: "x".into() },
            range: TextRange::new(TextSize::new(6), TextSize::new(7)),
        }]);
        check_expr(
            "print(x)",
            ExprKind::Call {
                callee: "print".into(),
                args: arg,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_func_def() {
        let value = Expr {
            kind: ExprKind::NameRef { name: "x".into() },
            range: TextRange::new(TextSize::new(22), TextSize::new(24)),
        };
        check_stmt(
            "function id(x) return x endfunction",
            StmtKind::SubprogramDef {
                name: "id".into(),
                params: vec!["x".into()],
                body: vec![Stmt {
                    range: TextRange::new(TextSize::new(15), TextSize::new(24)),
                    kind: StmtKind::ReturnStmt { value },
                }],
            },
        );
    }

    #[test]
    fn lower_proc_def() {
        let mut exprs = Arena::new();
        let arg = exprs.alloc_many([Expr {
            kind: ExprKind::Literal {
                value: Literal::String("something".into()),
            },
            range: TextRange::default(),
        }]);
        check_stmt(
            r#"procedure thing() print("something") endprocedure"#,
            StmtKind::SubprogramDef {
                name: "thing".into(),
                params: vec![],
                body: vec![
                    Stmt {
                        range: TextRange::new(
                            TextSize::new(18),
                            TextSize::new(37),
                        ),
                        kind: StmtKind::Expr(Expr {
                            kind: ExprKind::Call {
                                callee: "print".into(),
                                args: arg,
                            },
                            range: TextRange::new(
                                TextSize::new(18),
                                TextSize::new(37),
                            ),
                        }),
                    },
                    Stmt {
                        range: TextRange::empty(TextSize::new(0)),
                        kind: StmtKind::ReturnStmt {
                            value: Expr::default(),
                        },
                    },
                ],
            },
        );
    }

    #[test]
    fn lower_for_loop() {
        let mut exprs = Arena::new();
        let (start, end) = (
            exprs.alloc(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(1),
                },
                range: TextRange::default(),
            }),
            exprs.alloc(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(10),
                },
                range: TextRange::default(),
            }),
        );
        let step = exprs.alloc(Expr::default());
        let print_arg = exprs.alloc_many([Expr {
            kind: ExprKind::NameRef { name: "i".into() },
            range: TextRange::default(),
        }]);
        check_stmt(
            "for i = 1 to 10 print(i) next i",
            StmtKind::ForLoop {
                loop_var: Some("i".into()),
                start,
                end,
                step,
                body: vec![Stmt {
                    range: TextRange::new(TextSize::new(16), TextSize::new(25)),
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::Call {
                            callee: "print".into(),
                            args: print_arg,
                        },
                        range: TextRange::new(
                            TextSize::new(16),
                            TextSize::new(25),
                        ),
                    }),
                }],
            },
        );
    }

    #[test]
    fn lower_for_loop_with_step() {
        let mut exprs = Arena::new();
        let (start, end) = (
            exprs.alloc(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(1),
                },
                range: TextRange::default(),
            }),
            exprs.alloc(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(10),
                },
                range: TextRange::default(),
            }),
        );
        let step = exprs.alloc(Expr {
            kind: ExprKind::Literal {
                value: Literal::Int(2),
            },
            range: TextRange::default(),
        });
        let print_arg = exprs.alloc_many([Expr {
            kind: ExprKind::NameRef { name: "i".into() },
            range: TextRange::default(),
        }]);
        check_stmt(
            "for i = 1 to 10 step 2 print(i) next i",
            StmtKind::ForLoop {
                loop_var: Some("i".into()),
                start,
                end,
                step,
                body: vec![Stmt {
                    range: TextRange::new(TextSize::new(23), TextSize::new(32)),
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::Call {
                            callee: "print".into(),
                            args: print_arg,
                        },
                        range: TextRange::new(
                            TextSize::new(23),
                            TextSize::new(32),
                        ),
                    }),
                }],
            },
        );
    }

    #[test]
    fn lower_while_loop() {
        let mut exprs = Arena::new();
        let condition = {
            let lhs = exprs.alloc(Expr {
                kind: ExprKind::NameRef {
                    name: "answer".into(),
                },
                range: TextRange::default(),
            });
            let rhs = exprs.alloc(Expr {
                kind: ExprKind::Literal {
                    value: Literal::String("Correct".into()),
                },
                range: TextRange::default(),
            });
            exprs.alloc(Expr {
                kind: ExprKind::Binary {
                    op: BinaryOp::NotEquals,
                    lhs,
                    rhs,
                },
                range: TextRange::default(),
            })
        };
        let body = {
            vec![Stmt {
                range: TextRange::new(TextSize::new(26), TextSize::new(30)),
                kind: StmtKind::Expr(Expr {
                    kind: ExprKind::Call {
                        callee: "x".into(),
                        args: exprs.alloc_many([]),
                    },
                    range: TextRange::new(TextSize::new(26), TextSize::new(30)),
                }),
            }]
        };
        check_stmt(
            r#"while answer != "Correct" x() endwhile"#,
            StmtKind::WhileLoop { condition, body },
        );
    }

    #[test]
    fn lower_do_until() {
        let mut exprs = Arena::new();
        let body = vec![Stmt {
            range: TextRange::new(TextSize::new(3), TextSize::new(11)),
            kind: StmtKind::Expr(Expr {
                kind: ExprKind::Call {
                    callee: "thing".into(),
                    args: exprs.alloc_many([]),
                },
                range: TextRange::new(TextSize::new(3), TextSize::new(11)),
            }),
        }];
        let condition = exprs.alloc(Expr {
            kind: ExprKind::NameRef {
                name: "condition".into(),
            },
            range: TextRange::default(),
        });
        check_stmt(
            "do thing() until condition",
            StmtKind::DoUntilLoop { condition, body },
        );
    }

    #[test]
    fn lower_if_elseif_else() {
        let mut exprs = Arena::new();
        let condition = {
            let lhs = exprs.alloc(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Float(2.5),
                },
                range: TextRange::default(),
            });
            let rhs = exprs.alloc(Expr {
                kind: ExprKind::NameRef { name: "n".into() },
                range: TextRange::default(),
            });

            exprs.alloc(Expr {
                kind: ExprKind::Binary {
                    op: BinaryOp::LessThan,
                    lhs,
                    rhs,
                },
                range: TextRange::default(),
            })
        };
        let elseifs = {
            let lhs = exprs.alloc(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Float(2.5),
                },
                range: TextRange::default(),
            });
            let rhs = exprs.alloc(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Float(2.5),
                },
                range: TextRange::default(),
            });
            let conditions = vec![Expr {
                kind: ExprKind::Binary {
                    op: BinaryOp::GreaterEquals,
                    lhs,
                    rhs,
                },
                range: TextRange::default(),
            }];
            let bodies = vec![vec![Stmt {
                range: TextRange::new(TextSize::new(41), TextSize::new(43)),
                kind: StmtKind::Expr(Expr {
                    kind: ExprKind::Literal {
                        value: Literal::Int(2),
                    },
                    range: TextRange::new(TextSize::new(41), TextSize::new(43)),
                }),
            }]];

            conditions
                .into_iter()
                .map(|e| exprs.alloc(e))
                .zip(bodies)
                .collect()
        };
        let body = vec![Stmt {
            range: TextRange::new(TextSize::new(16), TextSize::new(18)),
            kind: StmtKind::Expr(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(5),
                },
                range: TextRange::new(TextSize::new(16), TextSize::new(18)),
            }),
        }];
        let else_body = vec![Stmt {
            range: TextRange::new(TextSize::new(48), TextSize::new(50)),
            kind: StmtKind::Expr(Expr {
                kind: ExprKind::Literal {
                    value: Literal::Int(7),
                },
                range: TextRange::new(TextSize::new(48), TextSize::new(50)),
            }),
        }];
        check_stmt(
            "if 2.5 < n then 5 elseif 2.5 >= 2.5 then 2 else 7 endif",
            StmtKind::IfElse {
                condition,
                body,
                elseifs,
                else_body,
            },
        );
    }

    #[test]
    fn lower_switch_stmt() {
        let mut exprs = Arena::new();
        let scrutinee = exprs.alloc(Expr {
            kind: ExprKind::NameRef { name: "c".into() },
            range: TextRange::default(),
        });
        let cases = exprs.alloc_many([
            Expr {
                kind: ExprKind::Literal {
                    value: Literal::Char('a'),
                },
                range: TextRange::default(),
            },
            Expr {
                kind: ExprKind::Literal {
                    value: Literal::Char('b'),
                },
                range: TextRange::default(),
            },
        ]);
        let case_bodies = vec![
            vec![Stmt {
                range: TextRange::new(TextSize::new(20), TextSize::new(24)),
                kind: StmtKind::Expr(Expr {
                    kind: ExprKind::Call {
                        callee: "a".into(),
                        args: exprs.alloc_many(std::iter::empty()),
                    },
                    range: TextRange::new(TextSize::new(20), TextSize::new(24)),
                }),
            }],
            vec![Stmt {
                range: TextRange::new(TextSize::new(34), TextSize::new(38)),
                kind: StmtKind::Expr(Expr {
                    kind: ExprKind::Call {
                        callee: "b".into(),
                        args: exprs.alloc_many(std::iter::empty()),
                    },
                    range: TextRange::new(TextSize::new(34), TextSize::new(38)),
                }),
            }],
        ];
        check_stmt(
            "switch c: case 'a': a() case 'b': b() default: d() endswitch",
            StmtKind::SwitchCase {
                scrutinee,
                cases,
                case_bodies,
                default_body: vec![Stmt {
                    range: TextRange::new(TextSize::new(47), TextSize::new(51)),
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::Call {
                            callee: "d".into(),
                            args: exprs.alloc_many([]),
                        },
                        range: TextRange::new(
                            TextSize::new(47),
                            TextSize::new(51),
                        ),
                    }),
                }],
            },
        );
    }
}
