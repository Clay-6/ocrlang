use la_arena::Arena;
use syntax::SyntaxKind;

use crate::{
    BinaryOp, Expr, ExprIdx, ExprRange, Literal, Stmt, SubprogramKind, UnaryOp, VarDefKind,
};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Database {
    exprs: Arena<Expr>,
}

impl Database {
    pub(crate) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Stmt> {
        Some(match ast {
            ast::Stmt::VarDef(ast) => self.lower_var_def(ast)?,
            ast::Stmt::ArrayDef(ast) => self.lower_array_def(ast)?,
            ast::Stmt::SubprogDef(ast) => self.lower_subprogram_def(ast)?,
            ast::Stmt::RetStmt(ast) => self.lower_return_stmt(ast),
            ast::Stmt::IfElse(ast) => self.lower_if_else(ast),
            ast::Stmt::SwitchCase(ast) => self.lower_switch_case(ast),
            ast::Stmt::ForLoop(ast) => self.lower_for_loop(ast),
            ast::Stmt::WhileLoop(ast) => self.lower_while_loop(ast),
            ast::Stmt::DoUntil(ast) => self.lower_do_until(ast),
            ast::Stmt::Expr(ast) => Stmt::Expr(self.lower_expr(Some(ast))),
        })
    }

    pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
        if let Some(ast) = ast {
            match ast {
                ast::Expr::Binary(ast) => self.lower_binary(ast),
                ast::Expr::Unary(ast) => self.lower_unary(ast),
                ast::Expr::Literal(ast) => self.lower_literal(ast),
                ast::Expr::ArrayLiteral(ast) => self.lower_array_literal(ast),
                ast::Expr::Paren(ast) => self.lower_expr(ast.expr()),
                ast::Expr::NameRef(ast) => self.lower_name_ref(ast),
                ast::Expr::Call(ast) => self.lower_call(ast),
            }
        } else {
            Expr::Missing
        }
    }

    pub fn get(&self, idx: ExprIdx) -> &Expr {
        &self.exprs[idx]
    }

    pub fn get_range(&self, range: ExprRange) -> &[Expr] {
        &self.exprs[range]
    }

    fn lower_do_until(&mut self, ast: ast::DoUntil) -> Stmt {
        let body = ast
            .body()
            .map(|b| b.filter_map(|ast| self.lower_stmt(ast)).collect())
            .unwrap_or_default();
        let condition = self.lower_expr(ast.condition());
        Stmt::DoUntilLoop {
            condition: self.exprs.alloc(condition),
            body,
        }
    }

    fn lower_while_loop(&mut self, ast: ast::WhileLoop) -> Stmt {
        let condition = {
            let expr = self.lower_expr(ast.condition());
            self.exprs.alloc(expr)
        };
        let body = ast
            .body()
            .map(|b| b.filter_map(|ast| self.lower_stmt(ast)).collect())
            .unwrap_or_default();
        Stmt::WhileLoop { condition, body }
    }

    fn lower_for_loop(&mut self, ast: ast::ForLoop) -> Stmt {
        let loop_var = ast.loop_var().map(|i| i.text().into());
        let (start, end) = {
            let tmp = ast
                .bounds()
                .map(|(s, e)| (self.lower_expr(Some(s)), self.lower_expr(Some(e))))
                .unwrap_or((Expr::Missing, Expr::Missing));
            (self.exprs.alloc(tmp.0), self.exprs.alloc(tmp.1))
        };
        let step = {
            let tmp = ast
                .step()
                .map(|ast| self.lower_expr(Some(ast)))
                .unwrap_or(Expr::Missing);
            self.exprs.alloc(tmp)
        };
        let body = ast
            .body()
            .map(|b| b.filter_map(|ast| self.lower_stmt(ast)).collect())
            .unwrap_or_default();
        Stmt::ForLoop {
            loop_var,
            start,
            end,
            step,
            body,
        }
    }

    fn lower_switch_case(&mut self, ast: ast::SwitchCase) -> Stmt {
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
            .map(|b| b.filter_map(|ast| self.lower_stmt(ast)).collect::<Vec<_>>())
            .unwrap_or_default();
        Stmt::SwitchCase {
            scrutinee,
            cases,
            case_bodies,
            default_body,
        }
    }

    fn lower_if_else(&mut self, ast: ast::IfElse) -> Stmt {
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
                .map(|body| body.filter_map(|ast| self.lower_stmt(ast)).collect())
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

        Stmt::IfElse {
            condition,
            body,
            elseifs,
            else_body,
        }
    }

    fn lower_return_stmt(&mut self, ast: ast::RetStmt) -> Stmt {
        let val = self.lower_expr(ast.value());
        Stmt::ReturnStmt {
            value: self.exprs.alloc(val),
        }
    }

    fn lower_subprogram_def(&mut self, ast: ast::SubprogDef) -> Option<Stmt> {
        Some(Stmt::SubprogramDef {
            kind: match ast.kind()?.kind() {
                SyntaxKind::Function => SubprogramKind::Function,
                SyntaxKind::Procedure => SubprogramKind::Procedure,
                _ => unreachable!(),
            },
            name: ast.name()?.text().into(),
            params: ast.params().map(|ast| ast.text().into()).collect(),
            body: ast.body().filter_map(|ast| self.lower_stmt(ast)).collect(),
        })
    }

    fn lower_var_def(&mut self, ast: ast::VarDef) -> Option<Stmt> {
        Some(Stmt::VarDef {
            name: ast.name()?.text().into(),
            kind: ast
                .kind()
                .map(|k| match k.kind() {
                    SyntaxKind::Const => VarDefKind::Constant,
                    SyntaxKind::Global => VarDefKind::Global,
                    _ => unreachable!(),
                })
                .unwrap_or(VarDefKind::Standard),
            value: self.lower_expr(ast.value()),
        })
    }

    fn lower_array_def(&mut self, ast: ast::ArrayDef) -> Option<Stmt> {
        let subscript = {
            if let Some(mut subs) = ast.subscript() {
                let first = self.lower_expr(subs.next());
                let last = self.lower_expr(subs.next());
                (first, last)
            } else {
                (Expr::Missing, Expr::Missing)
            }
        };
        let dimensions = {
            let mut dims = ast.dimensions();
            let outer = self.lower_expr(dims.next());
            let inner = self.lower_expr(dims.next());

            (outer, inner)
        };

        Some(Stmt::ArrayDef {
            name: ast.name()?.text().into(),
            kind: ast
                .kind()
                .map(|k| match k.kind() {
                    SyntaxKind::Const => VarDefKind::Constant,
                    SyntaxKind::Global => VarDefKind::Global,
                    _ => unreachable!(),
                })
                .unwrap_or(VarDefKind::Standard),
            subscript,
            dimensions,
            value: self.lower_expr(ast.value()),
        })
    }

    fn lower_binary(&mut self, ast: ast::BinaryExpr) -> Expr {
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

        Expr::Binary { lhs, rhs, op }
    }

    fn lower_unary(&mut self, ast: ast::UnaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Not => UnaryOp::Not,
            SyntaxKind::Minus => UnaryOp::Neg,
            _ => unreachable!(),
        };

        let opand = self.lower_expr(ast.expr());

        Expr::Unary {
            op,
            opand: self.exprs.alloc(opand),
        }
    }

    fn lower_literal(&mut self, ast: ast::Literal) -> Expr {
        match ast.parse() {
            Some(v) => match v {
                ast::Val::Int(i) => Expr::Literal {
                    value: Literal::Int(i),
                },
                ast::Val::Float(f) => Expr::Literal {
                    value: Literal::Float(f),
                },
                ast::Val::Char(c) => Expr::Literal {
                    value: Literal::Char(c),
                },
                ast::Val::String(s) => Expr::Literal {
                    value: Literal::String(s.trim_matches('"').into()),
                },
                ast::Val::Bool(b) => Expr::Literal {
                    value: Literal::Bool(b),
                },
            },
            None => Expr::Missing,
        }
    }

    fn lower_array_literal(&mut self, ast: ast::ArrayLiteral) -> Expr {
        match ast.items() {
            Some(v) => {
                let vals = v
                    .into_iter()
                    .map(|i| self.lower_expr(Some(i)))
                    .collect::<Vec<_>>();
                Expr::Literal {
                    value: Literal::Array(self.exprs.alloc_many(vals)),
                }
            }
            None => Expr::Missing,
        }
    }

    fn lower_name_ref(&mut self, ast: ast::NameRef) -> Expr {
        Expr::NameRef {
            name: ast.name().unwrap().text().into(),
        }
    }

    fn lower_call(&mut self, ast: ast::SubprogCall) -> Expr {
        let callee = ast.callee().unwrap().text().into();
        let args = ast
            .args()
            .map(|e| self.lower_expr(Some(e)))
            .collect::<Vec<_>>();

        Expr::Call {
            callee,
            args: self.exprs.alloc_many(args),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn parse(input: &str) -> ast::Root {
        ast::Root::cast(parser::parse(input).syntax()).unwrap()
    }

    fn check_stmt(input: &str, expected_hir: Stmt) {
        let root = parse(input);
        let ast = root.stmts().next().unwrap();
        let hir = Database::default().lower_stmt(ast).unwrap();

        assert_eq!(hir, expected_hir);
    }

    fn check_expr(input: &str, expected_hir: Expr, expected_database: Database) {
        let root = parse(input);
        let first_stmt = root.stmts().next().unwrap();
        let ast = match first_stmt {
            ast::Stmt::Expr(ast) => ast,
            _ => unreachable!(),
        };
        let mut database = Database::default();
        let hir = database.lower_expr(Some(ast));

        assert_eq!(hir, expected_hir);
        assert_eq!(database, expected_database)
    }

    #[test]
    fn lower_variable_def() {
        check_stmt(
            "foo = bar",
            Stmt::VarDef {
                name: "foo".into(),
                kind: VarDefKind::Standard,
                value: Expr::NameRef { name: "bar".into() },
            },
        );
    }

    #[test]
    fn lower_variable_def_without_value() {
        check_stmt(
            "a = ",
            Stmt::VarDef {
                name: "a".into(),
                kind: VarDefKind::Standard,
                value: Expr::Missing,
            },
        );
    }

    #[test]
    fn lower_const_variable_def() {
        check_stmt(
            "const a = 15",
            Stmt::VarDef {
                name: "a".into(),
                kind: VarDefKind::Constant,
                value: Expr::Literal {
                    value: Literal::Int(15),
                },
            },
        );
    }

    #[test]
    fn lower_global_variable_def() {
        check_stmt(
            "global COUNTER = 0",
            Stmt::VarDef {
                name: "COUNTER".into(),
                kind: VarDefKind::Global,
                value: Expr::Literal {
                    value: Literal::Int(0),
                },
            },
        );
    }

    #[test]
    fn lower_array_decl() {
        check_stmt(
            "const array nums[5]",
            Stmt::ArrayDef {
                name: "nums".into(),
                kind: VarDefKind::Constant,
                subscript: (Expr::Missing, Expr::Missing),
                dimensions: (
                    Expr::Literal {
                        value: Literal::Int(5),
                    },
                    Expr::Missing,
                ),
                value: Expr::Missing,
            },
        );
    }

    #[test]
    fn lower_array_def() {
        let mut exprs = Arena::new();
        let nums = exprs.alloc_many([
            Expr::Literal {
                value: Literal::Int(1),
            },
            Expr::Literal {
                value: Literal::Int(2),
            },
            Expr::Literal {
                value: Literal::Int(3),
            },
        ]);
        check_stmt(
            "array nums = [1, 2, 3]",
            Stmt::ArrayDef {
                name: "nums".into(),
                kind: VarDefKind::Standard,
                subscript: (Expr::Missing, Expr::Missing),
                dimensions: (Expr::Missing, Expr::Missing),
                value: Expr::Literal {
                    value: Literal::Array(nums),
                },
            },
        )
    }

    #[test]
    fn lower_2d_array_decl() {
        check_stmt(
            "global array board[8,8]",
            Stmt::ArrayDef {
                name: "board".into(),
                kind: VarDefKind::Global,
                subscript: (Expr::Missing, Expr::Missing),
                dimensions: (
                    Expr::Literal {
                        value: Literal::Int(8),
                    },
                    Expr::Literal {
                        value: Literal::Int(8),
                    },
                ),
                value: Expr::Missing,
            },
        );
    }

    #[test]
    fn lower_array_subscript_assign() {
        check_stmt(
            "arr[5] = 5",
            Stmt::ArrayDef {
                name: "arr".into(),
                kind: VarDefKind::Standard,
                subscript: (
                    Expr::Literal {
                        value: Literal::Int(5),
                    },
                    Expr::Missing,
                ),
                dimensions: (Expr::Missing, Expr::Missing),
                value: Expr::Literal {
                    value: Literal::Int(5),
                },
            },
        );
    }

    #[test]
    fn lower_2d_array_subscript_assign() {
        check_stmt(
            "arr[3,4] = 5",
            Stmt::ArrayDef {
                name: "arr".into(),
                kind: VarDefKind::Standard,
                subscript: (
                    Expr::Literal {
                        value: Literal::Int(3),
                    },
                    Expr::Literal {
                        value: Literal::Int(4),
                    },
                ),
                dimensions: (Expr::Missing, Expr::Missing),
                value: Expr::Literal {
                    value: Literal::Int(5),
                },
            },
        );
    }

    #[test]
    fn lower_expr_stmt() {
        check_stmt(
            "123",
            Stmt::Expr(Expr::Literal {
                value: Literal::Int(123),
            }),
        );
    }

    #[test]
    fn lower_binary_expr() {
        let mut exprs = Arena::new();
        let lhs = exprs.alloc(Expr::Literal {
            value: Literal::Int(5),
        });
        let rhs = exprs.alloc(Expr::Literal {
            value: Literal::Int(10),
        });

        check_expr(
            "5 + 10",
            Expr::Binary {
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
        let lhs = exprs.alloc(Expr::Literal {
            value: Literal::Float(17.0),
        });
        let rhs = exprs.alloc(Expr::Missing);

        check_expr(
            "17.0 * ",
            Expr::Binary {
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
            Expr::Literal {
                value: Literal::Int(99),
            },
            Database::default(),
        );
        check_expr(
            "1.5",
            Expr::Literal {
                value: Literal::Float(1.5),
            },
            Database::default(),
        );
    }

    #[test]
    fn lower_string_literal() {
        check_expr(
            r#""hello""#,
            Expr::Literal {
                value: Literal::String("hello".into()),
            },
            Database::default(),
        );
    }

    #[test]
    fn lower_char_literal() {
        check_expr(
            "'c'",
            Expr::Literal {
                value: Literal::Char('c'),
            },
            Database::default(),
        );
    }

    #[test]
    fn lower_bool_literal() {
        check_expr(
            "true",
            Expr::Literal {
                value: Literal::Bool(true),
            },
            Database::default(),
        );
        check_expr(
            "false",
            Expr::Literal {
                value: Literal::Bool(false),
            },
            Database::default(),
        );
    }

    #[test]
    fn lower_array_literal() {
        let mut exprs = Arena::new();
        let elems = [
            Expr::Literal {
                value: Literal::Int(1),
            },
            Expr::Literal {
                value: Literal::Int(2),
            },
            Expr::Literal {
                value: Literal::Int(3),
            },
            Expr::Literal {
                value: Literal::Int(4),
            },
            Expr::Literal {
                value: Literal::Int(5),
            },
        ];
        check_expr(
            "[1, 2, 3, 4, 5]",
            Expr::Literal {
                value: Literal::Array(exprs.alloc_many(elems)),
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_paren_expr() {
        check_expr(
            "(((((x)))))",
            Expr::NameRef { name: "x".into() },
            Database::default(),
        );
    }

    #[test]
    fn lower_unary_expr() {
        let mut exprs = Arena::new();
        let five = exprs.alloc(Expr::Literal {
            value: Literal::Int(5),
        });
        check_expr(
            "-5",
            Expr::Unary {
                op: UnaryOp::Neg,
                opand: five,
            },
            Database { exprs },
        )
    }

    #[test]
    fn lower_unary_expr_without_expr() {
        let mut exprs = Arena::new();
        let opand = exprs.alloc(Expr::Missing);

        check_expr(
            "NOT ",
            Expr::Unary {
                op: UnaryOp::Not,
                opand,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_name_ref() {
        check_expr("x", Expr::NameRef { name: "x".into() }, Database::default());
    }

    #[test]
    fn lower_subprog_call() {
        let mut exprs = Arena::default();
        let arg = exprs.alloc_many([Expr::NameRef { name: "x".into() }]);
        check_expr(
            "print(x)",
            Expr::Call {
                callee: "print".into(),
                args: arg,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_func_def() {
        let mut exprs = Arena::new();
        let value = exprs.alloc(Expr::NameRef { name: "x".into() });
        check_stmt(
            "function id(x) return x endfunction",
            Stmt::SubprogramDef {
                kind: SubprogramKind::Function,
                name: "id".into(),
                params: vec!["x".into()],
                body: vec![Stmt::ReturnStmt { value }],
            },
        )
    }

    #[test]
    fn lower_proc_def() {
        let mut exprs = Arena::new();
        let arg = exprs.alloc_many([Expr::Literal {
            value: Literal::String("something".into()),
        }]);
        check_stmt(
            r#"procedure thing() print("something") endprocedure"#,
            Stmt::SubprogramDef {
                kind: SubprogramKind::Procedure,
                name: "thing".into(),
                params: vec![],
                body: vec![Stmt::Expr(Expr::Call {
                    callee: "print".into(),
                    args: arg,
                })],
            },
        )
    }

    #[test]
    fn lower_for_loop() {
        let mut exprs = Arena::new();
        let (start, end) = (
            exprs.alloc(Expr::Literal {
                value: Literal::Int(1),
            }),
            exprs.alloc(Expr::Literal {
                value: Literal::Int(10),
            }),
        );
        let step = exprs.alloc(Expr::Missing);
        let print_arg = exprs.alloc_many([Expr::NameRef { name: "i".into() }]);
        check_stmt(
            "for i = 1 to 10 print(i) next i",
            Stmt::ForLoop {
                loop_var: Some("i".into()),
                start,
                end,
                step,
                body: vec![Stmt::Expr(Expr::Call {
                    callee: "print".into(),
                    args: print_arg,
                })],
            },
        )
    }

    #[test]
    fn lower_for_loop_with_step() {
        let mut exprs = Arena::new();
        let (start, end) = (
            exprs.alloc(Expr::Literal {
                value: Literal::Int(1),
            }),
            exprs.alloc(Expr::Literal {
                value: Literal::Int(10),
            }),
        );
        let step = exprs.alloc(Expr::Literal {
            value: Literal::Int(2),
        });
        let print_arg = exprs.alloc_many([Expr::NameRef { name: "i".into() }]);
        check_stmt(
            "for i = 1 to 10 step 2 print(i) next i",
            Stmt::ForLoop {
                loop_var: Some("i".into()),
                start,
                end,
                step,
                body: vec![Stmt::Expr(Expr::Call {
                    callee: "print".into(),
                    args: print_arg,
                })],
            },
        )
    }

    #[test]
    fn lower_while_loop() {
        let mut exprs = Arena::new();
        let condition = {
            let lhs = exprs.alloc(Expr::NameRef {
                name: "answer".into(),
            });
            let rhs = exprs.alloc(Expr::Literal {
                value: Literal::String("Correct".into()),
            });
            exprs.alloc(Expr::Binary {
                op: BinaryOp::NotEquals,
                lhs,
                rhs,
            })
        };
        let body = {
            vec![Stmt::Expr(Expr::Call {
                callee: "x".into(),
                args: exprs.alloc_many([]),
            })]
        };
        check_stmt(
            r#"while answer != "Correct" x() endwhile"#,
            Stmt::WhileLoop { condition, body },
        );
    }

    #[test]
    fn lower_do_until() {
        let mut exprs = Arena::new();
        let body = vec![Stmt::Expr(Expr::Call {
            callee: "thing".into(),
            args: exprs.alloc_many([]),
        })];
        let condition = exprs.alloc(Expr::NameRef {
            name: "condition".into(),
        });
        check_stmt(
            "do thing() until condition",
            Stmt::DoUntilLoop { condition, body },
        );
    }

    #[test]
    fn lower_if_elseif_else() {
        let mut exprs = Arena::new();
        let condition = {
            let lhs = exprs.alloc(Expr::Literal {
                value: Literal::Float(2.5),
            });
            let rhs = exprs.alloc(Expr::NameRef { name: "n".into() });

            exprs.alloc(Expr::Binary {
                op: BinaryOp::LessThan,
                lhs,
                rhs,
            })
        };
        let elseifs = {
            let lhs = exprs.alloc(Expr::Literal {
                value: Literal::Float(2.5),
            });
            let rhs = exprs.alloc(Expr::Literal {
                value: Literal::Float(2.5),
            });
            let conditions = vec![Expr::Binary {
                op: BinaryOp::GreaterEquals,
                lhs,
                rhs,
            }];
            let bodies = vec![vec![Stmt::Expr(Expr::Literal {
                value: Literal::Int(2),
            })]];

            conditions
                .into_iter()
                .map(|e| exprs.alloc(e))
                .zip(bodies)
                .collect()
        };
        let body = vec![Stmt::Expr(Expr::Literal {
            value: Literal::Int(5),
        })];
        let else_body = vec![Stmt::Expr(Expr::Literal {
            value: Literal::Int(7),
        })];
        check_stmt(
            "if 2.5 < n then 5 elseif 2.5 >= 2.5 then 2 else 7 endif",
            Stmt::IfElse {
                condition,
                body,
                elseifs,
                else_body,
            },
        )
    }

    #[test]
    fn lower_switch_stmt() {
        let mut exprs = Arena::new();
        let scrutinee = exprs.alloc(Expr::NameRef { name: "c".into() });
        let cases = exprs.alloc_many([
            Expr::Literal {
                value: Literal::Char('a'),
            },
            Expr::Literal {
                value: Literal::Char('b'),
            },
        ]);
        let case_bodies = vec![
            vec![Stmt::Expr(Expr::Call {
                callee: "a".into(),
                args: exprs.alloc_many(std::iter::empty()),
            })],
            vec![Stmt::Expr(Expr::Call {
                callee: "b".into(),
                args: exprs.alloc_many(std::iter::empty()),
            })],
        ];
        check_stmt(
            "switch c: case 'a': a() case 'b': b() default: d() endswitch",
            Stmt::SwitchCase {
                scrutinee,
                cases,
                case_bodies,
                default_body: vec![Stmt::Expr(Expr::Call {
                    callee: "d".into(),
                    args: exprs.alloc_many([]),
                })],
            },
        )
    }
}
