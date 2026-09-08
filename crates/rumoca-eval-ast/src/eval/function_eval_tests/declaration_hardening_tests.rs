use super::*;

fn named(name: &str, value: Expression) -> Expression {
    Expression::NamedArgument {
        name: token(name),
        value: Arc::new(value),
        span: test_span(),
    }
}

fn constant_function(name: &str) -> ClassDef {
    let mut function = ClassDef {
        name: token(name),
        class_type: ClassType::Function,
        pure: true,
        ..ClassDef::default()
    };
    function
        .components
        .insert("y".to_string(), output_parameter("y"));
    function.algorithms.push(vec![Statement::Assignment {
        comp: cref("y"),
        value: int_expr(7),
    }]);
    function
}

fn evaluate_registered(function: ClassDef, arguments: Vec<Expression>) -> Option<i64> {
    let name = function.name.text.to_string();
    let mut functions = FxHashMap::default();
    functions.insert(name.clone(), function);
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.functions = Arc::new(functions);
    eval_integer_with_scope(&call(&name, arguments), &ctx, "")
}

#[test]
fn unused_recovery_default_refuses_typecheck_time_function_evaluation() {
    let nested_recovery = if_expr(
        bool_expr(true),
        int_expr(1),
        Expression::Empty { span: test_span() },
    );
    let mut function = ClassDef {
        name: token("recoveredDefault"),
        class_type: ClassType::Function,
        pure: true,
        ..ClassDef::default()
    };
    function.components.insert(
        "unused".to_string(),
        input_parameter("unused", Some(nested_recovery)),
    );
    function
        .components
        .insert("y".to_string(), output_parameter("y"));
    function.algorithms.push(vec![Statement::Assignment {
        comp: cref("y"),
        value: int_expr(1),
    }]);
    let mut functions = FxHashMap::default();
    functions.insert("recoveredDefault".to_string(), function);
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.functions = Arc::new(functions);

    assert_eq!(
        eval_integer_with_scope(&call("recoveredDefault", vec![]), &ctx, ""),
        None,
    );
}

#[test]
fn context_only_syntax_in_unused_default_refuses_typecheck_function_evaluation() {
    let forms = [
        Expression::Binary {
            op: OpBinary::Assign,
            lhs: Arc::new(int_expr(1)),
            rhs: Arc::new(int_expr(2)),
            span: test_span(),
        },
        Expression::Terminal {
            terminal_type: TerminalType::End,
            token: token("end"),
            span: test_span(),
        },
    ];
    for invalid in forms {
        let mut function = ClassDef {
            name: token("invalidDefault"),
            class_type: ClassType::Function,
            pure: true,
            ..ClassDef::default()
        };
        function.components.insert(
            "unused".to_string(),
            input_parameter("unused", Some(invalid)),
        );
        function
            .components
            .insert("y".to_string(), output_parameter("y"));
        function.algorithms.push(vec![Statement::Assignment {
            comp: cref("y"),
            value: int_expr(1),
        }]);
        let mut functions = FxHashMap::default();
        functions.insert("invalidDefault".to_string(), function);
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.functions = Arc::new(functions);
        assert_eq!(
            eval_integer_with_scope(&call("invalidDefault", vec![]), &ctx, ""),
            None,
        );
    }
}

#[test]
fn malformed_explicit_binding_metadata_refuses_typecheck_function_evaluation() {
    for (binding, explicit) in [(None, true), (Some(int_expr(5)), false)] {
        let mut function = ClassDef {
            name: token("malformedBinding"),
            class_type: ClassType::Function,
            pure: true,
            ..ClassDef::default()
        };
        let mut unused = input_parameter("unused", binding);
        unused.has_explicit_binding = explicit;
        function.components.insert("unused".to_string(), unused);
        function
            .components
            .insert("y".to_string(), output_parameter("y"));
        function.algorithms.push(vec![Statement::Assignment {
            comp: cref("y"),
            value: int_expr(1),
        }]);
        let mut functions = FxHashMap::default();
        functions.insert("malformedBinding".to_string(), function);
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.functions = Arc::new(functions);
        assert_eq!(
            eval_integer_with_scope(&call("malformedBinding", vec![]), &ctx, ""),
            None,
        );
    }
}

#[test]
fn eval_multi_index_for_stmt_fails_evaluation() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let statements = vec![Statement::For {
        indices: vec![
            ForIndex {
                ident: token("i"),
                range: Expression::Range {
                    start: Arc::new(int_expr(1)),
                    step: None,
                    end: Arc::new(int_expr(2)),
                    span: rumoca_core::Span::DUMMY,
                },
            },
            ForIndex {
                ident: token("j"),
                range: Expression::Range {
                    start: Arc::new(int_expr(1)),
                    step: None,
                    end: Arc::new(int_expr(2)),
                    span: rumoca_core::Span::DUMMY,
                },
            },
        ],
        equations: vec![Statement::Assignment {
            comp: cref("x"),
            value: int_expr(1),
        }],
    }];

    assert_eq!(interpret_stmts(&statements, &mut ctx), None);
    assert!(!ctx.integers.contains_key("x"));
}

#[test]
fn typecheck_function_call_binding_requires_an_exact_argument_layout() {
    let function = || {
        let mut function = constant_function("layout");
        function
            .components
            .insert("a".to_string(), input_parameter("a", None));
        function
            .components
            .insert("b".to_string(), input_parameter("b", Some(int_expr(2))));
        function
    };

    assert_eq!(
        evaluate_registered(function(), vec![int_expr(1), named("b", int_expr(3))]),
        Some(7)
    );
    assert_eq!(
        evaluate_registered(function(), vec![named("a", int_expr(1))]),
        Some(7)
    );
    for malformed in [
        vec![named("b", int_expr(3))],
        vec![int_expr(1), named("unknown", int_expr(3))],
        vec![
            int_expr(1),
            named("b", int_expr(2)),
            named("b", int_expr(3)),
        ],
        vec![named("b", int_expr(2)), int_expr(1)],
        vec![int_expr(1), int_expr(2), int_expr(3)],
    ] {
        assert_eq!(evaluate_registered(function(), malformed), None);
    }
}

#[test]
fn typecheck_integer_function_fold_requires_pure_non_external_definition() {
    assert_eq!(
        evaluate_registered(constant_function("pure"), vec![]),
        Some(7)
    );

    let mut impure = constant_function("impure");
    impure.pure = false;
    assert_eq!(evaluate_registered(impure, vec![]), None);

    let mut external = constant_function("external");
    external.external = Some(rumoca_ir_ast::ExternalFunction::default());
    assert_eq!(evaluate_registered(external, vec![]), None);
}

#[test]
fn negative_function_output_extent_is_not_laundered_to_usize() {
    let mut function = constant_function("negativeShape");
    function.components.get_mut("y").unwrap().shape_expr =
        vec![Subscript::Expression(int_expr(-1))];
    let mut functions = FxHashMap::default();
    functions.insert("negativeShape".to_string(), function);
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.functions = Arc::new(functions);

    assert_eq!(
        infer_dimensions_from_binding(&call("negativeShape", vec![]), &ctx),
        None
    );
}

#[test]
fn typecheck_loop_budget_is_shared_across_nested_loops_and_checked_before_allocation() {
    let mut exact = TypeCheckEvalContext::for_pre_identity_structural();
    assert_eq!(
        interpret_stmts(
            &[for_statement(
                "i",
                range_expr(
                    1,
                    None,
                    crate::function_budget::MAX_AST_FUNCTION_EVAL_WORK as i64
                ),
                vec![Statement::Assignment {
                    comp: cref("ran"),
                    value: int_expr(1),
                }],
            )],
            &mut exact,
        ),
        Some(FunctionStmtFlow::Continue)
    );

    let mut too_large = TypeCheckEvalContext::for_pre_identity_structural();
    assert_eq!(
        interpret_stmts(
            &[for_statement(
                "i",
                range_expr(
                    1,
                    None,
                    crate::function_budget::MAX_AST_FUNCTION_EVAL_WORK as i64 + 1,
                ),
                vec![Statement::Assignment {
                    comp: cref("must_not_run"),
                    value: int_expr(1),
                }],
            )],
            &mut too_large,
        ),
        None
    );
    assert!(!too_large.integers.contains_key("must_not_run"));

    let nested = for_statement(
        "i",
        range_expr(1, None, 65),
        vec![for_statement(
            "j",
            range_expr(1, None, 65),
            vec![Statement::Assignment {
                comp: cref("visited"),
                value: int_expr(1),
            }],
        )],
    );
    assert_eq!(
        interpret_stmts(
            &[nested],
            &mut TypeCheckEvalContext::for_pre_identity_structural()
        ),
        None
    );
}

#[test]
fn typecheck_loop_budget_is_shared_by_recursive_function_calls() {
    let mut function = constant_function("budgetRecursive");
    function
        .components
        .insert("n".to_string(), input_parameter("n", None));
    function.algorithms = vec![vec![
        for_statement(
            "i",
            range_expr(1, None, 1000),
            vec![Statement::Assignment {
                comp: cref("y"),
                value: int_expr(1),
            }],
        ),
        Statement::Assignment {
            comp: cref("y"),
            value: if_expr(
                binary(OpBinary::Gt, cref_expr("n"), int_expr(0)),
                call(
                    "budgetRecursive",
                    vec![binary(OpBinary::Sub, cref_expr("n"), int_expr(1))],
                ),
                int_expr(1),
            ),
        },
    ]];

    assert_eq!(
        evaluate_registered(function.clone(), vec![int_expr(3)]),
        Some(1)
    );
    assert_eq!(evaluate_registered(function, vec![int_expr(4)]), None);
}
