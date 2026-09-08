use super::*;
use rumoca_core::{ClassType, Token, Variability};
use rumoca_ir_ast::{Component, ComponentRefPart, ComponentReference, ForIndex};
use rustc_hash::FxHashMap;

fn token(text: &str) -> Token {
    Token {
        text: text.into(),
        location: Default::default(),
        token_number: 0,
        token_type: 0,
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_eval_test.mo"),
        1,
        2,
    )
}

fn cref(name: &str) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: crate::path_utils::split_path_with_indices(name)
            .into_iter()
            .map(|part| ComponentRefPart {
                ident: token(part),
                subs: None,
                def_id: None,
            })
            .collect(),
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    }
}

fn int_expr(value: i64) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: token(&value.to_string()),

        span: rumoca_core::Span::DUMMY,
    }
}

fn real_expr(value: f64) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedReal,
        token: token(&value.to_string()),

        span: rumoca_core::Span::DUMMY,
    }
}

fn bool_expr(value: bool) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::Bool,
        token: token(if value { "true" } else { "false" }),

        span: rumoca_core::Span::DUMMY,
    }
}

fn cref_expr(name: &str) -> Expression {
    Expression::ComponentReference(cref(name))
}

fn field_expr(base: &str, field: &str) -> Expression {
    Expression::FieldAccess {
        base: Arc::new(cref_expr(base)),
        field: field.to_string(),
        field_def_id: None,
        span: rumoca_core::Span::DUMMY,
    }
}

fn call(name: &str, args: Vec<Expression>) -> Expression {
    Expression::FunctionCall {
        comp: cref(name),
        args,
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),

        span: rumoca_core::Span::DUMMY,
    }
}

fn range_expr(start: i64, step: Option<i64>, end: i64) -> Expression {
    Expression::Range {
        start: Arc::new(int_expr(start)),
        step: step.map(int_expr).map(Arc::new),
        end: Arc::new(int_expr(end)),
        span: rumoca_core::Span::DUMMY,
    }
}

fn for_statement(index: &str, range: Expression, body: Vec<Statement>) -> Statement {
    Statement::For {
        indices: vec![ForIndex {
            ident: token(index),
            range,
        }],
        equations: body,
    }
}

fn if_expr(
    condition: Expression,
    then_expression: Expression,
    else_expression: Expression,
) -> Expression {
    Expression::If {
        branches: vec![(condition, then_expression)],
        else_branch: Arc::new(else_expression),
        span: rumoca_core::Span::DUMMY,
    }
}

fn input_parameter(name: &str, binding: Option<Expression>) -> Component {
    let has_explicit_binding = binding.is_some();
    Component {
        name: name.to_string(),
        causality: Causality::Input(token("input")),
        variability: Variability::Parameter(token("parameter")),
        binding,
        has_explicit_binding,
        ..Component::empty_with_span(test_span())
    }
}

fn output_parameter(name: &str) -> Component {
    Component {
        name: name.to_string(),
        causality: Causality::Output(token("output")),
        variability: Variability::Parameter(token("parameter")),
        ..Component::empty_with_span(test_span())
    }
}

fn ns1_init_statement() -> Statement {
    Statement::Assignment {
        comp: cref("ns1"),
        value: binary(
            OpBinary::Mul,
            int_expr(2),
            call(
                "integer",
                vec![call(
                    "ceil",
                    vec![binary(
                        OpBinary::Div,
                        binary(OpBinary::Mul, cref_expr("f_max"), cref_expr("f_max_factor")),
                        cref_expr("f_resolution"),
                    )],
                )],
            ),
        ),
    }
}

fn ns_update_statement() -> Statement {
    let mod_ns1_2 = call("mod", vec![cref_expr("ns1"), int_expr(2)]);
    Statement::Assignment {
        comp: cref("ns"),
        value: Expression::If {
            branches: vec![(
                binary(OpBinary::Eq, mod_ns1_2, int_expr(0)),
                cref_expr("ns1"),
            )],
            else_branch: Arc::new(binary(OpBinary::Add, cref_expr("ns1"), int_expr(1))),

            span: rumoca_core::Span::DUMMY,
        },
    }
}

fn reduce_factors_loop(modulus: i64) -> Statement {
    let divisor = int_expr(modulus);
    Statement::While(StatementBlock {
        cond: binary(
            OpBinary::Eq,
            call("mod", vec![cref_expr("ns1"), divisor.clone()]),
            int_expr(0),
        ),
        stmts: vec![Statement::Assignment {
            comp: cref("ns1"),
            value: call("div", vec![cref_expr("ns1"), divisor]),
        }],
    })
}

fn ns_adjustment_loop() -> Statement {
    Statement::While(StatementBlock {
        cond: bool_expr(true),
        stmts: vec![
            Statement::Assignment {
                comp: cref("ns1"),
                value: cref_expr("ns"),
            },
            reduce_factors_loop(2),
            reduce_factors_loop(3),
            reduce_factors_loop(5),
            Statement::If {
                cond_blocks: vec![StatementBlock {
                    cond: binary(OpBinary::Le, cref_expr("ns1"), int_expr(1)),
                    stmts: vec![Statement::Break {
                        token: token("break"),
                    }],
                }],
                else_block: None,
            },
            Statement::Assignment {
                comp: cref("ns"),
                value: binary(OpBinary::Add, cref_expr("ns"), int_expr(2)),
            },
        ],
    })
}

fn build_sample_points_function() -> ClassDef {
    let mut function = ClassDef {
        name: token("samplePoints"),
        class_type: ClassType::Function,
        pure: true,
        ..ClassDef::default()
    };
    function
        .components
        .insert("f_max".to_string(), input_parameter("f_max", None));
    function.components.insert(
        "f_resolution".to_string(),
        input_parameter("f_resolution", None),
    );
    function.components.insert(
        "f_max_factor".to_string(),
        input_parameter("f_max_factor", Some(int_expr(5))),
    );
    function
        .components
        .insert("ns".to_string(), output_parameter("ns"));
    function.components.insert(
        "ns1".to_string(),
        Component {
            name: "ns1".to_string(),
            variability: Variability::Discrete(token("discrete")),
            ..Component::empty_with_span(test_span())
        },
    );

    function.algorithms.push(vec![
        ns1_init_statement(),
        ns_update_statement(),
        ns_adjustment_loop(),
        Statement::Return {
            token: token("return"),
        },
    ]);
    function
}

fn nfi_expr() -> Expression {
    call(
        "max",
        vec![
            int_expr(1),
            call(
                "min",
                vec![
                    binary(
                        OpBinary::Add,
                        call(
                            "integer",
                            vec![call(
                                "ceil",
                                vec![binary(OpBinary::Div, int_expr(4), real_expr(0.2))],
                            )],
                        ),
                        int_expr(1),
                    ),
                    binary(
                        OpBinary::Add,
                        call("div", vec![cref_expr("ns"), int_expr(2)]),
                        int_expr(1),
                    ),
                ],
            ),
        ],
    )
}

#[test]
fn eval_integer_with_scope_div_operator_requires_exact_quotient() {
    let ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let expr = binary(OpBinary::Div, int_expr(7), int_expr(2));
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), None);
}

#[test]
fn eval_integer_with_scope_div_builtin_remains_truncating() {
    let ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let expr = call("div", vec![int_expr(7), int_expr(2)]);
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), Some(3));
}

#[test]
fn eval_integer_with_scope_add_elem_uses_shared_binary_semantics() {
    let ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let expr = binary(OpBinary::AddElem, int_expr(2), int_expr(3));
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), Some(5));
}

#[test]
fn eval_integer_with_scope_exp_elem_uses_shared_binary_semantics() {
    let ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let expr = binary(OpBinary::ExpElem, int_expr(2), int_expr(5));
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), Some(32));
}

#[test]
fn eval_integer_if_with_unknown_condition_folds_equal_outcomes() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.add_integer("left.nRC", 2);
    ctx.add_integer("right.nRC", 2);
    let expression = if_expr(
        cref_expr("unresolvedCondition"),
        cref_expr("left.nRC"),
        cref_expr("right.nRC"),
    );

    assert_eq!(eval_integer_with_scope(&expression, &ctx, ""), Some(2));
}

#[test]
fn eval_integer_if_with_unknown_condition_rejects_distinct_outcomes() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.add_integer("left.nRC", 1);
    ctx.add_integer("right.nRC", 2);
    let expression = if_expr(
        cref_expr("unresolvedCondition"),
        cref_expr("left.nRC"),
        cref_expr("right.nRC"),
    );

    assert_eq!(eval_integer_with_scope(&expression, &ctx, ""), None);
}

#[test]
fn eval_real_if_requires_exactly_equal_unknown_outcomes() {
    let ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let expression = if_expr(
        cref_expr("unresolvedCondition"),
        real_expr(1.0),
        real_expr(f64::from_bits(1.0f64.to_bits() + 1)),
    );

    assert_eq!(eval_real_with_scope(&expression, &ctx, ""), None);
}

#[test]
fn eval_real_equality_does_not_merge_adjacent_values() {
    let ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let expression = binary(
        OpBinary::Eq,
        real_expr(1.0),
        real_expr(f64::from_bits(1.0f64.to_bits() + 1)),
    );

    assert_eq!(eval_boolean_with_scope(&expression, &ctx, ""), Some(false));
}

#[test]
fn typecheck_scalar_evaluation_preserves_deep_expression_support() {
    let ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let mut expression = int_expr(7);
    for _ in 0..32 {
        expression = Expression::Parenthesized {
            inner: Arc::new(expression),
            span: rumoca_core::Span::DUMMY,
        };
    }

    assert_eq!(eval_integer_with_scope(&expression, &ctx, ""), Some(7));
}

#[test]
fn scoped_evaluators_resolve_projected_field_paths() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.add_integer("source.nRC", 2);
    ctx.add_real("source.ratio", 0.5);
    ctx.booleans.insert("source.enabled".to_string(), true);
    ctx.enums
        .insert("source.mode".to_string(), "Mode.active".to_string());

    assert_eq!(
        eval_integer_with_scope(&field_expr("source", "nRC"), &ctx, ""),
        Some(2)
    );
    assert_eq!(
        eval_real_with_scope(&field_expr("source", "ratio"), &ctx, ""),
        Some(0.5)
    );
    assert_eq!(
        eval_boolean_with_scope(&field_expr("source", "enabled"), &ctx, ""),
        Some(true)
    );
    assert_eq!(
        eval_enum_with_scope(&field_expr("source", "mode"), &ctx, ""),
        Some("Mode.active".to_string())
    );
}

#[test]
fn eval_boolean_with_scope_enum_eq_accepts_suffix_qualification() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.enums.insert(
        "controllerType".to_string(),
        "Modelica.Blocks.Types.SimpleController.PI".to_string(),
    );

    let expr = binary(
        OpBinary::Eq,
        cref_expr("controllerType"),
        cref_expr("SimpleController.PI"),
    );

    assert_eq!(eval_boolean_with_scope(&expr, &ctx, ""), Some(true));
}

#[test]
fn eval_boolean_with_scope_enum_eq_accepts_shared_type_literal_tail() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.enums.insert(
        "frameResolve".to_string(),
        "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve".to_string(),
    );

    let expr = binary(
        OpBinary::Eq,
        cref_expr("frameResolve"),
        cref_expr("Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve"),
    );

    assert_eq!(eval_boolean_with_scope(&expr, &ctx, ""), Some(true));
}

#[test]
fn eval_boolean_with_scope_enum_eq_rejects_different_enum_type() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.enums.insert(
        "mode".to_string(),
        "Modelica.Blocks.Types.Init.PI".to_string(),
    );

    let expr = binary(
        OpBinary::Eq,
        cref_expr("mode"),
        cref_expr("Modelica.Blocks.Types.SimpleController.PI"),
    );

    assert_eq!(eval_boolean_with_scope(&expr, &ctx, ""), Some(false));
}

#[test]
fn eval_integer_with_scope_evaluates_while_based_function_with_real_inputs() {
    let mut functions = FxHashMap::default();
    functions.insert("samplePoints".to_string(), build_sample_points_function());

    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.functions = Arc::new(functions);

    let expr = call("samplePoints", vec![real_expr(4.0), real_expr(0.2)]);
    assert_eq!(eval_integer_with_scope(&expr, &ctx, ""), Some(200));

    ctx.integers.insert("ns".to_string(), 200);
    assert_eq!(eval_integer_with_scope(&nfi_expr(), &ctx, ""), Some(21));
}

#[test]
fn eval_for_stmt_break_exits_only_inner_loop() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let statements = vec![Statement::For {
        indices: vec![ForIndex {
            ident: token("i"),
            range: Expression::Range {
                start: Arc::new(int_expr(1)),
                step: None,
                end: Arc::new(int_expr(3)),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        equations: vec![
            Statement::Assignment {
                comp: cref("x"),
                value: cref_expr("i"),
            },
            Statement::Break {
                token: token("break"),
            },
        ],
    }];

    assert_eq!(
        interpret_stmts(&statements, &mut ctx),
        Some(FunctionStmtFlow::Continue)
    );
    assert_eq!(ctx.integers.get("x"), Some(&1));
    assert!(!ctx.integers.contains_key("i"));
}

#[test]
fn eval_for_stmt_honors_positive_and_negative_steps() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let assign_last = |name: &str| Statement::Assignment {
        comp: cref(name),
        value: cref_expr("i"),
    };
    let statements = vec![
        for_statement(
            "i",
            range_expr(1, Some(2), 5),
            vec![assign_last("ascending")],
        ),
        for_statement(
            "i",
            range_expr(5, Some(-2), 1),
            vec![assign_last("descending")],
        ),
    ];

    assert_eq!(
        interpret_stmts(&statements, &mut ctx),
        Some(FunctionStmtFlow::Continue),
    );
    assert_eq!(ctx.integers.get("ascending"), Some(&5));
    assert_eq!(ctx.integers.get("descending"), Some(&1));
}

#[test]
fn eval_for_stmt_rejects_zero_step_and_handles_extreme_endpoints() {
    let body = || {
        vec![Statement::Assignment {
            comp: cref("ran"),
            value: int_expr(1),
        }]
    };
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    assert_eq!(
        interpret_stmts(
            &[for_statement("i", range_expr(1, Some(0), 3), body())],
            &mut ctx
        ),
        None,
    );
    assert!(!ctx.integers.contains_key("ran"));

    for (range, expected) in [
        (range_expr(i64::MAX - 1, Some(2), i64::MAX), i64::MAX - 1),
        (range_expr(i64::MIN + 1, Some(-2), i64::MIN), i64::MIN + 1),
    ] {
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        let statement = for_statement(
            "i",
            range,
            vec![Statement::Assignment {
                comp: cref("last"),
                value: cref_expr("i"),
            }],
        );
        assert_eq!(
            interpret_stmts(&[statement], &mut ctx),
            Some(FunctionStmtFlow::Continue)
        );
        assert_eq!(ctx.integers.get("last"), Some(&expected));
    }

    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    assert_eq!(
        interpret_stmts(
            &[for_statement(
                "i",
                range_expr(i64::MAX - 1, Some(1), i64::MAX),
                vec![Statement::Assignment {
                    comp: cref("last"),
                    value: cref_expr("i"),
                }],
            )],
            &mut ctx,
        ),
        Some(FunctionStmtFlow::Continue),
    );
    assert_eq!(ctx.integers.get("last"), Some(&i64::MAX));
}

#[test]
fn nested_same_name_loop_indices_restore_every_shadowed_lane() {
    let inner = for_statement(
        "i",
        range_expr(7, None, 7),
        vec![Statement::Assignment {
            comp: cref("inner"),
            value: cref_expr("i"),
        }],
    );
    let outer = for_statement(
        "i",
        range_expr(1, None, 2),
        vec![
            inner,
            Statement::Assignment {
                comp: cref("outer"),
                value: cref_expr("i"),
            },
        ],
    );
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.integers.insert("i".to_string(), 42);
    ctx.reals.insert("i".to_string(), 42.0);
    ctx.booleans.insert("i".to_string(), true);

    assert_eq!(
        interpret_stmts(&[outer], &mut ctx),
        Some(FunctionStmtFlow::Continue),
    );
    assert_eq!(ctx.integers.get("inner"), Some(&7));
    assert_eq!(ctx.integers.get("outer"), Some(&2));
    assert_eq!(ctx.integers.get("i"), Some(&42));
    assert_eq!(ctx.reals.get("i"), Some(&42.0));
    assert_eq!(ctx.booleans.get("i"), Some(&true));
}

#[test]
fn assignment_to_active_loop_index_refuses_and_restores_the_shadowed_value() {
    let statement = for_statement(
        "i",
        range_expr(1, None, 1),
        vec![Statement::Assignment {
            comp: cref("i"),
            value: int_expr(9),
        }],
    );
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.integers.insert("i".to_string(), 42);
    ctx.reals.insert("i".to_string(), 42.0);

    assert_eq!(interpret_stmts(&[statement], &mut ctx), None);
    assert_eq!(ctx.integers.get("i"), Some(&42));
    assert_eq!(ctx.reals.get("i"), Some(&42.0));
}

#[test]
fn return_from_for_loop_restores_the_shadowed_value() {
    let statement = for_statement(
        "i",
        range_expr(1, None, 1),
        vec![Statement::Return {
            token: token("return"),
        }],
    );
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.integers.insert("i".to_string(), 42);
    ctx.reals.insert("i".to_string(), 42.0);

    assert_eq!(
        interpret_stmts(&[statement], &mut ctx),
        Some(FunctionStmtFlow::Return),
    );
    assert_eq!(ctx.integers.get("i"), Some(&42));
    assert_eq!(ctx.reals.get("i"), Some(&42.0));
}

#[test]
fn eval_unsupported_function_statement_fails_evaluation() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let statements = vec![Statement::Assert {
        condition: bool_expr(true),
        message: Expression::Terminal {
            terminal_type: TerminalType::String,
            token: token("ok"),

            span: rumoca_core::Span::DUMMY,
        },
        level: None,
    }];

    assert_eq!(interpret_stmts(&statements, &mut ctx), None);
}

#[test]
fn eval_recovery_statement_fails_instead_of_becoming_a_no_op() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();

    assert_eq!(
        interpret_stmts(
            &[
                Statement::Assignment {
                    comp: cref("must_not_start"),
                    value: int_expr(6),
                },
                Statement::Empty,
                Statement::Assignment {
                    comp: cref("must_not_run"),
                    value: int_expr(7),
                },
            ],
            &mut ctx,
        ),
        None
    );
    assert!(
        !ctx.integers.contains_key("must_not_start"),
        "the checked root must reject recovery before publishing a prefix"
    );
    assert!(
        !ctx.integers.contains_key("must_not_run"),
        "recovery must stop evaluation before later state is committed"
    );
    assert_eq!(
        interpret_stmts(&[], &mut ctx),
        Some(FunctionStmtFlow::Continue),
        "a genuinely empty algorithm section remains valid"
    );
}

#[test]
fn recovery_after_an_assignment_cannot_publish_a_partial_function_result() {
    let mut function = ClassDef {
        name: token("recovered"),
        class_type: ClassType::Function,
        pure: true,
        ..ClassDef::default()
    };
    function
        .components
        .insert("y".to_string(), output_parameter("y"));
    function.algorithms.push(vec![
        Statement::Assignment {
            comp: cref("y"),
            value: int_expr(7),
        },
        Statement::Empty,
    ]);

    let mut functions = FxHashMap::default();
    functions.insert("recovered".to_string(), function);
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.functions = Arc::new(functions);

    assert_eq!(
        eval_integer_with_scope(&call("recovered", vec![]), &ctx, ""),
        None
    );
    assert!(
        !ctx.integers.contains_key("y"),
        "the evaluator's local partial result must not escape into its caller"
    );
}

#[test]
fn recovery_expression_and_unsupported_rhs_cannot_publish_a_stale_result() {
    let recovery = Expression::Empty { span: test_span() };
    let equal_branch_recovery = Expression::If {
        branches: vec![(recovery.clone(), int_expr(1))],
        else_branch: Arc::new(int_expr(1)),
        span: test_span(),
    };
    for invalid in [recovery, equal_branch_recovery, call("unknown", vec![])] {
        let mut function = ClassDef {
            name: token("recoveredExpression"),
            class_type: ClassType::Function,
            pure: true,
            ..ClassDef::default()
        };
        function
            .components
            .insert("y".to_string(), output_parameter("y"));
        function.algorithms.push(vec![
            Statement::Assignment {
                comp: cref("y"),
                value: int_expr(7),
            },
            Statement::Assignment {
                comp: cref("y"),
                value: invalid,
            },
        ]);
        let mut functions = FxHashMap::default();
        functions.insert("recoveredExpression".to_string(), function);
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.functions = Arc::new(functions);

        assert_eq!(
            eval_integer_with_scope(&call("recoveredExpression", vec![]), &ctx, ""),
            None,
            "a failed required assignment must discard the local candidate result"
        );
    }
}

#[test]
fn scalar_reassignment_replaces_incompatible_typed_lanes() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    assert_eq!(
        interpret_stmts(
            &[
                Statement::Assignment {
                    comp: cref("y"),
                    value: int_expr(7),
                },
                Statement::Assignment {
                    comp: cref("y"),
                    value: real_expr(7.5),
                },
            ],
            &mut ctx,
        ),
        Some(FunctionStmtFlow::Continue),
    );
    assert_eq!(ctx.reals.get("y"), Some(&7.5));
    assert!(!ctx.integers.contains_key("y"));
    assert!(!ctx.booleans.contains_key("y"));

    assert_eq!(
        interpret_stmts(
            &[Statement::Assignment {
                comp: cref("y"),
                value: bool_expr(true),
            }],
            &mut ctx,
        ),
        Some(FunctionStmtFlow::Continue),
    );
    assert_eq!(ctx.booleans.get("y"), Some(&true));
    assert!(!ctx.integers.contains_key("y"));
    assert!(!ctx.reals.contains_key("y"));
}

#[test]
fn nonintegral_reassignment_cannot_publish_an_old_integer_function_result() {
    let mut function = ClassDef {
        name: token("changesTypeLane"),
        class_type: ClassType::Function,
        pure: true,
        ..ClassDef::default()
    };
    function
        .components
        .insert("y".to_string(), output_parameter("y"));
    function.algorithms.push(vec![
        Statement::Assignment {
            comp: cref("y"),
            value: int_expr(7),
        },
        Statement::Assignment {
            comp: cref("y"),
            value: real_expr(7.5),
        },
    ]);
    let mut functions = FxHashMap::default();
    functions.insert("changesTypeLane".to_string(), function);
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.functions = Arc::new(functions);

    assert_eq!(
        eval_integer_with_scope(&call("changesTypeLane", vec![]), &ctx, ""),
        None,
    );
}

mod declaration_hardening_tests;
mod identity_hardening_tests;
