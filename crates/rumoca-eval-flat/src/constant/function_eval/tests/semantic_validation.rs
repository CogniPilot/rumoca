use super::arrays_and_declarations::*;
use super::functions_and_defaults::*;
use super::*;
use rumoca_core::Reference;

#[test]
fn unknown_function_control_cannot_hide_a_malformed_call_statement() {
    let mut required = Function::new(
        "test.required_stmt",
        rumoca_core::DefId::new(10_001),
        test_span(),
    );
    required.pure = true;
    required.add_input(real_param("x"));

    let mut outer = Function::new(
        "test.outer_stmt",
        rumoca_core::DefId::new(10_002),
        test_span(),
    );
    outer.pure = true;
    outer.body = vec![Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: var_ref("runtime_condition"),
            stmts: vec![Statement::FunctionCall {
                comp: exact_reference("test.required_stmt", rumoca_core::DefId::new(10_001)),
                args: Vec::new(),
                outputs: Vec::new(),
                span: test_span(),
            }],
        }],
        else_block: None,
        span: test_span(),
    }];

    let mut ctx = EvalContext::structural_preidentity();
    ctx.insert_direct_function_fixture(required);
    let error = eval_function(
        &outer,
        Vec::new(),
        &ctx,
        &EvalLimits::default(),
        0,
        test_span(),
    )
    .expect_err("static statement call shape must be checked before runtime control");
    assert!(matches!(error, EvalError::FunctionError { .. }));
}

#[test]
fn unknown_function_control_cannot_hide_malformed_write_targets() {
    let span = test_span();
    let target_cases = [
        (
            component_reference("x"),
            "function input",
            rumoca_core::DefId::new(10_003),
        ),
        (
            component_reference("undeclared"),
            "undeclared component",
            rumoca_core::DefId::new(10_004),
        ),
    ];

    for (target, case, exposure_def_id) in target_cases {
        let mut func = Function::new(format!("test.invalid_{case}"), exposure_def_id, span);
        func.pure = true;
        func.add_input(integer_param("x"));
        func.add_output(integer_param("y"));
        func.body = vec![Statement::If {
            cond_blocks: vec![StatementBlock {
                cond: var_ref("runtime_condition"),
                stmts: vec![assign(target, integer_literal(1))],
            }],
            else_block: None,
            span,
        }];

        let error = eval_function(
            &func,
            vec![Value::Integer(0)],
            &EvalContext::structural_preidentity(),
            &EvalLimits::default(),
            0,
            span,
        )
        .expect_err("malformed target must outrank unknown branch control");
        assert!(
            matches!(error, EvalError::InvalidSemanticIr { .. }),
            "{case} must be a hard semantic-IR error: {error}"
        );
    }
}

#[test]
fn writable_target_identity_must_match_the_function_signature() {
    let span = test_span();
    let output_id = rumoca_core::DefId::new(81);
    let mut func = Function::new("test.exactOutput", rumoca_core::DefId::new(10_005), span);
    func.pure = true;
    func.add_output(integer_param("y").with_def_id(output_id));
    func.body = vec![assign(
        component_reference_with_def_id("y", output_id),
        integer_literal(7),
    )];
    assert_eq!(
        fold(&func).expect("exact output identity is writable"),
        Value::Integer(7)
    );

    func.body = vec![assign(
        component_reference_with_def_id("y", rumoca_core::DefId::new(82)),
        integer_literal(9),
    )];
    let error = fold(&func).expect_err("a matching spelling cannot replace declaration identity");
    assert!(matches!(error, EvalError::InvalidSemanticIr { .. }));
}

#[test]
fn unknown_function_control_cannot_hide_a_read_only_call_output() {
    let span = test_span();
    let mut callee = Function::new("test.oneOutput", rumoca_core::DefId::new(10_006), span);
    callee.pure = true;
    callee.add_output(integer_param("result"));

    let mut caller = Function::new(
        "test.invalidCallOutput",
        rumoca_core::DefId::new(10_007),
        span,
    );
    caller.pure = true;
    caller.add_input(integer_param("x"));
    caller.add_output(integer_param("y"));
    caller.body = vec![Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: var_ref("runtime_condition"),
            stmts: vec![Statement::FunctionCall {
                comp: exact_reference("test.oneOutput", rumoca_core::DefId::new(10_006)),
                args: Vec::new(),
                outputs: vec![Some(component_reference("x"))],
                span,
            }],
        }],
        else_block: None,
        span,
    }];

    let mut ctx = EvalContext::structural_preidentity();
    ctx.insert_direct_function_fixture(callee);
    let error = eval_function(
        &caller,
        vec![Value::Integer(0)],
        &ctx,
        &EvalLimits::default(),
        0,
        span,
    )
    .expect_err("call results cannot write a function input beneath unknown control");
    assert!(matches!(error, EvalError::InvalidSemanticIr { .. }));
}

#[test]
fn call_output_assignment_uses_declared_arity_and_exact_lvalues() {
    let span = test_span();

    let mut array_result = Function::new("test.arrayResult", rumoca_core::DefId::new(10_008), span);
    array_result.pure = true;
    array_result.add_output(integer_vector_param("values", 2));
    array_result.body = vec![assign(
        component_reference("values"),
        integer_vector(&[4, 5]),
    )];

    let mut array_caller = Function::new("test.arrayCaller", rumoca_core::DefId::new(10_009), span);
    array_caller.pure = true;
    array_caller.add_output(integer_vector_param("y", 2));
    array_caller.body = vec![Statement::FunctionCall {
        comp: exact_reference("test.arrayResult", rumoca_core::DefId::new(10_008)),
        args: Vec::new(),
        outputs: vec![Some(component_reference("y"))],
        span,
    }];
    let mut array_context = EvalContext::structural_preidentity();
    array_context.insert_direct_function_fixture(array_result);
    assert_eq!(
        eval_function(
            &array_caller,
            Vec::new(),
            &array_context,
            &EvalLimits::default(),
            0,
            span,
        )
        .expect("one declared array result is not a multi-result tuple"),
        Value::Array(vec![Value::Integer(4), Value::Integer(5)])
    );

    let mut scalar_result =
        Function::new("test.scalarResult", rumoca_core::DefId::new(10_010), span);
    scalar_result.pure = true;
    scalar_result.add_output(integer_param("value"));
    scalar_result.body = vec![assign(component_reference("value"), integer_literal(9))];

    let mut indexed_caller = Function::new(
        "test.indexedReceiver",
        rumoca_core::DefId::new(10_011),
        span,
    );
    indexed_caller.pure = true;
    indexed_caller.add_output(integer_vector_param("y", 2));
    indexed_caller.body = vec![Statement::FunctionCall {
        comp: exact_reference("test.scalarResult", rumoca_core::DefId::new(10_010)),
        args: Vec::new(),
        outputs: vec![Some(element_target(
            "y",
            vec![Subscript::Index { value: 2, span }],
        ))],
        span,
    }];
    let mut scalar_context = EvalContext::structural_preidentity();
    scalar_context.insert_direct_function_fixture(scalar_result);
    assert_eq!(
        eval_function(
            &indexed_caller,
            Vec::new(),
            &scalar_context,
            &EvalLimits::default(),
            0,
            span,
        )
        .expect("an indexed receiver updates only its selected element"),
        Value::Array(vec![Value::Integer(0), Value::Integer(9)])
    );
}

#[test]
fn call_output_assignment_accepts_omitted_and_trailing_unreceived_results() {
    let span = test_span();
    let mut callee = Function::new("test.threeResults", rumoca_core::DefId::new(10_012), span);
    callee.pure = true;
    for name in ["first", "second", "third"] {
        callee.add_output(integer_param(name));
    }
    callee.body = [1, 2, 3]
        .into_iter()
        .zip(["first", "second", "third"])
        .map(|(value, name)| assign(component_reference(name), integer_literal(value)))
        .collect();

    let mut caller = Function::new(
        "test.omittedReceiver",
        rumoca_core::DefId::new(10_013),
        span,
    );
    caller.pure = true;
    caller.add_output(integer_param("y"));
    caller.body = vec![Statement::FunctionCall {
        comp: exact_reference("test.threeResults", rumoca_core::DefId::new(10_012)),
        args: Vec::new(),
        // MLS §11.2.1.1: omit the first result, receive the second, and do
        // not name the trailing third result at all (m <= n).
        outputs: vec![None, Some(component_reference("y"))],
        span,
    }];

    let mut context = EvalContext::structural_preidentity();
    context.insert_direct_function_fixture(callee);
    assert_eq!(
        eval_function(
            &caller,
            Vec::new(),
            &context,
            &EvalLimits::default(),
            0,
            span,
        )
        .expect("omitted and trailing unreceived results are legal"),
        Value::Integer(2)
    );
}

#[test]
fn call_output_validation_rejects_excess_receivers_before_unknown_control() {
    let span = test_span();
    let mut callee = Function::new("test.twoResults", rumoca_core::DefId::new(10_014), span);
    callee.pure = true;
    callee.add_output(integer_param("first"));
    callee.add_output(integer_param("second"));

    let mut caller = Function::new(
        "test.tooManyReceivers",
        rumoca_core::DefId::new(10_015),
        span,
    );
    caller.pure = true;
    caller.add_output(integer_param("y"));
    caller.body = vec![Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: var_ref("runtime_condition"),
            stmts: vec![Statement::FunctionCall {
                comp: exact_reference("test.twoResults", rumoca_core::DefId::new(10_014)),
                args: Vec::new(),
                outputs: vec![
                    Some(component_reference("y")),
                    None,
                    Some(component_reference("y")),
                ],
                span,
            }],
        }],
        else_block: None,
        span,
    }];

    let mut context = EvalContext::structural_preidentity();
    context.insert_direct_function_fixture(callee);
    assert!(matches!(
        eval_function(
            &caller,
            Vec::new(),
            &context,
            &EvalLimits::default(),
            0,
            span,
        ),
        Err(EvalError::InvalidSemanticIr { .. })
    ));
}

#[test]
fn multi_output_receiver_failure_is_atomic() {
    let span = test_span();
    let mut outputs = IndexMap::new();
    outputs.insert("a".to_string(), Value::Integer(0));
    outputs.insert(
        "array".to_string(),
        Value::Array(vec![Value::Integer(0), Value::Integer(0)]),
    );
    let mut env = FunctionEnv {
        declarations: Vec::new(),
        formal_extents: Vec::new(),
        loop_bindings: Vec::new(),
        inputs: IndexMap::new(),
        outputs,
        locals: IndexMap::new(),
        declared_outputs: ["a".to_string(), "array".to_string()].into_iter().collect(),
        declared_locals: IndexSet::new(),
    };
    let context = EvalContext::structural_preidentity();
    let limits = EvalLimits::default();
    let eval = EvalState {
        ctx: &context,
        limits: &limits,
        depth: 0,
        span,
    };
    let receivers = vec![
        Some(component_reference("a")),
        Some(element_target(
            "array",
            vec![Subscript::Index { value: 3, span }],
        )),
    ];

    assert!(matches!(
        assign_fn_outputs(
            &receivers,
            Value::Array(vec![Value::Integer(11), Value::Integer(22)]),
            2,
            &mut env,
            &eval,
        ),
        Err(EvalError::IndexOutOfBounds { .. })
    ));
    assert_eq!(env.outputs.get("a"), Some(&Value::Integer(0)));
    assert_eq!(
        env.outputs.get("array"),
        Some(&Value::Array(vec![Value::Integer(0), Value::Integer(0)]))
    );
}

#[test]
fn loop_scope_validation_rejects_binder_writes_and_outside_breaks() {
    let span = test_span();
    let mut binder_write = Function::new("test.binderWrite", rumoca_core::DefId::new(10_016), span);
    binder_write.pure = true;
    binder_write.add_output(integer_param("y"));
    binder_write.body = vec![Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: "i".to_string(),
            range: Expression::Range {
                start: Box::new(integer_literal(1)),
                step: None,
                end: Box::new(integer_literal(2)),
                span,
            },
        }],
        equations: vec![assign(component_reference("i"), integer_literal(3))],
        span,
    }];
    assert!(matches!(
        fold(&binder_write),
        Err(EvalError::InvalidSemanticIr { .. })
    ));

    let mut outside_break =
        Function::new("test.outsideBreak", rumoca_core::DefId::new(10_017), span);
    outside_break.pure = true;
    outside_break.add_output(integer_param("y"));
    outside_break.body = vec![Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: var_ref("runtime_condition"),
            stmts: vec![Statement::Break { span }],
        }],
        else_block: None,
        span,
    }];
    assert!(matches!(
        fold(&outside_break),
        Err(EvalError::InvalidSemanticIr { .. })
    ));

    let mut empty_for = Function::new("test.emptyFor", rumoca_core::DefId::new(10_018), span);
    empty_for.pure = true;
    empty_for.add_output(integer_param("y"));
    empty_for.body = vec![Statement::For {
        indices: Vec::new(),
        equations: vec![assign(component_reference("y"), integer_literal(4))],
        span,
    }];
    assert!(matches!(
        fold(&empty_for),
        Err(EvalError::InvalidSemanticIr { .. })
    ));
}

#[test]
fn break_remains_valid_inside_for_and_while_loops() {
    let span = test_span();
    let mut func = Function::new("test.validBreaks", rumoca_core::DefId::new(10_019), span);
    func.pure = true;
    func.add_output(integer_param("y"));
    func.body = vec![
        Statement::While {
            block: StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(false),
                    span,
                },
                stmts: vec![Statement::Break { span }],
            },
            span,
        },
        Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "i".to_string(),
                range: Expression::Range {
                    start: Box::new(integer_literal(1)),
                    step: None,
                    end: Box::new(integer_literal(1)),
                    span,
                },
            }],
            equations: vec![Statement::Break { span }],
            span,
        },
    ];
    assert_eq!(
        fold(&func).expect("loop-local breaks are valid"),
        Value::Integer(0)
    );
}

#[test]
fn loop_binder_shadowing_is_lexical_and_restores_the_outer_binding() {
    // MLS 3.6 §11.2 resolves names through lexically enclosing for-statements
    // from inner-most to outer-most, and §11.2.2.3 defines a multi-index
    // loop as nested loops. Reusing `i` is therefore legal: the inner `i`
    // hides the outer value only for its own body, and the outer value must be
    // restored for the following statement.
    let span = test_span();
    let range = |start, end| Expression::Range {
        start: Box::new(integer_literal(start)),
        step: None,
        end: Box::new(integer_literal(end)),
        span,
    };
    let accumulate_i = || {
        assign(
            component_reference("y"),
            binary(rumoca_core::OpBinary::Add, var_ref("y"), var_ref("i")),
        )
    };

    let mut nested = Function::new(
        "test.nestedBinderShadow",
        rumoca_core::DefId::new(10_020),
        span,
    );
    nested.pure = true;
    nested.add_output(integer_param("y"));
    nested.body = vec![Statement::For {
        indices: vec![ForIndex {
            ident: "i".to_string(),
            range: range(1, 2),
        }],
        equations: vec![
            Statement::For {
                indices: vec![ForIndex {
                    ident: "i".to_string(),
                    range: range(3, 4),
                }],
                equations: vec![accumulate_i()],
                span,
            },
            accumulate_i(),
        ],
        span,
    }];
    assert_eq!(
        fold(&nested).expect("a nested iterator may lexically hide its outer namesake"),
        Value::Integer(17)
    );

    let mut same_list = Function::new(
        "test.sameListBinderShadow",
        rumoca_core::DefId::new(10_021),
        span,
    );
    same_list.pure = true;
    same_list.add_output(integer_param("y"));
    same_list.body = vec![Statement::For {
        indices: vec![
            ForIndex {
                ident: "i".to_string(),
                range: range(1, 2),
            },
            ForIndex {
                ident: "i".to_string(),
                range: range(3, 4),
            },
        ],
        equations: vec![accumulate_i()],
        span,
    }];
    assert_eq!(
        fold(&same_list).expect("multiple iterators have the semantics of nested loops"),
        Value::Integer(14)
    );

    let mut declared_name = Function::new(
        "test.binderShadowsInput",
        rumoca_core::DefId::new(10_022),
        span,
    );
    declared_name.pure = true;
    declared_name.add_input(integer_param("i").with_default(integer_literal(100)));
    declared_name.add_output(integer_param("y"));
    declared_name.body = vec![
        Statement::For {
            indices: vec![ForIndex {
                ident: "i".to_string(),
                range: range(1, 2),
            }],
            equations: vec![accumulate_i()],
            span,
        },
        accumulate_i(),
    ];
    assert_eq!(
        fold(&declared_name)
            .expect("an iterator hides a declaration and restores it after the loop"),
        Value::Integer(103)
    );
}

fn empty_function_env() -> FunctionEnv {
    FunctionEnv {
        declarations: Vec::new(),
        formal_extents: Vec::new(),
        loop_bindings: Vec::new(),
        inputs: IndexMap::new(),
        outputs: IndexMap::new(),
        locals: IndexMap::new(),
        declared_outputs: IndexSet::new(),
        declared_locals: IndexSet::new(),
    }
}

fn retained_empty_element_comprehension(span: Span) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(Expression::Array {
            elements: Vec::new(),
            is_matrix: false,
            span,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: Expression::Range {
                start: Box::new(integer_literal(1)),
                step: None,
                end: Box::new(integer_literal(1)),
                span,
            },
        }],
        filter: Some(Box::new(bool_literal(true))),
        span,
    }
}

fn malformed_empty_index_comprehension(span: Span) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(integer_literal(1)),
        indices: Vec::new(),
        filter: Some(Box::new(bool_literal(false))),
        span,
    }
}

fn dead_comprehension_validation_function(malformed: Expression, span: Span) -> Function {
    let mut function = Function::new(
        "test.emptyComprehensionIndex",
        rumoca_core::DefId::new(10_023),
        span,
    );
    function.pure = true;
    function.add_output(integer_param("y"));
    function.body = vec![Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: bool_literal(false),
            stmts: vec![assign(component_reference("y"), malformed)],
        }],
        else_block: None,
        span,
    }];
    function
}

fn filtered_cartesian_comprehension(span: Span) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(integer_literal(1)),
        indices: ["i", "j"]
            .into_iter()
            .map(|name| rumoca_core::ComprehensionIndex {
                name: name.to_string(),
                range: Expression::Range {
                    start: Box::new(integer_literal(1)),
                    step: None,
                    end: Box::new(integer_literal(3)),
                    span,
                },
            })
            .collect(),
        filter: Some(Box::new(bool_literal(false))),
        span,
    }
}

fn retained_node_budget_comprehension(span: Span) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(Expression::Array {
            elements: (0..8).map(integer_literal).collect(),
            is_matrix: false,
            span,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: Expression::Range {
                start: Box::new(integer_literal(1)),
                step: None,
                end: Box::new(integer_literal(1)),
                span,
            },
        }],
        filter: None,
        span,
    }
}

#[test]
fn filtered_comprehension_retains_a_legitimate_empty_array_element() {
    let span = test_span();
    let context = EvalContext::structural_preidentity();
    let limits = EvalLimits::default();
    let eval = EvalState {
        ctx: &context,
        limits: &limits,
        depth: 0,
        span,
    };
    let env = empty_function_env();
    let comprehension = retained_empty_element_comprehension(span);

    assert_eq!(
        eval_expr_in_function(&comprehension, &env, &eval)
            .expect("a true filter retains the expression value even when it is empty"),
        Value::Array(vec![Value::Array(Vec::new())])
    );

    let malformed = malformed_empty_index_comprehension(span);
    assert!(matches!(
        eval_expr_in_function(&malformed, &env, &eval),
        Err(EvalError::InvalidSemanticIr { .. })
    ));

    let dead_branch = dead_comprehension_validation_function(malformed, span);
    assert!(matches!(
        fold(&dead_branch),
        Err(EvalError::InvalidSemanticIr { .. })
    ));

    let tight_limits = EvalLimits {
        recursion_depth: 8,
        max_iterations: 8,
    };
    let tight_eval = EvalState {
        ctx: &context,
        limits: &tight_limits,
        depth: 0,
        span,
    };
    let cartesian = filtered_cartesian_comprehension(span);
    let work_error = eval_expr_in_function(&cartesian, &env, &tight_eval)
        .expect_err("filtered-out Cartesian products still consume evaluation work");
    assert!(matches!(
        work_error,
        EvalError::UnsupportedExpression { ref kind, .. } if kind.contains("work budget")
    ));

    let retained_nodes = retained_node_budget_comprehension(span);
    let retained_error = eval_expr_in_function(&retained_nodes, &env, &tight_eval)
        .expect_err("a small domain cannot retain a value beyond the node budget");
    assert!(matches!(
        retained_error,
        EvalError::UnsupportedExpression { ref kind, .. } if kind.contains("retained-node budget")
    ));
}

#[test]
fn forbidden_function_statements_are_rejected_before_unknown_control() {
    let span = test_span();
    let statements = [
        (
            Statement::When {
                blocks: vec![StatementBlock {
                    cond: var_ref("runtime_event"),
                    stmts: Vec::new(),
                }],
                span,
            },
            rumoca_core::DefId::new(10_024),
        ),
        (
            Statement::Reinit {
                variable: component_reference("y"),
                value: integer_literal(1),
                span,
            },
            rumoca_core::DefId::new(10_025),
        ),
    ];

    for (forbidden, exposure_def_id) in statements {
        let mut func = Function::new("test.forbiddenStatement", exposure_def_id, span);
        func.pure = true;
        func.add_output(integer_param("y"));
        func.body = vec![Statement::If {
            cond_blocks: vec![StatementBlock {
                cond: var_ref("runtime_condition"),
                stmts: vec![forbidden],
            }],
            else_block: None,
            span,
        }];
        assert!(matches!(
            fold(&func),
            Err(EvalError::InvalidSemanticIr { .. })
        ));
    }
}

#[test]
fn builtin_spelling_in_statement_call_is_invalid_semantic_ir() {
    let span = test_span();
    let mut func = Function::new(
        "test.invalidBuiltinStatement",
        rumoca_core::DefId::new(10_026),
        span,
    );
    func.pure = true;
    func.add_output(integer_param("y"));
    func.body = vec![Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: var_ref("runtime_condition"),
            stmts: vec![Statement::FunctionCall {
                comp: Reference::new("smooth"),
                args: vec![integer_literal(1)],
                outputs: Vec::new(),
                span,
            }],
        }],
        else_block: None,
        span,
    }];

    let error = fold(&func)
        .expect_err("statement calls require exact user-call identity, not builtin spelling");
    assert!(matches!(error, EvalError::InvalidSemanticIr { .. }));
    assert_eq!(error.span(), Some(span));
}

#[test]
fn unknown_function_control_cannot_hide_excess_builtin_or_string_receivers() {
    let span = test_span();
    for (callee, argument, exposure_def_id) in [
        ("abs", integer_literal(-1), rumoca_core::DefId::new(10_027)),
        (
            "String",
            integer_literal(1),
            rumoca_core::DefId::new(10_028),
        ),
    ] {
        let mut func = Function::new("test.excessBuiltinReceivers", exposure_def_id, span);
        func.pure = true;
        func.add_output(integer_param("y"));
        func.body = vec![Statement::If {
            cond_blocks: vec![StatementBlock {
                cond: var_ref("runtime_condition"),
                stmts: vec![Statement::FunctionCall {
                    comp: Reference::new(callee),
                    args: vec![argument],
                    outputs: vec![
                        Some(component_reference("y")),
                        Some(component_reference("y")),
                    ],
                    span,
                }],
            }],
            else_block: None,
            span,
        }];

        let error =
            fold(&func).expect_err("a statically scalar call cannot carry multiple receiver slots");
        assert!(matches!(error, EvalError::InvalidSemanticIr { .. }));
        assert_eq!(error.span(), Some(span));
    }
}

#[test]
fn shaped_defaults_bound_nodes_rank_and_unrepresentable_zero_extents() {
    let output = |name: &str, dimensions: Vec<i64>| {
        let real = rumoca_core::TypeId::new(1);
        let effective = rumoca_core::EffectiveType::new(real, real, dimensions)
            .expect("fixture shape is valid");
        rumoca_core::FunctionParam::new(name, "Real", effective, test_span())
            .with_def_id(fixture_def_id(name))
    };
    let ctx = EvalContext::structural_preidentity();
    let limits = EvalLimits::default();

    let mut oversized = Function::new(
        "test.oversized",
        rumoca_core::DefId::new(10_029),
        test_span(),
    );
    oversized.pure = true;
    oversized.add_output(output("y", vec![50_000, 2]));
    assert!(matches!(
        eval_function(&oversized, Vec::new(), &ctx, &limits, 0, test_span()),
        Err(EvalError::UnsupportedExpression { .. })
    ));

    let mut empty = Function::new("test.empty", rumoca_core::DefId::new(10_030), test_span());
    empty.pure = true;
    empty.add_output(output("y", vec![0, 100_000_000]));
    assert!(matches!(
        eval_function(&empty, Vec::new(), &ctx, &limits, 0, test_span()),
        Err(EvalError::UnsupportedExpression { .. })
    ));

    let mut excessive_rank = Function::new(
        "test.excessive_rank",
        rumoca_core::DefId::new(10_031),
        test_span(),
    );
    excessive_rank.pure = true;
    excessive_rank.add_output(output(
        "y",
        vec![1; super::super::DEFAULT_MATERIALIZED_RANK_BUDGET + 1],
    ));
    assert!(matches!(
        eval_function(&excessive_rank, Vec::new(), &ctx, &limits, 0, test_span()),
        Err(EvalError::UnsupportedExpression { .. })
    ));
}

#[test]
fn pending_formals_shadow_same_named_context_values() {
    // ctx carries b = 99. function f(input Integer a = b, input Integer
    // b = 2): the default of `a` reads the FORMAL b (MLS §12.2 shadowing),
    // so it must wait for b's binding and take 2 — never capture the outer
    // 99 through the not-yet-bound formal.
    let mut func = Function::new("test.shadow", rumoca_core::DefId::new(10_032), Span::DUMMY);
    func.pure = true;
    func.add_input(integer_param("a").with_default(var_ref("b")));
    func.add_input(integer_param("b").with_default(integer_literal(2)));
    func.add_output(integer_param("y"));
    func.body = vec![assign(component_reference("y"), var_ref("a"))];

    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter("b", Value::Integer(99));
    let result =
        eval_function(&func, vec![], &ctx, &EvalLimits::default(), 0, Span::DUMMY).unwrap();
    assert_eq!(result.as_integer(), Some(2));
}

#[test]
fn cyclic_defaults_stay_cyclic_despite_same_named_context_values() {
    // ctx carries a = 1, b = 2, but the formals' defaults a = b, b = a form
    // a cycle among the FORMALS: no §12.4.4 order exists and the call must
    // error, never bind the outer values.
    let mut func = Function::new(
        "test.cyclic_outer",
        rumoca_core::DefId::new(10_033),
        Span::DUMMY,
    );
    func.pure = true;
    func.add_input(integer_param("a").with_default(var_ref("b")));
    func.add_input(integer_param("b").with_default(var_ref("a")));
    func.add_output(integer_param("y"));
    func.body = vec![assign(component_reference("y"), var_ref("a"))];

    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter("a", Value::Integer(1));
    ctx.add_parameter("b", Value::Integer(2));
    assert!(matches!(
        eval_function(&func, vec![], &ctx, &EvalLimits::default(), 0, Span::DUMMY),
        Err(EvalError::CircularDependency { .. })
    ));
}

#[test]
fn default_dependency_is_identity_not_spelling() {
    // The formal b carries DefId 62; the default of a reads the QUALIFIED
    // external declaration `Outer.b`, structured root DefId 63. Identity
    // decides: the read selects no formal, so it consults the context
    // immediately and must not wait for (or take) the formal b's default.
    let mut a = integer_param("a");
    a.def_id = Some(rumoca_core::DefId::new(61));
    let mut b = integer_param("b");
    b.def_id = Some(rumoca_core::DefId::new(62));
    let outer_b_read = Expression::VarRef {
        name: qualified_reference(
            "Outer",
            rumoca_core::DefId::new(63),
            "b",
            rumoca_core::DefId::new(64),
        ),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    let mut func = Function::new(
        "test.identity",
        rumoca_core::DefId::new(10_034),
        Span::DUMMY,
    );
    func.pure = true;
    func.add_input(a.with_default(outer_b_read));
    func.add_input(b.with_default(integer_literal(2)));
    func.add_output(integer_param("y"));
    func.body = vec![assign(
        component_reference("y"),
        Expression::VarRef {
            name: exact_reference("a", rumoca_core::DefId::new(61)),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        },
    )];

    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter("Outer.b", Value::Integer(41));
    ctx.add_def_parameter(rumoca_core::DefId::new(63), Value::Integer(99));
    let result =
        eval_function(&func, vec![], &ctx, &EvalLimits::default(), 0, Span::DUMMY).unwrap();
    assert_eq!(result.as_integer(), Some(99));
}

#[test]
fn identity_absent_read_in_identity_carrying_function_fails_closed() {
    // The function's formals carry declaration identity, but the default of
    // `a` reads a bare name with none. Nothing proves what that read
    // selects, so the default is never ready and the call errors — it must
    // not fall back to spelling against the formal or the context.
    let mut a = integer_param("a");
    a.def_id = Some(rumoca_core::DefId::new(71));
    let mut b = integer_param("b");
    b.def_id = Some(rumoca_core::DefId::new(72));

    let mut func = Function::new(
        "test.mixed_identity",
        rumoca_core::DefId::new(10_035),
        Span::DUMMY,
    );
    func.pure = true;
    func.add_input(a.with_default(Expression::VarRef {
        name: Reference::new("b"),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }));
    func.add_input(b.with_default(integer_literal(2)));
    func.add_output(integer_param("y"));
    func.body = vec![assign(component_reference("y"), var_ref("a"))];

    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter("b", Value::Integer(99));
    assert!(matches!(
        eval_function(&func, vec![], &ctx, &EvalLimits::default(), 0, Span::DUMMY),
        Err(EvalError::InvalidSemanticIr { .. })
    ));
}

#[test]
fn one_declaration_distinguishes_direct_self_cycle_from_independent_binding() {
    let declaration = rumoca_core::DefId::new(81);
    let mut cyclic = Function::new(
        "test.direct_self_cycle",
        rumoca_core::DefId::new(10_036),
        test_span(),
    );
    let integer = rumoca_core::EffectiveType::new(
        rumoca_core::TypeId::new(2),
        rumoca_core::TypeId::new(2),
        Vec::new(),
    )
    .expect("fixture integer type is valid");
    cyclic.add_output(
        rumoca_core::FunctionParam::new("y", "Integer", integer.clone(), test_span())
            .with_def_id(declaration)
            .with_default(Expression::VarRef {
                name: exact_reference("y", declaration),
                subscripts: Vec::new(),
                span: test_span(),
            }),
    );

    let ctx = EvalContext::structural_preidentity();
    assert!(matches!(
        eval_function(
            &cyclic,
            Vec::new(),
            &ctx,
            &EvalLimits::default(),
            0,
            test_span(),
        ),
        Err(EvalError::CircularDependency { .. })
    ));

    let mut independent = Function::new(
        "test.independent_single",
        rumoca_core::DefId::new(10_037),
        test_span(),
    );
    independent.add_output(
        rumoca_core::FunctionParam::new("y", "Integer", integer, test_span())
            .with_def_id(declaration)
            .with_default(integer_literal(7)),
    );
    let value = eval_function(
        &independent,
        Vec::new(),
        &ctx,
        &EvalLimits::default(),
        0,
        test_span(),
    )
    .expect("an independent declaration default is evaluable");
    assert_eq!(value, Value::Integer(7));
}

#[test]
fn duplicate_declaration_name_or_identity_is_refused_before_binding() {
    let mut duplicate_name = Function::new(
        "test.duplicate_name",
        rumoca_core::DefId::new(10_038),
        test_span(),
    );
    let mut first = integer_param("x");
    first.def_id = Some(rumoca_core::DefId::new(91));
    let mut second = integer_param("x");
    second.def_id = Some(rumoca_core::DefId::new(92));
    duplicate_name.add_output(first);
    duplicate_name.add_local(second);

    let ctx = EvalContext::structural_preidentity();
    assert!(matches!(
        eval_function(
            &duplicate_name,
            Vec::new(),
            &ctx,
            &EvalLimits::default(),
            0,
            test_span(),
        ),
        Err(EvalError::InvalidSemanticIr { .. })
    ));

    let mut duplicate_identity = Function::new(
        "test.duplicate_identity",
        rumoca_core::DefId::new(10_039),
        test_span(),
    );
    let shared = rumoca_core::DefId::new(93);
    let mut first = integer_param("x");
    first.def_id = Some(shared);
    let mut second = integer_param("y");
    second.def_id = Some(shared);
    duplicate_identity.add_output(first);
    duplicate_identity.add_local(second);
    assert!(matches!(
        eval_function(
            &duplicate_identity,
            Vec::new(),
            &ctx,
            &EvalLimits::default(),
            0,
            test_span(),
        ),
        Err(EvalError::InvalidSemanticIr { .. })
    ));
}

/// A read of an iterator as Flat delivers it: Resolve issued the iterator its
/// own declaration identity and handed it only to the reads, so the reference
/// carries a root `DefId` that selects no function declaration and no
/// occurrence identity. `Modelica.Electrical.Polyphase.Functions.symmetricOrientation`
/// reads `k` in `orientation := {(k - 1)*2*pi/m for k in 1:m}` exactly so.
fn resolved_iterator_read(name: &str, declaration: u32) -> Expression {
    Expression::VarRef {
        name: exact_reference(name, rumoca_core::DefId::new(declaration)),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

#[test]
fn a_comprehension_iterator_read_with_its_resolved_identity_binds_the_iterator() {
    let mut func = Function::new(
        "test.resolvedComprehensionIterator",
        rumoca_core::DefId::new(10_040),
        Span::DUMMY,
    );
    func.pure = true;
    func.add_output(integer_vector_param("y", 3));
    func.body = vec![assign(
        component_reference("y"),
        Expression::ArrayComprehension {
            expr: Box::new(binary(
                rumoca_core::OpBinary::Mul,
                binary(
                    rumoca_core::OpBinary::Sub,
                    resolved_iterator_read("k", 10_041),
                    integer_literal(1),
                ),
                integer_literal(2),
            )),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "k".to_string(),
                range: Expression::Range {
                    start: Box::new(integer_literal(1)),
                    step: None,
                    end: Box::new(integer_literal(3)),
                    span: Span::DUMMY,
                },
            }],
            filter: None,
            span: Span::DUMMY,
        },
    )];

    assert_eq!(
        fold(&func).expect("the iterator read is bound by the active comprehension index"),
        Value::Array(vec![
            Value::Integer(0),
            Value::Integer(2),
            Value::Integer(4)
        ])
    );
}

#[test]
fn a_for_iterator_read_with_its_resolved_identity_binds_the_iterator() {
    let mut func = Function::new(
        "test.resolvedForIterator",
        rumoca_core::DefId::new(10_042),
        Span::DUMMY,
    );
    func.pure = true;
    func.add_output(integer_param("y"));
    func.body = vec![Statement::For {
        indices: vec![ForIndex {
            ident: "k".to_string(),
            range: Expression::Range {
                start: Box::new(integer_literal(1)),
                step: None,
                end: Box::new(integer_literal(3)),
                span: Span::DUMMY,
            },
        }],
        equations: vec![assign(
            component_reference("y"),
            binary(
                rumoca_core::OpBinary::Add,
                var_ref("y"),
                resolved_iterator_read("k", 10_043),
            ),
        )],
        span: Span::DUMMY,
    }];

    assert_eq!(
        fold(&func).expect("the iterator read is bound by the active for index"),
        Value::Integer(6)
    );
}

#[test]
fn an_occurrence_bearing_read_is_never_an_iterator() {
    // An occurrence identity names a Flat model variable, which no iterator
    // is; the spelling match alone must not capture it.
    let mut func = Function::new(
        "test.occurrenceReadInsideIterator",
        rumoca_core::DefId::new(10_044),
        Span::DUMMY,
    );
    func.pure = true;
    func.add_output(integer_vector_param("y", 1));
    func.body = vec![assign(
        component_reference("y"),
        Expression::ArrayComprehension {
            expr: Box::new(Expression::VarRef {
                name: exact_reference("k", rumoca_core::DefId::new(10_045))
                    .with_instance_id(rumoca_core::InstanceId::new(10_046)),
                subscripts: Vec::new(),
                span: Span::DUMMY,
            }),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "k".to_string(),
                range: Expression::Range {
                    start: Box::new(integer_literal(1)),
                    step: None,
                    end: Box::new(integer_literal(1)),
                    span: Span::DUMMY,
                },
            }],
            filter: None,
            span: Span::DUMMY,
        },
    )];

    assert!(matches!(
        fold(&func),
        Err(EvalError::UnknownVariable { .. })
    ));
}
