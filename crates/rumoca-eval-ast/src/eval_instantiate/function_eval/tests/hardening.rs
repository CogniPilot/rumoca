use super::*;

fn constant_layout_function(id: u32) -> ast::ClassDef {
    let mut function = ast::ClassDef {
        def_id: Some(rumoca_core::DefId::new(id)),
        name: token("layout"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    function
        .components
        .insert("a".to_string(), input_int_component("a"));
    let mut b = input_int_component("b");
    b.binding = Some(int_expr(2));
    b.has_explicit_binding = true;
    function.components.insert("b".to_string(), b);
    function
        .components
        .insert("y".to_string(), output_int_component("y"));
    function.algorithms.push(vec![assignment("y", int_expr(7))]);
    function
}

fn evaluate_layout(function: ast::ClassDef, arguments: Vec<ast::Expression>) -> Option<i64> {
    let tree = tree_with_function("layout", function);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    try_eval_integer_expr(&ctx, &func_call("layout", arguments))
}

#[test]
fn recovery_statement_refuses_instantiation_time_function_evaluation() {
    let mut locals = super::super::MixedLocals::default();
    let tree = ast::ClassTree::new();
    let mod_env = ast::ModificationEnvironment::new();
    let components = IndexMap::default();
    let env = super::super::IntegerEvalEnv {
        mod_env: &mod_env,
        effective_components: &components,
        tree: &tree,
        resolve_class_components: no_op_resolve_class_components,
        work_budget: None,
    };

    assert_eq!(
        super::super::interpret_function_statements(
            &[
                assignment("must_not_start", int_expr(6)),
                ast::Statement::Empty,
                assignment("must_not_run", int_expr(7)),
            ],
            env,
            0,
            &mut locals,
        ),
        None
    );
    assert!(
        !locals.ints.contains_key("must_not_start"),
        "the checked root must reject recovery before publishing a prefix"
    );
    assert!(
        !locals.ints.contains_key("must_not_run"),
        "recovery must stop evaluation before later state is committed"
    );
    assert!(
        super::super::interpret_function_statements(&[], env, 0, &mut locals).is_some(),
        "a genuinely empty algorithm section remains valid"
    );
}

#[test]
fn invalid_ignored_assert_fields_refuse_instantiation_time_evaluation() {
    let assignment = ast::Expression::Binary {
        op: rumoca_core::OpBinary::Assign,
        lhs: Arc::new(int_expr(1)),
        rhs: Arc::new(int_expr(2)),
        span: test_span(),
    };
    let end = ast::Expression::Terminal {
        terminal_type: ast::TerminalType::End,
        token: token("end"),
        span: test_span(),
    };
    for invalid in [assignment, end] {
        let statement = ast::Statement::Assert {
            condition: bool_expr(true),
            message: invalid,
            level: None,
        };
        let mut locals = super::super::MixedLocals::default();
        assert_eq!(interpret_test_statements(&[statement], &mut locals), None);
    }
}

#[test]
fn recovery_after_an_assignment_cannot_publish_a_structural_result() {
    let function_id = rumoca_core::DefId::new(44);
    let mut function = ast::ClassDef {
        def_id: Some(function_id),
        name: token("recovered"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    function
        .components
        .insert("y".to_string(), output_int_component("y"));
    function
        .algorithms
        .push(vec![assignment("y", int_expr(7)), ast::Statement::Empty]);
    let tree = tree_with_function("recovered", function);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(
        try_eval_integer_expr(&ctx, &func_call("recovered", vec![])),
        None
    );
}

#[test]
fn recovery_condition_with_equal_outcomes_cannot_publish_a_structural_result() {
    let function_id = rumoca_core::DefId::new(45);
    let recovery = ast::Expression::Empty { span: test_span() };
    let mut function = ast::ClassDef {
        def_id: Some(function_id),
        name: token("recoveredCondition"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    function
        .components
        .insert("y".to_string(), output_int_component("y"));
    function.algorithms.push(vec![assignment(
        "y",
        ast::Expression::If {
            branches: vec![(recovery, int_expr(1))],
            else_branch: Arc::new(int_expr(1)),
            span: test_span(),
        },
    )]);
    let tree = tree_with_function("recoveredCondition", function);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(
        try_eval_integer_expr(&ctx, &func_call("recoveredCondition", vec![])),
        None
    );
}

#[test]
fn recovery_in_declaration_or_default_binding_refuses_the_whole_function_fold() {
    let nested_recovery = || {
        if_expr(
            vec![(bool_expr(true), int_expr(1))],
            ast::Expression::Empty { span: test_span() },
        )
    };

    let function_id = rumoca_core::DefId::new(46);
    let mut output_binding = ast::ClassDef {
        def_id: Some(function_id),
        name: token("recoveredOutputBinding"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    let mut output = output_int_component_initialized("y", 0);
    output.binding = Some(nested_recovery());
    output_binding.components.insert("y".to_string(), output);
    let tree = tree_with_function("recoveredOutputBinding", output_binding);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    assert_eq!(
        try_eval_integer_expr(&ctx, &func_call("recoveredOutputBinding", vec![])),
        None,
    );

    let function_id = rumoca_core::DefId::new(47);
    let mut input_default = ast::ClassDef {
        def_id: Some(function_id),
        name: token("recoveredInputDefault"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    let mut input = input_int_component("unused");
    input.binding = Some(nested_recovery());
    input.has_explicit_binding = true;
    input_default.components.insert("unused".to_string(), input);
    input_default
        .components
        .insert("y".to_string(), output_int_component("y"));
    input_default
        .algorithms
        .push(vec![assignment("y", int_expr(1))]);
    let tree = tree_with_function("recoveredInputDefault", input_default);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    assert_eq!(
        try_eval_integer_expr(&ctx, &func_call("recoveredInputDefault", vec![])),
        None,
        "an unused recovery default must not be silently discarded",
    );
}

#[test]
fn context_only_syntax_in_unused_defaults_refuses_the_whole_function_fold() {
    let forms = [
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs: Arc::new(int_expr(1)),
            rhs: Arc::new(int_expr(2)),
            span: test_span(),
        },
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            token: token("end"),
            span: test_span(),
        },
    ];
    for (offset, invalid) in forms.into_iter().enumerate() {
        let function_id = rumoca_core::DefId::new(60 + offset as u32);
        let mut function = ast::ClassDef {
            def_id: Some(function_id),
            name: token("invalidDefault"),
            class_type: rumoca_core::ClassType::Function,
            pure: true,
            ..ast::ClassDef::default()
        };
        let mut input = input_int_component("unused");
        input.binding = Some(invalid);
        input.has_explicit_binding = true;
        function.components.insert("unused".to_string(), input);
        function
            .components
            .insert("y".to_string(), output_int_component("y"));
        function.algorithms.push(vec![assignment("y", int_expr(1))]);
        let tree = tree_with_function("invalidDefault", function);
        let ctx = InstantiateEvalCtx {
            tree: &tree,
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &IndexMap::default(),
            resolve_class_components: no_op_resolve_class_components,
        };
        assert_eq!(
            try_eval_integer_expr(&ctx, &func_call("invalidDefault", vec![])),
            None,
        );
    }
}

#[test]
fn malformed_explicit_binding_metadata_refuses_an_unrelated_output_fold() {
    for (offset, (binding, explicit)) in [(None, true), (Some(int_expr(5)), false)]
        .into_iter()
        .enumerate()
    {
        let function_id = rumoca_core::DefId::new(70 + offset as u32);
        let mut function = ast::ClassDef {
            def_id: Some(function_id),
            name: token("malformedBinding"),
            class_type: rumoca_core::ClassType::Function,
            pure: true,
            ..ast::ClassDef::default()
        };
        let mut unused = local_int_component("unused", 5);
        unused.binding = binding;
        unused.has_explicit_binding = explicit;
        function.components.insert("unused".to_string(), unused);
        function
            .components
            .insert("y".to_string(), output_int_component("y"));
        function.algorithms.push(vec![assignment("y", int_expr(1))]);
        let tree = tree_with_function("malformedBinding", function);
        let ctx = InstantiateEvalCtx {
            tree: &tree,
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &IndexMap::default(),
            resolve_class_components: no_op_resolve_class_components,
        };
        assert_eq!(
            try_eval_integer_expr(&ctx, &func_call("malformedBinding", vec![])),
            None,
        );
    }
}

#[test]
fn instantiation_function_call_binding_requires_an_exact_argument_layout() {
    assert_eq!(
        evaluate_layout(
            constant_layout_function(80),
            vec![int_expr(1), named_arg("b", int_expr(3))]
        ),
        Some(7)
    );
    assert_eq!(
        evaluate_layout(
            constant_layout_function(81),
            vec![named_arg("a", int_expr(1))]
        ),
        Some(7)
    );
    for (offset, malformed) in [
        vec![named_arg("b", int_expr(3))],
        vec![int_expr(1), named_arg("unknown", int_expr(3))],
        vec![
            int_expr(1),
            named_arg("b", int_expr(2)),
            named_arg("b", int_expr(3)),
        ],
        vec![named_arg("b", int_expr(2)), int_expr(1)],
        vec![int_expr(1), int_expr(2), int_expr(3)],
    ]
    .into_iter()
    .enumerate()
    {
        assert_eq!(
            evaluate_layout(constant_layout_function(82 + offset as u32), malformed),
            None
        );
    }
}

#[test]
fn instantiation_loop_budget_is_shared_across_nested_loops() {
    let mut exact = super::super::MixedLocals::default();
    let exact_loop = for_statement(
        "i",
        range_expr(
            1,
            None,
            crate::function_budget::MAX_AST_FUNCTION_EVAL_WORK as i64,
        ),
        vec![assignment("ran", int_expr(1))],
    );
    assert!(interpret_test_statements(&[exact_loop], &mut exact).is_some());

    let mut too_large = super::super::MixedLocals::default();
    let too_large_loop = for_statement(
        "i",
        range_expr(
            1,
            None,
            crate::function_budget::MAX_AST_FUNCTION_EVAL_WORK as i64 + 1,
        ),
        vec![assignment("must_not_run", int_expr(1))],
    );
    assert_eq!(
        interpret_test_statements(&[too_large_loop], &mut too_large),
        None
    );
    assert!(!too_large.ints.contains_key("must_not_run"));

    let nested = for_statement(
        "i",
        range_expr(1, None, 65),
        vec![for_statement(
            "j",
            range_expr(1, None, 65),
            vec![assignment("visited", int_expr(1))],
        )],
    );
    assert_eq!(
        interpret_test_statements(&[nested], &mut super::super::MixedLocals::default()),
        None
    );
}

#[test]
fn instantiation_loop_budget_is_shared_by_recursive_function_calls() {
    let function_id = rumoca_core::DefId::new(96);
    let mut function = ast::ClassDef {
        def_id: Some(function_id),
        name: token("budgetRecursive"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    function
        .components
        .insert("n".to_string(), input_int_component("n"));
    function
        .components
        .insert("y".to_string(), output_int_component("y"));
    function.algorithms.push(vec![
        for_statement(
            "i",
            range_expr(1, None, 2100),
            vec![assignment("y", int_expr(1))],
        ),
        assignment(
            "y",
            if_expr(
                vec![(
                    ast::Expression::Binary {
                        op: rumoca_core::OpBinary::Gt,
                        lhs: Arc::new(ast::Expression::ComponentReference(cref("n"))),
                        rhs: Arc::new(int_expr(0)),
                        span: test_span(),
                    },
                    func_call(
                        "budgetRecursive",
                        vec![ast::Expression::Binary {
                            op: rumoca_core::OpBinary::Sub,
                            lhs: Arc::new(ast::Expression::ComponentReference(cref("n"))),
                            rhs: Arc::new(int_expr(1)),
                            span: test_span(),
                        }],
                    ),
                )],
                int_expr(1),
            ),
        ),
    ]);
    let tree = tree_with_function("budgetRecursive", function);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(
        try_eval_integer_expr(&ctx, &func_call("budgetRecursive", vec![int_expr(0)])),
        Some(1)
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &func_call("budgetRecursive", vec![int_expr(1)])),
        None
    );
}

#[test]
fn instantiation_for_loop_honors_steps_and_restores_nested_same_name_bindings() {
    let inner = for_statement(
        "i",
        range_expr(9, None, 9),
        vec![assignment(
            "inner",
            ast::Expression::ComponentReference(cref("i")),
        )],
    );
    let outer = for_statement(
        "i",
        range_expr(5, Some(-2), 1),
        vec![
            inner,
            assignment("outer", ast::Expression::ComponentReference(cref("i"))),
        ],
    );
    let mut locals = super::super::MixedLocals::default();
    locals.ints.insert("i".to_string(), 42);
    locals.bools.insert("i".to_string(), true);
    locals.reals.insert("i".to_string(), 42.0);
    locals.real_arrays.insert("i".to_string(), vec![42.0, 43.0]);

    assert!(interpret_test_statements(&[outer], &mut locals).is_some());
    assert_eq!(locals.ints.get("inner"), Some(&9));
    assert_eq!(locals.ints.get("outer"), Some(&1));
    assert_eq!(locals.ints.get("i"), Some(&42));
    assert_eq!(locals.bools.get("i"), Some(&true));
    assert_eq!(locals.reals.get("i"), Some(&42.0));
    assert_eq!(locals.real_arrays.get("i"), Some(&vec![42.0, 43.0]));
}

#[test]
fn instantiation_for_loop_rejects_zero_step_and_handles_extreme_endpoints() {
    let mut locals = super::super::MixedLocals::default();
    let statement = for_statement(
        "i",
        range_expr(1, Some(0), 2),
        vec![assignment("ran", int_expr(1))],
    );
    assert_eq!(interpret_test_statements(&[statement], &mut locals), None);
    assert!(!locals.ints.contains_key("ran"));

    for (range, expected) in [
        (range_expr(i64::MAX - 1, Some(2), i64::MAX), i64::MAX - 1),
        (range_expr(i64::MIN + 1, Some(-2), i64::MIN), i64::MIN + 1),
    ] {
        let mut locals = super::super::MixedLocals::default();
        let statement = for_statement(
            "i",
            range,
            vec![assignment(
                "last",
                ast::Expression::ComponentReference(cref("i")),
            )],
        );
        assert!(interpret_test_statements(&[statement], &mut locals).is_some());
        assert_eq!(locals.ints.get("last"), Some(&expected));
    }

    let mut locals = super::super::MixedLocals::default();
    let endpoint = for_statement(
        "i",
        range_expr(i64::MAX - 1, Some(1), i64::MAX),
        vec![assignment(
            "last",
            ast::Expression::ComponentReference(cref("i")),
        )],
    );
    assert!(interpret_test_statements(&[endpoint], &mut locals).is_some());
    assert_eq!(locals.ints.get("last"), Some(&i64::MAX));
}

#[test]
fn instantiation_loop_iterator_is_read_only_and_restored_on_failure_or_return() {
    let mut locals = super::super::MixedLocals::default();
    locals.ints.insert("i".to_string(), 42);
    let writes_index = for_statement(
        "i",
        range_expr(1, None, 1),
        vec![assignment("i", int_expr(9))],
    );
    assert_eq!(
        interpret_test_statements(&[writes_index], &mut locals),
        None,
    );
    assert_eq!(locals.ints.get("i"), Some(&42));

    let returns = for_statement(
        "i",
        range_expr(1, None, 1),
        vec![ast::Statement::Return {
            token: token("return"),
        }],
    );
    assert_eq!(
        interpret_test_statements(&[returns], &mut locals),
        Some(crate::function_control::FunctionStmtFlow::Return),
    );
    assert_eq!(locals.ints.get("i"), Some(&42));
}

#[test]
fn call_statement_cannot_assign_an_active_instantiation_loop_iterator() {
    let call = ast::Statement::FunctionCall {
        comp: cref("integer"),
        args: vec![real_expr("2.5")],
        outputs: vec![ast::Expression::ComponentReference(cref("i"))],
    };
    let statement = for_statement("i", range_expr(1, None, 1), vec![call]);
    let mut locals = super::super::MixedLocals::default();

    assert_eq!(interpret_test_statements(&[statement], &mut locals), None);
    assert!(!locals.ints.contains_key("i"));
}
