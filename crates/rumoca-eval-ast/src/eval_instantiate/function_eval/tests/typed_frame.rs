use super::*;

fn type_name(spelling: &str) -> ast::Name {
    ast::Name {
        name: vec![token(spelling)],
        def_id: None,
    }
}

/// A type reference carrying the registered predefined `Real` identity.
fn real_type_name() -> ast::Name {
    ast::Name {
        name: vec![token("Real")],
        def_id: Some(predefined_real_id()),
    }
}

fn real_vector_input_component(name: &str) -> ast::Component {
    let mut component = ast::Component {
        name: name.to_string(),
        causality: rumoca_core::Causality::Input(token("input")),
        type_name: real_type_name(),
        ..ast::Component::empty_with_span(test_span())
    };
    component.shape_expr = vec![ast::Subscript::Range {
        token: rumoca_core::Token::default(),
    }];
    component
}

#[test]
fn declared_vector_extent_distinguishes_colon_from_parser_recovery() {
    let colon = real_vector_input_component("colon");
    let mut recovery = real_vector_input_component("recovery");
    recovery.shape_expr = vec![ast::Subscript::Empty];

    assert_eq!(declared_vector_extent(&colon), DeclaredExtent::Any);
    assert_eq!(declared_vector_extent(&recovery), DeclaredExtent::Unproved);
}

fn real_input_component_with_default(name: &str, default: ast::Expression) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        causality: rumoca_core::Causality::Input(token("input")),
        type_name: real_type_name(),
        binding: Some(default),
        has_explicit_binding: true,
        ..ast::Component::empty_with_span(test_span())
    }
}

fn local_int_component_bound(name: &str, binding: ast::Expression) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        binding: Some(binding),
        has_explicit_binding: true,
        ..ast::Component::empty_with_span(test_span())
    }
}

fn real_array_expr(values: &[&str]) -> ast::Expression {
    ast::Expression::Array {
        elements: values.iter().map(|value| real_expr(value)).collect(),
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    }
}

/// The MSL 4.1.0 `Vectors.isEqual` body verbatim in structure: protected
/// `n = size(v1, 1)` and `i = 1`, a length-match if, a bounded while with an
/// early exit, indexed reads and `abs`.
fn vec_equal_function(function_id: rumoca_core::DefId) -> ast::ClassDef {
    let mut function = ast::ClassDef {
        def_id: Some(function_id),
        name: token("vecEqual"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    function
        .components
        .insert("v1".to_string(), real_vector_input_component("v1"));
    function
        .components
        .insert("v2".to_string(), real_vector_input_component("v2"));
    function.components.insert(
        "eps".to_string(),
        real_input_component_with_default("eps", real_expr("0.0")),
    );
    function
        .components
        .insert("result".to_string(), output_bool_component("result"));
    function.components.insert(
        "n".to_string(),
        local_int_component_bound(
            "n",
            func_call(
                "size",
                vec![ast::Expression::ComponentReference(cref("v1")), int_expr(1)],
            ),
        ),
    );
    function
        .components
        .insert("i".to_string(), local_int_component("i", 1));
    function.algorithms.push(vec![
        assignment("result", bool_expr(false)),
        ast::Statement::If {
            cond_blocks: vec![ast::StatementBlock {
                cond: eq_expr(
                    func_call(
                        "size",
                        vec![ast::Expression::ComponentReference(cref("v2")), int_expr(1)],
                    ),
                    ast::Expression::ComponentReference(cref("n")),
                ),
                stmts: vec![assignment("result", bool_expr(true)), vec_equal_scan()],
            }],
            else_block: None,
        },
    ]);
    function
}

/// The early-exit element scan of the `vecEqual` body: `while i <= n loop
/// if abs(v1[i] - v2[i]) > eps then result := false; i := n; end if;
/// i := i + 1; end while`.
fn vec_equal_scan() -> ast::Statement {
    let element_read = |vector: &str| {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: token(vector),
                subs: Some(vec![ast::Subscript::Expression(
                    ast::Expression::ComponentReference(cref("i")),
                )]),
                def_id: None,
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        })
    };
    let mismatch = ast::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Arc::new(func_call(
            "abs",
            vec![ast::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Arc::new(element_read("v1")),
                rhs: Arc::new(element_read("v2")),
                span: rumoca_core::Span::DUMMY,
            }],
        )),
        rhs: Arc::new(ast::Expression::ComponentReference(cref("eps"))),
        span: rumoca_core::Span::DUMMY,
    };
    ast::Statement::While(ast::StatementBlock {
        cond: ast::Expression::Binary {
            op: rumoca_core::OpBinary::Le,
            lhs: Arc::new(ast::Expression::ComponentReference(cref("i"))),
            rhs: Arc::new(ast::Expression::ComponentReference(cref("n"))),
            span: rumoca_core::Span::DUMMY,
        },
        stmts: vec![
            ast::Statement::If {
                cond_blocks: vec![ast::StatementBlock {
                    cond: mismatch,
                    stmts: vec![
                        assignment("result", bool_expr(false)),
                        assignment("i", ast::Expression::ComponentReference(cref("n"))),
                    ],
                }],
                else_block: None,
            },
            assignment(
                "i",
                add_expr(ast::Expression::ComponentReference(cref("i")), int_expr(1)),
            ),
        ],
    })
}

fn structural_int_of_condition(condition: ast::Expression) -> ast::Expression {
    if_expr(vec![(condition, int_expr(1))], int_expr(0))
}

#[test]
fn real_vector_function_interprets_through_the_typed_frame() {
    let tree = tree_with_function("vecEqual", vec_equal_function(rumoca_core::DefId::new(9)));
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    // Equal vectors, eps omitted (declaration default binds).
    let equal = func_call(
        "vecEqual",
        vec![
            real_array_expr(&["1.0", "2.0"]),
            real_array_expr(&["1.0", "2.0"]),
        ],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(equal)),
        Some(1)
    );

    // Unequal values.
    let unequal = func_call(
        "vecEqual",
        vec![
            real_array_expr(&["1.0", "2.0"]),
            real_array_expr(&["1.0", "3.0"]),
        ],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(unequal)),
        Some(0)
    );

    // Unequal lengths.
    let length_mismatch = func_call(
        "vecEqual",
        vec![real_array_expr(&["1.0", "2.0"]), real_array_expr(&["1.0"])],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(length_mismatch)),
        Some(0)
    );

    // Explicit eps tolerance.
    let tolerant = func_call(
        "vecEqual",
        vec![
            real_array_expr(&["1.0", "2.0"]),
            real_array_expr(&["1.0005", "2.0"]),
            real_expr("0.01"),
        ],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(tolerant)),
        Some(1)
    );
}

#[test]
fn real_vector_function_fails_closed_on_rank_type_and_purity() {
    let mut function = vec_equal_function(rumoca_core::DefId::new(9));
    let tree = tree_with_function("vecEqual", function.clone());
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    // Rank-2 (matrix) actual fails closed.
    let matrix = ast::Expression::Array {
        elements: vec![real_array_expr(&["1.0"]), real_array_expr(&["2.0"])],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let rank2 = func_call("vecEqual", vec![matrix, real_array_expr(&["1.0"])]);
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(rank2)),
        None
    );

    // Boolean-element actual fails closed.
    let boolean_array = ast::Expression::Array {
        elements: vec![bool_expr(true), bool_expr(false)],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let wrong_type = func_call(
        "vecEqual",
        vec![boolean_array, real_array_expr(&["1.0", "2.0"])],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(wrong_type)),
        None
    );

    // Impure and external declarations are refused before interpretation.
    function.pure = false;
    let impure_tree = tree_with_function("vecEqual", function.clone());
    let impure_ctx = InstantiateEvalCtx {
        tree: &impure_tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let call = func_call(
        "vecEqual",
        vec![real_array_expr(&["1.0"]), real_array_expr(&["1.0"])],
    );
    assert_eq!(
        try_eval_integer_expr(&impure_ctx, &structural_int_of_condition(call.clone())),
        None
    );

    function.pure = true;
    function.external = Some(ast::ExternalFunction::default());
    let external_tree = tree_with_function("vecEqual", function);
    let external_ctx = InstantiateEvalCtx {
        tree: &external_tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    assert_eq!(
        try_eval_integer_expr(&external_ctx, &structural_int_of_condition(call)),
        None
    );
}

#[test]
fn unrelated_integer_function_accepts_vector_arguments() {
    // An array-input function unrelated to any comparison shape: its result
    // is the vector length plus the first element floored into the sum via
    // integer arithmetic on size alone.
    let function_id = rumoca_core::DefId::new(11);
    let mut function = ast::ClassDef {
        def_id: Some(function_id),
        name: token("lenOf"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    function
        .components
        .insert("v".to_string(), real_vector_input_component("v"));
    function
        .components
        .insert("y".to_string(), output_int_component("y"));
    function.algorithms.push(vec![assignment(
        "y",
        func_call(
            "size",
            vec![ast::Expression::ComponentReference(cref("v")), int_expr(1)],
        ),
    )]);
    let tree = tree_with_function("lenOf", function);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    let call = func_call("lenOf", vec![real_array_expr(&["4.0", "5.0", "6.0"])]);
    assert_eq!(
        try_eval_integer_expr(&ctx, &add_expr(call, int_expr(1))),
        Some(4)
    );
}

#[test]
fn typed_frame_binding_enforces_the_declared_formal_contract() {
    // Scalar formal refusing a vector actual.
    let mut scalar_formal = vec_equal_function(rumoca_core::DefId::new(9));
    let scalar_v1 = ast::Component {
        name: "v1".to_string(),
        causality: rumoca_core::Causality::Input(token("input")),
        type_name: real_type_name(),
        ..ast::Component::empty_with_span(test_span())
    };
    scalar_formal.components.insert("v1".to_string(), scalar_v1);
    let tree = tree_with_function("vecEqual", scalar_formal);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let call = func_call(
        "vecEqual",
        vec![real_array_expr(&["1.0"]), real_array_expr(&["1.0"])],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(call)),
        None
    );

    // Non-Real vector formal refusing a Real vector actual.
    let mut integer_formal = vec_equal_function(rumoca_core::DefId::new(9));
    let mut integer_v1 = ast::Component {
        name: "v1".to_string(),
        causality: rumoca_core::Causality::Input(token("input")),
        type_name: type_name("Integer"),
        ..ast::Component::empty_with_span(test_span())
    };
    integer_v1.shape_expr = vec![ast::Subscript::Range {
        token: rumoca_core::Token::default(),
    }];
    integer_formal
        .components
        .insert("v1".to_string(), integer_v1);
    let tree = tree_with_function("vecEqual", integer_formal);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let call = func_call(
        "vecEqual",
        vec![real_array_expr(&["1.0"]), real_array_expr(&["1.0"])],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(call)),
        None
    );

    // Fixed declared extent: two accepted, three refused.
    let mut fixed_formal = vec_equal_function(rumoca_core::DefId::new(9));
    let mut fixed_v1 = ast::Component {
        name: "v1".to_string(),
        causality: rumoca_core::Causality::Input(token("input")),
        type_name: real_type_name(),
        ..ast::Component::empty_with_span(test_span())
    };
    fixed_v1.shape_expr = vec![ast::Subscript::Expression(int_expr(2))];
    fixed_formal.components.insert("v1".to_string(), fixed_v1);
    let tree = tree_with_function("vecEqual", fixed_formal);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let matching = func_call(
        "vecEqual",
        vec![
            real_array_expr(&["1.0", "2.0"]),
            real_array_expr(&["1.0", "2.0"]),
        ],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(matching)),
        Some(1)
    );
    let oversized = func_call(
        "vecEqual",
        vec![
            real_array_expr(&["1.0", "2.0", "3.0"]),
            real_array_expr(&["1.0", "2.0", "3.0"]),
        ],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(oversized)),
        None
    );
}

#[test]
fn resolved_user_declarations_preempt_predefined_size_and_abs() {
    // A user function spelled `size` whose resolved identity is attached to
    // the call: the user body answers, never the predefined vector length.
    let size_id = rumoca_core::DefId::new(21);
    let mut user_size = ast::ClassDef {
        def_id: Some(size_id),
        name: token("size"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    user_size
        .components
        .insert("v".to_string(), real_vector_input_component("v"));
    user_size
        .components
        .insert("d".to_string(), input_int_component("d"));
    user_size
        .components
        .insert("y".to_string(), output_int_component("y"));
    user_size
        .algorithms
        .push(vec![assignment("y", int_expr(99))]);
    let tree = tree_with_function("size", user_size);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let mut resolved_call_ref = cref("size");
    resolved_call_ref.parts[0].def_id = Some(size_id);
    let resolved_call = ast::Expression::FunctionCall {
        comp: resolved_call_ref,
        args: vec![real_array_expr(&["1.0", "2.0", "3.0"]), int_expr(1)],
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(try_eval_integer_expr(&ctx, &resolved_call), Some(99));
}

#[test]
fn non_finite_real_arithmetic_refuses_to_fold() {
    // function bad(output Boolean r) algorithm r := 1.0 / 0.0 > 2.0;
    let make = |body_value: ast::Expression| {
        let mut function = ast::ClassDef {
            def_id: Some(rumoca_core::DefId::new(31)),
            name: token("bad"),
            class_type: rumoca_core::ClassType::Function,
            pure: true,
            ..ast::ClassDef::default()
        };
        function
            .components
            .insert("r".to_string(), output_bool_component("r"));
        function.algorithms.push(vec![assignment("r", body_value)]);
        function
    };

    let division = make(ast::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Arc::new(ast::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs: Arc::new(real_expr("1.0")),
            rhs: Arc::new(real_expr("0.0")),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Arc::new(real_expr("2.0")),
        span: rumoca_core::Span::DUMMY,
    });
    let overflow = make(ast::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Arc::new(ast::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Arc::new(real_expr("1.0e308")),
            rhs: Arc::new(real_expr("10.0")),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Arc::new(real_expr("2.0")),
        span: rumoca_core::Span::DUMMY,
    });
    for function in [division, overflow] {
        let tree = tree_with_function("bad", function);
        let ctx = InstantiateEvalCtx {
            tree: &tree,
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &IndexMap::default(),
            resolve_class_components: no_op_resolve_class_components,
        };
        assert_eq!(
            try_eval_integer_expr(
                &ctx,
                &structural_int_of_condition(func_call("bad", Vec::new()))
            ),
            None,
            "a non-finite structural result must refuse to fold"
        );
    }
}

#[test]
fn colon_range_vector_declaration_accepts_any_length() {
    // The parser spells `Real v[:]` as the colon Range subscript, not the
    // synthetic Empty placeholder; both must accept any actual length.
    let mut function = vec_equal_function(rumoca_core::DefId::new(9));
    for input in ["v1", "v2"] {
        let mut component = real_vector_input_component(input);
        component.shape_expr = vec![ast::Subscript::Range { token: token(":") }];
        function.components.insert(input.to_string(), component);
    }
    let tree = tree_with_function("vecEqual", function);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let call = func_call(
        "vecEqual",
        vec![
            real_array_expr(&["1.0", "2.0", "3.0"]),
            real_array_expr(&["1.0", "2.0", "3.0"]),
        ],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(call)),
        Some(1)
    );
}

#[test]
fn name_only_real_spelling_without_identity_fails_closed() {
    // A declaration spelled `Real` whose type reference carries no resolved
    // identity must not acquire predefined Real semantics: the vector actual
    // is refused and the call does not fold.
    let mut function = vec_equal_function(rumoca_core::DefId::new(9));
    for input in ["v1", "v2"] {
        let mut component = real_vector_input_component(input);
        component.type_name = type_name("Real");
        function.components.insert(input.to_string(), component);
    }
    let tree = tree_with_function("vecEqual", function);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let call = func_call(
        "vecEqual",
        vec![real_array_expr(&["1.0"]), real_array_expr(&["1.0"])],
    );
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(call)),
        None
    );
}

#[test]
fn non_finite_literal_refuses_to_feed_a_structural_boolean() {
    // 1e400 parses to Inf; the shared scalar evaluator must refuse the
    // literal before any comparison can fold it.
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let condition = ast::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Arc::new(real_expr("1e400")),
        rhs: Arc::new(real_expr("2.0")),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        try_eval_integer_expr(&ctx, &structural_int_of_condition(condition)),
        None
    );
}
