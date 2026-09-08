use super::*;

fn evaluate_array_dimensions(
    shape: &[usize],
    shape_expr: &[ast::Subscript],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
) -> Option<Vec<i64>> {
    let class_index = ast::ClassDefIndex::from_tree(tree);
    evaluate_array_dimensions_with_index(
        shape,
        shape_expr,
        mod_env,
        effective_components,
        tree,
        &class_index,
        resolve_class_components,
    )
}

#[test]
fn evaluate_component_condition_with_resolved_enum_ref() {
    let mut components = IndexMap::default();
    // MLS §4.4.4: the declaration binding carries the value. (`start` is an
    // initial guess, MLS §4.9, and is deliberately not read as one.)
    let model_structure = ast::Component {
        name: "modelStructure".to_string(),
        binding: Some(ast::Expression::ComponentReference(cref(
            "Types.ModelStructure.a_vb",
        ))),
        has_explicit_binding: true,
        ..ast::Component::empty_with_span(test_span())
    };
    components.insert("modelStructure".to_string(), model_structure);

    let condition = eq_expr(
        ast::Expression::ComponentReference(cref("modelStructure")),
        ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
    );
    // MLS §4.8.5.1: `Types.ModelStructure.a_vb` is an enumeration literal
    // only because `ModelStructure` is an enumeration declaring `a_vb`. The
    // evaluator establishes that from the class tree, not from the spelling.
    let tree = tree_with_enumeration("Types", "ModelStructure", &["a_v", "a_vb", "av_b"]);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };
    let value = evaluate_component_condition(&ctx, &condition);

    assert_eq!(value, Some(true));
}

/// SPEC_0032 §3: a name that merely looks like an enumeration literal is
/// not one. Without a declaring enumeration the comparison is unknown.
#[test]
fn evaluate_component_condition_with_undeclared_enum_literal_is_unknown() {
    let mut components = IndexMap::default();
    components.insert(
        "modelStructure".to_string(),
        ast::Component {
            name: "modelStructure".to_string(),
            binding: Some(ast::Expression::ComponentReference(cref(
                "Types.ModelStructure.a_vb",
            ))),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let condition = eq_expr(
        ast::Expression::ComponentReference(cref("modelStructure")),
        ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
    );
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(evaluate_component_condition(&ctx, &condition), None);
}

#[test]
fn evaluate_component_condition_uses_declaration_binding() {
    let mut components = IndexMap::default();
    components.insert(
        "use_numberPort".to_string(),
        ast::Component {
            name: "use_numberPort".to_string(),
            type_name: ast::Name::from_string("Boolean"),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(bool_expr(true)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let condition = ast::Expression::ComponentReference(cref("use_numberPort"));
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(evaluate_component_condition(&ctx, &condition), Some(true));
}

#[test]
fn evaluate_component_condition_unknown_modifier_blocks_declaration_default() {
    let mut components = IndexMap::default();
    components.insert(
        "condition".to_string(),
        ast::Component {
            name: "condition".to_string(),
            type_name: ast::Name::from_string("Boolean"),
            binding: Some(bool_expr(true)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let mut mod_env = ast::ModificationEnvironment::new();
    mod_env.add(
        ast::QualifiedName::from_ident("condition"),
        ast::ModificationValue::simple(ast::Expression::ComponentReference(cref("start"))),
    );

    let condition = ast::Expression::ComponentReference(cref("condition"));
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &mod_env,
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(evaluate_component_condition(&ctx, &condition), None);
}

#[test]
fn evaluate_component_condition_with_unresolved_enum_ref_is_unknown() {
    let mut components = IndexMap::default();
    let model_structure = ast::Component {
        name: "modelStructure".to_string(),
        ..ast::Component::empty_with_span(test_span())
    };
    components.insert("modelStructure".to_string(), model_structure);

    let condition = eq_expr(
        ast::Expression::ComponentReference(cref("modelStructure")),
        ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
    );
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };
    let value = evaluate_component_condition(&ctx, &condition);

    assert_eq!(value, None);
}

#[test]
fn evaluate_array_dimensions_supports_structural_if_shape_refs() {
    // MLS §4.4.4: structural parameters carry their value in the
    // declaration binding. `start` is an initial guess (MLS §4.9) that the
    // parser seeds with the type default, so it is never read as a value.
    let mut components = IndexMap::default();
    components.insert(
        "useLumpedPressure".to_string(),
        ast::Component {
            name: "useLumpedPressure".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(bool_expr(false)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nFMLumped".to_string(),
        ast::Component {
            name: "nFMLumped".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(int_expr(2)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nFMDistributed".to_string(),
        ast::Component {
            name: "nFMDistributed".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(int_expr(1)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nFM".to_string(),
        ast::Component {
            name: "nFM".to_string(),
            binding: Some(if_expr(
                vec![(
                    ast::Expression::ComponentReference(cref("useLumpedPressure")),
                    ast::Expression::ComponentReference(cref("nFMLumped")),
                )],
                ast::Expression::ComponentReference(cref("nFMDistributed")),
            )),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let dims = evaluate_array_dimensions(
        &[1],
        &[ast::Subscript::Expression(add_expr(
            ast::Expression::ComponentReference(cref("nFM")),
            int_expr(1),
        ))],
        &ast::ModificationEnvironment::new(),
        &components,
        &ast::ClassTree::new(),
        no_op_resolve_class_components,
    );

    assert_eq!(dims, Some(vec![2]));
}

#[test]
fn evaluate_array_dimensions_rejects_runtime_if_shape_condition() {
    let mut components = IndexMap::default();
    components.insert(
        "runtimeSwitch".to_string(),
        ast::Component {
            name: "runtimeSwitch".to_string(),
            start: bool_expr(false),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nA".to_string(),
        ast::Component {
            name: "nA".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            start: int_expr(2),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nB".to_string(),
        ast::Component {
            name: "nB".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            start: int_expr(1),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "n".to_string(),
        ast::Component {
            name: "n".to_string(),
            start: if_expr(
                vec![(
                    ast::Expression::ComponentReference(cref("runtimeSwitch")),
                    ast::Expression::ComponentReference(cref("nA")),
                )],
                ast::Expression::ComponentReference(cref("nB")),
            ),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let dims = evaluate_array_dimensions(
        &[1],
        &[ast::Subscript::Expression(add_expr(
            ast::Expression::ComponentReference(cref("n")),
            int_expr(1),
        ))],
        &ast::ModificationEnvironment::new(),
        &components,
        &ast::ClassTree::new(),
        no_op_resolve_class_components,
    );

    // A present symbolic dimension is authoritative. A runtime condition
    // cannot be replaced by a stale precomputed shape.
    assert_eq!(dims, None);
}

#[test]
fn precomputed_shape_cannot_hide_recovery_shape_syntax() {
    let components = IndexMap::default();
    let mod_env = ast::ModificationEnvironment::new();
    let tree = ast::ClassTree::new();

    assert_eq!(
        evaluate_array_dimensions(
            &[3],
            &[ast::Subscript::Empty],
            &mod_env,
            &components,
            &tree,
            no_op_resolve_class_components,
        ),
        None,
    );
    assert_eq!(
        evaluate_array_dimensions(
            &[3],
            &[ast::Subscript::Expression(ast::Expression::Parenthesized {
                inner: Arc::new(ast::Expression::Empty { span: test_span() }),
                span: test_span(),
            })],
            &mod_env,
            &components,
            &tree,
            no_op_resolve_class_components,
        ),
        None,
        "nested recovery in a dimension expression must not take the shape fallback",
    );

    let assignment = ast::Expression::Binary {
        op: rumoca_core::OpBinary::Assign,
        lhs: Arc::new(int_expr(1)),
        rhs: Arc::new(int_expr(3)),
        span: test_span(),
    };
    assert_eq!(
        evaluate_array_dimensions(
            &[3],
            &[ast::Subscript::Expression(assignment)],
            &mod_env,
            &components,
            &tree,
            no_op_resolve_class_components,
        ),
        None,
        "cached shape must not hide a modification-only assignment operator",
    );
}

#[test]
fn cached_shape_rejects_declaration_end_but_preserves_indexed_access_end() {
    let components = IndexMap::default();
    let mod_env = ast::ModificationEnvironment::new();
    let tree = ast::ClassTree::new();
    let end = ast::Expression::Terminal {
        terminal_type: ast::TerminalType::End,
        token: token("end"),
        span: test_span(),
    };

    assert_eq!(
        evaluate_array_dimensions(
            &[3],
            &[ast::Subscript::Expression(end.clone())],
            &mod_env,
            &components,
            &tree,
            no_op_resolve_class_components,
        ),
        None,
        "a declaration dimension has no indexed array whose bound `end` can denote",
    );
    assert_eq!(
        evaluate_array_dimensions(
            &[3],
            &[ast::Subscript::Expression(ast::Expression::Parenthesized {
                inner: Arc::new(end.clone()),
                span: test_span(),
            })],
            &mod_env,
            &components,
            &tree,
            no_op_resolve_class_components,
        ),
        None,
        "nesting cannot turn declaration-only `end` into an array selector",
    );

    let indexed_access = ast::Expression::ArrayIndex {
        base: Arc::new(ast::Expression::ComponentReference(cref("a"))),
        subscripts: vec![ast::Subscript::Expression(end)],
        span: test_span(),
    };
    assert_eq!(
        evaluate_array_dimensions(
            &[3],
            &[ast::Subscript::Expression(indexed_access)],
            &mod_env,
            &components,
            &tree,
            no_op_resolve_class_components,
        ),
        None,
        "nested `end` is syntactically legal, but an unknown indexed value cannot become a dimension",
    );
}

#[test]
fn try_eval_integer_expr_prefers_binding_over_start_for_component_refs() {
    let mut components = IndexMap::default();
    components.insert(
        "n".to_string(),
        ast::Component {
            name: "n".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            // Unresolvable start should not override explicit binding.
            start: ast::Expression::ComponentReference(cref("missing.scope.value")),
            binding: Some(int_expr(1)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    let value = try_eval_integer_expr(
        &ctx,
        &add_expr(ast::Expression::ComponentReference(cref("n")), int_expr(1)),
    );

    assert_eq!(value, Some(2));
}

#[test]
fn evaluate_array_dimensions_prefers_binding_over_start_for_shape_refs() {
    let mut components = IndexMap::default();
    components.insert(
        "m".to_string(),
        ast::Component {
            name: "m".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            // Keep start unresolved and provide the structural value via binding.
            start: ast::Expression::ComponentReference(cref("missing.scope.value")),
            binding: Some(int_expr(1)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let dims = evaluate_array_dimensions(
        &[0],
        &[ast::Subscript::Expression(add_expr(
            ast::Expression::ComponentReference(cref("m")),
            int_expr(1),
        ))],
        &ast::ModificationEnvironment::new(),
        &components,
        &ast::ClassTree::new(),
        no_op_resolve_class_components,
    );

    assert_eq!(dims, Some(vec![2]));
}

#[test]
fn shape_stability_proof_requires_exact_component_identity() {
    let target = rumoca_core::DefId::new(700);
    let mut components = IndexMap::default();
    components.insert(
        "n".to_string(),
        ast::Component {
            name: "n".to_string(),
            def_id: Some(target),
            variability: rumoca_core::Variability::Constant(token("constant")),
            binding: Some(int_expr(2)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let tree = ast::ClassTree::new();
    let class_index = ast::ClassDefIndex::from_tree(&tree);

    let unresolved = ast::Expression::ComponentReference(cref("n"));
    let unresolved_value = try_eval_integer_shape_expr_with_proof(
        &unresolved,
        &ast::ModificationEnvironment::new(),
        &components,
        &tree,
        &class_index,
        no_op_resolve_class_components,
    )
    .expect("textual lookup may compute a provisional value");
    assert_eq!(unresolved_value.value(), 2);
    assert!(!unresolved_value.is_translation_constant());

    let mut resolved_reference = cref("n");
    resolved_reference.set_target_def_id(Some(target));
    let resolved = ast::Expression::ComponentReference(resolved_reference);
    let resolved_value = try_eval_integer_shape_expr_with_proof(
        &resolved,
        &ast::ModificationEnvironment::new(),
        &components,
        &tree,
        &class_index,
        no_op_resolve_class_components,
    )
    .expect("resolved constant identity evaluates");
    assert_eq!(resolved_value.value(), 2);
    assert!(resolved_value.is_translation_constant());
}

#[test]
fn evaluate_array_dimensions_refuses_class_modification_without_field_metadata() {
    let mut components = IndexMap::default();
    components.insert(
        "stackData".to_string(),
        ast::Component {
            name: "stackData".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut mod_env = ast::ModificationEnvironment::new();
    let stack_data_mod = ast::Expression::ClassModification {
        target: cref("StackData"),
        modifications: vec![
            ast::Expression::NamedArgument {
                name: token("Ns"),
                value: Arc::new(int_expr(3)),

                span: rumoca_core::Span::DUMMY,
            },
            ast::Expression::NamedArgument {
                name: token("Np"),
                value: Arc::new(int_expr(2)),

                span: rumoca_core::Span::DUMMY,
            },
        ],

        each_flags: vec![false, false],
        final_flags: vec![false, false],
        redeclare_flags: vec![false, false],
        span: rumoca_core::Span::DUMMY,
    };
    mod_env.add(
        ast::QualifiedName::from_ident("stackData"),
        ast::ModificationValue::simple(stack_data_mod),
    );

    let dims = evaluate_array_dimensions(
        &[1, 1],
        &[
            ast::Subscript::Expression(ast::Expression::ComponentReference(cref("stackData.Ns"))),
            ast::Subscript::Expression(ast::Expression::ComponentReference(cref("stackData.Np"))),
        ],
        &mod_env,
        &components,
        &ast::ClassTree::new(),
        no_op_resolve_class_components,
    );

    assert_eq!(dims, None);
}

#[test]
fn try_eval_integer_expr_resolves_enclosing_scope_component_ref() {
    let mut components = IndexMap::default();
    components.insert(
        "pipe2.nFM".to_string(),
        ast::Component {
            name: "pipe2.nFM".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            start: int_expr(1),
            binding: Some(int_expr(1)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    let value = try_eval_integer_expr(
        &ctx,
        &add_expr(
            ast::Expression::ComponentReference(cref("pipe2.flowModel.nFM")),
            int_expr(1),
        ),
    );

    assert_eq!(value, Some(2));
}

#[test]
fn try_eval_integer_expr_evaluates_if_expressions() {
    let expr = if_expr(vec![(bool_expr(true), int_expr(2))], int_expr(1));
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let value = try_eval_integer_expr(&ctx, &expr);

    assert_eq!(value, Some(2));
}

#[test]
fn try_eval_integer_expr_evaluates_parameterized_if_expressions() {
    let mut components = IndexMap::default();
    components.insert(
        "ParDesired".to_string(),
        ast::Component {
            name: "ParDesired".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(int_expr(2)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "mSystems".to_string(),
        ast::Component {
            name: "mSystems".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(int_expr(2)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let expr = if_expr(
        vec![(
            func_call(
                "Modelica.Math.isPowerOf2",
                vec![ast::Expression::ComponentReference(cref("ParDesired"))],
            ),
            ast::Expression::ComponentReference(cref("ParDesired")),
        )],
        ast::Expression::ComponentReference(cref("mSystems")),
    );
    let tree = msl_math_tree();
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(try_eval_integer_expr(&ctx, &expr), Some(2));
}
