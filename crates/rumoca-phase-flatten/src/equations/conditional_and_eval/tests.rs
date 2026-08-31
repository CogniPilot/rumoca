use super::*;
use std::sync::Arc;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("conditional_and_eval_test.mo"),
        1,
        2,
    )
}

fn make_comp_ref(path: &str) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: crate::path_utils::segments(path)
            .into_iter()
            .enumerate()
            .map(|(index, name)| ComponentRefPart {
                ident: Token {
                    text: Arc::from(name.to_string()),
                    ..Token::default()
                },
                subs: None,
                def_id: Some(rumoca_core::DefId::new(15_001 + index as u32)),
            })
            .collect(),
        span: test_span(),
        qualified_display_name: None,
    }
}

fn make_comp_ref_with_first_index(path: &str, index: i64) -> ComponentReference {
    let mut cr = make_comp_ref(path);
    let sub = ast::Subscript::Expression(ast::Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: Arc::from(index.to_string()),
            ..Token::default()
        },
        span: rumoca_core::Span::DUMMY,
    });
    if let Some(first) = cr.parts.first_mut() {
        first.subs = Some(vec![sub]);
    }
    cr
}

fn make_int(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: Arc::from(value.to_string()),
            ..Token::default()
        },
        span: test_span(),
    }
}

fn make_range(start: i64, step: Option<i64>, end: i64) -> ast::Expression {
    ast::Expression::Range {
        start: Arc::new(make_int(start)),
        step: step.map(|value| Arc::new(make_int(value))),
        end: Arc::new(make_int(end)),
        span: test_span(),
    }
}

fn make_call(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: make_comp_ref(name),
        args,
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_for_index(name: &str, start: i64, end: i64) -> ForIndex {
    ForIndex {
        ident: Token {
            text: Arc::from(name.to_string()),
            ..Token::default()
        },
        range: ast::Expression::Range {
            start: Arc::new(make_int(start)),
            step: None,
            end: Arc::new(make_int(end)),
            span: rumoca_core::Span::DUMMY,
        },
    }
}

#[test]
fn generated_zero_real_expr_uses_owner_span() {
    let span = test_span();
    assert_eq!(zero_real_expr(span).span(), span);
}

#[test]
fn named_array_assignment_remains_one_tensor_owner() {
    let lhs = ast::Expression::ComponentReference(make_comp_ref("x"));
    let rhs = ast::Expression::Array {
        elements: vec![make_int(1), make_int(2)],
        is_matrix: false,
        span: test_span(),
    };
    let expanded = expand_array_assignment(&lhs, &rhs);
    assert_eq!(expanded.len(), 1);
    assert!(matches!(
        expanded[0].lhs,
        ast::Expression::ComponentReference(_)
    ));
    assert!(matches!(expanded[0].rhs, ast::Expression::Array { .. }));
}

fn simple_index_for_equation(start: i64, end: i64) -> InstanceEquation {
    InstanceEquation {
        equation: ast::Equation::For {
            indices: vec![make_for_index("i", start, end)],
            equations: vec![ast::Equation::Simple {
                lhs: ast::Expression::ComponentReference(make_comp_ref("y")),
                rhs: ast::Expression::ComponentReference(make_comp_ref("i")),
            }],
        },
        origin: QualifiedName::from_dotted("M"),
        source_scope: None,
        source_scope_id: None,
        span: test_span(),
    }
}

#[test]
fn test_try_eval_integer_with_ctx_div_operator_requires_exact_quotient() {
    let ctx = Context::new();
    let expr = ast::Expression::Binary {
        op: OpBinary::Div,
        lhs: Arc::new(make_int(7)),
        rhs: Arc::new(make_int(2)),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        try_eval_integer_with_ctx(&ctx, &expr, &QualifiedName::new()),
        None
    );
}

#[test]
fn range_materialization_accepts_extreme_singleton_endpoints() {
    let ctx = Context::new();
    let prefix = QualifiedName::new();

    assert_eq!(
        expand_range_indices(
            &ctx,
            &make_range(i64::MAX, None, i64::MAX),
            &prefix,
            test_span()
        )
        .expect("MAX:MAX contains exactly one representable element"),
        vec![i64::MAX]
    );
    assert_eq!(
        expand_range_indices(
            &ctx,
            &make_range(i64::MIN, Some(-1), i64::MIN),
            &prefix,
            test_span(),
        )
        .expect("MIN:-1:MIN contains exactly one representable element"),
        vec![i64::MIN]
    );
}

#[test]
fn range_materialization_accepts_the_budget_boundary_and_empty_ranges() {
    let ctx = Context::new();
    let prefix = QualifiedName::new();
    let boundary = expand_range_indices(
        &ctx,
        &make_range(1, None, MAX_EAGER_RANGE_ELEMENTS as i64),
        &prefix,
        test_span(),
    )
    .expect("the eager range budget is inclusive");

    assert_eq!(boundary.len(), MAX_EAGER_RANGE_ELEMENTS);
    assert_eq!(boundary.last(), Some(&(MAX_EAGER_RANGE_ELEMENTS as i64)));
    assert!(
        expand_range_indices(&ctx, &make_range(2, None, 1), &prefix, test_span())
            .expect("a descending default-step range is legally empty")
            .is_empty()
    );
}

#[test]
fn oversized_structural_ranges_fail_before_materialization() {
    let ctx = Context::new();
    let range = make_range(1, None, MAX_EAGER_RANGE_ELEMENTS as i64 + 1);
    let error = expand_range_indices(&ctx, &range, &QualifiedName::new(), test_span())
        .expect_err("an oversized eager range must be rejected without allocation");

    use miette::Diagnostic as _;
    assert_eq!(
        error.code().map(|code| code.to_string()),
        Some("rumoca::flatten::EF037".to_string())
    );
    assert!(matches!(
        error,
        FlattenError::RangeMaterializationLimit {
            element_count,
            limit: MAX_EAGER_RANGE_ELEMENTS,
            span,
        } if element_count == MAX_EAGER_RANGE_ELEMENTS as u128 + 1 && span == test_span()
    ));
}

#[test]
fn scalar_range_fallback_obeys_the_same_materialization_budget() {
    let ctx = Context::new();
    let count = make_int(MAX_EAGER_RANGE_ELEMENTS as i64 + 1);
    let error = expand_range_indices(&ctx, &count, &QualifiedName::new(), test_span())
        .expect_err("the implicit 1:n expansion must share the eager range budget");

    assert!(matches!(
        error,
        FlattenError::RangeMaterializationLimit {
            element_count,
            limit: MAX_EAGER_RANGE_ELEMENTS,
            ..
        } if element_count == MAX_EAGER_RANGE_ELEMENTS as u128 + 1
    ));
}

#[test]
fn structural_range_evaluation_propagates_overflow_and_lowering_errors() {
    let mut ctx = Context::new();
    ctx.parameter_values.insert("lowest".to_string(), i64::MIN);
    let negated_min = ast::Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("lowest"))),
        span: test_span(),
    };
    let overflow_range = ast::Expression::Range {
        start: Arc::new(negated_min.clone()),
        step: None,
        end: Arc::new(make_int(1)),
        span: test_span(),
    };

    assert_eq!(
        try_eval_integer_with_ctx(&ctx, &negated_min, &QualifiedName::new()),
        None,
        "best-effort unary evaluation must not panic or wrap i64::MIN"
    );
    assert!(matches!(
        expand_range_indices(&ctx, &overflow_range, &QualifiedName::new(), test_span(),),
        Err(FlattenError::ConstantEvaluationFailed { .. })
    ));

    let recovery_range = ast::Expression::Range {
        start: Arc::new(ast::Expression::Empty { span: test_span() }),
        step: None,
        end: Arc::new(make_int(1)),
        span: test_span(),
    };
    assert!(matches!(
        expand_range_indices(&ctx, &recovery_range, &QualifiedName::new(), test_span(),),
        Err(FlattenError::InvalidAstRecovery { .. })
    ));
}

#[test]
fn best_effort_abs_does_not_wrap_integer_minimum() {
    let mut ctx = Context::new();
    ctx.parameter_values.insert("lowest".to_string(), i64::MIN);
    let abs_min = make_call(
        "abs",
        vec![ast::Expression::ComponentReference(make_comp_ref("lowest"))],
    );

    assert_eq!(
        try_eval_integer_with_ctx(&ctx, &abs_min, &QualifiedName::new()),
        None
    );
    let range = ast::Expression::Range {
        start: Arc::new(abs_min),
        step: None,
        end: Arc::new(make_int(1)),
        span: test_span(),
    };
    assert!(matches!(
        expand_range_indices(&ctx, &range, &QualifiedName::new(), test_span()),
        Err(FlattenError::ConstantEvaluationFailed { .. })
    ));
}

#[test]
fn dynamic_nested_if_without_else_rejects_unbalanced_equation_count() {
    let block = EquationBlock {
        cond: ast::Expression::ComponentReference(make_comp_ref("dynamicCondition")),
        eqs: vec![ast::Equation::Simple {
            lhs: ast::Expression::ComponentReference(make_comp_ref("x")),
            rhs: make_int(1),
        }],
    };

    let result = expand_nested_if_to_simple(
        &Context::new(),
        &[block],
        &None,
        &QualifiedName::new(),
        test_span(),
        &crate::test_support::connection_operators(),
    );
    let Err(err) = result else {
        panic!("an omitted else branch must contribute zero equations");
    };

    assert!(
        err.to_string()
            .contains("omitted else branch has 0 equations"),
        "unexpected error: {err}"
    );
}

#[test]
fn test_try_eval_integer_with_ctx_div_builtin_remains_truncating() {
    let ctx = Context::new();
    let expr = make_call("div", vec![make_int(7), make_int(2)]);

    assert_eq!(
        try_eval_integer_with_ctx(&ctx, &expr, &QualifiedName::new()),
        Some(3)
    );
}

#[test]
fn array_max_min_requires_every_element_to_be_known() {
    let ctx = Context::new();
    let mixed = ast::Expression::Array {
        elements: vec![
            make_int(7),
            ast::Expression::ComponentReference(make_comp_ref("runtime_value")),
        ],
        is_matrix: false,
        span: test_span(),
    };
    let known = ast::Expression::Array {
        elements: vec![make_int(7), make_int(3)],
        is_matrix: false,
        span: test_span(),
    };

    assert_eq!(
        try_eval_integer_with_ctx(&ctx, &make_call("max", vec![mixed]), &QualifiedName::new()),
        None,
        "a reduction must not silently discard an unknown array element"
    );
    assert_eq!(
        try_eval_integer_with_ctx(&ctx, &make_call("min", vec![known]), &QualifiedName::new()),
        Some(3)
    );
}

#[test]
fn dimensions_only_context_answers_shape_without_inventing_elements() {
    let mut ctx = Context::new();
    ctx.array_dimensions.insert("A".to_string(), vec![3]);
    let prefix = QualifiedName::new();

    assert_eq!(
        try_eval_integer_with_ctx(
            &ctx,
            &make_call(
                "size",
                vec![
                    ast::Expression::ComponentReference(make_comp_ref("A")),
                    make_int(1),
                ],
            ),
            &prefix,
        ),
        Some(3)
    );
    assert_eq!(
        try_eval_integer_with_ctx(
            &ctx,
            &ast::Expression::ComponentReference(make_comp_ref_with_first_index("A", 1)),
            &prefix,
        ),
        None,
        "shape metadata must not supply an element value"
    );
    assert_eq!(
        try_eval_integer_with_ctx(
            &ctx,
            &make_call(
                "max",
                vec![ast::Expression::ComponentReference(make_comp_ref("A"))],
            ),
            &prefix,
        ),
        None,
        "shape metadata must not supply values to a reduction"
    );
}

#[test]
fn test_infer_simple_equation_scalar_count_dot_product_is_scalar() {
    let mut ctx = Context::new();
    ctx.array_dimensions.insert("a".to_string(), vec![3]);
    ctx.array_dimensions.insert("b".to_string(), vec![3]);

    let lhs = ast::Expression::Binary {
        op: OpBinary::Mul,
        lhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("a"))),
        rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("b"))),
        span: rumoca_core::Span::DUMMY,
    };
    let rhs = make_int(0);
    let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
    assert_eq!(scalar_count, 1);
}

#[test]
fn test_infer_simple_equation_scalar_count_matrix_vector_result() {
    let mut ctx = Context::new();
    ctx.array_dimensions.insert("y".to_string(), vec![2]);
    ctx.array_dimensions.insert("A".to_string(), vec![2, 2]);
    ctx.array_dimensions.insert("x".to_string(), vec![2]);

    let lhs = ast::Expression::ComponentReference(make_comp_ref("y"));
    let rhs = ast::Expression::Binary {
        op: OpBinary::Mul,
        lhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("A"))),
        rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("x"))),
        span: rumoca_core::Span::DUMMY,
    };

    let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
    assert_eq!(scalar_count, 2);
}

#[test]
fn whole_matrix_equation_creates_compact_structured_family() {
    let mut ctx = Context::new();
    ctx.array_dimensions.insert("x".to_string(), vec![2, 3]);
    let equation = InstanceEquation {
        equation: ast::Equation::Simple {
            lhs: ast::Expression::ComponentReference(make_comp_ref("x")),
            rhs: make_call("zeros", vec![make_int(2), make_int(3)]),
        },
        origin: QualifiedName::from_dotted("M"),
        source_scope: None,
        source_scope_id: None,
        span: test_span(),
    };

    let flattened = flatten_equation_with_def_map(
        &ctx,
        &equation,
        &QualifiedName::new(),
        None,
        &crate::test_support::connection_operators(),
    )
    .expect("whole-array equation should flatten");

    assert_eq!(flattened.equations.len(), 1);
    assert_eq!(flattened.equations[0].scalar_count, 6);
    assert_eq!(flattened.structured_equations.len(), 1);
    let family = &flattened.structured_equations[0];
    assert_eq!(family.domain.extents(), Ok(vec![2, 3]));
    assert_eq!(family.equations_per_point, 1);
    assert_eq!(family.first_equation_index, 0);
    assert_eq!(
        family.template.as_ref().map(|body| body.body.len()),
        Some(1)
    );
}

#[test]
fn direct_equation_helpers_reject_empty_recovery_nodes_at_owner_span() {
    let ctx = Context::new();
    let span = test_span();
    let equation = InstanceEquation {
        equation: ast::Equation::Empty,
        origin: QualifiedName::from_dotted("M"),
        source_scope: None,
        source_scope_id: None,
        span,
    };

    let top_level = flatten_equation_with_def_map(
        &ctx,
        &equation,
        &QualifiedName::new(),
        None,
        &crate::test_support::connection_operators(),
    )
    .err()
    .expect("the top-level helper must not discard Equation::Empty");
    assert!(matches!(
        top_level,
        FlattenError::InvalidAstRecovery {
            span: error_span,
            ..
        } if error_span == span
    ));

    let origin = rumoca_ir_flat::EquationOrigin::ComponentEquation {
        component: "M".to_string(),
    };
    let nested = flatten_equations_list(
        &ctx,
        &[ast::Equation::Empty],
        &QualifiedName::new(),
        span,
        &origin,
        None,
        &crate::test_support::connection_operators(),
    )
    .err()
    .expect("the nested-list helper must not discard Equation::Empty");
    assert!(matches!(
        nested,
        FlattenError::InvalidAstRecovery {
            span: error_span,
            ..
        } if error_span == span
    ));
}

#[test]
fn multi_output_equation_preserves_only_direct_omitted_receivers() {
    let ctx = Context::new();
    let span = test_span();
    let omitted = ast::Expression::Empty { span };
    let tuple = ast::Expression::Tuple {
        elements: vec![
            ast::Expression::ComponentReference(make_comp_ref("first")),
            omitted.clone(),
            ast::Expression::ComponentReference(make_comp_ref("third")),
        ],
        span,
    };
    let equation = |rhs| InstanceEquation {
        equation: ast::Equation::Simple {
            lhs: tuple.clone(),
            rhs,
        },
        origin: QualifiedName::from_dotted("M"),
        source_scope: None,
        source_scope_id: None,
        span,
    };

    let flattened = flatten_equation_with_def_map(
        &ctx,
        &equation(make_call("threeResults", vec![])),
        &QualifiedName::new(),
        None,
        &crate::test_support::connection_operators(),
    )
    .expect("an invocation tuple may preserve a direct omitted result slot");
    let rumoca_core::Expression::Binary { lhs, rhs, .. } = &flattened.equations[0].residual else {
        panic!("equation must lower to a residual");
    };
    assert!(matches!(
        lhs.as_ref(),
        rumoca_core::Expression::Tuple { elements, .. }
            if matches!(elements.get(1), Some(rumoca_core::Expression::Empty { span: empty_span }) if *empty_span == span)
    ));
    assert!(matches!(
        rhs.as_ref(),
        rumoca_core::Expression::FunctionCall {
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            ..
        }
    ));

    let error = flatten_equation_with_def_map(
        &ctx,
        &equation(make_int(1)),
        &QualifiedName::new(),
        None,
        &crate::test_support::connection_operators(),
    )
    .err()
    .expect("a tuple omission is not legal when the RHS is not an invocation");
    assert!(matches!(error, FlattenError::InvalidAstRecovery { .. }));
}

#[test]
fn parsed_omitted_multi_result_receiver_reaches_flat_and_dae() {
    const SOURCE: &str = r#"
function threeResults
  input Real u;
  output Real a;
  output Real b;
  output Real c;
algorithm
  a := u;
  b := 2*u;
  c := 3*u;
end threeResults;

model OmittedReceiver
  Real a;
  Real c;
equation
  (a, , c) = threeResults(time);
end OmittedReceiver;
"#;
    let file_name = "omitted_multi_result_receiver.mo";
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, file_name).expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("fixture resolves");
    let mut overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        "OmittedReceiver",
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    rumoca_phase_typecheck::typecheck_instanced(&resolved, &mut overlay, "OmittedReceiver")
        .expect("fixture typechecks");
    let tree = resolved.into_inner();
    let source_map = tree.source_map.clone();
    let flat =
        crate::flatten_ref(&tree, &overlay, "OmittedReceiver").expect("omitted receiver flattens");
    assert!(flat.equations.iter().any(|equation| {
        matches!(
            &equation.residual,
            rumoca_core::Expression::Binary { lhs, .. }
                if matches!(lhs.as_ref(), rumoca_core::Expression::Tuple { elements, .. }
                    if matches!(elements.get(1), Some(rumoca_core::Expression::Empty { .. })))
        )
    }));
    rumoca_phase_dae::to_dae(&flat, source_map)
        .expect("DAE construction consumes the preserved omitted receiver");
}

#[test]
fn test_infer_simple_equation_scalar_count_fallback_uses_lhs_dims() {
    let mut ctx = Context::new();
    ctx.array_dimensions.insert("y".to_string(), vec![3]);

    let lhs = ast::Expression::ComponentReference(make_comp_ref("y"));
    let rhs = make_call("zeros", vec![make_int(3)]);
    let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
    assert_eq!(scalar_count, 3);
}

#[test]
fn test_infer_simple_equation_scalar_count_zero_sized_array() {
    let mut ctx = Context::new();
    ctx.array_dimensions.insert("y".to_string(), vec![0]);

    let lhs = ast::Expression::ComponentReference(make_comp_ref("y"));
    let rhs = make_int(0);

    let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
    assert_eq!(scalar_count, 0);
}

#[test]
fn test_infer_simple_equation_scalar_count_indexed_parent_keeps_field_dims() {
    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("medium_T[2].state.X".to_string(), vec![2]);

    let lhs =
        ast::Expression::ComponentReference(make_comp_ref_with_first_index("medium_T.state.X", 2));
    let rhs = make_call("zeros", vec![make_int(2)]);

    let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
    assert_eq!(scalar_count, 2);
}

#[test]
fn test_infer_simple_equation_scalar_count_indexed_ref_uses_unscripted_dims() {
    let mut ctx = Context::new();
    ctx.array_dimensions.insert("A".to_string(), vec![3]);

    let lhs = ast::Expression::ComponentReference(make_comp_ref_with_first_index("A", 1));
    let rhs = make_int(0);

    let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
    assert_eq!(scalar_count, 1);
}

#[test]
fn test_infer_simple_equation_scalar_count_prefix_with_dot_inside_subscript_uses_field_dims() {
    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("bus[data.medium]".to_string(), vec![5]);
    ctx.array_dimensions
        .insert("bus[data.medium].x".to_string(), vec![2]);

    let lhs = ast::Expression::ComponentReference(make_comp_ref("x"));
    let rhs = make_call("zeros", vec![make_int(2)]);
    let prefix = QualifiedName {
        parts: vec![("bus[data.medium]".to_string(), Vec::new())],
    };

    let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, &prefix, &ctx);
    assert_eq!(scalar_count, 2);
}

#[test]
fn test_infer_size_constant_from_dims_ns_uses_substance_names() {
    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("m.medium.substanceNames".to_string(), vec![4]);
    ctx.array_dimensions
        .insert("m.medium.X".to_string(), vec![7]);

    let prefix = QualifiedName::from_dotted("m.medium");
    assert_eq!(infer_size_constant_from_dims(&ctx, "nS", &prefix), Some(4));
}

#[test]
fn test_infer_size_constant_from_dims_ns_does_not_fallback_to_x() {
    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("m.medium.X".to_string(), vec![7]);

    let prefix = QualifiedName::from_dotted("m.medium");
    assert_eq!(infer_size_constant_from_dims(&ctx, "nS", &prefix), None);
}

#[test]
fn test_lookup_parameter_in_scope_uses_unindexed_array_element_scope() {
    let mut ctx = Context::new();
    ctx.parameter_values
        .insert("adaptor.filter.transferFunction[1].nx".to_string(), 1_i64);

    let prefix = QualifiedName::from_dotted("adaptor.filter[1].transferFunction[1]");
    let cref = make_comp_ref("nx");
    assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), Some(1_i64));
}

#[test]
fn test_lookup_parameter_in_scope_prefers_indexed_scope_override() {
    let mut ctx = Context::new();
    ctx.parameter_values
        .insert("adaptor.filter.transferFunction.nx".to_string(), 1_i64);
    ctx.parameter_values.insert(
        "adaptor.filter[1].transferFunction[1].nx".to_string(),
        0_i64,
    );

    let prefix = QualifiedName::from_dotted("adaptor.filter[1].transferFunction[1]");
    let cref = make_comp_ref("nx");
    assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), Some(0_i64));
}

#[test]
fn test_size_call_uses_unindexed_array_element_scope() {
    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("adaptor.filter.transferFunction[1].a".to_string(), vec![2]);

    let prefix = QualifiedName::from_dotted("adaptor.filter[1].transferFunction[1]");
    let expr = make_call(
        "size",
        vec![
            ast::Expression::ComponentReference(make_comp_ref("a")),
            make_int(1),
        ],
    );

    assert_eq!(try_eval_integer_with_ctx(&ctx, &expr, &prefix), Some(2_i64));
}

#[test]
fn test_lookup_parameter_in_scope_does_not_drop_type_alias_segment() {
    let mut ctx = Context::new();
    ctx.parameter_values.insert("m.nXi".to_string(), 9_i64);
    ctx.parameter_values
        .insert("m.medium.nXi".to_string(), 2_i64);

    let prefix = QualifiedName::from_dotted("m");
    let cref = make_comp_ref("Medium.nXi");
    assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), Some(2_i64));

    ctx.parameter_values.remove("m.medium.nXi");
    assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), None);
}

#[test]
fn test_flatten_for_equation_records_iteration_grouping() {
    let ctx = Context::new();
    let inst_eq = simple_index_for_equation(1, 3);
    let flattened = flatten_equation_with_def_map(
        &ctx,
        &inst_eq,
        &QualifiedName::new(),
        None,
        &crate::test_support::connection_operators(),
    )
    .unwrap();
    assert_eq!(flattened.equations.len(), 3);
    assert_eq!(flattened.structured_equations.len(), 1);
    let for_eq = &flattened.structured_equations[0];
    assert_eq!(for_eq.domain.binders[0].display_name, "i");
    assert_eq!(for_eq.first_equation_index, 0);
    assert_eq!(for_eq.equations_per_point, 1);
    assert_eq!(
        for_eq
            .domain
            .index_tuples()
            .expect("fixture domain should enumerate index tuples"),
        vec![vec![1], vec![2], vec![3]]
    );
}

#[test]
fn test_flatten_empty_for_equation_produces_zero_rows() {
    let ctx = Context::new();
    let inst_eq = simple_index_for_equation(1, 0);
    let flattened = flatten_equation_with_def_map(
        &ctx,
        &inst_eq,
        &QualifiedName::new(),
        None,
        &crate::test_support::connection_operators(),
    )
    .unwrap();
    assert!(flattened.equations.is_empty());
    assert!(flattened.structured_equations.is_empty());
}

#[test]
fn test_flatten_nested_for_equation_records_cartesian_iterations() {
    let ctx = Context::new();
    let inst_eq = InstanceEquation {
        equation: ast::Equation::For {
            indices: vec![make_for_index("i", 1, 2), make_for_index("j", 1, 2)],
            equations: vec![ast::Equation::Simple {
                lhs: ast::Expression::ComponentReference(make_comp_ref("y")),
                rhs: ast::Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("i"))),
                    rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("j"))),
                    span: test_span(),
                },
            }],
        },
        origin: QualifiedName::from_dotted("M"),
        source_scope: None,
        source_scope_id: None,
        span: test_span(),
    };

    let flattened = flatten_equation_with_def_map(
        &ctx,
        &inst_eq,
        &QualifiedName::new(),
        None,
        &crate::test_support::connection_operators(),
    )
    .unwrap();
    assert_eq!(flattened.equations.len(), 4);
    assert_eq!(flattened.structured_equations.len(), 1);
    let for_eq = &flattened.structured_equations[0];
    assert_eq!(for_eq.domain.binders[0].display_name, "i");
    assert_eq!(for_eq.domain.binders[1].display_name, "j");
    assert_eq!(
        for_eq
            .domain
            .index_tuples()
            .expect("fixture domain should enumerate index tuples"),
        vec![vec![1, 1], vec![1, 2], vec![2, 1], vec![2, 2]]
    );
}

#[test]
fn test_flatten_nested_for_equation_lifts_inner_grouping() {
    let ctx = Context::new();
    let inner = ast::Equation::For {
        indices: vec![make_for_index("j", 1, 2)],
        equations: vec![ast::Equation::Simple {
            lhs: ast::Expression::ComponentReference(make_comp_ref("y")),
            rhs: ast::Expression::Binary {
                op: OpBinary::Add,
                lhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("i"))),
                rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("j"))),
                span: test_span(),
            },
        }],
    };
    let inst_eq = InstanceEquation {
        equation: ast::Equation::For {
            indices: vec![make_for_index("i", 1, 2)],
            equations: vec![inner],
        },
        origin: QualifiedName::from_dotted("M"),
        source_scope: None,
        source_scope_id: None,
        span: test_span(),
    };

    let flattened = flatten_equation_with_def_map(
        &ctx,
        &inst_eq,
        &QualifiedName::new(),
        None,
        &crate::test_support::connection_operators(),
    )
    .unwrap();

    assert_eq!(flattened.equations.len(), 4);
    assert_eq!(flattened.structured_equations.len(), 1);
    assert_eq!(
        flattened.structured_equations[0].domain.binders[0].display_name,
        "i"
    );
    assert_eq!(flattened.structured_equations[0].first_equation_index, 0);
    assert_eq!(
        flattened.structured_equations[0].domain.binders[1].display_name,
        "j"
    );
    assert_eq!(
        flattened.structured_equations[0]
            .domain
            .index_tuples()
            .expect("fixture domain should enumerate index tuples"),
        vec![vec![1, 1], vec![1, 2], vec![2, 1], vec![2, 2]]
    );
    assert_eq!(flattened.structured_equations[0].equations_per_point, 1);
}

#[test]
fn test_substitute_index_descends_into_array_comprehension_body() {
    let expr = ast::Expression::ArrayComprehension {
        expr: Arc::new(ast::Expression::Binary {
            op: OpBinary::Add,
            lhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("j"))),
            rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("k"))),
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![make_for_index("k", 1, 3)],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    let substituted = substitute_index_in_expression(&expr, "j", 2);

    let ast::Expression::ArrayComprehension { expr, indices, .. } = substituted else {
        panic!("expected array comprehension");
    };
    assert_eq!(indices[0].ident.text.as_ref(), "k");
    let ast::Expression::Binary { lhs, rhs, .. } = expr.as_ref() else {
        panic!("expected binary comprehension body");
    };
    assert_eq!(lhs.as_ref(), &make_int(2));
    assert!(matches!(
        rhs.as_ref(),
        ast::Expression::ComponentReference(cr) if cr.parts[0].ident.text.as_ref() == "k"
    ));
}

#[test]
fn test_substitute_index_respects_array_comprehension_shadowing() {
    let expr = ast::Expression::ArrayComprehension {
        expr: Arc::new(ast::Expression::ComponentReference(make_comp_ref("j"))),
        indices: vec![make_for_index("j", 1, 3)],
        filter: Some(Arc::new(ast::Expression::ComponentReference(
            make_comp_ref("j"),
        ))),
        span: rumoca_core::Span::DUMMY,
    };

    let substituted = substitute_index_in_expression(&expr, "j", 2);

    let ast::Expression::ArrayComprehension { expr, filter, .. } = substituted else {
        panic!("expected array comprehension");
    };
    assert!(matches!(
        expr.as_ref(),
        ast::Expression::ComponentReference(cr) if cr.parts[0].ident.text.as_ref() == "j"
    ));
    let Some(filter) = filter else {
        panic!("expected filter");
    };
    assert!(matches!(
        filter.as_ref(),
        ast::Expression::ComponentReference(cr) if cr.parts[0].ident.text.as_ref() == "j"
    ));
}

#[test]
fn test_eval_fallback_timing_stats_record_calls() {
    // Snapshot baseline before our calls (other parallel tests may
    // increment the global atomic counter concurrently).
    let baseline = crate::flatten_phase_timing_stats().eval_fallback.calls;

    let ctx = Context::new();
    assert!(!ctx.has_cached_eval_fallback_context());

    let prefix = QualifiedName::new();
    assert_eq!(
        try_eval_with_rumoca_eval_const(&ctx, &make_int(7), &prefix),
        Some(7)
    );
    assert!(ctx.has_cached_eval_fallback_context());
    let first_ctx_ptr = ctx.eval_fallback_context() as *const _;

    assert_eq!(
        try_eval_with_rumoca_eval_const(&ctx, &make_int(11), &prefix),
        Some(11)
    );
    let second_ctx_ptr = ctx.eval_fallback_context() as *const _;
    assert_eq!(first_ctx_ptr, second_ctx_ptr);

    let stats = crate::flatten_phase_timing_stats();
    assert!(
        stats.eval_fallback.calls >= baseline + 2,
        "expected at least 2 new eval_fallback calls, got {} (baseline was {})",
        stats.eval_fallback.calls,
        baseline,
    );
}
