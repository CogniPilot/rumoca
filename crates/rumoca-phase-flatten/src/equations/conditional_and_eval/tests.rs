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
            .map(|name| ComponentRefPart {
                ident: Token {
                    text: Arc::from(name.to_string()),
                    ..Token::default()
                },
                subs: None,
            })
            .collect(),
        def_id: None,
        span: test_span(),
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

fn make_call(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: make_comp_ref(name),
        args,
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
fn test_try_eval_integer_with_ctx_div_builtin_remains_truncating() {
    let ctx = Context::new();
    let expr = make_call("div", vec![make_int(7), make_int(2)]);

    assert_eq!(
        try_eval_integer_with_ctx(&ctx, &expr, &QualifiedName::new()),
        Some(3)
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
    let flattened =
        flatten_equation_with_def_map(&ctx, &inst_eq, &QualifiedName::new(), None).unwrap();
    assert_eq!(flattened.equations.len(), 3);
    assert_eq!(flattened.structured_equations.len(), 1);
    let for_eq = &flattened.structured_equations[0];
    assert_eq!(for_eq.domain.binders[0].display_name, "i");
    assert_eq!(for_eq.first_equation_index, 0);
    assert_eq!(for_eq.equation_counts, vec![1, 1, 1]);
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
    let flattened =
        flatten_equation_with_def_map(&ctx, &inst_eq, &QualifiedName::new(), None).unwrap();
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

    let flattened =
        flatten_equation_with_def_map(&ctx, &inst_eq, &QualifiedName::new(), None).unwrap();
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

    let flattened =
        flatten_equation_with_def_map(&ctx, &inst_eq, &QualifiedName::new(), None).unwrap();

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
    assert_eq!(
        flattened.structured_equations[0].equation_counts,
        vec![1, 1, 1, 1]
    );
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
