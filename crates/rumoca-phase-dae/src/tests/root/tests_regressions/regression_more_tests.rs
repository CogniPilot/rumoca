use super::*;

// SPEC_0021 file-size exception: this root regression bucket temporarily
// groups DAE lowering regressions that share root-level fixtures. split plan:
// move member-call, scalar-count, and discrete-alias regressions into focused
// modules under tests_regressions/.

#[test]
fn test_anchored_expandable_member_via_input_alias_is_not_interface_input() {
    // Reproduces Electrical.Cell bus pattern:
    // top-level expandable connector member is linked through an internal input,
    // but the same connection component has an internal output anchor.
    let mut flat = Model::new();
    flat.top_level_connectors.insert("cellBus".to_string());

    for (name, causality, from_expandable_connector) in [
        ("cellBus.i", rumoca_core::Causality::Empty, true),
        (
            "limIntegrator.u",
            rumoca_core::Causality::Input(rumoca_core::Token::default()),
            false,
        ),
        (
            "multiSensor.i",
            rumoca_core::Causality::Output(rumoca_core::Token::default()),
            false,
        ),
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                variability: rumoca_core::Variability::Empty,
                causality,
                is_primitive: true,
                connected: true,
                from_expandable_connector,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    add_connection_equation(&mut flat, "cellBus.i", "limIntegrator.u");
    add_connection_equation(&mut flat, "limIntegrator.u", "multiSensor.i");
    add_component_equation(
        &mut flat,
        "multiSensor.i",
        Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: crate::test_support::test_span(),
        },
    );

    let state_vars: indexmap::IndexSet<VarName> = indexmap::IndexSet::new();
    let connector_inputs = find_top_level_connector_input_members(&flat, &state_vars);

    assert!(
        !connector_inputs.contains(&VarName::new("cellBus.i")),
        "anchored expandable member should not be treated as external interface input"
    );

    let dae = to_dae(&flat).expect("to_dae should succeed");
    assert!(
        dae.variables
            .algebraics
            .contains_key(&rumoca_core::VarName::new("cellBus.i")),
        "anchored expandable member should remain an algebraic unknown"
    );
    assert!(
        !dae.variables
            .inputs
            .contains_key(&rumoca_core::VarName::new("cellBus.i")),
        "anchored expandable member should not remain in inputs"
    );
}

#[test]
fn test_has_evaluable_arithmetic_subscript() {
    // Evaluable integer arithmetic: should return true
    assert!(has_evaluable_arithmetic_subscript("pc[((2 * 1) - 1)].i"));
    assert!(has_evaluable_arithmetic_subscript("pc[(2 * 1)].i"));
    assert!(has_evaluable_arithmetic_subscript("x[(1 + 1)]"));
    assert!(has_evaluable_arithmetic_subscript("z[(2 - 1)]"));

    // Simple integer subscripts: should return false
    assert!(!has_evaluable_arithmetic_subscript("pc[1].i"));
    assert!(!has_evaluable_arithmetic_subscript("x[2]"));
    assert!(!has_evaluable_arithmetic_subscript("T[1,2]"));

    // No subscripts: should return false
    assert!(!has_evaluable_arithmetic_subscript("x"));
    assert!(!has_evaluable_arithmetic_subscript("a.b.c"));

    // Unresolved variable names in subscripts: should return false
    assert!(!has_evaluable_arithmetic_subscript("suspend[i]"));
    assert!(!has_evaluable_arithmetic_subscript("x[n]"));
    assert!(!has_evaluable_arithmetic_subscript("port_a[m].h"));
}

#[test]
fn test_strip_subscript_handles_nested_brackets_in_subscript_expression() {
    assert_eq!(
        strip_subscript("medium_T[1].X[medium_T[1].nX]").map(|v| v.to_string()),
        Some("medium_T[1].X".to_string())
    );
}

#[test]
fn test_count_embedded_subscripts_ignores_nested_component_indices() {
    assert_eq!(
        count_embedded_subscripts("medium_T[1].X[medium_T[1].nX]"),
        2
    );
}

#[test]
fn test_strip_subscript_preserves_field_suffix() {
    assert_eq!(
        strip_subscript("pc[1].i").map(|v| v.to_string()),
        Some("pc.i".to_string())
    );
    assert_eq!(
        strip_subscript("sum.u[1]").map(|v| v.to_string()),
        Some("sum.u".to_string())
    );
}

#[test]
fn test_infer_scalar_count_arithmetic_subscript_does_not_inflate() {
    let mut flat = Model::new();
    for name in ["pc[1].i", "pc[2].i"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let expr = Expression::VarRef {
        name: VarName::new("pc[((2 * 1) - 1)].i").into(),
        subscripts: vec![],
        span: crate::test_support::test_span(),
    };
    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, None,
        "unevaluated arithmetic subscripts should not be mapped to base-array scalar size"
    );
}

#[test]
fn test_extract_lhs_var_size_keeps_symbolic_tail_subscript_scalar_equation() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("medium_T[1].X"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("medium_T[1].X"),
            dims: vec![2],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("medium_T[1].X[medium_T[1].nX]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    assert_eq!(
        extract_lhs_var_size(&residual, &flat, &prefix_counts),
        Some(1)
    );
    assert_eq!(
        infer_equation_scalar_count(&residual, &flat, &prefix_counts),
        1
    );
}

#[test]
fn test_extract_lhs_var_size_multilayer_subscript_fallback_is_scalar() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("bus.signal"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("bus.signal"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("bus[1].signal[2]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    assert_eq!(
        extract_lhs_var_size(&residual, &flat, &prefix_counts),
        Some(1)
    );
    assert_eq!(
        infer_equation_scalar_count(&residual, &flat, &prefix_counts),
        1
    );
}

#[test]
// SPEC_0021: Exception - single regression fixture for conditional residual branch sizing.
#[allow(clippy::too_many_lines)]
fn test_extract_lhs_var_size_conditional_residual_uses_branch_lhs_size() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("add.y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("add.y"),
            dims: vec![],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("add.k"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("add.k"),
            dims: vec![2],
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("add.u"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("add.u"),
            dims: vec![2],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let residual = Expression::If {
        branches: vec![(
            Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(Expression::BuiltinCall {
                    function: BuiltinFunction::Size,
                    args: vec![
                        Expression::VarRef {
                            name: VarName::new("add.u").into(),
                            subscripts: vec![],
                            span: crate::test_support::test_span(),
                        },
                        Expression::Literal {
                            value: Literal::Integer(1),
                            span: crate::test_support::test_span(),
                        },
                    ],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Integer(0),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            },
            Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("add.y").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::Binary {
                    op: rumoca_core::OpBinary::Mul,
                    lhs: Box::new(Expression::VarRef {
                        name: VarName::new("add.k").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    rhs: Box::new(Expression::VarRef {
                        name: VarName::new("add.u").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            },
        )],
        else_branch: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: VarName::new("add.y").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Integer(0),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    assert_eq!(
        extract_lhs_var_size(&residual, &flat, &prefix_counts),
        Some(1),
        "conditional residual should keep scalar size from branch residual LHS"
    );
    assert_eq!(
        infer_equation_scalar_count(&residual, &flat, &prefix_counts),
        1,
        "conditional residual with vector dot-product branch should stay scalar"
    );
}

#[test]
fn test_overconstrained_interface_uses_optional_edges_for_rooted_component() {
    let mut flat = Model::new();
    flat.top_level_connectors.insert("frame_a".to_string());
    flat.definite_roots.insert("world.frame_b.R".to_string());

    for (name, rec_path, dims) in [
        ("frame_a.R.T", "frame_a.R", vec![3, 3]),
        ("frame_a.R.w", "frame_a.R", vec![3]),
        ("world.frame_b.R.T", "world.frame_b.R", vec![3, 3]),
        ("world.frame_b.R.w", "world.frame_b.R", vec![3]),
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims,
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                is_overconstrained: true,
                oc_record_path: Some(rec_path.to_string()),
                oc_eq_constraint_size: Some(3),
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let state_vars: indexmap::IndexSet<VarName> = indexmap::IndexSet::new();

    // Without optional connect-edges, frame_a.R appears rootless and contributes +9.
    let without_optional = count_overconstrained_interface(&flat, &state_vars).unwrap();
    assert_eq!(
        without_optional, 9,
        "missing optional connect edge should reproduce +9 overconstrained correction"
    );

    // With optional connect-edge, frame_a.R is in the same rooted component as world.frame_b.R.
    flat.optional_edges
        .push(("frame_a.R".to_string(), "world.frame_b.R".to_string()));
    let with_optional = count_overconstrained_interface(&flat, &state_vars).unwrap();
    assert_eq!(
        with_optional, 0,
        "optional connect edge should remove spurious +9 overconstrained correction"
    );
}

#[test]
fn test_build_record_components_matches_exact_vcg_node_paths() {
    // Keep world.x_label.R first so overly-broad prefix matching would choose it.
    let record_paths = vec!["world.x_label.R", "frame_a.R", "world.frame_b.R"];
    let branches: Vec<(String, String)> = Vec::new();
    let optional_edges = vec![("frame_a.R".to_string(), "world.frame_b.R".to_string())];

    let (comp_of, _n_comps) = build_record_components(&record_paths, &branches, &optional_edges);

    let frame_comp = comp_of["frame_a.R"];
    let world_frame_b_comp = comp_of["world.frame_b.R"];
    let world_label_comp = comp_of["world.x_label.R"];

    assert_eq!(
        frame_comp, world_frame_b_comp,
        "optional edge should connect frame_a.R to world.frame_b.R"
    );
    assert_ne!(
        frame_comp, world_label_comp,
        "world.x_label.R must not be connected just because it shares top-level prefix 'world'"
    );
}

#[test]
fn test_build_record_components_ignores_non_matching_vcg_nodes() {
    let record_paths = vec![
        "frame_a.R",
        "position.frame_a.R",
        "position.frame_resolve.R",
    ];
    let branches: Vec<(String, String)> = Vec::new();
    let optional_edges = vec![(
        "frame_a.frame_resolve.R".to_string(),
        "position.frame_resolve.R".to_string(),
    )];

    let (comp_of, _n_comps) = build_record_components(&record_paths, &branches, &optional_edges);

    let frame_a_comp = comp_of["frame_a.R"];
    let resolve_comp = comp_of["position.frame_resolve.R"];
    assert_ne!(
        frame_a_comp, resolve_comp,
        "non-existent VCG node paths must not force component merging by top-level prefix"
    );
}

#[test]
fn test_overconstrained_interface_skips_internally_defined_record_paths() {
    use rumoca_ir_flat as flat;

    let mut flat = Model::new();
    flat.top_level_connectors.insert("frame_a".to_string());

    for (name, rec_path, dims) in [
        ("frame_a.R.T", "frame_a.R", vec![3, 3]),
        ("frame_a.R.w", "frame_a.R", vec![3]),
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims,
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                is_overconstrained: true,
                oc_record_path: Some(rec_path.to_string()),
                oc_eq_constraint_size: Some(3),
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // Internal equation defines the overconstrained record, so OC interface
    // correction must not add +9.
    let lhs = Expression::VarRef {
        name: VarName::new("frame_a.R").into(),
        subscripts: vec![],
        span: crate::test_support::test_span(),
    };
    let rhs = Expression::FunctionCall {
        name: VarName::new("Frames.from_Q").into(),
        args: vec![Expression::VarRef {
            name: VarName::new("Q").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }],
        is_constructor: false,
        span: crate::test_support::test_span(),
    };
    flat.add_equation(flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: flat::EquationOrigin::ComponentEquation {
            component: "PointMass".to_string(),
        },
        scalar_count: 12,
    });

    let state_vars: indexmap::IndexSet<VarName> = indexmap::IndexSet::new();
    let correction = count_overconstrained_interface(&flat, &state_vars).unwrap();
    assert_eq!(
        correction, 0,
        "internally defined OC records should not receive extra interface correction"
    );
}

#[test]
fn test_overconstrained_interface_counts_only_top_level_records() {
    let mut flat = Model::new();
    flat.top_level_connectors.insert("frame_a".to_string());

    // Top-level OC record.
    for (name, rec_path, dims) in [
        ("frame_a.R.T", "frame_a.R", vec![3, 3]),
        ("frame_a.R.w", "frame_a.R", vec![3]),
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims,
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                is_overconstrained: true,
                oc_record_path: Some(rec_path.to_string()),
                oc_eq_constraint_size: Some(3),
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // Internal OC record connected to the same VCG component.
    for (name, rec_path, dims) in [
        ("body.frame_a.R.T", "body.frame_a.R", vec![3, 3]),
        ("body.frame_a.R.w", "body.frame_a.R", vec![3]),
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims,
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                is_overconstrained: true,
                oc_record_path: Some(rec_path.to_string()),
                oc_eq_constraint_size: Some(3),
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // Put both records in one rootless VCG component.
    flat.optional_edges
        .push(("frame_a.R".to_string(), "body.frame_a.R".to_string()));

    let state_vars: indexmap::IndexSet<VarName> = indexmap::IndexSet::new();
    let correction = count_overconstrained_interface(&flat, &state_vars).unwrap();
    assert_eq!(
        correction, 9,
        "only the top-level OC record should contribute interface correction"
    );
}

#[test]
fn test_infer_scalar_count_function_lhs_uses_function_output_dims() {
    let mut flat = Model::new();
    // Record-like argument prefix with scalar size 12 (9 + 3).
    flat.add_variable(
        VarName::new("R_b.T"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.T"),
            dims: vec![3, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("R_b.w"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.w"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("w_rel_b"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("w_rel_b"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let mut f =
        rumoca_core::Function::new("Frames.angularVelocity2", crate::test_support::test_span());
    f.add_input(rumoca_core::FunctionParam::new(
        "R",
        "Orientation",
        crate::test_support::test_span(),
    ));
    f.add_output(
        rumoca_core::FunctionParam::new("w", "Real", crate::test_support::test_span())
            .with_dims(vec![3]),
    );
    flat.add_function(f);

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.angularVelocity2").into(),
            args: vec![Expression::VarRef {
                name: VarName::new("R_b").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("w_rel_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "function-call LHS should use function output dims, not record argument size"
    );
}

#[test]
fn test_infer_scalar_count_single_element_array_lhs_is_scalar() {
    // Reproduces `{0} = Frames.Quaternions.orientationConstraint(body.Q)`.
    // The argument `body.Q` is Real[4], but the equation is scalar.
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("body.Q"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("body.Q"),
            dims: vec![4],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Array {
            elements: vec![Expression::Literal {
                value: Literal::Integer(0),
                span: crate::test_support::test_span(),
            }],
            is_matrix: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.Quaternions.orientationConstraint").into(),
            args: vec![Expression::VarRef {
                name: VarName::new("body.Q").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "single-element array LHS should force scalar count 1"
    );
}

#[test]
fn test_infer_scalar_count_array_lhs_der_array_plus_scalar() {
    // Reproduces equations like:
    // {{der(x)}, {xn}} = {{x1dot}, {x}}
    // where x is Real[10], so scalar count is 10 + 1 = 11.
    let mut flat = Model::new();
    let span = crate::test_support::test_span();
    flat.add_variable(
        VarName::new("x"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("x"),
            dims: vec![10],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        }),
    );
    flat.add_variable(
        VarName::new("xn"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("xn"),
            dims: vec![],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        }),
    );
    flat.add_variable(
        VarName::new("x1dot"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("x1dot"),
            dims: vec![],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        }),
    );

    let lhs = Expression::Array {
        elements: vec![
            Expression::Array {
                elements: vec![Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args: vec![Expression::VarRef {
                        name: VarName::new("x").into(),
                        subscripts: vec![],
                        span,
                    }],
                    span,
                }],
                is_matrix: false,
                span,
            },
            Expression::Array {
                elements: vec![Expression::VarRef {
                    name: VarName::new("xn").into(),
                    subscripts: vec![],
                    span,
                }],
                is_matrix: false,
                span,
            },
        ],
        is_matrix: true,
        span,
    };
    let rhs = Expression::Array {
        elements: vec![
            Expression::Array {
                elements: vec![Expression::VarRef {
                    name: VarName::new("x1dot").into(),
                    subscripts: vec![],
                    span,
                }],
                is_matrix: false,
                span,
            },
            Expression::Array {
                elements: vec![Expression::VarRef {
                    name: VarName::new("x").into(),
                    subscripts: vec![],
                    span,
                }],
                is_matrix: false,
                span,
            },
        ],
        is_matrix: true,
        span,
    };

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 11,
        "der(array) inside array LHS should contribute the array scalar size"
    );
}

#[test]
fn test_infer_scalar_count_varref_subscripts_use_element_size() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("line.i"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("line.i"),
            dims: vec![4],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Reproduces Electrical M_OLine-style references where scalarized element access
    // must not inherit the full base-array size.
    let expr = Expression::VarRef {
        name: VarName::new("line.i").into(),
        subscripts: vec![Subscript::generated_index(
            1,
            crate::test_support::test_span(),
        )],
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts);
    assert_eq!(
        scalar_count,
        Some(1),
        "subscripted varrefs should infer scalar element size, not full array size"
    );
}

#[test]
fn test_infer_scalar_count_varref_subscripts_zero_sized_dim_is_zero() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("line.i"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("line.i"),
            dims: vec![0],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let expr = Expression::VarRef {
        name: VarName::new("line.i").into(),
        subscripts: vec![Subscript::generated_index(
            1,
            crate::test_support::test_span(),
        )],
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts);
    assert_eq!(
        scalar_count,
        Some(0),
        "indexing a zero-sized dimension should produce zero scalar equations"
    );
}

#[test]
fn test_infer_varref_form_multilayer_embedded_subscript_is_scalar() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("bus.signal"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("bus.signal"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let prefix_counts = build_prefix_counts(&flat);
    let form = infer_varref_form("bus[1].signal[2]", &[], &flat, &prefix_counts);
    assert_eq!(form, ExpressionForm::Scalar);
}

#[test]
fn test_infer_scalar_count_multilayer_embedded_subscript_varref_is_scalar() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("bus.signal"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("bus.signal"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let expr = Expression::VarRef {
        name: VarName::new("bus[1].signal[2]").into(),
        subscripts: vec![],
        span: crate::test_support::test_span(),
    };
    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts);
    assert_eq!(scalar_count, Some(1));
}

#[test]
fn test_infer_scalar_count_function_lhs_supports_alias_suffix_lookup() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("R_b.T"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.T"),
            dims: vec![3, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("R_b.w"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.w"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("w_rel_b"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("w_rel_b"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Store function under a fully-qualified name, but call through a short alias.
    let mut f = rumoca_core::Function::new(
        "Modelica.Mechanics.MultiBody.Frames.angularVelocity2",
        crate::test_support::test_span(),
    );
    f.add_input(rumoca_core::FunctionParam::new(
        "R",
        "Orientation",
        crate::test_support::test_span(),
    ));
    f.add_output(
        rumoca_core::FunctionParam::new("w", "Real", crate::test_support::test_span())
            .with_dims(vec![3]),
    );
    flat.add_function(f);

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.angularVelocity2").into(),
            args: vec![Expression::VarRef {
                name: VarName::new("R_b").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("w_rel_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "alias function call should resolve to unique fully-qualified function output size"
    );
}

#[test]
fn test_infer_scalar_count_function_lhs_uses_rhs_unknown_size_when_signature_is_unavailable() {
    let mut flat = Model::new();
    // Record-like argument prefix with scalar size 12.
    flat.add_variable(
        VarName::new("R_b.T"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.T"),
            dims: vec![3, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("R_b.w"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.w"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("w_rel_b"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("w_rel_b"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // No function definition added on purpose: scalar inference should use the
    // opposite side's declared unknown size (w_rel_b:3) instead of treating the
    // function argument record size (12) as the function result shape.
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.angularVelocity2").into(),
            args: vec![Expression::VarRef {
                name: VarName::new("R_b").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("w_rel_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "when function signature is unavailable, LHS function-call equations should infer from RHS unknown size"
    );
}

#[test]
fn test_infer_scalar_count_function_lhs_skips_rhs_function_arg_records() {
    let mut flat = Model::new();
    for (name, dims) in [
        ("R_a.T", vec![3, 3]),
        ("R_a.w", vec![3]),
        ("R_b.T", vec![3, 3]),
        ("R_b.w", vec![3]),
        ("w_rel_b", vec![3]),
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims,
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // angularVelocity2(R_b) = resolve2(R_b, angularVelocity1(R_a)) + w_rel_b
    // All function names are intentionally unresolved in flat.functions.
    let lhs = Expression::FunctionCall {
        name: VarName::new("Frames.angularVelocity2").into(),
        args: vec![Expression::VarRef {
            name: VarName::new("R_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }],
        is_constructor: false,
        span: crate::test_support::test_span(),
    };
    let rhs = Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.resolve2").into(),
            args: vec![
                Expression::VarRef {
                    name: VarName::new("R_b").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                },
                Expression::FunctionCall {
                    name: VarName::new("Frames.angularVelocity1").into(),
                    args: vec![Expression::VarRef {
                        name: VarName::new("R_a").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }],
                    is_constructor: false,
                    span: crate::test_support::test_span(),
                },
            ],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("w_rel_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "record-typed arguments inside RHS function calls must not inflate scalar count"
    );
}

#[test]
fn test_infer_scalar_count_vector_dot_residual_is_scalar() {
    let mut flat = Model::new();
    for name in ["a", "b", "s"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims: if name == "s" { vec![] } else { vec![3] },
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // Residual for equation: 0 = a*b - s
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("a").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::VarRef {
                    name: VarName::new("b").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(Expression::VarRef {
                name: VarName::new("s").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "vector dot-product residual should count as scalar equation"
    );
}

#[test]
fn test_infer_scalar_count_vector_matrix_vector_residual_is_scalar() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("constraint.ex_a"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("constraint.ex_a"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("constraint.R_rel.T"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("constraint.R_rel.T"),
            dims: vec![3, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("constraint.e"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("constraint.e"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Residual for equation:
    // 0 = ((constraint.ex_a * constraint.R_rel.T) * constraint.e)
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("constraint.ex_a").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::VarRef {
                    name: VarName::new("constraint.R_rel.T").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(Expression::VarRef {
                name: VarName::new("constraint.e").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "vector*matrix*vector residual should count as a scalar equation"
    );
}

#[test]
fn test_infer_scalar_count_zero_equals_vector_stays_vector() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("v"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("v"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Residual for equation: 0 = v
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("v").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "vector residual with scalar zero lhs should remain vector-sized"
    );
}

#[test]
fn test_infer_scalar_count_record_constructor_lhs_uses_constructor_fields() {
    let mut flat = Model::new();
    let mut complex = rumoca_core::Function::new("Complex", crate::test_support::test_span());
    complex.is_constructor = true;
    complex.add_input(rumoca_core::FunctionParam::new(
        "re",
        "Real",
        crate::test_support::test_span(),
    ));
    complex.add_input(rumoca_core::FunctionParam::new(
        "im",
        "Real",
        crate::test_support::test_span(),
    ));
    flat.add_function(complex);

    for name in ["pin_p.i.re", "pin_p.i.im", "pin_n.i.re", "pin_n.i.im"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let zero_complex = Expression::FunctionCall {
        name: VarName::new("Complex").into(),
        args: vec![
            Expression::Literal {
                value: Literal::Integer(0),
                span: crate::test_support::test_span(),
            },
            Expression::Literal {
                value: Literal::Integer(0),
                span: crate::test_support::test_span(),
            },
        ],
        is_constructor: true,
        span: crate::test_support::test_span(),
    };
    let current_sum = Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(make_var_ref("pin_p.i")),
        rhs: Box::new(make_var_ref("pin_n.i")),
        span: crate::test_support::test_span(),
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(zero_complex),
        rhs: Box::new(current_sum),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 2,
        "record constructor equations should count constructor field lanes"
    );
}

#[test]
fn test_infer_scalar_count_elementwise_mul_residual_is_vector() {
    let mut flat = Model::new();
    for name in ["a", "b", "c"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims: vec![3],
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // Residual for equation: 0 = a .* b - c
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::MulElem,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("a").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::VarRef {
                    name: VarName::new("b").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(Expression::VarRef {
                name: VarName::new("c").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "element-wise vector multiply should remain vector-sized"
    );
}

#[test]
fn test_build_prefix_counts_normalizes_embedded_subscripts() {
    let mut flat = Model::new();
    for name in [
        "r1.v[1].re",
        "r1.v[1].im",
        "r1.v[2].re",
        "r1.v[2].im",
        "r1.v[3].re",
        "r1.v[3].im",
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let prefix_counts = build_prefix_counts(&flat);
    assert_eq!(
        prefix_counts.get("r1.v").copied(),
        Some(6),
        "normalized prefix should aggregate scalarized element fields"
    );
}

#[test]
fn test_infer_equation_scalar_count_connector_field_array_alias() {
    let mut flat = Model::new();
    for name in [
        "pin_n[1].v",
        "pin_n[2].v",
        "pin_n[3].v",
        "plug_n.pin[1].v",
        "plug_n.pin[2].v",
        "plug_n.pin[3].v",
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pin_n.v").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("plug_n.pin.v").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "connector-field array alias equation should infer phase count"
    );
}

#[test]
fn test_infer_equation_scalar_count_indexed_component_field_lhs_uses_field_width() {
    let mut flat = Model::new();
    for i in 1..=4 {
        for field in ["C_outflow", "m_flow", "p", "h_outflow"] {
            let name = format!("src[{i}].ports[1].{field}");
            flat.add_variable(
                VarName::new(name.clone()),
                flat::Variable {
                    name: VarName::new(name),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(crate::test_support::test_span())
                },
            );
        }
    }

    let lhs = Expression::FieldAccess {
        base: Box::new(Expression::FieldAccess {
            base: Box::new(Expression::Index {
                base: Box::new(Expression::VarRef {
                    name: VarName::new("src").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                subscripts: vec![rumoca_core::Subscript::Index {
                    value: 1,
                    span: crate::test_support::test_span(),
                }],
                span: crate::test_support::test_span(),
            }),
            field: "ports".to_string(),
            span: crate::test_support::test_span(),
        }),
        field: "C_outflow".to_string(),
        span: crate::test_support::test_span(),
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "indexed component field LHS must use the selected field width, not the whole component prefix"
    );
}

#[test]
fn test_infer_equation_scalar_count_repeated_indexed_component_leaf_is_scalar() {
    let mut flat = Model::new();
    for i in 1..=3 {
        for field in ["Goff", "Ron", "s", "unitCurrent", "unitVoltage", "v"] {
            let name = format!("triac.triac[{i}].thyristor1.{field}");
            flat.add_variable(
                VarName::new(name.clone()),
                flat::Variable {
                    name: VarName::new(name),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(crate::test_support::test_span())
                },
            );
        }
    }

    let indexed_component = Expression::FieldAccess {
        base: Box::new(Expression::Index {
            base: Box::new(Expression::VarRef {
                name: VarName::new("triac").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            subscripts: vec![rumoca_core::Subscript::Index {
                value: 1,
                span: crate::test_support::test_span(),
            }],
            span: crate::test_support::test_span(),
        }),
        field: "thyristor1".to_string(),
        span: crate::test_support::test_span(),
    };
    let lhs = Expression::FieldAccess {
        base: Box::new(indexed_component.clone()),
        field: "v".to_string(),
        span: crate::test_support::test_span(),
    };
    let rhs = Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: Box::new(Expression::FieldAccess {
            base: Box::new(indexed_component),
            field: "s".to_string(),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::FieldAccess {
            base: Box::new(Expression::FieldAccess {
                base: Box::new(Expression::Index {
                    base: Box::new(Expression::VarRef {
                        name: VarName::new("triac").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    subscripts: vec![rumoca_core::Subscript::Index {
                        value: 1,
                        span: crate::test_support::test_span(),
                    }],
                    span: crate::test_support::test_span(),
                }),
                field: "thyristor1".to_string(),
                span: crate::test_support::test_span(),
            }),
            field: "unitCurrent".to_string(),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "scalar leaf equation on a repeated indexed component must not inherit the full component aggregate width"
    );
}

#[test]
fn test_infer_equation_scalar_count_repeated_indexed_connection_residual_is_scalar() {
    let mut flat = Model::new();
    for i in 1..=3 {
        for field in ["p.i", "n.i"] {
            let name = format!("triac.triac[{i}].thyristor1.{field}");
            flat.add_variable(
                VarName::new(name.clone()),
                flat::Variable {
                    name: VarName::new(name),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(crate::test_support::test_span())
                },
            );
        }
    }

    let pin_current = |pin: &str| Expression::FieldAccess {
        base: Box::new(Expression::FieldAccess {
            base: Box::new(Expression::FieldAccess {
                base: Box::new(Expression::Index {
                    base: Box::new(Expression::VarRef {
                        name: VarName::new("triac").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    subscripts: vec![rumoca_core::Subscript::Index {
                        value: 1,
                        span: crate::test_support::test_span(),
                    }],
                    span: crate::test_support::test_span(),
                }),
                field: "thyristor1".to_string(),
                span: crate::test_support::test_span(),
            }),
            field: pin.to_string(),
            span: crate::test_support::test_span(),
        }),
        field: "i".to_string(),
        span: crate::test_support::test_span(),
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(pin_current("p")),
            rhs: Box::new(pin_current("n")),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "connection residual over scalar leaves must not inherit repeated component aggregate width"
    );
}

#[test]
fn test_infer_equation_scalar_count_record_prefix_uses_scalarized_children() {
    let mut flat = Model::new();

    flat.add_variable(
        VarName::new("state"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("state"),
            is_primitive: false,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    for name in ["state.p", "state.T"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("state").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Modelica.Media.Common.smoothStep").into(),
            args: vec![],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 2,
        "record prefix equations should count scalarized primitive child fields"
    );
}

#[test]
fn test_infer_equation_scalar_count_record_array_range_lhs_uses_full_slice_size() {
    let mut flat = Model::new();

    flat.add_variable(
        VarName::new("pipe.n"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("pipe.n"),
            is_primitive: true,
            binding: Some(Expression::Literal {
                value: Literal::Integer(2),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Record array fields for an array of 2 state records with 5 scalar members.
    for field in ["T", "d", "h", "p", "phase"] {
        flat.add_variable(
            VarName::new(format!("pipe.statesFM.{field}")),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(format!("pipe.statesFM.{field}")),
                dims: vec![2],
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // LHS is a range slice over the record array. This should count both
    // selected elements: 2 records * 5 scalars each = 10 equations.
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.statesFM[1:pipe.n]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 10,
        "record-array range LHS should scale by selected elements, not per-element size"
    );
}

#[test]
fn test_infer_equation_scalar_count_structured_range_subscript_uses_slice_size() {
    let mut flat = Model::new();
    let transformer_i_ref = rumoca_core::ComponentReference {
        local: false,
        span: crate::test_support::test_span(),
        parts: vec![
            rumoca_core::ComponentRefPart {
                ident: "transformerL".to_string(),
                span: crate::test_support::test_span(),
                subs: vec![],
            },
            rumoca_core::ComponentRefPart {
                ident: "i".to_string(),
                span: crate::test_support::test_span(),
                subs: vec![],
            },
        ],
        def_id: None,
    };

    flat.add_variable(
        VarName::new("m"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("m"),
            is_primitive: true,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Integer(3),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("transformerL.i"),
        flat::Variable {
            name: VarName::new("transformerL.i"),
            component_ref: Some(transformer_i_ref.clone()),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                "transformerL.i",
                transformer_i_ref,
            ),
            subscripts: vec![rumoca_core::Subscript::expr(
                Box::new(Expression::Range {
                    start: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: crate::test_support::test_span(),
                    }),
                    step: None,
                    end: Box::new(Expression::VarRef {
                        name: VarName::new("m").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    span: crate::test_support::test_span(),
                }),
                crate::test_support::test_span(),
            )],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Zeros,
            args: vec![Expression::VarRef {
                name: VarName::new("m").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "structured range subscripts should count as vector slices"
    );
}

#[test]
fn test_infer_equation_scalar_count_qualified_parameter_range_subscript() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("pipe.n"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("pipe.n"),
            is_primitive: true,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Integer(2),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    for name in ["pipe.m_flows", "pipe.flowModel.m_flows"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims: vec![3],
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let range = || {
        rumoca_core::Subscript::expr(
            Box::new(Expression::Range {
                start: Box::new(Expression::Literal {
                    value: Literal::Integer(1),
                    span: crate::test_support::test_span(),
                }),
                step: None,
                end: Box::new(Expression::Binary {
                    op: rumoca_core::OpBinary::Add,
                    lhs: Box::new(Expression::VarRef {
                        name: VarName::new("pipe.n").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    rhs: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: crate::test_support::test_span(),
                    }),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            }),
            crate::test_support::test_span(),
        )
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.m_flows").into(),
            subscripts: vec![range()],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.flowModel.m_flows").into(),
            subscripts: vec![range()],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(scalar_count, 3);
}

#[test]
fn test_infer_equation_scalar_count_record_array_range_uses_parameter_start_fallback() {
    let mut flat = Model::new();

    // Parameter without explicit binding (value available via start).
    flat.add_variable(
        VarName::new("pipe.n"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("pipe.n"),
            is_primitive: true,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            start: Some(Expression::Literal {
                value: Literal::Integer(1),
                span: crate::test_support::test_span(),
            }),
            binding: None,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Record array fields for an array of 2 state records with 5 scalar members.
    for field in ["T", "d", "h", "p", "phase"] {
        flat.add_variable(
            VarName::new(format!("pipe.statesFM.{field}")),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(format!("pipe.statesFM.{field}")),
                dims: vec![2],
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // 2:(pipe.n + 1) with pipe.n=1 should select one record element.
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.statesFM[2:(pipe.n + 1)]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 5,
        "record-array range LHS should use evaluable parameter start values for range bounds"
    );
}

#[test]
fn test_infer_equation_scalar_count_record_array_range_uses_known_lower_bound_when_upper_is_unknown()
 {
    let mut flat = Model::new();

    // Record array fields for an array of 2 state records with 5 scalar members.
    for field in ["T", "d", "h", "p", "phase"] {
        flat.add_variable(
            VarName::new(format!("pipe.statesFM.{field}")),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(format!("pipe.statesFM.{field}")),
                dims: vec![2],
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // End bound depends on an unknown symbol. Use the known lower bound and
    // declared dimension to avoid over-counting (2:dim -> one element).
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.statesFM[2:(pipe.n + 1)]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 5,
        "record-array range LHS should clamp unknown upper bounds using known lower bounds"
    );
}

#[test]
fn test_infer_equation_scalar_count_record_array_range_with_scalarized_field_indices() {
    let mut flat = Model::new();

    flat.add_variable(
        VarName::new("pipe.n"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("pipe.n"),
            is_primitive: true,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Integer(1),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Record fields already scalarized into indexed names (dims = []).
    for idx in [1, 2] {
        for field in ["T", "d", "h", "p", "phase"] {
            let name = format!("pipe.statesFM[{idx}].{field}");
            flat.add_variable(
                VarName::new(name.clone()),
                crate::test_support::with_component_ref(flat::Variable {
                    name: VarName::new(name),
                    is_primitive: true,
                    ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ))
                }),
            );
        }
    }

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.statesFM[2:(pipe.n + 1)]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 5,
        "record-array range LHS should infer array length from indexed scalarized fields"
    );
}
