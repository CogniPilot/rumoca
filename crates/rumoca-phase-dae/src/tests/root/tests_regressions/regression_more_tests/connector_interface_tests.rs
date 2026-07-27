//! Regression coverage for expandable-connector interface inputs and overconstrained-connection record components.

use super::*;

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
