// SPEC_0021 file-size exception: connection regression coverage spans scalar,
// array, expandable, and stream connectors. split plan: move each connector
// family into a focused test module sharing a small common fixture builder.
use super::*;
use rumoca_core::TypeId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_flatten_connections_source_7.mo"),
        11,
        23,
    )
}

fn create_test_model() -> flat::Model {
    let mut flat = flat::Model::new();

    // Add Pin.v (non-flow)
    let pin_v = flat::Variable {
        name: rumoca_core::VarName::new("pin.v"),
        flow: false,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("pin.v"), pin_v);

    // Add Pin.i (flow)
    let pin_i = flat::Variable {
        name: rumoca_core::VarName::new("pin.i"),
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("pin.i"), pin_i);

    flat
}

#[test]
fn test_connection_involves_disabled_handles_dot_inside_bracket_expression() {
    let conn = ast::InstanceConnection {
        a: ast::QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
            ],
        },
        b: ast::QualifiedName::from_ident("sink"),
        connector_type: None,
        span: Span::DUMMY,
        scope: String::new(),
        family: None,
    };

    let mut disabled = indexmap::IndexSet::new();
    disabled.insert(rumoca_core::ComponentPath::from_parts([
        "bus[data.medium]",
        "pin",
    ]));
    assert!(connection_involves_disabled(&conn, &disabled));
}

#[test]
fn test_connection_involves_disabled_ignores_non_matching_bracket_expression() {
    let conn = ast::InstanceConnection {
        a: ast::QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
            ],
        },
        b: ast::QualifiedName::from_ident("sink"),
        connector_type: None,
        span: Span::DUMMY,
        scope: String::new(),
        family: None,
    };

    let mut disabled = indexmap::IndexSet::new();
    disabled.insert(rumoca_core::ComponentPath::from_parts([
        "bus[data.other]",
        "pin",
    ]));
    assert!(!connection_involves_disabled(&conn, &disabled));
}

#[test]
fn test_is_flow_variable() {
    let flat = create_test_model();

    // Pin.i is flow
    assert!(is_flow_variable(&flat, &rumoca_core::VarName::new("pin.i")));

    // Pin.v is not flow
    assert!(!is_flow_variable(
        &flat,
        &rumoca_core::VarName::new("pin.v")
    ));

    // Unknown variable returns false
    assert!(!is_flow_variable(
        &flat,
        &rumoca_core::VarName::new("unknown")
    ));
}

#[test]
fn test_is_stream_variable() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("pin.h_outflow"),
        flat::Variable {
            stream: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    assert!(is_stream_variable(
        &flat,
        &rumoca_core::VarName::new("pin.h_outflow")
    ));
    assert!(!is_stream_variable(
        &flat,
        &rumoca_core::VarName::new("pin.v")
    ));
}

#[test]
fn test_connect_primitive_vars_routes_streams_to_stream_set() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("a.h_outflow"),
        flat::Variable {
            stream: true,
            source_span: test_span(),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("b.h_outflow"),
        flat::Variable {
            stream: true,
            source_span: test_span(),
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    connect_primitive_vars(
        &rumoca_core::VarName::new("a.h_outflow"),
        &rumoca_core::VarName::new("b.h_outflow"),
        &flat,
        &mut flow_pairs,
        &mut potential_uf,
        &mut stream_uf,
    )
    .expect("stream-to-stream is the pairing MLS §9.3 admits");

    assert!(flow_pairs.is_empty());
    assert!(
        potential_uf.get_sets().is_empty(),
        "stream connect() must not generate potential equality sets"
    );
    assert_eq!(stream_uf.get_sets().len(), 1);
}

#[test]
fn test_stream_connection_does_not_generate_potential_equality() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("a.h_outflow"),
        flat::Variable {
            name: rumoca_core::VarName::new("a.h_outflow"),
            stream: true,
            source_span: test_span(),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("b.h_outflow"),
        flat::Variable {
            name: rumoca_core::VarName::new("b.h_outflow"),
            stream: true,
            source_span: test_span(),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    for name in ["a.m_flow", "b.m_flow"] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                flow: true,
                source_span: test_span(),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: rumoca_core::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("a.h_outflow"),
                b: ast::QualifiedName::from_dotted("b.h_outflow"),
                connector_type: None,
                span: Span::DUMMY,
                scope: String::new(),
                family: None,
            },
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("a.m_flow"),
                b: ast::QualifiedName::from_dotted("b.m_flow"),
                connector_type: None,
                span: Span::DUMMY,
                scope: String::new(),
                family: None,
            },
        ],
        ..Default::default()
    });

    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    process_connections(&mut flat, &overlay, false, &mut oc_forest)
        .expect("stream connection processing");

    assert_eq!(flat.equations.len(), 1, "only the flow sum is expected");
    assert!(matches!(
        flat.equations[0].origin,
        flat::EquationOrigin::FlowSum { .. }
    ));
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.h_outflow"))
            .is_some_and(|var| var.connected)
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("b.h_outflow"))
            .is_some_and(|var| var.connected)
    );
}

#[test]
fn test_connector_path_with_structural_member_expands_nonstructural_members() {
    let mut flat = flat::Model::new();
    for name in ["a", "b"] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                is_primitive: false,
                source_span: test_span(),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        flat.add_variable(
            rumoca_core::VarName::new(format!("{name}.m")),
            flat::Variable {
                name: rumoca_core::VarName::new(format!("{name}.m")),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                is_primitive: true,
                source_span: test_span(),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        flat.add_variable(
            rumoca_core::VarName::new(format!("{name}.v")),
            flat::Variable {
                name: rumoca_core::VarName::new(format!("{name}.v")),
                is_primitive: true,
                source_span: test_span(),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        flat.add_variable(
            rumoca_core::VarName::new(format!("{name}.i")),
            flat::Variable {
                name: rumoca_core::VarName::new(format!("{name}.i")),
                flow: true,
                is_primitive: true,
                source_span: test_span(),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: rumoca_core::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_ident("a"),
            b: ast::QualifiedName::from_ident("b"),
            connector_type: None,
            span: Span::DUMMY,
            scope: String::new(),
            family: None,
        }],
        ..Default::default()
    });

    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    process_connections(&mut flat, &overlay, false, &mut oc_forest)
        .expect("connector connection processing");

    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.v"))
            .is_some_and(|var| var.connected)
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.i"))
            .is_some_and(|var| var.connected)
    );
    assert!(
        !flat
            .variables
            .get(&rumoca_core::VarName::new("a.m"))
            .is_some_and(|var| var.connected),
        "structural connector members must not prevent nonstructural members from connecting"
    );
}

#[test]
fn collapsed_connector_array_connects_to_expanded_connector_elements() {
    let mut flat = flat::Model::new();
    for (name, dims, flow) in [
        ("source.port.T", vec![2], false),
        ("source.port.Q_flow", vec![2], true),
        ("sink.ports[1].T", vec![], false),
        ("sink.ports[1].Q_flow", vec![], true),
        ("sink.ports[2].T", vec![], false),
        ("sink.ports[2].Q_flow", vec![], true),
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                dims,
                flow,
                is_primitive: true,
                source_span: test_span(),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: rumoca_core::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("source.port"),
            b: ast::QualifiedName::from_dotted("sink.ports"),
            connector_type: None,
            span: test_span(),
            scope: String::new(),
            family: None,
        }],
        ..Default::default()
    });

    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    process_connections(&mut flat, &overlay, false, &mut oc_forest)
        .expect("asymmetric connector-array connection processing");

    for name in [
        "source.port.T",
        "source.port.Q_flow",
        "sink.ports[1].T",
        "sink.ports[1].Q_flow",
        "sink.ports[2].T",
        "sink.ports[2].Q_flow",
    ] {
        assert!(
            flat.variables
                .get(&rumoca_core::VarName::new(name))
                .is_some_and(|var| var.connected),
            "{name} should belong to the expanded connection set"
        );
    }
    assert!(
        flat.equations.iter().any(|equation| {
            equation.origin.to_string().contains("source.port.T[1]")
                && equation.origin.to_string().contains("sink.ports[1].T")
        }),
        "the collapsed potential array should be projected elementwise"
    );
}

fn expandable_connector_test_overlay() -> ast::InstanceOverlay {
    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: rumoca_core::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_ident("a"),
            b: ast::QualifiedName::from_ident("b"),
            connector_type: None,
            span: test_span(),
            scope: String::new(),
            family: None,
        }],
        ..Default::default()
    });
    overlay
}

fn add_expandable_member(flat: &mut flat::Model, name: &str) {
    flat.add_variable(
        rumoca_core::VarName::new(name),
        flat::Variable {
            name: rumoca_core::VarName::new(name),
            is_primitive: true,
            from_expandable_connector: true,
            source_span: test_span(),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
}

#[test]
fn expandable_connectors_with_same_declared_members_are_supported() {
    let mut flat = flat::Model::new();
    add_expandable_member(&mut flat, "a.signal");
    add_expandable_member(&mut flat, "b.signal");
    let overlay = expandable_connector_test_overlay();
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();

    process_connections(&mut flat, &overlay, false, &mut oc_forest)
        .expect("identical declared members require no augmentation");

    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.signal"))
            .is_some_and(|var| var.connected)
    );
}

#[test]
fn expandable_connector_member_union_is_rejected_before_connection_sets() {
    let mut flat = flat::Model::new();
    add_expandable_member(&mut flat, "a.left_only");
    add_expandable_member(&mut flat, "b.right_only");
    let overlay = expandable_connector_test_overlay();
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_connections(&mut flat, &overlay, false, &mut oc_forest)
        .expect_err("member-union augmentation must not silently drop both endpoints");

    assert!(matches!(
        error,
        FlattenError::UnsupportedExpandableConnectorAugmentation { .. }
    ));
    assert!(
        flat.equations.is_empty(),
        "the unsupported connection must fail before equations are generated"
    );
}

#[test]
fn expandable_connector_partial_member_union_is_rejected() {
    let mut flat = flat::Model::new();
    for name in ["a.shared", "a.left_only", "b.shared"] {
        add_expandable_member(&mut flat, name);
    }
    let overlay = expandable_connector_test_overlay();
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_connections(&mut flat, &overlay, false, &mut oc_forest)
        .expect_err("connecting only the declared intersection is not MLS §9.1.3");

    assert!(matches!(
        error,
        FlattenError::UnsupportedExpandableConnectorAugmentation { .. }
    ));
}

#[test]
fn test_is_flow_variable_subscripted_element_of_array_field() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("arr.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    assert!(is_flow_variable(
        &flat,
        &rumoca_core::VarName::new("arr.n.i[2]")
    ));
}

#[test]
fn test_union_find() {
    let mut uf = UnionFind::new();

    let a = rumoca_core::VarName::new("a");
    let b = rumoca_core::VarName::new("b");
    let c = rumoca_core::VarName::new("c");

    // Initially, each is its own set
    assert_eq!(uf.find(&a), a);
    assert_eq!(uf.find(&b), b);

    // Union a and b
    uf.union(&a, &b);
    assert_eq!(uf.find(&a), uf.find(&b));

    // Union b and c
    uf.union(&b, &c);
    assert_eq!(uf.find(&a), uf.find(&c));

    // Should have one set with all three
    let sets = uf.get_sets();
    assert_eq!(sets.len(), 1);
    assert_eq!(sets.values().next().unwrap().len(), 3);
}

#[test]
fn test_create_equality_residual() -> Result<(), rumoca_core::MissingProvenanceSpan> {
    let span = test_span().require_provenance("test connection equality")?;
    let lhs = var_to_expr(&rumoca_core::VarName::new("a"), span);
    let rhs = var_to_expr(&rumoca_core::VarName::new("b"), span);
    let residual = create_equality_residual(lhs, rhs, span);

    // Should be Binary { op: Sub, lhs: a, rhs: b }
    match residual {
        rumoca_core::Expression::Binary { op, .. } => {
            assert!(matches!(op, rumoca_core::OpBinary::Sub));
        }
        _ => panic!("Expected Binary expression"),
    }
    Ok(())
}

#[test]
fn test_create_sum() -> Result<(), rumoca_core::MissingProvenanceSpan> {
    let span = test_span().require_provenance("test connection sum")?;
    let exprs = vec![
        var_to_expr(&rumoca_core::VarName::new("a"), span),
        var_to_expr(&rumoca_core::VarName::new("b"), span),
        var_to_expr(&rumoca_core::VarName::new("c"), span),
    ];

    let sum = create_sum(exprs, span);

    // Should be ((a + b) + c)
    match sum {
        rumoca_core::Expression::Binary { op, .. } => {
            assert!(matches!(op, rumoca_core::OpBinary::Add));
        }
        _ => panic!("Expected Binary expression"),
    }
    Ok(())
}

#[test]
fn test_generate_equality_equations() {
    let mut flat = flat::Model::new();

    // Add variables
    flat.add_variable(
        rumoca_core::VarName::new("r1.n.v"),
        flat::Variable::empty_with_span(test_span()),
    );
    flat.add_variable(
        rumoca_core::VarName::new("r2.p.v"),
        flat::Variable::empty_with_span(test_span()),
    );
    flat.add_variable(
        rumoca_core::VarName::new("r3.p.v"),
        flat::Variable::empty_with_span(test_span()),
    );

    let vars = vec![
        rumoca_core::VarName::new("r1.n.v"),
        rumoca_core::VarName::new("r2.p.v"),
        rumoca_core::VarName::new("r3.p.v"),
    ];

    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    generate_equality_equations(&mut flat, &vars, test_span(), &mut oc_forest).unwrap();

    // Should generate 2 equations (n-1 for n=3)
    assert_eq!(flat.equations.len(), 2);

    // All variables should be marked as connected
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("r1.n.v"))
            .unwrap()
            .connected
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("r2.p.v"))
            .unwrap()
            .connected
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("r3.p.v"))
            .unwrap()
            .connected
    );
}

#[test]
fn zero_constraint_equality_generation_respects_required_vcg_branch() {
    let mut flat = flat::Model::new();
    for record in ["a.R", "b.R", "c.R"] {
        let name = rumoca_core::VarName::new(format!("{record}.gamma"));
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name,
                is_primitive: true,
                is_overconstrained: true,
                oc_record_path: Some(record.to_string()),
                oc_eq_constraint_size: Some(0),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    let branches = vec![("a.R".to_string(), "b.R".to_string())];
    let optional = vec![
        ("a.R".to_string(), "c.R".to_string()),
        ("c.R".to_string(), "b.R".to_string()),
    ];
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::new(
        crate::vcg::test_required_forest(&Default::default(), &branches, &optional),
    );
    let vars = [
        rumoca_core::VarName::new("a.R.gamma"),
        rumoca_core::VarName::new("c.R.gamma"),
        rumoca_core::VarName::new("b.R.gamma"),
    ];

    generate_equality_equations(&mut flat, &vars, test_span(), &mut oc_forest).unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert!(matches!(
        &flat.equations[0].origin,
        flat::EquationOrigin::Connection { lhs, rhs }
            if lhs == "a.R.gamma" && rhs == "c.R.gamma"
    ));
}

#[test]
fn test_generate_flow_equation() {
    let mut flat = flat::Model::new();

    // Add flow variables
    let v1 = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("r1.n.i"), v1);

    let v2 = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("r2.p.i"), v2);

    let vars = vec![
        rumoca_core::VarName::new("r1.n.i"),
        rumoca_core::VarName::new("r2.p.i"),
    ];

    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, indexmap::IndexSet<rumoca_core::VarName>>::default(),
        test_span(),
    )
    .unwrap();

    // Should generate 1 equation (sum = 0)
    assert_eq!(flat.equations.len(), 1);

    // Variables should be marked as connected
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("r1.n.i"))
            .unwrap()
            .connected
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("r2.p.i"))
            .unwrap()
            .connected
    );
}

#[test]
fn test_generate_flow_equation_marks_base_connected_for_subscripted_var() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("a.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("b.n.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let vars = vec![
        rumoca_core::VarName::new("a.n.i[2]"),
        rumoca_core::VarName::new("b.n.i"),
    ];
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, indexmap::IndexSet<rumoca_core::VarName>>::default(),
        test_span(),
    )
    .unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 1);
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.n.i"))
            .unwrap()
            .connected
    );
}

#[test]
fn test_generate_flow_equation_subscripted_unknown_dims_is_scalar() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("a.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("b.n.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let vars = vec![
        rumoca_core::VarName::new("a.n.i[2]"),
        rumoca_core::VarName::new("b.n.i"),
    ];
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, indexmap::IndexSet<rumoca_core::VarName>>::default(),
        test_span(),
    )
    .unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 1);
}

#[test]
fn test_generate_flow_equation_mixed_scalar_and_array_is_scalar_sum() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("arr.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![2],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("s.n.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let vars = vec![
        rumoca_core::VarName::new("arr.n.i"),
        rumoca_core::VarName::new("s.n.i"),
    ];
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, indexmap::IndexSet<rumoca_core::VarName>>::default(),
        test_span(),
    )
    .unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(
        flat.equations[0].scalar_count, 1,
        "scalar+array flow connection sets should contribute one scalar flow-sum equation"
    );
}

#[test]
fn test_generate_flow_equation_two_arrays_and_scalar_keeps_array_scalar_count() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("arr1.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![2],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("arr2.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![2],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("s.n.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let vars = vec![
        rumoca_core::VarName::new("arr1.n.i"),
        rumoca_core::VarName::new("arr2.n.i"),
        rumoca_core::VarName::new("s.n.i"),
    ];
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, indexmap::IndexSet<rumoca_core::VarName>>::default(),
        test_span(),
    )
    .unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(
        flat.equations[0].scalar_count, 2,
        "flow sum with multiple array terms should preserve array scalarization"
    );
    assert_eq!(flat.structured_equations.len(), 1);
    let family = &flat.structured_equations[0];
    assert_eq!(family.first_equation_index, 0);
    assert_eq!(
        family
            .domain
            .scalar_count()
            .expect("valid connection family"),
        2
    );
    assert!(matches!(
        family.origin,
        flat::EquationOrigin::FlowSum { .. }
    ));
}

#[test]
fn test_generate_flow_equation_sign_convention() {
    let mut flat = flat::Model::new();

    // Add inside connector (3 parts: component.connector.variable)
    let v_inside = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("r.p.i"), v_inside);

    // Add outside connector (2 parts: connector.variable)
    let v_outside = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("p.i"), v_outside);

    let vars = vec![
        rumoca_core::VarName::new("r.p.i"),
        rumoca_core::VarName::new("p.i"),
    ];

    let mut interface_flow_vars_by_scope = IndexMap::default();
    interface_flow_vars_by_scope.insert(
        String::new(),
        indexmap::IndexSet::from([rumoca_core::VarName::new("p.i")]),
    );
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &interface_flow_vars_by_scope,
        test_span(),
    )
    .unwrap();

    // Should generate 1 equation
    assert_eq!(flat.equations.len(), 1);

    // Check the origin shows correct signs:
    // r.p.i is inside (+), p.i is outside (-)
    let origin = &flat.equations[0].origin;
    let origin_str = origin.to_string();
    assert!(
        origin_str.contains("r.p.i") && origin_str.contains("-p.i"),
        "Expected 'r.p.i + -p.i = 0', got: {}",
        origin_str
    );
}

#[test]
fn test_generate_flow_equation_sign_for_nested_outside_connector_member() {
    let mut flat = flat::Model::new();

    let outside_nested = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("port.frame.f"), outside_nested);

    let inside = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("comp.port.f"), inside);

    let vars = vec![
        rumoca_core::VarName::new("port.frame.f"),
        rumoca_core::VarName::new("comp.port.f"),
    ];
    let mut interface_flow_vars_by_scope = IndexMap::default();
    interface_flow_vars_by_scope.insert(
        String::new(),
        indexmap::IndexSet::from([rumoca_core::VarName::new("port.frame.f")]),
    );
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &interface_flow_vars_by_scope,
        test_span(),
    )
    .unwrap();

    let origin = &flat.equations[0].origin;
    let origin_str = origin.to_string();
    assert!(
        origin_str.contains("-port.frame.f") && origin_str.contains("comp.port.f"),
        "Expected outside nested connector member to be negated, got: {}",
        origin_str
    );
}

#[test]
fn test_generate_flow_equation_sign_for_scalarized_outside_connector_array_member() {
    let mut flat = flat::Model::new();

    let outside_scalarized = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(
        rumoca_core::VarName::new("cell.plug.pin[1].i"),
        outside_scalarized,
    );

    let inside = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("cell.diode.p.i"), inside);

    let vars = vec![
        rumoca_core::VarName::new("cell.plug.pin[1].i"),
        rumoca_core::VarName::new("cell.diode.p.i"),
    ];
    let mut interface_flow_vars_by_scope = IndexMap::default();
    interface_flow_vars_by_scope.insert(
        "cell".to_string(),
        indexmap::IndexSet::from([rumoca_core::VarName::new("cell.plug.pin.i")]),
    );
    generate_flow_equation(
        &mut flat,
        &vars,
        "cell",
        &interface_flow_vars_by_scope,
        test_span(),
    )
    .unwrap();

    let origin = &flat.equations[0].origin;
    let origin_str = origin.to_string();
    assert!(
        origin_str.contains("-cell.plug.pin[1].i") && origin_str.contains("cell.diode.p.i"),
        "Expected scalarized outside connector array member to be negated, got: {}",
        origin_str
    );
}

#[test]
fn test_process_connections_negates_nested_connector_under_outside_root() {
    let mut flat = flat::Model::new();
    for name in ["cell.plug.pin[1].i", "cell.diode.p.i"] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                flow: true,
                is_primitive: true,
                source_span: test_span(),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_component(ast::InstanceData {
        instance_id: rumoca_core::InstanceId(1),
        qualified_name: ast::QualifiedName::from_dotted("cell.plug"),
        is_connector_type: true,
        is_protected: false,
        ..Default::default()
    });
    overlay.add_class(ast::ClassInstanceData {
        instance_id: rumoca_core::InstanceId(0),
        qualified_name: ast::QualifiedName::from_dotted("cell"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("cell.plug.pin"),
            b: ast::QualifiedName::from_dotted("cell.diode.p"),
            connector_type: None,
            span: test_span(),
            scope: "cell".to_string(),
            family: None,
        }],
        ..Default::default()
    });

    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    process_connections(&mut flat, &overlay, false, &mut oc_forest)
        .expect("nested connector connection");

    let flow_origins: Vec<String> = flat
        .equations
        .iter()
        .map(|eq| eq.origin.to_string())
        .collect();
    assert!(
        flow_origins.iter().any(|origin| {
            origin.contains("-cell.plug.pin[1].i") && origin.contains("cell.diode.p.i")
        }),
        "Expected nested connector member under outside root to be negated, got: {:?}",
        flow_origins
    );
}

#[test]
fn test_interface_path_uses_single_identifier_fallback_when_roots_do_not_match() {
    let mut roots = InterfaceConnectorRootsByScope::default();
    roots
        .entry("cell".to_string())
        .or_default()
        .insert(rumoca_core::ComponentPath::from_flat_path("cell.unrelated"));

    assert!(is_interface_connection_path_for_scope(
        "cell.plug",
        "cell",
        &roots
    ));
    assert!(!is_interface_connection_path_for_scope(
        "cell.inner.plug",
        "cell",
        &roots
    ));
}

#[test]
fn test_generate_flow_equation_uses_scope_specific_interface_flows() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("cell.p.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("cell.multiSensor.pc.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let vars = vec![
        rumoca_core::VarName::new("cell.p.i"),
        rumoca_core::VarName::new("cell.multiSensor.pc.i"),
    ];
    let mut interface_flow_vars_by_scope = IndexMap::default();
    interface_flow_vars_by_scope.insert(
        "cell".to_string(),
        indexmap::IndexSet::from([rumoca_core::VarName::new("cell.p.i")]),
    );
    generate_flow_equation(
        &mut flat,
        &vars,
        "cell",
        &interface_flow_vars_by_scope,
        test_span(),
    )
    .unwrap();

    let origin_str = flat.equations[0].origin.to_string();
    assert!(
        origin_str.contains("-cell.p.i") && origin_str.contains("cell.multiSensor.pc.i"),
        "Expected nested scope outside connector to be negated, got: {}",
        origin_str
    );
}

#[test]
fn test_validate_flow_consistency_ok() {
    let mut flat = flat::Model::new();

    // Both flow
    let v1 = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a.i"), v1);

    let v2 = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b.i"), v2);

    // Should succeed
    let result = validate_flow_consistency(
        &flat,
        &rumoca_core::VarName::new("a.i"),
        &rumoca_core::VarName::new("b.i"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn test_validate_flow_consistency_mismatch() {
    let mut flat = flat::Model::new();

    // One flow, one non-flow
    let v1 = flat::Variable {
        flow: true,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a.i"), v1);

    let v2 = flat::Variable {
        flow: false,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b.v"), v2);

    // Should fail
    let result = validate_flow_consistency(
        &flat,
        &rumoca_core::VarName::new("a.i"),
        &rumoca_core::VarName::new("b.v"),
        Span::DUMMY,
    );
    assert!(result.is_err());
}

#[test]
fn test_validate_dimension_compatibility_ok() {
    let mut flat = flat::Model::new();

    // Same dimensions
    let v1 = flat::Variable {
        dims: vec![3],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        dims: vec![3],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b"), v2);

    // Should succeed
    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn test_validate_dimension_compatibility_mismatch() {
    let mut flat = flat::Model::new();

    // Different dimensions
    let v1 = flat::Variable {
        dims: vec![3],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        dims: vec![5],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b"), v2);

    // Mismatched dimensions should fail
    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_err());
}

#[test]
fn test_validate_dimension_compatibility_io_mismatch_still_fails() {
    let mut flat = flat::Model::new();

    let v1 = flat::Variable {
        dims: vec![2],
        causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("u"), v1);

    let v2 = flat::Variable {
        dims: vec![3],
        causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("y"), v2);

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("u"),
        &rumoca_core::VarName::new("y"),
        Span::DUMMY,
    );
    assert!(
        result.is_err(),
        "MLS §9.2 requires connect() array dimensions to match even for input/output pairs"
    );
}

#[test]
fn test_validate_dimension_compatibility_partial_subscript_projects_remaining_dims() {
    let mut flat = flat::Model::new();

    let lhs = flat::Variable {
        dims: vec![2, 3],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a"), lhs);

    let rhs = flat::Variable {
        dims: vec![3],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b"), rhs);

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("a[1]"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(
        result.is_ok(),
        "A[1] for A[2,3] should preserve trailing dimension [3]"
    );
}

#[test]
fn test_validate_dimension_compatibility_partial_subscript_mismatch_fails() {
    let mut flat = flat::Model::new();

    let lhs = flat::Variable {
        dims: vec![2, 3],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a"), lhs);

    let rhs = flat::Variable {
        dims: vec![4],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b"), rhs);

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("a[1]"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(
        result.is_err(),
        "A[1] for A[2,3] has projected dims [3], so it must reject [4]"
    );
}

#[test]
fn test_generate_equality_marks_base_connected_for_multidimensional_subscript() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("bus.values"),
        flat::Variable {
            dims: vec![3, 2],
            from_expandable_connector: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("gain.y"),
        flat::Variable::empty_with_span(test_span()),
    );

    generate_equality_equations(
        &mut flat,
        &[
            rumoca_core::VarName::new("bus.values[3,2]"),
            rumoca_core::VarName::new("gain.y"),
        ],
        test_span(),
        &mut crate::vcg::OverconstrainedEquationForest::empty(),
    )
    .unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("bus.values"))
            .unwrap()
            .connected
    );
}

#[test]
fn test_split_trailing_index_groups_multi_index() {
    let (base, groups) =
        split_trailing_index_groups("connector.field[2][3]").expect("should parse");
    assert_eq!(base, "connector.field");
    assert_eq!(groups, vec!["[2]".to_string(), "[3]".to_string()]);
}

#[test]
fn test_validate_type_compatibility_ok() {
    let mut flat = flat::Model::new();
    let type_roots = IndexMap::default();

    // Both same type (type_id = 1 for both)
    let v1 = flat::Variable {
        type_id: TypeId(1), // Same type
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: TypeId(1), // Same type
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b"), v2);

    // Should succeed
    let result = validate_type_compatibility(
        &flat,
        &type_roots,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn test_validate_type_compatibility_mismatch() {
    let mut flat = flat::Model::new();
    let type_roots = IndexMap::default();

    // Different types (type_id = 1 vs 2)
    let v1 = flat::Variable {
        type_id: TypeId(1), // e.g., Real
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: TypeId(2), // e.g., Integer
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b"), v2);

    // Should fail
    let result = validate_type_compatibility(
        &flat,
        &type_roots,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_err());
}

#[test]
fn test_validate_type_compatibility_unknown_allowed() {
    let mut flat = flat::Model::new();
    let type_roots = IndexMap::default();

    let v1 = flat::Variable {
        type_id: TypeId::UNKNOWN,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: TypeId(1),
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b"), v2);

    let result = validate_type_compatibility(
        &flat,
        &type_roots,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn test_validate_type_compatibility_alias_root_allowed() {
    let mut flat = flat::Model::new();
    let mut type_roots = IndexMap::default();

    let alias = TypeId(9);
    let root = TypeId(1);
    type_roots.insert(alias, root);

    let v1 = flat::Variable {
        type_id: alias,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: root,
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b"), v2);

    let result = validate_type_compatibility(
        &flat,
        &type_roots,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

mod member_pairing_tests;
mod path_matching_tests;
