// SPEC_0021 file-size exception: connection regression coverage spans scalar,
// array, expandable, and stream connectors. split plan: move each connector
// family into a focused test module sharing a small common fixture builder.
use super::*;
use indexmap::IndexSet;
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
    let mut flat = connection_test_model();

    // Add Pin.v (non-flow)
    let pin_v = flat::Variable {
        name: rumoca_core::VarName::new("pin.v"),
        flow: false,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("pin.v"), pin_v);

    // Add Pin.i (flow)
    let pin_i = flat::Variable {
        name: rumoca_core::VarName::new("pin.i"),
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("pin.i"), pin_i);

    flat
}

fn process_test_connections(
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    forest: &mut crate::vcg::OverconstrainedEquationForest,
) -> Result<(), FlattenError> {
    finalize_connection_test_flat(flat);
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("connection fixture must construct finalized occurrence proofs");
    equation_generation::process_connections_for_test(flat, &overconstrained, forest)
}

fn pair_ordinary_component(flat: &flat::Model, overlay: &mut ast::InstanceOverlay, name: &str) {
    let flat_id = flat.variables[&rumoca_core::VarName::new(name)].instance_id;
    let overlay_id = overlay.alloc_id();
    assert_eq!(
        overlay_id, flat_id,
        "the fixture must issue Flat and Instance occurrence identity atomically"
    );
    overlay
        .type_roots
        .insert(CONNECTION_TEST_SCALAR_TYPE, CONNECTION_TEST_SCALAR_TYPE);
    overlay
        .add_component(ast::InstanceData {
            instance_id: overlay_id,
            qualified_name: ast::QualifiedName::from_dotted(name),
            type_id: CONNECTION_TEST_SCALAR_TYPE,
            is_primitive: flat.variables[&rumoca_core::VarName::new(name)].is_primitive,
            ..Default::default()
        })
        .expect("the paired fixture occurrence is allocated exactly once");
}

fn generate_flow_equation(
    flat: &mut flat::Model,
    variables: &[rumoca_core::VarName],
    scope: &str,
    interface_flows: &IndexMap<String, IndexSet<rumoca_core::VarName>>,
    span: Span,
) -> Result<(), FlattenError> {
    finalize_connection_test_flat(flat);
    equation_generation::generate_flow_equation(flat, variables, scope, interface_flows, span)
}

fn generate_outside_stream_equations(
    flat: &mut flat::Model,
    endpoints: &InterfaceStreamEndpointsByScope,
    stream_endpoints: &super::stream_operators::StreamConnectionEndpoints,
) -> Result<(), FlattenError> {
    finalize_connection_test_flat(flat);
    equation_generation::generate_outside_stream_equations(flat, endpoints, stream_endpoints)
}

fn test_var_expr(var_name: &rumoca_core::VarName, span: ProvenanceSpan) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: var_name.clone().into(),
        subscripts: Vec::new(),
        span: span.span(),
    }
}

#[test]
fn test_connection_involves_disabled_handles_dot_inside_bracket_expression() {
    let conn = ast::InstanceScalarConnection::new(
        ast::QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
            ],
        },
        ast::QualifiedName::from_ident("sink"),
        None,
        test_span(),
        String::new(),
    )
    .expect("test scalar connection is valid");

    let mut disabled = indexmap::IndexSet::new();
    disabled.insert(rumoca_core::ComponentPath::from_parts([
        "bus[data.medium]",
        "pin",
    ]));
    assert!(connection_involves_disabled(&conn, &disabled));
}

#[test]
fn test_connection_involves_disabled_ignores_non_matching_bracket_expression() {
    let conn = ast::InstanceScalarConnection::new(
        ast::QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
            ],
        },
        ast::QualifiedName::from_ident("sink"),
        None,
        test_span(),
        String::new(),
    )
    .expect("test scalar connection is valid");

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
    assert!(is_flow_variable(&flat, &rumoca_core::VarName::new("pin.i"), test_span()).unwrap());

    // Pin.v is not flow
    assert!(!is_flow_variable(&flat, &rumoca_core::VarName::new("pin.v"), test_span(),).unwrap());

    // Missing semantic evidence is not a non-flow default.
    assert!(is_flow_variable(&flat, &rumoca_core::VarName::new("unknown"), test_span()).is_err());
}

#[test]
fn test_is_stream_variable() {
    let mut flat = connection_test_model();
    flat.add_test_variable(
        rumoca_core::VarName::new("pin.h_outflow"),
        flat::Variable {
            stream: true,
            ..connection_test_variable(test_span())
        },
    );
    flat.add_test_variable(
        rumoca_core::VarName::new("pin.v"),
        connection_test_variable(test_span()),
    );

    assert!(
        is_stream_variable(
            &flat,
            &rumoca_core::VarName::new("pin.h_outflow"),
            test_span(),
        )
        .unwrap()
    );
    assert!(!is_stream_variable(&flat, &rumoca_core::VarName::new("pin.v"), test_span(),).unwrap());
}

#[test]
fn test_connect_primitive_vars_routes_streams_to_stream_set() {
    let mut flat = connection_test_model();
    flat.add_test_variable(
        rumoca_core::VarName::new("a.h_outflow"),
        flat::Variable {
            stream: true,
            source_span: test_span(),
            ..connection_test_variable(test_span())
        },
    );
    flat.add_test_variable(
        rumoca_core::VarName::new("b.h_outflow"),
        flat::Variable {
            stream: true,
            source_span: test_span(),
            ..connection_test_variable(test_span())
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
        test_span(),
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
    let mut flat = connection_test_model();
    flat.add_test_variable(
        rumoca_core::VarName::new("a.h_outflow"),
        flat::Variable {
            name: rumoca_core::VarName::new("a.h_outflow"),
            stream: true,
            source_span: test_span(),
            ..connection_test_variable(test_span())
        },
    );
    flat.add_test_variable(
        rumoca_core::VarName::new("b.h_outflow"),
        flat::Variable {
            name: rumoca_core::VarName::new("b.h_outflow"),
            stream: true,
            source_span: test_span(),
            ..connection_test_variable(test_span())
        },
    );
    for name in ["a.m_flow", "b.m_flow"] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                flow: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }

    let mut overlay = ast::InstanceOverlay::new();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            connections: vec![
                ast::InstanceConnection::scalar(
                    ast::QualifiedName::from_dotted("a.h_outflow"),
                    ast::QualifiedName::from_dotted("b.h_outflow"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
                ast::InstanceConnection::scalar(
                    ast::QualifiedName::from_dotted("a.m_flow"),
                    ast::QualifiedName::from_dotted("b.m_flow"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
            ],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");

    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    process_test_connections(&mut flat, &overlay, &mut oc_forest)
        .expect("stream connection processing");

    assert_eq!(flat.equations.len(), 1, "only the flow sum is expected");
    assert!(matches!(
        flat.equations[0].origin,
        flat::EquationOrigin::FlowSum { .. }
    ));
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.h_outflow"))
            .is_some_and(|var| !var.connected.is_unconnected())
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("b.h_outflow"))
            .is_some_and(|var| !var.connected.is_unconnected())
    );
}

#[test]
fn test_connector_path_with_structural_member_expands_nonstructural_members() {
    let mut flat = connection_test_model();
    let mut overlay = ast::InstanceOverlay::new();
    for name in ["a", "b"] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                is_primitive: false,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
        pair_ordinary_component(&flat, &mut overlay, name);
        flat.add_test_variable(
            rumoca_core::VarName::new(format!("{name}.m")),
            flat::Variable {
                name: rumoca_core::VarName::new(format!("{name}.m")),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
        pair_ordinary_component(&flat, &mut overlay, &format!("{name}.m"));
        flat.add_test_variable(
            rumoca_core::VarName::new(format!("{name}.v")),
            flat::Variable {
                name: rumoca_core::VarName::new(format!("{name}.v")),
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
        pair_ordinary_component(&flat, &mut overlay, &format!("{name}.v"));
        flat.add_test_variable(
            rumoca_core::VarName::new(format!("{name}.i")),
            flat::Variable {
                name: rumoca_core::VarName::new(format!("{name}.i")),
                flow: true,
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
        pair_ordinary_component(&flat, &mut overlay, &format!("{name}.i"));
    }

    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            connections: vec![
                ast::InstanceConnection::scalar(
                    ast::QualifiedName::from_ident("a"),
                    ast::QualifiedName::from_ident("b"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
            ],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");

    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    process_test_connections(&mut flat, &overlay, &mut oc_forest)
        .expect("connector connection processing");

    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.v"))
            .is_some_and(|var| !var.connected.is_unconnected())
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.i"))
            .is_some_and(|var| !var.connected.is_unconnected())
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.m"))
            .is_none_or(|var| var.connected.is_unconnected()),
        "structural connector members must not prevent nonstructural members from connecting"
    );
}

#[test]
fn collapsed_connector_array_refuses_until_connected_subdomains_are_representable() {
    let mut flat = connection_test_model();
    for (name, dims, flow) in [
        ("source.port.T", vec![2], false),
        ("source.port.Q_flow", vec![2], true),
        ("sink.ports[1].T", vec![], false),
        ("sink.ports[1].Q_flow", vec![], true),
        ("sink.ports[2].T", vec![], false),
        ("sink.ports[2].Q_flow", vec![], true),
    ] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                dims,
                flow,
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }

    let mut overlay = ast::InstanceOverlay::new();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            connections: vec![
                ast::InstanceConnection::scalar(
                    ast::QualifiedName::from_dotted("source.port"),
                    ast::QualifiedName::from_dotted("sink.ports"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
            ],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");

    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    let error = process_test_connections(&mut flat, &overlay, &mut oc_forest)
        .expect_err("compact partial connected-state needs a structured Flat owner");

    assert!(
        matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
        "unexpected refusal: {error:?}"
    );
    assert!(flat.equations.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}

fn expandable_connector_test_overlay() -> ast::InstanceOverlay {
    let mut overlay = ast::InstanceOverlay::new();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            connections: vec![
                ast::InstanceConnection::scalar(
                    ast::QualifiedName::from_ident("a"),
                    ast::QualifiedName::from_ident("b"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
            ],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

fn add_expandable_member(flat: &mut flat::Model, name: &str) {
    flat.add_test_variable(
        rumoca_core::VarName::new(name),
        flat::Variable {
            name: rumoca_core::VarName::new(name),
            is_primitive: true,
            from_expandable_connector: true,
            source_span: test_span(),
            ..connection_test_variable(test_span())
        },
    );
}

#[test]
fn expandable_connectors_with_same_declared_members_are_supported() {
    let mut flat = connection_test_model();
    let mut overlay = ast::InstanceOverlay::new();
    add_expandable_member(&mut flat, "a.signal");
    pair_ordinary_component(&flat, &mut overlay, "a.signal");
    add_expandable_member(&mut flat, "b.signal");
    pair_ordinary_component(&flat, &mut overlay, "b.signal");
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            connections: vec![
                ast::InstanceConnection::scalar(
                    ast::QualifiedName::from_ident("a"),
                    ast::QualifiedName::from_ident("b"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
            ],
            ..Default::default()
        })
        .expect("fixture root is allocated");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();

    process_test_connections(&mut flat, &overlay, &mut oc_forest)
        .expect("identical declared members require no augmentation");

    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("a.signal"))
            .is_some_and(|var| !var.connected.is_unconnected())
    );
}

#[test]
fn expandable_connector_member_union_is_rejected_before_connection_sets() {
    let mut flat = connection_test_model();
    add_expandable_member(&mut flat, "a.left_only");
    add_expandable_member(&mut flat, "b.right_only");
    let overlay = expandable_connector_test_overlay();
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut oc_forest)
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
    let mut flat = connection_test_model();
    for name in ["a.shared", "a.left_only", "b.shared"] {
        add_expandable_member(&mut flat, name);
    }
    let overlay = expandable_connector_test_overlay();
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut oc_forest)
        .expect_err("connecting only the declared intersection is not MLS §9.1.3");

    assert!(matches!(
        error,
        FlattenError::UnsupportedExpandableConnectorAugmentation { .. }
    ));
}

/// MLS §9.1.3 sizes a virtual expandable member from the union of every
/// connection that contributes to it.  Until that augmentation exists, the
/// complete two-connect input must take the dedicated early refusal path; it
/// must never reach ordinary member classification one connect at a time and
/// fail as generic missing declaration evidence (or fabricate a rank-one
/// member from the first index).
#[test]
fn disjoint_expandable_array_indices_are_refused_before_member_classification() {
    let mut flat = connection_test_model();
    for name in ["source_one.y", "source_three.y"] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                type_id: CONNECTION_TEST_SCALAR_TYPE,
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }

    let indexed_virtual_member = |index| {
        let mut endpoint = ast::QualifiedName::from_dotted("bus.signal");
        endpoint
            .parts
            .last_mut()
            .expect("virtual member leaf exists")
            .1 = vec![index];
        endpoint
    };
    let mut overlay = ast::InstanceOverlay::new();
    let bus_type = rumoca_core::TypeId(0x52_0001);
    overlay.type_roots.insert(bus_type, bus_type);
    let bus_instance_id = overlay.alloc_id();
    overlay
        .add_component(ast::InstanceData {
            instance_id: bus_instance_id,
            qualified_name: ast::QualifiedName::from_ident("bus"),
            type_id: bus_type,
            is_expandable_connector_type: true,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            connections: vec![
                ast::InstanceConnection::scalar(
                    indexed_virtual_member(1),
                    ast::QualifiedName::from_dotted("source_one.y"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("valid first source connection"),
                ast::InstanceConnection::scalar(
                    indexed_virtual_member(3),
                    ast::QualifiedName::from_dotted("source_three.y"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("valid second source connection"),
            ],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    finalize_connection_test_flat(&mut flat);
    let before = connection_mutation_snapshot(&flat);
    let overconstrained = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
    let forest_before = forest.state_snapshot();

    let error =
        equation_generation::process_connections_for_test(&mut flat, &overconstrained, &mut forest)
            .expect_err("virtual member-array union is not implemented yet");

    assert!(matches!(
        error,
        FlattenError::UnsupportedExpandableConnectorAugmentation { .. }
    ));
    assert_eq!(
        connection_mutation_snapshot(&flat),
        before,
        "early refusal must leave Flat IR unchanged"
    );
    assert_eq!(forest.state_snapshot(), forest_before);
}

#[test]
fn nonempty_connector_connection_with_no_matched_members_is_rejected() {
    let mut flat = connection_test_model();
    for name in ["a.left", "b.right"] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }
    let overlay = expandable_connector_test_overlay();
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("a semantic connect must not disappear when member names do not match");

    assert!(matches!(error, FlattenError::IncompatibleConnectors { .. }));
    assert!(flat.equations.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}

#[test]
fn nonexpandable_connector_partial_member_intersection_is_rejected_atomically() {
    let mut flat = connection_test_model();
    for name in ["a.shared", "a.left_only", "b.shared"] {
        let name = rumoca_core::VarName::new(name);
        flat.add_test_variable(
            name.clone(),
            flat::Variable {
                name,
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }
    let overlay = expandable_connector_test_overlay();
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("a normal connector must have the same named primitive members");

    assert!(matches!(error, FlattenError::IncompatibleConnectors { .. }));
    assert!(flat.equations.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}

#[test]
fn test_is_flow_variable_subscripted_element_of_array_field() {
    let mut flat = connection_test_model();
    flat.add_test_variable(
        rumoca_core::VarName::new("arr.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..connection_test_variable(test_span())
        },
    );
    assert!(
        is_flow_variable(&flat, &rumoca_core::VarName::new("arr.n.i[2]"), test_span(),).unwrap()
    );
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
    let lhs = test_var_expr(&rumoca_core::VarName::new("a"), span);
    let rhs = test_var_expr(&rumoca_core::VarName::new("b"), span);
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
        test_var_expr(&rumoca_core::VarName::new("a"), span),
        test_var_expr(&rumoca_core::VarName::new("b"), span),
        test_var_expr(&rumoca_core::VarName::new("c"), span),
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
    let mut flat = connection_test_model();
    let mut overlay = ast::InstanceOverlay::new();

    // Add variables
    flat.add_test_variable(
        rumoca_core::VarName::new("r1.n.v"),
        connection_test_variable(test_span()),
    );
    pair_ordinary_component(&flat, &mut overlay, "r1.n.v");
    flat.add_test_variable(
        rumoca_core::VarName::new("r2.p.v"),
        connection_test_variable(test_span()),
    );
    pair_ordinary_component(&flat, &mut overlay, "r2.p.v");
    flat.add_test_variable(
        rumoca_core::VarName::new("r3.p.v"),
        connection_test_variable(test_span()),
    );
    pair_ordinary_component(&flat, &mut overlay, "r3.p.v");

    let vars = vec![
        rumoca_core::VarName::new("r1.n.v"),
        rumoca_core::VarName::new("r2.p.v"),
        rumoca_core::VarName::new("r3.p.v"),
    ];

    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    finalize_connection_test_flat(&mut flat);
    let catalog = crate::test_support::finalized_test_overlay(&mut overlay);
    equation_generation::generate_equality_equations(
        &mut flat,
        &catalog,
        &vars,
        test_span(),
        &mut oc_forest,
    )
    .unwrap();

    // Should generate 2 equations (n-1 for n=3)
    assert_eq!(flat.equations.len(), 2);

    // All variables should be marked as connected
    assert!(
        !flat
            .variables
            .get(&rumoca_core::VarName::new("r1.n.v"))
            .unwrap()
            .connected
            .is_unconnected()
    );
    assert!(
        !flat
            .variables
            .get(&rumoca_core::VarName::new("r2.p.v"))
            .unwrap()
            .connected
            .is_unconnected()
    );
    assert!(
        !flat
            .variables
            .get(&rumoca_core::VarName::new("r3.p.v"))
            .unwrap()
            .connected
            .is_unconnected()
    );
}

#[test]
fn test_generate_flow_equation() {
    let mut flat = connection_test_model();

    // Add flow variables
    let v1 = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("r1.n.i"), v1);

    let v2 = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("r2.p.i"), v2);

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
        !flat
            .variables
            .get(&rumoca_core::VarName::new("r1.n.i"))
            .unwrap()
            .connected
            .is_unconnected()
    );
    assert!(
        !flat
            .variables
            .get(&rumoca_core::VarName::new("r2.p.i"))
            .unwrap()
            .connected
            .is_unconnected()
    );
}

fn selected_singleton_connection(
    selected_member: (&str, &str),
    inside_member: &str,
) -> ast::InstanceScalarConnection {
    let (selected_base, selected_leaf) = selected_member;
    ast::InstanceScalarConnection::new(
        ast::QualifiedName {
            parts: vec![
                (selected_base.to_string(), Vec::new()),
                (selected_leaf.to_string(), vec![1]),
            ],
        },
        ast::QualifiedName::from_dotted(inside_member),
        None,
        test_span(),
        String::new(),
    )
    .expect("selected singleton fixture is a valid scalar connection")
}

fn root_interface_connector_map() -> InterfaceConnectorRootsByScope {
    let mut result = IndexMap::default();
    result.insert(
        String::new(),
        indexmap::IndexSet::from([rumoca_core::ComponentPath::from_parts(["port"])]),
    );
    result
}

#[test]
fn selected_singleton_interface_flow_keeps_outside_sign() {
    let mut flat = connection_test_model();
    for (name, dims) in [("port.f", vec![1]), ("inside.f", Vec::new())] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                dims,
                flow: true,
                is_primitive: true,
                ..connection_test_variable(test_span())
            },
        );
    }
    let connection = selected_singleton_connection(("port", "f"), "inside.f");
    let connections = [&connection];
    let prefix_children = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let interface = collect_interface_flow_vars_by_scope(
        &connections,
        &flat,
        &prefix_children,
        &var_index,
        &root_interface_connector_map(),
    )
    .expect("selected compact flow member has required declaration evidence");
    let selected = rumoca_core::VarName::new("port.f[1]");
    assert!(interface[""].contains(&selected));

    generate_flow_equation(
        &mut flat,
        &[selected, rumoca_core::VarName::new("inside.f")],
        "",
        &interface,
        test_span(),
    )
    .expect("singleton selection denotes a scalar flow member");

    assert_eq!(flat.equations.len(), 1);
    let flat::EquationOrigin::FlowSum { description } = &flat.equations[0].origin else {
        panic!("the selected interface flow connection must own a flow sum");
    };
    assert_eq!(description, "-port.f[1] + inside.f = 0");
}

#[test]
fn selected_singleton_interface_stream_generates_outside_equation() {
    let mut flat = connection_test_model();
    for (name, dims, stream, flow) in [
        ("port.h", vec![1], true, false),
        ("inside.h", Vec::new(), true, false),
        ("port.m_flow", vec![1], false, true),
        ("inside.m_flow", Vec::new(), false, true),
    ] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                dims,
                stream,
                flow,
                is_primitive: true,
                ..connection_test_variable(test_span())
            },
        );
    }
    let connection = selected_singleton_connection(("port", "h"), "inside.h");
    let connections = [&connection];
    let prefix_children = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let outside = collect_interface_stream_endpoints_by_scope(
        &connections,
        &flat,
        &prefix_children,
        &var_index,
        &root_interface_connector_map(),
    )
    .expect("selected compact stream member has required declaration evidence");
    let selected = rumoca_core::VarName::new("port.h[1]");
    assert!(outside[""].contains_key(&selected));

    let stream_sets = [StreamConnectionSet {
        variables: vec![selected.clone(), rumoca_core::VarName::new("inside.h")],
        scope: String::new(),
        span: Span::DUMMY,
    }];
    let endpoints =
        super::stream_operators::build_stream_connection_endpoints(&flat, &stream_sets, &outside)
            .expect("selected stream endpoint resolves its associated compact flow");
    generate_outside_stream_equations(&mut flat, &outside, &endpoints)
        .expect("outside selected stream member constructs its connection equation");

    assert_eq!(flat.equations.len(), 1);
    let flat::EquationOrigin::OutsideStream { variable } = &flat.equations[0].origin else {
        panic!("the selected stream connection must own an outside-stream equation");
    };
    assert_eq!(variable, "port.h[1]");
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("port.h"))
            .is_some_and(|variable| !variable.connected.is_unconnected())
    );
}

#[test]
fn test_generate_flow_equation_marks_only_the_selected_element_of_a_subscripted_var() {
    let mut flat = connection_test_model();
    flat.add_test_variable(
        rumoca_core::VarName::new("a.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..connection_test_variable(test_span())
        },
    );
    flat.add_test_variable(
        rumoca_core::VarName::new("b.n.i"),
        flat::Variable {
            flow: true,
            ..connection_test_variable(test_span())
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
    .expect("one element of a compact flow array has a checked domain owner");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 1);
    let base = &flat.variables[&rumoca_core::VarName::new("a.n.i")].connected;
    assert_eq!(base.selections().collect::<Vec<_>>(), vec![&[2][..]]);
    assert_eq!(
        base.unconnected_coordinates(&[4]),
        Ok(vec![vec![1], vec![3], vec![4]])
    );
    assert_eq!(
        flat.variables[&rumoca_core::VarName::new("b.n.i")]
            .connected
            .coverage(&[]),
        Ok(flat::ConnectedCoverage::Whole)
    );
}

#[test]
fn test_generate_flow_equation_subscripted_unknown_dims_is_refused() {
    let mut flat = connection_test_model();
    flat.add_test_variable(
        rumoca_core::VarName::new("a.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![],
            ..connection_test_variable(test_span())
        },
    );
    flat.add_test_variable(
        rumoca_core::VarName::new("b.n.i"),
        flat::Variable {
            flow: true,
            ..connection_test_variable(test_span())
        },
    );

    let vars = vec![
        rumoca_core::VarName::new("a.n.i[2]"),
        rumoca_core::VarName::new("b.n.i"),
    ];
    let error = generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, indexmap::IndexSet<rumoca_core::VarName>>::default(),
        test_span(),
    )
    .expect_err("missing declaration rank cannot prove a scalar element selection");

    assert!(matches!(
        error,
        FlattenError::InvalidConnectionEvidence { .. }
    ));
    assert!(flat.equations.is_empty());
}

#[test]
fn connection_freeze_mixed_scalar_and_array_flow_is_refused_atomically() {
    let mut flat = connection_test_model();
    flat.add_test_variable(
        rumoca_core::VarName::new("arr.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![2],
            ..connection_test_variable(test_span())
        },
    );
    flat.add_test_variable(
        rumoca_core::VarName::new("s.n.i"),
        flat::Variable {
            flow: true,
            ..connection_test_variable(test_span())
        },
    );

    let vars = vec![
        rumoca_core::VarName::new("arr.n.i"),
        rumoca_core::VarName::new("s.n.i"),
    ];
    let error = generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, indexmap::IndexSet<rumoca_core::VarName>>::default(),
        test_span(),
    )
    .expect_err("mixed scalar and array flow shapes cannot construct a Flat residual");

    assert!(matches!(error, FlattenError::IncompatibleConnectors { .. }));
    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}

#[test]
fn connection_freeze_two_arrays_and_scalar_flow_is_refused_atomically() {
    let mut flat = connection_test_model();
    flat.add_test_variable(
        rumoca_core::VarName::new("arr1.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![2],
            ..connection_test_variable(test_span())
        },
    );
    flat.add_test_variable(
        rumoca_core::VarName::new("arr2.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![2],
            ..connection_test_variable(test_span())
        },
    );
    flat.add_test_variable(
        rumoca_core::VarName::new("s.n.i"),
        flat::Variable {
            flow: true,
            ..connection_test_variable(test_span())
        },
    );

    let vars = vec![
        rumoca_core::VarName::new("arr1.n.i"),
        rumoca_core::VarName::new("arr2.n.i"),
        rumoca_core::VarName::new("s.n.i"),
    ];
    let error = generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, indexmap::IndexSet<rumoca_core::VarName>>::default(),
        test_span(),
    )
    .expect_err("a scalar cannot enter an array-valued flow-sum owner");

    assert!(matches!(error, FlattenError::IncompatibleConnectors { .. }));
    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}

#[test]
fn test_generate_flow_equation_sign_convention() {
    let mut flat = connection_test_model();

    // Add inside connector (3 parts: component.connector.variable)
    let v_inside = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("r.p.i"), v_inside);

    // Add outside connector (2 parts: connector.variable)
    let v_outside = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("p.i"), v_outside);

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
    let mut flat = connection_test_model();

    let outside_nested = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("port.frame.f"), outside_nested);

    let inside = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("comp.port.f"), inside);

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
    let mut flat = connection_test_model();

    let outside_scalarized = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(
        rumoca_core::VarName::new("cell.plug.pin[1].i"),
        outside_scalarized,
    );

    let inside = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("cell.diode.p.i"), inside);

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
    let mut flat = connection_test_model();
    for name in ["cell.plug.pin[1].i", "cell.diode.p.i"] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                flow: true,
                dims: Vec::new(),
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }

    let mut overlay = ast::InstanceOverlay::new();
    let connector_type = rumoca_core::TypeId(0x52_0002);
    overlay.type_roots.insert(connector_type, connector_type);
    let connector_instance_id = overlay.alloc_id();
    overlay
        .add_component(ast::InstanceData {
            instance_id: connector_instance_id,
            qualified_name: ast::QualifiedName::from_dotted("cell.plug"),
            type_id: connector_type,
            is_connector_type: true,
            is_protected: false,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_dotted("cell"),
            connections: vec![
                ast::InstanceConnection::scalar(
                    {
                        let mut endpoint = ast::QualifiedName::from_dotted("cell.plug.pin");
                        endpoint.parts.last_mut().expect("pin leaf exists").1 = vec![1];
                        endpoint
                    },
                    ast::QualifiedName::from_dotted("cell.diode.p"),
                    None,
                    test_span(),
                    "cell".to_string(),
                )
                .expect("test scalar connection is valid"),
            ],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");

    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut oc_forest = crate::vcg::OverconstrainedEquationForest::empty();
    process_test_connections(&mut flat, &overlay, &mut oc_forest)
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
    let mut flat = connection_test_model();
    flat.add_test_variable(
        rumoca_core::VarName::new("cell.p.i"),
        flat::Variable {
            flow: true,
            ..connection_test_variable(test_span())
        },
    );
    flat.add_test_variable(
        rumoca_core::VarName::new("cell.multiSensor.pc.i"),
        flat::Variable {
            flow: true,
            ..connection_test_variable(test_span())
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
    let mut flat = connection_test_model();

    // Both flow
    let v1 = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a.i"), v1);

    let v2 = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b.i"), v2);

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
    let mut flat = connection_test_model();

    // One flow, one non-flow
    let v1 = flat::Variable {
        flow: true,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a.i"), v1);

    let v2 = flat::Variable {
        flow: false,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b.v"), v2);

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
    let mut flat = connection_test_model();

    // Same dimensions
    let v1 = flat::Variable {
        dims: vec![3],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        dims: vec![3],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b"), v2);

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
    let mut flat = connection_test_model();

    // Different dimensions
    let v1 = flat::Variable {
        dims: vec![3],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        dims: vec![5],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b"), v2);

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
    let mut flat = connection_test_model();

    let v1 = flat::Variable {
        dims: vec![2],
        causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("u"), v1);

    let v2 = flat::Variable {
        dims: vec![3],
        causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("y"), v2);

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
    let mut flat = connection_test_model();

    let lhs = flat::Variable {
        dims: vec![2, 3],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a"), lhs);

    let rhs = flat::Variable {
        dims: vec![3],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b"), rhs);

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
    let mut flat = connection_test_model();

    let lhs = flat::Variable {
        dims: vec![2, 3],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a"), lhs);

    let rhs = flat::Variable {
        dims: vec![4],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b"), rhs);

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
fn test_split_trailing_index_groups_multi_index() {
    let (base, groups) =
        split_trailing_index_groups("connector.field[2][3]").expect("should parse");
    assert_eq!(base, "connector.field");
    assert_eq!(groups, vec!["[2]".to_string(), "[3]".to_string()]);
}

#[test]
fn test_validate_type_compatibility_ok() {
    let mut flat = connection_test_model();
    flat.effective_types.insert(
        CONNECTION_TEST_SCALAR_TYPE,
        rumoca_core::EffectiveType::new(
            CONNECTION_TEST_SCALAR_TYPE,
            CONNECTION_TEST_SCALAR_TYPE,
            Vec::<i64>::new(),
        )
        .unwrap(),
    );

    // Both same type (type_id = 1 for both)
    let v1 = flat::Variable {
        type_id: CONNECTION_TEST_SCALAR_TYPE, // Same type
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: CONNECTION_TEST_SCALAR_TYPE, // Same type
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b"), v2);

    // Should succeed
    let result = validate_type_compatibility(
        &flat,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn test_validate_type_compatibility_mismatch() {
    let mut flat = connection_test_model();
    for type_id in [CONNECTION_TEST_SCALAR_TYPE, TypeId(2)] {
        flat.effective_types.insert(
            type_id,
            rumoca_core::EffectiveType::new(type_id, type_id, Vec::<i64>::new()).unwrap(),
        );
    }

    // Different types (type_id = 1 vs 2)
    let v1 = flat::Variable {
        type_id: CONNECTION_TEST_SCALAR_TYPE, // e.g., Real
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: TypeId(2), // e.g., Integer
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b"), v2);

    // Should fail
    let result = validate_type_compatibility(
        &flat,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_err());
}

#[test]
fn test_validate_type_compatibility_unknown_is_invalid_evidence() {
    let mut flat = connection_test_model();
    flat.effective_types.insert(
        CONNECTION_TEST_SCALAR_TYPE,
        rumoca_core::EffectiveType::new(
            CONNECTION_TEST_SCALAR_TYPE,
            CONNECTION_TEST_SCALAR_TYPE,
            Vec::<i64>::new(),
        )
        .unwrap(),
    );

    let v1 = flat::Variable {
        type_id: TypeId::UNKNOWN,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: CONNECTION_TEST_SCALAR_TYPE,
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b"), v2);

    let result = validate_type_compatibility(
        &flat,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(matches!(
        result,
        Err(FlattenError::InvalidConnectionEvidence { .. })
    ));
}

#[test]
fn connection_freeze_late_shaped_alias_uses_finalized_type_catalog() {
    let mut flat = connection_test_model();

    let alias = TypeId(9);
    let root = CONNECTION_TEST_SCALAR_TYPE;
    flat.type_roots.insert(alias, root);
    flat.type_roots.insert(root, root);
    flat.effective_types.insert(
        alias,
        rumoca_core::EffectiveType::new(alias, root, Vec::<i64>::new()).unwrap(),
    );
    flat.effective_types.insert(
        root,
        rumoca_core::EffectiveType::new(root, root, Vec::<i64>::new()).unwrap(),
    );

    let v1 = flat::Variable {
        type_id: alias,
        dims: vec![2],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: root,
        dims: vec![2],
        ..connection_test_variable(test_span())
    };
    flat.add_test_variable(rumoca_core::VarName::new("b"), v2);

    flat.finalize_effective_type_shapes()
        .expect("late occurrence shapes intern effective identities");
    let result = validate_type_compatibility(
        &flat,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn connection_freeze_missing_effective_type_catalog_is_refused() {
    let mut flat = connection_test_model();
    for name in ["a", "b"] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                type_id: TypeId(7),
                ..connection_test_variable(test_span())
            },
        );
    }

    let error = validate_type_compatibility(
        &flat,
        &rumoca_core::VarName::new("a"),
        &rumoca_core::VarName::new("b"),
        test_span(),
    )
    .expect_err("a non-UNKNOWN identity is not evidence without its Flat descriptor");

    assert!(matches!(
        error,
        FlattenError::InvalidConnectionEvidence { .. }
    ));
    assert!(error.to_string().contains("effective-type catalog"));
}

mod array_expanded_tests;
mod member_pairing_tests;
mod path_matching_tests;
