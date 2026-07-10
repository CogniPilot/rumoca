// SPEC_0021 file-size exception: split plan is to move focused connection
// fixtures into owned test modules after BOPTEST parity stabilization.
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

fn test_provenance_span() -> rumoca_core::ProvenanceSpan {
    rumoca_core::ProvenanceSpan::new(test_span(), "phase flatten connection test")
        .expect("test span has source context")
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
    );

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

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("a.h_outflow"),
            b: ast::QualifiedName::from_dotted("b.h_outflow"),
            connector_type: None,
            span: Span::DUMMY,
            scope: String::new(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false).expect("stream connection processing");

    assert!(
        flat.equations.is_empty(),
        "stream connect() must not become an ordinary equality equation"
    );
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
fn test_stream_interface_alias_generated_when_anchor_is_defined() {
    let mut flat = flat::Model::new();
    for name in ["port.h_outflow", "component.port.h_outflow"] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                stream: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    flat.add_equation(flat::Equation::new(
        create_equality_residual(
            var_to_expr(
                &rumoca_core::VarName::new("component.port.h_outflow"),
                test_provenance_span(),
            ),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                span: Span::DUMMY,
            },
            test_provenance_span(),
        ),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "component.port".to_string(),
        },
    ));

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_ident("port"),
            b: ast::QualifiedName::from_dotted("component.port"),
            connector_type: None,
            span: Span::DUMMY,
            scope: String::new(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false).expect("stream alias processing");

    assert_eq!(flat.equations.len(), 2);
    assert!(matches!(
        flat.equations[1].origin,
        flat::EquationOrigin::Connection { .. }
    ));
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("port.h_outflow"))
            .is_some_and(|var| var.connected)
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("component.port.h_outflow"))
            .is_some_and(|var| var.connected)
    );
}

#[test]
fn test_stream_interface_alias_expands_collapsed_stream_endpoint() {
    let mut flat = flat::Model::new();
    for name in ["port.h_outflow", "component.port.h_outflow"] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                stream: true,
                is_primitive: true,
                dims: vec![2],
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    flat.add_equation(flat::Equation::new(
        create_equality_residual(
            var_to_expr(
                &rumoca_core::VarName::new("component.port.h_outflow"),
                test_provenance_span(),
            ),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                span: Span::DUMMY,
            },
            test_provenance_span(),
        ),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "component.port".to_string(),
        },
    ));

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("port.h_outflow[2]"),
            b: ast::QualifiedName::from_dotted("component.port.h_outflow[2]"),
            connector_type: None,
            span: Span::DUMMY,
            scope: String::new(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false)
        .expect("collapsed stream endpoint alias processing");

    assert_eq!(flat.equations.len(), 2);
    assert!(matches!(
        flat.equations[1].origin,
        flat::EquationOrigin::Connection { .. }
    ));
}

#[test]
fn test_stream_non_interface_alias_generated_for_explicit_connector_port() {
    let mut flat = flat::Model::new();
    for name in [
        "wrapper.sensor.port.h_outflow",
        "wrapper.source.port.h_outflow",
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                stream: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    flat.add_equation(flat::Equation::new(
        create_equality_residual(
            var_to_expr(
                &rumoca_core::VarName::new("wrapper.source.port.h_outflow"),
                test_provenance_span(),
            ),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                span: Span::DUMMY,
            },
            test_provenance_span(),
        ),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "wrapper.source.port".to_string(),
        },
    ));

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("wrapper.sensor.port"),
            b: ast::QualifiedName::from_dotted("wrapper.source.port"),
            connector_type: None,
            span: Span::DUMMY,
            scope: "wrapper".to_string(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false).expect("non-interface stream alias processing");

    assert_eq!(flat.equations.len(), 2);
    assert!(matches!(
        flat.equations[1].origin,
        flat::EquationOrigin::Connection { .. }
    ));
}

#[test]
fn test_outside_stream_connectors_are_counted_without_equality_aliases() {
    let mut flat = flat::Model::new();
    for name in ["port_a.h_outflow", "port_b.h_outflow"] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                stream: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("port_a"),
            b: ast::QualifiedName::from_dotted("port_b"),
            connector_type: None,
            span: Span::DUMMY,
            scope: String::new(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false).expect("outside stream connector processing");

    assert_eq!(flat.equations.len(), 0);
    assert_eq!(flat.stream_interface_equation_count, 2);
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("port_a.h_outflow"))
            .is_some_and(|var| var.connected)
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("port_b.h_outflow"))
            .is_some_and(|var| var.connected)
    );
}

#[test]
fn test_internal_stream_connectors_without_anchor_are_counted() {
    let mut flat = flat::Model::new();
    for name in [
        "wrapper.a.port.p",
        "wrapper.a.port.m_flow",
        "wrapper.a.port.h_outflow",
        "wrapper.b.port.p",
        "wrapper.b.port.m_flow",
        "wrapper.b.port.h_outflow",
    ] {
        let mut variable = flat::Variable {
            name: rumoca_core::VarName::new(name),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        };
        if name.ends_with(".m_flow") {
            variable.flow = true;
        }
        if name.ends_with(".h_outflow") {
            variable.stream = true;
        }
        flat.add_variable(rumoca_core::VarName::new(name), variable);
    }

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("wrapper.a.port"),
            b: ast::QualifiedName::from_dotted("wrapper.b.port"),
            connector_type: None,
            span: Span::DUMMY,
            scope: "wrapper".to_string(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false).expect("internal stream connector processing");

    assert_eq!(flat.stream_interface_equation_count, 2);
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("wrapper.a.port.h_outflow"))
            .is_some_and(|var| var.connected)
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("wrapper.b.port.h_outflow"))
            .is_some_and(|var| var.connected)
    );
}

#[test]
fn test_multiway_stream_connectors_with_component_defined_outflow_are_counted() {
    let mut flat = flat::Model::new();
    for name in ["a.h_outflow", "b.h_outflow", "c.h_outflow"] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                stream: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        flat.add_equation(flat::Equation::new(
            create_equality_residual(
                var_to_expr(&var_name, test_provenance_span()),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: test_span(),
                },
                test_provenance_span(),
            ),
            test_span(),
            flat::EquationOrigin::ComponentEquation {
                component: name.to_string(),
            },
        ));
    }

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("a.h_outflow"),
                b: ast::QualifiedName::from_dotted("b.h_outflow"),
                connector_type: None,
                span: Span::DUMMY,
                scope: String::new(),
            },
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("a.h_outflow"),
                b: ast::QualifiedName::from_dotted("c.h_outflow"),
                connector_type: None,
                span: Span::DUMMY,
                scope: String::new(),
            },
        ],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false).expect("multiway stream connector processing");

    assert_eq!(flat.stream_interface_equation_count, 3);
    for name in ["a.h_outflow", "b.h_outflow", "c.h_outflow"] {
        assert!(
            flat.variables
                .get(&rumoca_core::VarName::new(name))
                .is_some_and(|var| var.connected),
            "{name} should be marked connected"
        );
    }
}

#[test]
fn test_stream_internal_dynbal_port_alias_is_not_generated() {
    let mut flat = flat::Model::new();
    for name in [
        "wrapper.vol.dynBal.ports[1].Xi_outflow",
        "wrapper.source.port.Xi_outflow",
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                stream: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    flat.add_equation(flat::Equation::new(
        create_equality_residual(
            var_to_expr(
                &rumoca_core::VarName::new("wrapper.source.port.Xi_outflow"),
                test_provenance_span(),
            ),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                span: Span::DUMMY,
            },
            test_provenance_span(),
        ),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "wrapper.source.port".to_string(),
        },
    ));

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("wrapper.vol.dynBal.ports[1]"),
            b: ast::QualifiedName::from_dotted("wrapper.source.port"),
            connector_type: None,
            span: Span::DUMMY,
            scope: "wrapper.vol.dynBal".to_string(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false)
        .expect("internal dynBal stream alias processing");

    assert_eq!(flat.equations.len(), 1);
}

#[test]
fn test_stream_internal_dynbal_energy_port_alias_is_generated() {
    let mut flat = flat::Model::new();
    for name in [
        "wrapper.vol.dynBal.ports[1].h_outflow",
        "wrapper.source.port.h_outflow",
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                stream: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    flat.add_equation(flat::Equation::new(
        create_equality_residual(
            var_to_expr(
                &rumoca_core::VarName::new("wrapper.source.port.h_outflow"),
                test_provenance_span(),
            ),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                span: Span::DUMMY,
            },
            test_provenance_span(),
        ),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "wrapper.source.port".to_string(),
        },
    ));

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("wrapper.vol.dynBal.ports[1]"),
            b: ast::QualifiedName::from_dotted("wrapper.source.port"),
            connector_type: None,
            span: Span::DUMMY,
            scope: "wrapper.vol.dynBal".to_string(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false)
        .expect("internal dynBal energy stream alias processing");

    assert_eq!(flat.equations.len(), 2);
    assert!(matches!(
        flat.equations[1].origin,
        flat::EquationOrigin::Connection { .. }
    ));
}

#[test]
fn test_stream_pass_through_alias_generated_from_defined_dynbal_port() {
    let mut flat = flat::Model::new();
    for name in [
        "wrapper.vol.dynBal.ports[1].h_outflow",
        "wrapper.vol.ports[1].h_outflow",
        "wrapper.port_b.h_outflow",
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                stream: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    flat.add_equation(flat::Equation::new(
        create_equality_residual(
            var_to_expr(
                &rumoca_core::VarName::new("wrapper.vol.dynBal.ports[1].h_outflow"),
                test_provenance_span(),
            ),
            var_to_expr(
                &rumoca_core::VarName::new("wrapper.vol.dynBal.medium.h"),
                test_provenance_span(),
            ),
            test_provenance_span(),
        ),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "wrapper.vol.dynBal".to_string(),
        },
    ));

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("wrapper.vol.ports"),
                b: ast::QualifiedName::from_dotted("wrapper.vol.dynBal.ports"),
                connector_type: None,
                span: Span::DUMMY,
                scope: "wrapper.vol".to_string(),
            },
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("wrapper.vol.ports[1]"),
                b: ast::QualifiedName::from_dotted("wrapper.port_b"),
                connector_type: None,
                span: Span::DUMMY,
                scope: "wrapper".to_string(),
            },
        ],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false).expect("stream pass-through alias processing");

    let origins = flat
        .equations
        .iter()
        .map(|eq| eq.origin.to_string())
        .collect::<Vec<_>>();
    assert!(
        origins
            .iter()
            .any(|origin| origin.contains("wrapper.vol.ports[1].h_outflow")),
        "vol.ports stream alias should be generated from dynBal anchor: {origins:?}"
    );
    assert!(
        origins
            .iter()
            .any(|origin| origin.contains("wrapper.port_b.h_outflow")),
        "outer port_b stream alias should be generated from dynBal anchor: {origins:?}"
    );
}

#[test]
fn test_stream_pass_through_alias_generated_for_all_array_ports() {
    let mut flat = flat::Model::new();
    for name in [
        "wrapper.vol.dynBal.ports[1].h_outflow",
        "wrapper.vol.dynBal.ports[2].h_outflow",
        "wrapper.vol.ports[1].h_outflow",
        "wrapper.vol.ports[2].h_outflow",
        "wrapper.port_a.h_outflow",
        "wrapper.port_b.h_outflow",
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                stream: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    for idx in 1..=2 {
        flat.add_equation(flat::Equation::new(
            create_equality_residual(
                var_to_expr(
                    &rumoca_core::VarName::new(format!(
                        "wrapper.vol.dynBal.ports[{idx}].h_outflow"
                    )),
                    test_provenance_span(),
                ),
                var_to_expr(
                    &rumoca_core::VarName::new("wrapper.vol.dynBal.medium.h"),
                    test_provenance_span(),
                ),
                test_provenance_span(),
            ),
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "wrapper.vol.dynBal".to_string(),
            },
        ));
    }

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("wrapper.vol.ports"),
                b: ast::QualifiedName::from_dotted("wrapper.vol.dynBal.ports"),
                connector_type: None,
                span: Span::DUMMY,
                scope: "wrapper.vol".to_string(),
            },
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("wrapper.vol.ports[1]"),
                b: ast::QualifiedName::from_dotted("wrapper.port_a"),
                connector_type: None,
                span: Span::DUMMY,
                scope: "wrapper".to_string(),
            },
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("wrapper.vol.ports[2]"),
                b: ast::QualifiedName::from_dotted("wrapper.port_b"),
                connector_type: None,
                span: Span::DUMMY,
                scope: "wrapper".to_string(),
            },
        ],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false)
        .expect("array stream pass-through alias processing");

    let origins = flat
        .equations
        .iter()
        .map(|eq| eq.origin.to_string())
        .collect::<Vec<_>>();
    for expected in [
        "wrapper.vol.ports[1].h_outflow",
        "wrapper.vol.ports[2].h_outflow",
        "wrapper.port_a.h_outflow",
        "wrapper.port_b.h_outflow",
    ] {
        assert!(
            origins.iter().any(|origin| origin.contains(expected)),
            "stream alias should be generated for {expected}: {origins:?}"
        );
    }
}

#[test]
fn test_stream_pass_through_alias_survives_top_level_boundary_port() {
    let mut flat = flat::Model::new();
    flat.top_level_connectors.insert("port_b".to_string());
    for name in [
        "wrapper.vol.dynBal.ports[1].h_outflow",
        "wrapper.vol.ports[1].h_outflow",
        "wrapper.port_b.h_outflow",
        "port_b.h_outflow",
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                stream: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    flat.add_equation(flat::Equation::new(
        create_equality_residual(
            var_to_expr(
                &rumoca_core::VarName::new("wrapper.vol.dynBal.ports[1].h_outflow"),
                test_provenance_span(),
            ),
            var_to_expr(
                &rumoca_core::VarName::new("wrapper.vol.dynBal.medium.h"),
                test_provenance_span(),
            ),
            test_provenance_span(),
        ),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "wrapper.vol.dynBal".to_string(),
        },
    ));

    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("wrapper.vol.ports"),
                b: ast::QualifiedName::from_dotted("wrapper.vol.dynBal.ports"),
                connector_type: None,
                span: Span::DUMMY,
                scope: "wrapper.vol".to_string(),
            },
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("wrapper.vol.ports[1]"),
                b: ast::QualifiedName::from_dotted("wrapper.port_b"),
                connector_type: None,
                span: Span::DUMMY,
                scope: "wrapper".to_string(),
            },
            ast::InstanceConnection {
                a: ast::QualifiedName::from_dotted("wrapper.port_b"),
                b: ast::QualifiedName::from_dotted("port_b"),
                connector_type: None,
                span: Span::DUMMY,
                scope: String::new(),
            },
        ],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false)
        .expect("top-level boundary stream pass-through alias processing");

    let origins = flat
        .equations
        .iter()
        .map(|eq| eq.origin.to_string())
        .collect::<Vec<_>>();
    assert!(
        origins
            .iter()
            .any(|origin| origin.contains("wrapper.vol.ports[1].h_outflow")),
        "internal vol.ports stream alias should not be suppressed by top-level boundary: {origins:?}"
    );
    assert!(
        origins
            .iter()
            .any(|origin| origin.contains("wrapper.port_b.h_outflow")),
        "internal component port stream alias should not be suppressed by top-level boundary: {origins:?}"
    );
    assert!(
        origins
            .iter()
            .all(|origin| !origin.contains("connection equation: port_b.h_outflow")),
        "top-level stream boundary must not be converted into an equality alias: {origins:?}"
    );
    assert!(
        flat.variables
            .get(&rumoca_core::VarName::new("port_b.h_outflow"))
            .is_some_and(|var| var.connected),
        "top-level stream boundary should still be marked connected"
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
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_ident("Root"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_ident("a"),
            b: ast::QualifiedName::from_ident("b"),
            connector_type: None,
            span: Span::DUMMY,
            scope: String::new(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false).expect("connector connection processing");

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

    generate_equality_equations(&mut flat, &vars, test_span()).unwrap();

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
        instance_id: ast::InstanceId(1),
        qualified_name: ast::QualifiedName::from_dotted("cell.plug"),
        is_connector_type: true,
        is_protected: false,
        ..Default::default()
    });
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId(0),
        qualified_name: ast::QualifiedName::from_dotted("cell"),
        connections: vec![ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted("cell.plug.pin"),
            b: ast::QualifiedName::from_dotted("cell.diode.p"),
            connector_type: None,
            span: test_span(),
            scope: "cell".to_string(),
        }],
        ..Default::default()
    });

    process_connections(&mut flat, &overlay, false).expect("nested connector connection");

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
fn test_validate_dimension_compatibility_scalar_accepts_length_one_array() {
    let mut flat = flat::Model::new();

    flat.add_variable(
        rumoca_core::VarName::new("y"),
        flat::Variable::empty_with_span(test_span()),
    );
    flat.add_variable(
        rumoca_core::VarName::new("u"),
        flat::Variable {
            dims: vec![1],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("y"),
        &rumoca_core::VarName::new("u"),
        Span::DUMMY,
    );
    assert!(
        result.is_ok(),
        "scalar connector and length-one connector array both have one connection element"
    );
}

#[test]
fn test_validate_dimension_compatibility_accepts_full_reduced_composition_pair() {
    let mut flat = flat::Model::new();

    flat.add_variable(
        rumoca_core::VarName::new("src.X_in_internal"),
        flat::Variable {
            dims: vec![2],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("src.Xi_in_internal"),
        flat::Variable {
            dims: vec![1],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("src.X_in_internal"),
        &rumoca_core::VarName::new("src.Xi_in_internal"),
        Span::DUMMY,
    );
    assert!(
        result.is_ok(),
        "Modelica.Media full composition X[nX] may connect to reduced Xi[nX-1] through X[1:nXi]"
    );
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
fn test_validate_dimension_compatibility_embedded_subscript_projects_exact_vars() {
    let mut flat = flat::Model::new();

    let lhs = flat::Variable {
        dims: vec![6],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("a.pin[1].v"), lhs);

    let rhs = flat::Variable {
        dims: vec![2],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("b.pin[1].v"), rhs);

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("a.pin[1].v"),
        &rumoca_core::VarName::new("b.pin[1].v"),
        Span::DUMMY,
    );
    assert!(
        result.is_ok(),
        "indexed connector members are scalar element connections even when flat vars retain parent dims"
    );
}

#[test]
fn test_validate_dimension_compatibility_embedded_subscript_preserves_parent_dims() {
    let mut flat = flat::Model::new();

    let lhs = flat::Variable {
        dims: vec![6],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("harmonic.sin1.y"), lhs);

    let rhs = flat::Variable {
        dims: vec![6, 2],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("harmonic.product1.u[1]"), rhs);

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("harmonic.sin1.y"),
        &rumoca_core::VarName::new("harmonic.product1.u[1]"),
        Span::DUMMY,
    );
    assert!(
        result.is_ok(),
        "embedded subscripts consume child dimensions while preserving component-array parent dimensions"
    );
}

#[test]
fn test_validate_dimension_compatibility_indexed_child_can_use_counterpart_parent_dims() {
    let mut flat = flat::Model::new();

    let lhs = flat::Variable {
        dims: vec![6],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("harmonic.sin1.y"), lhs);

    let rhs = flat::Variable {
        dims: vec![2],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("harmonic.product1.u[1]"), rhs);

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("harmonic.sin1.y"),
        &rumoca_core::VarName::new("harmonic.product1.u[1]"),
        Span::DUMMY,
    );
    assert!(
        result.is_ok(),
        "collapsed indexed children may be missing component-array parent dims carried by the counterpart"
    );
}

#[test]
fn test_validate_dimension_compatibility_accepts_collapsed_pin_member_subsystem_dims() {
    let mut flat = flat::Model::new();

    let lhs = flat::Variable {
        dims: vec![6],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("multi.plug_p.pin.v"), lhs);

    let rhs = flat::Variable {
        dims: vec![2],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("multi.starpoints.pin.v"), rhs);

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("multi.plug_p.pin.v"),
        &rumoca_core::VarName::new("multi.starpoints.pin.v"),
        Span::DUMMY,
    );
    assert!(
        result.is_ok(),
        "collapsed pin member validation must not reject loop-expanded scalar subsystem connections"
    );
}

#[test]
fn test_validate_dimension_compatibility_accepts_collapsed_pin_member_to_scalar_pin() {
    let mut flat = flat::Model::new();

    let lhs = flat::Variable {
        dims: vec![2],
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("star.plug_p.pin.v"), lhs);

    let rhs = flat::Variable {
        dims: Vec::new(),
        ..flat::Variable::empty_with_span(test_span())
    };
    flat.add_variable(rumoca_core::VarName::new("star.pin_n.v"), rhs);

    let result = validate_dimension_compatibility(
        &flat,
        &rumoca_core::VarName::new("star.plug_p.pin.v"),
        &rumoca_core::VarName::new("star.pin_n.v"),
        Span::DUMMY,
    );
    assert!(
        result.is_ok(),
        "collapsed for-loop pin element connections may map a plug pin array onto a scalar pin"
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

// =============================================================================
// Array Index Matching Tests
// =============================================================================

#[test]
fn test_split_path_with_indices() {
    // Simple path
    assert_eq!(path_segments_of("a.b.c"), vec!["a", "b", "c"]);

    // Path with array indices
    assert_eq!(
        path_segments_of("resistor[1].p.v"),
        vec!["resistor[1]", "p", "v"]
    );

    // Multiple array indices
    assert_eq!(
        path_segments_of("plug_p.pin[2].v"),
        vec!["plug_p", "pin[2]", "v"]
    );

    // Complex indices
    assert_eq!(
        path_segments_of("a[1,2].b[3].c"),
        vec!["a[1,2]", "b[3]", "c"]
    );
}

#[test]
fn test_strip_array_index() {
    assert_eq!(strip_array_index("resistor[1]"), "resistor");
    assert_eq!(strip_array_index("pin[2]"), "pin");
    assert_eq!(strip_array_index("p"), "p");
    assert_eq!(strip_array_index("a[1,2,3]"), "a");
}

#[test]
fn test_strip_embedded_array_indices() {
    assert_eq!(
        strip_embedded_array_indices("battery.resistor[2].p.i"),
        Some("battery.resistor.p.i".to_string())
    );
    assert_eq!(
        strip_embedded_array_indices("battery.capacitor.p.i[2]"),
        Some("battery.capacitor.p.i".to_string())
    );
    assert_eq!(strip_embedded_array_indices("battery.p.i"), None);
}

#[test]
fn test_extract_array_index() {
    assert_eq!(extract_array_index("resistor[1]"), Some("[1]".to_string()));
    assert_eq!(extract_array_index("pin[2]"), Some("[2]".to_string()));
    assert_eq!(extract_array_index("p"), None);
    assert_eq!(extract_array_index("a[1,2,3]"), Some("[1,2,3]".to_string()));
}

#[test]
fn test_parse_array_element_ref_simple_and_indexed_prefix() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("x"),
        flat::Variable {
            dims: vec![3],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("cell[2].v"),
        flat::Variable {
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    assert_eq!(
        parse_array_element_ref("x[1]", &flat),
        Some((rumoca_core::VarName::new("x"), 1))
    );
    assert_eq!(
        parse_array_element_ref("cell[2].v[3]", &flat),
        Some((rumoca_core::VarName::new("cell[2].v"), 3))
    );
}

#[test]
fn test_parse_array_element_ref_rejects_non_scalar_or_non_terminal_subscripts() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("x"),
        flat::Variable {
            dims: vec![2, 2],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    assert_eq!(parse_array_element_ref("x[1,2]", &flat), None);
    assert_eq!(parse_array_element_ref("x[1].y", &flat), None);
}

#[test]
fn test_scalarize_collapsed_connector_element() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("s[1].inductance.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let scalarized = scalarize_collapsed_connector_element(
        &rumoca_core::VarName::new("s[1].inductance.n.i"),
        "s[1].inductance.n[2]",
        &flat,
    );
    assert_eq!(
        scalarized,
        rumoca_core::VarName::new("s[1].inductance.n.i[2]")
    );
}

#[test]
fn test_scalarize_collapsed_connector_element_without_dims_still_scalarizes() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("cell.cell.resistor.p.i"),
        flat::Variable {
            flow: true,
            // Collapsed connector-array fields can reach flatten with dims=[].
            dims: vec![],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let scalarized = scalarize_collapsed_connector_element(
        &rumoca_core::VarName::new("cell.cell.resistor.p.i"),
        "cell.cell.resistor[2].p",
        &flat,
    );
    assert_eq!(
        scalarized,
        rumoca_core::VarName::new("cell.cell.resistor.p.i[2]")
    );
}

#[test]
fn test_is_flow_variable_subscripted_with_unknown_dims() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("arr.n.i"),
        flat::Variable {
            flow: true,
            // Unknown dims in flat::Variable must still allow element flow handling.
            dims: vec![],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    assert!(is_flow_variable(
        &flat,
        &rumoca_core::VarName::new("arr.n.i[2]")
    ));
}

#[test]
fn test_matches_with_array_indices() {
    let matches = |name: &str, segments: &[&str]| {
        let parsed = path_segments_of(name)
            .into_iter()
            .map(std::borrow::ToOwned::to_owned)
            .collect::<Vec<_>>();
        matches_with_array_indices_cached(&parsed, segments)
    };

    // resistor.p should match resistor[1].p.v
    assert!(matches("resistor[1].p.v", &["resistor", "p"]));
    assert!(matches("resistor[2].p.i", &["resistor", "p"]));

    // plug_p.pin should match plug_p.pin[1].v
    assert!(matches("plug_p.pin[1].v", &["plug_p", "pin"]));

    // Exact match (no array indices)
    assert!(matches("r1.n.v", &["r1", "n"]));

    // No match - wrong base name
    assert!(!matches("resistor[1].p.v", &["capacitor", "p"]));

    // No match - not enough parts (no suffix)
    assert!(!matches("resistor[1].p", &["resistor", "p"]));
}

#[test]
fn test_find_exact_match_with_array_expansion_handles_dot_inside_subscript() {
    let var_names = [
        rumoca_core::VarName::new("bus[data.medium].pin"),
        rumoca_core::VarName::new("bus[data.other].pin"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(var_names.iter());

    let matches = find_exact_match_with_array_expansion("bus[data.medium].pin", &var_index);
    assert_eq!(
        matches,
        vec![rumoca_core::VarName::new("bus[data.medium].pin")]
    );
}

#[test]
fn test_find_sub_variables_with_array_expansion_handles_dot_inside_subscript() {
    let var_names = [
        rumoca_core::VarName::new("bus[data.medium].pin.i"),
        rumoca_core::VarName::new("bus[data.medium].pin.v"),
        rumoca_core::VarName::new("bus[data.other].pin.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(var_names.iter());

    let matches =
        find_sub_variables_with_array_expansion_indexed("bus[data.medium].pin", &var_index);
    assert_eq!(matches.len(), 2);
    assert!(matches.contains(&rumoca_core::VarName::new("bus[data.medium].pin.i")));
    assert!(matches.contains(&rumoca_core::VarName::new("bus[data.medium].pin.v")));
}

#[test]
fn test_extract_suffix_exact() {
    // Exact prefix match
    let result = extract_suffix("r1.n.v", "r1.n");
    assert_eq!(result, Some(("v".to_string(), "".to_string())));

    let result = extract_suffix("a.b.c.d", "a.b.c");
    assert_eq!(result, Some(("d".to_string(), "".to_string())));
}

#[test]
fn test_extract_suffix_with_array_indices() {
    // Array prefix match
    let result = extract_suffix("resistor[1].p.v", "resistor.p");
    assert_eq!(result, Some(("v".to_string(), "[1]".to_string())));

    let result = extract_suffix("plug_p.pin[2].i", "plug_p.pin");
    assert_eq!(result, Some(("i".to_string(), "[2]".to_string())));

    // Multiple segments with indices
    let result = extract_suffix("resistor[1].p.v", "resistor.p");
    assert_eq!(result, Some(("v".to_string(), "[1]".to_string())));
}

#[test]
fn test_extract_suffix_preserves_segment_index_when_name_is_collapsed_array_field() {
    let result = extract_suffix("s[1].inductance.n.i", "s[1].inductance.n[2]");
    assert_eq!(result, Some(("i".to_string(), "[1][2]".to_string())));
}

#[test]
fn test_extract_suffix_handles_dot_inside_subscript_expression() {
    let result = extract_suffix("bus[data.medium].pin.i", "bus[data.medium].pin[2]");
    assert_eq!(
        result,
        Some(("i".to_string(), "[data.medium][2]".to_string()))
    );
}

#[test]
fn test_find_matching_var_b_exact() {
    let subs_b = vec![
        rumoca_core::VarName::new("plug_p.pin.v"),
        rumoca_core::VarName::new("plug_p.pin.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("plug_p.pin", &subs_b, &var_index);

    // Exact match
    let result = find_matching_var_b_indexed("v", "", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("plug_p.pin.v")));

    let result = find_matching_var_b_indexed("i", "", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("plug_p.pin.i")));

    // No match
    let result = find_matching_var_b_indexed("x", "", &sub_match_index);
    assert_eq!(result, None);
}

#[test]
fn test_find_matching_var_b_exact_with_dotted_suffix() {
    let subs_b = vec![
        rumoca_core::VarName::new("plug_p.pin.inner.v"),
        rumoca_core::VarName::new("plug_p.pin.inner.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("plug_p.pin", &subs_b, &var_index);

    let result = find_matching_var_b_indexed("inner.v", "", &sub_match_index);
    assert_eq!(
        result,
        Some(rumoca_core::VarName::new("plug_p.pin.inner.v"))
    );
}

#[test]
fn test_find_matching_var_b_with_array_indices() {
    let subs_b = vec![
        rumoca_core::VarName::new("plug_p.pin[1].v"),
        rumoca_core::VarName::new("plug_p.pin[1].i"),
        rumoca_core::VarName::new("plug_p.pin[2].v"),
        rumoca_core::VarName::new("plug_p.pin[2].i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("plug_p.pin", &subs_b, &var_index);

    // Should find matching indexed variable
    let result = find_matching_var_b_indexed("v", "[1]", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("plug_p.pin[1].v")));

    let result = find_matching_var_b_indexed("i", "[2]", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("plug_p.pin[2].i")));

    // Wrong index
    let result = find_matching_var_b_indexed("v", "[3]", &sub_match_index);
    assert_eq!(result, None);
}

#[test]
fn test_find_matching_var_b_with_collapsed_indexed_connector_path() {
    let subs_b = vec![
        rumoca_core::VarName::new("s[1].n.i"),
        rumoca_core::VarName::new("s[1].n.v"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("s[1].n[2]", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("i", "", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("s[1].n.i")));
}

#[test]
fn test_find_matching_var_b_preserves_explicit_cross_index_path() {
    // Connects like resistor[1].n <-> resistor[2].p should use B's explicit
    // index even when A/B indices differ.
    let subs_b = vec![
        rumoca_core::VarName::new("cell.cell.resistor.p.v"),
        rumoca_core::VarName::new("cell.cell.resistor.p.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index =
        ConnectionSubMatchIndex::new("cell.cell.resistor[2].p", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("v", "", &sub_match_index);
    assert_eq!(
        result,
        Some(rumoca_core::VarName::new("cell.cell.resistor.p.v"))
    );
}

#[test]
fn test_find_matching_var_b_does_not_cross_match_connector_member_name() {
    let subs_b = vec![
        rumoca_core::VarName::new("resistor.p.v"),
        rumoca_core::VarName::new("resistor.n.v"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("resistor[1].n", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("v", "", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("resistor.n.v")));
}

#[test]
fn test_find_matching_var_b_allows_indexed_b_when_a_has_no_indices() {
    // Reproduces scalar-to-indexed connector matches like:
    // connect(internalHeatPort, resistor[1].heatPort)
    // where A suffix extraction yields empty indices.
    let subs_b = vec![
        rumoca_core::VarName::new("battery.resistor.heatPort.T"),
        rumoca_core::VarName::new("battery.resistor.heatPort.Q_flow"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index =
        ConnectionSubMatchIndex::new("battery.resistor[1].heatPort", &subs_b, &var_index);

    let t_match = find_matching_var_b_indexed("T", "", &sub_match_index);
    let q_match = find_matching_var_b_indexed("Q_flow", "", &sub_match_index);

    assert_eq!(
        t_match,
        Some(rumoca_core::VarName::new("battery.resistor.heatPort.T"))
    );
    assert_eq!(
        q_match,
        Some(rumoca_core::VarName::new(
            "battery.resistor.heatPort.Q_flow"
        ))
    );
}

#[test]
fn test_strip_explicit_path_indices() {
    assert_eq!(
        strip_explicit_path_indices("[1][2]", "s[1].inductance.n"),
        "[2]"
    );
    assert_eq!(
        strip_explicit_path_indices("[1][2]", "s[1].inductance.n[2]"),
        ""
    );
    assert_eq!(strip_explicit_path_indices("[2]", "plug_p.pin"), "[2]");
}

#[test]
fn test_find_matching_var_b_keeps_trailing_connector_index_with_explicit_prefix() {
    let subs_b = vec![
        rumoca_core::VarName::new("s[1].p[1].i"),
        rumoca_core::VarName::new("s[1].p[2].i"),
        rumoca_core::VarName::new("s[1].p[3].i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("s[1].p", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("i", "[2]", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("s[1].p[2].i")));
}

#[test]
fn test_connect_sub_variable_indexes_collapsed_b_array_member() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("plug_p.pin[2].i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("plugs_n.pin.i"),
        flat::Variable {
            flow: true,
            dims: vec![3],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let sub_a = rumoca_core::VarName::new("plug_p.pin[2].i");
    let subs_b = vec![rumoca_core::VarName::new("plugs_n.pin.i")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("plugs_n.pin", &subs_b, &var_index);

    connect_sub_variable(
        &sub_a,
        "plug_p.pin",
        "plugs_n.pin",
        &sub_match_index,
        &mut ctx,
    );

    assert_eq!(
        flow_pairs,
        vec![(
            rumoca_core::VarName::new("plug_p.pin[2].i"),
            rumoca_core::VarName::new("plugs_n.pin.i[2]")
        )]
    );
}

#[test]
fn test_connect_sub_variable_does_not_index_scalar_b_member() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("resistor[1].p.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("r0.n.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let sub_a = rumoca_core::VarName::new("resistor[1].p.i");
    let subs_b = vec![rumoca_core::VarName::new("r0.n.i")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("r0.n", &subs_b, &var_index);

    connect_sub_variable(&sub_a, "resistor.p", "r0.n", &sub_match_index, &mut ctx);

    assert_eq!(
        flow_pairs,
        vec![(
            rumoca_core::VarName::new("resistor[1].p.i"),
            rumoca_core::VarName::new("r0.n.i")
        )]
    );
}

#[test]
fn test_connect_sub_variable_does_not_index_single_element_b_array_member() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("plug_p.pin[1].i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("starpoints.pin.i"),
        flat::Variable {
            flow: true,
            dims: vec![1],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let sub_a = rumoca_core::VarName::new("plug_p.pin[1].i");
    let subs_b = vec![rumoca_core::VarName::new("starpoints.pin.i")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("starpoints.pin", &subs_b, &var_index);

    connect_sub_variable(
        &sub_a,
        "plug_p.pin",
        "starpoints.pin",
        &sub_match_index,
        &mut ctx,
    );

    assert_eq!(
        flow_pairs,
        vec![(
            rumoca_core::VarName::new("plug_p.pin[1].i"),
            rumoca_core::VarName::new("starpoints.pin.i")
        )]
    );
}

#[test]
fn test_find_sub_variables_with_array_expansion() {
    let mut flat = flat::Model::new();

    // Add variables for resistor[1-3].p.v and resistor[1-3].p.i
    for i in 1..=3 {
        flat.add_variable(
            rumoca_core::VarName::new(format!("resistor[{}].p.v", i)),
            flat::Variable::empty_with_span(test_span()),
        );
        flat.add_variable(
            rumoca_core::VarName::new(format!("resistor[{}].p.i", i)),
            flat::Variable {
                flow: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    // Searching for "resistor.p" should find all resistor[*].p.* variables
    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("resistor.p", &pc, &var_index);
    assert_eq!(subs.len(), 6);

    // Verify all expected variables are found
    for i in 1..=3 {
        assert!(subs.contains(&rumoca_core::VarName::new(format!("resistor[{}].p.v", i))));
        assert!(subs.contains(&rumoca_core::VarName::new(format!("resistor[{}].p.i", i))));
    }
}

#[test]
fn test_find_sub_variables_indexed_prefix_matches_collapsed_connector_array_fields() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("s[1].inductance.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("s[1].inductance.n.v"),
        flat::Variable {
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("s[1].inductance.n[2]", &pc, &var_index);
    assert_eq!(subs.len(), 2);
    assert!(subs.contains(&rumoca_core::VarName::new("s[1].inductance.n.i")));
    assert!(subs.contains(&rumoca_core::VarName::new("s[1].inductance.n.v")));
}

#[test]
fn test_find_sub_variables_exact_match_preferred() {
    let mut flat = flat::Model::new();

    // Add both exact and indexed variables
    flat.add_variable(
        rumoca_core::VarName::new("r1.n.v"),
        flat::Variable::empty_with_span(test_span()),
    );
    flat.add_variable(
        rumoca_core::VarName::new("r1.n.i"),
        flat::Variable::empty_with_span(test_span()),
    );
    flat.add_variable(
        rumoca_core::VarName::new("r1[1].n.v"),
        flat::Variable::empty_with_span(test_span()),
    );
    flat.add_variable(
        rumoca_core::VarName::new("r1[1].n.i"),
        flat::Variable::empty_with_span(test_span()),
    );

    // Searching for "r1.n" should find exact matches
    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("r1.n", &pc, &var_index);
    assert_eq!(subs.len(), 2);
    assert!(subs.contains(&rumoca_core::VarName::new("r1.n.v")));
    assert!(subs.contains(&rumoca_core::VarName::new("r1.n.i")));
}

#[test]
fn test_find_sub_variables_indexed_prefix_does_not_cross_match_connector_members() {
    let mut flat = flat::Model::new();
    // Collapsed connector-array fields commonly appear as indexless members with
    // array dims kept on the primitive variable itself.
    for name in [
        "resistor.p.v",
        "resistor.p.i",
        "resistor.n.v",
        "resistor.n.i",
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                dims: vec![1],
                flow: name.ends_with(".i"),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("resistor[1].n", &pc, &var_index);

    assert_eq!(subs.len(), 2);
    assert!(subs.contains(&rumoca_core::VarName::new("resistor.n.v")));
    assert!(subs.contains(&rumoca_core::VarName::new("resistor.n.i")));
    assert!(!subs.contains(&rumoca_core::VarName::new("resistor.p.v")));
    assert!(!subs.contains(&rumoca_core::VarName::new("resistor.p.i")));
}
