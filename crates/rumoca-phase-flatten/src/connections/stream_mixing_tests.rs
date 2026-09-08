use super::*;
use rumoca_ir_ast as ast;

fn stream_test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_flatten_stream_mixing_source.mo"),
        11,
        23,
    )
}

fn fixture_def_id(name: &str) -> rumoca_core::DefId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::DefId::new(hash.max(1))
}

fn add_port(model: &mut flat::Model, port: &str, nominal: f64) {
    let span = stream_test_span();
    let connector_id = model.materialize_instance(flat::InstanceRelation {
        owner: None,
        declaration: None,
        indices: Box::new([]),
        kind: flat::InstanceKind::Materialized,
    });
    let stream_name = rumoca_core::VarName::new(format!("{port}.h_outflow"));
    let stream_id = model.materialize_instance(flat::InstanceRelation {
        owner: Some(connector_id),
        declaration: None,
        indices: Box::new([]),
        kind: flat::InstanceKind::Materialized,
    });
    model.add_variable(
        stream_name.clone(),
        flat::Variable {
            name: stream_name,
            instance_id: stream_id,
            stream: true,
            source_span: span,
            ..connection_test_variable(span)
        },
    );
    let flow_name = rumoca_core::VarName::new(format!("{port}.m_flow"));
    let flow_id = model.materialize_instance(flat::InstanceRelation {
        owner: Some(connector_id),
        declaration: None,
        indices: Box::new([]),
        kind: flat::InstanceKind::Materialized,
    });
    model.add_variable(
        flow_name.clone(),
        flat::Variable {
            name: flow_name,
            instance_id: flow_id,
            flow: true,
            nominal: Some(real_literal(nominal)),
            source_span: span,
            ..connection_test_variable(span)
        },
    );
}

fn add_standalone_variable(
    model: &mut flat::Model,
    name: rumoca_core::VarName,
    mut variable: flat::Variable,
) {
    variable.name = name.clone();
    variable.instance_id = model.materialize_instance(flat::InstanceRelation {
        owner: None,
        declaration: None,
        indices: Box::new([]),
        kind: flat::InstanceKind::Materialized,
    });
    model.add_variable(name, variable);
}

fn real_literal(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: stream_test_span(),
    }
}

fn stream_call(name: &str, target: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: resolved_stream_operator(name),
        args: vec![variable_reference(target)],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: stream_test_span(),
    }
}

fn indexed_stream_call(name: &str, target: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: resolved_stream_operator(name),
        args: vec![rumoca_core::Expression::Index {
            base: Box::new(variable_reference(target)),
            subscripts: vec![rumoca_core::Subscript::Index {
                value: index,
                span: stream_test_span(),
            }],
            span: stream_test_span(),
        }],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: stream_test_span(),
    }
}

fn indexed_connector_field_stream_call(
    name: &str,
    connector: &str,
    field: &str,
    index: i64,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: resolved_stream_operator(name),
        args: vec![rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(variable_reference(connector)),
                subscripts: vec![rumoca_core::Subscript::Index {
                    value: index,
                    span: stream_test_span(),
                }],
                span: stream_test_span(),
            }),
            field: field.to_string(),
            field_def_id: fixture_def_id(field),
            span: stream_test_span(),
        }],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: stream_test_span(),
    }
}

fn symbolic_indexed_connector_field_stream_call(
    name: &str,
    connector: &str,
    field: &str,
    index: &str,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: resolved_stream_operator(name),
        args: vec![rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(variable_reference(connector)),
                subscripts: vec![rumoca_core::Subscript::Expr {
                    expr: Box::new(variable_reference(index)),
                    span: stream_test_span(),
                }],
                span: stream_test_span(),
            }),
            field: field.to_string(),
            field_def_id: fixture_def_id(field),
            span: stream_test_span(),
        }],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: stream_test_span(),
    }
}

fn resolved_stream_operator(name: &str) -> rumoca_core::Reference {
    let role = match name {
        "inStream" => stream_operators::StreamOperatorRole::InStream,
        "actualStream" => stream_operators::StreamOperatorRole::ActualStream,
        _ => panic!("fixture requested unknown stream operator `{name}`"),
    };
    stream_operators::StreamOperatorIdentities::fixture().reference(role, stream_test_span())
}

fn variable_reference(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: stream_test_span(),
    }
}

fn add_observation_equation(model: &mut flat::Model, expression: rumoca_core::Expression) {
    let span = stream_test_span();
    model.add_equation(flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(expression),
            rhs: Box::new(real_literal(0.0)),
            span,
        },
        span,
        origin: flat::EquationOrigin::ComponentEquation {
            component: "observation".to_string(),
        },
        scalar_count: 1,
    });
}

fn stream_overlay(ports: &[&str]) -> ast::InstanceOverlay {
    let span = stream_test_span();
    let mut overlay = ast::InstanceOverlay::new();
    let connections = ports
        .windows(2)
        .map(|pair| {
            ast::InstanceConnection::scalar(
                ast::QualifiedName::from_dotted(&format!("{}.h_outflow", pair[0])),
                ast::QualifiedName::from_dotted(&format!("{}.h_outflow", pair[1])),
                None,
                span,
                String::new(),
            )
            .expect("test scalar connection is valid")
        })
        .collect();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            connections,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

fn outside_stream_overlay(scope: &str, ports: &[&str]) -> ast::InstanceOverlay {
    let span = stream_test_span();
    let mut overlay = ast::InstanceOverlay::new();
    let connector_type = rumoca_core::TypeId(0x51_0001);
    overlay.type_roots.insert(connector_type, connector_type);
    for port in ports {
        let instance_id = overlay.alloc_id();
        overlay
            .add_component(ast::InstanceData {
                instance_id,
                qualified_name: ast::QualifiedName::from_dotted(&format!("{scope}.{port}")),
                type_id: connector_type,
                is_connector_type: true,
                ..Default::default()
            })
            .expect("fixture occurrence insertion must succeed");
    }
    let connections = ports
        .windows(2)
        .map(|pair| {
            ast::InstanceConnection::scalar(
                ast::QualifiedName::from_dotted(&format!("{scope}.{}", pair[0])),
                ast::QualifiedName::from_dotted(&format!("{scope}.{}", pair[1])),
                None,
                span,
                scope.to_string(),
            )
            .expect("test scalar connection is valid")
        })
        .collect();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident(scope),
            connections,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

fn elementwise_array_stream_overlay() -> ast::InstanceOverlay {
    let span = stream_test_span();
    let connections = [1, 2]
        .into_iter()
        .map(|index| {
            ast::InstanceConnection::scalar(
                ast::QualifiedName::from_dotted(&format!("a[{index}].h_outflow")),
                ast::QualifiedName::from_dotted(&format!("b[{index}].h_outflow")),
                None,
                span,
                String::new(),
            )
            .expect("test scalar connection is valid")
        })
        .collect();
    let mut overlay = ast::InstanceOverlay::new();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            connections,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

/// `src.port` connected to `p1.port_a` and `p2.port_a` at the root scope, with
/// each `Pipe` instance connecting its own `port_a` to `v.port` internally.
///
/// `pN.port_a` is therefore the MLS §9.1.2 *outside* connector of the set
/// declared inside `Pipe` and the *inside* connector of the root set.
fn hierarchical_pass_through_overlay() -> ast::InstanceOverlay {
    let span = stream_test_span();
    let mut overlay = ast::InstanceOverlay::new();
    let connector_type = rumoca_core::TypeId(0x51_0002);
    overlay.type_roots.insert(connector_type, connector_type);
    for connector in [
        "src.port",
        "p1.port_a",
        "p1.v.port",
        "p2.port_a",
        "p2.v.port",
    ] {
        let instance_id = overlay.alloc_id();
        overlay
            .add_component(ast::InstanceData {
                instance_id,
                qualified_name: ast::QualifiedName::from_dotted(connector),
                type_id: connector_type,
                is_connector_type: true,
                ..Default::default()
            })
            .expect("fixture occurrence insertion must succeed");
    }
    let connections = [
        ("", "src.port", "p1.port_a"),
        ("", "src.port", "p2.port_a"),
        ("p1", "p1.port_a", "p1.v.port"),
        ("p2", "p2.port_a", "p2.v.port"),
    ]
    .into_iter()
    .map(|(scope, a, b)| {
        ast::InstanceConnection::scalar(
            ast::QualifiedName::from_dotted(a),
            ast::QualifiedName::from_dotted(b),
            None,
            span,
            scope.to_string(),
        )
        .expect("test scalar connection is valid")
    })
    .collect();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Sys"),
            connections,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

fn hierarchical_pass_through_model() -> flat::Model {
    let mut model = connection_test_model();
    for connector in [
        "src.port",
        "p1.port_a",
        "p1.v.port",
        "p2.port_a",
        "p2.v.port",
    ] {
        add_port(&mut model, connector, 1.0);
    }
    model
}

fn count_var_refs(expression: &rumoca_core::Expression, name: &str) -> usize {
    let mut count = 0;
    expression.contains_subexpression(|candidate| {
        if let rumoca_core::Expression::VarRef {
            name: reference, ..
        } = candidate
            && reference.as_str() == name
        {
            count += 1;
        }
        false
    });
    count
}

/// True when the expression weighs `flow` with `max(-flow, eps)` (MLS §15.2
/// inside connector) rather than `max(+flow, eps)` (outside connector).
fn weights_negated_flow(expression: &rumoca_core::Expression, flow: &str) -> bool {
    expression.contains_subexpression(|candidate| {
        matches!(
            candidate,
            rumoca_core::Expression::BuiltinCall { function: rumoca_core::BuiltinFunction::Max, args, .. }
                if matches!(
                    args.first(),
                    Some(rumoca_core::Expression::Unary { op: rumoca_core::OpUnary::Minus, rhs, .. })
                        if matches!(
                            rhs.as_ref(),
                            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == flow
                        )
                )
        )
    })
}

fn weights_positive_flow(expression: &rumoca_core::Expression, flow: &str) -> bool {
    expression.contains_subexpression(|candidate| {
        matches!(
            candidate,
            rumoca_core::Expression::BuiltinCall { function: rumoca_core::BuiltinFunction::Max, args, .. }
                if matches!(
                    args.first(),
                    Some(rumoca_core::Expression::VarRef { name, .. }) if name.as_str() == flow
                )
        )
    })
}

fn process(model: &mut flat::Model, overlay: &ast::InstanceOverlay) -> Result<(), FlattenError> {
    finalize_connection_test_flat(model);
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("stream fixture must construct finalized occurrence proofs");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
    process_connections_for_test(model, &overconstrained, &mut forest)
}

fn observed_expression(model: &flat::Model) -> &rumoca_core::Expression {
    let rumoca_core::Expression::Binary { lhs, .. } = &model.equations[0].residual else {
        panic!("expected observation residual");
    };
    lhs
}

#[test]
fn three_connector_instream_lowers_to_regularized_weighted_mean() {
    let mut model = connection_test_model();
    for (port, nominal) in [("a", 2.0), ("b", 4.0), ("c", 6.0)] {
        add_port(&mut model, port, nominal);
    }
    add_observation_equation(&mut model, stream_call("inStream", "a.h_outflow"));

    process(&mut model, &stream_overlay(&["a", "b", "c"]))
        .expect("three-connector stream mixing should lower");

    let expression = observed_expression(&model);
    assert!(matches!(
        expression,
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            ..
        }
    ));
    let mut references = std::collections::HashSet::new();
    expression.collect_var_refs(&mut references);
    for expected in ["b.h_outflow", "c.h_outflow", "b.m_flow", "c.m_flow"] {
        assert!(
            references.contains(&rumoca_core::VarName::new(expected)),
            "weighted mean should reference {expected}: {expression:?}"
        );
    }
    assert!(!references.contains(&rumoca_core::VarName::new("a.m_flow")));
    assert!(!expression.contains_subexpression(|candidate| {
        matches!(
            candidate,
            rumoca_core::Expression::FunctionCall { name, .. }
                if matches!(name.var_name().last_segment(), "inStream" | "actualStream")
        )
    }));
}

#[test]
fn connected_outside_stream_connectors_generate_one_equation_each() {
    let mut model = connection_test_model();
    for port in ["port_1", "port_2", "port_3"] {
        add_port(&mut model, &format!("junction.{port}"), 1.0);
    }

    process(
        &mut model,
        &outside_stream_overlay("junction", &["port_1", "port_2", "port_3"]),
    )
    .expect("outside stream equations should lower");

    let outside_stream_equations = model
        .equations
        .iter()
        .filter(|equation| matches!(&equation.origin, flat::EquationOrigin::OutsideStream { .. }))
        .collect::<Vec<_>>();
    assert_eq!(
        outside_stream_equations.len(),
        3,
        "MLS §15.1 requires one equation per outside stream connector"
    );
    assert!(outside_stream_equations.iter().all(|equation| {
        !equation.residual.contains_subexpression(|expression| {
            matches!(
                expression,
                rumoca_core::Expression::FunctionCall { name, .. }
                    if name.var_name().last_segment() == "inStream"
            )
        })
    }));
}

#[test]
fn actual_stream_lowers_to_flow_direction_if_expression() {
    let mut model = connection_test_model();
    add_port(&mut model, "a", 1.0);
    add_port(&mut model, "b", 1.0);
    add_observation_equation(&mut model, stream_call("actualStream", "a.h_outflow"));

    process(&mut model, &stream_overlay(&["a", "b"]))
        .expect("actualStream should lower during connection expansion");

    let rumoca_core::Expression::If {
        branches,
        else_branch,
        ..
    } = observed_expression(&model)
    else {
        panic!("expected actualStream to lower to an if-expression");
    };
    assert_eq!(branches.len(), 1);
    assert!(matches!(
        &branches[0].0,
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs,
            ..
        } if matches!(
            lhs.as_ref(),
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "a.m_flow"
        )
    ));
    assert!(matches!(
        &branches[0].1,
        rumoca_core::Expression::VarRef { name, .. }
            if name.as_str() == "b.h_outflow"
    ));
    assert!(matches!(
        else_branch.as_ref(),
        rumoca_core::Expression::VarRef { name, .. }
            if name.as_str() == "a.h_outflow"
    ));
}

#[test]
fn indexed_instream_preserves_the_element_access_on_each_peer() {
    let mut model = connection_test_model();
    add_port(&mut model, "a", 1.0);
    add_port(&mut model, "b", 1.0);
    add_observation_equation(
        &mut model,
        indexed_stream_call("inStream", "a.h_outflow", 2),
    );

    process(&mut model, &stream_overlay(&["a", "b"]))
        .expect("indexed inStream should lower during connection expansion");

    assert!(matches!(
        observed_expression(&model),
        rumoca_core::Expression::Index {
            base,
            subscripts,
            ..
        } if matches!(
            base.as_ref(),
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "b.h_outflow"
        ) && matches!(
            subscripts.as_slice(),
            [rumoca_core::Subscript::Index { value: 2, .. }]
        )
    ));
}

#[test]
fn indexed_connector_field_stream_access_preserves_the_connector_index() {
    let mut model = connection_test_model();
    add_port(&mut model, "a", 1.0);
    add_port(&mut model, "b", 1.0);
    add_observation_equation(
        &mut model,
        indexed_connector_field_stream_call("inStream", "a", "h_outflow", 2),
    );

    process(&mut model, &stream_overlay(&["a", "b"]))
        .expect("indexed connector-field inStream should lower during connection expansion");

    assert!(matches!(
        observed_expression(&model),
        rumoca_core::Expression::FieldAccess {
            base,
            field,
            ..
        } if matches!(
            base.as_ref(),
            rumoca_core::Expression::Index {
                base,
                subscripts,
                ..
            } if matches!(
                base.as_ref(),
                rumoca_core::Expression::VarRef { name, .. }
                    if name.as_str() == "b"
            ) && matches!(
                subscripts.as_slice(),
                [rumoca_core::Subscript::Index { value: 2, .. }]
            )
        ) && field == "h_outflow"
    ));
}

#[test]
fn symbolic_connector_index_selects_the_matching_scalar_stream_set() {
    let mut model = connection_test_model();
    for port in ["a[1]", "a[2]", "b[1]", "b[2]"] {
        add_port(&mut model, port, 1.0);
    }
    add_observation_equation(
        &mut model,
        symbolic_indexed_connector_field_stream_call("inStream", "a", "h_outflow", "i"),
    );

    process(&mut model, &elementwise_array_stream_overlay())
        .expect("symbolic array-connector inStream should retain a compact indexed selection");

    let rumoca_core::Expression::If {
        branches,
        else_branch,
        ..
    } = observed_expression(&model)
    else {
        panic!("expected indexed stream-set selection to lower to an if-expression");
    };
    assert_eq!(branches.len(), 1);
    assert!(matches!(
        &branches[0].1,
        rumoca_core::Expression::VarRef { name, .. }
            if name.as_str() == "b[1].h_outflow"
    ));
    assert!(matches!(
        else_branch.as_ref(),
        rumoca_core::Expression::VarRef { name, .. }
            if name.as_str() == "b[2].h_outflow"
    ));
}

#[test]
fn stream_operator_rejects_non_stream_argument_with_source_span() {
    let span = stream_test_span();
    let mut model = connection_test_model();
    let name = rumoca_core::VarName::new("a.temperature");
    add_standalone_variable(
        &mut model,
        name.clone(),
        flat::Variable {
            source_span: span,
            ..connection_test_variable(span)
        },
    );
    add_port(&mut model, "a", 1.0);
    add_observation_equation(&mut model, stream_call("actualStream", "a.temperature"));

    let error = process(&mut model, &stream_overlay(&[]))
        .expect_err("actualStream on a non-stream variable must fail");
    assert!(
        matches!(
            &error,
            FlattenError::UnsupportedEquation {
                description,
                span: _
            } if description.contains("is not a stream variable")
        ),
        "unexpected stream diagnostic: {error:?}"
    );
}

/// MLS §15.2 defines `inStream` elementwise over an array-valued stream member,
/// so a zero-sized member (`stream Real Xi_outflow[nXi]` with `nXi = 0`, as in
/// `Modelica.Fluid.Interfaces.FluidPort` for a single-substance medium) leaves
/// no scalar Flat variable behind and must lower to the empty array rather than
/// being rejected as a non-stream reference.
#[test]
fn zero_sized_array_stream_member_of_a_connector_array_lowers_to_an_empty_array() {
    let mut model = connection_test_model();
    add_port(&mut model, "a[1]", 1.0);
    add_port(&mut model, "b[1]", 1.0);
    add_observation_equation(
        &mut model,
        indexed_connector_field_stream_call("inStream", "a", "Xi_outflow", 1),
    );

    process(&mut model, &stream_overlay(&["a[1]", "b[1]"]))
        .expect("a zero-sized array-valued stream member must lower, not fail");

    assert!(
        matches!(
            observed_expression(&model),
            rumoca_core::Expression::Array { elements, is_matrix: false, .. }
                if elements.is_empty()
        ),
        "expected an empty array result: {:?}",
        observed_expression(&model)
    );
}

/// `Modelica.Fluid.Examples.Tanks.TanksWithOverflow` reaches the zero-sized
/// member through an unsubscripted connector-array projection
/// (`upperTank.ports.Xi_outflow`), which reduces to a plain dotted name rather
/// than a `FieldAccess` on an `Index`. That spelling must lower identically.
#[test]
fn zero_sized_stream_member_projected_off_a_whole_connector_array_lowers() {
    let mut model = connection_test_model();
    add_port(&mut model, "a[1]", 1.0);
    add_port(&mut model, "b[1]", 1.0);
    add_observation_equation(&mut model, stream_call("inStream", "a.Xi_outflow"));

    process(&mut model, &stream_overlay(&["a[1]", "b[1]"]))
        .expect("a whole-array stream-member projection must lower, not fail");

    assert!(
        matches!(
            observed_expression(&model),
            rumoca_core::Expression::Array { elements, is_matrix: false, .. }
                if elements.is_empty()
        ),
        "expected an empty array result: {:?}",
        observed_expression(&model)
    );
}

/// `actualStream` is defined elementwise by the same MLS §15.2 rule, so a
/// zero-sized member must yield the empty array instead of a flow-direction
/// `if` expression over a nonexistent scalar.
#[test]
fn zero_sized_array_stream_member_lowers_actual_stream_to_an_empty_array() {
    let mut model = connection_test_model();
    add_port(&mut model, "a[1]", 1.0);
    add_port(&mut model, "b[1]", 1.0);
    add_observation_equation(
        &mut model,
        indexed_connector_field_stream_call("actualStream", "a", "Xi_outflow", 1),
    );

    process(&mut model, &stream_overlay(&["a[1]", "b[1]"]))
        .expect("a zero-sized array-valued stream member must lower, not fail");

    assert!(
        matches!(
            observed_expression(&model),
            rumoca_core::Expression::Array { elements, is_matrix: false, .. }
                if elements.is_empty()
        ),
        "expected an empty array result: {:?}",
        observed_expression(&model)
    );
}

/// The empty-array fallback must stay confined to members of connectors that
/// actually carry stream variables; anything else keeps the EF004 diagnostic.
#[test]
fn missing_member_of_a_non_stream_connector_still_reports_a_stream_diagnostic() {
    let span = stream_test_span();
    let mut model = connection_test_model();
    add_port(&mut model, "a", 1.0);
    let name = rumoca_core::VarName::new("plain.p");
    add_standalone_variable(
        &mut model,
        name.clone(),
        flat::Variable {
            source_span: span,
            ..connection_test_variable(span)
        },
    );
    add_observation_equation(&mut model, stream_call("inStream", "plain.Xi_outflow"));

    let error = process(&mut model, &stream_overlay(&[]))
        .expect_err("inStream on a non-stream connector member must fail");
    assert!(
        matches!(
            &error,
            FlattenError::UnsupportedEquation { description, span: _ }
                if description.contains("is not a stream variable")
        ),
        "unexpected stream diagnostic: {error:?}"
    );
}

/// MLS §15.2: the mixing enthalpy weighs every *branch* of the connection set
/// exactly once. A hierarchical pass-through connector pair
/// (`p1.port_a` / `p1.v.port`, which the §9.2 flow sum already forces to carry
/// the same mass flow) is one branch, not two.
#[test]
fn hierarchical_pass_through_branch_is_weighted_exactly_once() {
    let mut model = hierarchical_pass_through_model();
    add_observation_equation(&mut model, stream_call("inStream", "src.port.h_outflow"));

    process(&mut model, &hierarchical_pass_through_overlay())
        .expect("hierarchical stream mixing should lower");

    let expression = observed_expression(&model).clone();
    for branch in ["p1.port_a", "p2.port_a"] {
        assert_eq!(
            count_var_refs(&expression, &format!("{branch}.h_outflow")),
            1,
            "branch {branch} must contribute exactly one value: {expression:?}"
        );
        assert_eq!(
            count_var_refs(&expression, &format!("{branch}.m_flow")),
            2,
            "branch {branch} must contribute one weight to numerator and denominator: {expression:?}"
        );
        assert!(
            weights_negated_flow(&expression, &format!("{branch}.m_flow")),
            "inside connector {branch} must be weighted with max(-m_flow, eps): {expression:?}"
        );
    }
    for hidden in ["p1.v.port", "p2.v.port"] {
        assert_eq!(
            count_var_refs(&expression, &format!("{hidden}.h_outflow")),
            0,
            "{hidden} is behind the pass-through connector and must not be a second branch: {expression:?}"
        );
        assert_eq!(
            count_var_refs(&expression, &format!("{hidden}.m_flow")),
            0,
            "{hidden} must not contribute a second weight: {expression:?}"
        );
    }
    assert_eq!(count_var_refs(&expression, "src.port.m_flow"), 0);
}

/// MLS §15.2: an outside peer carries `max(+m_flow, 0)` and contributes
/// `inStream()` of that connector — the value the *enclosing* set pushes in —
/// which resolves one hierarchy level up.
#[test]
fn outside_peer_uses_positive_flow_sign_and_the_enclosing_set() {
    let mut model = hierarchical_pass_through_model();
    add_observation_equation(&mut model, stream_call("inStream", "p1.v.port.h_outflow"));

    process(&mut model, &hierarchical_pass_through_overlay())
        .expect("hierarchical stream mixing should lower");

    let expression = observed_expression(&model).clone();
    assert!(
        !expression.contains_subexpression(|candidate| {
            matches!(
                candidate,
                rumoca_core::Expression::FunctionCall { name, .. }
                    if matches!(name.var_name().last_segment(), "inStream" | "actualStream")
            )
        }),
        "nested stream operators must be resolved: {expression:?}"
    );
    for source in ["src.port", "p2.port_a"] {
        assert_eq!(
            count_var_refs(&expression, &format!("{source}.h_outflow")),
            1,
            "{source} feeds pipe 1 through the root set: {expression:?}"
        );
        assert!(
            weights_negated_flow(&expression, &format!("{source}.m_flow")),
            "inside connector {source} keeps the max(-m_flow, eps) weight: {expression:?}"
        );
    }
    assert_eq!(
        count_var_refs(&expression, "p1.v.port.h_outflow"),
        0,
        "a connector never mixes with itself: {expression:?}"
    );
    assert_eq!(
        count_var_refs(&expression, "p1.port_a.h_outflow"),
        0,
        "h_outflow of the pass-through connector is what pipe 1 pushes out; it must not appear in what flows in: {expression:?}"
    );
}

/// `src.port` feeds `p1.port_a`, which fans out to two volumes inside the pipe.
/// The set declared inside `Pipe` then has one outside and two inside members,
/// so the outside weighting is visible rather than cancelled.
fn branching_pipe_overlay() -> ast::InstanceOverlay {
    let span = stream_test_span();
    let mut overlay = ast::InstanceOverlay::new();
    let connector_type = rumoca_core::TypeId(0x51_0003);
    overlay.type_roots.insert(connector_type, connector_type);
    for connector in ["src.port", "p1.port_a", "p1.v.port", "p1.w.port"] {
        let instance_id = overlay.alloc_id();
        overlay
            .add_component(ast::InstanceData {
                instance_id,
                qualified_name: ast::QualifiedName::from_dotted(connector),
                type_id: connector_type,
                is_connector_type: true,
                ..Default::default()
            })
            .expect("fixture occurrence insertion must succeed");
    }
    let connections = [
        ("", "src.port", "p1.port_a"),
        ("p1", "p1.port_a", "p1.v.port"),
        ("p1", "p1.port_a", "p1.w.port"),
    ]
    .into_iter()
    .map(|(scope, a, b)| {
        ast::InstanceConnection::scalar(
            ast::QualifiedName::from_dotted(a),
            ast::QualifiedName::from_dotted(b),
            None,
            span,
            scope.to_string(),
        )
        .expect("test scalar connection is valid")
    })
    .collect();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::from_ident("Sys"),
            connections,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

/// MLS §15.2: within one set, an inside connector is weighted `max(-m_flow, 0)`
/// and an outside connector `max(+m_flow, 0)`.
#[test]
fn outside_connector_is_weighted_with_the_unnegated_flow() {
    let mut model = connection_test_model();
    for connector in ["src.port", "p1.port_a", "p1.v.port", "p1.w.port"] {
        add_port(&mut model, connector, 1.0);
    }
    add_observation_equation(&mut model, stream_call("inStream", "p1.v.port.h_outflow"));

    process(&mut model, &branching_pipe_overlay()).expect("branching stream mixing should lower");

    let expression = observed_expression(&model).clone();
    assert!(
        weights_positive_flow(&expression, "p1.port_a.m_flow"),
        "outside connector p1.port_a must be weighted max(+m_flow, eps): {expression:?}"
    );
    assert!(
        !weights_negated_flow(&expression, "p1.port_a.m_flow"),
        "outside connector p1.port_a must not keep the inside sign: {expression:?}"
    );
    assert!(
        weights_negated_flow(&expression, "p1.w.port.m_flow"),
        "inside connector p1.w.port must be weighted max(-m_flow, eps): {expression:?}"
    );
    assert_eq!(
        count_var_refs(&expression, "src.port.h_outflow"),
        1,
        "the outside peer contributes inStream(p1.port_a), i.e. what the source pushes in: {expression:?}"
    );
    assert_eq!(
        count_var_refs(&expression, "p1.port_a.h_outflow"),
        0,
        "the outside peer contributes inStream(p1.port_a), never p1.port_a.h_outflow: {expression:?}"
    );
}

/// MLS §15.2 / STRM-004: the equation generated for an outside connector is the
/// mix of the set declared *inside* its own model, not `inStream()` of itself
/// (which by definition looks at the set one level up).
#[test]
fn outside_connector_equation_reports_its_own_scope_mixture() {
    let mut model = hierarchical_pass_through_model();

    process(&mut model, &hierarchical_pass_through_overlay())
        .expect("hierarchical stream mixing should lower");

    let equation = model
        .equations
        .iter()
        .find(|equation| {
            matches!(
                &equation.origin,
                flat::EquationOrigin::OutsideStream { variable }
                    if variable == "p1.port_a.h_outflow"
            )
        })
        .expect("MLS §15.1 requires one equation per outside stream connector");
    let rumoca_core::Expression::Binary { rhs, .. } = &equation.residual else {
        panic!("expected an equality residual");
    };
    assert!(
        matches!(
            rhs.as_ref(),
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "p1.v.port.h_outflow"
        ),
        "the pass-through connector must report what pipe 1 pushes out: {rhs:?}"
    );
}

#[test]
fn malformed_stream_set_reports_missing_flat_variable_without_panicking() {
    let model = connection_test_model();
    let missing = rumoca_core::VarName::new("missing.h_outflow");
    let sets = vec![StreamConnectionSet {
        variables: vec![missing],
        scope: String::new(),
        span: Span::DUMMY,
    }];

    let Err(error) = stream_operators::build_stream_connection_endpoints(
        &model,
        &sets,
        &InterfaceStreamEndpointsByScope::default(),
    ) else {
        panic!("a stream-set endpoint without a Flat variable must fail");
    };

    assert!(
        matches!(
            &error,
            FlattenError::InvalidConnectionEvidence { description, .. }
                if description.contains("missing.h_outflow")
        ),
        "unexpected stream diagnostic: {error:?}"
    );
}

#[test]
fn missing_stream_member_refuses_compatibility_before_rewriting_or_projection_commit() {
    let mut model = hierarchical_pass_through_model();
    finalize_connection_test_flat(&mut model);
    model.add_equation(flat::Equation::new(
        stream_call("inStream", "src.port.h_outflow"),
        stream_test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "sentinel".to_string(),
        },
    ));
    model
        .variables
        .shift_remove(&rumoca_core::VarName::new("p1.port_a.m_flow"));
    let before = connection_mutation_snapshot(&model);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let overlay = hierarchical_pass_through_overlay();
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("stream fixture must construct finalized occurrence proofs");
    let error = process_connections_for_test(&mut model, &overconstrained, &mut forest)
        .expect_err("a stream endpoint without its exact associated flow must refuse");

    assert!(matches!(error, FlattenError::IncompatibleConnectors { .. }));
    assert_eq!(connection_mutation_snapshot(&model), before);
    assert!(
        model.equations[0]
            .residual
            .contains_subexpression(|expression| {
                matches!(
                    expression,
                    rumoca_core::Expression::FunctionCall { name, .. }
                        if name.var_name().last_segment() == "inStream"
                )
            })
    );
}
