use super::*;

#[test]
fn test_count_interface_flows_requires_top_level_connector_membership() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("delta.pin.i"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("delta.pin.i"),
            variability: rumoca_core::Variability::Empty,
            flow: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    assert_eq!(
        count_interface_flows(&flat),
        0,
        "flow vars outside top-level connector components must not contribute interface-flow equations"
    );

    flat.top_level_connectors.insert("delta".to_string());
    assert_eq!(
        count_interface_flows(&flat),
        1,
        "top-level connector membership is the sole interface-flow eligibility criterion"
    );
}

#[test]
fn test_classify_equations_preserves_flat_scalar_count_for_flow_sum() {
    let span = crate::test_support::test_span();
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("arr.i"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("arr.i"),
            flow: true,
            dims: vec![2],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("s.i"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("s.i"),
            flow: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Integer(0),
                span,
            }),
            rhs: Box::new(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(make_var_ref("arr.i")),
                rhs: Box::new(make_var_ref("s.i")),
                span,
            }),
            span,
        },
        span,
        origin: rumoca_ir_flat::EquationOrigin::FlowSum {
            description: "arr.i + s.i = 0".to_string(),
        },
        scalar_count: 1,
    });

    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("arr.i"),
        Variable::new(
            rumoca_core::VarName::new("arr.i"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("s.i"),
        Variable::new(
            rumoca_core::VarName::new("s.i"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts).unwrap();

    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(
        dae.continuous.equations[0].scalar_count, 1,
        "flow-sum scalar_count from flatten should not be re-inflated by VarRef inference"
    );
}

#[test]
fn test_classify_equations_flow_sum_with_multiple_arrays_is_array_sized() {
    let span = crate::test_support::test_span();
    let mut flat = Model::new();
    for name in ["arr1.i", "arr2.i"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                flow: true,
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
    flat.add_variable(
        VarName::new("s.i"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("s.i"),
            flow: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Integer(0),
                span,
            }),
            rhs: Box::new(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(make_var_ref("arr1.i")),
                rhs: Box::new(rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Add,
                    lhs: Box::new(make_var_ref("arr2.i")),
                    rhs: Box::new(make_var_ref("s.i")),
                    span,
                }),
                span,
            }),
            span,
        },
        span,
        origin: rumoca_ir_flat::EquationOrigin::FlowSum {
            description: "arr1.i + arr2.i + s.i = 0".to_string(),
        },
        scalar_count: 1,
    });

    let mut dae = Dae::new();
    for name in ["arr1.i", "arr2.i", "s.i"] {
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            Variable::new(
                rumoca_core::VarName::new(name),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
    }

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts).unwrap();

    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(
        dae.continuous.equations[0].scalar_count, 2,
        "flow sum with multiple array terms should keep array-sized scalarization"
    );
}
