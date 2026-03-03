use super::*;
use rumoca_ir_ast as ast;

#[test]
fn test_classify_equations_keeps_unconnected_flow_for_regular_top_level_connector() {
    let mut flat = Model::new();
    flat.top_level_connectors.insert("pin".to_string());
    flat.add_variable(
        VarName::new("pin.i"),
        flat::Variable {
            name: VarName::new("pin.i"),
            flow: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(make_var_ref("pin.i")),
            rhs: Box::new(flat::Expression::Literal(Literal::Integer(0))),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::UnconnectedFlow {
            variable: "pin.i".to_string(),
        },
        scalar_count: 1,
    });

    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("pin.i"), Variable::new(VarName::new("pin.i")));

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert_eq!(
        dae.f_x.len(),
        1,
        "non-OC connector unconnected flow closure remains a counted structural equation"
    );
}

#[test]
fn test_count_interface_flows_requires_top_level_connector_membership() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("delta.pin.i"),
        flat::Variable {
            name: VarName::new("delta.pin.i"),
            variability: ast::Variability::Empty,
            flow: true,
            is_primitive: true,
            ..Default::default()
        },
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
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("arr.i"),
        flat::Variable {
            name: VarName::new("arr.i"),
            flow: true,
            dims: vec![2],
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("s.i"),
        flat::Variable {
            name: VarName::new("s.i"),
            flow: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::Literal(Literal::Integer(0))),
            rhs: Box::new(flat::Expression::Binary {
                op: ast::OpBinary::Add(ast::Token::default()),
                lhs: Box::new(make_var_ref("arr.i")),
                rhs: Box::new(make_var_ref("s.i")),
            }),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::FlowSum {
            description: "arr.i + s.i = 0".to_string(),
        },
        scalar_count: 1,
    });

    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("arr.i"), Variable::new(VarName::new("arr.i")));
    dae.algebraics
        .insert(VarName::new("s.i"), Variable::new(VarName::new("s.i")));

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert_eq!(dae.f_x.len(), 1);
    assert_eq!(
        dae.f_x[0].scalar_count, 1,
        "flow-sum scalar_count from flatten should not be re-inflated by VarRef inference"
    );
}

#[test]
fn test_classify_equations_flow_sum_with_multiple_arrays_is_array_sized() {
    let mut flat = Model::new();
    for name in ["arr1.i", "arr2.i"] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                flow: true,
                dims: vec![2],
                is_primitive: true,
                ..Default::default()
            },
        );
    }
    flat.add_variable(
        VarName::new("s.i"),
        flat::Variable {
            name: VarName::new("s.i"),
            flow: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::Literal(Literal::Integer(0))),
            rhs: Box::new(flat::Expression::Binary {
                op: ast::OpBinary::Add(ast::Token::default()),
                lhs: Box::new(make_var_ref("arr1.i")),
                rhs: Box::new(flat::Expression::Binary {
                    op: ast::OpBinary::Add(ast::Token::default()),
                    lhs: Box::new(make_var_ref("arr2.i")),
                    rhs: Box::new(make_var_ref("s.i")),
                }),
            }),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::FlowSum {
            description: "arr1.i + arr2.i + s.i = 0".to_string(),
        },
        scalar_count: 1,
    });

    let mut dae = Dae::new();
    for name in ["arr1.i", "arr2.i", "s.i"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert_eq!(dae.f_x.len(), 1);
    assert_eq!(
        dae.f_x[0].scalar_count, 2,
        "flow sum with multiple array terms should keep array-sized scalarization"
    );
}
