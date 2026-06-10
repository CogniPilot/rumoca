use super::*;

#[test]
fn test_get_output_in_input_output_connection_subscripted_output() {
    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("gain.u"),
        Variable::new(rumoca_core::VarName::new("gain.u")),
    );
    dae.variables.outputs.insert(
        rumoca_core::VarName::new("table.y"),
        Variable::new(rumoca_core::VarName::new("table.y")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("table.y[1]").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("gain.u").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: "table.y[1]".to_string(),
            rhs: "gain.u".to_string(),
        },
        scalar_count: 1,
    };

    let out = get_output_in_input_output_connection(&eq, &dae)
        .expect("subscripted output/input connection should resolve output side");
    assert_eq!(out, VarName::new("table.y[1]"));
}

#[test]
fn test_classify_equations_skips_subscripted_output_input_connection_when_output_has_component_equation()
 {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("table.y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("table.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        }),
    );
    flat.add_variable(
        VarName::new("gain.u"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("gain.u"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        }),
    );

    // Component equation for one output element.
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("table.y[1]").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "table".to_string(),
        },
        scalar_count: 1,
    });

    // Input-output alias should be skipped because output already has a component equation.
    add_connection_equation(&mut flat, "table.y[1]", "gain.u");

    let mut dae = Dae::new();
    dae.variables.outputs.insert(
        rumoca_core::VarName::new("table.y"),
        Variable::new(rumoca_core::VarName::new("table.y")),
    );
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("gain.u"),
        Variable::new(rumoca_core::VarName::new("gain.u")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts).unwrap();

    assert_eq!(
        dae.continuous.equations.len(),
        1,
        "connection equation should be skipped for output element defined by component equation"
    );
    assert!(
        dae.continuous.equations[0]
            .origin
            .contains("equation from table")
    );
}

#[test]
fn test_classify_equations_skips_output_known_connection_when_output_has_component_equation() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("gain.y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("gain.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        }),
    );
    flat.add_variable(
        VarName::new("gain.u"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("gain.u"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        }),
    );
    flat.add_variable(
        VarName::new("outBus.x"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("outBus.x"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        }),
    );

    // Output has an explicit component equation.
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("gain.y").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("gain.u").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "gain".to_string(),
        },
        scalar_count: 1,
    });

    // Redundant alias to non-unknown bus member should be skipped.
    add_connection_equation(&mut flat, "gain.y", "outBus.x");

    let mut dae = Dae::new();
    dae.variables.outputs.insert(
        rumoca_core::VarName::new("gain.y"),
        Variable::new(rumoca_core::VarName::new("gain.y")),
    );
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("gain.u"),
        Variable::new(rumoca_core::VarName::new("gain.u")),
    );
    dae.variables.parameters.insert(
        rumoca_core::VarName::new("outBus.x"),
        Variable::new(rumoca_core::VarName::new("outBus.x")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts).unwrap();

    assert_eq!(
        dae.continuous.equations.len(),
        1,
        "output->known connection should be skipped when output has defining component equation"
    );
    assert!(
        dae.continuous.equations[0]
            .origin
            .contains("equation from gain")
    );
}

#[test]
fn test_classify_equations_skips_unconnected_flow_for_top_level_overconstrained_connector() {
    let mut flat = Model::new();
    flat.top_level_connectors.insert("port".to_string());

    flat.add_variable(
        VarName::new("port.reference.gamma"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("port.reference.gamma"),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            is_overconstrained: true,
            oc_record_path: Some("port.reference".to_string()),
            oc_eq_constraint_size: Some(0),
            ..Default::default()
        }),
    );
    flat.add_variable(
        VarName::new("port.Phi.re"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("port.Phi.re"),
            flow: true,
            is_primitive: true,
            ..Default::default()
        }),
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("port.Phi.re")),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::UnconnectedFlow {
            variable: "port.Phi.re".to_string(),
        },
        scalar_count: 1,
    });

    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("port.Phi.re"),
        Variable::new(rumoca_core::VarName::new("port.Phi.re")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts).unwrap();

    assert!(
        dae.continuous.equations.is_empty(),
        "top-level OC connector unconnected flow closure should not be counted in local structural equations"
    );
}
