use super::*;

#[test]
fn test_equation_defined_indexed_array_input_promotes_internal_input() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("plant.omega_cmd"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("plant.omega_cmd"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            dims: vec![4],
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    add_component_equation(
        &mut flat,
        "plant.omega_cmd[1]",
        make_var_ref("motor_cmd[1]"),
    );

    let defined_inputs = find_equation_defined_inputs_for_test(&flat);
    assert!(
        defined_inputs.contains(&VarName::new("plant.omega_cmd")),
        "internal array input assigned by an indexed equation should be promoted"
    );
}

#[test]
fn test_equation_defined_array_lhs_promotes_internal_input_elements() {
    let mut flat = Model::new();
    for name in ["plant.motor[1].omega_cmd", "plant.motor[2].omega_cmd"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    make_var_ref("plant.motor[1].omega_cmd"),
                    make_var_ref("plant.motor[2].omega_cmd"),
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(make_var_ref("plant.omega_cmd")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "plant.motor".to_string(),
        },
        scalar_count: 2,
    });

    let defined_inputs = find_equation_defined_inputs_for_test(&flat);
    for name in ["plant.motor[1].omega_cmd", "plant.motor[2].omega_cmd"] {
        assert!(
            defined_inputs.contains(&VarName::new(name)),
            "array LHS input {name} should be promoted"
        );
    }
}

#[test]
fn test_rhs_input_alias_with_lhs_internal_input_promotes_rhs_input() {
    let mut flat = Model::new();
    for name in ["plant.omega_cmd", "plant.motor[1].omega_cmd"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    add_component_equation(
        &mut flat,
        "plant.omega_cmd",
        make_var_ref("plant.motor[1].omega_cmd"),
    );

    let defined_inputs = find_equation_defined_inputs_for_test(&flat);
    assert!(
        defined_inputs.contains(&VarName::new("plant.motor[1].omega_cmd")),
        "RHS child input should be promoted when aliased to another internal input"
    );
}

#[test]
fn test_rhs_array_alias_with_lhs_internal_input_promotes_rhs_inputs() {
    let mut flat = Model::new();
    for (name, dims) in [
        ("plant.omega_cmd", vec![2]),
        ("plant.motor[1].omega_cmd", vec![]),
        ("plant.motor[2].omega_cmd", vec![]),
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                dims,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("plant.omega_cmd")),
            rhs: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    make_var_ref("plant.motor[1].omega_cmd"),
                    make_var_ref("plant.motor[2].omega_cmd"),
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "plant".to_string(),
        },
        scalar_count: 2,
    });

    let defined_inputs = find_equation_defined_inputs_for_test(&flat);
    for name in [
        "plant.omega_cmd",
        "plant.motor[1].omega_cmd",
        "plant.motor[2].omega_cmd",
    ] {
        assert!(
            defined_inputs.contains(&VarName::new(name)),
            "internal input {name} should be promoted through vector alias"
        );
    }
}

#[test]
fn test_component_array_selection_lhs_promotes_indexed_internal_inputs() {
    let mut flat = Model::new();
    for (name, dims) in [
        ("plant.omega_cmd", vec![2]),
        ("plant.motor[1].omega_cmd", vec![]),
        ("plant.motor[2].omega_cmd", vec![]),
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                dims,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    add_component_equation(
        &mut flat,
        "plant.motor.omega_cmd",
        make_var_ref("plant.omega_cmd"),
    );

    let defined_inputs = find_equation_defined_inputs_for_test(&flat);
    for name in [
        "plant.omega_cmd",
        "plant.motor[1].omega_cmd",
        "plant.motor[2].omega_cmd",
    ] {
        assert!(
            defined_inputs.contains(&VarName::new(name)),
            "component-array selection should promote internal input {name}"
        );
    }
}
