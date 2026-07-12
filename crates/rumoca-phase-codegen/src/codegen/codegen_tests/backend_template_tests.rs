use super::*;

#[test]
fn test_embedded_c_alg_rhs_indexes_common_array_binary_rhs() {
    let rhs = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: "error_dot".into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: "error".into(),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Pre,
                args: vec![rumoca_core::Expression::VarRef {
                    name: "q".into(),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    let dae_json = serde_json::json!({
        "f_x": [
            {
                "rhs": serde_json::to_value(rhs).unwrap()
            }
        ]
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "powf", "float_literals": true, "subscript_underscore": true} %}
{{ alg_rhs_for_var("error_dot[2]", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("(error_2 + pre(q_2))"),
        "expected indexed array algebraic RHS in generated C, got:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found for error_dot[2]"),
        "codegen should not fall back to warning stub for indexed array algebraics:\n{rendered}"
    );
}

#[test]
fn test_alg_rhs_indexes_structured_residual_equations_for_indexed_var() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "lhs": null,
            "rhs": {
                "Binary": {
                    "op": "Sub",
                    "lhs": {
                        "VarRef": {
                            "name": {
                                "name": "network.heat",
                                "component_ref": {
                                    "local": false,
                                    "parts": [
                                        {"ident": "network", "subs": []},
                                        {"ident": "heat", "subs": []}
                                    ],
                                    "def_id": 1
                                }
                            },
                            "subscripts": [{"Index": {"value": 2}}]
                        }
                    },
                    "rhs": {
                        "Binary": {
                            "op": "Add",
                            "lhs": {
                                "VarRef": {
                                    "name": {"name": "floor_heat"},
                                    "subscripts": [
                                        {"Index": {"value": 1}},
                                        {"Index": {"value": 2}}
                                    ]
                                }
                            },
                            "rhs": {
                                "VarRef": {
                                    "name": {"name": "internal_gain"},
                                    "subscripts": [
                                        {"Index": {"value": 1}},
                                        {"Index": {"value": 2}}
                                    ]
                                }
                            }
                        }
                    }
                }
            },
            "origin": "top-level model equation",
            "scalar_count": 1
        }],
        "w": {},
        "y": {
            "network.heat[2]": {},
            "floor_heat[1,2]": {},
            "internal_gain[1,2]": {}
        },
        "x": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
{{ alg_rhs_for_var_with_dae("network.heat[2]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert_eq!(
        rendered.trim(),
        "(floor_heat_1_2 + internal_gain_1_2)",
        "indexed residual equations must be discoverable through the generic algebraic RHS path:\n{rendered}"
    );
}

#[test]
fn test_c_alg_rhs_projects_indexed_function_array_rhs_before_rendering() {
    let dae_json = serde_json::json!({
        "f_x": [{
            "lhs": {
                "VarRef": {
                    "name": {"name": "selector.expr"},
                    "subscripts": [{"Index": {"value": 1}}]
                }
            },
            "rhs": {
                "FunctionCall": {
                    "name": {"name": "linspace"},
                    "args": [
                        {"Literal": {"value": {"Integer": 0}}},
                        {"VarRef": {"name": {"name": "selector.n"}, "subscripts": []}},
                        {"Binary": {
                            "op": "Add",
                            "lhs": {"VarRef": {"name": {"name": "selector.n"}, "subscripts": []}},
                            "rhs": {"Literal": {"value": {"Integer": 1}}}
                        }}
                    ]
                }
            }
        }],
        "w": {
            "selector.expr[1]": {}
        },
        "x": {},
        "y": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
{{ alg_rhs_for_var_with_dae("selector.expr[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert_eq!(
        rendered.trim(),
        "0",
        "indexed scalar targets should project array-producing RHS expressions before rendering:\n{rendered}"
    );
}

#[test]
fn test_c_alg_rhs_prefers_direct_array_connection_over_rearranged_equation() {
    fn var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: name.into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    let indirect_motor_equation = sub(
        var("motor_1_omega_error"),
        sub(var("motor_1_omega_cmd"), var("motor_1_omega")),
    );
    let direct_array_connection = sub(
        rumoca_core::Expression::Array {
            elements: vec![
                var("motor_1_omega_cmd"),
                var("motor_2_omega_cmd"),
                var("motor_3_omega_cmd"),
                var("motor_4_omega_cmd"),
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
        var("plant_omega_cmd"),
    );
    let dae_json = serde_json::json!({
        "symbols": {
            "plant_omega_cmd[1]": "plant_omega_cmd_1"
        },
        "f_x": [
            {"rhs": serde_json::to_value(indirect_motor_equation).unwrap()},
            {"rhs": serde_json::to_value(direct_array_connection).unwrap()}
        ]
    });
    let template = r#"
{% set cfg = {"power": "powf", "subscript_underscore": true, "symbols": dae.symbols} %}
{{ alg_rhs_for_var("motor_1_omega_cmd", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert_eq!(rendered.trim(), "plant_omega_cmd_1");
}

#[test]
fn test_c_alg_rhs_prefers_direct_indexed_equation_for_array_element() {
    fn var(name: &str, subscripts: Vec<i64>) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: name.into(),
            subscripts: subscripts
                .into_iter()
                .map(|index| {
                    rumoca_core::Subscript::generated_index(index, rumoca_core::Span::DUMMY)
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    let earlier_reverse_array_alias = sub(
        rumoca_core::Expression::Array {
            elements: vec![
                var("motor_1_omega_cmd", vec![]),
                var("motor_2_omega_cmd", vec![]),
                var("motor_3_omega_cmd", vec![]),
                var("motor_4_omega_cmd", vec![]),
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
        var("plant_omega_cmd", vec![]),
    );
    let direct_indexed_equation = sub(var("plant_omega_cmd", vec![1]), var("motor_cmd", vec![1]));
    let dae_json = serde_json::json!({
        "symbols": {
            "plant_omega_cmd[1]": "plant_omega_cmd_1",
            "motor_cmd[1]": "motor_cmd_1"
        },
        "f_x": [
            {"rhs": serde_json::to_value(earlier_reverse_array_alias).unwrap()},
            {"rhs": serde_json::to_value(direct_indexed_equation).unwrap()}
        ]
    });
    let template = r#"
{% set cfg = {"power": "powf", "subscript_underscore": true, "symbols": dae.symbols} %}
{{ alg_rhs_for_var("plant_omega_cmd[1]", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert_eq!(rendered.trim(), "motor_cmd_1");
}

#[test]
fn test_c_alg_rhs_prefers_direct_conditional_equation_over_indirect_alias() {
    fn var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: name.into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn int(value: i64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn mul(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    let indirect_alias_equation = sub(var("fan_demand"), mul(var("fan_enable"), var("fan_gain")));
    let direct_enable_equation = sub(
        var("fan_enable"),
        rumoca_core::Expression::If {
            branches: vec![(var("unoccupied_mode"), int(1))],
            else_branch: Box::new(int(0)),
            span: rumoca_core::Span::DUMMY,
        },
    );
    let dae_json = serde_json::json!({
        "f_x": [
            {"rhs": serde_json::to_value(indirect_alias_equation).unwrap()},
            {"rhs": serde_json::to_value(direct_enable_equation).unwrap()}
        ]
    });
    let template = r#"
{% set cfg = {"power": "powf", "if_style": "ternary", "subscript_underscore": true} %}
{{ alg_rhs_for_var("fan_enable", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert_eq!(rendered.trim(), "(unoccupied_mode ? 1 : 0)");
    assert!(
        !rendered.contains("fan_demand"),
        "direct conditional equation should win over an earlier indirect alias:\n{rendered}"
    );
}

#[test]
fn test_c_alg_rhs_projects_whole_array_assignment_for_indexed_scalar_target() {
    fn var(name: &str, subscripts: Vec<i64>) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: name.into(),
            subscripts: subscripts
                .into_iter()
                .map(|index| {
                    rumoca_core::Subscript::generated_index(index, rumoca_core::Span::DUMMY)
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    let dae_json = serde_json::json!({
        "symbols": {
            "setpoint_u": "setpoint_u",
            "setpoint_u[1]": "setpoint_u_1",
            "setpoint_y": "setpoint_y"
        },
        "f_x": [
            {
                "lhs": serde_json::to_value(var("setpoint_y", vec![])).unwrap(),
                "rhs": serde_json::to_value(var("setpoint_u", vec![])).unwrap()
            }
        ]
    });
    let template = r#"
{% set cfg = {"power": "powf", "subscript_underscore": true, "symbols": dae.symbols} %}
{{ alg_rhs_for_var("setpoint_y[1]", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert_eq!(rendered.trim(), "setpoint_u_1");
    assert!(
        !rendered.trim().ends_with("setpoint_u"),
        "indexed scalar assignment targets must project whole-array RHS by index:\n{rendered}"
    );
}

#[test]
fn test_c_alg_rhs_keeps_structurally_indexed_component_rhs_scalar() {
    fn var(name: &str, subscripts: Vec<i64>) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: name.into(),
            subscripts: subscripts
                .into_iter()
                .map(|index| {
                    rumoca_core::Subscript::generated_index(index, rumoca_core::Span::DUMMY)
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    let dae_json = serde_json::json!({
        "symbols": {
            "plant.power[1]": "plant_power_1",
            "plant.module[1].power": "plant_module_1_power"
        },
        "f_x": [
            {
                "lhs": serde_json::to_value(var("plant.power", vec![1])).unwrap(),
                "rhs": serde_json::to_value(var("plant.module[1].power", vec![])).unwrap()
            }
        ]
    });
    let template = r#"
{% set cfg = {"power": "powf", "subscript_underscore": true, "symbols": dae.symbols} %}
{{ alg_rhs_for_var("plant.power[1]", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert_eq!(rendered.trim(), "plant_module_1_power");
    assert!(
        !rendered.contains("plant_module_1_power_1"),
        "structurally indexed component fields are scalar RHS values and must not be re-indexed:\n{rendered}"
    );
}

#[test]
fn test_render_expr_at_index_projects_structurally_indexed_array_field() {
    let dae_json = serde_json::json!({
        "symbols": {
            "coil.ele[1].x_start": "coil_ele_1_x_start",
            "coil.ele[1].x_start[1]": "coil_ele_1_x_start_1"
        }
    });
    let template = r#"
{% set cfg = {"power": "powf", "subscript_underscore": true, "symbols": dae.symbols} %}
{{ render_expr_at_index({"VarRef": {"name": "coil.ele[1].x_start", "subscripts": []}}, 1, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert_eq!(rendered.trim(), "coil_ele_1_x_start_1");
    assert!(
        !rendered.contains("coil_ele_1_x_start\n"),
        "structurally indexed array fields must still project their own field index:\n{rendered}"
    );
}

#[test]
fn test_discrete_rhs_keeps_guarded_when_ternary_for_non_sample_conditions() {
    let dae_json = serde_json::json!({
        "f_z": [{
            "lhs": "z",
            "rhs": {
                "If": {
                    "branches": [[
                        {"VarRef": {"name": "trigger", "subscripts": []}},
                        {"Literal": {"value": {"Real": 1.0}}}
                    ]],
                    "else_branch": {
                        "BuiltinCall": {
                            "function": "Pre",
                            "args": [{"VarRef": {"name": "z", "subscripts": []}}]
                        }
                    }
                }
            }
        }],
        "f_m": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "and_op": "&&", "or_op": "||", "not_op": "!", "true_val": "1", "false_val": "0", "if_style": "ternary", "subscript_underscore": true} %}
z={{ discrete_rhs_for_var("z", dae.f_z, dae.f_m, dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("z=(trigger ? 1.0 : pre(z))"),
        "ordinary guarded when RHS must render as C ternary, not unconditional update:\n{rendered}"
    );
}

#[test]
fn test_discrete_rhs_sample_guard_keeps_event_update_value() {
    let dae_json = serde_json::json!({
        "f_z": [{
            "lhs": "z",
            "rhs": {
                "If": {
                    "branches": [[
                        {
                            "BuiltinCall": {
                                "function": "Sample",
                                "args": [{"VarRef": {"name": "clocked", "subscripts": []}}]
                            }
                        },
                        {"Literal": {"value": {"Real": 1.0}}}
                    ]],
                    "else_branch": {
                        "BuiltinCall": {
                            "function": "Pre",
                            "args": [{"VarRef": {"name": "z", "subscripts": []}}]
                        }
                    }
                }
            }
        }],
        "f_m": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "and_op": "&&", "or_op": "||", "not_op": "!", "true_val": "1", "false_val": "0", "if_style": "ternary", "subscript_underscore": true} %}
z={{ discrete_rhs_for_var("z", dae.f_z, dae.f_m, dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert_eq!(rendered.trim(), "z=1.0");
}

#[test]
fn test_c_ode_rhs_solves_preserved_matrix_vector_derivative_equation() {
    let residual = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: "J".into(),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args: vec![rumoca_core::Expression::VarRef {
                    name: "omega".into(),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::VarRef {
            name: "M_body".into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    let residual_value = serde_json::to_value(residual).unwrap();
    let dae_json = serde_json::json!({
        "symbols": {
            "J[1,1]": "J_1_1",
            "J[1,2]": "J_1_2",
            "J[1,3]": "J_1_3",
            "J[2,1]": "J_2_1",
            "J[2,2]": "J_2_2",
            "J[2,3]": "J_2_3",
            "J[3,1]": "J_3_1",
            "J[3,2]": "J_3_2",
            "J[3,3]": "J_3_3",
            "M_body[1]": "M_body_1",
            "M_body[2]": "M_body_2",
            "M_body[3]": "M_body_3"
        },
        "f_x": [
            {
                "rhs": residual_value,
                "scalar_count": 3
            }
        ]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true, "symbols": dae.symbols} %}
{{ ode_rhs_for_state("omega[2]", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("__rumoca_solve_linear_component((double[]){J_1_1, J_1_2, J_1_3, J_2_1, J_2_2, J_2_3, J_3_1, J_3_2, J_3_3}, (double[]){M_body_1, M_body_2, M_body_3}, 3, 1)"),
        "expected a generated dense linear solve for the vector derivative equation, got:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no ODE equation found"),
        "matrix-vector derivative equation should not fall back to a zero derivative:\n{rendered}"
    );
}

#[test]
fn test_c_ode_rhs_solves_scalarized_coupled_derivative_rows() {
    fn var(name: &str, subscripts: Vec<i64>) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: name.into(),
            subscripts: subscripts
                .into_iter()
                .map(|index| {
                    rumoca_core::Subscript::generated_index(index, rumoca_core::Span::DUMMY)
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn der_omega(index: i64) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![var("omega", vec![index])],
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn mul(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn add(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }
    fn row(row: i64) -> rumoca_core::Expression {
        sub(
            add(
                add(
                    mul(var("J", vec![row, 1]), der_omega(1)),
                    mul(var("J", vec![row, 2]), der_omega(2)),
                ),
                mul(var("J", vec![row, 3]), der_omega(3)),
            ),
            var("M_body", vec![row]),
        )
    }

    let row_1 = serde_json::to_value(row(1)).unwrap();
    let row_2 = serde_json::to_value(row(2)).unwrap();
    let row_3 = serde_json::to_value(row(3)).unwrap();
    let dae_json = serde_json::json!({
        "symbols": {
            "J[1,1]": "J_1_1",
            "J[1,2]": "J_1_2",
            "J[1,3]": "J_1_3",
            "J[2,1]": "J_2_1",
            "J[2,2]": "J_2_2",
            "J[2,3]": "J_2_3",
            "J[3,1]": "J_3_1",
            "J[3,2]": "J_3_2",
            "J[3,3]": "J_3_3",
            "M_body[1]": "M_body_1",
            "M_body[2]": "M_body_2",
            "M_body[3]": "M_body_3",
            "omega[1]": "omega_1",
            "omega[2]": "omega_2",
            "omega[3]": "omega_3"
        },
        "f_x": [
            {"rhs": row_1, "scalar_count": 1},
            {"rhs": row_2, "scalar_count": 1},
            {"rhs": row_3, "scalar_count": 1}
        ]
    });
    let template = r#"
{% set cfg = {"power": "pow", "subscript_underscore": true, "symbols": dae.symbols} %}
{{ ode_rhs_for_state("omega[3]", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("__rumoca_solve_linear_component((double[]){J_1_1, J_1_2, J_1_3, J_2_1, J_2_2, J_2_3, J_3_1, J_3_2, J_3_3}, (double[]){M_body_1, M_body_2, M_body_3}, 3, 2)"),
        "expected scalarized derivative rows to become one dense solve, got:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no ODE equation found"),
        "coupled scalar derivative rows should not fall back to a zero derivative:\n{rendered}"
    );
}

#[test]
fn test_mul_elem_rendering_can_use_backend_function() {
    let lhs = rumoca_core::Expression::VarRef {
        name: "a".into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    };
    let rhs = rumoca_core::Expression::VarRef {
        name: "b".into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    };
    let mul_expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: Box::new(lhs.clone()),
        rhs: Box::new(rhs.clone()),
        span: rumoca_core::Span::DUMMY,
    };
    let mul_elem_expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::MulElem,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    };

    let cfg = ExprConfig {
        mul_elem_fn: Some("ca.times".to_string()),
        ..ExprConfig::default()
    };

    let mul_rendered = render_expression(&Value::from_serialize(&mul_expr), &cfg).unwrap();
    let mul_elem_rendered =
        render_expression(&Value::from_serialize(&mul_elem_expr), &cfg).unwrap();

    assert_eq!(mul_rendered, "(a * b)");
    assert_eq!(mul_elem_rendered, "ca.times(a, b)");
}

#[test]
fn test_render_array_comprehension_expression() {
    let expr = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(rumoca_core::Expression::VarRef {
            name: "i".into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                }),
                step: None,
                end: Box::new(rumoca_core::Expression::VarRef {
                    name: "n".into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: Some(Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: "i".into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        })),
        span: rumoca_core::Span::DUMMY,
    };

    let rendered =
        render_expression(&Value::from_serialize(&expr), &ExprConfig::default()).unwrap();
    assert_eq!(rendered, "{i for i in 1:n if (i > 0)}");
}

#[test]
fn test_render_integer_builtin_truncates_for_c_targets() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Integer,
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(-1.5),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };
    let cfg = ExprConfig {
        if_style: IfStyle::Ternary,
        ..ExprConfig::default()
    };

    let rendered = render_expression(&Value::from_serialize(&expr), &cfg).unwrap();
    assert_eq!(rendered, "trunc(-1.5)");
}

#[test]
fn test_c_array_comprehension_unroll_substitutes_only_var_refs() {
    let expr = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: "i".into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: "signal".into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                }),
                step: None,
                end: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };
    let cfg = ExprConfig {
        if_style: IfStyle::Ternary,
        ..ExprConfig::default()
    };

    let rendered = render_expression(&Value::from_serialize(&expr), &cfg).unwrap();
    assert_eq!(rendered, "[(1 + signal), (2 + signal)]");
}

#[test]
fn test_product_filter() {
    let dae = dae::Dae::new();
    let template = "{{ [3, 4] | product }}";
    let result = render_template(&dae, template).unwrap();
    assert_eq!(result, "12");
}

#[test]
fn test_product_filter_single() {
    let dae = dae::Dae::new();
    let template = "{{ [5] | product }}";
    let result = render_template(&dae, template).unwrap();
    assert_eq!(result, "5");
}

#[test]
fn test_product_filter_empty() {
    let dae = dae::Dae::new();
    let template = "{{ [] | product }}";
    let result = render_template(&dae, template).unwrap();
    assert_eq!(result, "1");
}

#[test]
fn test_product_filter_rejects_overflow() {
    let dae = dae::Dae::new();
    let template = "{{ [9223372036854775807, 2] | product }}";
    let err = render_template(&dae, template)
        .expect_err("overflowing product filter should fail rendering");

    assert!(
        err.to_string()
            .contains("product filter overflows Modelica integer range"),
        "{err:?}"
    );
}

#[test]
fn test_casadi_mx_template_empty_dae() {
    let dae = dae::Dae::new();
    let result =
        render_template(&dae, builtin_template("casadi-mx", "casadi_mx.py.jinja")).unwrap();
    assert!(result.contains("import casadi as ca"));
    assert!(result.contains("def create_model()"));
    assert!(result.contains("n_x = 0"));
    assert!(result.contains("n_z = 0"));
    assert!(result.contains("dae_fn = ca.Function"));
}

#[test]
fn test_casadi_mx_template_flattens_array_start_values_for_x0() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            dims: vec![2],
            start: Some(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(2.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae.variables.states.insert(
        "y".into(),
        rumoca_ir_dae::Variable {
            name: "y".into(),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(3.0),
                span: rumoca_core::Span::DUMMY,
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let result = normalize_newlines(
        &render_template(&dae, builtin_template("casadi-mx", "casadi_mx.py.jinja")).unwrap(),
    );
    assert!(result.contains("def _flat_start(value, expected_size, var_name):"));
    assert!(result.contains("x0 = np.concatenate(_x0_parts) if _x0_parts else np.array([])"));
    assert!(result.contains("p0 = np.concatenate(_p0_parts) if _p0_parts else np.array([])"));
    assert!(result.contains("np.repeat(arr, expected_size)"));
    assert!(result.contains("Start value size mismatch for"));
    assert!(result.contains("2,\n        'x'"));
    assert!(result.contains("1,\n        'y'"));
    assert!(!result.contains("x0 = np.array(["));
    assert!(!result.contains("p0 = np.array(["));
}

#[test]
fn test_casadi_sx_template_uses_scalar_counts_and_defines_derivatives() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            dims: vec![3],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae.variables.algebraics.insert(
        "z".into(),
        rumoca_ir_dae::Variable {
            name: "z".into(),
            dims: vec![2],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae.variables.inputs.insert(
        "u".into(),
        rumoca_ir_dae::Variable {
            name: "u".into(),
            dims: vec![4],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae.variables.parameters.insert(
        "p".into(),
        rumoca_ir_dae::Variable {
            name: "p".into(),
            dims: vec![5],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let result =
        render_template(&dae, builtin_template("casadi-sx", "casadi_sx.py.jinja")).unwrap();
    assert!(result.contains("n_x = 3"));
    assert!(result.contains("n_z = 2"));
    assert!(result.contains("n_u = 4"));
    assert!(result.contains("n_p = 5"));
    assert!(result.contains("def der(v):"));
    assert!(result.contains("xdot = _xdot"));
    assert!(result.contains("g = f_x"));
    assert!(result.contains("'n_x': n_x"));
    assert!(result.contains("'n_z': n_z"));
    assert!(result.contains("'n_u': n_u"));
    assert!(result.contains("'n_p': n_p"));
}

#[test]
fn test_fmi3_model_description_uses_fmi3_schema_order() {
    let mut dae = dae::Dae::new();
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2);
    dae.variables.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            ..rumoca_ir_dae::Variable::empty_with_span(span)
        },
    );
    dae.variables.outputs.insert(
        "y".into(),
        rumoca_ir_dae::Variable {
            name: "y".into(),
            unit: Some("m/s".into()),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span,
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(span)
        },
    );

    let xml = render_template_with_name(
        &dae,
        builtin_template("fmi3", "modelDescription.xml.jinja"),
        "M",
    )
    .expect("render FMI3 modelDescription");
    let model_variables = xml
        .find("<ModelVariables>")
        .expect("FMI3 XML should contain ModelVariables");
    let model_structure = xml
        .find("<ModelStructure>")
        .expect("FMI3 XML should contain ModelStructure");

    let unit_definitions = xml
        .find("<UnitDefinitions>")
        .expect("FMI3 XML should contain UnitDefinitions");
    assert!(
        unit_definitions < model_variables && model_variables < model_structure,
        "{xml}"
    );
    assert!(
        xml.contains(r#"<Unit name="s"><BaseUnit s="1"/></Unit>"#),
        "{xml}"
    );
    assert!(xml.contains(r#"<Unit name="m/s"></Unit>"#), "{xml}");
    assert!(xml.contains(r#"<Float64 name="y" valueReference="2" causality="output" variability="continuous" initial="calculated" unit="m/s"/>"#), "{xml}");
    assert!(!xml.contains(r#"initial="calculated" start="#), "{xml}");
    assert!(xml.contains(r#"<Float64 name="time" valueReference="3" causality="independent" variability="continuous" unit="s"/>"#), "{xml}");
    assert!(
        xml.contains(r#"<InitialUnknown valueReference="1"/>"#),
        "{xml}"
    );
    assert!(
        xml.contains(r#"<InitialUnknown valueReference="2"/>"#),
        "{xml}"
    );
    assert!(
        !xml.contains("<BuildConfiguration"),
        "FMI 3 build configuration belongs in sources/buildDescription.xml:\n{xml}"
    );
    assert!(
        !xml.contains("<Terminals"),
        "FMI 3 terminals belong in terminalsAndIcons/terminalsAndIcons.xml, not modelDescription.xml:\n{xml}"
    );
}

#[test]
fn test_fmi3_build_description_declares_source_file_set() {
    let dae = dae::Dae::new();

    let xml = render_template_with_name(
        &dae,
        builtin_template("fmi3", "buildDescription.xml.jinja"),
        "M",
    )
    .expect("render FMI3 buildDescription");

    assert!(xml.contains(r#"<fmiBuildDescription fmiVersion="3.0">"#));
    assert!(xml.contains(r#"<BuildConfiguration modelIdentifier="M">"#));
    assert!(xml.contains(r#"<SourceFileSet language="C99">"#));
    assert!(xml.contains(r#"<SourceFile name="M.c"/>"#));
}

#[test]
fn test_fmi3_model_description_exports_dae_inputs_as_inputs() {
    let mut dae = dae::Dae::new();
    dae.variables.inputs.insert(
        "u".into(),
        rumoca_ir_dae::Variable {
            name: "u".into(),
            dims: vec![2],
            start: Some(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(2.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let xml = render_template_with_name(
        &dae,
        builtin_template("fmi3", "modelDescription.xml.jinja"),
        "M",
    )
    .expect("render FMI3 modelDescription");
    assert!(
        xml.contains(r#"<Float64 name="u" valueReference="0" causality="input" variability="continuous" start="1.0 2.0">"#),
        "{xml}"
    );
}

#[test]
fn test_fmi_model_description_escapes_expression_attributes() {
    let less_than_expression = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(2.0),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    let mut dae = dae::Dae::new();
    dae.variables.inputs.insert(
        "u".into(),
        rumoca_ir_dae::Variable {
            name: "u".into(),
            min: Some(less_than_expression),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let fmi2_xml = render_template_with_name(
        &dae,
        builtin_template("fmi2", "modelDescription.xml.jinja"),
        "M",
    )
    .expect("render FMI2 modelDescription");
    let fmi3_xml = render_template_with_name(
        &dae,
        builtin_template("fmi3", "modelDescription.xml.jinja"),
        "M",
    )
    .expect("render FMI3 modelDescription");

    for xml in [fmi2_xml, fmi3_xml] {
        assert!(
            xml.contains(r#"min="(1.0 &lt; 2.0)""#),
            "modelDescription expression attributes must be XML escaped:\n{xml}"
        );
        assert!(
            !xml.contains(r#"min="(1.0 < 2.0)""#),
            "modelDescription must not emit raw '<' inside attributes:\n{xml}"
        );
    }
}

#[test]
fn test_fmi_model_description_renders_string_start_without_c_quotes() {
    let mut dae = dae::Dae::new();
    dae.variables.parameters.insert(
        "metadata.provenance".into(),
        rumoca_ir_dae::Variable {
            name: "metadata.provenance".into(),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("metadata_ready_state".into()),
                span: rumoca_core::Span::DUMMY,
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let fmi2_xml = render_template_with_name(
        &dae,
        builtin_template("fmi2", "modelDescription.xml.jinja"),
        "M",
    )
    .expect("render FMI2 modelDescription");
    let fmi3_xml = render_template_with_name(
        &dae,
        builtin_template("fmi3", "modelDescription.xml.jinja"),
        "M",
    )
    .expect("render FMI3 modelDescription");

    for xml in [fmi2_xml, fmi3_xml] {
        assert!(
            xml.contains(r#"start="metadata_ready_state""#),
            "string start values must render as XML attribute values:\n{xml}"
        );
        assert!(
            !xml.contains(r#"start="&quot;metadata_ready_state&quot;""#),
            "string starts must not keep C string quotes in XML attributes:\n{xml}"
        );
    }
}

#[test]
fn test_fmi3_build_templates_use_fmi3_platform_directory_names() {
    let cmake = builtin_template("fmi3", "CMakeLists.txt.jinja");
    let shell = builtin_template("fmi3", "build.sh.jinja");
    assert!(
        cmake.contains(r#"set(FMU_PLATFORM "${FMU_ARCH}-linux")"#)
            && cmake.contains(r#"set(FMU_ARCH "aarch64")"#)
            && cmake.contains(r#"set(FMU_ARCH "x86_64")"#),
        "FMI 3 CMake packaging must select a standard platform tuple"
    );
    assert!(
        shell.contains("PLATFORM=x86_64-linux") && shell.contains("PLATFORM=aarch64-linux"),
        "FMI 3 shell packaging must support x86_64 and aarch64 Linux"
    );
    assert!(!cmake.contains("linux64") && !shell.contains("linux64"));
    assert!(cmake.contains("DESTINATION binaries/${FMU_PLATFORM}"));
    assert!(shell.contains("rm -f {{ model_name }}.fmu"));
}

#[test]
fn test_fmi3_model_description_only_advertises_implemented_capabilities() {
    let mut dae = dae::Dae::new();
    dae.variables.parameters.insert(
        "gain".into(),
        rumoca_ir_dae::Variable {
            name: "gain".into(),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    let xml = render_template_with_name(
        &dae,
        builtin_template("fmi3", "modelDescription.xml.jinja"),
        "Capabilities",
    )
    .unwrap();

    assert!(!xml.contains("providesDirectionalDerivatives"), "{xml}");
    assert!(!xml.contains("providesAdjointDerivatives"), "{xml}");
    assert!(
        xml.contains(r#"name="gain" valueReference="0" causality="parameter" variability="fixed""#),
        "{xml}"
    );
    assert!(!xml.contains("structuralParameter"), "{xml}");
}

#[test]
fn test_fmi_build_scripts_package_only_needed_external_libraries() {
    for target in ["fmi2", "fmi3"] {
        let script = builtin_template(target, "build.sh.jinja");
        assert!(
            script.contains("UNRESOLVED_SYMBOLS_FILE")
                && script.contains("external_library_declares_unresolved_symbol")
                && script.contains("external_library_exports_unresolved_symbol"),
            "{target} shell build should inspect unresolved symbols before linking external libraries"
        );
        assert!(
            script.contains("EXTERNAL_LIBS_NEEDED=0")
                && script.contains(r#"cp "$external_lib_file" "binaries/$PLATFORM/$(basename "$external_lib_file")""#),
            "{target} shell build should copy only external libraries needed by the FMU binary"
        );
        assert!(
            script.contains("RUNTIME_PATH_FLAGS")
                && script.contains("rewrite_darwin_runtime_paths"),
            "{target} shell build should make copied runtime libraries loader-relative"
        );
        assert!(
            !script.contains("EXTERNAL_LIB_ARGS=\"$EXTERNAL_LIB_ARGS"),
            "{target} shell build should not accumulate every declared external library unconditionally"
        );
    }
}

#[test]
fn test_fmi_external_include_directories_resolve_modelica_uris() {
    let root =
        std::env::temp_dir().join(format!("rumoca-modelica-uri-test-{}", std::process::id()));
    let include_dir = root.join("Buildings").join("Resources").join("Include");
    std::fs::create_dir_all(&include_dir).expect("create temporary Modelica include dir");

    let resolved = resolve_modelica_uri_with_roots(
        "modelica://Buildings/Resources/Include",
        std::iter::once(root.as_path()),
    );
    assert_eq!(resolved, include_dir.to_string_lossy());
    for target in ["fmi2", "fmi3"] {
        assert!(
            builtin_template(target, "externalIncludeDirectories.txt.jinja")
                .contains("resolve_modelica_uri(directory)"),
            "{target} external include directories template should resolve modelica:// URIs"
        );
    }

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn test_fmi3_initial_builtin_tracks_initialization_mode() {
    assert!(
        builtin_template("fmi3", "model.c.jinja").contains("modelInitializationMode"),
        "FMI 3 generated C must evaluate initial() from the FMI initialization state"
    );
    assert!(
        !builtin_template("fmi3", "model.c.jinja").contains("#define initial() 0"),
        "MLS initial() cannot be hard-coded false in FMI 3 initialization"
    );
}

#[test]
fn test_fmi2_initial_builtin_tracks_initialization_mode() {
    assert!(
        builtin_template("fmi2", "model.c.jinja").contains("modelInitializationMode"),
        "FMI 2 generated C must evaluate initial() from the FMI initialization state"
    );
    assert!(
        !builtin_template("fmi2", "model.c.jinja").contains("#define initial() 0"),
        "MLS initial() cannot be hard-coded false in FMI 2 initialization"
    );
}

#[test]
fn test_fmi3_exit_initialization_seeds_pre_discrete_values() {
    let template = builtin_template("fmi3", "model.c.jinja");
    let exit_initialization = template
        .split("FMI3_Export fmi3Status fmi3ExitInitializationMode")
        .nth(1)
        .expect("FMI 3 template should define exit initialization");

    assert!(
        exit_initialization.contains("memcpy(m->pre_z, m->z, sizeof(m->z));"),
        "FMI 3 exit initialization should seed previous discrete Real slots"
    );
    assert!(
        exit_initialization.contains("memcpy(m->pre_m, m->m, sizeof(m->m));"),
        "FMI 3 exit initialization should seed previous discrete-valued slots"
    );
    assert!(
        exit_initialization.contains("compute_discrete_updates(m);"),
        "FMI 3 exit initialization should evaluate initial discrete updates"
    );
}

#[test]
fn test_fmi3_cosimulation_uses_opt_in_fixed_rk4() {
    let template = builtin_template("fmi3", "model.c.jinja");
    assert!(template.contains("static fmi3Status rk4_integrate"));
    assert!(template.contains("#ifdef RUMOCA_FMI3_COSIM_FIXED_RK4"));
    assert!(template.contains("rk4_integrate(m, currentCommunicationPoint, t_end)"));
    assert!(template.contains("rk45_integrate(m, currentCommunicationPoint, t_end)"));
    assert!(template.contains("#define RUMOCA_FMI3_COSIM_RK4_MAX_STEP INFINITY"));
    assert!(
        !template.contains("#define RUMOCA_FMI3_COSIM_RK4_MAX_STEP 4.0e-3"),
        "the generic backend must not embed a vehicle-specific RK4 step"
    );
    assert!(
        builtin_template("fmi3", "build.sh.jinja").contains("${CFLAGS:=-O2}"),
        "the packaged FMU build should allow a caller-selected integration policy"
    );
}

#[test]
fn test_fmi3_uses_official_fmi_3_0_2_headers() {
    let model = builtin_template("fmi3", "model.c.jinja");
    let driver = builtin_template("fmi3", "test_driver.c.jinja");
    let platform = builtin_template("fmi3", "fmi3PlatformTypes.h.jinja");
    let function_types = builtin_template("fmi3", "fmi3FunctionTypes.h.jinja");
    let manifest = crate::templates::builtin_target("fmi3")
        .expect("builtin FMI 3 target")
        .manifest;

    assert!(model.contains("#include \"fmi3Functions.h\""));
    assert!(driver.contains("#include \"fmi3Functions.h\""));
    assert!(!model.contains("typedef int          fmi3Boolean;"));
    assert!(platform.contains("typedef            bool fmi3Boolean;"));
    assert!(platform.contains("typedef const fmi3Byte* fmi3Binary;"));
    assert!(!function_types.contains("clocksTicked"));
    for header in [
        "fmi3PlatformTypes.h.jinja",
        "fmi3FunctionTypes.h.jinja",
        "fmi3Functions.h.jinja",
    ] {
        assert!(manifest.contains(header), "FMI 3 target omits {header}");
    }
}

#[test]
fn test_fmi3_exports_complete_standard_symbol_surface() {
    let generated = render_template_with_name(
        &dae::Dae::new(),
        builtin_template("fmi3", "model.c.jinja"),
        "CompleteApi",
    )
    .expect("render FMI 3 API");
    let functions = builtin_template("fmi3", "fmi3Functions.h.jinja");

    for line in functions
        .lines()
        .map(str::trim)
        .filter(|line| line.starts_with("FMI3_Export "))
    {
        let symbol = line
            .strip_suffix(';')
            .and_then(|line| line.split_whitespace().last())
            .expect("official FMI function declaration");
        assert!(
            generated.contains(&format!("{symbol}(")),
            "generated FMI library omits {symbol}"
        );
    }
}

#[test]
fn test_fmi3_model_description_only_advertises_cosim_for_eventless_models() {
    let template = builtin_template("fmi3", "modelDescription.xml.jinja");
    let eventless = dae::Dae::new();
    let eventless_xml = render_template_with_name(&eventless, template, "Eventless").unwrap();
    assert!(eventless_xml.contains("<CoSimulation"), "{eventless_xml}");
    assert!(!eventless_xml.contains("canReturnEarlyAfterIntermediateUpdate"));
    let c_template = builtin_template("fmi3", "model.c.jinja");
    let eventless_c = render_template_with_name(&eventless, c_template, "Eventless").unwrap();
    assert!(
        eventless_c.contains("#define RUMOCA_COSIM_SUPPORTED 1"),
        "{eventless_c}"
    );

    let mut discrete = dae::Dae::new();
    discrete.variables.discrete_reals.insert(
        "z".into(),
        dae::Variable {
            name: "z".into(),
            ..dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    let discrete_xml = render_template_with_name(&discrete, template, "Discrete").unwrap();
    assert!(!discrete_xml.contains("<CoSimulation"), "{discrete_xml}");
    assert!(discrete_xml.contains("<ModelExchange"), "{discrete_xml}");
    let discrete_c = render_template_with_name(&discrete, c_template, "Discrete").unwrap();
    assert!(
        discrete_c.contains("#define RUMOCA_COSIM_SUPPORTED 0"),
        "{discrete_c}"
    );
}

fn fmi3_decay_fixture() -> serde_json::Value {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi3_decay_fixture.mo"),
        1,
        2,
    );
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        "x".into(),
        dae::Variable {
            name: "x".into(),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            }),
            ..dae::Variable::empty_with_span(span)
        },
    );
    model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args: vec![rumoca_core::Expression::VarRef {
                    name: "x".into(),
                    subscripts: Vec::new(),
                    span,
                }],
                span,
            }),
            rhs: Box::new(rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Box::new(rumoca_core::Expression::VarRef {
                    name: "x".into(),
                    subscripts: Vec::new(),
                    span,
                }),
                span,
            }),
            span,
        },
        span,
        origin: "test".into(),
        scalar_count: 1,
    });
    let mut json = dae_template_json(&model).unwrap();
    json.as_object_mut().unwrap().insert(
        "solve".to_string(),
        serde_json::json!({
            "continuous": {
                "derivative_rhs": {
                    "programs": [[
                        {"LoadY": {"dst": 0, "index": 0}},
                        {"Unary": {"dst": 1, "op": "Neg", "arg": 0}},
                        {"StoreOutput": {"src": 1}}
                    ]],
                    "output_indices": [0]
                }
            },
            "events": {
                "root_conditions": {"programs": []},
                "dynamic_time_event_rhs": {"programs": []},
                "actions": []
            }
        }),
    );
    json
}

fn compile_and_run_fmi3_cosim(
    source_path: &std::path::Path,
    dir: &std::path::Path,
    name: &str,
    extra_flags: &[&str],
) {
    let binary = dir.join(name);
    let output = std::process::Command::new("cc")
        .args(["-std=c11", "-O2", "-Wall", "-Wextra"])
        .args(extra_flags)
        .arg(source_path)
        .args(["-lm", "-o"])
        .arg(&binary)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{name} FMI3 C compile failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let output = std::process::Command::new(&binary).output().unwrap();
    assert!(
        output.status.success(),
        "{name} FMI3 Co-Simulation runtime exited {:?}\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn write_fmi3_headers(dir: &std::path::Path) {
    for (template, output) in [
        ("fmi3PlatformTypes.h.jinja", "fmi3PlatformTypes.h"),
        ("fmi3FunctionTypes.h.jinja", "fmi3FunctionTypes.h"),
        ("fmi3Functions.h.jinja", "fmi3Functions.h"),
    ] {
        std::fs::write(dir.join(output), builtin_template("fmi3", template)).unwrap();
    }
}

#[test]
fn test_fmi3_cosimulation_runtime_and_state_roundtrip() {
    if std::process::Command::new("cc")
        .arg("--version")
        .output()
        .is_err()
    {
        eprintln!("skipping FMI3 Co-Simulation runtime test: cc not available");
        return;
    }
    let generated = render_template_with_dae_json_and_name(
        &fmi3_decay_fixture(),
        builtin_template("fmi3", "model.c.jinja"),
        "Decay",
    )
    .unwrap();
    let driver = r#"
int main(void) {
    fmi3Instance rejected = fmi3InstantiateCoSimulation(
        "early", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, 0, 1, NULL, 0, NULL, NULL, NULL);
    if (rejected != NULL) return 10;
    rejected = fmi3InstantiateCoSimulation(
        "events", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, 1, 0, NULL, 0, NULL, NULL, NULL);
    if (rejected != NULL) return 33;

    fmi3Instance instance = fmi3InstantiateCoSimulation(
        "decay", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, 0, 0, NULL, 0, NULL, NULL, NULL);
    if (!instance) return 11;
    if (fmi3EnterConfigurationMode(NULL) != fmi3Error) return 36;
    if (fmi3EnterConfigurationMode(instance) != fmi3OK) return 37;
    if (fmi3EnterConfigurationMode(instance) != fmi3Error) return 38;
    if (fmi3ExitConfigurationMode(instance) != fmi3OK) return 39;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-8, 0.0, 0, 0.0) != fmi3OK) return 12;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 13;

    const fmi3ValueReference x_vr = 0;
    fmi3Float64 x = 0.0;
    if (fmi3GetFloat64(instance, &x_vr, 1, &x, 1) != fmi3OK || fabs(x - 1.0) > 1.0e-12) return 14;
    fmi3Float64 guards[2] = {123.0, 456.0};
    if (fmi3GetFloat64(instance, &x_vr, 1, guards, 0) != fmi3Error ||
        guards[0] != 123.0 || guards[1] != 456.0) return 40;
    if (fmi3GetFloat64(instance, &x_vr, 1, guards, 2) != fmi3Error ||
        guards[0] != 123.0 || guards[1] != 456.0) return 41;
    if (fmi3GetFloat64(NULL, &x_vr, 1, &x, 1) != fmi3Error) return 42;
    if (fmi3SetFloat64(instance, &x_vr, 1, guards, 2) != fmi3Error) return 43;
    if (fmi3GetFloat64(instance, &x_vr, 1, &x, 1) != fmi3OK || fabs(x - 1.0) > 1.0e-12) return 44;
    fmi3Float32 x32 = 0.0f;
    if (fmi3GetFloat32(instance, &x_vr, 1, &x32, 1) != fmi3Error) return 45;
    if (fmi3GetString(instance, &x_vr, 1, NULL, 1) != fmi3Error) return 46;

    fmi3Boolean event_needed = 0, terminate = 0, early = 0;
    fmi3Float64 last = 0.0;
    if (fmi3DoStep(instance, 0.0, 0.1, 1, &event_needed, &terminate, &early, &last) != fmi3OK) return 15;
    if (event_needed || terminate || early || fabs(last - 0.1) > 1.0e-12) return 16;
    if (fmi3GetFloat64(instance, &x_vr, 1, &x, 1) != fmi3OK || fabs(x - exp(-0.1)) > 2.0e-7) return 17;
    const fmi3ValueReference time_vr = VR_TIME;
    fmi3Float64 reported_time = 0.0;
    if (fmi3GetFloat64(instance, &time_vr, 1, &reported_time, 1) != fmi3OK ||
        fabs(reported_time - 0.1) > 1.0e-12) return 35;

    fmi3FMUState state = NULL;
    if (fmi3GetFMUState(instance, &state) != fmi3OK || !state) return 18;
    size_t state_size = 0;
    if (fmi3SerializedFMUStateSize(instance, state, &state_size) != fmi3OK || state_size == 0) return 19;
    fmi3Byte* bytes = (fmi3Byte*)malloc(state_size);
    if (!bytes || fmi3SerializeFMUState(instance, state, bytes, state_size) != fmi3OK) return 20;
    if (fmi3SerializeFMUState(instance, state, bytes, state_size + 1) != fmi3Error) return 34;
    if (fmi3FreeFMUState(instance, &state) != fmi3OK || state != NULL) return 21;

    if (fmi3DoStep(instance, 0.1, 0.1, 1, &event_needed, &terminate, &early, &last) != fmi3OK) return 22;
    fmi3Float64 advanced = 0.0;
    if (fmi3GetFloat64(instance, &x_vr, 1, &advanced, 1) != fmi3OK) return 23;
    if (fmi3DeserializeFMUState(instance, bytes, state_size, &state) != fmi3OK || !state) return 24;
    free(bytes);
    if (fmi3SetFMUState(instance, state) != fmi3OK) return 25;
    if (fmi3FreeFMUState(instance, &state) != fmi3OK) return 26;
    if (fmi3GetFloat64(instance, &x_vr, 1, &x, 1) != fmi3OK || fabs(x - exp(-0.1)) > 2.0e-7) return 27;
    if (!(advanced < x)) return 28;

    if (fmi3DoStep(instance, 0.0, 0.1, 1, &event_needed, &terminate, &early, &last) != fmi3Error) return 29;
    if (fabs(last - 0.1) > 1.0e-12) return 30;
    if (fmi3DoStep(instance, 0.1, 0.1, 1, &event_needed, &terminate, &early, &last) != fmi3OK) return 31;

    if (fmi3Terminate(instance) != fmi3OK) return 32;
    fmi3FreeInstance(instance);
    return 0;
}
"#;
    let source = format!("{generated}\n{driver}");
    let dir = std::env::temp_dir().join(format!("rumoca_fmi3_cosim_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    write_fmi3_headers(&dir);
    let source_path = dir.join("decay.c");
    std::fs::write(&source_path, source).unwrap();

    for (name, extra_flags) in [
        ("adaptive", Vec::<&str>::new()),
        ("fixed-default", vec!["-DRUMOCA_FMI3_COSIM_FIXED_RK4"]),
        (
            "fixed",
            vec![
                "-DRUMOCA_FMI3_COSIM_FIXED_RK4",
                "-DRUMOCA_FMI3_COSIM_RK4_MAX_STEP=1.0e-3",
            ],
        ),
    ] {
        compile_and_run_fmi3_cosim(&source_path, &dir, name, &extra_flags);
    }
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_render_dae_equation_via_template() {
    // Test render_equation function via template with a simple DAE
    // that has residual equations (the common case from todae)
    let dae = dae::Dae::new();

    // Test with an empty DAE - just verify the template compiles
    let tmpl = builtin_template("dae-modelica", "dae_modelica.mo.jinja");
    let result = render_template_with_name(&dae, tmpl, "TestModel").unwrap();
    assert!(result.contains("class TestModel"));
    assert!(result.contains("equation"));
    assert!(result.contains("end TestModel"));
}

#[test]
fn test_dae_template_includes_model_description() {
    // Test that DAE template includes model description when present
    let mut dae = dae::Dae::new();
    dae.metadata.model_description = Some("Test model description".to_string());

    // Render template
    let tmpl = builtin_template("dae-modelica", "dae_modelica.mo.jinja");
    let result = render_template_with_name(&dae, tmpl, "TestModel").unwrap();
    assert!(result.contains(r#"class TestModel "Test model description""#));
}

#[test]
fn test_render_flat_equation_via_template() {
    // Test render_flat_equation function via template with an empty Model
    let flat = flat::Model::new();

    let tmpl = builtin_template("flat-modelica", "flat_modelica.mo.jinja");
    let result = render_flat_template_with_name(&flat, tmpl, "TestModel").unwrap();
    assert!(result.contains("class TestModel"));
    assert!(result.contains("equation"));
    assert!(result.contains("end TestModel"));
}

#[test]
fn test_flat_template_uses_parameter_start_as_default_binding() {
    let mut flat = flat::Model::new();
    let mut var = rumoca_ir_flat::Variable {
        name: "T".into(),
        variability: rumoca_core::Variability::Parameter(Default::default()),
        start: Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        }),
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };
    var.fixed = None; // Parameter default: fixed=true
    flat.add_variable("T".into(), var);

    let rendered = render_flat_template_with_name(
        &flat,
        builtin_template("flat-modelica", "flat_modelica.mo.jinja"),
        "M",
    )
    .unwrap();
    assert!(
        rendered.contains("parameter Real T(start = 1) = 1;"),
        "{rendered}"
    );
}

#[test]
fn test_flat_template_does_not_materialize_start_binding_when_fixed_false() {
    let mut flat = flat::Model::new();
    let var = rumoca_ir_flat::Variable {
        name: "p".into(),
        variability: rumoca_core::Variability::Parameter(Default::default()),
        start: Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        }),
        fixed: Some(false),
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };
    flat.add_variable("p".into(), var);

    let rendered = render_flat_template_with_name(
        &flat,
        builtin_template("flat-modelica", "flat_modelica.mo.jinja"),
        "M",
    )
    .unwrap();
    assert!(
        rendered.contains("parameter Real p(start = 1, fixed = false);"),
        "{rendered}"
    );
    assert!(
        !rendered.contains("parameter Real p(start = 1, fixed = false) = 1;"),
        "{rendered}"
    );
}

#[test]
fn test_embedded_c_templates_render_solve_ir() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("embedded_c_solve_fixture.mo"),
        1,
        2,
    );
    let solve =
        solve::SolveProblem::with_derivative_rhs(solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_source_span(
                vec![vec![
                    solve::LinearOp::LoadTime { dst: 0 },
                    solve::LinearOp::Const { dst: 1, value: 2.0 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Add,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ]],
                span,
            ),
        ));
    let artifacts = solve::SolveArtifacts::default();

    let header = render_solve_template_with_name(
        &solve,
        &artifacts,
        builtin_template("embedded-c", "model.h.jinja"),
        "EmbeddedDemo",
    )
    .unwrap();
    let source = render_solve_template_with_name(
        &solve,
        &artifacts,
        builtin_template("embedded-c", "model.c.jinja"),
        "EmbeddedDemo",
    )
    .unwrap();

    assert!(header.contains("EMBEDDEDDEMO_DERIVATIVE_LEN = 1"));
    assert!(source.contains("out[0] ="));
    assert!(source.contains("m->time"));
    assert!(source.contains("2.0"));
    assert!(
        source.contains(
            "real_t dx[EMBEDDEDDEMO_DERIVATIVE_LEN > 0 ? EMBEDDEDDEMO_DERIVATIVE_LEN : 1]"
        )
    );
    assert!(source.contains("EmbeddedDemo_derivative_rhs(m, dx);"));
}

#[test]
fn test_embedded_c_header_macros_use_allocated_symbols() {
    let mut dae = dae::Dae::new();
    for name in ["a.b", "a_b"] {
        dae.variables.algebraics.insert(
            name.into(),
            rumoca_ir_dae::Variable {
                name: name.into(),
                ..rumoca_ir_dae::Variable::empty_with_span(fixture_span())
            },
        );
    }

    let mut bindings = indexmap::IndexMap::new();
    bindings.insert("a.b".to_string(), solve::scalar_slot_y(0));
    bindings.insert("a_b".to_string(), solve::scalar_slot_y(1));
    let mut problem = solve::SolveProblem {
        layout: solve::VarLayout::from_parts(bindings, 2, 0),
        ..Default::default()
    };
    problem.solve_layout.algebraic_scalar_count = 2;
    let renderer = solve_renderer::SolveTemplateRenderer::new_with_dae(
        &problem,
        &solve::SolveArtifacts::default(),
        dae,
    )
    .unwrap();

    let header = renderer
        .render_with_name(builtin_template("embedded-c", "model.h.jinja"), "M")
        .unwrap();

    assert!(
        header.contains("#define M_Y_b 0  /* algebraic a.b */"),
        "dotted name should use its collision-free allocated symbol:\n{header}"
    );
    assert!(
        header.contains("#define M_Y_a_b 1  /* algebraic a_b */"),
        "plain sanitized name should remain available for the real source name:\n{header}"
    );
    assert_eq!(
        header.matches("#define M_Y_a_b").count(),
        1,
        "header macro names must not collide:\n{header}"
    );
}

#[test]
fn test_embedded_c_array_start_helpers_are_defined() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            dims: vec![2],
            start: Some(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Zeros,
                args: vec![rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                }],
                span: rumoca_core::Span::DUMMY,
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(fixture_span())
        },
    );

    let mut bindings = indexmap::IndexMap::new();
    bindings.insert("x[1]".to_string(), solve::scalar_slot_y(0));
    bindings.insert("x[2]".to_string(), solve::scalar_slot_y(1));
    let mut problem = solve::SolveProblem {
        layout: solve::VarLayout::from_parts(bindings, 2, 0),
        ..Default::default()
    };
    problem.solve_layout.state_scalar_count = 2;
    let renderer = solve_renderer::SolveTemplateRenderer::new_with_dae(
        &problem,
        &solve::SolveArtifacts::default(),
        dae,
    )
    .unwrap();

    let header = renderer
        .render_with_name(builtin_template("embedded-c", "model.h.jinja"), "M")
        .unwrap();
    let source = renderer
        .render_with_name(builtin_template("embedded-c", "model.c.jinja"), "M")
        .unwrap();

    assert!(
        header.contains("#define REAL_C(x) ((real_t)(x))"),
        "embedded-C header should define helper macros emitted by expression rendering:\n{header}"
    );
    assert!(
        source.contains("REAL_C(0.0)"),
        "zeros(...) array starts should still render through the shared expression path:\n{source}"
    );
}

#[test]
fn test_julia_mtk_template_empty_dae() {
    let dae = dae::Dae::new();
    let result =
        render_template(&dae, builtin_template("julia-mtk", "julia_mtk.jl.jinja")).unwrap();
    assert!(result.contains("using ModelingToolkit"));
    assert!(result.contains("using SciMLBase: CallbackSet, ContinuousCallback, ODEProblem, solve"));
    assert!(result.contains("using OrdinaryDiffEqTsit5: Tsit5"));
    assert!(result.contains("using IfElse: ifelse"));
    assert!(result.contains("@independent_variables t"));
    assert!(result.contains("D = Differential(t)"));
    assert!(result.contains("@named sys = ODESystem(eqs, t)"));
    assert!(result.contains("structural_simplify(sys)"));
}

#[test]
fn test_julia_mtk_template_with_state() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some("x".into()),
        rhs: rumoca_core::Expression::VarRef {
            name: "x".into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: "test".into(),
        scalar_count: 1,
    });

    let result =
        render_template(&dae, builtin_template("julia-mtk", "julia_mtk.jl.jinja")).unwrap();
    assert!(
        result.contains("x(t)"),
        "state should be time-dependent: {result}"
    );
    assert!(
        result.contains("D(x) ~"),
        "should generate derivative equation: {result}"
    );
}

#[test]
fn test_julia_mtk_template_with_params_and_constants() {
    let mut dae = dae::Dae::new();
    dae.variables.parameters.insert(
        "k".into(),
        rumoca_ir_dae::Variable {
            name: "k".into(),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.5),
                span: rumoca_core::Span::DUMMY,
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae.variables.constants.insert(
        "g".into(),
        rumoca_ir_dae::Variable {
            name: "g".into(),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(9.81),
                span: rumoca_core::Span::DUMMY,
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let result =
        render_template(&dae, builtin_template("julia-mtk", "julia_mtk.jl.jinja")).unwrap();
    assert!(
        result.contains("@parameters"),
        "should have @parameters block: {result}"
    );
    assert!(
        result.contains("k = 2.5"),
        "parameter should have default: {result}"
    );
    assert!(
        result.contains("g = 9.81"),
        "constant should be assigned: {result}"
    );
}
