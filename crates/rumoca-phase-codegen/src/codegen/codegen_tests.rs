use super::*;
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

fn normalize_newlines(input: &str) -> String {
    input.replace("\r\n", "\n")
}

#[test]
fn test_render_simple_template() {
    let dae = dae::Dae::new();
    let template = "# States: {{ dae.x | length }}";
    let result = render_template(&dae, template).unwrap();
    assert!(result.contains("# States: 0"));
}

#[test]
fn test_render_template_for_input_supports_dae_flat_and_ast() {
    let dae = dae::Dae::new();
    let dae_rendered = render_template_for_input(
        CodegenInput::Dae(&dae),
        "{{ ir_kind }} {{ dae.x | length }} {{ ir.x | length }}",
    )
    .unwrap();
    assert_eq!(dae_rendered, "dae 0 0");

    let flat = flat::Model::new();
    let flat_rendered = render_template_for_input(
        CodegenInput::Flat(&flat),
        "{{ ir_kind }} {{ flat.variables | length }} {{ ir.variables | length }}",
    )
    .unwrap();
    assert_eq!(flat_rendered, "flat 0 0");

    let ast = ast::ClassTree::new();
    let ast_rendered = render_template_for_input(
        CodegenInput::Ast(&ast),
        "{{ ir_kind }} {{ ast.definitions.classes | length }} {{ ir.definitions.classes | length }}",
    )
    .unwrap();
    assert_eq!(ast_rendered, "ast 0 0");
}

#[test]
fn test_render_ast_template_with_name() {
    let ast = ast::ClassTree::new();
    let rendered =
        render_ast_template_with_name(&ast, "model {{ model_name }} end {{ model_name }};", "M")
            .unwrap();
    assert_eq!(rendered, "model M end M;");
}

#[test]
fn test_sanitize_filter() {
    let dae = dae::Dae::new();
    let template = "{{ 'body.position.x' | sanitize }}";
    let result = render_template(&dae, template).unwrap();
    assert_eq!(result, "body_position_x");
}

#[test]
fn test_access_dae_fields() {
    let dae = dae::Dae::new();
    let template = r#"
n_x: {{ dae.x | length }}
n_y: {{ dae.y | length }}
n_p: {{ dae.p | length }}
"#;
    let result = render_template(&dae, template).unwrap();
    assert!(result.contains("n_x: 0"));
    assert!(result.contains("n_y: 0"));
    assert!(result.contains("n_p: 0"));
}

#[test]
fn test_dae_template_json_uses_canonical_keys_only() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            ..Default::default()
        },
    );
    dae.derivative_aliases.insert(
        "dx".into(),
        rumoca_ir_dae::Variable {
            name: "dx".into(),
            ..Default::default()
        },
    );
    dae.synthetic_root_conditions
        .push(rumoca_ir_dae::Expression::If {
            branches: vec![(
                rumoca_ir_dae::Expression::Literal(rumoca_ir_dae::Literal::Boolean(true)),
                rumoca_ir_dae::Expression::Literal(rumoca_ir_dae::Literal::Real(1.0)),
            )],
            else_branch: Box::new(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(0.0),
            )),
        });

    let value = dae_template_json(&dae);
    let object = value
        .as_object()
        .expect("template JSON should be an object");

    assert!(object.contains_key("x"));
    assert!(object.contains_key("x_dot_alias"));
    assert!(!object.contains_key("states"));
    assert!(!object.contains_key("derivative_aliases"));
    assert!(
        object
            .get("synthetic_root_conditions")
            .and_then(serde_json::Value::as_array)
            .is_some_and(|items| items.len() == 1),
        "synthetic_root_conditions should serialize nested if-expression branches",
    );
}

#[test]
fn test_dae_template_json_includes_projected_function_output_refs() {
    let mut dae = dae::Dae::new();
    let mut function = dae::Function::new("LieGroup.SO3.rotationMatrix", Default::default());
    function.add_input(dae::FunctionParam::new("q", "Real").with_dims(vec![4]));
    function.add_output(dae::FunctionParam::new("R", "Real").with_dims(vec![3, 3]));
    dae.functions
        .insert("LieGroup.SO3.rotationMatrix".into(), function);

    let value = dae_template_json(&dae);
    let refs = value
        .get("symbol_refs")
        .and_then(serde_json::Value::as_array)
        .expect("symbol_refs should be present")
        .iter()
        .filter_map(serde_json::Value::as_str)
        .collect::<Vec<_>>();

    assert!(
        refs.contains(&"LieGroup.SO3.rotationMatrix.R[1]"),
        "first projected array-output function symbol should be allocated: {refs:?}",
    );
    assert!(
        refs.contains(&"LieGroup.SO3.rotationMatrix.R[9]"),
        "last projected array-output function symbol should be allocated: {refs:?}",
    );
}

#[test]
fn test_render_expr_function() {
    let dae = dae::Dae::new();
    // Test the render_expr function is available
    let template = r#"{% set cfg = {"prefix": "ca.", "power": "**"} %}OK"#;
    let result = render_template(&dae, template).unwrap();
    assert!(result.contains("OK"));
}

#[test]
fn test_render_event_indicator_lowers_relation_to_numeric_residual() {
    let expr = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Lt(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: "a".into(),
            subscripts: vec![],
        }),
        rhs: Box::new(dae::Expression::VarRef {
            name: "b".into(),
            subscripts: vec![],
        }),
    };
    let value = Value::from_serialize(&expr);

    let rendered = render_event_indicator(&value, &ExprConfig::default()).unwrap();
    assert_eq!(rendered, "((a) - (b))");

    let binary = get_field(&value, "Binary").unwrap();
    let rendered_from_inner = render_event_indicator(&binary, &ExprConfig::default()).unwrap();
    assert_eq!(rendered_from_inner, "((a) - (b))");
}

#[test]
fn test_render_event_indicator_template_function() {
    let mut dae = dae::Dae::new();
    dae.relation.push(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Ge(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: "height".into(),
            subscripts: vec![],
        }),
        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
    });

    let template =
        r#"{% set cfg = {"power": "pow"} %}{{ render_event_indicator(dae.relation[0], cfg) }}"#;
    let rendered = render_template(&dae, template).unwrap();
    assert_eq!(rendered, "((height) - (0.0))");
}

#[test]
fn test_target_symbols_use_short_readable_names_without_collisions() {
    let mut dae = dae::Dae::new();
    for name in ["body.x", "other.x", "body_x"] {
        dae.algebraics.insert(
            name.into(),
            dae::Variable {
                name: name.into(),
                ..Default::default()
            },
        );
    }
    let template = r#"
{% set policy = {"separator": "_", "reserved": [], "generated_prefixes": []} %}
{% set symbols = target_symbols(dae.symbol_refs, policy) %}
{{ symbol(symbols, "body.x") }} {{ symbol(symbols, "other.x") }} {{ symbol(symbols, "body_x") }}
"#;
    let rendered = render_template(&dae, template).unwrap();
    assert_eq!(rendered.trim(), "x other_x body_x");
}

#[test]
fn test_target_symbols_scalarize_array_refs_readably_and_without_collision() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        "plant.leg_f_b".into(),
        dae::Variable {
            name: "plant.leg_f_b".into(),
            dims: vec![4, 3],
            ..Default::default()
        },
    );
    dae.algebraics.insert(
        "leg_f_b_2_1".into(),
        dae::Variable {
            name: "leg_f_b_2_1".into(),
            ..Default::default()
        },
    );
    let template = r#"
{% set policy = {"separator": "_", "reserved": [], "generated_prefixes": []} %}
{% set symbols = target_symbols(dae.symbol_refs, policy) %}
{{ symbol(symbols, "plant.leg_f_b[2,1]") }}
"#;
    let rendered = render_template(&dae, template).unwrap();
    assert_eq!(rendered.trim(), "plant_leg_f_b_2_1");
}

#[test]
fn test_source_ref_template_helper_preserves_scalar_names() {
    let dae = dae::Dae::new();
    let rendered = render_template(&dae, r#"{{ source_ref("x", [], 1) }}"#).unwrap();
    assert_eq!(rendered, "x");
}

#[test]
fn test_render_expr_uses_template_symbol_map_for_indexed_refs() {
    let expr = dae::Expression::VarRef {
        name: "plant.leg_f_b".into(),
        subscripts: vec![dae::Subscript::Index(2), dae::Subscript::Index(1)],
    };
    let symbols = serde_json::json!({
        "plant.leg_f_b": "leg_f_b",
        "plant.leg_f_b[2,1]": "leg_f_b_2_1"
    });
    let cfg = ExprConfig {
        subscript_underscore: true,
        symbols: Some(Value::from_serialize(symbols)),
        ..ExprConfig::default()
    };

    let rendered = render_expression(&Value::from_serialize(&expr), &cfg).unwrap();
    assert_eq!(rendered, "leg_f_b_2_1");
}

#[test]
fn test_fmi3_initialize_defaults_uses_allocated_symbols_for_start_aliases() {
    let mut dae = dae::Dae::new();
    for (name, start) in [
        ("plant.ground_z", 0.0),
        ("plant.leg_z", -0.1),
        ("plant.initial_ground_clearance", 0.02),
    ] {
        dae.parameters.insert(
            name.into(),
            dae::Variable {
                name: name.into(),
                start: Some(dae::Expression::Literal(dae::Literal::Real(start))),
                ..Default::default()
            },
        );
    }

    let p3_start = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Add(Default::default()),
        lhs: Box::new(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: "ground_z".into(),
                subscripts: Vec::new(),
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: "leg_z".into(),
                subscripts: Vec::new(),
            }),
        }),
        rhs: Box::new(dae::Expression::VarRef {
            name: "initial_ground_clearance".into(),
            subscripts: Vec::new(),
        }),
    };
    dae.states.insert(
        "plant.p".into(),
        dae::Variable {
            name: "plant.p".into(),
            dims: vec![3],
            start: Some(dae::Expression::Array {
                elements: vec![
                    dae::Expression::Literal(dae::Literal::Integer(0)),
                    dae::Expression::Literal(dae::Literal::Integer(0)),
                    p3_start,
                ],
                is_matrix: false,
            }),
            ..Default::default()
        },
    );

    let rendered =
        render_template_with_name(&dae, crate::templates::FMI3_MODEL, "TestModel").unwrap();

    assert!(
        rendered.contains("double ground_z = 0.0;"),
        "parameter alias should use the allocated readable symbol:\n{rendered}"
    );
    assert!(
        !rendered.contains("double plant_ground_z = 0.0;"),
        "initialize_defaults must not bypass the symbol allocator:\n{rendered}"
    );
    assert!(
        rendered.contains("m->x[2] = ((ground_z - leg_z) + initial_ground_clearance);"),
        "state start expression should compile against the local aliases:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_indexes_common_array_binary_rhs() {
    let rhs = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: "error_dot".into(),
            subscripts: Vec::new(),
        }),
        rhs: Box::new(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: "error".into(),
                subscripts: Vec::new(),
            }),
            rhs: Box::new(dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Pre,
                args: vec![dae::Expression::VarRef {
                    name: "q".into(),
                    subscripts: Vec::new(),
                }],
            }),
        }),
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
fn test_c_alg_rhs_prefers_direct_array_connection_over_rearranged_equation() {
    fn var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: name.into(),
            subscripts: Vec::new(),
        }
    }
    fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    let indirect_motor_equation = sub(
        var("motor_1_omega_error"),
        sub(var("motor_1_omega_cmd"), var("motor_1_omega")),
    );
    let direct_array_connection = sub(
        dae::Expression::Array {
            elements: vec![
                var("motor_1_omega_cmd"),
                var("motor_2_omega_cmd"),
                var("motor_3_omega_cmd"),
                var("motor_4_omega_cmd"),
            ],
            is_matrix: false,
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
    fn var(name: &str, subscripts: Vec<i64>) -> dae::Expression {
        dae::Expression::VarRef {
            name: name.into(),
            subscripts: subscripts.into_iter().map(dae::Subscript::Index).collect(),
        }
    }
    fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    let earlier_reverse_array_alias = sub(
        dae::Expression::Array {
            elements: vec![
                var("motor_1_omega_cmd", vec![]),
                var("motor_2_omega_cmd", vec![]),
                var("motor_3_omega_cmd", vec![]),
                var("motor_4_omega_cmd", vec![]),
            ],
            is_matrix: false,
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
fn test_c_ode_rhs_solves_preserved_matrix_vector_derivative_equation() {
    let residual = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Mul(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: "J".into(),
                subscripts: Vec::new(),
            }),
            rhs: Box::new(dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![dae::Expression::VarRef {
                    name: "omega".into(),
                    subscripts: Vec::new(),
                }],
            }),
        }),
        rhs: Box::new(dae::Expression::VarRef {
            name: "M_body".into(),
            subscripts: Vec::new(),
        }),
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
    fn var(name: &str, subscripts: Vec<i64>) -> dae::Expression {
        dae::Expression::VarRef {
            name: name.into(),
            subscripts: subscripts.into_iter().map(dae::Subscript::Index).collect(),
        }
    }
    fn der_omega(index: i64) -> dae::Expression {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("omega", vec![index])],
        }
    }
    fn mul(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Mul(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    fn add(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    fn row(row: i64) -> dae::Expression {
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
    let lhs = rumoca_ir_flat::Expression::VarRef {
        name: "a".into(),
        subscripts: vec![],
    };
    let rhs = rumoca_ir_flat::Expression::VarRef {
        name: "b".into(),
        subscripts: vec![],
    };
    let mul_expr = rumoca_ir_flat::Expression::Binary {
        op: rumoca_ir_flat::OpBinary::Mul(Default::default()),
        lhs: Box::new(lhs.clone()),
        rhs: Box::new(rhs.clone()),
    };
    let mul_elem_expr = rumoca_ir_flat::Expression::Binary {
        op: rumoca_ir_flat::OpBinary::MulElem(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
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
    let expr = rumoca_ir_flat::Expression::ArrayComprehension {
        expr: Box::new(rumoca_ir_flat::Expression::VarRef {
            name: "i".into(),
            subscripts: vec![],
        }),
        indices: vec![rumoca_ir_flat::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_ir_flat::Expression::Range {
                start: Box::new(rumoca_ir_flat::Expression::Literal(
                    rumoca_ir_flat::Literal::Integer(1),
                )),
                step: None,
                end: Box::new(rumoca_ir_flat::Expression::VarRef {
                    name: "n".into(),
                    subscripts: vec![],
                }),
            },
        }],
        filter: Some(Box::new(rumoca_ir_flat::Expression::Binary {
            op: rumoca_ir_flat::OpBinary::Gt(Default::default()),
            lhs: Box::new(rumoca_ir_flat::Expression::VarRef {
                name: "i".into(),
                subscripts: vec![],
            }),
            rhs: Box::new(rumoca_ir_flat::Expression::Literal(
                rumoca_ir_flat::Literal::Integer(0),
            )),
        })),
    };

    let rendered =
        render_expression(&Value::from_serialize(&expr), &ExprConfig::default()).unwrap();
    assert_eq!(rendered, "{i for i in 1:n if (i > 0)}");
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
fn test_casadi_mx_template_empty_dae() {
    let dae = dae::Dae::new();
    let result = render_template(&dae, crate::templates::CASADI_MX).unwrap();
    assert!(result.contains("import casadi as ca"));
    assert!(result.contains("def create_model()"));
    assert!(result.contains("n_x = 0"));
    assert!(result.contains("n_z = 0"));
    assert!(result.contains("dae_fn = ca.Function"));
}

#[test]
fn test_casadi_mx_template_flattens_array_start_values_for_x0() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            dims: vec![2],
            start: Some(rumoca_ir_dae::Expression::Array {
                elements: vec![
                    rumoca_ir_dae::Expression::Literal(rumoca_ir_dae::Literal::Real(1.0)),
                    rumoca_ir_dae::Expression::Literal(rumoca_ir_dae::Literal::Real(2.0)),
                ],
                is_matrix: false,
            }),
            ..Default::default()
        },
    );
    dae.states.insert(
        "y".into(),
        rumoca_ir_dae::Variable {
            name: "y".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(3.0),
            )),
            ..Default::default()
        },
    );

    let result = normalize_newlines(&render_template(&dae, crate::templates::CASADI_MX).unwrap());
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
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae.algebraics.insert(
        "z".into(),
        rumoca_ir_dae::Variable {
            name: "z".into(),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae.inputs.insert(
        "u".into(),
        rumoca_ir_dae::Variable {
            name: "u".into(),
            dims: vec![4],
            ..Default::default()
        },
    );
    dae.parameters.insert(
        "p".into(),
        rumoca_ir_dae::Variable {
            name: "p".into(),
            dims: vec![5],
            ..Default::default()
        },
    );

    let result = render_template(&dae, crate::templates::CASADI_SX).unwrap();
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
fn test_render_dae_equation_via_template() {
    // Test render_equation function via template with a simple DAE
    // that has residual equations (the common case from todae)
    let dae = dae::Dae::new();

    // Test with an empty DAE - just verify the template compiles
    let tmpl = crate::templates::DAE_MODELICA;
    let result = render_template_with_name(&dae, tmpl, "TestModel").unwrap();
    assert!(result.contains("class TestModel"));
    assert!(result.contains("equation"));
    assert!(result.contains("end TestModel"));
}

#[test]
fn test_dae_template_includes_model_description() {
    // Test that DAE template includes model description when present
    let mut dae = dae::Dae::new();
    dae.model_description = Some("Test model description".to_string());

    // Render template
    let tmpl = crate::templates::DAE_MODELICA;
    let result = render_template_with_name(&dae, tmpl, "TestModel").unwrap();
    assert!(result.contains(r#"class TestModel "Test model description""#));
}

#[test]
fn test_render_flat_equation_via_template() {
    // Test render_flat_equation function via template with an empty Model
    let flat = flat::Model::new();

    let tmpl = crate::templates::FLAT_MODELICA;
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
        variability: rumoca_ir_core::Variability::Parameter(Default::default()),
        start: Some(rumoca_ir_flat::Expression::Literal(
            rumoca_ir_flat::Literal::Integer(1),
        )),
        ..Default::default()
    };
    var.fixed = None; // Parameter default: fixed=true
    flat.add_variable("T".into(), var);

    let rendered =
        render_flat_template_with_name(&flat, crate::templates::FLAT_MODELICA, "M").unwrap();
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
        variability: rumoca_ir_core::Variability::Parameter(Default::default()),
        start: Some(rumoca_ir_flat::Expression::Literal(
            rumoca_ir_flat::Literal::Integer(1),
        )),
        fixed: Some(false),
        ..Default::default()
    };
    flat.add_variable("p".into(), var);

    let rendered =
        render_flat_template_with_name(&flat, crate::templates::FLAT_MODELICA, "M").unwrap();
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
fn test_render_error_contains_context() {
    // Verify that errors from custom functions propagate with context
    let dae = dae::Dae::new();
    // Use a template that calls render_expr with invalid data
    let template = r#"{{ render_expr(none, {}) }}"#;
    let err = render_template(&dae, template).unwrap_err();
    let msg = format!("{err}");
    assert!(
        msg.contains("template") || msg.contains("error"),
        "error should contain diagnostic info, got: {msg}"
    );
}

#[test]
fn test_template_undefined_field_fails_fast() {
    let dae = dae::Dae::new();
    let template = "{% for x in dae.missing_field %}{{ x }}{% endfor %}";
    let err = render_template(&dae, template).expect_err("missing field must fail");
    let msg = format!("{err}");
    assert!(
        msg.contains("missing_field") || msg.contains("undefined"),
        "expected undefined-field error, got: {msg}"
    );
}

#[test]
fn test_template_missing_assignment_target_fails_fast() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"Assignment": {"value": {"Literal": {"Real": 1.0}}}} %}
{{ render_statement(stmt, {"if_style": "modelica"}, "") }}
"#;
    let err = render_template(&dae, template).expect_err("missing assignment target must fail");
    let msg = format!("{err}");
    assert!(
        msg.contains("Assignment missing 'comp' field")
            || msg.contains("target resolved to empty component reference"),
        "expected strict assignment error, got: {msg}"
    );
}

#[test]
fn test_render_statement_if_blocks_use_stmts_field() {
    let dae = dae::Dae::new();
    let template = r#"
{% set stmt = {"If": {"cond_blocks": [
    {"cond": {"Literal": {"Boolean": true}}, "stmts": [
        {"Assignment": {"comp": "lat", "value": {"Literal": {"Real": 40.0}}}},
        {"Assignment": {"comp": "lon", "value": {"Literal": {"Real": -86.0}}}}
    ]}
], "else_block": [
    {"Assignment": {"comp": "lat", "value": {"Literal": {"Real": 0.0}}}}
]}} %}
{{ render_statement(stmt, {"if_style": "ternary"}, "") }}
"#;
    let rendered = render_template(&dae, template).expect("render if statement");
    assert!(
        rendered.contains("lat = 40") && rendered.contains("lon = -86"),
        "if branch assignments should render from StatementBlock.stmts:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_templates_reject_continuous_models() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(1.0),
            )),
            ..Default::default()
        },
    );
    dae.f_x.push(rumoca_ir_dae::Equation {
        lhs: Some("x".into()),
        rhs: rumoca_ir_dae::Expression::VarRef {
            name: "x".into(),
            subscripts: vec![],
        },
        span: Default::default(),
        origin: "test".into(),
        scalar_count: 1,
    });

    for template in [
        crate::templates::EMBEDDED_C_H,
        crate::templates::EMBEDDED_C_IMPL,
    ] {
        let err = render_template(&dae, template).expect_err("continuous DAE must fail fast");
        let msg = format!("{err}");
        assert!(
            msg.contains("only support discrete models") || msg.contains("dae.f_x must be empty"),
            "expected embedded-C continuous-model rejection, got: {msg}"
        );
    }
}

#[test]
fn test_julia_mtk_template_empty_dae() {
    let dae = dae::Dae::new();
    let result = render_template(&dae, crate::templates::JULIA_MTK).unwrap();
    assert!(result.contains("using ModelingToolkit"));
    assert!(result.contains("using DifferentialEquations"));
    assert!(result.contains("@independent_variables t"));
    assert!(result.contains("D = Differential(t)"));
    assert!(result.contains("@named sys = ODESystem(eqs, t)"));
    assert!(result.contains("structural_simplify(sys)"));
}

#[test]
fn test_julia_mtk_template_with_state() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(1.0),
            )),
            ..Default::default()
        },
    );
    dae.f_x.push(rumoca_ir_dae::Equation {
        lhs: Some("x".into()),
        rhs: rumoca_ir_dae::Expression::VarRef {
            name: "x".into(),
            subscripts: vec![],
        },
        span: Default::default(),
        origin: "test".into(),
        scalar_count: 1,
    });

    let result = render_template(&dae, crate::templates::JULIA_MTK).unwrap();
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
    dae.parameters.insert(
        "k".into(),
        rumoca_ir_dae::Variable {
            name: "k".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(2.5),
            )),
            ..Default::default()
        },
    );
    dae.constants.insert(
        "g".into(),
        rumoca_ir_dae::Variable {
            name: "g".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(9.81),
            )),
            ..Default::default()
        },
    );

    let result = render_template(&dae, crate::templates::JULIA_MTK).unwrap();
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
