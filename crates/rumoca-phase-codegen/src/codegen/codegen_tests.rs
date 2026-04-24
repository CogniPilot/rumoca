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
fn test_sanitize_filter_folds_static_component_subscript_arithmetic() {
    let dae = dae::Dae::new();
    let template = "{{ 'zone[(1 + 1)].T' | sanitize }} {{ 'floor3Zones[2 - 1 + 3].T' | sanitize }}";
    let result = render_template(&dae, template).unwrap();
    assert_eq!(result, "zone_2_T floor3Zones_4_T");
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
fn test_render_expr_function() {
    let dae = dae::Dae::new();
    // Test the render_expr function is available
    let template = r#"{% set cfg = {"prefix": "ca.", "power": "**"} %}OK"#;
    let result = render_template(&dae, template).unwrap();
    assert!(result.contains("OK"));
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
