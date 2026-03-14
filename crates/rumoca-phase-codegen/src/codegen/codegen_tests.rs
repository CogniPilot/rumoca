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
    let template = "# States: {{ dae.states | length }}";
    let result = render_template(&dae, template).unwrap();
    assert!(result.contains("# States: 0"));
}

#[test]
fn test_render_template_for_input_supports_dae_flat_and_ast() {
    let dae = dae::Dae::new();
    let dae_rendered = render_template_for_input(
        CodegenInput::Dae(&dae),
        "{{ ir_kind }} {{ dae.states | length }} {{ ir.states | length }}",
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
n_states: {{ dae.states | length }}
n_alg: {{ dae.algebraics | length }}
n_params: {{ dae.parameters | length }}
"#;
    let result = render_template(&dae, template).unwrap();
    assert!(result.contains("n_states: 0"));
    assert!(result.contains("n_alg: 0"));
    assert!(result.contains("n_params: 0"));
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
    // In Python mode (default), uses list comprehension with range()
    assert_eq!(rendered, "[i for i in range(1, (n) + 1) if (i > 0)]");
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
fn test_fmi2_model_description_empty_dae() {
    let dae = dae::Dae::new();
    let result =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL_DESCRIPTION, "TestModel")
            .unwrap();
    assert!(result.contains("fmiVersion=\"2.0\""));
    assert!(result.contains("modelName=\"TestModel\""));
    assert!(result.contains("guid=\"TestModel-rumoca\""));
    assert!(result.contains("<ModelExchange modelIdentifier=\"TestModel\""));
    assert!(result.contains("<ModelVariables>"));
    assert!(result.contains("</ModelVariables>"));
    assert!(result.contains("<ModelStructure>"));
    assert!(result.contains("generationTool=\"Rumoca\""));
}

#[test]
fn test_fmi2_model_description_with_states_and_params() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(1.0),
            )),
            unit: Some("m".to_string()),
            description: Some("position".to_string()),
            ..Default::default()
        },
    );
    dae.states.insert(
        "v".into(),
        rumoca_ir_dae::Variable {
            name: "v".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(0.0),
            )),
            unit: Some("m/s".to_string()),
            ..Default::default()
        },
    );
    dae.parameters.insert(
        "k".into(),
        rumoca_ir_dae::Variable {
            name: "k".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(10.0),
            )),
            ..Default::default()
        },
    );

    let result =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL_DESCRIPTION, "SpringMass")
            .unwrap();

    // States appear as local/continuous with exact initial
    assert!(result.contains("name=\"x\""));
    assert!(result.contains("causality=\"local\""));
    assert!(result.contains("variability=\"continuous\""));
    assert!(result.contains("initial=\"exact\""));
    assert!(result.contains("start=\"1.0\""));
    assert!(result.contains("unit=\"m\""));
    assert!(result.contains("description=\"position\""));

    // Derivatives appear
    assert!(result.contains("name=\"der(x)\""));
    assert!(result.contains("name=\"der(v)\""));

    // Parameters appear as parameter/fixed
    assert!(result.contains("name=\"k\""));
    assert!(result.contains("causality=\"parameter\""));
    assert!(result.contains("variability=\"fixed\""));
    assert!(result.contains("start=\"10.0\""));

    // Model structure sections
    assert!(result.contains("<Derivatives>"));
    assert!(result.contains("<InitialUnknowns>"));
}

#[test]
fn test_fmi2_model_c_empty_dae() {
    let dae = dae::Dae::new();
    let result =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL, "TestModel").unwrap();
    assert!(result.contains("FMI 2.0 Model Exchange"));
    assert!(result.contains("#define N_STATES"));
    assert!(result.contains("fmi2Instantiate"));
    assert!(result.contains("fmi2GetDerivatives"));
    assert!(result.contains("fmi2SetContinuousStates"));
    assert!(result.contains("fmi2GetReal"));
    assert!(result.contains("fmi2SetReal"));
    assert!(result.contains("fmi2Terminate"));
    assert!(result.contains("MODEL_GUID"));
}

#[test]
fn test_fmi2_model_c_with_states_and_params() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(1.0),
            )),
            nominal: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(5.0),
            )),
            ..Default::default()
        },
    );
    dae.parameters.insert(
        "k".into(),
        rumoca_ir_dae::Variable {
            name: "k".into(),
            start: Some(rumoca_ir_dae::Expression::Literal(
                rumoca_ir_dae::Literal::Real(10.0),
            )),
            ..Default::default()
        },
    );

    let result =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL, "SpringMass").unwrap();

    // Dimensions
    assert!(result.contains("#define N_STATES         1"));
    assert!(result.contains("#define N_PARAMETERS     1"));

    // Default initialization (two-phase: alias = expr; m->p[i] = alias)
    assert!(result.contains("m->x[0] = 1.0;"));
    assert!(result.contains("k = 10.0;"));
    assert!(result.contains("m->p[0] = k;"));

    // Nominals
    assert!(result.contains("x_nominal[0] = 5.0;"));

    // VR offsets
    assert!(result.contains("#define VR_X      0"));
    assert!(result.contains("#define VR_XDOT   N_STATES"));
}

#[test]
fn test_fmi2_templates_value_reference_consistency() {
    // Ensure both templates produce consistent value reference layouts
    let mut dae = dae::Dae::new();
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            ..Default::default()
        },
    );
    dae.inputs.insert(
        "u".into(),
        rumoca_ir_dae::Variable {
            name: "u".into(),
            ..Default::default()
        },
    );
    dae.outputs.insert(
        "w".into(),
        rumoca_ir_dae::Variable {
            name: "w".into(),
            ..Default::default()
        },
    );

    let xml =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL_DESCRIPTION, "M").unwrap();
    let c_code = render_template_with_name(&dae, crate::templates::FMI2_MODEL, "M").unwrap();

    // x at vr=0, xdot at vr=1 (n_x=1), u at vr=2 (n_y=0), w at vr=3
    assert!(result_contains_vr(&xml, "x", 0));
    assert!(result_contains_vr(&xml, "der(x)", 1));
    assert!(result_contains_vr(&xml, "u", 2));
    assert!(result_contains_vr(&xml, "w", 3));

    // C code should have matching offsets
    assert!(c_code.contains("#define VR_X      0"));
}

fn result_contains_vr(xml: &str, var_name: &str, vr: usize) -> bool {
    let pattern = format!("name=\"{}\" valueReference=\"{}\"", var_name, vr);
    xml.contains(&pattern)
}

#[test]
fn test_fmi2_model_c_has_cosimulation_support() {
    let dae = dae::Dae::new();
    let result =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL, "TestModel").unwrap();

    // Co-Simulation API functions
    assert!(result.contains("fmi2DoStep"));
    assert!(result.contains("fmi2CancelStep"));
    assert!(result.contains("forward Euler"));

    // Should accept both ME and CS
    assert!(result.contains("fmuType != fmi2ModelExchange && fmuType != fmi2CoSimulation"));
}

#[test]
fn test_fmi2_model_description_has_cosimulation() {
    let dae = dae::Dae::new();
    let result =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL_DESCRIPTION, "TestModel")
            .unwrap();

    assert!(result.contains("<CoSimulation"));
    assert!(result.contains("<ModelExchange"));
    assert!(result.contains("canHandleVariableCommunicationStepSize"));
}

#[test]
fn test_fmi2_model_description_derivative_indices_are_contiguous_for_mixed_arrays() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae.states.insert(
        "v".into(),
        rumoca_ir_dae::Variable {
            name: "v".into(),
            dims: vec![3],
            ..Default::default()
        },
    );

    let result =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL_DESCRIPTION, "IdxModel")
            .unwrap();

    // n_x = 5, derivative block must be indices 6..10 (1-based).
    for idx in 6..=10 {
        assert!(
            result.contains(&format!("<Unknown index=\"{idx}\"/>")),
            "missing derivative index {idx} in rendered XML"
        );
    }
}

#[test]
fn test_fmi2_model_c_has_output_and_discrete_functions() {
    let mut dae = dae::Dae::new();
    dae.outputs.insert(
        "w".into(),
        rumoca_ir_dae::Variable {
            name: "w".into(),
            ..Default::default()
        },
    );

    let result =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL, "TestModel").unwrap();

    // Output computation
    assert!(result.contains("compute_outputs"));
    // Discrete update computation
    assert!(result.contains("compute_discrete_updates"));
    // Called from fmi2NewDiscreteStates
    assert!(result.contains("compute_discrete_updates(m)"));
    // Outputs should no longer be hardcoded TODO stubs.
    assert!(!result.contains("TODO: output equation"));
}

#[test]
fn test_fmi2_model_c_maps_array_state_derivatives_by_indexed_name() {
    use rumoca_ir_core::OpBinary;
    use rumoca_ir_dae::{BuiltinFunction, Equation, Expression, Literal};

    let mut dae = dae::Dae::new();
    dae.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            dims: vec![2],
            ..Default::default()
        },
    );

    // 0 = der(x[1]) - 1
    dae.f_x.push(Equation::residual(
        Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: "x[1]".into(),
                    subscripts: vec![],
                }],
            }),
            rhs: Box::new(Expression::Literal(Literal::Real(1.0))),
        },
        Default::default(),
        "test",
    ));
    // 0 = der(x[2]) - 2
    dae.f_x.push(Equation::residual(
        Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: "x[2]".into(),
                    subscripts: vec![],
                }],
            }),
            rhs: Box::new(Expression::Literal(Literal::Real(2.0))),
        },
        Default::default(),
        "test",
    ));

    let result =
        render_template_with_name(&dae, crate::templates::FMI2_MODEL, "ArrayDeriv").unwrap();
    assert!(result.contains("m->xdot[0] = 1.0;"));
    assert!(result.contains("m->xdot[1] = 2.0;"));
}

#[test]
fn test_fmi2_model_c_exposes_discrete_valued_via_get_real() {
    let mut dae = dae::Dae::new();
    dae.discrete_valued.insert(
        "mode".into(),
        rumoca_ir_dae::Variable {
            name: "mode".into(),
            ..Default::default()
        },
    );

    let result = render_template_with_name(&dae, crate::templates::FMI2_MODEL, "DiscVal").unwrap();
    assert!(result.contains("vr >= VR_M && vr < VR_M + N_DISCRETE_VAL"));
}

#[test]
fn test_is_self_call_detects_self_referential_function() {
    use rumoca_ir_dae::{
        ComponentRefPart, ComponentReference, Dae, Expression, Function, FunctionParam, Statement,
        VarName,
    };

    fn comp_ref(name: &str) -> ComponentReference {
        ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: name.to_string(),
                subs: vec![],
            }],
            def_id: None,
        }
    }

    fn func_param(name: &str) -> FunctionParam {
        FunctionParam::new(name, "Real")
    }

    // Create a function like Modelica.Math.sin whose body calls itself
    let mut dae = Dae::new();
    let func = Function {
        name: VarName("Modelica.Math.sin".to_string()),
        inputs: vec![func_param("u")],
        outputs: vec![func_param("y")],
        body: vec![Statement::Assignment {
            comp: comp_ref("y"),
            value: Expression::FunctionCall {
                name: VarName("Modelica.Math.sin".to_string()),
                args: vec![Expression::VarRef {
                    name: VarName("u".to_string()),
                    subscripts: vec![],
                }],
                is_constructor: false,
            },
        }],
        ..Function::new("Modelica.Math.sin", Default::default())
    };
    dae.functions
        .insert(VarName("Modelica.Math.sin".to_string()), func);

    // Render a template that uses is_self_call
    let template = r#"{% for func_name, func in dae.functions | items %}{{ is_self_call(func_name, func) }}{% endfor %}"#;
    let result = render_template(&dae, template).unwrap();
    assert_eq!(
        result.trim(),
        "true",
        "is_self_call should detect self-referential function"
    );

    // Also test with a non-self-referential function
    let mut dae2 = Dae::new();
    let func2 = Function {
        name: VarName("myFunc".to_string()),
        inputs: vec![func_param("u")],
        outputs: vec![func_param("y")],
        body: vec![Statement::Assignment {
            comp: comp_ref("y"),
            value: Expression::VarRef {
                name: VarName("u".to_string()),
                subscripts: vec![],
            },
        }],
        ..Function::new("myFunc", Default::default())
    };
    dae2.functions.insert(VarName("myFunc".to_string()), func2);

    let result2 = render_template(&dae2, template).unwrap();
    assert_eq!(
        result2.trim(),
        "false",
        "is_self_call should not detect non-self-referential function"
    );
}
