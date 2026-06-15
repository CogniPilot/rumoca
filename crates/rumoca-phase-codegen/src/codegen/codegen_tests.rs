//! Codegen integration tests for built-in targets.
//!
//! tests stabilize after the DAE API migration.

use super::*;
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rumoca_ir_solve as solve;

fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
}

fn solve_problem_with_one_by_one_matmul_derivative() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::MatMul {
            lhs_ops: vec![solve::LinearOp::Const { dst: 0, value: 2.0 }],
            lhs_start: 0,
            rhs_ops: vec![solve::LinearOp::Const { dst: 1, value: 3.0 }],
            rhs_start: 1,
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: Default::default(),
            rhs_sparsity: Default::default(),
            metadata: Default::default(),
            span: Default::default(),
        }],
    };
    problem
}

fn solve_problem_with_two_by_two_linsolve_derivative() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::LinSolve {
            setup_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 2.0 },
                solve::LinearOp::Const { dst: 1, value: 0.0 },
                solve::LinearOp::Const { dst: 2, value: 0.0 },
                solve::LinearOp::Const { dst: 3, value: 4.0 },
                solve::LinearOp::Const { dst: 4, value: 8.0 },
                solve::LinearOp::Const {
                    dst: 5,
                    value: 20.0,
                },
            ],
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            next_reg: 6,
            metadata: Default::default(),
            span: Default::default(),
        }],
    };
    problem
}

#[test]
fn test_render_simple_template() {
    let dae = dae::Dae::new();
    let template = "# States: {{ dae.x | length }}";
    let result = render_template(&dae, template).unwrap();
    assert!(result.contains("# States: 0"));
}

#[test]
fn test_record_param_template_skip_uses_type_class_metadata() {
    let dae_json = serde_json::json!({
        "functions": {
            "ByMetadata": {
                "inputs": [{"name": "r", "type_name": "Pkg.Record", "type_class": "Record"}]
            },
            "ByNameOnly": {
                "inputs": [{"name": "c", "type_name": "Complex", "type_class": null}]
            }
        }
    });
    let template = "{% for name, func in dae.functions | items %}{{ name }}={{ has_complex_params(func) | default(value='') | trim }};{% endfor %}";

    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(rendered.contains("ByMetadata=yes;"));
    assert!(rendered.contains("ByNameOnly=;"));
}

#[test]
fn test_simulation_template_rejects_external_function_with_stable_diagnostic() {
    let mut dae = dae::Dae::new();
    let mut function = rumoca_core::Function::new("ExternalUser", Default::default());
    function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
    function.external = Some(rumoca_core::ExternalFunction::default());
    dae.symbols
        .functions
        .insert("ExternalUser".into(), function);

    let err = render_template_with_name(&dae, builtin_template("fmi3", "model.c.jinja"), "M")
        .expect_err("simulation templates must reject unsupported external functions");

    use miette::Diagnostic;
    assert_eq!(
        err.code().map(|code| code.to_string()),
        Some("rumoca::codegen::EC004".to_string())
    );
    match err {
        crate::errors::CodegenError::ExternalFunctionNotCallable { function, span, .. } => {
            assert_eq!(function, "ExternalUser");
            assert!(!span.is_empty());
        }
        other => panic!("expected ExternalFunctionNotCallable, got {other:?}"),
    }
}

#[test]
fn test_fmi_external_dependencies_manifest_renders_library_metadata() {
    let mut dae = dae::Dae::new();
    let mut function = rumoca_core::Function::new(
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.initialize",
        Default::default(),
    );
    function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("initialize_Modelica_EnergyPlus_9_6_0".to_string()),
        output_name: None,
        arg_names: vec![
            "adapter".to_string(),
            "isSynchronized".to_string(),
            "nObj".to_string(),
        ],
        libraries: vec![
            "ModelicaBuildingsEnergyPlus_9_6_0".to_string(),
            "fmilib_shared".to_string(),
        ],
        include_directories: vec!["modelica://Buildings/Resources/C-Sources".to_string()],
        ..Default::default()
    });
    dae.symbols
        .functions
        .insert(function.name.clone(), function);

    let rendered = render_template_with_name(
        &dae,
        builtin_template("fmi2", "externalDependencies.json.jinja"),
        "ExternalEnergyPlusDiag_M",
    )
    .expect("external dependency manifest should render without enabling external calls");
    let manifest: serde_json::Value =
        serde_json::from_str(&rendered).expect("manifest should be valid JSON");

    assert_eq!(manifest["schema"], "rumoca.externalDependencies.v1");
    assert_eq!(
        manifest["functions"][0]["symbol"],
        "initialize_Modelica_EnergyPlus_9_6_0"
    );
    assert_eq!(
        manifest["functions"][0]["libraries"],
        serde_json::json!(["ModelicaBuildingsEnergyPlus_9_6_0", "fmilib_shared"])
    );
    assert_eq!(
        manifest["functions"][0]["include_directories"],
        serde_json::json!(["modelica://Buildings/Resources/C-Sources"])
    );

    let libraries = render_template_with_name(
        &dae,
        builtin_template("fmi2", "externalLibraries.txt.jinja"),
        "ExternalEnergyPlusDiag_M",
    )
    .expect("external library list should render from the same metadata");
    assert_eq!(
        libraries.lines().collect::<Vec<_>>(),
        vec!["ModelicaBuildingsEnergyPlus_9_6_0", "fmilib_shared"]
    );

    let include_directories = render_template_with_name(
        &dae,
        builtin_template("fmi2", "externalIncludeDirectories.txt.jinja"),
        "ExternalEnergyPlusDiag_M",
    )
    .expect("external include directory list should render from the same metadata");
    assert_eq!(
        include_directories.lines().collect::<Vec<_>>(),
        vec!["modelica://Buildings/Resources/C-Sources"]
    );
}

#[test]
fn test_fmi_templates_render_json_function_body_key() {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "functions".to_string(),
        serde_json::json!({
            "UserFunction": {
                "name": "UserFunction",
                "inputs": [],
                "outputs": [{"name": "y", "dims": [], "default": null}],
                "locals": [],
                "body": ["Return"],
                "is_constructor": false,
                "pure": true,
                "external": null,
                "derivatives": [],
                "span": null
            }
        }),
    );

    for target in ["fmi2", "fmi3"] {
        let rendered = render_template_with_dae_json_and_name(
            &dae_json,
            builtin_template(target, "model.c.jinja"),
            "M",
        )
        .unwrap_or_else(|err| panic!("{target} template should render function body: {err}"));

        assert!(
            rendered.contains("return y;"),
            "{target} template should render the body key from JSON-backed functions:\n{rendered}"
        );
    }
}

#[test]
fn test_fmi_function_body_renders_bare_component_reference_expression() {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "functions".to_string(),
        serde_json::json!({
            "ForwardInput": {
                "name": "ForwardInput",
                "inputs": [{"name": "x", "dims": [], "default": null}],
                "outputs": [{"name": "y", "dims": [], "default": null}],
                "locals": [],
                "body": [{
                    "Assignment": {
                        "comp": {"local": false, "parts": [{"ident": "y", "subs": []}]},
                        "value": {"local": false, "parts": [{"ident": "x", "subs": []}]}
                    }
                }],
                "is_constructor": false,
                "pure": true,
                "external": null,
                "derivatives": [],
                "span": null
            }
        }),
    );

    let rendered = render_template_with_dae_json_and_name(
        &dae_json,
        builtin_template("fmi2", "model.c.jinja"),
        "M",
    )
    .expect("FMI2 template should render component-reference expression values");

    assert!(
        rendered.contains("y = x;"),
        "function body should render bare ComponentReference expressions:\n{rendered}"
    );
}

#[test]
fn test_fmi_function_body_renders_spanned_expression_wrapper() {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "functions".to_string(),
        serde_json::json!({
            "ForwardSpannedInput": {
                "name": "ForwardSpannedInput",
                "inputs": [{"name": "x", "dims": [], "default": null}],
                "outputs": [{"name": "y", "dims": [], "default": null}],
                "locals": [],
                "body": [{
                    "Assignment": {
                        "comp": {"local": false, "parts": [{"ident": "y", "subs": []}]},
                        "value": {"expr": {"VarRef": {"name": {"name": "x"}, "subscripts": []}}, "span": null}
                    }
                }],
                "is_constructor": false,
                "pure": true,
                "external": null,
                "derivatives": [],
                "span": null
            }
        }),
    );

    let rendered = render_template_with_dae_json_and_name(
        &dae_json,
        builtin_template("fmi2", "model.c.jinja"),
        "M",
    )
    .expect("FMI2 template should render spanned expression wrappers");

    assert!(
        rendered.contains("y = x;"),
        "function body should render expression wrappers:\n{rendered}"
    );
}

#[test]
fn test_simulation_template_file_rejects_external_function_with_stable_diagnostic() {
    let mut dae = dae::Dae::new();
    let mut function = rumoca_core::Function::new("ExternalFileUser", Default::default());
    function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
    function.external = Some(rumoca_core::ExternalFunction::default());
    dae.symbols
        .functions
        .insert("ExternalFileUser".into(), function);

    let path = std::env::temp_dir().join(format!(
        "rumoca_external_function_template_{}.jinja",
        std::process::id()
    ));
    std::fs::write(&path, builtin_template("fmi3", "model.c.jinja"))
        .expect("write temporary template");
    let err = render_template_file(&dae, &path)
        .expect_err("file-backed simulation templates must reject unsupported external functions");
    let _ = std::fs::remove_file(&path);

    use miette::Diagnostic;
    assert_eq!(
        err.code().map(|code| code.to_string()),
        Some("rumoca::codegen::EC004".to_string())
    );
    match err {
        crate::errors::CodegenError::ExternalFunctionNotCallable { function, span, .. } => {
            assert_eq!(function, "ExternalFileUser");
            assert!(!span.is_empty());
        }
        other => panic!("expected ExternalFunctionNotCallable, got {other:?}"),
    }
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

    let solve = rumoca_ir_solve::SolveProblem::default();
    let solve_artifacts = rumoca_ir_solve::SolveArtifacts::default();
    let solve_rendered = render_template_for_input(
        CodegenInput::Solve {
            problem: &solve,
            artifacts: &solve_artifacts,
        },
        "{{ ir_kind }} {{ solve.continuous.residual.programs | length }} {{ ir.continuous.residual.programs | length }}",
    )
    .unwrap();
    assert_eq!(solve_rendered, "solve 0 0");

    let ast = ast::ClassTree::new();
    let ast_rendered = render_template_for_input(
        CodegenInput::Ast(&ast),
        "{{ ir_kind }} {{ ast.definitions.classes | length }} {{ ir.definitions.classes | length }}",
    )
    .unwrap();
    assert_eq!(ast_rendered, "ast 0 0");
}

#[test]
fn test_solve_template_context_exposes_tensor_nodes_and_scalar_fallback_rows() {
    let problem = solve_problem_with_two_by_two_linsolve_derivative();
    let artifacts = solve::SolveArtifacts::default();

    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        "{{ solve_blocks.continuous.derivative_rhs.nodes | length }} {{ solve_blocks.continuous.derivative_rhs.tensor_node_count }} {{ solve_blocks.continuous.derivative_rhs.scalar_programs.programs | length }} {{ solve_blocks.continuous.derivative_rhs.scalar_programs_use_linear_solve_component }}",
        "TensorDemo",
    )
    .expect("solve template should render tensor block context");

    assert_eq!(rendered, "1 1 2 true");
}

#[test]
fn test_c_solve_builtin_target_renders_scalar_fallback_derivative_kernel() {
    let problem = solve_problem_with_two_by_two_linsolve_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("c-solve", "model_solve.c.jinja"),
        "TensorDemo",
    )
    .expect("c-solve template should render");

    assert!(rendered.contains("void TensorDemo_derivative_rhs"));
    assert!(rendered.contains("out[0] ="));
    assert!(
        rendered.contains("*"),
        "scalar fallback should preserve the multiply in generated C: {rendered}"
    );
}

#[test]
fn test_c_solve_builtin_target_syntax_checks_when_cc_available() {
    if std::process::Command::new("cc")
        .arg("--version")
        .output()
        .is_err()
    {
        eprintln!("skipping c-solve syntax smoke: cc not available");
        return;
    }

    let problem = solve_problem_with_two_by_two_linsolve_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let header = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("c-solve", "model_solve.h.jinja"),
        "TensorDemo",
    )
    .expect("c-solve header should render");
    let source = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("c-solve", "model_solve.c.jinja"),
        "TensorDemo",
    )
    .expect("c-solve source should render");
    assert!(source.contains("__rumoca_solve_linear_component"));
    let dir = std::env::temp_dir().join(format!("rumoca_c_solve_smoke_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create c-solve smoke dir");
    let header_path = dir.join("TensorDemo_solve.h");
    let source_path = dir.join("TensorDemo_solve.c");
    std::fs::write(&header_path, header).expect("write generated c-solve header");
    std::fs::write(&source_path, source).expect("write generated c-solve source");

    let output = std::process::Command::new("cc")
        .arg("-std=c11")
        .arg("-fsyntax-only")
        .arg(&source_path)
        .current_dir(&dir)
        .output()
        .expect("run cc syntax check");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        output.status.success(),
        "generated c-solve source must pass cc syntax check\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn test_rust_solve_builtin_target_renders_scalar_fallback_derivative_kernel() {
    let problem = solve_problem_with_two_by_two_linsolve_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-solve", "model_solve.rs.jinja"),
        "TensorDemo",
    )
    .expect("rust-solve template should render");

    assert!(rendered.contains("pub fn derivative_rhs"));
    assert!(rendered.contains("out[0] ="));
    assert!(rendered.contains("rumoca_solve_linear_component"));
}

#[test]
fn test_rust_solve_builtin_target_syntax_checks_when_rustc_available() {
    if std::process::Command::new("rustc")
        .arg("--version")
        .output()
        .is_err()
    {
        eprintln!("skipping rust-solve syntax smoke: rustc not available");
        return;
    }

    let problem = solve_problem_with_two_by_two_linsolve_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let source = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-solve", "model_solve.rs.jinja"),
        "TensorDemo",
    )
    .expect("rust-solve source should render");
    assert!(source.contains("rumoca_solve_linear_component"));
    let dir = std::env::temp_dir().join(format!("rumoca_rust_solve_smoke_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create rust-solve smoke dir");
    let source_path = dir.join("TensorDemo_solve.rs");
    std::fs::write(&source_path, source).expect("write generated rust-solve source");

    let output = std::process::Command::new("rustc")
        .arg("--crate-type")
        .arg("lib")
        .arg(&source_path)
        .current_dir(&dir)
        .output()
        .expect("run rustc syntax check");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        output.status.success(),
        "generated rust-solve source must pass rustc syntax check\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn test_mlir_builtin_target_renders_tensor_scalar_fallback_rows() {
    let problem = solve_problem_with_one_by_one_matmul_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("mlir", "mlir.mlir.jinja"),
        "TensorDemo",
    )
    .expect("mlir template should render tensor fallback rows");

    assert!(rendered.contains("func.func @eval_derivative"));
    assert!(
        rendered.contains("arith.mulf"),
        "MLIR scalar fallback should preserve the tensor multiply as scalar ops: {rendered}"
    );
    assert!(
        !rendered.contains("render_matmul_mlir"),
        "MLIR template must not expose unfinished native tensor macro names: {rendered}"
    );
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
    dae.variables.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            ..Default::default()
        },
    );
    dae.events
        .synthetic_root_conditions
        .push(rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        });

    let value = dae_template_json(&dae).expect("dae_template_json should not fail");
    let object = value
        .as_object()
        .expect("template JSON should be an object");

    assert!(object.contains_key("x"));
    assert!(!object.contains_key("states"));
    assert!(!object.contains_key("x_dot_alias"));
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
    let mut function =
        rumoca_core::Function::new("LieGroup.SO3.rotationMatrix", Default::default());
    function.add_input(rumoca_core::FunctionParam::new("q", "Real").with_dims(vec![4]));
    function.add_output(rumoca_core::FunctionParam::new("R", "Real").with_dims(vec![3, 3]));
    dae.symbols
        .functions
        .insert("LieGroup.SO3.rotationMatrix".into(), function);

    let value = dae_template_json(&dae).expect("dae_template_json should not fail");
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
    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: "a".into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::VarRef {
            name: "b".into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
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
    dae.conditions
        .relations
        .push(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Ge,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: "height".into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        });

    let template =
        r#"{% set cfg = {"power": "pow"} %}{{ render_event_indicator(dae.relation[0], cfg) }}"#;
    let rendered = render_template(&dae, template).unwrap();
    assert_eq!(rendered, "((height) - (0.0))");
}

#[test]
fn test_render_solve_row_c_template_function_uses_solver_slots() {
    let row = vec![
        rumoca_ir_solve::LinearOp::LoadY { dst: 0, index: 2 },
        rumoca_ir_solve::LinearOp::LoadP { dst: 1, index: 1 },
        rumoca_ir_solve::LinearOp::Binary {
            dst: 2,
            op: rumoca_ir_solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        rumoca_ir_solve::LinearOp::StoreOutput { src: 2 },
    ];
    let template =
        r#"{{ render_solve_row_c(dae.row, {"time": "m->time", "y": "Y({})", "p": "P({})"}) }}"#;
    let rendered = render_template_with_dae_json(
        &serde_json::json!({
            "row": row,
        }),
        template,
    )
    .unwrap();

    assert_eq!(rendered, "((Y(2)) - (P(1)))");
}

#[test]
fn test_render_solve_row_c_template_function_uses_seed_slots() {
    let row = vec![
        rumoca_ir_solve::LinearOp::LoadSeed { dst: 0, index: 3 },
        rumoca_ir_solve::LinearOp::StoreOutput { src: 0 },
    ];
    let template = r#"{{ render_solve_row_c(dae.row, {"time": "m->time", "y": "Y({})", "p": "P({})", "seed": "S({})"}) }}"#;
    let rendered = render_template_with_dae_json(
        &serde_json::json!({
            "row": row,
        }),
        template,
    )
    .unwrap();

    assert_eq!(rendered, "S(3)");
}

#[test]
fn test_render_solve_row_rust_template_function_uses_rust_numeric_methods() {
    let row = vec![
        rumoca_ir_solve::LinearOp::LoadY { dst: 0, index: 0 },
        rumoca_ir_solve::LinearOp::LoadP { dst: 1, index: 0 },
        rumoca_ir_solve::LinearOp::Binary {
            dst: 2,
            op: rumoca_ir_solve::BinaryOp::Pow,
            lhs: 0,
            rhs: 1,
        },
        rumoca_ir_solve::LinearOp::Unary {
            dst: 3,
            op: rumoca_ir_solve::UnaryOp::Sqrt,
            arg: 2,
        },
        rumoca_ir_solve::LinearOp::StoreOutput { src: 3 },
    ];
    let template =
        r#"{{ render_solve_row_rust(dae.row, {"time": "time", "y": "y[{}]", "p": "p[{}]"}) }}"#;
    let rendered = render_template_with_dae_json(
        &serde_json::json!({
            "row": row,
        }),
        template,
    )
    .unwrap();

    assert_eq!(rendered, "((y[0]).powf(p[0])).sqrt()");
}

#[test]
fn test_render_solve_row_c_template_function_uses_strict_compare_ops() {
    let row = vec![
        rumoca_ir_solve::LinearOp::LoadY { dst: 0, index: 0 },
        rumoca_ir_solve::LinearOp::LoadP { dst: 1, index: 0 },
        rumoca_ir_solve::LinearOp::Compare {
            dst: 2,
            op: rumoca_ir_solve::CompareOp::Eq,
            lhs: 0,
            rhs: 1,
        },
        rumoca_ir_solve::LinearOp::Compare {
            dst: 3,
            op: rumoca_ir_solve::CompareOp::Ne,
            lhs: 0,
            rhs: 1,
        },
        rumoca_ir_solve::LinearOp::Binary {
            dst: 4,
            op: rumoca_ir_solve::BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        rumoca_ir_solve::LinearOp::StoreOutput { src: 4 },
    ];
    let template =
        r#"{{ render_solve_row_c(dae.row, {"time": "m->time", "y": "Y({})", "p": "P({})"}) }}"#;
    let rendered = render_template_with_dae_json(
        &serde_json::json!({
            "row": row,
        }),
        template,
    )
    .unwrap();

    assert!(rendered.contains("((Y(0)) == (P(0)))"));
    assert!(rendered.contains("((Y(0)) != (P(0)))"));
    assert!(!rendered.contains("EPSILON"));
    assert!(!rendered.contains("fabs"));
}
