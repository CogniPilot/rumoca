//! Codegen integration tests for built-in targets and Kelvin BOPTEST parity.
//!
//! SPEC_0021 file-size exception: JSON-backed BOPTEST fixtures share one
//! render harness; split plan is `codegen_tests_boptest.rs` once upstream ODE
//! tests stabilize after the DAE API migration.

use super::*;
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rumoca_ir_solve as solve;

fn normalize_newlines(input: &str) -> String {
    input.replace("\r\n", "\n")
}

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

#[test]
fn test_render_solve_row_c_template_function_uses_dense_linear_solve_op() {
    let row = vec![
        rumoca_ir_solve::LinearOp::Const { dst: 0, value: 2.0 },
        rumoca_ir_solve::LinearOp::Const { dst: 1, value: 0.0 },
        rumoca_ir_solve::LinearOp::Const { dst: 2, value: 0.0 },
        rumoca_ir_solve::LinearOp::Const { dst: 3, value: 4.0 },
        rumoca_ir_solve::LinearOp::Const { dst: 4, value: 8.0 },
        rumoca_ir_solve::LinearOp::Const {
            dst: 5,
            value: 20.0,
        },
        rumoca_ir_solve::LinearOp::LinearSolveComponent {
            dst: 6,
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            component: 1,
        },
        rumoca_ir_solve::LinearOp::StoreOutput { src: 6 },
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

    assert!(rendered.contains("__rumoca_solve_linear_component"));
    assert!(rendered.contains("(double[]){2"));
    assert!(rendered.contains("(double[]){8"));
}

#[test]
fn test_fmi3_event_indicators_render_from_solver_ir() {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "solve".to_string(),
        serde_json::json!({
            "events": {
                "root_conditions": {
                    "programs": [[
                        {"LoadY": {"dst": 0, "index": 0}},
                        {"Const": {"dst": 1, "value": 0.0}},
                        {"Binary": {"dst": 2, "op": "Sub", "lhs": 0, "rhs": 1}},
                        {"StoreOutput": {"src": 2}}
                    ]]
                }
            }
        }),
    );

    let rendered = render_template_with_dae_json_and_name(
        &dae_json,
        builtin_template("fmi3", "model.c.jinja"),
        "M",
    )
    .unwrap();

    assert!(rendered.contains("#define N_EVENT_INDICATORS 1"));
    assert!(rendered.contains("m->event_indicators[0] = ((__rumoca_solve_y(m, 0)) - (0.0));"));
    assert!(
        !rendered.contains("render_event_indicator"),
        "FMI3 event indicators should be generated from solve IR rows"
    );
}

#[test]
fn test_fmi3_derivative_api_renders_from_solver_ad_ir() {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "solve".to_string(),
        serde_json::json!({
            "artifacts": {
                "continuous": {
                    "full_jacobian_v": {
                        "programs": [[
                            {"LoadSeed": {"dst": 0, "index": 0}},
                            {"StoreOutput": {"src": 0}}
                        ]]
                    }
                }
            },
            "root_conditions": {
                "programs": []
            }
        }),
    );

    let rendered = render_template_with_dae_json_and_name(
        &dae_json,
        builtin_template("fmi3", "model.c.jinja"),
        "M",
    )
    .unwrap();

    assert!(rendered.contains("#define N_SOLVE_JACOBIAN_ROWS 1"));
    assert!(rendered.contains("out[0] = seed[0];"));
    assert!(
        !rendered.contains("Finite-difference") && !rendered.contains("finite-difference"),
        "FMI3 derivative APIs should consume solve AD rows, not finite differences"
    );
}

#[test]
fn test_fmi3_derivatives_do_not_treat_implicit_solver_residuals_as_xdot() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable::new(rumoca_core::VarName::new("x")),
    );
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args: vec![rumoca_core::Expression::VarRef {
                    name: "x".into(),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Box::new(rumoca_core::Expression::VarRef {
                    name: "x".into(),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        origin: "test".into(),
        scalar_count: 1,
    });
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "solve".to_string(),
        serde_json::json!({
            "continuous": {
                "residual": {
                    "programs": [[
                        {"Const": {"dst": 0, "value": 42.0}},
                        {"StoreOutput": {"src": 0}}
                    ]]
                },
                "derivative_rhs": {
                    "programs": [[
                        {"LoadY": {"dst": 0, "index": 0}},
                        {"Unary": {"dst": 1, "op": "Neg", "arg": 0}},
                        {"StoreOutput": {"src": 1}}
                    ]]
                }
            },
            "events": {
                "root_conditions": {
                    "programs": []
                }
            }
        }),
    );

    let rendered = render_template_with_dae_json_and_name(
        &dae_json,
        builtin_template("fmi3", "model.c.jinja"),
        "M",
    )
    .unwrap();

    assert!(
        rendered.contains("m->xdot[0] = (-(__rumoca_solve_y(m, 0)));"),
        "FMI3 derivatives should come from solve derivative rows, got:\n{rendered}"
    );
    assert!(
        !rendered.contains("m->xdot[0] = 42.0;"),
        "implicit solve residual rows are not ordered xdot rows"
    );
}

#[test]
fn test_target_symbols_use_short_readable_names_without_collisions() {
    let mut dae = dae::Dae::new();
    for name in ["body.x", "other.x", "body_x"] {
        dae.variables.algebraics.insert(
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
    dae.variables.algebraics.insert(
        "plant.leg_f_b".into(),
        dae::Variable {
            name: "plant.leg_f_b".into(),
            dims: vec![4, 3],
            ..Default::default()
        },
    );
    dae.variables.algebraics.insert(
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
    let expr = rumoca_core::Expression::VarRef {
        name: "plant.leg_f_b".into(),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
            rumoca_core::Subscript::generated_index(1, rumoca_core::Span::DUMMY),
        ],
        span: rumoca_core::Span::DUMMY,
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
        dae.variables.parameters.insert(
            name.into(),
            dae::Variable {
                name: name.into(),
                start: Some(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(start),
                    span: rumoca_core::Span::DUMMY,
                }),
                ..Default::default()
            },
        );
    }

    let p3_start = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: "ground_z".into(),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: "leg_z".into(),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::VarRef {
            name: "initial_ground_clearance".into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    dae.variables.states.insert(
        "plant.p".into(),
        dae::Variable {
            name: "plant.p".into(),
            dims: vec![3],
            start: Some(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(0),
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(0),
                        span: rumoca_core::Span::DUMMY,
                    },
                    p3_start,
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );

    let rendered =
        render_template_with_name(&dae, builtin_template("fmi3", "model.c.jinja"), "TestModel")
            .unwrap();

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
            ..Default::default()
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
            ..Default::default()
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
            ..Default::default()
        },
    );
    dae.variables.algebraics.insert(
        "z".into(),
        rumoca_ir_dae::Variable {
            name: "z".into(),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae.variables.inputs.insert(
        "u".into(),
        rumoca_ir_dae::Variable {
            name: "u".into(),
            dims: vec![4],
            ..Default::default()
        },
    );
    dae.variables.parameters.insert(
        "p".into(),
        rumoca_ir_dae::Variable {
            name: "p".into(),
            dims: vec![5],
            ..Default::default()
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
    dae.variables.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            ..Default::default()
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

    assert!(model_variables < model_structure, "{xml}");
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
            ..Default::default()
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
fn test_fmi3_build_templates_use_fmi3_platform_directory_names() {
    assert!(
        builtin_template("fmi3", "CMakeLists.txt.jinja")
            .contains(r#"set(FMU_PLATFORM "x86_64-linux")"#),
        "FMI 3 Linux binaries must use binaries/x86_64-linux"
    );
    assert!(
        builtin_template("fmi3", "build.sh.jinja").contains("PLATFORM=x86_64-linux"),
        "FMI 3 shell build must use binaries/x86_64-linux"
    );
    assert!(
        !builtin_template("fmi3", "CMakeLists.txt.jinja").contains("linux64"),
        "linux64 is the FMI 2 platform directory, not FMI 3"
    );
    assert!(
        !builtin_template("fmi3", "build.sh.jinja").contains("linux64"),
        "linux64 is the FMI 2 platform directory, not FMI 3"
    );
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
fn test_fmi3_exit_initialization_seeds_pre_discrete_values() {
    let template = builtin_template("fmi3", "model.c.jinja");
    let exit_initialization = template
        .split("FMI3_EXPORT fmi3Status fmi3ExitInitializationMode")
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
        ..Default::default()
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
        ..Default::default()
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
    let solve = solve::SolveProblem::with_derivative_rhs(
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]])),
    );
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
    assert!(source.contains("EmbeddedDemo_derivative_rhs(m, dx);"));
}

#[test]
fn test_julia_mtk_template_empty_dae() {
    let dae = dae::Dae::new();
    let result =
        render_template(&dae, builtin_template("julia-mtk", "julia_mtk.jl.jinja")).unwrap();
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
    dae.variables.states.insert(
        "x".into(),
        rumoca_ir_dae::Variable {
            name: "x".into(),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some("x".into()),
        rhs: rumoca_core::Expression::VarRef {
            name: "x".into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
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
            ..Default::default()
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
            ..Default::default()
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

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_zone_supply_return_streams() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[1].Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[1].C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[2].Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[2].C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[3].Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[3].C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].vol.ports[3].Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].vol.ports[3].C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.m_flow_mulSup[3].port_b.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.m_flow_mulSup[3].port_b.C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.m_flow_mulRet[3].port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.m_flow_mulRet[3].port_a.C_outflow[1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
sup_xi={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[1].Xi_outflow[1]", dae, cfg) }}
sup_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[1].C_outflow[1]", dae, cfg) }}
ret_xi={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[2].Xi_outflow[1]", dae, cfg) }}
ret_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[2].C_outflow[1]", dae, cfg) }}
leak={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[3].Xi_outflow[1]", dae, cfg) }}
leak_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[3].C_outflow[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "sup_xi=floor1BoptestAirNetwork_floor_fivZonVAV_m_flow_mulSup_3_port_b_Xi_outflow_1"
        ) && rendered.contains(
            "sup_c=floor1BoptestAirNetwork_floor_fivZonVAV_m_flow_mulSup_3_port_b_C_outflow_1"
        ) && rendered.contains(
            "ret_xi=floor1BoptestAirNetwork_floor_fivZonVAV_m_flow_mulRet_3_port_a_Xi_outflow_1"
        ) && rendered.contains(
            "ret_c=floor1BoptestAirNetwork_floor_fivZonVAV_m_flow_mulRet_3_port_a_C_outflow_1"
        ),
        "BOPTEST zone supply/return streams should follow the explicit VAV supply/return connector surfaces:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "leak=floor1BoptestAirNetwork_floor_fivZonVAV_zon_3_vol_ports_3_Xi_outflow_1"
        ) && rendered.contains(
            "leak_c=floor1BoptestAirNetwork_floor_fivZonVAV_zon_3_vol_ports_3_C_outflow_1"
        ),
        "BOPTEST zone auxiliary leakage/opening streams should follow the matching ThermalZone volume port, not the supply/return shortcut:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_air_network_port_streams() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor1BoptestAirNetwork.floor.fivZonVAV.senCO2[3].port.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.senCO2[3].port.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.senCO2[3].port.C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[1].h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[1].Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[3].ports[1].C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_b[5].h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_b[5].Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_b[5].C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.junSup4.res2.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.junSup4.res2.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.junSup4.res2.port_a.C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_a[4].h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_a[4].Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_a[4].C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.junRet4.res3.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.junRet4.res3.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.junRet4.res3.port_a.C_outflow[1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
sen_h={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.senCO2[3].port.h_outflow", dae, cfg) }}
sen_xi={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.senCO2[3].port.Xi_outflow[1]", dae, cfg) }}
sen_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.senCO2[3].port.C_outflow[1]", dae, cfg) }}
sup5_h={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_b[5].h_outflow", dae, cfg) }}
sup5_xi={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_b[5].Xi_outflow[1]", dae, cfg) }}
sup5_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_b[5].C_outflow[1]", dae, cfg) }}
ret4_h={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_a[4].h_outflow", dae, cfg) }}
ret4_xi={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_a[4].Xi_outflow[1]", dae, cfg) }}
ret4_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.AirNetWor.ports_a[4].C_outflow[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("sen_h=floor1BoptestAirNetwork_floor_fivZonVAV_zon_3_ports_1_h_outflow")
            && rendered.contains(
                "sen_xi=floor1BoptestAirNetwork_floor_fivZonVAV_zon_3_ports_1_Xi_outflow_1"
            )
            && rendered.contains(
                "sen_c=floor1BoptestAirNetwork_floor_fivZonVAV_zon_3_ports_1_C_outflow_1"
            ),
        "BOPTEST TraceSubstances one-port sensors should expose the connected zone port stream variables:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "sup5_h=floor1BoptestAirNetwork_floor_fivZonVAV_AirNetWor_junSup4_res2_port_a_h_outflow"
        ) && rendered.contains(
            "sup5_xi=floor1BoptestAirNetwork_floor_fivZonVAV_AirNetWor_junSup4_res2_port_a_Xi_outflow_1"
        ) && rendered.contains(
            "sup5_c=floor1BoptestAirNetwork_floor_fivZonVAV_AirNetWor_junSup4_res2_port_a_C_outflow_1"
        ) && rendered.contains(
            "ret4_h=floor1BoptestAirNetwork_floor_fivZonVAV_AirNetWor_junRet4_res3_port_a_h_outflow"
        ) && rendered.contains(
            "ret4_xi=floor1BoptestAirNetwork_floor_fivZonVAV_AirNetWor_junRet4_res3_port_a_Xi_outflow_1"
        ) && rendered.contains(
            "ret4_c=floor1BoptestAirNetwork_floor_fivZonVAV_AirNetWor_junRet4_res3_port_a_C_outflow_1"
        ),
        "BOPTEST FiveZoneDuctNetwork external stream ports should follow the same internal branch peers as h_outflow:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_zone_tinlet_from_port_stream() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
t={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[4].fmuZon.TInlet[3]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("t=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_fivZonVAV_zon_4_ports_3_p, floor1BoptestAirNetwork_floor_fivZonVAV_zon_4_ports_3_h_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_zon_4_ports_3_Xi_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_zon_4_ports_3_Xi_outflow__len)"),
        "BOPTEST EnergyPlus ThermalZone TInlet should be computed from the matching fluid port stream state, not copied from another inlet:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "TInlet stream-temperature lowering should avoid no-equation fallback:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_wet_coil_volume_moisture_chain() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "coil.ele[1].vol2.C[1]": {},
            "coil.ele[1].vol2.Xi[1]": {},
            "coil.ele[1].vol2.mC[1]": {},
            "coil.ele[1].vol2.mXi[1]": {},
            "coil.ele[1].vol2.ports[1].C_outflow[1]": {},
            "coil.ele[1].vol2.ports[1].Xi_outflow[1]": {},
            "coil.ele[1].vol2.dynBal.C[1]": {},
            "coil.ele[1].vol2.dynBal.COut[1]": {},
            "coil.ele[1].vol2.dynBal.XiOut[1]": {},
            "coil.ele[1].vol2.dynBal.m": {},
            "coil.ele[1].vol2.dynBal.mC[1]": {},
            "coil.ele[1].vol2.dynBal.mCOut[1]": {},
            "coil.ele[1].vol2.dynBal.mXi[1]": {},
            "coil.ele[1].vol2.dynBal.mXiOut[1]": {},
            "coil.ele[1].vol2.dynBal.medium.Xi[1]": {},
            "coil.ele[1].vol2.dynBal.ports[1].C_outflow[1]": {},
            "coil.ele[1].vol2.dynBal.ports[1].Xi_outflow[1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
vol_xi={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.Xi[1]", dae, cfg) }}
vol_c={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.C[1]", dae, cfg) }}
vol_mxi={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.mXi[1]", dae, cfg) }}
vol_mc={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.mC[1]", dae, cfg) }}
port_xi={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.ports[1].Xi_outflow[1]", dae, cfg) }}
port_c={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.ports[1].C_outflow[1]", dae, cfg) }}
dyn_c={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.dynBal.C[1]", dae, cfg) }}
dyn_cout={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.dynBal.COut[1]", dae, cfg) }}
dyn_xiout={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.dynBal.XiOut[1]", dae, cfg) }}
dyn_port_c={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.dynBal.ports[1].C_outflow[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("vol_xi=coil_ele_1_vol2_dynBal_medium_Xi_1")
            && rendered.contains("vol_c=coil_ele_1_vol2_dynBal_C_1")
            && rendered.contains("vol_mxi=coil_ele_1_vol2_dynBal_mXiOut_1")
            && rendered.contains("vol_mc=coil_ele_1_vol2_dynBal_mCOut_1")
            && rendered.contains("port_xi=coil_ele_1_vol2_dynBal_medium_Xi_1")
            && rendered.contains("port_c=coil_ele_1_vol2_C_1")
            && rendered.contains(
                "dyn_c=((fabs(coil_ele_1_vol2_dynBal_m) > 1e-12) ? (coil_ele_1_vol2_dynBal_mC_1 / coil_ele_1_vol2_dynBal_m) : 0.0)"
            )
            && rendered.contains("dyn_cout=coil_ele_1_vol2_dynBal_C_1")
            && rendered.contains("dyn_xiout=coil_ele_1_vol2_dynBal_medium_Xi_1")
            && rendered.contains("dyn_port_c=coil_ele_1_vol2_dynBal_C_1"),
        "wet-coil MixingVolumeHeatMoisturePort should expose the wrapped dynBal moisture and trace state through PartialMixingVolume/ConservationEquation source equations:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_wet_coil_volume_chain_from_array_aliases() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "w": {
            "coil.ele[1].vol2.mXi": {},
            "coil.ele[1].vol2.mC": {},
            "coil.ele[1].vol2.COut_internal": {},
            "coil.ele[1].vol2.dynBal.COut": {},
            "coil.ele[1].vol2.dynBal.mXiOut": {},
            "coil.ele[1].vol2.dynBal.mCOut": {},
            "coil.ele[1].vol2.dynBal.mbXi_flow": {},
            "coil.ele[1].vol2.dynBal.mbC_flow": {},
            "coil.ele[1].vol2.dynBal.C_flow_internal": {}
        },
        "y": {
            "coil.ele[1].vol2.dynBal.mXi": {},
            "coil.ele[1].vol2.dynBal.mC": {},
            "coil.ele[1].vol2.dynBal.C": {},
            "coil.ele[1].vol2.dynBal.medium.Xi": {},
            "coil.ele[1].vol2.dynBal.ports_mXi_flow[1,1]": {},
            "coil.ele[1].vol2.dynBal.ports_mXi_flow[2,1]": {},
            "coil.ele[1].vol2.dynBal.ports_mC_flow[1,1]": {},
            "coil.ele[1].vol2.dynBal.ports_mC_flow[2,1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
vol_mxi={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.mXi[1]", dae, cfg) }}
vol_mc={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.mC[1]", dae, cfg) }}
cout={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.COut_internal[1]", dae, cfg) }}
dyn_cout={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.dynBal.COut[1]", dae, cfg) }}
mbxi={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.dynBal.mbXi_flow[1]", dae, cfg) }}
mbc={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.dynBal.mbC_flow[1]", dae, cfg) }}
cflow={{ alg_rhs_for_var_with_dae("coil.ele[1].vol2.dynBal.C_flow_internal[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("vol_mxi=coil_ele_1_vol2_dynBal_mXiOut_1")
            && rendered.contains("vol_mc=coil_ele_1_vol2_dynBal_mCOut_1")
            && rendered.contains("cout=coil_ele_1_vol2_dynBal_C_1")
            && rendered.contains("dyn_cout=coil_ele_1_vol2_dynBal_C_1")
            && rendered.contains("mbxi=(coil_ele_1_vol2_dynBal_ports_mXi_flow_1_1 + coil_ele_1_vol2_dynBal_ports_mXi_flow_2_1)")
            && rendered.contains("mbc=(coil_ele_1_vol2_dynBal_ports_mC_flow_1_1 + coil_ele_1_vol2_dynBal_ports_mC_flow_2_1)")
            && rendered.contains("cflow=0.0"),
        "array-valued DAE variable maps should prove indexed wet-coil ConservationEquation aliases exist:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_wet_coil_counterflow_aliases() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
t1={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.T1[3]", dae, cfg) }}
t2={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.T2[3]", dae, cfg) }}
tm={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.T_m[3]", dae, cfg) }}
gc={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.ele[3].masExc.Gc", dae, cfg) }}
x={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.ele[3].vol1.dynBal.medium.X[1]", dae, cfg) }}
air1={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.mAirFloRat1", dae, cfg) }}
air2_floor2={{ alg_rhs_for_var_with_dae("floor2BoptestAirNetwork.floor.mAirFloRat2", dae, cfg) }}
ahu_air={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.mAirFloRat", dae, cfg) }}
ahu_ua={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.UA", dae, cfg) }}
cool_ua={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.UA", dae, cfg) }}
wet_ua={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.UA_nominal", dae, cfg) }}
ele_ua={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.ele[3].UA_nominal", dae, cfg) }}
ele_tau={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.ele[3].tau_m", dae, cfg) }}
ele_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.ele[3].C", dae, cfg) }}
mas_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.ele[3].mas.C", dae, cfg) }}
der={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.ele[3].mas.der_T", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "t1=floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_vol1_T"
        ) && rendered.contains(
            "t2=floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_vol2_T"
        ) && rendered.contains(
            "tm=floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_con1_solid_T"
        ) && rendered.contains(
            "gc=floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_Gc_2"
        ) && rendered.contains("x=1.0")
            && rendered.contains("air1=49.140000000000")
            && rendered.contains("air2_floor2=101.250000000000")
            && rendered.contains("ahu_air=(floor1BoptestAirNetwork_floor_mAirFloRat1 + floor1BoptestAirNetwork_floor_mAirFloRat2 + floor1BoptestAirNetwork_floor_mAirFloRat3 + floor1BoptestAirNetwork_floor_mAirFloRat4 + floor1BoptestAirNetwork_floor_mAirFloRat5)")
            && rendered.contains("ahu_ua=(-floor1BoptestAirNetwork_floor_duaFanAirHanUni_mAirFloRat * (1000.0 * 17.0) /")
            && rendered.contains("cool_ua=(floor1BoptestAirNetwork_floor_duaFanAirHanUni_UA * 1.2 * floor1BoptestAirNetwork_floor_duaFanAirHanUni_eps)")
            && rendered.contains("wet_ua=floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_UA")
            && rendered.contains("ele_ua=(floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_UA_nominal / fmax(1.0, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_nEle))")
            && rendered.contains("ele_tau=(floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_tau_m / fmax(1.0, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_nEle))")
            && rendered.contains("ele_c=(2.0 * floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_UA_nominal * floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_tau_m)")
            && rendered.contains("mas_c=floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_C")
            && rendered.contains("der=((floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_mas_C > 1e-9) ? (floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_mas_port_Q_flow / floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_ele_3_mas_C) : 0.0)"),
        "BOPTEST WetCoilCounterFlow array and latent mass-exchange aliases should follow Buildings source equations:\n{rendered}"
    );
    assert!(
        !rendered.contains("no equation found"),
        "BOPTEST WetCoilCounterFlow aliases must not fall through to no-equation placeholders:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_named_gain_and_replicator_blocks() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor.fivZonVAV.gaiCO2[1].u": {},
            "floor.fivZonVAV.gaiCO2[1].y": {},
            "floor.fivZonVAV.gaiCO2[1].k": {},
            "floor.fivZonVAV.zon[1].mWat_flow.u": {},
            "floor.fivZonVAV.zon[1].mWat_flow.y": {},
            "floor.fivZonVAV.zon[1].mWat_flow.k": {},
            "floor.fivZonVAV.zon[1].C_flow[1]": {},
            "floor.fivZonVAV.zon[1].CTot_flow.k1[1]": {},
            "floor.fivZonVAV.zon[1].CTot_flow.k2[1]": {},
            "floor.fivZonVAV.zon[1].CTot_flow.u1[1]": {},
            "floor.fivZonVAV.zon[1].CTot_flow.u2[1]": {},
            "floor.fivZonVAV.zon[1].CTot_flow.y[1]": {},
            "floor.fivZonVAV.zon[1].QPeaRep.u": {},
            "floor.fivZonVAV.zon[1].QPeaRep.y[1]": {},
            "floor.fivZonVAV.replicator[1].u": {},
            "floor.fivZonVAV.replicator[1].y[1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
co2={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.gaiCO2[1].y", dae, cfg) }}
mwat={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.zon[1].mWat_flow.y", dae, cfg) }}
cflow={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.zon[1].C_flow[1]", dae, cfg) }}
ctot_u1={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.zon[1].CTot_flow.u1[1]", dae, cfg) }}
ctot_u2={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.zon[1].CTot_flow.u2[1]", dae, cfg) }}
ctot={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.zon[1].CTot_flow.y[1]", dae, cfg) }}
qpea={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.zon[1].QPeaRep.y[1]", dae, cfg) }}
rep={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.replicator[1].y[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("co2=(floor_fivZonVAV_gaiCO2_1_k * floor_fivZonVAV_gaiCO2_1_u)")
            && rendered.contains(
                "mwat=(floor_fivZonVAV_zon_1_mWat_flow_k * floor_fivZonVAV_zon_1_mWat_flow_u)"
            )
            && rendered.contains("cflow=floor_fivZonVAV_gaiCO2_1_y")
            && rendered.contains("ctot_u1=floor_fivZonVAV_zon_1_C_flow_1")
            && rendered.contains("ctot_u2=floor_fivZonVAV_zon_1_QPeaRep_y_1")
            && rendered.contains("ctot=((floor_fivZonVAV_zon_1_CTot_flow_k1_1 * floor_fivZonVAV_zon_1_CTot_flow_u1_1) + (floor_fivZonVAV_zon_1_CTot_flow_k2_1 * floor_fivZonVAV_zon_1_CTot_flow_u2_1))")
            && rendered.contains("qpea=floor_fivZonVAV_zon_1_QPeaRep_u")
            && rendered.contains("rep=floor_fivZonVAV_replicator_1_u"),
        "BOPTEST-named Modelica Gain and Replicator blocks should lower from their source equations:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_mass_flow_source_weather_data_bus_reads() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "building.weaDat.TDryBul": {},
            "building.weaDat.pAtm": {},
            "floor.fivZonVAV.infAir[1].TDryBul": {},
            "floor.fivZonVAV.infAir[1].pAtm": {},
            "floor.fivZonVAV.out.TDryBul": {},
            "floor.fivZonVAV.out.pAtm": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
inf_t={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.infAir[1].TDryBul", dae, cfg) }}
inf_p={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.infAir[1].pAtm", dae, cfg) }}
out_t={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.out.TDryBul", dae, cfg) }}
out_p={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.out.pAtm", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("inf_t=building_weaDat_TDryBul")
            && rendered.contains("inf_p=building_weaDat_pAtm")
            && rendered.contains("out_t=building_weaDat_TDryBul")
            && rendered.contains("out_p=building_weaDat_pAtm"),
        "BOPTEST MassFlowSource_WeatherData should lower TDryBul/pAtm from the weather-bus source reads:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_air_source_trace_inputs() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor.fivZonVAV.infAir[1].X_in_internal[1]": {},
            "floor.fivZonVAV.infAir[1].C[1]": {},
            "floor.fivZonVAV.infAir[1].Xi_in_internal[1]": {},
            "floor.fivZonVAV.infAir[1].C_in_internal[1]": {},
            "floor.fivZonVAV.infAir[1].ports[1].Xi_outflow[1]": {},
            "floor.fivZonVAV.infAir[1].ports[1].C_outflow[1]": {},
            "floor.fivZonVAV.exfAir[2].C_in[1]": {},
            "floor.fivZonVAV.exfAir[2].C_in_internal[1]": {},
            "floor.fivZonVAV.exfAir[2].Xi_in_internal[1]": {},
            "floor.fivZonVAV.exfAir[2].X_in_internal[1]": {},
            "floor.fivZonVAV.exfAir[2].ports[1].Xi_outflow[1]": {},
            "floor.fivZonVAV.exfAir[2].ports[1].C_outflow[1]": {},
            "floor.fivZonVAV.replicator[2].y[1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
inf_xi={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.infAir[1].Xi_in_internal[1]", dae, cfg) }}
inf_c={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.infAir[1].C_in_internal[1]", dae, cfg) }}
inf_port_xi={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.infAir[1].ports[1].Xi_outflow[1]", dae, cfg) }}
inf_port_c={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.infAir[1].ports[1].C_outflow[1]", dae, cfg) }}
exf_cin={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.exfAir[2].C_in[1]", dae, cfg) }}
exf_c={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.exfAir[2].C_in_internal[1]", dae, cfg) }}
exf_xi={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.exfAir[2].Xi_in_internal[1]", dae, cfg) }}
exf_port_xi={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.exfAir[2].ports[1].Xi_outflow[1]", dae, cfg) }}
exf_port_c={{ alg_rhs_for_var_with_dae("floor.fivZonVAV.exfAir[2].ports[1].C_outflow[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("inf_xi=floor_fivZonVAV_infAir_1_X_in_internal_1")
            && rendered.contains("inf_c=floor_fivZonVAV_infAir_1_C_1")
            && rendered.contains("inf_port_xi=floor_fivZonVAV_infAir_1_Xi_in_internal_1")
            && rendered.contains("inf_port_c=floor_fivZonVAV_infAir_1_C_in_internal_1")
            && rendered.contains("exf_cin=floor_fivZonVAV_replicator_2_y_1")
            && rendered.contains("exf_c=floor_fivZonVAV_exfAir_2_C_in_1")
            && rendered.contains("exf_xi=floor_fivZonVAV_exfAir_2_X_in_internal_1")
            && rendered.contains("exf_port_xi=floor_fivZonVAV_exfAir_2_Xi_in_internal_1")
            && rendered.contains("exf_port_c=floor_fivZonVAV_exfAir_2_C_in_internal_1"),
        "BOPTEST air source trace inputs should lower from MassFlowSource source equations and FiveZoneVAV connections:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_outdoor_air_boundary_ports() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "outdoorAirSource[1].ports[1].Xi_outflow[1]": {},
            "outdoorAirSource[1].ports[2].Xi_outflow[1]": {},
            "outdoorAirSource[1].ports[3].Xi_outflow[1]": {},
            "outdoorAirSource[1].ports[3].m_flow": {},
            "floor1BoptestAirNetwork.port_Exh_Air.m_flow": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
xi1={{ alg_rhs_for_var_with_dae("outdoorAirSource[1].ports[1].Xi_outflow[1]", dae, cfg) }}
xi2={{ alg_rhs_for_var_with_dae("outdoorAirSource[1].ports[2].Xi_outflow[1]", dae, cfg) }}
xi3={{ alg_rhs_for_var_with_dae("outdoorAirSource[1].ports[3].Xi_outflow[1]", dae, cfg) }}
unused_m={{ alg_rhs_for_var_with_dae("outdoorAirSource[1].ports[3].m_flow", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("xi1=0.01")
            && rendered.contains("xi2=0.01")
            && rendered.contains("xi3=0.01")
            && rendered.contains("unused_m=0.0"),
        "BOPTEST outdoor-air Boundary_pT compatibility wrapper should expose Medium.X_default[1] and zero flow for its unconnected third port:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "outdoor-air boundary source equations should avoid no-equation fallbacks:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_thermodynamic_state_species_from_port_stream() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
x1={{ alg_rhs_for_var_with_dae("coil.ele[1].state_a2_inflow.X[1]", dae, cfg) }}
x2={{ alg_rhs_for_var_with_dae("coil.ele[1].state_a2_inflow.X[2]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("x1=coil_ele_1_port_a2_Xi_outflow_1")
            && rendered.contains("x2=(1.0 - coil_ele_1_port_a2_Xi_outflow_1)"),
        "thermodynamic state X should come from the matching reduced-composition port stream:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_moist_air_medium_density_fields() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.dT": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.T": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.X[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.X[2]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.d": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.R_s": {},
            "plant.vol.dynBal.medium.p": {},
            "plant.vol.dynBal.medium.T": {},
            "plant.vol.dynBal.medium.X[1]": {},
            "plant.vol.dynBal.medium.d": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
rs={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.R_s", dae, cfg) }}
den={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.d", dae, cfg) }}
water={{ alg_rhs_for_var_with_dae("plant.vol.dynBal.medium.d", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "rs=(287.05 * floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_X_2 + 461.52 * floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_X_1)"
        ),
        "moist-air medium R_s should follow Buildings Air BaseProperties gas-constant mixture equation:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "den=(((287.05 * floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_X_2 + 461.52 * floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_X_1) > 1e-9 && (floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_dT + 273.15) > 1e-9) ? (floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_p / ((287.05 * floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_X_2 + 461.52 * floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_X_1) * (floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_dT + 273.15))) : 1.2)"
        ),
        "moist-air medium density should follow d=p/(R_s*T) using the current dT-derived temperature:\n{rendered}"
    );
    assert!(
        rendered.contains("water=0.0"),
        "single-substance water-like media without X[2] should not use the moist-air formula:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_internal_volume_port_enthalpy_flow() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "plant.vol.ports[2].m_flow": {},
            "plant.vol.ports[2].h_outflow": {},
            "plant.vol.ports_H_flow[2]": {},
            "plant.vol.dynBal.ports[2].m_flow": {},
            "plant.vol.dynBal.ports[2].h_outflow": {},
            "plant.vol.dynBal.ports_H_flow[2]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
vol={{ alg_rhs_for_var_with_dae("plant.vol.ports_H_flow[2]", dae, cfg) }}
dyn={{ alg_rhs_for_var_with_dae("plant.vol.dynBal.ports_H_flow[2]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("vol=(plant_vol_ports_2_m_flow * plant_vol_ports_2_h_outflow)"),
        "internal fluid volume port enthalpy flow should use m_flow * h_outflow:\n{rendered}"
    );
    assert!(
        rendered
            .contains("dyn=(plant_vol_dynBal_ports_2_m_flow * plant_vol_dynBal_ports_2_h_outflow)"),
        "internal dynBal port enthalpy flow should use m_flow * h_outflow:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "ports_H_flow should not fall back to a zero warning when its port stream variables are available:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_internal_volume_species_flow() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "room.vol.medium.Xi[1]": {},
            "room.vol.ports[2].m_flow": {},
            "room.vol.ports[2].Xi_outflow[1]": {},
            "room.vol.ports_mXi_flow[2,1]": {},
            "room.vol.C[1]": {},
            "room.vol.ports[2].C_outflow[1]": {},
            "room.vol.ports_mC_flow[2,1]": {},
            "room.vol.dynBal.ports[2].m_flow": {},
            "room.vol.dynBal.ports[2].Xi_outflow[1]": {},
            "room.vol.dynBal.ports_mXi_flow[2,1]": {},
            "room.vol.dynBal.ports[2].C_outflow[1]": {},
            "room.vol.dynBal.ports_mC_flow[2,1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
port={{ alg_rhs_for_var_with_dae("room.vol.ports[2].Xi_outflow[1]", dae, cfg) }}
flow={{ alg_rhs_for_var_with_dae("room.vol.ports_mXi_flow[2,1]", dae, cfg) }}
dyn_port={{ alg_rhs_for_var_with_dae("room.vol.dynBal.ports[2].Xi_outflow[1]", dae, cfg) }}
dyn_flow={{ alg_rhs_for_var_with_dae("room.vol.dynBal.ports_mXi_flow[2,1]", dae, cfg) }}
c_port={{ alg_rhs_for_var_with_dae("room.vol.ports[2].C_outflow[1]", dae, cfg) }}
c_flow={{ alg_rhs_for_var_with_dae("room.vol.ports_mC_flow[2,1]", dae, cfg) }}
c_dyn_port={{ alg_rhs_for_var_with_dae("room.vol.dynBal.ports[2].C_outflow[1]", dae, cfg) }}
c_dyn_flow={{ alg_rhs_for_var_with_dae("room.vol.dynBal.ports_mC_flow[2,1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("port=room_vol_medium_Xi_1"),
        "PartialLumpedVolume port Xi_outflow should follow medium.Xi:\n{rendered}"
    );
    assert!(
        rendered.contains("flow=(room_vol_ports_2_m_flow * room_vol_ports_2_Xi_outflow_1)"),
        "PartialLumpedVolume ports_mXi_flow should use m_flow * Xi_outflow:\n{rendered}"
    );
    assert!(
        rendered.contains("dyn_port=room_vol_medium_Xi_1")
            && rendered.contains(
                "dyn_flow=(room_vol_dynBal_ports_2_m_flow * room_vol_dynBal_ports_2_Xi_outflow_1)"
            ),
        "dynBal species ports should follow the ConservationEquation medium species state:\n{rendered}"
    );
    assert!(
        rendered.contains("c_port=room_vol_C_1")
            && rendered.contains("c_flow=(room_vol_ports_2_m_flow * room_vol_ports_2_C_outflow_1)"),
        "PartialLumpedVolume trace-substance C_outflow and ports_mC_flow should follow C and m_flow*C_outflow:\n{rendered}"
    );
    assert!(
        rendered.contains("c_dyn_port=room_vol_C_1")
            && rendered.contains(
                "c_dyn_flow=(room_vol_dynBal_ports_2_m_flow * room_vol_dynBal_ports_2_C_outflow_1)"
            ),
        "dynBal trace-substance ports should follow the ConservationEquation trace state:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "internal volume species flow should not fall back to zero warnings:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_dynamic_fluid_sensor_gain_chain() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "plant.sensor.mNor_flow": {},
            "plant.sensor.k": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
mnor={{ alg_rhs_for_var_with_dae("plant.sensor.mNor_flow", dae, cfg) }}
k={{ alg_rhs_for_var_with_dae("plant.sensor.k", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "mnor=(plant_sensor_dynamic ? ((fabs(plant_sensor_m_flow_nominal) > 1e-12) ? (plant_sensor_port_a_m_flow / plant_sensor_m_flow_nominal) : 1.0) : 1.0)"
        ),
        "dynamic sensor mNor_flow should preserve the Buildings normalized-flow equation with a finite nominal-flow guard:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "k=(plant_sensor_dynamic ? Modelica_Fluid_Utilities_regStep(__rumoca_named_arg___x(plant_sensor_port_a_m_flow), __rumoca_named_arg___y1(plant_sensor_mNor_flow), __rumoca_named_arg___y2(-plant_sensor_mNor_flow), __rumoca_named_arg___x_small(plant_sensor_m_flow_small)) : 1.0)"
        ),
        "dynamic sensor k should preserve the Buildings regStep gain chain:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "dynamic sensor gain chain should not fall back to zero warnings:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_temperature_sensor_inflow_chain() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "plant.senT.port_a.p": {},
            "plant.senT.port_a.h_outflow": {},
            "plant.senT.port_a.Xi_outflow": {},
            "plant.senT.port_b.p": {},
            "plant.senT.port_b.h_outflow": {},
            "plant.senT.port_b.Xi_outflow": {},
            "plant.senT.T_a_inflow": {},
            "plant.senT.T_b_inflow": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
ta={{ alg_rhs_for_var_with_dae("plant.senT.T_a_inflow", dae, cfg) }}
tb={{ alg_rhs_for_var_with_dae("plant.senT.T_b_inflow", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "ta=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(plant_senT_port_b_p, plant_senT_port_b_h_outflow, plant_senT_port_b_Xi_outflow, plant_senT_port_b_Xi_outflow__len)"
        ),
        "TemperatureTwoPort T_a_inflow should use the Buildings opposite-port source equation:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "tb=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(plant_senT_port_a_p, plant_senT_port_a_h_outflow, plant_senT_port_a_Xi_outflow, plant_senT_port_a_Xi_outflow__len)"
        ),
        "TemperatureTwoPort T_b_inflow should use the Buildings opposite-port source equation:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "temperature sensor inflow temperatures should not fall back to zero warnings:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_vav_temperature_sensor_inflows() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TEnt.port_a.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TEnt.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TEnt.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TEnt.port_b.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TEnt.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TEnt.port_b.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TLea.port_a.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TLea.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TLea.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TLea.port_b.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TLea.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TLea.port_b.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temEntWat.port_a.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temEntWat.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temEntWat.port_a.Xi_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temEntWat.port_b.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temEntWat.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temEntWat.port_b.Xi_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temLeaAir.port_a.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temLeaAir.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temLeaAir.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temLeaAir.port_b.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temLeaAir.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temLeaAir.port_b.Xi_outflow[1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
tent_a={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TEnt.T_a_inflow", dae, cfg) }}
tlea_b={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TLea.T_b_inflow", dae, cfg) }}
wat_a={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temEntWat.T_a_inflow", dae, cfg) }}
air_b={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.temLeaAir.T_b_inflow", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "tent_a=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TEnt_port_b_p, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TEnt_port_b_h_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TEnt_port_b_Xi_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TEnt_port_b_Xi_outflow__len)"
        ),
        "VAV TEnt T_a_inflow should read the opposite port stream temperature:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "tlea_b=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TLea_port_a_p, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TLea_port_a_h_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TLea_port_a_Xi_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TLea_port_a_Xi_outflow__len)"
        ),
        "VAV TLea T_b_inflow should read the opposite port stream temperature:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "wat_a=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_heaCoil_temEntWat_port_b_p, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_heaCoil_temEntWat_port_b_h_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_heaCoil_temEntWat_port_b_Xi_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_heaCoil_temEntWat_port_b_Xi_outflow__len)"
        ),
        "reheat water temperature sensor should use the same Buildings TemperatureTwoPort equation:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "air_b=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_heaCoil_temLeaAir_port_a_p, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_heaCoil_temLeaAir_port_a_h_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_heaCoil_temLeaAir_port_a_Xi_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_heaCoil_temLeaAir_port_a_Xi_outflow__len)"
        ),
        "reheat air temperature sensor should use the same Buildings TemperatureTwoPort equation:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "BOPTEST VAV temperature sensor inflows should not fall back to zero warnings:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_ahu_temperature_sensor_inflows() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TEntWat.port_a.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TEntWat.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TEntWat.port_a.Xi_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TEntWat.port_b.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TEntWat.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TEntWat.port_b.Xi_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TEntWat.T_a_inflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TLeaAir.port_a.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TLeaAir.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TLeaAir.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TLeaAir.port_b.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TLeaAir.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TLeaAir.port_b.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TLeaAir.T_b_inflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TOutSen.port_a.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TOutSen.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TOutSen.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TOutSen.port_b.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TOutSen.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TOutSen.port_b.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TOutSen.T_a_inflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TMix.port_a.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TMix.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TMix.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TMix.port_b.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TMix.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TMix.port_b.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TMix.T_b_inflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.temSen_1.port_a.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.temSen_1.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.temSen_1.port_a.Xi_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.temSen_1.port_b.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.temSen_1.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.temSen_1.port_b.Xi_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.temSen_1.T_a_inflow": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
tent_wat={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TEntWat.T_a_inflow", dae, cfg) }}
tlea_air={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TLeaAir.T_b_inflow", dae, cfg) }}
tout={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TOutSen.T_a_inflow", dae, cfg) }}
tmix={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TMix.T_b_inflow", dae, cfg) }}
temsen={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.cooCoi.temSen_1.T_a_inflow", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "tent_wat=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TEntWat_port_b_p, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TEntWat_port_b_h_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TEntWat_port_b_Xi_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TEntWat_port_b_Xi_outflow__len)"
        ),
        "AHU cooling-coil TEntWat should use TemperatureTwoPort opposite port_b stream:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "tlea_air=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TLeaAir_port_a_p, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TLeaAir_port_a_h_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TLeaAir_port_a_Xi_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TLeaAir_port_a_Xi_outflow__len)"
        ),
        "AHU cooling-coil TLeaAir should use TemperatureTwoPort opposite port_a stream:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "tout=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TOutSen_port_b_p, floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TOutSen_port_b_h_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TOutSen_port_b_Xi_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TOutSen_port_b_Xi_outflow__len)"
        ),
        "mixing-box TOutSen should use TemperatureTwoPort opposite port_b stream:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "tmix=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TMix_port_a_p, floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TMix_port_a_h_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TMix_port_a_Xi_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TMix_port_a_Xi_outflow__len)"
        ),
        "mixing-box TMix should use TemperatureTwoPort opposite port_a stream:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "temsen=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_temSen_1_port_b_p, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_temSen_1_port_b_h_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_temSen_1_port_b_Xi_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_cooCoi_temSen_1_port_b_Xi_outflow__len)"
        ),
        "Buildings coil temSen_1 should use TemperatureTwoPort opposite port_b stream:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "BOPTEST AHU temperature sensor inflows should not fall back to zero warnings:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_volume_flow_sensor_density_inflows() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.port_a.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.port_b.p": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.port_b.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.d_a_inflow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.d_b_inflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.port_a.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.port_a.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.port_a.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.port_b.p": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.port_b.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.port_b.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.rho_a_inflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.rho_b_inflow": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
sup_a={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.d_a_inflow", dae, cfg) }}
sup_b={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.d_b_inflow", dae, cfg) }}
vav_a={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.rho_a_inflow", dae, cfg) }}
vav_b={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.V_flowLea.rho_b_inflow", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "sup_a=__rumoca_media_density_pTX(floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_b_p, Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_b_p, floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_b_h_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_b_Xi_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_b_Xi_outflow__len), floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_b_Xi_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_b_Xi_outflow__len)"
        ),
        "VolumeFlowRate d_a_inflow should use the opposite port_b density state:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "sup_b=__rumoca_media_density_pTX(floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_a_p, Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_a_p, floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_a_h_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_a_Xi_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_a_Xi_outflow__len), floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_a_Xi_outflow, floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_port_a_Xi_outflow__len)"
        ),
        "VolumeFlowRate d_b_inflow should use the opposite port_a density state:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "vav_a=__rumoca_media_density_pTX(floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_b_p, Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_b_p, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_b_h_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_b_Xi_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_b_Xi_outflow__len), floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_b_Xi_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_b_Xi_outflow__len)"
        ),
        "VAV V_flowLea rho_a_inflow should use the opposite port_b density state:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "vav_b=__rumoca_media_density_pTX(floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_a_p, Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_a_p, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_a_h_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_a_Xi_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_a_Xi_outflow__len), floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_a_Xi_outflow, floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_V_flowLea_port_a_Xi_outflow__len)"
        ),
        "VAV V_flowLea rho_b_inflow should use the opposite port_a density state:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "BOPTEST volume-flow sensor density inflows should not fall back to zero warnings:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_trace_substances_two_port_cmed() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senCO2RetAir.port_a.m_flow": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senCO2RetAir.port_a.C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senCO2RetAir.port_b.C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senCO2RetAir.s[1]": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senCO2RetAir.m_flow_small": {},
            "floor1BoptestAirNetwork.floor.duaFanAirHanUni.senCO2RetAir.CMed": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.CMed": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
cmed={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.duaFanAirHanUni.senCO2RetAir.CMed", dae, cfg) }}
zone={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.CMed", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "cmed=Modelica_Fluid_Utilities_regStep(__rumoca_named_arg___x(floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_port_a_m_flow), __rumoca_named_arg___y1((floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_s_1 * floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_port_b_C_outflow_1)), __rumoca_named_arg___y2((floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_s_1 * floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_port_a_C_outflow_1)), __rumoca_named_arg___x_small(floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_m_flow_small))"
        ),
        "TraceSubstancesTwoPort CMed should follow the Buildings reversible-stream regStep equation:\n{rendered}"
    );
    assert!(
        rendered.contains("zone=0.0"),
        "TraceSubstancesTwoPort CMed synthesis must stay scoped to trace-substance sensor components:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_vav_pressure_sensor_stream_defaults() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.pEnt.port.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.pEnt.port.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.pEnt.port.C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.pLea.port.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.preAirEnt.port.Xi_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.preAirEnt.port.C_outflow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.preAirLea.port.h_outflow": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.preAirLea.port.C_outflow[1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
h={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.pEnt.port.h_outflow", dae, cfg) }}
xi={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.pEnt.port.Xi_outflow[1]", dae, cfg) }}
c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.pEnt.port.C_outflow[1]", dae, cfg) }}
plea={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.pLea.port.Xi_outflow[1]", dae, cfg) }}
pre_xi={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.preAirEnt.port.Xi_outflow[1]", dae, cfg) }}
pre_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.preAirEnt.port.C_outflow[1]", dae, cfg) }}
pre_h={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.preAirLea.port.h_outflow", dae, cfg) }}
pre_lea_c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.heaCoil.preAirLea.port.C_outflow[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    for label in [
        "h=0.0",
        "xi=0.0",
        "c=0.0",
        "plea=0.0",
        "pre_xi=0.0",
        "pre_c=0.0",
        "pre_h=0.0",
        "pre_lea_c=0.0",
    ] {
        assert!(
            rendered.contains(label),
            "BOPTEST VAV pressure sensor stream defaults should follow Buildings PartialAbsoluteSensor source equations:\n{rendered}"
        );
    }
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "BOPTEST VAV pressure sensor stream defaults should not fall back to warning zeros:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_zone_volume_trace_substance_without_alias_guard() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
c={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.C[1]", dae, cfg) }}
port={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports[2].C_outflow[1]", dae, cfg) }}
flow={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mC_flow[2,1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("c=((fabs(floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_m) > 1e-12) ? (floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_mC_1 / floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_m) : 0.0)"),
        "BOPTEST zone ConservationEquation C should lower from mC/m even when the template alias set is incomplete:\n{rendered}"
    );
    assert!(
        rendered.contains("port=floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_C_1")
            && rendered.contains("flow=(floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_2_m_flow * floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_2_C_outflow_1)"),
        "BOPTEST zone ConservationEquation trace ports should lower from C and m_flow*C_outflow:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "volume trace-substance lowering should avoid no-equation warning fallbacks:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_zone_volume_conservation_summaries() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.m": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.Xi[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.mXi[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.mbXi_flow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.mbC_flow[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.C_flow_internal[1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mXi_flow[1,1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mXi_flow[2,1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mXi_flow[3,1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mXi_flow[4,1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mXi_flow[5,1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mC_flow[1,1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mC_flow[2,1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mC_flow[3,1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mC_flow[4,1]": {},
            "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.ports_mC_flow[5,1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
mxi={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.mXi[1]", dae, cfg) }}
mbxi={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.mbXi_flow[1]", dae, cfg) }}
mbc={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.mbC_flow[1]", dae, cfg) }}
cflow={{ alg_rhs_for_var_with_dae("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.C_flow_internal[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("mxi=(floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_m * floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_medium_Xi_1)"),
        "BOPTEST zone volume mXi should follow the ConservationEquation mass-species definition:\n{rendered}"
    );
    assert!(
        rendered.contains("mbxi=(floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mXi_flow_1_1 + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mXi_flow_2_1 + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mXi_flow_3_1 + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mXi_flow_4_1 + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mXi_flow_5_1)"),
        "BOPTEST zone volume mbXi_flow should sum the five port species flows:\n{rendered}"
    );
    assert!(
        rendered.contains("mbc=(floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mC_flow_1_1 + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mC_flow_2_1 + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mC_flow_3_1 + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mC_flow_4_1 + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_ports_mC_flow_5_1)"),
        "BOPTEST zone volume mbC_flow should sum the five port trace flows:\n{rendered}"
    );
    assert!(
        rendered.contains("cflow=0.0"),
        "BOPTEST zone volume without a conditional trace input should use zero C_flow_internal:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "zone volume ConservationEquation summary lowering should avoid no-equation warning fallbacks:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_fluid_state_record_fields() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "plant.chi.port_a1.p": {},
            "plant.chi.port_a1.h_outflow": {},
            "plant.chi.port_a1.Xi_outflow": {},
            "plant.chi.port_b2.p": {},
            "plant.chi.port_b2.h_outflow": {},
            "plant.chi.port_b2.Xi_outflow": {},
            "plant.boi.port_a.p": {},
            "plant.boi.port_a.h_outflow": {},
            "plant.boi.port_a.Xi_outflow": {},
            "plant.chi.state_a1_inflow.p": {},
            "plant.chi.state_b2_inflow.T": {},
            "plant.boi.sta_a.T": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
p={{ alg_rhs_for_var_with_dae("plant.chi.state_a1_inflow.p", dae, cfg) }}
t4={{ alg_rhs_for_var_with_dae("plant.chi.state_b2_inflow.T", dae, cfg) }}
t2={{ alg_rhs_for_var_with_dae("plant.boi.sta_a.T", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("p=plant_chi_port_a1_p"),
        "ThermodynamicState.p should preserve the setState_phX source pressure:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "t4=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(plant_chi_port_b2_p, plant_chi_port_b2_h_outflow, plant_chi_port_b2_Xi_outflow, plant_chi_port_b2_Xi_outflow__len)"
        ),
        "four-port ThermodynamicState.T should recover temperature from the source port state:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "t2=Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX(plant_boi_port_a_p, plant_boi_port_a_h_outflow, plant_boi_port_a_Xi_outflow, plant_boi_port_a_Xi_outflow__len)"
        ),
        "two-port ThermodynamicState.T should recover temperature from the source port state:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "thermodynamic state fields should not fall back to zero warnings:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_fixed_resistance_velocity() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "duct.res.m_flow": {},
            "duct.res.rho_default": {},
            "duct.res.ARound": {},
            "duct.res.v": {},
            "duct.unbound.v": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
v={{ alg_rhs_for_var_with_dae("duct.res.v", dae, cfg) }}
missing={{ alg_rhs_for_var_with_dae("duct.unbound.v", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "v=((fabs(duct_res_rho_default * duct_res_ARound) > 1e-12) ? (duct_res_m_flow / (duct_res_rho_default * duct_res_ARound)) : 0.0)"
        ),
        "HydraulicDiameter velocity should preserve v=m_flow/(rho_default*ARound) with a finite denominator guard:\n{rendered}"
    );
    assert!(
        rendered.contains("WARNING: no equation found for duct.unbound.v"),
        "velocity synthesis must require the source m_flow/rho_default/ARound aliases:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_internal_volume_medium_fields() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "y": {
            "chilledWaterPlant.loop.pump.vol.p": {},
            "chilledWaterPlant.loop.pump.vol.ports[1].p": {},
            "chilledWaterPlant.loop.pump.vol.dynBal.medium.X[1]": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.vol1.p": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.vol1.ports[1].p": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.vol1.dynBal.medium.X[1]": {},
            "floor.zone.vol.m": {},
            "floor.zone.vol.mC[1]": {},
            "floor.zone.vol.mXi[1]": {},
            "floor.zone.vol.medium.Xi[1]": {},
            "floor.zone.vol.C[1]": {},
            "floor.zone.vol.COut[1]": {},
            "floor.zone.vol.COut_internal[1]": {},
            "floor.zone.vol.mCOut[1]": {},
            "floor.zone.vol.XiOut[1]": {},
            "floor.zone.vol.XiOut_internal[1]": {},
            "floor.zone.vol.mXiOut[1]": {},
            "floor.zone.vol.dynBal.COut[1]": {},
            "floor.zone.vol.dynBal.mCOut[1]": {},
            "floor.zone.vol.dynBal.XiOut[1]": {},
            "floor.zone.vol.dynBal.mXiOut[1]": {}
        }
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
p={{ alg_rhs_for_var_with_dae("chilledWaterPlant.loop.pump.vol.p", dae, cfg) }}
x={{ alg_rhs_for_var_with_dae("chilledWaterPlant.loop.pump.vol.dynBal.medium.X[1]", dae, cfg) }}
chp={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.vol1.p", dae, cfg) }}
chx={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.vol1.dynBal.medium.X[1]", dae, cfg) }}
c={{ alg_rhs_for_var_with_dae("floor.zone.vol.C[1]", dae, cfg) }}
cout={{ alg_rhs_for_var_with_dae("floor.zone.vol.COut[1]", dae, cfg) }}
couti={{ alg_rhs_for_var_with_dae("floor.zone.vol.COut_internal[1]", dae, cfg) }}
mcout={{ alg_rhs_for_var_with_dae("floor.zone.vol.mCOut[1]", dae, cfg) }}
xiout={{ alg_rhs_for_var_with_dae("floor.zone.vol.XiOut[1]", dae, cfg) }}
xiouti={{ alg_rhs_for_var_with_dae("floor.zone.vol.XiOut_internal[1]", dae, cfg) }}
mxiout={{ alg_rhs_for_var_with_dae("floor.zone.vol.mXiOut[1]", dae, cfg) }}
dyn_cout={{ alg_rhs_for_var_with_dae("floor.zone.vol.dynBal.COut[1]", dae, cfg) }}
dyn_mcout={{ alg_rhs_for_var_with_dae("floor.zone.vol.dynBal.mCOut[1]", dae, cfg) }}
dyn_xiout={{ alg_rhs_for_var_with_dae("floor.zone.vol.dynBal.XiOut[1]", dae, cfg) }}
dyn_mxiout={{ alg_rhs_for_var_with_dae("floor.zone.vol.dynBal.mXiOut[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("p=chilledWaterPlant_loop_pump_vol_ports_1_p"),
        "internal fluid volume pressure should preserve the source port pressure equality:\n{rendered}"
    );
    assert!(
        rendered.contains("chp=chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_ports_1_p"),
        "numbered internal fluid volume pressure should preserve the source port pressure equality:\n{rendered}"
    );
    assert!(
        rendered.contains("x=1.0") && rendered.contains("chx=1.0"),
        "single-substance water volume composition should use the source medium mass fraction:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "c=((fabs(floor_zone_vol_m) > 1e-12) ? (floor_zone_vol_mC_1 / floor_zone_vol_m) : 0.0)"
        ) && rendered.contains("cout=floor_zone_vol_C_1")
            && rendered.contains("couti=floor_zone_vol_C_1")
            && rendered.contains("mcout=floor_zone_vol_mC_1"),
        "trace substance outputs should follow mC=m*C and Buildings ConservationEquation outputs:\n{rendered}"
    );
    assert!(
        rendered.contains("xiout=floor_zone_vol_medium_Xi_1")
            && rendered.contains("xiouti=floor_zone_vol_medium_Xi_1")
            && rendered.contains("mxiout=floor_zone_vol_mXi_1"),
        "substance outputs should follow medium.Xi and mXi:\n{rendered}"
    );
    assert!(
        rendered.contains("dyn_cout=floor_zone_vol_C_1")
            && rendered.contains("dyn_mcout=floor_zone_vol_mC_1")
            && rendered.contains("dyn_xiout=floor_zone_vol_medium_Xi_1")
            && rendered.contains("dyn_mxiout=floor_zone_vol_mXi_1"),
        "dynBal ConservationEquation outputs should stay connected to the parent volume state:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "internal fluid volume medium fields should not fall back to zero warnings:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_preserves_boptest_constant_speed_pump_chain() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
pri_flow={{ alg_rhs_for_var("chilledWaterPlant.chillerPlant.pumPriCHW.pumConSpe[1].m_flow_in", dae.f_x, cfg) }}
cw_flow={{ alg_rhs_for_var("chilledWaterPlant.chillerPlant.pumCW.pumConSpe[2].m_flow_in", dae.f_x, cfg) }}
pri_volume={{ alg_rhs_for_var("chilledWaterPlant.chillerPlant.pumPriCHW.pumConSpe[1].preSou.V_flow", dae.f_x, cfg) }}
cw_volume={{ alg_rhs_for_var("chilledWaterPlant.chillerPlant.pumCW.pumConSpe[2].preSou.V_flow", dae.f_x, cfg) }}
pri_power={{ alg_rhs_for_var("chilledWaterPlant.chillerPlant.pumPriCHW.P[1]", dae.f_x, cfg) }}
cw_power={{ alg_rhs_for_var("chilledWaterPlant.chillerPlant.pumCW.P[2]", dae.f_x, cfg) }}
pri_qthe={{ alg_rhs_for_var("chilledWaterPlant.chillerPlant.pumPriCHW.pumConSpe[1].heaDis.QThe_flow", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "pri_flow=fmax(0.0, chilledWaterPlant_chillerPlant_pumPriCHW_m_flow_nominal_1 * chilledWaterPlant_chillerPlant_pumPriCHW_On_1)"
        )
            && rendered.contains(
                "cw_flow=fmax(0.0, chilledWaterPlant_chillerPlant_pumCW_m_flow_nominal_2 * chilledWaterPlant_chillerPlant_pumCW_On_2)"
            ),
        "BOPTEST SimPumpSystem mass-flow inputs should follow the original Gain(k=m_flow_nominal) * On chain:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "pri_volume=(chilledWaterPlant_chillerPlant_pumPriCHW_pumConSpe_1_m_flow_in / fmax(0.00001, chilledWaterPlant_chillerPlant_pumPriCHW_pumConSpe_1_rho_default))"
        )
            && rendered.contains(
                "cw_volume=(chilledWaterPlant_chillerPlant_pumCW_pumConSpe_2_m_flow_in / fmax(0.00001, chilledWaterPlant_chillerPlant_pumCW_pumConSpe_2_rho_default))"
            ),
        "BOPTEST SimPumpSystem prescribed source volume flow should be the pump mass-flow input over density:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "pri_power=fmax(0.0, (chilledWaterPlant_chillerPlant_pumPriCHW_pumConSpe_1_m_flow_in / 996.0) * chilledWaterPlant_chillerPlant_pumPriCHW_dp_nominal"
        )
            && rendered.contains(
                "cw_power=fmax(0.0, (chilledWaterPlant_chillerPlant_pumCW_pumConSpe_2_m_flow_in / 996.0) * chilledWaterPlant_chillerPlant_pumCW_dp_nominal"
            ),
        "BOPTEST SimPumpSystem power should read the original mover m_flow_in and dp_nominal fields:\n{rendered}"
    );
    assert!(
        !rendered.contains("pumPriCHW_m_flow_nominal_1 * chilledWaterPlant_chillerPlant_pumPriCHW_On_1) / 996.0) * chilledWaterPlant_chillerPlant_dPCHW_nominal")
            && !rendered.contains("pumCW_m_flow_nominal_2 * chilledWaterPlant_chillerPlant_pumCW_On_2) / 996.0) * (chilledWaterPlant_chillerPlant_dPCW_nominal"),
        "constant-speed pump public power must not bypass the original SimPumpSystem chain:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "pri_qthe=fmax(0.0, (chilledWaterPlant_chillerPlant_pumPriCHW_pumConSpe_1_eff_WFlo / fmax(0.00001, chilledWaterPlant_chillerPlant_pumPriCHW_pumConSpe_1_eff_etaHyd)) - chilledWaterPlant_chillerPlant_pumPriCHW_pumConSpe_1_eff_WFlo)"
        )
            && !rendered.contains("pri_qthe=fmax(0.0, chilledWaterPlant_chillerPlant_pumPriCHW_pumConSpe_1_heaDis_PEle -"),
        "SimPumpSystem heat dissipation must follow PowerInterface WHyd-WFlo when motorCooledByFluid=false:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_propagates_boptest_plant_read_surfaces_from_pump_network_flow() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
{{ alg_rhs_for_var("chilledWaterPlant.secondaryChilledWaterMassFlow", dae.f_x, cfg) }}
{{ alg_rhs_for_var("chilledWaterPlant.chillerPlant.mCHW_tot", dae.f_x, cfg) }}
{{ alg_rhs_for_var("reaChiWatSys_mCHWTot_y", dae.f_x, cfg) }}
{{ alg_rhs_for_var("chilledWaterPlant.loopDifferentialPressure", dae.f_x, cfg) }}
{{ alg_rhs_for_var("reaChiWatSys_dp_y", dae.f_x, cfg) }}
{{ alg_rhs_for_var("hotWaterPlant.hotWaterDistributionMassFlow", dae.f_x, cfg) }}
{{ alg_rhs_for_var("hotWaterPlant.boilerPlant.mHW_tot", dae.f_x, cfg) }}
{{ alg_rhs_for_var("reaHotWatSys_mHWTot_y", dae.f_x, cfg) }}
{{ alg_rhs_for_var("hotWaterPlant.loopDifferentialPressure", dae.f_x, cfg) }}
{{ alg_rhs_for_var("reaHotWatSys_dp_y", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered
            .matches("__rumoca_mover_network_resistance_flow(6,")
            .count()
            >= 8,
        "chilled-water plant mass-flow and dp read surfaces should inline both secondary pump/network flow solves:\n{rendered}"
    );
    assert!(
        rendered
            .matches("__rumoca_mover_network_resistance_flow(5,")
            .count()
            >= 8,
        "hot-water plant mass-flow and dp read surfaces should inline both secondary pump/network flow solves:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "__rumoca_network_resistance_dp(chilledWaterPlant_chilledWaterStaticPressureSetpoint, (chilledWaterPlant_chillerPlant_pumSecCHW_pum_1_VolFloCur_5 + chilledWaterPlant_chillerPlant_pumSecCHW_pum_2_VolFloCur_5)"
        ),
        "chilled-water dp read surfaces should come from the total-flow network resistance curve:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "__rumoca_network_resistance_dp(hotWaterPlant_boptestHotWaterStaticPressureSetpoint, (hotWaterPlant_boilerPlant_pumSecHW_pum_1_VolFloCur_4 + hotWaterPlant_boilerPlant_pumSecHW_pum_2_VolFloCur_4)"
        ),
        "hot-water dp read surfaces should come from the total-flow network resistance curve:\n{rendered}"
    );
    assert!(
        !rendered.contains("chilledWaterPlant_chillerPlant_mCHW_tot\n")
            && !rendered.contains("hotWaterPlant_boilerPlant_mHW_tot\n")
            && !rendered.contains("chilledWaterPlant_secondaryChilledWaterMassFlow\n")
            && !rendered.contains("hotWaterPlant_hotWaterDistributionMassFlow\n"),
        "BOPTEST plant read surfaces must not preserve the aggregate alias cycles:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_propagates_boptest_ahu_power_read_surfaces() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
{{ alg_rhs_for_var("floor1_reaAHU_PFanSup_y", dae.f_x, cfg) }}
{{ alg_rhs_for_var("floor2_reaAHU_PFanRet_y", dae.f_x, cfg) }}
{{ alg_rhs_for_var("floor3_reaAHU_PFreCoi_y", dae.f_x, cfg) }}
{{ alg_rhs_for_var("floor1Ahu.coolingCoilPower", dae.f_x, cfg) }}
{{ alg_rhs_for_var("floor1BoptestAirNetwork.PFanSup", dae.f_x, cfg) }}
{{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.PFanSup", dae.f_x, cfg) }}
{{ alg_rhs_for_var("floor2BoptestAirNetwork.PFanRet", dae.f_x, cfg) }}
{{ alg_rhs_for_var("floor2BoptestAirNetwork.floor.duaFanAirHanUni.PFanRet", dae.f_x, cfg) }}
{{ alg_rhs_for_var("floor1_reaAHU_V_flowSup_y", dae.f_x, cfg) }}
{{ alg_rhs_for_var("floor1Ahu.supplyMassFlow", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("floor1BoptestAirNetwork_PFanSup")
            && rendered.contains("floor2BoptestAirNetwork_PFanRet")
            && rendered.contains("floor3BoptestAirNetwork_PFreCoi")
            && rendered.contains("floor1BoptestAirNetwork_PFreCoi")
            && rendered.contains(
                "__rumoca_mover_pressure_curve(4, ((fmax(0.0, controlSemantics_effectiveZoneTerminalAirflowSetpointCommand_1"
            )
            && rendered.contains("floor1BoptestAirNetwork_SupPreCur_1")
            && rendered.contains("floor1BoptestAirNetwork_HydEff_1")
            && rendered.contains("floor1BoptestAirNetwork_MotEff_1")
            && rendered.contains(
                "__rumoca_mover_pressure_curve(4, ((fmax(0.0, controlSemantics_effectiveZoneTerminalAirflowSetpointCommand_6"
            )
            && rendered.contains("floor2BoptestAirNetwork_RetPreCur_1")
            && rendered.contains("controlSemantics_rawAhuSupplyFanSpeed_1")
            && rendered.contains("controlSemantics_terminalDamperMean_1")
            && rendered.contains("floor1BoptestAirNetwork_V_flowSupAir")
            && rendered
                .contains("floor1BoptestAirNetwork_rhoAir * floor1BoptestAirNetwork_V_flowSupAir"),
        "AHU public power/flow read surfaces should synthesize from BOPTEST floor read surfaces:\n{rendered}"
    );
    assert!(
        !rendered.contains("floor_cooling_demand_3 *")
            && !rendered.contains("ahu_cooling_coil_command_1")
            && !rendered.contains("lower_floor_ahu_cooling_enable_1")
            && !rendered.contains("floor_physical_ahu_supply_mass_flow")
            && !rendered.contains("floor_physical_ahu_duct_static_pressure")
            && !rendered.contains("floor1PhysicalAhu_terminalToWaterDemand")
            && !rendered.contains("floor3PhysicalAhu_terminalToWaterDemand"),
        "AHU public power/read surfaces must not bypass BOPTEST floor wrapper through reduced AHU equations:\n{rendered}"
    );
    assert!(
        !rendered.contains("no equation found"),
        "AHU public read surfaces must not fall through to no-equation placeholders:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_propagates_boptest_floor_load_read_surfaces() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
chw_load={{ alg_rhs_for_var("floor1BoptestAirNetwork.chilledWaterThermalLoad", dae.f_x, cfg) }}
chw_tsup={{ alg_rhs_for_var("floor1BoptestAirNetwork.TSupCHW", dae.f_x, cfg) }}
chw_tret={{ alg_rhs_for_var("floor1BoptestAirNetwork.TRetCHW", dae.f_x, cfg) }}
chw_floor={{ alg_rhs_for_var("floor_cooling_demand[1]", dae.f_x, cfg) }}
chw_branch={{ alg_rhs_for_var("chilledWaterPlant.chilledWaterBranchDemand[2]", dae.f_x, cfg) }}
hw_load={{ alg_rhs_for_var("floor3BoptestAirNetwork.hotWaterReheatThermalLoad", dae.f_x, cfg) }}
hw_zone={{ alg_rhs_for_var("floor3BoptestAirNetwork.hotWaterReheatThermalLoadByZone[5]", dae.f_x, cfg) }}
hw_public={{ alg_rhs_for_var("floor3_reaZonWes_QTerHea_y", dae.f_x, cfg) }}
vav_flow={{ alg_rhs_for_var("floor2BoptestAirNetwork.Vflow[1]", dae.f_x, cfg) }}
vav_tsup={{ alg_rhs_for_var("floor2BoptestAirNetwork.TSup[1]", dae.f_x, cfg) }}
ahu_tsup={{ alg_rhs_for_var("floor1BoptestAirNetwork.TSupAir", dae.f_x, cfg) }}
ahu_mix={{ alg_rhs_for_var("floor1BoptestAirNetwork.TMixAir", dae.f_x, cfg) }}
hw_floor={{ alg_rhs_for_var("floor_terminal_reheat_equipment_demand[3]", dae.f_x, cfg) }}
hw_branch={{ alg_rhs_for_var("hotWaterPlant.hotWaterBranchDemand[3]", dae.f_x, cfg) }}
hw_total={{ alg_rhs_for_var("hotWaterPlant.terminalReheatDemand", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("floor1BoptestAirNetwork_rhoAir")
            && rendered.contains("floor1BoptestAirNetwork_cpAir")
            && rendered.contains("controlSemantics_rawAhuSupplyAirTemperatureSetpoint_1")
            && rendered.contains("controlSemantics_rawZoneHeatingSetpointCommand_1")
            && rendered.contains("controlSemantics_effectiveZoneTerminalAirflowSetpointCommand_1")
            && rendered.contains(
                "chw_tsup=fmax(275.15, fmin(285.15, chilledWaterPlant_plantChilledWaterSetpoint))"
            )
            && rendered.contains("chw_floor=fmax(0.0, (floor1BoptestAirNetwork_rhoAir")
            && rendered.contains("chw_branch=fmax(0.0, (floor2BoptestAirNetwork_rhoAir"),
        "CHW plant demand surfaces should be driven by BOPTEST floor air-side cooling load:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "hw_load=__rumoca_sum_d(floor3BoptestAirNetwork_hotWaterReheatThermalLoadByZone"
        )
            && rendered.contains("hw_zone=fmax(0.0, floor3BoptestAirNetwork_rhoAir * floor3BoptestAirNetwork_cpAir")
            && rendered.contains("controlSemantics_effectiveZoneTerminalAirflowSetpointCommand_15")
            && rendered.contains("controlSemantics_effectiveZoneTerminalReheatCommand_15")
            && rendered.contains("hw_public=floor3BoptestAirNetwork_hotWaterReheatThermalLoadByZone_5")
            && rendered.contains("vav_flow=fmax(0.0, controlSemantics_effectiveZoneTerminalAirflowSetpointCommand_6")
            && rendered.contains("vav_tsup=(floor2BoptestAirNetwork_TSupAir +")
            && rendered.contains("ahu_tsup=floor1BoptestAirNetwork_disTSet")
            && rendered.contains("ahu_mix=((controlSemantics_rawZoneHeatingSetpointCommand_1")
            && rendered.contains(
                "hw_floor=floor3BoptestAirNetwork_hotWaterReheatThermalLoad"
            )
            && rendered.contains(
                "hw_branch=floor3BoptestAirNetwork_hotWaterReheatThermalLoad"
            )
            && rendered.contains("hw_total=(floor1BoptestAirNetwork_hotWaterReheatThermalLoad + floor2BoptestAirNetwork_hotWaterReheatThermalLoad + floor3BoptestAirNetwork_hotWaterReheatThermalLoad)"),
        "HW plant demand surfaces should be driven by BOPTEST floor reheat load:\n{rendered}"
    );
    assert!(
        !rendered.contains("cooling_capacity")
            && !rendered.contains("heating_capacity")
            && !rendered.contains("zone_terminal_cooling_command")
            && !rendered.contains("zone_terminal_reheat_command")
            && !rendered.contains("no equation found"),
        "BOPTEST floor load read surfaces must not fall back to reduced capacity-command demand:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_top_down_control_fan_enable() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
raw={{ alg_rhs_for_var("controlSemantics.rawAhuSupplyFanSpeed[1]", dae.f_x, cfg) }}
enable={{ alg_rhs_for_var("controlSemantics.fanEnable[1]", dae.f_x, cfg) }}
effective={{ alg_rhs_for_var("controlSemantics.effectiveAhuSupplyFanSpeed[1]", dae.f_x, cfg) }}
raw_hw={{ alg_rhs_for_var("controlSemantics.rawPlantHotWaterSetpoint", dae.f_x, cfg) }}
eff_hw={{ alg_rhs_for_var("controlSemantics.effectivePlantHotWaterSetpoint", dae.f_x, cfg) }}
raw_sat={{ alg_rhs_for_var("controlSemantics.rawAhuSupplyAirTemperatureSetpoint[2]", dae.f_x, cfg) }}
eff_sat={{ alg_rhs_for_var("controlSemantics.effectiveAhuSupplyAirTemperatureSetpoint[2]", dae.f_x, cfg) }}
raw_dp={{ alg_rhs_for_var("controlSemantics.rawAhuDuctStaticPressureSetpoint[3]", dae.f_x, cfg) }}
eff_dp={{ alg_rhs_for_var("controlSemantics.effectiveAhuDuctStaticPressureSetpoint[3]", dae.f_x, cfg) }}
raw_dam={{ alg_rhs_for_var("controlSemantics.rawZoneTerminalDamperCommand[4]", dae.f_x, cfg) }}
eff_dam={{ alg_rhs_for_var("controlSemantics.effectiveZoneTerminalDamperCommand[4]", dae.f_x, cfg) }}
raw_air={{ alg_rhs_for_var("controlSemantics.rawZoneTerminalAirflowSetpointCommand[5]", dae.f_x, cfg) }}
eff_air={{ alg_rhs_for_var("controlSemantics.effectiveZoneTerminalAirflowSetpointCommand[5]", dae.f_x, cfg) }}
raw_reh={{ alg_rhs_for_var("controlSemantics.rawZoneTerminalReheatCommand[6]", dae.f_x, cfg) }}
eff_reh={{ alg_rhs_for_var("controlSemantics.effectiveZoneTerminalReheatCommand[6]", dae.f_x, cfg) }}
raw_cool={{ alg_rhs_for_var("controlSemantics.rawZoneCoolingSetpointCommand[7]", dae.f_x, cfg) }}
eff_cool={{ alg_rhs_for_var("controlSemantics.effectiveZoneCoolingSetpointCommand[7]", dae.f_x, cfg) }}
raw_heat={{ alg_rhs_for_var("controlSemantics.rawZoneHeatingSetpointCommand[8]", dae.f_x, cfg) }}
eff_heat={{ alg_rhs_for_var("controlSemantics.effectiveZoneHeatingSetpointCommand[8]", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("raw=ahu_supply_fan_speed_1")
            && rendered.contains("time <= 0.0")
            && rendered.contains("controlSemantics_initialFanEnable_1")
            && rendered.contains("controlSemantics_fanStageRequest_1")
            && rendered.contains("controlSemantics_stagingOnThreshold")
            && rendered.contains(
                "effective=(((fmin(1.0, fmax(0.0, controlSemantics_rawAhuSupplyFanSpeed_1))) > 1e-6"
            )
            && rendered.contains("controlSemantics_terminalDamperMean_1")
            && rendered.contains("controlSemantics_fanEnable_1")
            && rendered.contains("raw_hw=plant_hot_water_setpoint")
            && rendered.contains("eff_hw=controlSemantics_rawPlantHotWaterSetpoint")
            && rendered.contains("raw_sat=ahu_supply_air_temperature_setpoint_2")
            && rendered.contains("eff_sat=controlSemantics_rawAhuSupplyAirTemperatureSetpoint_2")
            && rendered.contains("raw_dp=ahu_duct_static_pressure_setpoint_3")
            && rendered
                .contains("eff_dp=fmax(0.0, controlSemantics_rawAhuDuctStaticPressureSetpoint_3)")
            && rendered.contains("raw_dam=zone_terminal_damper_command_4")
            && rendered.contains("eff_dam=controlSemantics_rawZoneTerminalDamperCommand_4")
            && rendered.contains("raw_air=zone_terminal_airflow_setpoint_command_5")
            && rendered
                .contains("eff_air=controlSemantics_rawZoneTerminalAirflowSetpointCommand_5")
            && rendered.contains("raw_reh=zone_terminal_reheat_command_6")
            && rendered.contains(
                "eff_reh=fmin(1.0, fmax(0.0, controlSemantics_rawZoneTerminalReheatCommand_6))"
            )
            && rendered.contains("raw_cool=zone_cooling_setpoint_command_7")
            && rendered.contains("eff_cool=controlSemantics_rawZoneCoolingSetpointCommand_7")
            && rendered.contains("raw_heat=zone_heating_setpoint_command_8")
            && rendered.contains("eff_heat=controlSemantics_rawZoneHeatingSetpointCommand_8"),
        "top-down fan speed and fanEnable should be synthesized from the command semantics:\n{rendered}"
    );
    assert!(
        !rendered.contains("no equation found") && !rendered.trim().starts_with("0.0"),
        "top-down fan speed semantics must not fall through to zero placeholders:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_top_down_control_override_and_economizer_outputs() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
raw_mix={{ alg_rhs_for_var("controlSemantics.rawAhuMixingDamperCommand[2]", dae.f_x, cfg) }}
eff_mix={{ alg_rhs_for_var("controlSemantics.effectiveAhuMixingDamperCommand[2]", dae.f_x, cfg) }}
override={{ alg_rhs_for_var("controlSemantics.overrideActive[4]", dae.f_x, cfg) }}
overwrite={{ alg_rhs_for_var("controlSemantics.overwriteState[4]", dae.f_x, cfg) }}
econ_enable={{ alg_rhs_for_var("controlSemantics.economizerEnable[2]", dae.f_x, cfg) }}
econ_state={{ alg_rhs_for_var("controlSemantics.economizerState[2]", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("raw_mix=ahu_mixing_damper_command_2")
            && rendered.contains(
                "eff_mix=fmin(1.0, fmax(0.0, controlSemantics_rawAhuMixingDamperCommand_2))"
            )
            && rendered.contains(
                "override=((time <= 0.0) ? controlSemantics_initialOverrideActive_4 : 0.0)"
            )
            && rendered.contains(
                "overwrite=((time <= 0.0) ? controlSemantics_initialOverwriteState_4 : 0.0)"
            )
            && rendered.contains(
                "econ_enable=((time <= 0.0) ? controlSemantics_initialEconomizerEnable_2 : ((controlSemantics_economizerStagingState_2 > 0.5) ? 1.0 : 0.0))"
            )
            && rendered.contains("controlSemantics_initialEconomizerState_2")
            && rendered.contains("controlSemantics_rawAhuMixingDamperCommand_2")
            && rendered.contains("controlSemantics_economizerEnable_2")
            && rendered.contains("controlSemantics_weatherProfile"),
        "top-down override and economizer outputs should follow source control semantics:\n{rendered}"
    );
    assert!(
        !rendered.contains("no equation found") && !rendered.trim().starts_with("0.0"),
        "top-down override and economizer outputs must not fall through to zero placeholders:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_ahu_fan_command_chain() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
ySup={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.varSpe.ySup", dae.f_x, cfg) }}
ovrU={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.oveSpeSupFan.u", dae.f_x, cfg) }}
ovrY={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.oveSpeSupFan.y", dae.f_x, cfg) }}
motU={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.withoutMotor.u", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("ySup=floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_varSpe_swi_y")
            && rendered.contains(
                "ovrU=floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_varSpe_swi_y"
            )
            && rendered.contains(
                "ovrY=floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_oveSpeSupFan_swi_u2 ? floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_oveSpeSupFan_swi_u1 : floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_varSpe_swi_y"
            )
            && rendered.contains(
                "motU=floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_oveSpeSupFan_swi_u2 ? floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_oveSpeSupFan_swi_u1 : floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_varSpe_swi_y"
            ),
        "BOPTEST AHU fan command chain should route controller output through overwrite into the mover:\n{rendered}"
    );
    assert!(
        !rendered.contains("no equation found")
            && !rendered.contains(
                "varSpe_ySup=floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_oveSpeSupFan_u"
            )
            && !rendered.contains(
                "withoutMotor_u=floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_oveSpeSupFan_y"
            ),
        "fan command chain must not preserve alias cycles:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_ahu_fan_mover_flow_and_power() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
supV={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.withoutMotor.varSpeFloMov.eff.V_flow", dae.f_x, cfg) }}
supM={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.withoutMotor.varSpeFloMov.eff.m_flow", dae.f_x, cfg) }}
supDp={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.withoutMotor.varSpeFloMov.eff.dp", dae.f_x, cfg) }}
supEta={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.withoutMotor.varSpeFloMov.eff.eta", dae.f_x, cfg) }}
supWFlo={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.withoutMotor.varSpeFloMov.eff.WFlo", dae.f_x, cfg) }}
supPEle={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.withoutMotor.varSpeFloMov.eff.PEle", dae.f_x, cfg) }}
supP={{ alg_rhs_for_var("floor1BoptestAirNetwork.floor.duaFanAirHanUni.supFan.withoutMotor.varSpeFloMov.P", dae.f_x, cfg) }}
retV={{ alg_rhs_for_var("floor2BoptestAirNetwork.floor.duaFanAirHanUni.retFan.varSpeFloMov.eff.V_flow", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "supV=__rumoca_inverse_mover_pressure_curve(4, fmax(0.0, floor1BoptestAirNetwork_pSet), fmin(1.0, fmax(0.0, floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_u))"
        )
            && rendered.contains("floor1BoptestAirNetwork_SupPreCur_1")
            && rendered.contains(
                "retV=__rumoca_inverse_mover_pressure_curve(4, fmax(0.0, floor2BoptestAirNetwork_pSet), fmin(1.0, fmax(0.0, floor2BoptestAirNetwork_floor_duaFanAirHanUni_retFan_u))"
            )
            && rendered.contains("floor2BoptestAirNetwork_RetPreCur_1"),
        "AHU supply/return fan flow should solve against BOPTEST fan pressure curves:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "supM=(floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_varSpeFloMov_rho_default * __rumoca_inverse_mover_pressure_curve"
        )
            && rendered.contains(
                "supDp=__rumoca_mover_pressure_curve(4, floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_varSpeFloMov_eff_V_flow"
            )
            && rendered.contains(
                "supEta=(__rumoca_mover_curve_value(4, floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_varSpeFloMov_eff_V_flow"
            )
            && rendered.contains(
                "supWFlo=fmax(0.0, floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_varSpeFloMov_eff_V_flow * floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_varSpeFloMov_eff_dp)"
            )
            && rendered.contains(
                "supPEle=(floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_varSpeFloMov_eff_WFlo / Buildings_Utilities_Math_Functions_smoothMax"
            )
            && rendered.contains(
                "supP=floor1BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_varSpeFloMov_heaDis_PEle"
            ),
        "AHU fan mover power chain should be synthesized from flow, pressure, and BOPTEST efficiencies:\n{rendered}"
    );
    assert!(
        !rendered.contains("senMasFlo_m_flow")
            && !rendered.contains("heaDis_WFlo")
            && !rendered.contains("no equation found"),
        "AHU fan mover lowering must cut the zero self-cycle aliases:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_parameter_binding_synthesizes_boptest_ahu_volume_curve() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
floor1v1={{ parameter_binding_rhs("floor1BoptestAirNetwork.VolFloCur", {"Literal": {"Real": 0.0}}, 1, cfg) }}
floor2v4={{ parameter_binding_rhs("floor2BoptestAirNetwork.VolFloCur", {"Literal": {"Real": 0.0}}, 4, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "floor1v1=((10.92 + 2.25 + 1.49 + 1.9 + 1.73) * floor1BoptestAirNetwork_alpha * 3.0 * 1.0 * 0.5)"
        )
            && rendered.contains(
                "floor2v4=((10.92 + 2.25 + 1.49 + 1.9 + 1.73) * floor2BoptestAirNetwork_alpha * 3.0 * 10.0 * 1.2)"
            ),
        "BOPTEST AHU VolFloCur should be synthesized from the source floor mAirFloRat sizing formula:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_cooling_tower_bypass_control() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "reaChiWatSys_yCooTowByp_y": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.conPI.y": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.y": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
out={{ alg_rhs_for_var_with_dae("reaChiWatSys_yCooTowByp_y", dae, cfg) }}
pi={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.conPI.y", dae, cfg) }}
valve={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.y", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_TCWLowSet")
            && rendered.contains("chilledWaterPlant_wetBulbTemperature")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_dTApp_nominal")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_dTCW_nominal"),
        "BOPTEST cooling-tower bypass should synthesize from tower low-limit control variables:\n{rendered}"
    );
    assert!(
        !rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_conPI_y\n"),
        "BOPTEST cooling-tower bypass should not read a stale conPI.y alias:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_cooling_tower_bypass_mass_flow() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {},
        "w": {
            "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.senMasFloByp.m_flow": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.m_flow_bypass": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.senMasFloTow.m_flow": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.m_flow": {}
        },
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
byp={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.senMasFloByp.m_flow", dae, cfg) }}
byp_out={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.m_flow_bypass", dae, cfg) }}
tow={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.senMasFloTow.m_flow", dae, cfg) }}
tow_out={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.m_flow", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("chilledWaterPlant_chillerPlant_pumCW_pumConSpe_1_m_flow_in")
            && rendered.contains("chilledWaterPlant_chillerPlant_pumCW_pumConSpe_2_m_flow_in")
            && rendered.contains("chilledWaterPlant_chillerPlant_pumCW_pumConSpe_3_m_flow_in")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_TCWLowSet")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_dTCW_nominal"),
        "BOPTEST cooling-tower bypass branch flows should synthesize from condenser-water pump flow and bypass control:\n{rendered}"
    );
    assert!(
        rendered
            .contains("tow=fmax(0.0, (chilledWaterPlant_chillerPlant_pumCW_pumConSpe_1_m_flow_in"),
        "tower branch mass flow should subtract the bypass branch from total condenser-water flow:\n{rendered}"
    );
    assert!(
        !rendered.contains("byp=chilledWaterPlant_chillerPlant_cooTowWithByp_byp_m_flow_bypass")
            && !rendered.contains(
                "byp_out=chilledWaterPlant_chillerPlant_cooTowWithByp_byp_senMasFloByp_m_flow"
            )
            && !rendered.contains("tow=chilledWaterPlant_chillerPlant_cooTowWithByp_byp_m_flow")
            && !rendered.contains(
                "tow_out=chilledWaterPlant_chillerPlant_cooTowWithByp_byp_senMasFloTow_m_flow"
            ),
        "BOPTEST cooling-tower bypass mass-flow outputs should not be mutual aliases:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_cooling_tower_bypass_valve_curve() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.phi": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.k": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.kVal": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.m_flow": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
phi={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.phi", dae, cfg) }}
k={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.k", dae, cfg) }}
kval={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.kVal", dae, cfg) }}
m={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.m_flow", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("pow(chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_R")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_l")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_delta0")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_Kv_SI")
            && rendered.contains(
                "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_dpFixed_nominal"
            )
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_TCWLowSet"),
        "BOPTEST cooling-tower bypass valve should synthesize the Buildings equal-percentage phi/kVal/k chain:\n{rendered}"
    );
    assert!(
        !rendered.contains("k=0.0")
            && !rendered
                .contains("phi=chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_kVal")
            && !rendered
                .contains("kval=chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_phi"),
        "BOPTEST cooling-tower bypass valve should not fall back to the phi/kVal alias cycle or zero k:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_linear_plant_valve_k() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].valCHW.phi": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].valCHW.kVal": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].valCHW.k": {},
            "hotWaterPlant.boilerPlant.mulBoi.boi[1].valHW.k": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
phi={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].valCHW.phi", dae, cfg) }}
kval={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].valCHW.kVal", dae, cfg) }}
k={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].valCHW.k", dae, cfg) }}
hw={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.mulBoi.boi[1].valHW.k", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("chilledWaterPlant_chillerPlant_mulChiSys_ch_1_valCHW_y_actual")
            && rendered.contains("chilledWaterPlant_chillerPlant_mulChiSys_ch_1_valCHW_l")
            && rendered.contains("chilledWaterPlant_chillerPlant_mulChiSys_ch_1_valCHW_Kv_SI")
            && rendered
                .contains("chilledWaterPlant_chillerPlant_mulChiSys_ch_1_valCHW_dpFixed_nominal")
            && rendered.contains("hotWaterPlant_boilerPlant_mulBoi_boi_1_valHW_kFixed"),
        "BOPTEST plant linear valves should synthesize the Buildings TwoWayLinear phi/kVal/k chain:\n{rendered}"
    );
    assert!(
        !rendered.contains("k=0.0")
            && !rendered.contains("hw=0.0")
            && !rendered.contains("phi=chilledWaterPlant_chillerPlant_mulChiSys_ch_1_valCHW_kVal"),
        "BOPTEST plant linear valves should not fall back to zero k or a phi/kVal alias cycle:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_equal_percentage_plant_valve_k() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "chilledWaterPlant.chillerPlant.pumSecCHW.val[1].phi": {},
            "chilledWaterPlant.chillerPlant.pumSecCHW.val[1].kVal": {},
            "chilledWaterPlant.chillerPlant.pumSecCHW.val[1].k": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[1].val.k": {},
            "hotWaterPlant.boilerPlant.pumSecHW.val[1].k": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
phi={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.pumSecCHW.val[1].phi", dae, cfg) }}
kval={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.pumSecCHW.val[1].kVal", dae, cfg) }}
k={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.pumSecCHW.val[1].k", dae, cfg) }}
tower={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[1].val.k", dae, cfg) }}
hw={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.pumSecHW.val[1].k", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("pow(chilledWaterPlant_chillerPlant_pumSecCHW_val_1_R")
            && rendered.contains("chilledWaterPlant_chillerPlant_pumSecCHW_val_1_delta0")
            && rendered.contains("chilledWaterPlant_chillerPlant_pumSecCHW_val_1_y_actual")
            && rendered.contains("chilledWaterPlant_chillerPlant_pumSecCHW_val_1_Kv_SI")
            && rendered.contains(
                "chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_1_val_kFixed"
            )
            && rendered.contains("hotWaterPlant_boilerPlant_pumSecHW_val_1_kFixed"),
        "BOPTEST plant equal-percentage valves should synthesize the Buildings phi/kVal/k curve:\n{rendered}"
    );
    assert!(
        !rendered.contains("k=0.0")
            && !rendered.contains("tower=0.0")
            && !rendered.contains("hw=0.0")
            && !rendered.contains("phi=chilledWaterPlant_chillerPlant_pumSecCHW_val_1_kVal"),
        "BOPTEST plant equal-percentage valves should not fall back to zero k or a phi/kVal alias cycle:\n{rendered}"
    );
}

#[test]
fn test_fmi_parameter_and_alg_rhs_synthesizes_boptest_vav_terminal_valves() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.dam.phi": {},
            "floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.dam.kVal": {},
            "floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.dam.k": {},
            "floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.rehVal.k": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
vav_m={{ parameter_binding_rhs("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.mAirFloRat", {"Literal": {"Real": 0.0}}, 0, cfg) }}
vav_w={{ parameter_binding_rhs("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.mWatFloRat", {"Literal": {"Real": 0.0}}, 0, cfg) }}
dam_m={{ parameter_binding_rhs("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.dam.m_flow_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
dam_dp={{ parameter_binding_rhs("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.dam.dpValve_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
dam_kv={{ parameter_binding_rhs("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.dam.Kv_SI", {"Literal": {"Real": 0.0}}, 0, cfg) }}
reh_m={{ parameter_binding_rhs("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.rehVal.m_flow_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
reh_dp={{ parameter_binding_rhs("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.rehVal.dpValve_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
reh_kv={{ parameter_binding_rhs("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.rehVal.Kv_SI", {"Literal": {"Real": 0.0}}, 0, cfg) }}
phi={{ alg_rhs_for_var_with_dae("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.dam.phi", dae, cfg) }}
kval={{ alg_rhs_for_var_with_dae("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.dam.kVal", dae, cfg) }}
k={{ alg_rhs_for_var_with_dae("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.dam.k", dae, cfg) }}
reh_k={{ alg_rhs_for_var_with_dae("floor2BoptestAirNetwork.floor.fivZonVAV.vAV3.rehVal.k", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("vav_m=floor2BoptestAirNetwork_mAirFloRat_8")
            && rendered.contains("vav_w=floor2BoptestAirNetwork_mWatFloRat_8")
            && rendered.contains("dam_m=floor2BoptestAirNetwork_mAirFloRat_8")
            && rendered.contains("dam_dp=floor2BoptestAirNetwork_PreDroAir_3")
            && rendered.contains(
                "dam_kv=(floor2BoptestAirNetwork_mAirFloRat_8 / sqrt(fmax(0.00001, floor2BoptestAirNetwork_PreDroAir_3)))"
            )
            && rendered.contains("reh_m=floor2BoptestAirNetwork_mWatFloRat_8")
            && rendered.contains("reh_dp=floor2BoptestAirNetwork_PreDroWat_3")
            && rendered.contains(
                "reh_kv=(floor2BoptestAirNetwork_mWatFloRat_8 / sqrt(fmax(0.00001, floor2BoptestAirNetwork_PreDroWat_3)))"
            ),
        "BOPTEST VAV terminal nominal parameters should come from the BoptestFloorAirNetwork source matrix:\n{rendered}"
    );
    assert!(
        rendered.contains("pow(floor2BoptestAirNetwork_floor_fivZonVAV_vAV3_dam_R")
            && rendered.contains("floor2BoptestAirNetwork_floor_fivZonVAV_vAV3_dam_y_actual")
            && rendered.contains("floor2BoptestAirNetwork_floor_fivZonVAV_vAV3_dam_Kv_SI")
            && rendered.contains("floor2BoptestAirNetwork_floor_fivZonVAV_vAV3_rehVal_Kv_SI")
            && rendered.contains("floor2BoptestAirNetwork_floor_fivZonVAV_vAV3_rehVal_kFixed"),
        "BOPTEST VAV terminal valves should synthesize the Buildings equal-percentage phi/kVal/k chain:\n{rendered}"
    );
    assert!(
        !rendered.contains("k=0.0")
            && !rendered.contains("reh_k=0.0")
            && !rendered.contains("phi=floor2BoptestAirNetwork_floor_fivZonVAV_vAV3_dam_kVal"),
        "BOPTEST VAV terminal valves should not fall back to zero k or phi/kVal alias cycles:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_cooling_tower_power() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "reaChiWatSys_reaPCooTow_y": {},
            "reaChiWatSys_PCooTow_1_y": {},
            "chilledWaterPlant.coolingTowerElectricalPowerByUnit[2]": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[3].yorkCalc.PFan": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
ptot={{ alg_rhs_for_var_with_dae("reaChiWatSys_reaPCooTow_y", dae, cfg) }}
p1={{ alg_rhs_for_var_with_dae("reaChiWatSys_PCooTow_1_y", dae, cfg) }}
p2={{ alg_rhs_for_var_with_dae("chilledWaterPlant.coolingTowerElectricalPowerByUnit[2]", dae, cfg) }}
p3={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[3].yorkCalc.PFan", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered
            .contains("chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_1_P_nominal")
            && rendered.contains(
                "chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_2_P_nominal"
            )
            && rendered.contains(
                "chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_3_P_nominal"
            )
            && rendered
                .contains("chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_1_conPI_y")
            && rendered.contains("0.027")
            && rendered.contains("0.216")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_On_1")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_On_2")
            && rendered.contains("chilledWaterPlant_chillerPlant_cooTowWithByp_On_3"),
        "BOPTEST cooling tower power read surfaces should synthesize from per-tower BOPTEST sizing, fan PI, and fan curve:\n{rendered}"
    );
    assert!(
        !rendered.contains("no equation found"),
        "BOPTEST cooling tower power should not fall back to missing tower internals:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_cooling_tower_water_fraction() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[1].yorkCalc.FRWat": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
fr={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[1].yorkCalc.FRWat", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "fr=fmax(0.0, chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_1_yorkCalc_m_flow / fmax(0.00001, chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_1_yorkCalc_mWat_flow_nominal))"
        ),
        "BOPTEST YorkCalc FRWat should follow noEvent(if FRWatRaw > 0 then FRWatRaw else 0):\n{rendered}"
    );
    assert!(
        !rendered.contains("no equation found"),
        "BOPTEST YorkCalc FRWat should not fall back to a no-equation zero:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_parameter_binding_synthesizes_boptest_cooling_tower_sizing() {
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
p={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[1].P_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
pfan={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[1].yorkCalc.PFan_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
fra={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[1].yorkCalc.fraPFan_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
mwat={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[1].yorkCalc.mWat_flow_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
m={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[1].mCW_flow_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
arr={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.P_nominal", {"Literal": {"Real": 0.0}}, 2, cfg) }}
byp={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.m_flow_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
byp_sen={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.senMasFloByp.m_flow_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
dp={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.dpValve_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
val_m={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.m_flow_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
kv={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.Kv_SI", {"Literal": {"Real": 0.0}}, 0, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&serde_json::json!({}), template).unwrap();

    assert!(
        rendered.contains("p=(2391666.6666666665 * (5.06 + 1.0) / 5.06 * 0.015)")
            && rendered.contains("pfan=(2391666.6666666665 * (5.06 + 1.0) / 5.06 * 0.015)")
            && rendered.contains(
                "fra=((2391666.6666666665 * (5.06 + 1.0) / 5.06 * 0.015) / 131.656925135186)"
            )
            && rendered.contains("mwat=131.656925135186")
            && rendered.contains("m=131.656925135186")
            && rendered.contains("arr=(2391666.6666666665 * (5.06 + 1.0) / 5.06 * 0.015)")
            && rendered.contains("byp=(3.0 * 131.656925135186)")
            && rendered.contains("byp_sen=(3.0 * 131.656925135186)")
            && rendered.contains("dp=chilledWaterPlant_chillerPlant_dPByp_nominal")
            && rendered.contains("val_m=(3.0 * 131.656925135186)")
            && rendered.contains(
                "kv=((3.0 * 131.656925135186) / sqrt(fmax(0.00001, chilledWaterPlant_chillerPlant_dPByp_nominal)))"
            ),
        "BOPTEST cooling tower sizing parameters should not initialize to zero:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_wet_bulb_from_weather_input() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "wet_bulb_temperature": {},
            "synthetic_wet_bulb_temperature": {},
            "chilledWaterPlant.wetBulbTemperature": {},
            "chilledWaterPlant.chillerPlant.TWetBul": {},
            "chilledWaterPlant.chillerPlant.cooTowWithByp.TWetBul": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {
            "weather_source_override_enable": {},
            "weather_twetbul_input": {}
        },
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
wet={{ alg_rhs_for_var_with_dae("wet_bulb_temperature", dae, cfg) }}
plant={{ alg_rhs_for_var_with_dae("chilledWaterPlant.wetBulbTemperature", dae, cfg) }}
chiller={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.TWetBul", dae, cfg) }}
tower={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.cooTowWithByp.TWetBul", dae, cfg) }}
synthetic={{ alg_rhs_for_var_with_dae("synthetic_wet_bulb_temperature", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.matches("weather_twetbul_input").count() >= 4
            && rendered.matches("weather_source_override_enable").count() >= 4,
        "BOPTEST plant wet-bulb path should consume the weather override input:\n{rendered}"
    );
    assert!(
        rendered.contains("synthetic=(synthetic_outdoor_temperature - 2.0)"),
        "synthetic wet-bulb should remain source-owned fallback:\n{rendered}"
    );
    assert!(
        !rendered.contains("wet=chilledWaterPlant_wetBulbTemperature")
            && !rendered.contains("plant=chilledWaterPlant_chillerPlant_TWetBul")
            && !rendered.contains("chiller=chilledWaterPlant_wetBulbTemperature"),
        "wet-bulb variables must not preserve reverse alias cycles:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_discrete_rhs_synthesizes_con_pi_enable_chain() {
    let dae_json = serde_json::json!({
        "m": {
            "plant.secPumCon.On": {},
            "plant.secPumCon.conPI.On": {},
            "plant.secPumCon.conPI.booToRea.u": {},
            "plant.secPumCon.conPI.conPID.trigger": {},
            "plant.secPumCon.conPI.conPID.I.trigger": {}
        },
        "z": {},
        "y": {},
        "u": {},
        "x": {},
        "p": {},
        "c": {},
        "f_z": [],
        "f_m": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
on={{ discrete_rhs_for_var("plant.secPumCon.conPI.On", dae.f_z, dae.f_m, dae, cfg) }}
boo={{ discrete_rhs_for_var("plant.secPumCon.conPI.booToRea.u", dae.f_z, dae.f_m, dae, cfg) }}
trigger={{ discrete_rhs_for_var("plant.secPumCon.conPI.conPID.trigger", dae.f_z, dae.f_m, dae, cfg) }}
itrigger={{ discrete_rhs_for_var("plant.secPumCon.conPI.conPID.I.trigger", dae.f_z, dae.f_m, dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("on=plant_secPumCon_On"),
        "conPI.On should synthesize from the parent component On signal:\n{rendered}"
    );
    assert!(
        rendered.contains("boo=plant_secPumCon_conPI_On"),
        "BooleanToReal.u should synthesize from conPI.On:\n{rendered}"
    );
    assert!(
        rendered.contains("trigger=plant_secPumCon_conPI_On"),
        "PID trigger should synthesize from conPI.On:\n{rendered}"
    );
    assert!(
        rendered.contains("itrigger=plant_secPumCon_conPI_conPID_trigger"),
        "PID integrator trigger should synthesize from the PID trigger:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_rhs_synthesizes_buildings_actuator_filter_chain() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
fcut={{ parameter_binding_rhs("floor.ahu.val.fCut", {"Literal": {"Real": 0.0}}, 0, cfg) }}
n={{ parameter_binding_rhs("floor.ahu.val.filter.n", {"Literal": {"Real": 0.0}}, 0, cfg) }}
f={{ parameter_binding_rhs("floor.ahu.val.filter.f", {"Literal": {"Real": 0.0}}, 0, cfg) }}
u_nom={{ parameter_binding_rhs("floor.ahu.val.filter.u_nom", {"Literal": {"Real": 0.0}}, 0, cfg) }}
w_u={{ parameter_binding_rhs("floor.ahu.val.filter.w_u", {"Literal": {"Real": 0.0}}, 0, cfg) }}
x1={{ alg_rhs_for_var("floor.ahu.val.filter.x[1]", dae.f_x, cfg) }}
x2={{ alg_rhs_for_var("floor.ahu.val.filter.x[2]", dae.f_x, cfg) }}
y={{ alg_rhs_for_var("floor.ahu.val.filter.y", dae.f_x, cfg) }}
ds1={{ ode_rhs_for_state("floor.ahu.val.filter.s[1]", dae.f_x, cfg) }}
ds2={{ ode_rhs_for_state("floor.ahu.val.filter.s[2]", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("fcut=(5.0 / (2.0 * 3.14159265358979323846 * floor_ahu_val_riseTime))"),
        "fCut should follow Buildings ActuatorSignal/PartialFlowMachine riseTime semantics:\n{rendered}"
    );
    assert!(
        rendered.contains("n=2.0"),
        "ActuatorFilter order should stay at the source constant n=2:\n{rendered}"
    );
    assert!(
        rendered.contains("f=floor_ahu_val_fCut"),
        "ActuatorFilter.f should read the parent fCut binding:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "u_nom=((fabs(floor_ahu_val_filter_u_nominal - 1.0) < 1e-12) ? (1.0 - 1e-12) : floor_ahu_val_filter_u_nominal)"
        ),
        "ActuatorFilter.u_nom should preserve the source anti-elimination guard:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "w_u=(2.0 * 3.14159265358979323846 * floor_ahu_val_filter_f / floor_ahu_val_filter_alpha / floor_ahu_val_filter_u_nom)"
        ),
        "ActuatorFilter.w_u should follow the source angular-frequency chain:\n{rendered}"
    );
    assert!(
        rendered.contains("x1=(floor_ahu_val_filter_u_nom * floor_ahu_val_filter_s_1)"),
        "ActuatorFilter.x[1] should expose the transformed state:\n{rendered}"
    );
    assert!(
        rendered.contains("x2=(floor_ahu_val_filter_u_nom * floor_ahu_val_filter_s_2)"),
        "ActuatorFilter.x[2] should expose the transformed state:\n{rendered}"
    );
    assert!(
        rendered.contains("y=(floor_ahu_val_filter_u_nom * floor_ahu_val_filter_s_2)"),
        "ActuatorFilter.y should read the second transformed state:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "ds1=((floor_ahu_val_filter_u - (floor_ahu_val_filter_u_nom * floor_ahu_val_filter_s_1)) * floor_ahu_val_filter_w_u)"
        ),
        "ActuatorFilter.s[1] derivative should follow the source first filter stage:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "ds2=((floor_ahu_val_filter_u_nom * (floor_ahu_val_filter_s_1 - floor_ahu_val_filter_s_2)) * floor_ahu_val_filter_w_u)"
        ),
        "ActuatorFilter.s[2] derivative should follow the source second filter stage:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_rhs_synthesizes_buildings_limit_slew_rate() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
fall={{ parameter_binding_rhs("floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.ecoCon.ramLim.fallingSlewRate", {"Literal": {"Real": 0.0}}, 0, cfg) }}
td={{ parameter_binding_rhs("floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.ecoCon.ramLim.Td", {"Literal": {"Real": 0.0}}, 0, cfg) }}
dy={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.ecoCon.ramLim.y", dae.f_x, cfg) }}
other={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.ecoCon.lim.y", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "fall=(-floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_raisingSlewRate)"
        ),
        "LimitSlewRate fallingSlewRate should follow the Buildings default -raisingSlewRate:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "td=(floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_raisingSlewRate * 10.0)"
        ),
        "LimitSlewRate Td should follow the Buildings default raisingSlewRate*10:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "dy=(floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_enable ?"
        )
            && rendered.contains(
                "((floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_u - floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_y) / floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_Td) < floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_fallingSlewRate"
            )
            && rendered.contains(
                "? floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_fallingSlewRate"
            )
            && rendered.contains(
                "((floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_u - floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_y) / floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_Td) > floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_raisingSlewRate"
            )
            && rendered.contains(
                "? floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_raisingSlewRate"
            )
            && rendered.contains(
                ": ((floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_u - floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_y) / floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_ecoCon_ramLim_Td)"
            )
            && rendered.contains(
                ": 0.0)"
            ),
        "LimitSlewRate.y should follow the Buildings source clamp on (u-y)/Td:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "WARNING: no ODE equation found for der(floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.ecoCon.lim.y)"
        ),
        "LimitSlewRate ODE synthesis must stay scoped to ramLim components:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_ode_rhs_synthesizes_top_down_radiant_tail_states() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
core={{ ode_rhs_for_state("floor1_core_radiant_surface_temperature", dae.f_x, cfg) }}
per={{ ode_rhs_for_state("floor2_radiant_surface_temperature_5", dae.f_x, cfg) }}
top={{ ode_rhs_for_state("floor3_radiant_surface_temperature_2", dae.f_x, cfg) }}
tail={{ ode_rhs_for_state("floor3_late_perimeter_tail_temperature", dae.f_x, cfg) }}
other={{ ode_rhs_for_state("floor4_radiant_surface_temperature_2", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "core=((floor_internal_gain_rad_1_1 - floor1_core_radiant_heat_to_air) / lower_core_radiant_capacity)"
        ),
        "lower-core radiant state should follow the source heat balance:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "per=((floor_internal_gain_rad_2_5 - floor2_radiant_heat_to_air_5) / lower_perimeter_radiant_capacity_4)"
        ),
        "lower-floor perimeter radiant state should follow the source heat balance:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "top=((floor_internal_gain_rad_3_2 - floor3_radiant_heat_to_air_2 + floor3_shared_perimeter_slab_heat_2) / floor3_perimeter_radiant_capacity_1)"
        ),
        "floor3 perimeter radiant state should include the source shared-slab heat term:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "tail=(((0.35 * floor_internal_gain_rad_3_2) + (0.35 * floor_internal_gain_rad_3_5) - floor3_late_perimeter_tail_heat_2 - floor3_late_perimeter_tail_heat_5) / floor3_late_perimeter_tail_capacity)"
        ),
        "floor3 late perimeter tail state should follow the source diagnostic heat balance:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "WARNING: no ODE equation found for der(floor4_radiant_surface_temperature_2)"
        ),
        "top-down radiant/tail synthesis must stay scoped to known source states:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_ode_rhs_synthesizes_internal_fluid_volume_mass_balance() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
dm={{ ode_rhs_for_state("plant.loop.junSup.vol.dynBal.m", dae.f_x, cfg) }}
du={{ ode_rhs_for_state("plant.chiller.vol1.dynBal.U", dae.f_x, cfg) }}
dxi={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.retFan.varSpeFloMov.vol.dynBal.medium.Xi[1]", dae.f_x, cfg) }}
dmc={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.retFan.varSpeFloMov.vol.dynBal.mC[1]", dae.f_x, cfg) }}
zm={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.m", dae.f_x, cfg) }}
zxi={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.medium.Xi[1]", dae.f_x, cfg) }}
zmc={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.mC[1]", dae.f_x, cfg) }}
dt={{ ode_rhs_for_state("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.vol1.dynBal.medium.T", dae.f_x, cfg) }}
dh={{ ode_rhs_for_state("chilledWaterPlant.chillerPlant.expVesCHW.H", dae.f_x, cfg) }}
other={{ ode_rhs_for_state("plant.chiller.expVesCHW.H", dae.f_x, cfg) }}
int={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.zonVAVCon[1].cooCon.I.y", dae.f_x, cfg) }}
noint={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.zonVAVCon[1].cooCon.P.y", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("dm=plant_loop_junSup_vol_dynBal_mb_flow"),
        "PartialLumpedVolume mass state should synthesize der(m)=mb_flow:\n{rendered}"
    );
    assert!(
        rendered
            .contains("du=(plant_chiller_vol1_dynBal_Hb_flow + plant_chiller_vol1_dynBal_Q_flow)"),
        "PartialLumpedVolume energy state should synthesize der(U)=Hb_flow+Q_flow:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "dxi=((floor1BoptestAirNetwork_floor_duaFanAirHanUni_retFan_varSpeFloMov_vol_dynBal_m > 1e-9) ? ((floor1BoptestAirNetwork_floor_duaFanAirHanUni_retFan_varSpeFloMov_vol_dynBal_mbXi_flow_1 + (floor1BoptestAirNetwork_floor_duaFanAirHanUni_retFan_varSpeFloMov_vol_dynBal_mWat_flow_internal * floor1BoptestAirNetwork_floor_duaFanAirHanUni_retFan_varSpeFloMov_vol_dynBal_s_1)) / floor1BoptestAirNetwork_floor_duaFanAirHanUni_retFan_varSpeFloMov_vol_dynBal_m) : 0.0)"
        ),
        "Buildings ConservationEquation medium.Xi should follow dynamic substance balance:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "dmc=(floor1BoptestAirNetwork_floor_duaFanAirHanUni_retFan_varSpeFloMov_vol_dynBal_mbC_flow_1 + floor1BoptestAirNetwork_floor_duaFanAirHanUni_retFan_varSpeFloMov_vol_dynBal_C_flow_internal_1)"
        ),
        "Buildings ConservationEquation mC should follow dynamic trace-substance balance:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "zm=(floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_mb_flow + (floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_simplify_mWat_flow ? 0.0 : floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_mWat_flow_internal))"
        ),
        "BOPTEST EnergyPlus ThermalZone volume m should follow ConservationEquation dynamic mass balance:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "zxi=((floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_m > 1e-9) ? ((floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_mbXi_flow_1 + (floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_mWat_flow_internal * floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_s_1)) / floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_m) : 0.0)"
        ),
        "BOPTEST EnergyPlus ThermalZone volume medium.Xi should follow ConservationEquation dynamic substance balance:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "zmc=(floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_mbC_flow_1 + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_C_flow_internal_1)"
        ),
        "BOPTEST EnergyPlus ThermalZone volume mC should follow ConservationEquation dynamic trace balance:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "dt=((chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_m > 1e-9 && fabs(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_cp_default) > 1e-9) ? ((((chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_Hb_flow + chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_Q_flow) * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_m) - (chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_U * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_mb_flow)) / (chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_cp_default * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_m * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_vol1_dynBal_m)) : 0.0)"
        ),
        "water-volume medium.T should follow d(U/m/cp)/dt from source medium equations:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "dh=(chilledWaterPlant_chillerPlant_expVesCHW_port_a_m_flow * ((chilledWaterPlant_chillerPlant_expVesCHW_port_a_m_flow > 0.0) ? chilledWaterPlant_chillerPlant_expVesCHW_port_a_h_outflow : ((chilledWaterPlant_chillerPlant_expVesCHW_m > 1e-9) ? (chilledWaterPlant_chillerPlant_expVesCHW_H / chilledWaterPlant_chillerPlant_expVesCHW_m) : chilledWaterPlant_chillerPlant_expVesCHW_port_a_h_outflow)))"
        ),
        "ExpansionVessel.H should follow der(H)=port_a.m_flow*actualStream(port_a.h_outflow):\n{rendered}"
    );
    assert!(
        rendered.contains("WARNING: no ODE equation found for der(plant.chiller.expVesCHW.H)"),
        "ExpansionVessel.H synthesis should stay scoped to BOPTEST CHW/HW plant vessels:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "int=(floor1BoptestAirNetwork_floor_zonVAVCon_1_cooCon_I_k * floor1BoptestAirNetwork_floor_zonVAVCon_1_cooCon_I_u)"
        ),
        "Modelica/Buildings integrator I.y should follow der(y)=k*u:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "WARNING: no ODE equation found for der(floor1BoptestAirNetwork.floor.zonVAVCon[1].cooCon.P.y)"
        ),
        "Integrator synthesis should not apply to arbitrary block outputs:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_ode_rhs_synthesizes_buildings_temperature_two_port_sensor() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
dT={{ ode_rhs_for_state("plant.chiller.senTCHWEnt.T", dae.f_x, cfg) }}
tlea={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.cooCoi.coi.TLeaAir.T", dae.f_x, cfg) }}
tout={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TOutSen.T", dae.f_x, cfg) }}
tmix={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.mixingBox.mixBox.TMix.T", dae.f_x, cfg) }}
vav={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.fivZonVAV.vAV1.TLea.T", dae.f_x, cfg) }}
medium={{ ode_rhs_for_state("plant.chiller.vol1.dynBal.medium.T", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "dT=(((plant_chiller_senTCHWEnt_TMed - plant_chiller_senTCHWEnt_T) * plant_chiller_senTCHWEnt_k * plant_chiller_senTCHWEnt_tauInv) + (plant_chiller_senTCHWEnt_transferHeat ? (((plant_chiller_senTCHWEnt_TAmb - plant_chiller_senTCHWEnt_T) * plant_chiller_senTCHWEnt_tauHeaTraInv) / ((plant_chiller_senTCHWEnt_ratTau * plant_chiller_senTCHWEnt_k) + 1.0)) : 0.0))"
        ),
        "TemperatureTwoPort.T should follow Buildings sensor dynamics:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "tlea=(((floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TLeaAir_TMed - floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TLeaAir_T) * floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TLeaAir_k * floor1BoptestAirNetwork_floor_duaFanAirHanUni_cooCoi_coi_TLeaAir_tauInv)"
        )
            && rendered.contains(
                "tout=(((floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TOutSen_TMed - floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TOutSen_T) * floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TOutSen_k * floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TOutSen_tauInv)"
            )
            && rendered.contains(
                "tmix=(((floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TMix_TMed - floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TMix_T) * floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TMix_k * floor1BoptestAirNetwork_floor_duaFanAirHanUni_mixingBox_mixBox_TMix_tauInv)"
            )
            && rendered.contains(
                "vav=(((floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TLea_TMed - floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TLea_T) * floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TLea_k * floor1BoptestAirNetwork_floor_fivZonVAV_vAV1_TLea_tauInv)"
            ),
        "BOPTEST AHU and VAV TemperatureTwoPort aliases should use the same sensor dynamics:\n{rendered}"
    );
    assert!(
        rendered
            .contains("WARNING: no ODE equation found for der(plant.chiller.vol1.dynBal.medium.T)"),
        "fluid medium temperature state must not be treated as a TemperatureTwoPort sensor:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_ode_rhs_synthesizes_buildings_dynamic_flow_sensors() {
    let dae_json = serde_json::json!({
        "f_x": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
d={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.senVolFloSupAir.d", dae.f_x, cfg) }}
c={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.senCO2RetAir.C", dae.f_x, cfg) }}
phi={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.duaFanAirHanUni.senRelHumRetAir.phi", dae.f_x, cfg) }}
zone={{ ode_rhs_for_state("floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.C", dae.f_x, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "d=((floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_dMed - floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_d) * floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_k * floor1BoptestAirNetwork_floor_duaFanAirHanUni_senVolFloSupAir_tauInv)"
        ),
        "VolumeFlowRate.d should follow Buildings PartialDynamicFlowSensor dynamics:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "c=((floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_CMed - floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_C) * floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_k * floor1BoptestAirNetwork_floor_duaFanAirHanUni_senCO2RetAir_tauInv)"
        ),
        "TraceSubstancesTwoPort.C should follow Buildings PartialDynamicFlowSensor dynamics:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "phi=((floor1BoptestAirNetwork_floor_duaFanAirHanUni_senRelHumRetAir_phiMed - floor1BoptestAirNetwork_floor_duaFanAirHanUni_senRelHumRetAir_phi) * floor1BoptestAirNetwork_floor_duaFanAirHanUni_senRelHumRetAir_k * floor1BoptestAirNetwork_floor_duaFanAirHanUni_senRelHumRetAir_tauInv)"
        ),
        "RelativeHumidityTwoPort.phi should follow Buildings PartialDynamicFlowSensor dynamics:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "WARNING: no ODE equation found for der(floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.C)"
        ),
        "dynamic-flow sensor ODE synthesis must not apply to arbitrary volume C states:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_discrete_rhs_synthesizes_boptest_boolean_expression_on() {
    let dae_json = serde_json::json!({
        "f_z": [],
        "f_m": []
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
on={{ discrete_rhs_for_var("chilledWaterPlant.chillerPlant.On.y", dae.f_z, dae.f_m, dae, cfg) }}
on1={{ discrete_rhs_for_var("chilledWaterPlant.chillerPlant.On1.y", dae.f_z, dae.f_m, dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("on=1.0"),
        "BOPTEST ChillerPlant BooleanExpression On(y=true) should synthesize to true:\n{rendered}"
    );
    assert!(
        rendered.contains("on1=1.0"),
        "BOPTEST ChillerPlant BooleanExpression On1(y=true) should synthesize to true:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_plant_stage_condition_plr() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.PLR": {},
            "chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.cap_avi": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
plr={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.PLR", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("chilledWaterPlant_chillerPlant_chiSta_plantNStageCondition_Loa")
            && rendered.contains("chilledWaterPlant_chillerPlant_chiSta_y_1")
            && rendered
                .contains("chilledWaterPlant_chillerPlant_chiSta_plantNStageCondition_Cap_1"),
        "BOPTEST PlantStageCondition.PLR should synthesize from Loa and same-step cap_avi:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_chilled_water_load_and_temperatures() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "chilledWaterPlant.chillerPlant.Loa.y": {},
            "chilledWaterPlant.chillerPlant.TCHW_sup": {},
            "chilledWaterPlant.chillerPlant.TCHW_ret": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
loa={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.Loa.y", dae, cfg) }}
sup={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.TCHW_sup", dae, cfg) }}
ret={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.TCHW_ret", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("loa=fmax(0.0, (fmax(0.0, (floor1BoptestAirNetwork_rhoAir")
            && rendered.contains("floor2BoptestAirNetwork_rhoAir")
            && rendered.contains("floor3BoptestAirNetwork_rhoAir")
            && rendered.contains("controlSemantics_rawAhuSupplyAirTemperatureSetpoint_1")
            && rendered.contains("controlSemantics_rawZoneHeatingSetpointCommand_15"),
        "BOPTEST ChillerPlant Loa.y should synthesize from the three BOPTEST floor air-side chilled-water loads:\n{rendered}"
    );
    assert!(
        !rendered.contains("cooling_capacity_10")
            && !rendered.contains("zone_terminal_damper_command_15"),
        "BOPTEST ChillerPlant Loa.y must not fall back to reduced capacity-command cooling demand:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "sup=fmax(275.15, fmin(285.15, chilledWaterPlant_plantChilledWaterSetpoint))"
        ),
        "BOPTEST ChillerPlant TCHW_sup should synthesize from the clamped plant setpoint:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "ret=(fmax(275.15, fmin(285.15, chilledWaterPlant_plantChilledWaterSetpoint))"
        ) && rendered.contains("4200.0")
            && rendered.contains(
                "chilledWaterPlant_chillerPlant_pumSecCHW_pum_1_varSpeFloMov_rho_default"
            )
            && rendered.contains(
                "chilledWaterPlant_chillerPlant_pumSecCHW_pum_2_varSpeFloMov_rho_default"
            ),
        "BOPTEST ChillerPlant TCHW_ret should synthesize from load, heat capacity, and secondary-pump mass flow:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_chiller_unit_load_and_power() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "reaChiWatSys_QChi_1_y": {},
            "reaChiWatSys_PChi_1_y": {},
            "reaChiWatSys_reaPChi_y": {},
            "chilledWaterPlant.chillerThermalLoadByUnit[2]": {},
            "chilledWaterPlant.chillerElectricalPowerByUnit[3]": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
q1={{ alg_rhs_for_var_with_dae("reaChiWatSys_QChi_1_y", dae, cfg) }}
p1={{ alg_rhs_for_var_with_dae("reaChiWatSys_PChi_1_y", dae, cfg) }}
ptot={{ alg_rhs_for_var_with_dae("reaChiWatSys_reaPChi_y", dae, cfg) }}
q2={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerThermalLoadByUnit[2]", dae, cfg) }}
p3={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerElectricalPowerByUnit[3]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("q1=fmax(0.0, -(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_QEva_flow))")
            && rendered.contains("p1=chilledWaterPlant_chillerPlant_mulChiSys_P_1")
            && rendered.contains("ptot=(chilledWaterPlant_chillerPlant_mulChiSys_P_1 + chilledWaterPlant_chillerPlant_mulChiSys_P_2 + chilledWaterPlant_chillerPlant_mulChiSys_P_3)")
            && rendered.contains("q2=fmax(0.0, -(chilledWaterPlant_chillerPlant_mulChiSys_ch_2_chi_QEva_flow))")
            && rendered.contains("p3=chilledWaterPlant_chillerPlant_mulChiSys_P_3"),
        "BOPTEST chiller unit read surfaces should read the original MultiChillers/ElectricEIR fields:\n{rendered}"
    );
    assert!(
        !rendered.contains("/ 5.06")
            && !rendered.contains("p1=(")
            && !rendered.contains("no equation found"),
        "BOPTEST chiller unit read surfaces must not bypass chiller internals with public COP shortcuts:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_preserves_boptest_chiller_eir_chain() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.QEva_flow": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.COP": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.TEvaEnt": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.TConEnt": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.hSet": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.QEva_flow_set": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.capFunT": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.EIRFunT": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.PLR1": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.PLR2": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.CR": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.EIRFunPLR": {}
        },
        "w": {
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.P": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[1].P": {},
            "chilledWaterPlant.chillerPlant.mulChiSys.P[1]": {}
        },
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
qeva={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.QEva_flow", dae, cfg) }}
cop={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.COP", dae, cfg) }}
tevaent={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.TEvaEnt", dae, cfg) }}
tconent={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.TConEnt", dae, cfg) }}
hset={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.hSet", dae, cfg) }}
qset={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.QEva_flow_set", dae, cfg) }}
cap={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.capFunT", dae, cfg) }}
eirt={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.EIRFunT", dae, cfg) }}
plr1={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.PLR1", dae, cfg) }}
plr2={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.PLR2", dae, cfg) }}
cr={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.CR", dae, cfg) }}
eirplr={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.EIRFunPLR", dae, cfg) }}
chip={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.P", dae, cfg) }}
chp={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].P", dae, cfg) }}
bankp={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.mulChiSys.P[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("qeva=-(")
            && rendered.contains("2391666.6666666665")
            && rendered.contains("cop=fmax(0.0, -(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_QEva_flow) / fmax(0.00001, chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_P - (chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_QEva_flow_nominal * 1e-9)))")
            && rendered.contains("tevaent=chilledWaterPlant_chillerPlant_TCHW_ret")
            && rendered.contains("tconent=chilledWaterPlant_chillerPlant_cooTowWithByp_senTCWEntChi_T")
            && rendered.contains("hset=4200.0 * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_TSet")
            && rendered.contains("qset=fmin(fmax(0.0, chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_m2_flow) * (4200.0 * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_TSet - 4200.0 * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_TEvaEnt), (chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_QEva_flow_nominal * 1e-9))")
            && rendered.contains("cap=fmax(1e-6, (chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_capFunT_1")
            && rendered.contains("eirt=(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_EIRFunT_1")
            && rendered.contains("plr1=fmin(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_PLRMax")
            && rendered.contains("(fmin(fmax(0.0, chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_m2_flow) * (4200.0 * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_TSet - 4200.0 * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_TEvaEnt), (chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_QEva_flow_nominal * 1e-9))) / -(fmax(1e-6")
            && rendered.contains("plr2=fmax(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_PLRMinUnl")
            && rendered.contains("cr=fmin(1.0, fmax(0.0, chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_PLR1")
            && rendered.contains("eirplr=(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_EIRFunPLR_1")
            && rendered.contains("chip=fmax(0.0, (fmax(1e-6, -(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_QEva_flow_nominal * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_capFunT))) / fmax(0.00001, chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_COP_nominal) * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_EIRFunT * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_EIRFunPLR * chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_CR)")
            && rendered.contains("chp=chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_P")
            && rendered.contains("bankp=chilledWaterPlant_chillerPlant_mulChiSys_ch_1_P")
            && !rendered.contains("chip=fmax(0.0, -(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_QEva_flow) /"),
        "BOPTEST chiller chain should preserve MultiChillers.P -> ch.P -> ElectricEIR QEva_flow_set/PLR/curve-chain power:\n{rendered}"
    );
    assert!(
        !rendered.contains("plr1=fmin(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_per_PLRMax, fmax(0.0, -(chilledWaterPlant_chillerPlant_mulChiSys_ch_1_chi_QEva_flow)")
            && !rendered.contains("cop=0.0")
            && !rendered.contains("qset=0.0")
            && !rendered.contains("no equation found"),
        "ElectricEIR COP/PLR1/QEva_flow_set must be lowered from PartialElectric equations:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_parameter_binding_preserves_boptest_chiller_nominals() {
    let dae_json = serde_json::json!({});
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
q={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.per.QEva_flow_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
cop={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.mulChiSys.ch[3].chi.per.COP_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
plr={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.mulChiSys.ch[2].chi.per.PLRMax", {"Literal": {"Real": 0.0}}, 0, cfg) }}
cap4={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.per.capFunT", {"Literal": {"Real": 0.0}}, 4, cfg) }}
eirt2={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.per.EIRFunT", {"Literal": {"Real": 0.0}}, 2, cfg) }}
eirplr3={{ parameter_binding_rhs("chilledWaterPlant.chillerPlant.mulChiSys.ch[1].chi.per.EIRFunPLR", {"Literal": {"Real": 0.0}}, 3, cfg) }}
wrapper={{ parameter_binding_rhs("chilledWaterPlant.nominalChillerCapacity", {"Literal": {"Real": 0.0}}, 0, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("q=-2391666.6666666665")
            && rendered.contains("cop=5.06")
            && rendered.contains("plr=1.07")
            && rendered.contains("cap4=5.574667E-02")
            && rendered.contains("eirt2=-3.024605E-02")
            && rendered.contains("eirplr3=5.818753E-01")
            && rendered.contains("wrapper=2391666.6666666665"),
        "BOPTEST chiller nominal, wrapper capacity, and ElectricEIR curve parameters should come from original datChi records:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_hot_water_load_temperatures_and_boilers() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "hotWaterPlant.boilerPlant.realExpression.y": {},
            "hotWaterPlant.boilerPlant.QTot.y": {},
            "hotWaterPlant.boilerPlant.THW_sup": {},
            "hotWaterPlant.boilerPlant.THW_ret": {},
            "reaHotWatSys_QBoi_1_y": {},
            "reaHotWatSys_PBoi_1_y": {},
            "reaHotWatSys_mHWBranch_1_y": {},
            "reaHotWatSys_mHWBranch_2_y": {},
            "hotWaterPlant.hotWaterDistributionNetwork.junctionBranchMassFlow[3]": {},
            "hotWaterPlant.boilerPlant.boiSta.y[1]": {},
            "hotWaterPlant.boilerPlant.boiSta.y[2]": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
load={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.realExpression.y", dae, cfg) }}
qtot={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.QTot.y", dae, cfg) }}
sup={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.THW_sup", dae, cfg) }}
ret={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.THW_ret", dae, cfg) }}
q1={{ alg_rhs_for_var_with_dae("reaHotWatSys_QBoi_1_y", dae, cfg) }}
p1={{ alg_rhs_for_var_with_dae("reaHotWatSys_PBoi_1_y", dae, cfg) }}
mbr1={{ alg_rhs_for_var_with_dae("reaHotWatSys_mHWBranch_1_y", dae, cfg) }}
mbr2={{ alg_rhs_for_var_with_dae("reaHotWatSys_mHWBranch_2_y", dae, cfg) }}
mbr3={{ alg_rhs_for_var_with_dae("hotWaterPlant.hotWaterDistributionNetwork.junctionBranchMassFlow[3]", dae, cfg) }}
stage1={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.boiSta.y[1]", dae, cfg) }}
stage2={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.boiSta.y[2]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "load=fmax(0.0, (floor1BoptestAirNetwork_hotWaterReheatThermalLoad + floor2BoptestAirNetwork_hotWaterReheatThermalLoad + floor3BoptestAirNetwork_hotWaterReheatThermalLoad))"
        ) && rendered.contains(
            "qtot=(hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_QWat_flow + hotWaterPlant_boilerPlant_mulBoi_boi_2_boi_QWat_flow)"
        ),
        "BOPTEST BoilerPlant load should synthesize from the three BOPTEST floor reheat loads:\n{rendered}"
    );
    assert!(
        !rendered.contains("heating_capacity_10")
            && !rendered.contains("zone_terminal_reheat_command_15")
            && !rendered.contains("floor_multiplier_2"),
        "BOPTEST BoilerPlant load must not fall back to reduced capacity-command reheat demand:\n{rendered}"
    );
    assert!(
        rendered.contains("sup=fmax(291.15, fmin(353.15, hotWaterPlant_plantHotWaterSetpoint))"),
        "BOPTEST BoilerPlant THW_sup should synthesize from the clamped plant setpoint:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "ret=fmax(273.15, (fmax(291.15, fmin(353.15, hotWaterPlant_plantHotWaterSetpoint))"
        ) && rendered.contains("4200.0")
            && rendered
                .contains("hotWaterPlant_boilerPlant_pumSecHW_pum_1_varSpeFloMov_rho_default"),
        "BOPTEST BoilerPlant THW_ret should synthesize from load and secondary-pump mass flow:\n{rendered}"
    );
    assert!(
        rendered.contains("q1=hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_QWat_flow")
            && rendered.contains("p1=hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_QFue_flow")
            && !rendered.contains("p1=(fmin(2619375.0,")
            && !rendered.contains("/ 0.8"),
        "BOPTEST BoilerPlant per-boiler heat and fuel should read the original BoilerPolynomial fields:\n{rendered}"
    );
    assert!(
        rendered
            .contains("mbr1=hotWaterPlant_hotWaterDistributionNetwork_junctionBranchMassFlow_1")
            && rendered.contains(
                "mbr2=hotWaterPlant_hotWaterDistributionNetwork_junctionBranchMassFlow_2"
            )
            && rendered.contains(
                "mbr3=(-hotWaterPlant_hotWaterDistributionNetwork_junSup2_port_2_m_flow)"
            )
            && !rendered.contains("* 0.08333333333333333")
            && !rendered.contains("* 0.8333333333333334"),
        "BOPTEST BoilerPlant HW branch mass flows should read the source-owned PipeNetwork wrapper chain, not a fixed 1:10:1 split:\n{rendered}"
    );
    assert!(
        rendered.contains("stage1=((fmax(0.0,")
            && rendered.contains("> 0.1 ? 1.0 : 0.0)")
            && rendered.contains("stage2=((fmax(0.0,")
            && rendered.contains("> (0.95 * 2619375.0) ? 1.0 : 0.0)"),
        "BOPTEST BoilerPlant stage outputs should synthesize from load thresholds:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_preserves_boptest_three_branch_water_network_flows() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "reaChiWatSys_mCHWBranch_2_y": {},
            "reaHotWatSys_mHWBranch_1_y": {},
            "chilledWaterPlant.chilledWaterDistributionNetwork.junctionBranchMassFlow[2]": {},
            "hotWaterPlant.hotWaterDistributionNetwork.junctionBranchMassFlow[1]": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
chw_read={{ alg_rhs_for_var_with_dae("reaChiWatSys_mCHWBranch_2_y", dae, cfg) }}
hw_read={{ alg_rhs_for_var_with_dae("reaHotWatSys_mHWBranch_1_y", dae, cfg) }}
chw_branch={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chilledWaterDistributionNetwork.junctionBranchMassFlow[2]", dae, cfg) }}
hw_branch={{ alg_rhs_for_var_with_dae("hotWaterPlant.hotWaterDistributionNetwork.junctionBranchMassFlow[1]", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "chw_read=chilledWaterPlant_chilledWaterDistributionNetwork_junctionBranchMassFlow_2"
        ) && rendered.contains(
            "hw_read=hotWaterPlant_hotWaterDistributionNetwork_junctionBranchMassFlow_1"
        ) && rendered.contains(
            "chw_branch=(-chilledWaterPlant_chilledWaterDistributionNetwork_junSup2_port_3_m_flow)"
        ) && rendered.contains(
            "hw_branch=(-hotWaterPlant_hotWaterDistributionNetwork_junSup1_port_3_m_flow)"
        ) && !rendered.contains("* 0.08333333333333333")
            && !rendered.contains("* 0.8333333333333334"),
        "BOPTEST branch flow read surfaces should preserve BoptestThreeBranchWaterNetwork -> PipeNetwork ports_b flow:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_parameter_binding_preserves_boptest_boiler_polynomial_nominals() {
    let dae_json = serde_json::json!({});
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
q={{ parameter_binding_rhs("hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.Q_flow_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
eta={{ parameter_binding_rhs("hotWaterPlant.boilerPlant.mulBoi.boi[2].boi.eta_nominal", {"Literal": {"Real": 0.0}}, 0, cfg) }}
eps={{ parameter_binding_rhs("hotWaterPlant.boilerPlant.mulBoi.boi[2].boi.eps", {"Literal": {"Real": 0.0}}, 0, cfg) }}
mdry={{ parameter_binding_rhs("hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.mDry", {"Literal": {"Real": 0.0}}, 0, cfg) }}
cap={{ parameter_binding_rhs("hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.heaCapDry.C", {"Literal": {"Real": 0.0}}, 0, cfg) }}
wrapper={{ parameter_binding_rhs("hotWaterPlant.nominalBoilerCapacity", {"Literal": {"Real": 0.0}}, 0, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("q=3276000.0")
            && rendered.contains("eta=0.8")
            && rendered.contains("eps=1.0")
            && rendered.contains(
                "mdry=(0.0015 * hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_Q_flow_nominal)"
            )
            && rendered.contains("cap=(500.0 * hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_mDry)")
            && rendered.contains("wrapper=2619375.0"),
        "BOPTEST BoilerPolynomial nominal, dry-mass, dry-capacity, and wrapper capacity parameters should come from the original source bindings:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_synthesizes_boptest_plant_wrapper_diagnostics() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "chilledWaterPlant.plantEnable": {},
            "chilledWaterPlant.chillerStagingEnable": {},
            "chilledWaterPlant.pumpStagingEnable": {},
            "hotWaterPlant.plantEnable": {},
            "hotWaterPlant.boilerStagingEnable": {},
            "hotWaterPlant.pumpStagingEnable": {}
        },
        "w": {
            "chilledWaterPlant.enabledCirculationState": {},
            "chilledWaterPlant.chillerStageCount": {},
            "chilledWaterPlant.chillerThermalLoad": {},
            "chilledWaterPlant.chillerElectricalPower": {},
            "chilledWaterPlant.chillerPartLoadRatio": {},
            "chilledWaterPlant.equivalentEnergyInputRatio": {},
            "hotWaterPlant.enabledCirculationState": {},
            "hotWaterPlant.boilerStageCount": {},
            "hotWaterPlant.boilerThermalLoad": {},
            "hotWaterPlant.boilerFuelPower": {},
            "hotWaterPlant.boilerPartLoadRatio": {},
            "hotWaterPlant.equivalentBoilerEfficiency": {},
            "hotWaterPlant.boilerEnable[1]": {}
        },
        "z": {},
        "m": {
            "hotWaterPlant.boilerPlant.boiSta.nSta.iOn[1].active": {},
            "hotWaterPlant.boilerPlant.boiSta.nSta.iOn[2].active": {},
            "hotWaterPlant.boilerPlant.boiSta.nSta.nOn.active": {}
        },
        "u": {},
        "p": {
            "chilledWaterPlant.nominalChillerCapacity": {},
            "hotWaterPlant.nominalBoilerCapacity": {},
            "hotWaterPlant.boilerEfficiency": {}
        },
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
chw_enabled={{ alg_rhs_for_var_with_dae("chilledWaterPlant.enabledCirculationState", dae, cfg) }}
hw_enabled={{ alg_rhs_for_var_with_dae("hotWaterPlant.enabledCirculationState", dae, cfg) }}
boiler_enable={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerEnable[1]", dae, cfg) }}
chw_plr={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPartLoadRatio", dae, cfg) }}
chw_eir={{ alg_rhs_for_var_with_dae("chilledWaterPlant.equivalentEnergyInputRatio", dae, cfg) }}
hw_plr={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPartLoadRatio", dae, cfg) }}
hw_eff={{ alg_rhs_for_var_with_dae("hotWaterPlant.equivalentBoilerEfficiency", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("chw_enabled=((chilledWaterPlant_pumpStagingEnable > 0.5 || chilledWaterPlant_plantEnable > 0.5 || chilledWaterPlant_chillerStagingEnable > 0.5) ? 1.0 : 0.0)")
            && rendered.contains("hw_enabled=((hotWaterPlant_pumpStagingEnable > 0.5 || hotWaterPlant_plantEnable > 0.5 || hotWaterPlant_boilerStagingEnable > 0.5) ? 1.0 : 0.0)"),
        "BOPTEST plant enabledCirculationState diagnostics should preserve the wrapper enable logic:\n{rendered}"
    );
    assert!(
        rendered.contains("boiler_enable=(((hotWaterPlant_boilerPlant_boiSta_nSta_iOn_1_active ? 1.0 : 0.0) + (hotWaterPlant_boilerPlant_boiSta_nSta_iOn_2_active ? 2.0 : 0.0) + (hotWaterPlant_boilerPlant_boiSta_nSta_nOn_active ? 3.0 : 0.0)) >= 1.0 ? 1.0 : 0.0)"),
        "BOPTEST hot-water boilerEnable should read the original boiSta stage chain:\n{rendered}"
    );
    assert!(
        rendered.contains("chw_plr=(chilledWaterPlant_chillerStageCount <= 0.5 ? 0.0 : fmin(1.2, chilledWaterPlant_chillerThermalLoad / (chilledWaterPlant_chillerStageCount * chilledWaterPlant_nominalChillerCapacity)))")
            && rendered.contains("chw_eir=(chilledWaterPlant_chillerThermalLoad <= 1e-6 ? 0.0 : fmax(0.0, chilledWaterPlant_chillerElectricalPower / chilledWaterPlant_chillerThermalLoad))")
            && rendered.contains("hw_plr=(hotWaterPlant_boilerStageCount <= 0.5 ? 0.0 : fmin(1.2, hotWaterPlant_boilerThermalLoad / (hotWaterPlant_boilerStageCount * hotWaterPlant_nominalBoilerCapacity)))")
            && rendered.contains("hw_eff=(hotWaterPlant_boilerFuelPower <= 1e-6 ? hotWaterPlant_boilerEfficiency : fmax(0.0, fmin(1.0, hotWaterPlant_boilerThermalLoad / hotWaterPlant_boilerFuelPower)))"),
        "BOPTEST plant PLR/EIR/efficiency diagnostics should preserve the wrapper load and power formulas:\n{rendered}"
    );
    assert!(
        !rendered.contains("WARNING: no equation found"),
        "BOPTEST plant wrapper diagnostics must not fall back to missing-equation zeros:\n{rendered}"
    );
}

#[test]
fn test_embedded_c_alg_rhs_preserves_boptest_boiler_polynomial_chain() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.y": {},
            "hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.eta": {},
            "hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.QFue_flow": {},
            "hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.QWat_flow": {},
            "hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.heaCapDry.der_T": {},
            "hotWaterPlant.boilerPlant.mulBoi.boi[2].boi.QFue_flow": {},
            "hotWaterPlant.boilerPlant.mulBoi.boi[2].boi.QWat_flow": {},
            "reaHotWatSys_reaPBoi_y": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
y={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.y", dae, cfg) }}
eta={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.eta", dae, cfg) }}
qfue={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.QFue_flow", dae, cfg) }}
qwat={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.QWat_flow", dae, cfg) }}
dert={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.heaCapDry.der_T", dae, cfg) }}
ptot={{ alg_rhs_for_var_with_dae("reaHotWatSys_reaPBoi_y", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("y=hotWaterPlant_boilerPlant_mulBoi_boi_1_conPI_y")
            && rendered.contains("eta=hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_a_1")
            && rendered.contains("qfue=fmax(0.0, hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_y * hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_Q_flow_nominal / fmax(0.00001, hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_eta_nominal))")
            && rendered.contains("qwat=fmax(0.0, hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_eta * hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_QFue_flow * hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_eps)")
            && rendered.contains("dert=((hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_heaCapDry_C > 1e-9) ? (hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_heaCapDry_port_Q_flow / hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_heaCapDry_C) : 0.0)")
            && rendered.contains("ptot=(hotWaterPlant_boilerPlant_mulBoi_boi_1_boi_QFue_flow + hotWaterPlant_boilerPlant_mulBoi_boi_2_boi_QFue_flow)"),
        "BOPTEST BoilerPolynomial and dry heat-capacitor chain should use original component fields, not branch-load shortcuts:\n{rendered}"
    );
    assert!(
        !rendered.contains("2619375.0") && !rendered.contains("/ 0.8"),
        "BOPTEST BoilerPolynomial chain must not use staging-capacity or direct efficiency shortcuts:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_stategraph_transition_timer() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "sg.transition.t_dummy": {}
        },
        "w": {
            "sg.transition.t": {}
        },
        "z": {},
        "m": {
            "sg.transition.enableFire": {}
        },
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
t={{ alg_rhs_for_var_with_dae("sg.transition.t", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("t=(sg_transition_enableFire ? sg_transition_t_dummy : 0.0)"),
        "Modelica.StateGraph.PartialTransition.t should follow if enableFire then t_dummy else 0:\n{rendered}"
    );
    assert!(
        !rendered.contains("no equation found"),
        "StateGraph transition timer output should not fall back to a missing-equation warning:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_boptest_stage_condition_io_connections() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "chilledWaterPlant.chillerPlant.chiSta.loa": {},
            "chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.Loa": {},
            "chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.cap_avi": {},
            "chilledWaterPlant.chillerPlant.chiSta.sta[1]": {},
            "chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.Status[1]": {},
            "chilledWaterPlant.chillerPlant.Loa.y": {},
            "chilledWaterPlant.chillerPlant.chiSta.y[1]": {},
            "hotWaterPlant.boilerPlant.boiSta.loa": {},
            "hotWaterPlant.boilerPlant.boiSta.plantNStageCondition.Loa": {},
            "hotWaterPlant.boilerPlant.realExpression.y": {}
        },
        "w": {},
        "z": {},
        "m": {},
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
loa={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.chiSta.loa", dae, cfg) }}
stage_loa={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.Loa", dae, cfg) }}
sta={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.chiSta.sta[1]", dae, cfg) }}
status={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.Status[1]", dae, cfg) }}
cap={{ alg_rhs_for_var_with_dae("chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.cap_avi", dae, cfg) }}
hw_loa={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.boiSta.loa", dae, cfg) }}
hw_stage_loa={{ alg_rhs_for_var_with_dae("hotWaterPlant.boilerPlant.boiSta.plantNStageCondition.Loa", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains("loa=chilledWaterPlant_chillerPlant_Loa_y")
            && rendered.contains("stage_loa=chilledWaterPlant_chillerPlant_Loa_y"),
        "BOPTEST PlantStageN load inputs should read original ChillerPlant Loa.y:\n{rendered}"
    );
    assert!(
        rendered.contains("sta=chilledWaterPlant_chillerPlant_chiSta_y_1")
            && rendered.contains("status=chilledWaterPlant_chillerPlant_chiSta_y_1"),
        "BOPTEST PlantStageN status inputs should read the same-step chiller stage output:\n{rendered}"
    );
    assert!(
        rendered.contains("cap=(chilledWaterPlant_chillerPlant_chiSta_y_1 * chilledWaterPlant_chillerPlant_chiSta_plantNStageCondition_Cap_1)")
            && rendered.contains("(chilledWaterPlant_chillerPlant_chiSta_y_3 * chilledWaterPlant_chillerPlant_chiSta_plantNStageCondition_Cap_3)"),
        "BOPTEST PlantStageCondition cap_avi should use same-step stage outputs and capacity vector:\n{rendered}"
    );
    assert!(
        rendered.contains("hw_loa=hotWaterPlant_boilerPlant_realExpression_y")
            && rendered.contains("hw_stage_loa=hotWaterPlant_boilerPlant_realExpression_y"),
        "BOPTEST BoilerPlant PlantStageN load inputs should read original realExpression.y:\n{rendered}"
    );
}

#[test]
fn test_fmi_alg_rhs_synthesizes_stategraph_stage_from_dae_variables_without_fx() {
    let dae_json = serde_json::json!({
        "f_x": [],
        "x": {},
        "y": {
            "plant.nSta.stateGraphRoot.subgraphStatePort.activeSteps": {},
            "plant.nSta.multiSwitch.y": {},
            "plant.nSta.y": {}
        },
        "w": {},
        "z": {},
        "m": {
            "plant.nSta.iOn[1].active": {},
            "plant.nSta.iOn[2].active": {},
            "plant.nSta.nOn.active": {},
            "plant.chiSta.nSta.iOn[1].active": {},
            "plant.chiSta.nSta.iOn[2].active": {},
            "plant.chiSta.nSta.nOn.active": {},
            "chilledWaterPlant.chillerPlant.chiSta.nSta.iOn[1].active": {},
            "chilledWaterPlant.chillerPlant.chiSta.nSta.iOn[2].active": {},
            "chilledWaterPlant.chillerPlant.chiSta.nSta.nOn.active": {}
        },
        "u": {},
        "p": {},
        "constants": {}
    });
    let template = r#"
{% set cfg = {"prefix": "", "power": "pow", "if_style": "ternary", "subscript_underscore": true} %}
steps={{ alg_rhs_for_var_with_dae("plant.nSta.stateGraphRoot.subgraphStatePort.activeSteps", dae, cfg) }}
stage={{ alg_rhs_for_var_with_dae("plant.nSta.multiSwitch.y", dae, cfg) }}
stage_vec={{ alg_rhs_for_var_with_dae("plant.chiSta.y[2]", dae, cfg) }}
read={{ alg_rhs_for_var_with_dae("reaChiWatSys_ChiSta_2_y", dae, cfg) }}
"#;
    let rendered = render_template_with_dae_json(&dae_json, template).unwrap();

    assert!(
        rendered.contains(
            "steps=plant_nSta_iOn_1_active + plant_nSta_iOn_2_active + plant_nSta_nOn_active"
        ),
        "StateGraph activeSteps should synthesize from DAE variable maps when f_x is empty:\n{rendered}"
    );
    assert!(
        rendered.contains("(plant_nSta_iOn_1_active ? 1.0 : 0.0)")
            && rendered.contains("(plant_nSta_iOn_2_active ? 2.0 : 0.0)")
            && rendered.contains("(plant_nSta_nOn_active ? 3.0 : 0.0)"),
        "StageN multiSwitch.y should synthesize from DAE variable maps when f_x is empty:\n{rendered}"
    );
    assert!(
        rendered.contains("stage_vec=(((plant_chiSta_nSta_iOn_1_active ? 1.0 : 0.0) + (plant_chiSta_nSta_iOn_2_active ? 2.0 : 0.0) + (plant_chiSta_nSta_nOn_active ? 3.0 : 0.0)) >= 2.0 ? 1.0 : 0.0)")
            && rendered.contains("read=(((chilledWaterPlant_chillerPlant_chiSta_nSta_iOn_1_active ? 1.0 : 0.0) + (chilledWaterPlant_chillerPlant_chiSta_nSta_iOn_2_active ? 2.0 : 0.0) + (chilledWaterPlant_chillerPlant_chiSta_nSta_nOn_active ? 3.0 : 0.0)) >= 2.0 ? 1.0 : 0.0)"),
        "StageN vector/read outputs should synthesize from DAE variable maps when f_x is empty:\n{rendered}"
    );
}
