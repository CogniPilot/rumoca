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
