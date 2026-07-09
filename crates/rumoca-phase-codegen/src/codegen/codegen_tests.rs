// SPEC_0021 file-size exception: codegen regression coverage is still grouped
// around template behavior. split plan: move target-specific regression suites
// into focused test modules alongside their renderers.

use super::*;
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rumoca_ir_solve as solve;

mod backend_template_tests;

fn normalize_newlines(input: &str) -> String {
    input.replace("\r\n", "\n")
}

pub(super) fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
}

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("codegen_solve_fixture.mo"),
        1,
        2,
    )
}

#[test]
fn condition_aliases_use_condition_equation_span() {
    let span = fixture_span();
    let mut dae = dae::Dae::new();
    dae.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::Reference::new("__c0"),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(true),
            span,
        },
        span,
        "condition equation",
    ));

    let aliases = condition_aliases_from_dae(&dae).expect("condition aliases should serialize");
    let condition = aliases[0]
        .get("condition")
        .cloned()
        .expect("condition alias should include condition expression");
    let condition: rumoca_core::Expression =
        serde_json::from_value(condition).expect("condition alias should deserialize");

    assert_eq!(condition.span(), Some(span));
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
            span: fixture_span(),
        }],
    };
    problem
}

pub(super) fn solve_problem_with_two_by_two_linsolve_derivative() -> solve::SolveProblem {
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
            span: fixture_span(),
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
    let mut function = rumoca_core::Function::new("ExternalUser", fixture_span());
    function.add_output(rumoca_core::FunctionParam::new("y", "Real", fixture_span()));
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
    let mut function = rumoca_core::Function::new("ExternalFileUser", fixture_span());
    function.add_output(rumoca_core::FunctionParam::new("y", "Real", fixture_span()));
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
        "{{ ir_kind }} {{ solve.continuous.residual.nodes | length }} {{ ir.continuous.residual.nodes | length }} {{ solve_blocks.continuous.residual.scalar_programs.programs | length }}",
    )
    .unwrap();
    assert_eq!(solve_rendered, "solve 0 0 0");

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

    // The 2×2 linsolve now lowers to ONE multi-output scalar program (2 outputs)
    // rather than two single-output programs.
    assert_eq!(rendered, "1 1 1 true");
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
fn test_rust_fixed_solve_builtin_target_renders_fixed_derivative_kernel() {
    let problem = solve_problem_with_one_by_one_matmul_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-fixed-solve", "model_fixed_solve.rs.jinja"),
        "TensorDemo",
    )
    .expect("rust-fixed-solve template should render");

    assert!(rendered.contains("pub type State = [Scalar; Y_LEN];"));
    assert!(rendered.contains("pub type Parameters = [Scalar; P_LEN];"));
    assert!(rendered.contains("pub fn derivative_rhs_into"));
    assert!(rendered.contains("out[0] ="));
    assert!(!rendered.contains("Vec<"));
    assert!(!rendered.contains("to_vec"));
}

#[test]
fn test_rust_fixed_solve_builtin_target_syntax_checks_when_rustc_available() {
    if std::process::Command::new("rustc")
        .arg("--version")
        .output()
        .is_err()
    {
        eprintln!("skipping rust-fixed-solve syntax smoke: rustc not available");
        return;
    }

    let problem = solve_problem_with_one_by_one_matmul_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let source = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-fixed-solve", "model_fixed_solve.rs.jinja"),
        "TensorDemo",
    )
    .expect("rust-fixed-solve source should render");
    let dir = std::env::temp_dir().join(format!(
        "rumoca_rust_fixed_solve_smoke_{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create rust-fixed-solve smoke dir");
    let source_path = dir.join("TensorDemo_fixed_solve.rs");
    std::fs::write(&source_path, source).expect("write generated rust-fixed-solve source");

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
        "generated rust-fixed-solve source must pass rustc syntax check\nstdout:\n{}\nstderr:\n{}",
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
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
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
    let mut function = rumoca_core::Function::new("LieGroup.SO3.rotationMatrix", fixture_span());
    function
        .add_input(rumoca_core::FunctionParam::new("q", "Real", fixture_span()).with_dims(vec![4]));
    function.add_output(
        rumoca_core::FunctionParam::new("R", "Real", fixture_span()).with_dims(vec![3, 3]),
    );
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
fn dae_template_json_rejects_source_ref_dimension_overflow() {
    let mut dae = dae::Dae::new();
    dae.variables.algebraics.insert(
        "huge".into(),
        dae::Variable {
            name: "huge".into(),
            dims: vec![i64::MAX],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let result = dae_template_json(&dae);

    #[cfg(target_pointer_width = "32")]
    {
        let err = result.expect_err("oversized source-ref dimension should fail");
        assert!(
            err.to_string()
                .contains("source ref dimension 9223372036854775807 for `huge`"),
            "{err:?}"
        );
    }

    #[cfg(target_pointer_width = "64")]
    {
        let err = result.expect_err("oversized source-ref enumeration should fail");
        assert!(
            err.to_string()
                .contains("source ref scalar count for `huge`"),
            "{err:?}"
        );
        assert!(
            err.to_string().contains("exceeds enumeration limit"),
            "{err:?}"
        );
    }
}

#[cfg(target_pointer_width = "64")]
#[test]
fn dae_template_json_rejects_source_ref_scalar_count_overflow() {
    let mut dae = dae::Dae::new();
    dae.variables.algebraics.insert(
        "huge".into(),
        dae::Variable {
            name: "huge".into(),
            dims: vec![i64::MAX, 3],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let err = dae_template_json(&dae).expect_err("oversized source-ref shape should fail");

    assert!(
        err.to_string()
            .contains("source ref scalar count for `huge` overflows host index range"),
        "{err:?}"
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
    // The root condition `y[0] - 0` is materialized into a temp whose RHS is the
    // inline subtraction; the event indicator is assigned from that temp.
    assert!(rendered.contains("((__rumoca_solve_y(m, 0)) - (0.0))"));
    assert!(rendered.contains("m->event_indicators[0] = __r"));
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
        dae::Variable::new(
            rumoca_core::VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
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
        span: rumoca_core::Span::DUMMY,
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
                    ]],
                    "output_indices": [0]
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

    // `der = -y` materializes the negation into a temp assigned to xdot[0].
    assert!(
        rendered.contains("(-(__rumoca_solve_y(m, 0)))") && rendered.contains("m->xdot[0] = __r"),
        "FMI3 derivatives should come from solve derivative rows, got:\n{rendered}"
    );
    assert!(rendered.contains("memset(m->xdot, 0, sizeof(m->xdot));"));
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
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
    }
    let template = r#"
{% set policy = {"separator": "_", "reserved": [], "generated_prefixes": []} %}
{% set symbols = target_symbols(dae.symbol_refs, policy, dae.symbol_aliases) %}
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
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae.variables.algebraics.insert(
        "leg_f_b_2_1".into(),
        dae::Variable {
            name: "leg_f_b_2_1".into(),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    let template = r#"
{% set policy = {"separator": "_", "reserved": [], "generated_prefixes": []} %}
{% set symbols = target_symbols(dae.symbol_refs, policy, dae.symbol_aliases) %}
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
fn test_source_ref_template_helper_rejects_nonnumeric_flat_index() {
    let dae = dae::Dae::new();
    let err = render_template(&dae, r#"{{ source_ref("x", [4], "bad") }}"#)
        .expect_err("nonnumeric source_ref flat index should fail rendering");

    assert!(
        err.to_string()
            .contains("source_ref flat index `bad` is not numeric"),
        "{err:?}"
    );
}

#[test]
fn test_source_ref_template_helper_rejects_zero_flat_index() {
    let dae = dae::Dae::new();
    let err = render_template(&dae, r#"{{ source_ref("x", [4], 0) }}"#)
        .expect_err("zero source_ref flat index should fail rendering");

    assert!(
        err.to_string()
            .contains("source_ref flat index must be one-based"),
        "{err:?}"
    );
}

#[test]
fn test_source_ref_template_helper_rejects_out_of_range_flat_index() {
    let dae = dae::Dae::new();
    let err = render_template(&dae, r#"{{ source_ref("x", [2], 3) }}"#)
        .expect_err("out-of-range source_ref flat index should fail rendering");

    assert!(
        err.to_string()
            .contains("source_ref flat index 3 exceeds dimensions [2]"),
        "{err:?}"
    );
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
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
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
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
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
