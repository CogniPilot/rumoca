//! WGSL solve-target render tests (split from `codegen_tests` to stay
//! under the SPEC_0021 file-size limit).

use super::codegen_tests::{builtin_template, solve_problem_with_two_by_two_linsolve_derivative};
use super::render_solve_template_with_name;
use rumoca_ir_solve as solve;

fn solve_problem_with_scalar_derivative_rows() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock {
                programs: vec![
                    // der0 = -p[0] * y[0]
                    vec![
                        solve::LinearOp::LoadP { dst: 0, index: 0 },
                        solve::LinearOp::LoadY { dst: 1, index: 0 },
                        solve::LinearOp::Binary {
                            dst: 2,
                            op: solve::BinaryOp::Mul,
                            lhs: 0,
                            rhs: 1,
                        },
                        solve::LinearOp::Unary {
                            dst: 3,
                            op: solve::UnaryOp::Neg,
                            arg: 2,
                        },
                        solve::LinearOp::StoreOutput { src: 3 },
                    ],
                    // der1 = if y[0] > 2 then 1 else pow(y[0], 3)
                    vec![
                        solve::LinearOp::LoadY { dst: 0, index: 0 },
                        solve::LinearOp::Const { dst: 1, value: 2.0 },
                        solve::LinearOp::Compare {
                            dst: 2,
                            op: solve::CompareOp::Gt,
                            lhs: 0,
                            rhs: 1,
                        },
                        solve::LinearOp::Const { dst: 3, value: 1.0 },
                        solve::LinearOp::Const { dst: 4, value: 3.0 },
                        solve::LinearOp::Binary {
                            dst: 5,
                            op: solve::BinaryOp::Pow,
                            lhs: 0,
                            rhs: 4,
                        },
                        solve::LinearOp::Select {
                            dst: 6,
                            cond: 2,
                            if_true: 3,
                            if_false: 5,
                        },
                        solve::LinearOp::StoreOutput { src: 6 },
                    ],
                ],
                ..Default::default()
            },
        )],
    };
    problem
}

fn solve_problem_with_affine_stencil_derivative() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::AffineStencil {
            count: 8,
            domain: solve::AffineStencilDomain {
                index_names: vec!["i".to_string()],
                iterations: (0..8)
                    .map(|idx| solve::AffineStencilIteration {
                        index_values: vec![idx + 1],
                    })
                    .collect(),
            },
            base_ops: vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::LoadY { dst: 1, index: 0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::Unary {
                    dst: 3,
                    op: solve::UnaryOp::Neg,
                    arg: 2,
                },
                solve::LinearOp::StoreOutput { src: 3 },
            ],
            load_strides: vec![
                solve::AffineStencilLoadStride {
                    op_position: 0,
                    stride: 0,
                },
                solve::AffineStencilLoadStride {
                    op_position: 1,
                    stride: 1,
                },
            ],
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };
    problem
}

#[test]
fn test_wgsl_solve_builtin_target_renders_row_parallel_kernel() {
    let problem = solve_problem_with_scalar_derivative_rows();
    let artifacts = solve::SolveArtifacts::default();
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-solve", "model_solve.wgsl.jinja"),
        "WgslDemo",
    )
    .expect("wgsl-solve template should render scalar rows");

    assert!(rendered.contains("fn derivative_rhs_chunk0"));
    assert!(rendered.contains("const ROWS: u32 = 2u;"));
    // der0: negated product of p[0] and y[0]
    assert!(rendered.contains("out[0] = (-(((p[0]) * (y[0]))));"));
    // der1: select-based conditional with float-forced constants and pow
    assert!(rendered.contains("select(0.0, 1.0, (y[0]) > (2.0))"));
    assert!(rendered.contains("pow(y[0], 3.0)"));
    assert!(
        !rendered.contains(" ? "),
        "WGSL output must not contain C ternaries: {rendered}"
    );
}

#[test]
fn test_wgsl_solve_builtin_target_renders_native_stencil_kernel() {
    let problem = solve_problem_with_affine_stencil_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-solve", "model_solve.wgsl.jinja"),
        "StencilDemo",
    )
    .expect("wgsl-solve template should render native stencil node");

    assert!(rendered.contains("fn derivative_rhs_stencil0"));
    assert!(rendered.contains("const ROWS: u32 = 8u;"));
    assert!(rendered.contains("out[0u + r] = (-(((p[0]) * (y[0u + r]))));"));
    assert!(
        !rendered.contains("fn derivative_rhs_chunk0"),
        "pure stencil block should not emit residual chunk: {rendered}"
    );
}

#[test]
fn test_wgsl_solve_builtin_target_declines_linear_solve_component() {
    let problem = solve_problem_with_two_by_two_linsolve_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let error = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-solve", "model_solve.wgsl.jinja"),
        "TensorDemo",
    )
    .expect_err("wgsl-solve must decline implicit linear blocks");

    assert!(
        error.to_string().contains("LinearSolveComponent"),
        "decline should name the unsupported op: {error}"
    );
}

#[test]
fn test_wgsl_solve_layout_manifest_renders_bindings() {
    let problem = solve_problem_with_scalar_derivative_rows();
    let artifacts = solve::SolveArtifacts::default();
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-solve", "model_layout.json.jinja"),
        "WgslDemo",
    )
    .expect("wgsl-solve layout manifest should render");

    let parsed: serde_json::Value =
        serde_json::from_str(&rendered).expect("layout manifest must be valid JSON");
    assert_eq!(parsed["model"], "WgslDemo");
    assert_eq!(parsed["kernel_prefix"], "derivative_rhs_chunk");
    assert_eq!(parsed["chunks"], 1);
    assert_eq!(parsed["rows"], 2);
}

#[test]
fn test_wgsl_solve_layout_manifest_reports_native_stencil_kernel() {
    let problem = solve_problem_with_affine_stencil_derivative();
    let artifacts = solve::SolveArtifacts::default();
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-solve", "model_layout.json.jinja"),
        "StencilDemo",
    )
    .expect("wgsl-solve layout manifest should render native stencil node");

    let parsed: serde_json::Value =
        serde_json::from_str(&rendered).expect("layout manifest must be valid JSON");
    assert_eq!(parsed["rows"], 8);
    assert_eq!(parsed["chunks"], 0);
    assert_eq!(parsed["kernels"][0]["entry"], "derivative_rhs_stencil0");
    assert_eq!(parsed["kernels"][0]["rows"], 8);
}
