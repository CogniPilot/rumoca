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
