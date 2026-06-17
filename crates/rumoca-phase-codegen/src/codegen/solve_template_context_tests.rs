use super::*;
use rumoca_ir_solve as solve;

fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
}

fn single_slot_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn implicit_problem_with_artifacts() -> (solve::SolveProblem, solve::SolveArtifacts) {
    let row = single_slot_row();
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            row.clone(),
        ]));
    problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            row.clone(),
        ]));

    let mut artifacts = solve::SolveArtifacts::default();
    artifacts.continuous.implicit_jacobian_v_scalar =
        solve::ScalarProgramBlock::new(vec![row.clone()]);
    artifacts.continuous.full_jacobian_v = solve::ScalarProgramBlock::new(vec![row]);
    (problem, artifacts)
}

fn explicit_problem() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            single_slot_row(),
        ]));
    problem
}

#[test]
fn test_solve_template_context_exposes_optional_rows_as_sequences() {
    let (problem, artifacts) = implicit_problem_with_artifacts();

    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        "{{ solve_implicit_rows | length }} {{ solve_jacobian_rows | length }} {{ solve_full_jacobian_rows | length }}",
        "ImplicitDemo",
    )
    .expect("solve template should render direct optional row sequences");
    assert_eq!(rendered, "1 1 1");

    let mlir = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("mlir", "mlir.mlir.jinja"),
        "ImplicitDemo",
    )
    .expect("mlir template should render optional functions");
    assert!(mlir.contains("func.func @eval_implicit_rhs"));
    assert!(mlir.contains("func.func @eval_jacobian_v"));

    let rendered = render_solve_template_with_name(
        &explicit_problem(),
        &artifacts,
        "{{ solve_implicit_rows | length }} {{ solve_jacobian_rows | length }} {{ solve_full_jacobian_rows | length }}",
        "ExplicitOnlyDemo",
    )
    .expect("implicit JVP rows should be empty without an implicit residual");
    assert_eq!(rendered, "0 0 1");
}

#[test]
fn test_serialized_solve_template_context_exposes_optional_rows_as_sequences() {
    let (problem, artifacts) = implicit_problem_with_artifacts();

    let mut solve_json = serde_json::to_value(&problem).expect("serialize solve problem");
    solve_json["artifacts"] = serde_json::to_value(&artifacts).expect("serialize artifacts");
    let dae_json = serde_json::json!({
        "__ir_kind": "solve",
        "solve": solve_json,
    });
    let rendered = render_template_with_dae_json(
        &dae_json,
        "{{ solve_implicit_rows | length }} {{ solve_jacobian_rows | length }} {{ solve_full_jacobian_rows | length }}",
    )
    .expect("serialized solve context should render direct optional row sequences");
    assert_eq!(rendered, "1 1 1");

    let mut solve_json = serde_json::to_value(explicit_problem()).expect("serialize solve problem");
    solve_json["artifacts"] = serde_json::to_value(&artifacts).expect("serialize artifacts");
    let dae_json = serde_json::json!({
        "__ir_kind": "solve",
        "solve": solve_json,
    });
    let rendered = render_template_with_dae_json(
        &dae_json,
        "{{ solve_implicit_rows | length }} {{ solve_jacobian_rows | length }} {{ solve_full_jacobian_rows | length }}",
    )
    .expect("serialized implicit JVP rows should be empty without implicit residual");
    assert_eq!(rendered, "0 0 1");
}
