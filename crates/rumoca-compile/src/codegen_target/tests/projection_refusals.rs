//! The shared ME projection refuses a block with alternate reduced charts, a
//! runtime shape it does not execute, and an algebraic refresh that leaves a
//! coordinate unsettled, at the Solve admissibility gate and again in the
//! renderer, before any byte. The retained state-manifold case
//! lives with the state-only FMI fixtures of `rumoca-phase-codegen`.

use super::*;
use rumoca_ir_solve as solve;

fn with_alternate_chart() -> solve::SolveProblem {
    let mut projection = solve_with_dependent_algebraic_assignments()
        .continuous
        .algebraic_projection_plan;
    projection.blocks[0]
        .alternate_charts
        .push(solve::BlockTearing {
            tear_y_indices: vec![0],
            residual_rows: vec![0],
            causal_steps: Vec::new(),
        });
    solve_with_dependent_algebraic_projection(projection)
}

fn render_error(problem: solve::SolveProblem) -> String {
    let model = solve::SolveModel {
        problem,
        ..solve::SolveModel::default()
    };
    let component = solve::fmi::FmiComponent::construct(model, Vec::new())
        .expect("the mutated fixture is a checked component");
    let view = component
        .into_codegen_view()
        .try_c()
        .expect("the fixture narrows to the C profile");
    match rumoca_phase_codegen::SolveTemplateRenderer::new_owned_with_fmi(view) {
        Ok(_) => panic!("the renderer must refuse an unexecuted runtime shape"),
        Err(error) => error.to_string(),
    }
}

#[test]
fn projection_gate_and_renderer_refuse_alternate_charts() {
    const REASON: &str = "a projection block carries alternate reduced charts";
    assert!(
        rumoca_phase_codegen::me_refresh_admissible(&solve_with_dependent_algebraic_assignments()),
        "the unmutated fixture is admissible, so the refusal below is the chart's own"
    );
    let problem = with_alternate_chart();
    problem
        .validate()
        .expect("the chart-bearing fixture is a valid Solve problem");
    assert!(!rumoca_phase_codegen::me_refresh_admissible(&problem));
    let error = render_error(problem);
    assert!(
        error.contains("unsupported-feature:algebraic_projection") && error.contains(REASON),
        "the renderer must refuse with `{REASON}`: {error}"
    );
}

/// A causally certified algebraic refresh that settles only `y0`, while the
/// layout declares two algebraic coordinates.
fn solve_with_unsettled_algebraic_coordinate() -> solve::SolveProblem {
    let mut problem = solve_with_dependent_algebraic_assignments();
    let row = problem.continuous.refresh_owners.algebraic().rows[0].clone();
    let partial = solve::RefreshPlan {
        simultaneous_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![problem.continuous.algebraic_projection_plan.blocks[0].clone()],
        },
        simultaneous_block_indices: vec![0],
        rows: vec![row],
        causal_seed_rows: solve::RefreshRowSelection::checked(1, [0])
            .expect("fixture selection is valid"),
        dynamic_causal_seed_rows: solve::RefreshRowSelection::checked(1, [0])
            .expect("fixture selection is valid"),
        causal_solution_certified: true,
        ..solve::RefreshPlan::default()
    };
    problem.continuous.refresh_owners = solve::ContinuousRefreshOwners::checked_for_source(
        &problem.continuous.implicit_rhs,
        partial,
        solve::RefreshPlan::default(),
        solve::RefreshPlan::default(),
        solve::RefreshPlan::default(),
        Vec::new(),
    )
    .expect("a one-row causal refresh is internally consistent");
    problem
}

#[test]
fn projection_gate_and_renderer_refuse_an_unsettled_algebraic_coordinate() {
    const REASON: &str = "the algebraic refresh leaves an algebraic coordinate unsettled";
    let problem = solve_with_unsettled_algebraic_coordinate();
    problem
        .validate()
        .expect("the partially settled fixture is a valid Solve problem");
    assert!(!rumoca_phase_codegen::me_refresh_admissible(&problem));
    let error = render_error(problem);
    assert!(
        error.contains("unsupported-feature:algebraic_projection") && error.contains(REASON),
        "the renderer must refuse with `{REASON}`: {error}"
    );
}
