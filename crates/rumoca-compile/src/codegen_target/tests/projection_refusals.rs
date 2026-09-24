//! The shared ME projection refuses a block with alternate reduced charts, a
//! runtime shape it does not execute, at the Solve admissibility gate and
//! again in the renderer, before any byte. The retained state-manifold case
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
