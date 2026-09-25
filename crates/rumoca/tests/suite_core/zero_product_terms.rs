//! Solve lowering omits the product terms structural incidence proves exactly
//! zero, so the executed rows read exactly the coordinates the block ordering
//! matched and the staged refresh stays certified.

use rumoca::Compiler;
use rumoca_ir_solve::{RefreshStageSchedule, StagedRefreshStep};
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

/// `e[2]` and `e[3]` are proved zero through the non-tunable binding, so
/// `f = -e*fb` solves `fb[1]` alone; `fb[2]` and `fb[3]` come later from it.
const AXIS: &str = "
model ZeroAxisTerms
  final parameter Real e[3] = {1, 0, 0};
  Real f;
  Real fb[3];
  Real x(start = 1, fixed = true);
equation
  f = x;
  f = -e * fb;
  fb[2] = 2 * fb[1];
  fb[3] = fb[2] + fb[1];
  der(x) = fb[3];
end ZeroAxisTerms;";

#[test]
fn a_proved_zero_product_term_keeps_the_derivative_refresh_certified() {
    let dae = Compiler::new()
        .model("ZeroAxisTerms")
        .compile_str(AXIS, "zero_product_terms.mo")
        .unwrap()
        .dae;
    let lowered =
        rumoca_phase_solve::lower_solve_model(&dae, &std::collections::HashMap::new(), |_| {})
            .unwrap();
    let model = lowered.model();
    let (continuous, _) =
        rumoca_eval_solve::derive_solve_structural_artifacts(&model.problem, &model.artifacts)
            .unwrap();
    let owners = &model.problem.continuous.refresh_owners;
    let plan = owners.derivative();
    let schedule = plan.stage_schedule(&continuous);
    assert_eq!(
        schedule,
        RefreshStageSchedule::Certified,
        "no later block's coordinate is read by an earlier row"
    );
    let Ok(steps) = owners.staged_refresh_steps(plan, schedule) else {
        panic!("the certified derivative stages have a straight-line form");
    };
    assert!(
        !steps
            .iter()
            .any(|step| matches!(step, StagedRefreshStep::ProjectComplete)),
        "the complete simultaneous plan never runs"
    );
    let result = simulate_dae_with_diagnostics(
        &dae,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .unwrap();
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        &result.data[index]
    };
    // der(x) = fb[3] = 3 * fb[1] = -3 * x.
    for (row, &time) in result.times.iter().enumerate() {
        let x = (-3.0 * time).exp();
        for (name, expected) in [("x", x), ("fb[1]", -x), ("fb[3]", -3.0 * x)] {
            let actual = column(name)[row];
            assert!(
                (actual - expected).abs() < 1e-4,
                "{name} at {time}: {actual} != {expected}"
            );
        }
    }
}
