//! Public observations must settle coordinates, not only equation residuals.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, SimulationSession, lower_dae_for_simulation};
use rumoca_solver::SimExecutionPolicy;

const SOURCE: &str = r#"
model CoupledObservation
  parameter Real epsilon = 1e-6;
  output Real x(start=1);
  output Real y(start=1);
equation
  x*x + y*y = 2;
  x*x + (1 + epsilon)*y*y = 2 + epsilon + epsilon*1e-4*time;
end CoupledObservation;
"#;

fn check_observations(solver_mode: SimSolverMode, execution_policy: SimExecutionPolicy) {
    let compiled = Compiler::new()
        .model("CoupledObservation")
        .compile_str(SOURCE, "coupled_observation.mo")
        .unwrap();
    let options = SimOptions {
        solver_mode,
        execution_policy,
        t_end: 1.0,
        rtol: 1e-8,
        atol: 1e-8,
        ..SimOptions::default()
    };
    let lowered = lower_dae_for_simulation(&compiled.dae, &options).unwrap();
    assert!(
        lowered
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .iter()
            .any(|block| block.y_indices.len() == 2),
        "the regression must exercise a coupled algebraic solve"
    );
    let mut session = SimulationSession::new(&compiled.dae, options).unwrap();
    for time in [0.0_f64, 0.1, 0.5, 1.0] {
        session.advance_to(time).unwrap();
        let observed = session.state().unwrap();
        assert_eq!(observed.time, time);
        // Subtracting the two equations gives y² = 1 + 1e-4*time;
        // the positive starts select the positive continuous solution branch.
        for (name, expected) in [
            ("x", (1.0 - 1e-4 * time).sqrt()),
            ("y", (1.0 + 1e-4 * time).sqrt()),
        ] {
            let value = observed.values[name];
            assert!(
                (value - expected).abs() < 2e-8,
                "{solver_mode:?}/{execution_policy:?} {name} at {time}: {value}, expected {expected}"
            );
            assert_eq!(session.get(name).unwrap(), Some(value));
        }
    }
}

#[test]
fn bdf_native_observations_settle_coupled_coordinates() {
    check_observations(SimSolverMode::Bdf, SimExecutionPolicy::Auto);
}

#[test]
fn bdf_interpreted_observations_settle_coupled_coordinates() {
    check_observations(SimSolverMode::Bdf, SimExecutionPolicy::Interpreter);
}

#[test]
fn rk_native_observations_settle_coupled_coordinates() {
    check_observations(SimSolverMode::RkLike, SimExecutionPolicy::Auto);
}

#[test]
fn rk_interpreted_observations_settle_coupled_coordinates() {
    check_observations(SimSolverMode::RkLike, SimExecutionPolicy::Interpreter);
}
