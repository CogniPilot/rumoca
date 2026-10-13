//! The scheduled executor drives every solver session through one trait;
//! each implementation must forward to the session it wraps.

use super::*;
use crate::SimulationSessionApi;

const INTEGRATOR_MODEL: &str = r#"
model SessionApiIntegrator
  input Real u = 1;
  output Real x(start = 0, fixed = true);
equation
  der(x) = u;
end SessionApiIntegrator;
"#;

fn options(solver_mode: SimSolverMode) -> SimOptions {
    SimOptions {
        solver_mode,
        ..SimOptions::default()
    }
}

fn artifact(
    solver_mode: SimSolverMode,
) -> (
    rumoca_solver::fmi_me::MeModelArtifact,
    Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    SimOptions,
) {
    let model = compile(INTEGRATOR_MODEL, "SessionApiIntegrator");
    let opts = options(solver_mode);
    let (artifact, backend) =
        crate::solve_lowering::lower_runtime_fmi_artifact(&model, &opts).unwrap();
    (artifact, backend, opts)
}

/// Integrate `der(x) = u` through the trait alone and return the visible `x`.
fn drive<S: SimulationSessionApi>(session: &mut S) -> f64 {
    session.set_inputs(&[("u", 2.0)]).unwrap();
    session.ensure_end_time(1.0);
    session.advance_to(0.5).unwrap();
    assert!((session.time() - 0.5).abs() < 1e-9);
    let advanced = session.get("x").unwrap().unwrap();
    assert!((advanced - 1.0).abs() < 1e-3, "x(0.5) = {advanced}");
    session.reset(0.0).unwrap();
    assert_eq!(session.time(), 0.0);
    assert!(session.get("missing").unwrap().is_none());
    advanced
}

#[test]
fn wrapper_session_forwards_the_trait_to_each_solver() {
    let model = compile(INTEGRATOR_MODEL, "SessionApiIntegrator");
    for (mode, schedule_cap) in [(SimSolverMode::RkLike, None), (SimSolverMode::Bdf, None)] {
        let mut session = SimulationSession::new(&model, options(mode)).unwrap();
        drive(&mut session);
        let values = SimulationSessionApi::values_for(&session, &["x".to_string(), "no".into()])
            .unwrap()
            .expect("both solvers expose a direct value read");
        assert_eq!(values.keys().collect::<Vec<_>>(), ["x"], "{mode:?}");
        assert_eq!(
            SimulationSessionApi::max_schedule_advance_dt(&session),
            schedule_cap,
            "{mode:?}"
        );
    }
}

#[test]
fn rk45_session_uses_the_trait_defaults_for_batched_reads() {
    let (artifact, backend, opts) = artifact(SimSolverMode::RkLike);
    let mut session =
        crate::rk45::SimulationSession::from_artifact(artifact, opts, backend).unwrap();
    drive(&mut session);
    assert!(
        SimulationSessionApi::values_for(&session, &["x".to_string()])
            .unwrap()
            .is_none()
    );
    assert_eq!(
        SimulationSessionApi::max_schedule_advance_dt(&session),
        Some(0.002)
    );
}

#[test]
fn diffsol_session_forwards_the_trait_and_leaves_the_schedule_uncapped() {
    let (artifact, backend, opts) = artifact(SimSolverMode::Bdf);
    let mut session =
        crate::diffsol::SimulationSession::from_artifact(artifact, opts, backend).unwrap();
    drive(&mut session);
    assert_eq!(
        SimulationSessionApi::max_schedule_advance_dt(&session),
        None
    );
}
