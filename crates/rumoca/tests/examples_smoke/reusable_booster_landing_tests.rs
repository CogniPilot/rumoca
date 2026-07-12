use super::{compile_toml_model_if_source_roots_exist, example_root};
use rumoca_sim::{SimOptions, SimPacingMode, SimSolverMode, SimulationSession};

fn compile_reusable_booster() -> Option<rumoca::CompilationResult> {
    let config_path = example_root().join("interactive/reusable_booster/rumoca-scenario.toml");
    compile_toml_model_if_source_roots_exist(&config_path)
}

fn reusable_booster_session(
    dae: &rumoca_ir_dae::Dae,
    position_gain_scale: f64,
) -> SimulationSession {
    let mut session = SimulationSession::new_with_diagnostics(
        dae,
        SimOptions {
            rtol: 1.0e-6,
            atol: 1.0e-6,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            pacing_mode: SimPacingMode::AsFastAsPossible,
            ..Default::default()
        },
    )
    .expect("ReusableBoosterLanding should create an RK-like simulation session");
    session
        .set_inputs(&[
            ("launch_command", 0.0),
            ("wind_speed_input", 5.0),
            ("wind_direction_input", 35.0),
            ("wind_direction_variation_input", 15.0),
            ("dryden_intensity_input", 4.0),
            ("wave_heave_amplitude_input", 0.35),
            ("wave_roll_amplitude_input", 1.2),
            ("wave_pitch_amplitude_input", 0.8),
            ("position_gain_scale_input", position_gain_scale),
            ("velocity_gain_scale_input", 1.0),
            ("attitude_gain_scale_input", 1.0),
            ("rate_gain_scale_input", 1.0),
        ])
        .expect("ReusableBoosterLanding live inputs should bind to the session");
    session
}

#[test]
fn position_gain_changes_translational_acceleration_command() {
    let Some(result) = compile_reusable_booster() else {
        eprintln!(
            "skipping ReusableBoosterLanding gain regression: run \
             `cargo xtask repo modelica-deps ensure`"
        );
        return;
    };

    let mut zero_gain = reusable_booster_session(&result.dae, 0.0);
    let mut doubled_gain = reusable_booster_session(&result.dae, 2.0);
    zero_gain
        .advance_to(0.05)
        .expect("zero-position-gain session should advance");
    doubled_gain
        .advance_to(0.05)
        .expect("doubled-position-gain session should advance");

    let zero_state = zero_gain
        .state()
        .expect("zero-gain state should be readable");
    let doubled_state = doubled_gain
        .state()
        .expect("doubled-gain state should be readable");
    let zero_ax = zero_state.values["acceleration_command[1]"];
    let doubled_ax = doubled_state.values["acceleration_command[1]"];

    assert!(
        (doubled_ax - zero_ax).abs() > 5.0e-3,
        "position gain must materially change translational feedback; \
         zero_gain_ax={zero_ax}, doubled_gain_ax={doubled_ax}"
    );
}

#[test]
fn inertial_prediction_couples_attitude_uncertainty_into_position() {
    let Some(result) = compile_reusable_booster() else {
        eprintln!(
            "skipping ReusableBoosterLanding covariance regression: run \
             `cargo xtask repo modelica-deps ensure`"
        );
        return;
    };
    let mut session = reusable_booster_session(&result.dae, 1.0);
    session
        .advance_to(0.5)
        .expect("ReusableBoosterLanding estimator should advance");

    let state = session.state().expect("estimator state should be readable");
    let position_attitude_covariance = [
        state.values["estimated_position_attitude_covariance[1]"],
        state.values["estimated_position_attitude_covariance[2]"],
        state.values["estimated_position_attitude_covariance[3]"],
    ];
    assert!(
        position_attitude_covariance
            .iter()
            .any(|value| value.abs() > 1.0e-6),
        "specific-force propagation must couple attitude uncertainty into position; \
         covariance={position_attitude_covariance:?}"
    );
}

#[test]
fn gps_update_limits_coupled_attitude_uncertainty_growth() {
    let Some(result) = compile_reusable_booster() else {
        eprintln!(
            "skipping ReusableBoosterLanding covariance correction regression: run \
             `cargo xtask repo modelica-deps ensure`"
        );
        return;
    };
    let mut session = reusable_booster_session(&result.dae, 1.0);
    session
        .advance_to(0.15)
        .expect("estimator should advance before the GPS correction");
    let before = session
        .state()
        .expect("pre-correction state should be readable")
        .values["estimated_attitude_variance[1]"];
    session
        .advance_to(0.25)
        .expect("estimator should advance through the GPS correction");
    let after = session
        .state()
        .expect("post-correction state should be readable")
        .values["estimated_attitude_variance[1]"];

    // The gyro process model adds 4e-5 rad^2 between these samples. A GPS
    // correction cannot remove that fresh noise entirely, but the coupled
    // covariance should keep the net growth well below the open-loop value.
    assert!(
        after < before + 2.0e-5,
        "GPS correction should limit coupled attitude uncertainty growth: \
         before={before}, after={after}"
    );
}
