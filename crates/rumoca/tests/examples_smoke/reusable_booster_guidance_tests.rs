use super::{compile_toml_model_if_source_roots_exist, example_root};
use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimPacingMode, SimSolverMode, SimulationSession};
use std::fs;

fn reusable_booster_session() -> Option<SimulationSession> {
    reusable_booster_session_with_auto_launch(None)
}

fn reusable_booster_session_with_auto_launch(
    auto_launch_time: Option<f64>,
) -> Option<SimulationSession> {
    let config_path = example_root().join("interactive/reusable_booster/rumoca-scenario.toml");
    let result = compile_toml_model_if_source_roots_exist(&config_path)?;
    let mut session = SimulationSession::new_with_diagnostics(
        &result.dae,
        SimOptions {
            rtol: 1.0e-6,
            atol: 1.0e-6,
            t_end: 75.0,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            pacing_mode: SimPacingMode::AsFastAsPossible,
            param_overrides: auto_launch_time
                .map(|value| vec![("auto_launch_time".to_string(), value)])
                .unwrap_or_default(),
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
            ("position_gain_scale_input", 1.0),
            ("velocity_gain_scale_input", 1.0),
            ("attitude_gain_scale_input", 1.0),
            ("rate_gain_scale_input", 1.0),
        ])
        .expect("ReusableBoosterLanding live inputs should bind to the session");
    Some(session)
}

#[test]
fn autonomous_nominal_mission_reaches_supported_touchdown() {
    let Some(mut session) = reusable_booster_session_with_auto_launch(Some(0.1)) else {
        eprintln!("skipping ReusableBoosterLanding mission regression: cached CMM unavailable");
        return;
    };
    session
        .set_inputs(&[
            ("wind_speed_input", 0.0),
            ("wind_direction_variation_input", 0.0),
            ("dryden_intensity_input", 0.0),
            ("wave_heave_amplitude_input", 0.0),
            ("wave_roll_amplitude_input", 0.0),
            ("wave_pitch_amplitude_input", 0.0),
        ])
        .expect("nominal mission should disable external disturbances");
    session
        .advance_to(0.1)
        .expect("ReusableBoosterLanding should reach the automatic launch event");
    session
        .advance_to(0.11)
        .expect("ReusableBoosterLanding should settle the automatic launch event");
    session
        .advance_to(30.0)
        .expect("ReusableBoosterLanding autonomous mission should reach settled touchdown");
    let state = session.state().expect("mission state should be readable");

    assert_eq!(
        state_value(&state.values, "mission_phase"),
        4.0,
        "mission did not land: t={}, elapsed={}, altitude={}, impact_speed={}, impact_tilt={}, legs={}, margin={}, normal_speed={}, tangential_speed={}, feasible={}, launch_time={:?}",
        state.time,
        state_value(&state.values, "mission_elapsed"),
        state_value(&state.values, "altitude"),
        state_value(&state.values, "impact_speed"),
        state_value(&state.values, "impact_tilt_deg"),
        state_value(&state.values, "supporting_legs"),
        state_value(&state.values, "support_margin"),
        state_value(&state.values, "touchdown_normal_speed"),
        state_value(&state.values, "touchdown_tangential_speed"),
        state_value(&state.values, "reference_feasible"),
        state.values.get("launch_time")
    );
    assert_eq!(state_value(&state.values, "crash_indicator"), 0.0);
    assert!(state.values.values().all(|value| value.is_finite()));
    assert!(state_value(&state.values, "supporting_legs") >= 3.0);
    assert!(state_value(&state.values, "support_margin") >= 0.25);
    assert!(state_value(&state.values, "touchdown_normal_speed") <= 2.0);
    assert!(state_value(&state.values, "touchdown_tangential_speed") <= 1.5);
    assert!(state_value(&state.values, "impact_tilt_deg") <= 10.0);
    let quaternion_norm = (1..=4)
        .map(|index| state_value(&state.values, &format!("quat[{index}]")))
        .map(|component| component * component)
        .sum::<f64>()
        .sqrt();
    assert!((quaternion_norm - 1.0).abs() < 1.0e-3);
    assert!(state_value(&state.values, "vehicle_mass") > 0.0);
}

fn state_value(values: &indexmap::IndexMap<String, f64>, name: &str) -> f64 {
    *values
        .get(name)
        .unwrap_or_else(|| panic!("ReusableBoosterLanding should expose {name}"))
}

fn compile_probe(probe: &str, model: &str) -> Option<rumoca::CompilationResult> {
    let cmm_root = super::cached_cmm_root()?;
    let source = fs::read_to_string(
        example_root().join("interactive/reusable_booster/ReusableBoosterLanding.mo"),
    )
    .expect("ReusableBoosterLanding source should be readable");
    let directory = tempfile::tempdir().expect("probe directory should be created");
    let path = directory.path().join("ReusableBoosterGuidanceProbe.mo");
    fs::write(&path, format!("{source}\n{probe}"))
        .expect("ReusableBooster guidance probe should be written");
    Some(
        Compiler::new()
            .source_root(cmm_root.to_string_lossy().as_ref())
            .model(model)
            .compile_file(path.to_string_lossy().as_ref())
            .expect("ReusableBooster guidance probe should compile"),
    )
}

fn probe_state(probe: &str, model: &str) -> Option<indexmap::IndexMap<String, f64>> {
    let result = compile_probe(probe, model)?;
    let mut session = SimulationSession::new_with_diagnostics(
        &result.dae,
        SimOptions {
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            pacing_mode: SimPacingMode::AsFastAsPossible,
            ..Default::default()
        },
    )
    .expect("ReusableBooster guidance probe should create a simulation session");
    session
        .advance_to(0.01)
        .expect("ReusableBooster guidance probe should evaluate");
    Some(
        session
            .state()
            .expect("ReusableBooster guidance probe state should be readable")
            .values,
    )
}

#[test]
fn moving_deck_plan_exposes_nonstationary_terminal_state() {
    let Some(mut session) = reusable_booster_session() else {
        eprintln!(
            "skipping ReusableBoosterLanding deck-plan regression: run \
             `cargo xtask repo modelica-deps ensure`"
        );
        return;
    };
    session
        .advance_to(0.01)
        .expect("ReusableBoosterLanding deck plan should evaluate");
    let state = session.state().expect("deck-plan state should be readable");

    let target_velocity = [
        state_value(&state.values, "plan_target_velocity[1]"),
        state_value(&state.values, "plan_target_velocity[2]"),
        state_value(&state.values, "plan_target_velocity[3]"),
    ];
    let target_acceleration = [
        state_value(&state.values, "plan_target_acceleration[1]"),
        state_value(&state.values, "plan_target_acceleration[2]"),
        state_value(&state.values, "plan_target_acceleration[3]"),
    ];
    let normal_offset = [
        state_value(&state.values, "plan_target_normal_offset[1]"),
        state_value(&state.values, "plan_target_normal_offset[2]"),
    ];

    assert!(
        target_velocity.iter().any(|value| value.abs() > 1.0e-4),
        "moving deck target must retain terminal velocity: {target_velocity:?}"
    );
    assert!(
        target_acceleration.iter().any(|value| value.abs() > 1.0e-4),
        "moving deck target must retain terminal acceleration: {target_acceleration:?}"
    );
    assert!(
        normal_offset.iter().any(|value| value.abs() > 1.0e-3),
        "tilted deck target must offset the CG along the terminal deck normal: \
         {normal_offset:?}"
    );
}

#[test]
fn quintic_reference_matches_position_velocity_and_acceleration_boundaries() {
    let probe = r#"
model QuinticBoundaryProbe
  output Real initial_state[9];
  output Real terminal_state[9];
equation
  initial_state = quinticFlatnessReference(
    {0.0, 4.0},
    {1, 2, 3, 4, 5, 6, 0.1, 0.2, 0.3},
    {7, 8, 9, -1, -2, -3, -0.1, -0.2, -0.3});
  terminal_state = quinticFlatnessReference(
    {4.0, 4.0},
    {1, 2, 3, 4, 5, 6, 0.1, 0.2, 0.3},
    {7, 8, 9, -1, -2, -3, -0.1, -0.2, -0.3});
end QuinticBoundaryProbe;
"#;
    let Some(values) = probe_state(probe, "QuinticBoundaryProbe") else {
        eprintln!("skipping ReusableBoosterLanding quintic regression: cached CMM unavailable");
        return;
    };
    let expected_initial = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 0.1, 0.2, 0.3];
    let expected_terminal = [7.0, 8.0, 9.0, -1.0, -2.0, -3.0, -0.1, -0.2, -0.3];
    for index in 1..=9 {
        let initial = state_value(&values, &format!("initial_state[{index}]"));
        let terminal = state_value(&values, &format!("terminal_state[{index}]"));
        assert!((initial - expected_initial[index - 1]).abs() < 1.0e-10);
        assert!((terminal - expected_terminal[index - 1]).abs() < 1.0e-10);
    }
}

#[test]
fn touchdown_acceptance_rejects_tilt_slide_and_incomplete_support() {
    let probe = r#"
model TouchdownAcceptanceProbe
  output Real nominal;
  output Real tipped;
  output Real sliding;
  output Real supported_triangle;
  output Real marginal_triangle;
equation
  nominal = touchdownAcceptance(4.0, 1.0, 0.5, 0.5, 5.0);
  tipped = touchdownAcceptance(4.0, 1.0, 0.5, 0.5, 11.0);
  sliding = touchdownAcceptance(4.0, 1.0, 0.5, 2.0, 5.0);
  supported_triangle = touchdownAcceptance(3.0, 0.5, 0.5, 0.5, 5.0);
  marginal_triangle = touchdownAcceptance(3.0, 0.1, 0.5, 0.5, 5.0);
end TouchdownAcceptanceProbe;
"#;
    let Some(values) = probe_state(probe, "TouchdownAcceptanceProbe") else {
        eprintln!("skipping ReusableBoosterLanding touchdown regression: cached CMM unavailable");
        return;
    };
    assert_eq!(state_value(&values, "nominal"), 1.0);
    assert_eq!(state_value(&values, "tipped"), 0.0);
    assert_eq!(state_value(&values, "sliding"), 0.0);
    assert_eq!(state_value(&values, "supported_triangle"), 1.0);
    assert_eq!(state_value(&values, "marginal_triangle"), 0.0);
}

#[test]
fn touchdown_speed_uses_light_foot_contact_before_surface_fallback() {
    let probe = r#"
model LoadedFootMaximumProbe
  output Real loaded_maximum;
  output Real light_contact_maximum;
  output Real light_contact_safe;
  output Real no_load_fallback;
equation
  loaded_maximum = loadedFootMaximum(
    {0.5, 0.7, 0.6, 100.0}, {1, 1, 1, 0}, {1, 1, 1, 1}, 4.0);
  light_contact_maximum = loadedFootMaximum(
    {0.5, 0.7, 0.6, 100.0}, {0, 0, 0, 0}, {0, 0, 0, 1}, 4.0);
  light_contact_safe = touchdownKinematicsSafe(100.0, 0.5, 5.0);
  no_load_fallback = loadedFootMaximum(
    {0.5, 0.7, 0.6, 100.0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 4.0);
end LoadedFootMaximumProbe;
"#;
    let Some(values) = probe_state(probe, "LoadedFootMaximumProbe") else {
        eprintln!("skipping ReusableBoosterLanding loaded-foot regression: cached CMM unavailable");
        return;
    };

    let mixed_contact_maximum = state_value(&values, "loaded_maximum");
    assert!((mixed_contact_maximum - 100.0).abs() < 1.0e-12);
    assert!(mixed_contact_maximum > 2.0);
    assert!((state_value(&values, "light_contact_maximum") - 100.0).abs() < 1.0e-12);
    assert_eq!(state_value(&values, "light_contact_safe"), 0.0);
    assert!((state_value(&values, "no_load_fallback") - 4.0).abs() < 1.0e-12);
}

#[test]
fn tangential_contact_force_is_zero_unloaded_and_coulomb_limited() {
    let probe = r#"
model TangentialContactForceProbe
  output Real unloaded[3];
  output Real loaded[3];
equation
  unloaded = tangentialContactForce(0.0, 1.0, {3, 4, 0}, 40000.0, 0.8);
  loaded = tangentialContactForce(1000.0, 1.0, {3, 4, 0}, 40000.0, 0.8);
end TangentialContactForceProbe;
"#;
    let Some(values) = probe_state(probe, "TangentialContactForceProbe") else {
        eprintln!(
            "skipping ReusableBoosterLanding contact-force regression: cached CMM unavailable"
        );
        return;
    };
    let unloaded_norm = (1..=3)
        .map(|index| state_value(&values, &format!("unloaded[{index}]")))
        .map(|component| component * component)
        .sum::<f64>()
        .sqrt();
    let loaded_norm = (1..=3)
        .map(|index| state_value(&values, &format!("loaded[{index}]")))
        .map(|component| component * component)
        .sum::<f64>()
        .sqrt();

    assert!(unloaded_norm < 1.0e-12);
    assert!((loaded_norm - 800.0).abs() < 1.0e-9);
}

#[test]
fn gps_error_state_correction_does_not_rotate_absolute_position() {
    let probe = r#"
function estimatorRetractionSeed
  output Real packet[91];
algorithm
  packet := initialSe23EstimatorPacket({100, 0, 0}, {0, 0, 0});
  packet[28] := 0.5;
  packet[84] := 0.5;
end estimatorRetractionSeed;

model EstimatorRetractionProbe
  output Real posterior[91];
equation
  posterior = sampledSe23GpsImuFilter(
    estimatorRetractionSeed(),
    {0.0, 1.0, 100, 1, 0, 0, 0, 0, 0, 0, 0, 0.1});
end EstimatorRetractionProbe;
"#;
    let Some(values) = probe_state(probe, "EstimatorRetractionProbe") else {
        eprintln!(
            "skipping ReusableBoosterLanding estimator retraction regression: cached CMM unavailable"
        );
        return;
    };
    let posterior_y = state_value(&values, "posterior[2]");
    assert!(
        (posterior_y - 1.0).abs() < 1.0,
        "a local attitude correction must not rotate the absolute position: posterior_y={posterior_y}"
    );
}

#[test]
fn innovation_inverse_remains_finite_for_nearly_singular_covariance() {
    let probe = r#"
model InnovationInverseProbe
  output Real inverse[3, 3];
equation
  inverse = symmetricPositiveDefiniteInverse3([
    1.0e-14, 0, 0;
    0, 1.0, 0.1;
    0, 0.1, 1.0]);
end InnovationInverseProbe;
"#;
    let Some(values) = probe_state(probe, "InnovationInverseProbe") else {
        eprintln!("skipping ReusableBoosterLanding innovation regression: cached CMM unavailable");
        return;
    };
    let mut entries = Vec::with_capacity(9);
    for row in 1..=3 {
        for col in 1..=3 {
            entries.push(state_value(&values, &format!("inverse[{row},{col}]")));
        }
    }

    assert!(entries.iter().all(|entry| entry.is_finite()));
    assert!(entries[0] > 0.0);
    assert!((entries[1] - entries[3]).abs() < 1.0e-9);
    assert!((entries[5] - entries[7]).abs() < 1.0e-9);
}

#[test]
fn unloaded_feet_do_not_form_a_valid_touchdown_support() {
    let Some(mut session) = reusable_booster_session() else {
        eprintln!(
            "skipping ReusableBoosterLanding touchdown regression: run \
             `cargo xtask repo modelica-deps ensure`"
        );
        return;
    };
    session
        .advance_to(0.01)
        .expect("ReusableBoosterLanding touchdown state should evaluate");
    let state = session
        .state()
        .expect("touchdown-support state should be readable");

    let supporting_legs = state_value(&state.values, "supporting_legs");
    let support_margin = state_value(&state.values, "support_margin");
    let touchdown_safe = state_value(&state.values, "touchdown_safe");

    assert_eq!(
        supporting_legs, 0.0,
        "unloaded feet must not count as support"
    );
    assert!(
        support_margin <= 0.0,
        "unloaded support margin={support_margin}"
    );
    assert_eq!(
        touchdown_safe, 0.0,
        "proximity without loaded-foot support must not be a safe touchdown"
    );
}

#[test]
fn launch_command_does_not_immediately_latch_an_impact() {
    let Some(mut session) = reusable_booster_session() else {
        eprintln!(
            "skipping ReusableBoosterLanding launch-contact regression: run \
             `cargo xtask repo modelica-deps ensure`"
        );
        return;
    };
    session
        .set_inputs(&[("launch_command", 1.0)])
        .expect("ReusableBoosterLanding launch command should bind");
    session
        .advance_to(0.01)
        .expect("ReusableBoosterLanding should accept the launch command");
    let state = session.state().expect("launch state should be readable");

    assert_eq!(state_value(&state.values, "crash_indicator"), 0.0);
}

#[test]
fn mission_elapsed_times_are_zero_before_their_events() {
    let Some(mut session) = reusable_booster_session() else {
        eprintln!(
            "skipping ReusableBoosterLanding elapsed-time regression: run \
             `cargo xtask repo modelica-deps ensure`"
        );
        return;
    };
    session
        .advance_to(0.01)
        .expect("ReusableBoosterLanding waiting phase should evaluate");
    let state = session.state().expect("waiting state should be readable");

    assert_eq!(state_value(&state.values, "mission_elapsed"), 0.0);
    assert_eq!(state_value(&state.values, "landing_elapsed"), 0.0);
}

#[test]
fn rcs_schedule_and_propellant_model_are_bounded() {
    let probe = r#"
model RcsActuatorProbe
  output Real unavailable;
  output Real transitioning;
  output Real available;
  output Real idle_flow;
  output Real active_flow;
equation
  unavailable = scheduledRcsAvailability(79.0, 1.0);
  transitioning = scheduledRcsAvailability(100.0, 1.0);
  available = scheduledRcsAvailability(121.0, 1.0);
  idle_flow = rcsPropellantMassFlow({0, 0, 0}, 0.0);
  active_flow = rcsPropellantMassFlow({6000, -3000, 0}, 18000.0);
end RcsActuatorProbe;
"#;
    let Some(values) = probe_state(probe, "RcsActuatorProbe") else {
        eprintln!("skipping ReusableBoosterLanding RCS regression: cached CMM unavailable");
        return;
    };
    assert_eq!(state_value(&values, "unavailable"), 0.0);
    assert!((state_value(&values, "transitioning") - 0.5).abs() < 1.0e-12);
    assert_eq!(state_value(&values, "available"), 1.0);
    assert_eq!(state_value(&values, "idle_flow"), 0.0);
    assert!(state_value(&values, "active_flow") > 0.0);
}

#[test]
fn quaternion_reference_rate_matches_relative_rotation() {
    let probe = r#"
model ReferenceRateProbe
  output Real rate[3];
equation
  rate = quaternionReferenceRate(
    {1, 0, 0, 0},
    {0.7071067811865476, 0, 0, 0.7071067811865475},
    1.0);
end ReferenceRateProbe;
"#;
    let Some(values) = probe_state(probe, "ReferenceRateProbe") else {
        eprintln!(
            "skipping ReusableBoosterLanding reference-rate regression: cached CMM unavailable"
        );
        return;
    };
    assert!(state_value(&values, "rate[1]").abs() < 1.0e-12);
    assert!(state_value(&values, "rate[2]").abs() < 1.0e-12);
    assert!((state_value(&values, "rate[3]") - std::f64::consts::FRAC_PI_2).abs() < 1.0e-10);
}

#[test]
fn translational_feedback_uses_per_axis_vector_gains() {
    let probe = r#"
model TranslationalGainProbe
  output Real correction[3];
equation
  correction = se23TranslationalCorrection(
    {10, 10, 10, 0, 0, 0, 0, 0, 0},
    {1, 1, 1},
    {1, 1, 1});
end TranslationalGainProbe;
"#;
    let Some(values) = probe_state(probe, "TranslationalGainProbe") else {
        eprintln!("skipping ReusableBoosterLanding vector-gain regression: cached CMM unavailable");
        return;
    };
    let correction = [
        state_value(&values, "correction[1]"),
        state_value(&values, "correction[2]"),
        state_value(&values, "correction[3]"),
    ];
    assert!((correction[0] - 0.4).abs() < 1.0e-12, "{correction:?}");
    assert!((correction[1] - 0.4).abs() < 1.0e-12, "{correction:?}");
    assert!((correction[2] - 0.8).abs() < 1.0e-12, "{correction:?}");
}

#[test]
fn quaternion_reference_slew_limits_the_rotation_vector() {
    let probe = r#"
model ReferenceSlewProbe
  output Real limited[4];
  output Real step[3];
equation
  limited = slewLimitedQuaternionReference(
    {1, 0, 0, 0},
    {0.7071067811865476, 0, 0.7071067811865475, 0},
    0.05,
    0.10);
  step = LieGroups.SO3.Quat.log_map(limited);
end ReferenceSlewProbe;
"#;
    let Some(values) = probe_state(probe, "ReferenceSlewProbe") else {
        eprintln!(
            "skipping ReusableBoosterLanding reference-slew regression: cached CMM unavailable"
        );
        return;
    };
    let step = [
        state_value(&values, "step[1]"),
        state_value(&values, "step[2]"),
        state_value(&values, "step[3]"),
    ];
    let step_norm = step
        .iter()
        .map(|component| component * component)
        .sum::<f64>()
        .sqrt();
    assert!((step_norm - 0.005).abs() < 1.0e-10, "step={step:?}");
    assert!(step[0].abs() < 1.0e-12, "step={step:?}");
    assert!(step[2].abs() < 1.0e-12, "step={step:?}");
    assert!(step[1] > 0.0, "step={step:?}");
}

#[test]
fn desired_rate_is_expressed_in_the_actual_body_frame() {
    let probe = r#"
model ReferenceRateFrameProbe
  output Real actual_body_rate[3];
equation
  actual_body_rate = expressReferenceRateInBody(
    {1, 0, 0, 0},
    {0.7071067811865476, 0, 0, 0.7071067811865475},
    {1, 0, 0});
end ReferenceRateFrameProbe;
"#;
    let Some(values) = probe_state(probe, "ReferenceRateFrameProbe") else {
        eprintln!("skipping ReusableBoosterLanding rate-frame regression: cached CMM unavailable");
        return;
    };
    let rate = [
        state_value(&values, "actual_body_rate[1]"),
        state_value(&values, "actual_body_rate[2]"),
        state_value(&values, "actual_body_rate[3]"),
    ];
    assert!(rate[0].abs() < 1.0e-10, "rate={rate:?}");
    assert!((rate[1] - 1.0).abs() < 1.0e-10, "rate={rate:?}");
    assert!(rate[2].abs() < 1.0e-10, "rate={rate:?}");
}

#[test]
fn plan_feasibility_rejects_an_actuator_infeasible_reference() {
    let probe = r#"
model PlanFeasibilityProbe
  output Real feasible;
  output Real thrust_limited;
equation
  feasible = quinticPlanFeasible(
    {0, 0, 30, 0, 0, 0, 0, 0, 0},
    {0, 0, 30, 0, 0, 0, 0, 0, 0},
    {10.0, 1000.0, 1000.0, 20000.0, 20.0, 500.0});
  thrust_limited = quinticPlanFeasible(
    {0, 0, 30, 0, 0, 0, 0, 0, 0},
    {0, 0, 30, 0, 0, 0, 0, 0, 0},
    {10.0, 1000.0, 1000.0, 5000.0, 20.0, 500.0});
end PlanFeasibilityProbe;
"#;
    let Some(values) = probe_state(probe, "PlanFeasibilityProbe") else {
        eprintln!("skipping ReusableBoosterLanding feasibility regression: cached CMM unavailable");
        return;
    };
    assert_eq!(state_value(&values, "feasible"), 1.0);
    assert_eq!(state_value(&values, "thrust_limited"), 0.0);
}
