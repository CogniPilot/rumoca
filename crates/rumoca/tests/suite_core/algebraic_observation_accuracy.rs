use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_dae_for_simulation, simulate_dae_with_diagnostics};
use rumoca_solver::{SimExecutionPolicy, SolveRuntime};

const SOURCE: &str = r#"
model AmplifiedObservation
  Real state(start=0, fixed=true);
  Real current;
  Real voltage;
equation
  der(state) = 0;
  voltage = 1e6*current;
  current = 0.5e-6*voltage + (time-0.25)*1e-11;
end AmplifiedObservation;
"#;

#[test]
fn algebraic_observation_refresh_bounds_recovered_coordinate_error() {
    let compiled = Compiler::new()
        .model("AmplifiedObservation")
        .compile_str(SOURCE, "amplified_observation.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let runtime = SolveRuntime::new(&model).unwrap();
    let index = model
        .problem
        .solve_layout
        .solver_maps
        .names
        .iter()
        .position(|name| name == "voltage")
        .unwrap();
    for time in [0.0, 0.5] {
        let mut y = model.initial_y.clone();
        runtime
            .refresh_algebraic_and_output_slots_certified(
                time,
                &mut y,
                &model.parameters,
                1e-10,
                64,
            )
            .unwrap();
        let expected = 2e-5 * (time - 0.25);
        assert!(
            (y[index] - expected).abs() < 1e-10,
            "voltage at {time}: {} instead of {expected}",
            y[index]
        );
    }
}

#[test]
fn algebraic_observation_trace_retains_small_residual_large_response() {
    let compiled = Compiler::new()
        .model("AmplifiedObservation")
        .compile_str(SOURCE, "amplified_observation.mo")
        .unwrap();
    for execution_policy in [SimExecutionPolicy::Auto, SimExecutionPolicy::Interpreter] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.5,
                dt: Some(0.05),
                rtol: 1e-10,
                atol: 1e-10,
                execution_policy,
                ..Default::default()
            },
        )
        .unwrap();
        let voltage = result
            .names
            .iter()
            .position(|name| name == "voltage")
            .unwrap();
        for (time, value) in result.times.iter().zip(&result.data[voltage]) {
            let expected = 2e-5 * (time - 0.25);
            assert!(
                (value - expected).abs() < 1e-10,
                "{execution_policy:?}: voltage at {time} is {value}, expected {expected}"
            );
        }
    }
}
