//! MLS §8.6: an initial output equation determines the state behind a gain.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model InitialOutputPrecision
  Real controllerState(start=100.000000001, fixed=false);
  Real outputSignal;
  Real response(start=0, fixed=true);
initial equation
  outputSignal = 0;
equation
  outputSignal = (100.0 / 3.0) * (controllerState - 100.0);
  der(controllerState) = 1;
  der(response) = outputSignal / 0.0003;
end InitialOutputPrecision;
"#;

#[test]
fn initial_output_equation_is_settled_before_me_derivatives() {
    let dae = Compiler::new()
        .model("InitialOutputPrecision")
        .compile_str(SOURCE, "initial_output_precision.mo")
        .expect("source model compiles")
        .dae;
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        let result = simulate_dae_with_diagnostics(
            &dae,
            &SimOptions {
                t_end: 0.001,
                dt: Some(0.001),
                rtol: 1.0e-6,
                atol: 1.0e-6,
                solver_mode,
                ..SimOptions::default()
            },
        )
        .expect("source-owned initial output permits simulation");
        let output = result
            .names
            .iter()
            .position(|name| name == "outputSignal")
            .expect("output is published");
        assert_eq!(result.times[0], 0.0);
        assert_eq!(result.data[output][0], 0.0, "{solver_mode:?}");
        assert!(result.data[output].last().unwrap() > &0.03);
    }
}
