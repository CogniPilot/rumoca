use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
function forwardVelocity
  input Real unused[3,3];
  input Real angular[3];
  output Real result[3];
algorithm
  result := angular;
  annotation(Inline=true);
end forwardVelocity;
model ForwardedConstraint
  Real x[3](start={1,2,3}, each fixed=true);
  Real y[3];
  Real a[3];
  Real b[3];
equation
  x = forwardVelocity(identity(3), a);
  a = b;
  y = forwardVelocity(identity(3), b);
  der(x) + der(y) = -2*x;
end ForwardedConstraint;
"#;

#[test]
fn forwarding_calls_preserve_redundant_state_constraints_and_their_traces() {
    let compiled = Compiler::new()
        .model("ForwardedConstraint")
        .compile_str(SOURCE, "forwarded_constraint.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
        for (name, index) in ["x", "y", "a", "b"]
            .into_iter()
            .flat_map(|name| (1..=3).map(move |index| (name, index)))
        {
            let channel = format!("{name}[{index}]");
            let column = result
                .names
                .iter()
                .position(|name| name == &channel)
                .unwrap();
            for (&time, &actual) in result.times.iter().zip(&result.data[column]) {
                let expected = index as f64 * (-time).exp();
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?} {channel}({time}): {actual} != {expected}"
                );
            }
        }
    }
}
