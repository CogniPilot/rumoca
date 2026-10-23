//! The adjoint of a trajectory objective through tensor operations: a damped
//! pendulum driven by a small tanh network whose weights are array parameters.
//! Its right-hand side lowers to tensor loads, a matrix-vector product, an
//! elementwise map and an output range, so the reverse sweep must carry every
//! one of them; forward sensitivity, which differentiates the same lowering
//! independently, is the reference.

use rumoca::Compiler;
use rumoca_sim::{ObjectiveGradient, RunningKind, RunningTerm, SimOptions, TrajectoryObjective};

const NEURAL_PENDULUM: &str = r#"
model NeuralPendulum
  parameter Real g = 9.81;
  parameter Real l = 1.0;
  parameter Real damping = 0.1;
  parameter Real W1[4, 2] = {{0.3*sin(0.7*i + 0.4*j) for j in 1:2} for i in 1:4};
  parameter Real b1[4] = {0.05*cos(0.3*i) for i in 1:4};
  parameter Real W2[4] = {0.2*cos(0.5*i) for i in 1:4};
  parameter Real b2 = 0.0;
  Real theta(start = 0.8, fixed = true);
  Real omega(start = 0.0, fixed = true);
  Real h[4];
  Real u;
  Real cost;
equation
  h = tanh(W1 * {theta, omega} + b1);
  u = W2 * h + b2;
  der(theta) = omega;
  der(omega) = -g / l * sin(theta) - damping * omega + u;
  cost = theta^2 + 0.1 * omega^2 + 0.01 * u^2;
end NeuralPendulum;
"#;

fn gradient(adjoint: bool) -> ObjectiveGradient {
    let compiled = Compiler::new()
        .model("NeuralPendulum")
        .compile_str(NEURAL_PENDULUM, "NeuralPendulum.mo")
        .unwrap_or_else(|error| panic!("NeuralPendulum should compile: {error:?}"));
    let options = SimOptions {
        t_end: 5.0,
        dt: Some(0.01),
        rtol: 1.0e-11,
        atol: 1.0e-12,
        ..SimOptions::default()
    };
    let objective = TrajectoryObjective {
        running: vec![RunningTerm {
            variable: "cost".to_string(),
            weight: 1.0,
            kind: RunningKind::Value,
        }],
        terminal: Vec::new(),
    };
    rumoca_sim::trajectory_objective_gradient_for_dae(
        &compiled.dae,
        &options,
        &[],
        &objective,
        adjoint,
    )
    .unwrap_or_else(|error| panic!("adjoint={adjoint}: {error}"))
}

#[test]
fn the_adjoint_through_a_tensor_network_matches_forward_sensitivity() {
    let forward = gradient(false);
    let adjoint = gradient(true);
    assert_eq!(forward.parameters, adjoint.parameters);
    for weight in ["W1[2,1]", "W1[4,2]", "b1[3]", "W2[1]", "b2"] {
        assert!(
            adjoint.parameters.iter().any(|name| name == weight),
            "the adjoint differentiates the array weight {weight}: {:?}",
            adjoint.parameters
        );
    }
    for (index, name) in forward.parameters.iter().enumerate() {
        let (want, got) = (forward.gradient[index], adjoint.gradient[index]);
        assert!(
            (got - want).abs() <= 1.0e-8 * (1.0 + want.abs()),
            "dJ/d{name}: adjoint {got}, forward {want}"
        );
    }
}
