//! MLS §12.7.1: a supplied derivative with a `noDerivative` input is the
//! derivative the author intends, and formal prolongation must keep it
//! (SPEC_0040 STRUCT-T07).
//!
//! `Rotational.Sources.Move.position` takes `q_qd_qdd` as `noDerivative`: its
//! first derivative is `u[2]` and its second `u[3]`, not the derivatives of
//! `u[1]`. Driving two rigidly coupled inertias with the deliberately
//! inconsistent `u = {sin(t), 2*cos(t), 0}` shows which derivative was used:
//! the annotation gives `i1.w = 2*cos(t)` and `i1.a = 0`, while
//! differentiating the body would give `cos(t)` and `-sin(t)`.

use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

use super::msl_sim_regression::require_msl_compiler;

const SOURCE: &str = "model MoveRigidPair
  Modelica.Mechanics.Rotational.Sources.Move move;
  Modelica.Mechanics.Rotational.Components.Inertia i1(J = 1);
  Modelica.Mechanics.Rotational.Components.Inertia i2(J = 2);
  Modelica.Blocks.Sources.RealExpression u[3](y = {sin(time), 2*cos(time), 0});
equation
  connect(u.y, move.u);
  connect(move.flange, i1.flange_a);
  connect(i1.flange_b, i2.flange_a);
end MoveRigidPair;
";

#[test]
fn move_keeps_its_supplied_rate_and_acceleration() {
    let compiled = require_msl_compiler()
        .model("MoveRigidPair")
        .compile_str(SOURCE, "MoveRigidPair.mo")
        .unwrap_or_else(|error| panic!("{error}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 2.0,
            dt: Some(0.05),
            solver_mode: SimSolverMode::Bdf,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("{error}"));
    let series = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("simulation result missing {name}"));
        &result.data[index]
    };
    let (rate, acceleration) = (series("i1.w"), series("i1.a"));
    for (index, &t) in result.times.iter().enumerate() {
        assert!(
            (rate[index] - 2.0 * t.cos()).abs() < 1e-9,
            "i1.w({t}) = {} is not the supplied u[2]",
            rate[index]
        );
        assert!(
            acceleration[index].abs() < 1e-9,
            "i1.a({t}) = {} is not the supplied u[3]",
            acceleration[index]
        );
    }
}
