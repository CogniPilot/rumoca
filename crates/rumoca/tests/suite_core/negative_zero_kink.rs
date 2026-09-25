//! Reverse and forward derivatives of `abs` agree at a negative zero.
//!
//! `NegatedFluxKink.Loop` holds a nonlinear flux loop at exactly zero flux until
//! `t = 0.5`. `Phi1 + Phi2 = 0` makes `Phi2` read as `-Phi1`, which is `-0.0`,
//! and `BN2 = abs(Phi2 / 2)` sits on the kink. The state-sensitivity projection
//! assembles the loop matrix from reverse rows and certifies it with forward
//! Jacobian-vector products, so both must take the same one-sided derivative:
//! the forward rule's right derivative at `x >= 0`, including `-0.0`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = include_str!("../fixtures/sensitivity/NegatedFluxKink.mo");

#[test]
fn a_stiff_loop_resting_on_an_abs_kink_at_negative_zero_simulates() {
    let compiled = Compiler::new()
        .model("NegatedFluxKink.Loop")
        .compile_str(SOURCE, "NegatedFluxKink.mo")
        .unwrap();
    let mut finals = Vec::new();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                t_end: 1.0,
                dt: Some(0.01),
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{solver_mode:?} must simulate: {error}"));
        let column = |name: &str| {
            let index = result.names.iter().position(|n| n == name).unwrap();
            &result.data[index]
        };
        let (phi1, phi2, i) = (column("Phi1"), column("Phi2"), column("i"));
        for row in 0..result.times.len() {
            assert_eq!(phi1[row] + phi2[row], 0.0, "the flux balance holds exactly");
        }
        finals.push((*i.last().unwrap(), *phi1.last().unwrap()));
    }
    let (bdf, rk) = (finals[0], finals[1]);
    assert!(bdf.0 > 0.1, "the drive has charged the loop: {bdf:?}");
    assert!(
        (bdf.0 - rk.0).abs() < 1e-3 && (bdf.1 - rk.1).abs() < 1e-3 * rk.1.abs().max(1.0),
        "BDF and RK agree: {bdf:?} vs {rk:?}"
    );
}
