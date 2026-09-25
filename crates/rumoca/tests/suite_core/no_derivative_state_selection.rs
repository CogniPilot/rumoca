//! Constrained state selection through a supplied derivative with a
//! `noDerivative` record input (MLS §12.7.1, SPEC_0040 STRUCT-T07).
//!
//! The closure `rotate(R2, {1,0})[2] = rotate(R1, {1,0})[2]` is an index-3
//! constraint. `rotate_der` obtains the rate of `R` from its `w` field, which
//! the body of `rotate` never reads, and `R2.w` is an algebraic alias of the
//! state `w2`. Selecting independent coordinates must see how the velocity
//! closure depends on `w2`; the model is the frictionless two-inertia rotor
//! `(J1 + J2) q'' = -q`, so `q(t) = 0.3 cos(t / sqrt(3))` and `phi2 = q`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

#[test]
fn no_derivative_rate_alias_selects_states_and_follows_the_closed_form() {
    let compiled = Compiler::new()
        .model("NoDerivativeRateLoop")
        .compile_str(
            include_str!("../fixtures/index_reduction/NoDerivativeRateLoop.mo"),
            "NoDerivativeRateLoop.mo",
        )
        .unwrap();
    let result = match simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 2.0,
            dt: Some(0.05),
            solver_mode: SimSolverMode::Bdf,
            ..Default::default()
        },
    ) {
        Ok(result) => result,
        Err(error) => panic!("{error}"),
    };
    let column = |name: &str| {
        let Some(index) = result.names.iter().position(|candidate| candidate == name) else {
            panic!("missing {name}");
        };
        &result.data[index]
    };
    let (q, phi2) = (column("q"), column("phi2"));
    for (index, &t) in result.times.iter().enumerate() {
        let expected = 0.3 * (t / 3f64.sqrt()).cos();
        assert!(
            (q[index] - expected).abs() < 1e-4,
            "q({t}) = {} != {expected}",
            q[index]
        );
        assert!(
            (phi2[index] - q[index]).abs() < 1e-8,
            "closure violated at {t}: phi2 = {}, q = {}",
            phi2[index],
            q[index]
        );
    }
}
