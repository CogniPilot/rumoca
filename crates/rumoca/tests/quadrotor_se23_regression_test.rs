//! End-to-end regression guard for the SE_2(3) log-linear dynamic-inversion
//! quadrotor (13-state square-trajectory model).
//!
//! This exercises the full Lie-group control chain
//! (`inverse` -> `product` -> `log_map` -> `left_jacobian`, the `Jl*(BK*xi)`
//! matmul, quaternion kinematics, and the angular inner loop) against the real
//! `LieGroups` library, in a 13-element-state context with a non-zero initial
//! body rate. That combination is what two separate solve-lowering regressions
//! (both from PR #198 / commit 117ad7ccb) silently corrupted:
//!
//!   1. if-branch indexed-binding merge — `log_map`'s small-angle guard fills
//!      `xi[1..6]` in branches; losing them truncated the Lie-algebra vector.
//!   2. inlined-output shadow — `product`'s output `X[10]` shadows the model
//!      state `X[13]`; the inlined output inherited the caller's `X[11..13]`,
//!      so the control read `X[1]` as the caller's `X[11]`.
//!
//! Both made the body-rate derivatives diverge (`der(X[12])` jumped from 0 to
//! ~10x `der(X[11])`). The model fixture lives under `tests/fixtures/` and the
//! `LieGroups` library is pulled from the cached CMM release (via
//! `cargo xtask repo cmm ensure`), so this guards the actual model against the
//! real library, not a hand-written approximation.

use std::path::{Path, PathBuf};

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

/// The `LieGroups` library ships in the cached CogniPilot Modelica Models
/// (CMM) release, pulled by `cargo xtask repo cmm ensure`. Resolve its package
/// directory, or `None` when the cache is absent (so the test skips locally
/// without the cache, matching the other CMM-dependent regressions).
fn cached_lie_groups() -> Option<PathBuf> {
    let lie_groups =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../target/cmm/CMM-v0.0.2/LieGroups");
    lie_groups
        .join("package.mo")
        .is_file()
        .then_some(lie_groups)
}

fn der_value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .derivatives
        .iter()
        .find(|slot| slot.name == name)
        .unwrap_or_else(|| {
            panic!(
                "missing derivative {name}; have: {:?}",
                report
                    .derivatives
                    .iter()
                    .map(|s| s.name.clone())
                    .collect::<Vec<_>>()
            )
        })
        .value
}

#[test]
fn quadrotor_se23_13state_initial_derivatives_are_stable() {
    let Some(lie_groups) = cached_lie_groups() else {
        eprintln!(
            "skipping SE_2(3) 13-state quadrotor regression: requires cached CMM at \
             target/cmm/CMM-v0.0.2; run `cargo xtask repo cmm ensure`"
        );
        return;
    };
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/quadrotor_se23");
    let model = fixtures.join("QuadrotorSquare13_fn.mo");

    let compiled = Compiler::new()
        .model("QuadrotorSquare13_fn")
        .source_root(&lie_groups.to_string_lossy())
        .compile_path_dae(&model)
        .expect("SE_2(3) 13-state quadrotor should compile to DAE");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("SE_2(3) 13-state quadrotor should lower and evaluate at t=0");
    let report = &probe.report;
    assert!(report.error.is_none(), "eval error: {:?}", report.error);

    // Known-good derivative of the 13-state at its initial state (body rate
    // X[11] = 1.1326...). Captured from the validated build; the regressions
    // showed up here as der(X[12]) = ~10 x der(X[11]) instead of 0.
    let expected = [
        ("der(X[1])", 0.0),
        ("der(X[2])", 0.0),
        ("der(X[3])", 0.0),
        ("der(X[4])", 0.0),
        ("der(X[5])", 0.0),
        ("der(X[6])", 0.0),
        ("der(X[7])", 0.0),
        ("der(X[8])", 0.5663155510250311),
        ("der(X[9])", 0.0),
        ("der(X[10])", 0.0),
        ("der(X[11])", -2.2652622041001242),
        ("der(X[12])", 0.0),
        ("der(X[13])", 0.0),
    ];

    for (name, want) in expected {
        let got = der_value(report, name);
        assert!(
            got.is_finite(),
            "{name} is not finite ({got}) — SE_2(3) chain produced NaN/inf"
        );
        assert!(
            (got - want).abs() < 1e-9,
            "{name} = {got}, expected {want} (tol 1e-9). A solve-lowering \
             regression in the Lie-group control chain corrupted the derivative."
        );
    }
}
