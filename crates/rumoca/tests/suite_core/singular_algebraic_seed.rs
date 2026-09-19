use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};
use rumoca_solver::SimExecutionPolicy;

// `r` has no start attribute, so it defaults to zero. The only equation that
// determines it, `r*r = x + 0.1`, has a Jacobian `2*r` that vanishes at that
// seed, so Newton carries no direction to leave the critical point. The block
// must still resolve to `r = +sqrt(x + 0.1)`; this reproduces the silently
// unsolved algebraic equation from the Ball playground report.
const POSITIVE_SEED: &str = r#"
model SingularAlgebraicSeed
  Real x(start=10);
  Real v(start=1);
  Real r;
  parameter Real g = 9.81;
equation
  der(x) = v;
  der(v) = -g;
  r*r = x + 0.1;
end SingularAlgebraicSeed;
"#;

// A negative start selects the negative branch. The default-zero seed above and
// this signed seed together pin the OpenModelica convention: the sign of the
// start value chooses the root, and the default zero resolves toward `+`.
const NEGATIVE_SEED: &str = r#"
model SingularAlgebraicSeedNegative
  Real x(start=10);
  Real v(start=1);
  Real r(start=-1);
  parameter Real g = 9.81;
equation
  der(x) = v;
  der(v) = -g;
  r*r = x + 0.1;
end SingularAlgebraicSeedNegative;
"#;

fn simulate_channel(source: &str, model: &str, policy: SimExecutionPolicy) -> (Vec<f64>, Vec<f64>) {
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(source, "singular_algebraic_seed.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
            rtol: 1e-10,
            atol: 1e-10,
            execution_policy: policy,
            ..Default::default()
        },
    )
    .unwrap();
    let x = result.names.iter().position(|name| name == "x").unwrap();
    let r = result.names.iter().position(|name| name == "r").unwrap();
    (result.data[x].clone(), result.data[r].clone())
}

#[test]
fn nonlinear_algebraic_default_seed_resolves_to_positive_root() {
    for policy in [SimExecutionPolicy::Auto, SimExecutionPolicy::Interpreter] {
        let (xs, rs) = simulate_channel(POSITIVE_SEED, "SingularAlgebraicSeed", policy);
        for (x, r) in xs.iter().zip(&rs) {
            let expected = (x + 0.1).sqrt();
            assert!(
                r > &0.0,
                "{policy:?}: default seed must land on the positive root, got r={r}"
            );
            assert!(
                (r - expected).abs() < 1e-6,
                "{policy:?}: r={r} does not satisfy r*r=x+0.1 (expected {expected})"
            );
        }
    }
}

#[test]
fn nonlinear_algebraic_negative_seed_keeps_negative_root() {
    for policy in [SimExecutionPolicy::Auto, SimExecutionPolicy::Interpreter] {
        let (xs, rs) = simulate_channel(NEGATIVE_SEED, "SingularAlgebraicSeedNegative", policy);
        for (x, r) in xs.iter().zip(&rs) {
            let expected = -(x + 0.1).sqrt();
            assert!(
                r < &0.0,
                "{policy:?}: negative seed must keep the negative root, got r={r}"
            );
            assert!(
                (r - expected).abs() < 1e-6,
                "{policy:?}: r={r} does not satisfy r*r=x+0.1 (expected {expected})"
            );
        }
    }
}
