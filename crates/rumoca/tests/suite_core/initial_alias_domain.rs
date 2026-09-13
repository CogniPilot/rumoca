//! MLS §8.6: a fixed alias determines the coordinate before dependent evaluation.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model InitialAliasDomain
  Real a;
  Real b(start=0.6, fixed=true);
  Real g;
equation
  a = b;
  g = -1/a;
  der(a) = g;
end InitialAliasDomain;
"#;

fn check_initial_alias(solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("InitialAliasDomain")
        .compile_str(SOURCE, "initial_alias_domain.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.001,
            dt: Some(0.0001),
            rtol: 1e-6,
            atol: 1e-6,
            solver_mode,
            ..SimOptions::default()
        },
    )
    .unwrap();
    for name in ["a", "b"] {
        let values = &result.data[result.names.iter().position(|n| n == name).unwrap()];
        assert_eq!(values[0], 0.6, "the fixed alias owns the initial value");
        assert!(values.iter().all(|value| value.is_finite()));
    }
    assert_eq!(*result.times.last().unwrap(), 0.001);
}

#[test]
fn bdf_fixed_alias_precedes_domain_restricted_algebraic() {
    check_initial_alias(SimSolverMode::Bdf);
}

#[test]
fn rk_fixed_alias_precedes_domain_restricted_algebraic() {
    check_initial_alias(SimSolverMode::RkLike);
}
