//! Index reduction differentiates `fill` and `sum` (MLS 3.7 §10.3.3, §10.3.4):
//! `d/dt fill(s, n) = fill(ds, n)` with the extent fixed, and
//! `d/dt sum(u) = sum(du)`.
//!
//! `y` is differentiated but defined by the state `x`, so its derivative is
//! the derivative of that definition: `y = x = exp(-t)` and `w = -exp(-t)`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model Reduction
  Real x(start=1, fixed=true);
  Real y;
  Real w;
equation
  der(x) = -x;
  y = sum(fill(x, 3))/3;
  w = der(y);
end Reduction;
"#;

fn check(source: &str) {
    let compiled = Compiler::new()
        .model("Reduction")
        .compile_str(source, "fill_sum_reduction.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 1.0,
                dt: Some(0.1),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
        let column = |name: &str| {
            let index = result.names.iter().position(|n| n == name).unwrap();
            &result.data[index]
        };
        for (k, &t) in result.times.iter().enumerate() {
            let x = (-t).exp();
            for (name, expected) in [("x", x), ("y", x), ("w", -x)] {
                let actual = column(name)[k];
                assert!(
                    (actual - expected).abs() < 1e-4,
                    "{solver_mode:?}: {name}({t}) = {actual}, expected {expected}"
                );
            }
        }
    }
}

#[test]
fn sum_of_fill_definition_is_differentiated() {
    check(SOURCE);
}

#[test]
fn fill_definition_is_differentiated() {
    check(&SOURCE.replace("sum(fill(x, 3))/3", "fill(x, 3)*{1, 1, 1}/3"));
}

#[test]
fn sum_definition_is_differentiated() {
    check(&SOURCE.replace("sum(fill(x, 3))/3", "sum({x, 2*x, 3*x})/6"));
}
