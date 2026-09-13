use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
function permute
  input Real u[2];
  output Real result[2,2];
algorithm
  result := [u[2]*u[2], 7; u[1]*u[1], 11];
  annotation(Inline=true);
end permute;
model IndexedConstraint
  Real x[2](start={1,2}, each fixed=true);
  Real y[2,2];
  Real a[2,2];
equation
  y = permute(x);
  der(x) = -x;
  a = der(y);
end IndexedConstraint;
"#;

#[test]
fn constant_index_and_matrix_constructor_maps_preserve_constraint_derivatives() {
    let compiled = Compiler::new()
        .model("IndexedConstraint")
        .compile_str(SOURCE, "indexed_constraint.mo")
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
        for (name, initial, rate) in [
            ("x[1]", 1.0, 1.0),
            ("x[2]", 2.0, 1.0),
            ("y[1,1]", 4.0, 2.0),
            ("y[2,1]", 1.0, 2.0),
            ("y[1,2]", 7.0, 0.0),
            ("y[2,2]", 11.0, 0.0),
            ("a[1,1]", -8.0, 2.0),
            ("a[2,1]", -2.0, 2.0),
            ("a[1,2]", 0.0, 0.0),
            ("a[2,2]", 0.0, 0.0),
        ] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (&time, &actual) in result.times.iter().zip(&result.data[column]) {
                let expected = initial * (-rate * time).exp();
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?} {name}({time}): {actual} != {expected}"
                );
            }
        }
    }
}

#[test]
fn a_runtime_index_does_not_acquire_an_invariant_projection_proof() {
    let source = r#"
model RuntimeIndex
  input Integer k;
  Real x[2];
  Real y;
  Real rate;
equation
  der(x) = -x;
  y = x[k];
  rate = der(y);
end RuntimeIndex;
"#;
    let compiled = Compiler::new()
        .model("RuntimeIndex")
        .compile_str(source, "runtime_index.mo")
        .unwrap();
    let error = simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default()).expect_err(
        "changing which state is selected cannot use the constant-index derivative rule",
    );
    assert!(
        error.to_string().contains("structurally singular"),
        "{error}"
    );
}
