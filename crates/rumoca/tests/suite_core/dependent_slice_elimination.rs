//! Triangular loop domains in a function: a slice whose start is an enclosing
//! loop variable (`matrix[row, column:n]` under `for row in column + 1:n`) and
//! a reduction over `i + 1:n` keep compact checked owners (MLS §10.5, §11.2.2)
//! and evaluate the exact elimination.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
function eliminate
  input Real A[:, size(A, 1)];
  input Real b[size(A, 1)];
  output Real x[size(A, 1)];
protected
  Integer n = size(A, 1);
  Real matrix[size(A, 1), size(A, 1)];
  Real rhs[size(A, 1)];
  Real factor;
algorithm
  matrix := A;
  rhs := b;
  for column in 1:n loop
    for row in column + 1:n loop
      factor := matrix[row, column] / matrix[column, column];
      rhs[row] := rhs[row] - factor * rhs[column];
      matrix[row, column:n] := matrix[row, column:n] - factor * matrix[column, column:n];
    end for;
  end for;
  x := zeros(n);
  for reverse in 1:n loop
    x[n - reverse + 1] := (rhs[n - reverse + 1]
      - sum(matrix[n - reverse + 1, k] * x[k] for k in n - reverse + 2:n))
      / matrix[n - reverse + 1, n - reverse + 1];
  end for;
end eliminate;
model TriangularSolve
  parameter Real A[3, 3] = {{4, 1, 0}, {1, 3, 1}, {0, 1, 2}};
  parameter Real b[3] = {1, 2, 3};
  Real x[3];
equation
  x = eliminate(A, b);
end TriangularSolve;
"#;

#[test]
fn dependent_slices_and_reductions_solve_the_exact_system() {
    let compiled = Compiler::new()
        .model("TriangularSolve")
        .compile_str(SOURCE, "TriangularSolve.mo")
        .expect("triangular loop domains have compact checked owners");
    let simulation =
        simulate_dae(&compiled.dae, &SimOptions::default()).expect("the elimination simulates");
    for (name, expected) in [
        ("x[1]", 2.0 / 9.0),
        ("x[2]", 1.0 / 9.0),
        ("x[3]", 13.0 / 9.0),
    ] {
        let output = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("simulation exposes {name}"));
        assert!(
            simulation.data[output]
                .iter()
                .all(|value| (*value - expected).abs() <= 1.0e-12),
            "{name} should be {expected}: {:?}",
            simulation.data[output]
        );
    }
}
