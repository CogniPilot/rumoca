//! Element definedness through array comprehensions in function bodies.
//!
//! MLS §12.4.4 leaves an unwritten function local without a value, so the DAE
//! admits a read of an element-written local only where every element it reads
//! already has a definition. MLS §10.4.2 binds a comprehension iterator to each
//! value of its range in turn, so `{u[i] * V[i, j + 1] for i in 1:size(u, 1)}`
//! reads exactly the elements of column `j + 1`. The Vandermonde column fill of
//! `Modelica.Math.Polynomials.fitting` writes column `n + 1` first and then each
//! column from the one after it, which is defined at every read.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let Some(index) = result.names.iter().position(|candidate| candidate == name) else {
        panic!("simulation result missing column {name}");
    };
    result.data[index].as_slice()
}

const COLUMN_FILL: &str = r#"
model ColumnFill
  function vandermondeSum
    input Real u[:];
    input Integer n;
    output Real s;
  protected
    Real V[size(u, 1), n + 1];
  algorithm
    V[:, n + 1] := ones(size(u, 1));
    for j in n:-1:1 loop
      V[:, j] := {u[i] * V[i, j + 1] for i in 1:size(u, 1)};
    end for;
    s := sum(V);
  end vandermondeSum;
  Real x = time + 2;
  Real y = vandermondeSum({x, 2 * x}, 2);
end ColumnFill;
"#;

/// Column `j` reads column `j + 1`, which the previous iteration (or the
/// initial `n + 1` write) defined, so the comprehension's reads are covered.
#[test]
fn comprehension_reads_of_defined_columns_are_admitted() {
    let compiled = Compiler::new()
        .model("ColumnFill")
        .compile_str(COLUMN_FILL, "ColumnFill.mo")
        .expect("the Vandermonde column fill compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("the Vandermonde column fill simulates");
    for (x, y) in series(&result, "x").iter().zip(series(&result, "y")) {
        // Rows [u^2, u, 1] for u = x and u = 2x.
        let expected = 5.0 * x * x + 3.0 * x + 2.0;
        assert!(
            (y - expected).abs() < 1e-9 * expected.abs().max(1.0),
            "vandermondeSum at x = {x}: {y}, expected {expected}"
        );
    }
}

const COLUMN_FILL_FORWARD: &str = r#"
model ColumnFillForward
  function vandermondeSum
    input Real u[:];
    input Integer n;
    output Real s;
  protected
    Real V[size(u, 1), n + 1];
  algorithm
    V[:, n + 1] := ones(size(u, 1));
    for j in 1:n loop
      V[:, j] := {u[i] * V[i, j + 1] for i in 1:size(u, 1)};
    end for;
    s := sum(V);
  end vandermondeSum;
  Real x = time + 2;
  Real y = vandermondeSum({x, 2 * x}, 2);
end ColumnFillForward;
"#;

/// Filling the columns forward reads column 2 before any write defines it, so
/// the comprehension names an undefined element and the read stays refused.
#[test]
fn comprehension_reads_of_undefined_columns_are_refused() {
    let error = Compiler::new()
        .model("ColumnFillForward")
        .compile_str(COLUMN_FILL_FORWARD, "ColumnFillForward.mo")
        .expect_err("a read of an undefined column must be refused");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("reads elements of `V` that do not all have a definition"),
        "expected the element definedness refusal, got: {rendered}"
    );
}
