//! Dependency projection through a function fold whose carried values read
//! one another.
//!
//! A bounded `while` loop (the density inversions of `Modelica.Media.Water`
//! IF97) lowers to a fold whose update of each carried value reads the other
//! carried values. A carried value's dependency spans every point of the
//! fold, so a read of it from inside the fold's own update must not be keyed
//! by the current point; keying it so re-walked the whole fold once per
//! point, recursively, and a 100-iteration bound took seconds and a 200-iteration bound minutes.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const NEWTON: &str = r#"
model NewtonFold
  function g
    input Real d;
    input Real t;
    output Real f;
    output Real fd;
  algorithm
    f := d*d*d + t*d;
    fd := 3*d*d + t;
  end g;
  function newton
    input Real p;
    input Real t;
    output Real d;
  protected
    Real f;
    Real fd;
    Integer i = 0;
    Boolean found = false;
  algorithm
    d := 1;
    while i < 100 and not found loop
      (f, fd) := g(d, t);
      d := d - (f - p)/fd;
      found := abs(f - p) < 1e-12;
      i := i + 1;
    end while;
  end newton;
  Real v;
equation
  newton(v, 2) = time + 1;
end NewtonFold;
"#;

#[test]
fn a_fold_whose_carried_values_read_each_other_projects_once_per_point() {
    let compiled = Compiler::new()
        .model("NewtonFold")
        .compile_str(NEWTON, "NewtonFold.mo")
        .unwrap_or_else(|error| panic!("NewtonFold compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("NewtonFold simulates");
    let v = result
        .names
        .iter()
        .position(|name| name == "v")
        .expect("v is recorded");
    for (sample, time) in result.times.iter().enumerate() {
        let value = result.data[v][sample];
        // newton(v, 2) is the root d of d^3 + 2 d = v, so v = d^3 + 2 d at d = t + 1.
        let d = time + 1.0;
        let residual = value - (d * d * d + 2.0 * d);
        assert!(residual.abs() < 1e-6, "v({time}) = {value}");
    }
}
