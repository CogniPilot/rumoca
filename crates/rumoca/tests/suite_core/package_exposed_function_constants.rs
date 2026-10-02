//! A function's constants take the values of the package that exposes it
//! (MLS 3.7 §7.3, SPEC_0040 FLAT-C02).
//!
//! `M` extends `PMix(names = {"a", "b"})` and redeclares its state record, so
//! `n = size(names, 1)` is two in `M` while the shared declaration defaults
//! to one. This is the shape of `Modelica.Media.Air.MoistAir`, whose
//! `substanceNames` sets the extent of every `ThermodynamicState.X`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package PM
  constant String names[:] = {"unusable"};
  final constant Integer n = size(names, 1);
  constant String extra[:] = fill("", 0);
  replaceable record State
  end State;
  replaceable partial function f
    input State s;
    output Real y;
  end f;
end PM;
partial package PMix
  extends PM;
  redeclare replaceable record extends State
    Real p;
    Real T;
    Real X[n];
  end State;
end PMix;
package M
  extends PMix(names = {"a", "b"});
  redeclare record extends State
  end State;
  redeclare function extends f
  algorithm
    y := 2*s.T + s.X[n];
  end f;
end M;
model ExposedConstants
  package Medium = M(extra = {"c"});
  Medium.State s = Medium.State(p = 1, T = time, X = {0.25, 0.75});
  Real y = Medium.f(s);
end ExposedConstants;
"#;

#[test]
fn a_redeclared_record_and_function_use_the_exposing_package_constants() {
    let compiled = Compiler::new()
        .model("ExposedConstants")
        .compile_str(SOURCE, "ExposedConstants.mo")
        .unwrap_or_else(|error| panic!("ExposedConstants compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("ExposedConstants simulates");
    let y = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("y is recorded");
    for (sample, time) in result.times.iter().enumerate() {
        let expected = 2.0 * time + 0.75;
        let value = result.data[y][sample];
        assert!((value - expected).abs() < 1e-12, "y({time}) = {value}");
    }
}

/// A helper called without a prefix from functions of two packages reads
/// the value the exposing package gives its constant: packages that agree
/// share it, and packages that disagree each get their own (no package is
/// read through another). A single instance exposed through packages that
/// disagree is refused (EF034, `function_exposures` unit tests).
const SHARED_HELPER: &str = r#"
package Base
  constant Real k = 1;
  function h
    input Real x;
    output Real y;
  algorithm
    y := k*x;
  end h;
  function g
    input Real x;
    output Real y;
  algorithm
    y := h(x);
  end g;
end Base;
package A
  extends Base(k = 2);
end A;
package B
  extends Base(k = 2);
end B;
package C
  extends Base(k = 3);
end C;
model Agree
  Real ya = A.g(time);
  Real yb = B.g(time);
end Agree;
model Disagree
  Real ya = A.g(time);
  Real yc = C.g(time);
end Disagree;
"#;

#[test]
fn a_helper_shared_by_packages_that_agree_reads_their_value() {
    let compiled = Compiler::new()
        .model("Agree")
        .compile_str(SHARED_HELPER, "SharedHelper.mo")
        .unwrap_or_else(|error| panic!("Agree compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("Agree simulates");
    for name in ["ya", "yb"] {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        let last = *result.data[column].last().expect("samples");
        assert!((last - 2.0).abs() < 1e-12, "{name}(1) = {last}");
    }
}

#[test]
fn packages_that_disagree_each_read_their_own_value() {
    let compiled = Compiler::new()
        .model("Disagree")
        .compile_str(SHARED_HELPER, "SharedHelper.mo")
        .unwrap_or_else(|error| panic!("Disagree compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("Disagree simulates");
    for (name, expected) in [("ya", 2.0), ("yc", 3.0)] {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        let last = *result.data[column].last().expect("samples");
        assert!((last - expected).abs() < 1e-12, "{name}(1) = {last}");
    }
}
