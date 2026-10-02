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
