//! A record result is initialized by the declaration equations of its type's
//! fields before the algorithm runs (MLS 3.7 §12.4.4): a field the algorithm
//! never writes keeps its default, also when only some fields have one. A
//! field with neither a default nor an assignment is returned uninitialized,
//! which "is an error" (MLS 3.7 §12.4.4), reported as ED022.

use rumoca::Compiler;
use rumoca_compile::compile::{FailedPhase, Session, SessionConfig};
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const PARTIAL_DEFAULTS: &str = r#"
package Defaults
  record Data
    Real d;
    Real k = 2;
    Real c0 = 1;
  end Data;
  function make
    input Real d;
    output Data data;
  algorithm
    data.d := d;
    data.k := 3*d;
  end make;
  model Top
    Data r = make(time);
    Real y = r.d + r.k + r.c0;
  end Top;
end Defaults;
"#;

#[test]
fn an_unwritten_field_of_a_record_result_keeps_its_default() {
    let compiled = Compiler::new()
        .model("Defaults.Top")
        .compile_str(PARTIAL_DEFAULTS, "Defaults.mo")
        .unwrap_or_else(|error| panic!("Defaults.Top compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Defaults.Top simulates: {error}"));
    let index = result.names.iter().position(|n| n == "y").expect("y");
    for (row, &time) in result.times.iter().enumerate() {
        let expected = 4.0 * time + 1.0;
        assert!((result.data[index][row] - expected).abs() < 1e-12);
    }
}

#[test]
fn a_field_without_default_or_assignment_is_returned_uninitialized() {
    let source = PARTIAL_DEFAULTS
        .replace("    Real k = 2;\n", "    Real k;\n")
        .replace("    data.k := 3*d;\n", "");
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("Defaults.mo", &source)
        .expect("fixture parses");
    let failure = session
        .compile_model_dae_strict_reachable_uncached_with_recovery_detailed("Defaults.Top")
        .expect_err("an uninitialized result field is an error");
    assert_eq!(failure.phase, Some(FailedPhase::ToDae));
    assert_eq!(failure.error_code.as_deref(), Some("ED022"));
}

#[test]
fn a_default_that_reads_a_constant_initializes_its_unwritten_field() {
    let source = PARTIAL_DEFAULTS
        .replace(
            "package Defaults\n",
            "package Defaults\n  constant Real scale = 5;\n",
        )
        .replace(
            "    Real c0 = 1;\n",
            "    Real c0 = 1;\n    Real c1 = scale;\n",
        )
        .replace("r.d + r.k + r.c0;", "r.d + r.k + r.c0 + r.c1;");
    let compiled = Compiler::new()
        .model("Defaults.Top")
        .compile_str(&source, "Defaults.mo")
        .unwrap_or_else(|error| panic!("Defaults.Top compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Defaults.Top simulates: {error}"));
    let index = result.names.iter().position(|n| n == "y").expect("y");
    for (row, &time) in result.times.iter().enumerate() {
        let expected = 4.0 * time + 6.0;
        assert!((result.data[index][row] - expected).abs() < 1e-12);
    }
}

#[test]
fn an_unwritten_field_whose_default_reads_a_sibling_is_refused() {
    let source = PARTIAL_DEFAULTS
        .replace(
            "    Real c0 = 1;\n",
            "    Real c0 = 1;\n    Real c2 = c0 + k;\n",
        )
        .replace("r.d + r.k + r.c0;", "r.d + r.k + r.c0 + r.c2;");
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("Defaults.mo", &source)
        .expect("fixture parses");
    let failure = session
        .compile_model_dae_strict_reachable_uncached_with_recovery_detailed("Defaults.Top")
        .expect_err("a default that reads a written sibling is not seeded");
    assert_eq!(failure.error_code.as_deref(), Some("ED022"));
}

/// MSL R134a's Helmholtz coefficients: a protected record local whose type
/// gives its values in an extends clause (`extends Coeff(nc = 3, a = ...)`),
/// with a field sized by a sibling field (`Real a[nc]`) and a loop bounded
/// by one (`1:id.ns`). The constructor carries the extends modifiers (MLS 3.7
/// §12.6), and the never-written local holds them for the whole call. The
/// second loop's bound is R134a's `scalar(size(breaks))` (MLS 3.7 §10.3.3).
#[test]
fn a_record_local_holds_the_extends_modifiers_of_its_type() {
    let source = r#"
package Coefficients
  record Coeff
    parameter Integer nc = 5;
    parameter Integer ns;
    parameter Real[nc] a;
  end Coeff;
  record Cubic
    extends Coeff(nc = 3, ns = 2, a = {1.0, 2.0, 4.0});
  end Cubic;
  function f
    input Real x;
    output Real y;
  protected
    Cubic id;
  algorithm
    y := 0;
    for i in 1:id.ns loop
      y := y + id.a[i]*x^i;
    end for;
    for i in id.ns + 1:scalar(size(id.a)) loop
      y := y - id.a[i];
    end for;
  end f;
  model Top
    Real y = f(time);
  end Top;
end Coefficients;
"#;
    let compiled = Compiler::new()
        .model("Coefficients.Top")
        .compile_str(source, "Coefficients.mo")
        .unwrap_or_else(|error| panic!("Coefficients.Top compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Coefficients.Top simulates: {error}"));
    let index = result.names.iter().position(|n| n == "y").expect("y");
    for (row, &time) in result.times.iter().enumerate() {
        let expected = time + 2.0 * time * time - 4.0;
        assert!((result.data[index][row] - expected).abs() < 1e-12);
    }
}

/// A record redeclared by extending its base (`redeclare record extends
/// SaturationProperties end SaturationProperties;`, as MSL R134a's medium
/// does) declares no field itself; a local's modifiers of the inherited
/// fields, `sat(psat = p, Tsat = 0)`, are still its declaration value.
#[test]
fn modifiers_of_inherited_fields_initialize_a_redeclared_record_local() {
    let source = r#"
package Redeclared
  partial package Base
    replaceable record Sat
      Real psat;
      Real Tsat;
    end Sat;
  end Base;
  package Medium
    extends Base;
    redeclare record extends Sat
    end Sat;
    function twice
      input Sat sat;
      output Real y;
    algorithm
      y := 2*sat.psat + sat.Tsat;
    end twice;
    function f
      input Real p;
      output Real y;
    protected
      Sat sat(psat = p, Tsat = 1);
      Real v = twice(sat = sat);
    algorithm
      y := v + 1;
    end f;
  end Medium;
  model Top
    Real y = Medium.f(time);
  end Top;
end Redeclared;
"#;
    let compiled = Compiler::new()
        .model("Redeclared.Top")
        .compile_str(source, "Redeclared.mo")
        .unwrap_or_else(|error| panic!("Redeclared.Top compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Redeclared.Top simulates: {error}"));
    let index = result.names.iter().position(|n| n == "y").expect("y");
    for (row, &time) in result.times.iter().enumerate() {
        assert!((result.data[index][row] - (2.0 * time + 2.0)).abs() < 1e-12);
    }
}
