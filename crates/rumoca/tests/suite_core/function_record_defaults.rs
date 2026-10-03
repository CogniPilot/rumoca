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
