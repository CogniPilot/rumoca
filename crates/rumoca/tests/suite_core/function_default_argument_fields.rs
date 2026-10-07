//! MLS 3.7 section 12.4.1: an omitted function input takes its default
//! expression, which may read an earlier input, so a default that selects a
//! nested field of a record input reads that field of the actual the call
//! supplied, whatever path the caller spells for it.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const NESTED_ACTUAL: &str = r#"
package D
  record Cat
    Real pos[2];
  end Cat;
  record Loc
    Cat catalog;
    Real g;
  end Loc;
  record Est
    Loc localization;
  end Est;
  record S
    Est estimator;
  end S;
  function Pub
    input Loc previous;
    input Real k;
    input Real p[2] = previous.catalog.pos;
    output Real y;
  algorithm
    y := k * (p[1] + p[2]) + previous.g;
  end Pub;
  function Outer
    input S previous;
    output Real y;
  algorithm
    y := Pub(previous.estimator.localization, 2);
  end Outer;
  function MissingDefault
    input Real a;
    input Real b;
    output Real y;
  algorithm
    y := a + b;
  end MissingDefault;
end D;
model Nested
  parameter Real u = 1;
  Real y = D.Outer(D.S(D.Est(D.Loc(D.Cat({u, 2}), 3))));
end Nested;
model Omitted
  Real y = D.MissingDefault(1);
end Omitted;
"#;

/// The default `previous.catalog.pos` reads the supplied nested field path
/// `previous.estimator.localization`: 2 * (1 + 2) + 3.
#[test]
fn a_default_reads_a_field_of_a_nested_field_actual() {
    let compiled = Compiler::new()
        .model("Nested")
        .compile_str(NESTED_ACTUAL, "Nested.mo")
        .expect("the default resolves through the supplied nested field path");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the default-argument DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let y = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == "y")
        .expect("solver value y")
        .value;
    assert_eq!(y, 9.0);
}

/// An omitted input with no default is still refused (nothing to read).
#[test]
fn an_omitted_input_without_a_default_is_refused() {
    let error = Compiler::new()
        .model("Omitted")
        .compile_str(NESTED_ACTUAL, "Omitted.mo")
        .expect_err("an input with neither argument nor default is refused");
    assert!(
        error.to_string().contains("no argument and no default"),
        "{error}"
    );
}
