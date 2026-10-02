//! Initial equations that relate two discrete coordinates (MLS 3.7 §8.6).
//!
//! `Modelica.StateGraph.InitialStep` states `pre(newActive) = pre(localActive)`
//! with `active = true`, `active = localActive`, and `localActive =
//! pre(newActive)`: the relation's open side is fixed through the equation
//! section. The coordinates initial relations and plain aliases join take the
//! one determined value among them, in a breadth-first order from it; a
//! component with no determined coordinate, or with two, is refused.
//! OpenModelica gives every coordinate `true` until `reset` rises at 0.5.

use rumoca::Compiler;
use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model InitSteps
  Boolean active;
  Boolean localActive;
  Boolean newActive;
  Boolean oldActive;
  Boolean reset = time > 0.5;
initial equation
  active = true;
  pre(newActive) = pre(localActive);
  pre(oldActive) = pre(localActive);
equation
  active = localActive;
  localActive = pre(newActive);
  newActive = localActive and not reset;
  when reset then
    oldActive = localActive;
  end when;
end InitSteps;
model Underdetermined
  Boolean localActive;
  Boolean newActive;
initial equation
  pre(newActive) = pre(localActive);
equation
  localActive = pre(newActive);
  newActive = localActive and time < 0.5;
end Underdetermined;
model Overdetermined
  Boolean active;
  Boolean localActive;
  Boolean newActive;
initial equation
  active = true;
  pre(localActive) = false;
  pre(newActive) = pre(localActive);
equation
  active = localActive;
  localActive = pre(newActive);
  newActive = localActive and time < 0.5;
end Overdetermined;
"#;

#[test]
fn an_initial_relation_takes_the_value_the_equation_section_fixes() {
    let compiled = Compiler::new()
        .model("InitSteps")
        .compile_str(SOURCE, "InitSteps.mo")
        .unwrap_or_else(|error| panic!("InitSteps compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("InitSteps simulates: {error}"));
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        &result.data[index]
    };
    for name in ["active", "localActive", "newActive", "oldActive"] {
        assert_eq!(column(name)[0], 1.0, "{name} at t = 0");
    }
    for name in ["active", "localActive", "newActive"] {
        assert_eq!(
            *column(name).last().expect("samples"),
            0.0,
            "{name} at t = 1"
        );
    }
    assert_eq!(*column("oldActive").last().expect("samples"), 1.0);
}

fn refusal(model: &str) -> String {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("InitSteps.mo", SOURCE)
        .expect("fixture parses");
    let failure = session
        .compile_model_dae_strict_reachable_uncached_with_recovery_detailed(model)
        .expect_err("the relation is refused");
    assert_eq!(failure.error_code.as_deref(), Some("ED013"), "{model}");
    failure.summary
}

#[test]
fn an_undetermined_relation_is_refused() {
    assert!(refusal("Underdetermined").contains("undetermined"));
}

#[test]
fn an_over_determined_relation_is_refused() {
    assert!(refusal("Overdetermined").contains("over-determines"));
}
