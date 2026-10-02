//! A Boolean, Integer, String, or enumeration variable is discrete-time (MLS
//! 3.7 §4.5) and changes only at events, so a definition outside a
//! when-clause must be a discrete-time expression (MLS 3.7 §3.8.4). A
//! definition that reads `time` or a continuous variable outside an
//! event-generating relation would change between events without one; it is
//! refused (ED023) rather than held at its last event value. Definitions built
//! from relations, `integer`, `pre`/`edge`, `sample`, discrete arguments, or
//! when-clauses are unaffected.

use rumoca::Compiler;
use rumoca_compile::compile::{FailedPhase, Session, SessionConfig};
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const EVENT_DEFINED: &str = r#"
model Ev
  function g
    input Boolean u;
    input Real x;
    output Boolean y;
  algorithm
    y := u and x > 0.2;
  end g;
  Real x(start = 1, fixed = true);
  Boolean b = time > 0.5;
  Integer n = integer(time*3);
  Boolean c = g(time > 0.5, 1.0);
  Boolean d;
  Integer k(start = 0, fixed = true);
  Integer m;
equation
  der(x) = if b then -x else x;
  d = pre(b) or edge(b);
  when sample(0, 0.25) then
    k = integer(10*time);
  end when;
  m = if x > 1.2 then 1 else 0;
  annotation(experiment(StopTime = 1));
end Ev;
"#;

#[test]
fn event_generating_discrete_definitions_simulate() {
    let compiled = Compiler::new()
        .model("Ev")
        .compile_str(EVENT_DEFINED, "Ev.mo")
        .unwrap_or_else(|error| panic!("Ev compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Ev simulates: {error}"));
    let last = |name: &str| {
        let index = result.names.iter().position(|n| n == name).expect(name);
        *result.data[index].last().expect("samples")
    };
    assert_eq!(last("b"), 1.0);
    assert_eq!(last("n"), 3.0);
    assert_eq!(last("c"), 1.0);
    assert_eq!(last("k"), 10.0);
}

fn rejection(source: &str, model: &str) -> Option<String> {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("Continuous.mo", source)
        .expect("fixture parses");
    let failure = session
        .compile_model_dae_strict_reachable_uncached_with_recovery_detailed(model)
        .expect_err("a continuous-time discrete definition is refused");
    assert_eq!(failure.phase, Some(FailedPhase::ToDae));
    failure.error_code
}

#[test]
fn a_boolean_record_field_from_a_function_of_time_is_refused() {
    let source = r#"
package P
  record Data
    Real zeta;
    Boolean flag;
  end Data;
  function make
    input Real d;
    output Data data;
  algorithm
    data.zeta := d;
    data.flag := d > 0.5;
  end make;
  model Top
    Data r = make(time);
  end Top;
end P;
"#;
    assert_eq!(rejection(source, "P.Top").as_deref(), Some("ED023"));
}

#[test]
fn a_relation_under_no_event_does_not_make_a_boolean_discrete() {
    let source = r#"
model NoEventBoolean
  Boolean b;
equation
  b = noEvent(time > 0.5);
end NoEventBoolean;
"#;
    assert_eq!(
        rejection(source, "NoEventBoolean").as_deref(),
        Some("ED023")
    );
}

/// MLS 3.7 §3.8.5: `mod` and `rem` generate events but are not discrete-time
/// expressions, so a String defined from `mod(time, 1)` is refused.
#[test]
fn a_string_of_mod_of_time_is_refused() {
    let source = r#"
model ModString
  String s = String(mod(time, 1));
end ModString;
"#;
    assert_eq!(rejection(source, "ModString").as_deref(), Some("ED023"));
}
