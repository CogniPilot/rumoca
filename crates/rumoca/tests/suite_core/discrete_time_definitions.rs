//! A Boolean, Integer, String, or enumeration variable is discrete-time (MLS
//! 3.7 §4.5) and changes only at events, so a definition outside a
//! when-clause must be a discrete-time expression (MLS 3.7 §3.8.5). A
//! definition that reads `time` or a continuous variable outside an
//! event-generating relation would change between events without one; it is
//! refused (ED023) rather than held at its last event value. Definitions built
//! from relations, `integer`, `pre`/`edge`, `sample`, discrete arguments, or
//! when-clauses are unaffected.
//!
//! Documented deviation (SPEC_0022 EXPR-012): such a definition is accepted when
//! nothing reads the variable, as MSL `Media.Water` binds the Integer
//! `ThermodynamicState.phase` field from `setState_phX` of continuous port
//! values. The unread value is evaluated at every output point, matching
//! OpenModelica.

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
    Real y = if r.flag then 1 else 0;
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
  Real y;
equation
  b = noEvent(time > 0.5);
  y = if b then 1 else 0;
end NoEventBoolean;
"#;
    assert_eq!(
        rejection(source, "NoEventBoolean").as_deref(),
        Some("ED023")
    );
}

/// MLS 3.7 §3.8.5: `mod` and `rem` generate events but are not discrete-time
/// expressions, so a String defined from `mod(time, 1)` that another
/// definition reads is refused.
#[test]
fn a_string_of_mod_of_time_is_refused() {
    let source = r#"
model ModString
  String s = String(mod(time, 1));
  String t = s + "!";
end ModString;
"#;
    assert_eq!(rejection(source, "ModString").as_deref(), Some("ED023"));
}

/// A medium state whose Integer `phase` field is a function of a continuous
/// pressure. `pressure(state)` receives the field but never reads it.
const OBSERVED_PHASE: &str = r#"
package Obs
  record State
    Real p;
    Integer phase;
  end State;
  function setState
    input Real p;
    output State state;
  algorithm
    state := State(p = p, phase = if p > 1.5 then 2 else 1);
  end setState;
  function pressure
    input State state;
    output Real p;
  algorithm
    p := state.p;
  end pressure;
  function phaseOf
    input State state;
    output Integer phase;
  algorithm
    phase := state.phase;
  end phaseOf;
  model Top
    Real x(start = 1, fixed = true);
    State s = setState(x);
    Real y = pressure(s);
  equation
    der(x) = 1;
  end Top;
  model ReadByEquation
    extends Top;
    Real z = if s.phase == 2 then 1 else 0;
  end ReadByEquation;
  model ReadByWhen
    extends Top;
    Real w(start = 0, fixed = true);
  equation
    when s.phase == 2 then
      w = time;
    end when;
  end ReadByWhen;
  model ReadThroughFunction
    extends Top;
    Integer k = phaseOf(s);
  end ReadThroughFunction;
end Obs;
"#;

#[test]
fn an_unread_integer_field_of_a_continuous_state_is_observed_at_output_points() {
    let compiled = Compiler::new()
        .model("Obs.Top")
        .compile_str(OBSERVED_PHASE, "Obs.mo")
        .unwrap_or_else(|error| panic!("Obs.Top compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Obs.Top simulates: {error}"));
    let phase = result
        .names
        .iter()
        .position(|name| name == "s.phase")
        .expect("s.phase is reported");
    // OpenModelica reports phase 1 through t = 0.5 (x = 1.5 is not > 1.5)
    // and 2 afterwards, at every output point, with no event.
    for (time, value) in result.times.iter().zip(&result.data[phase]) {
        let expected = if *time > 0.5 + 1e-9 { 2.0 } else { 1.0 };
        assert_eq!(*value, expected, "s.phase at t = {time}");
    }
}

#[test]
fn a_continuous_integer_field_read_by_an_equation_is_refused() {
    assert_eq!(
        rejection(OBSERVED_PHASE, "Obs.ReadByEquation").as_deref(),
        Some("ED023")
    );
}

#[test]
fn a_continuous_integer_field_read_by_a_when_condition_is_refused() {
    assert_eq!(
        rejection(OBSERVED_PHASE, "Obs.ReadByWhen").as_deref(),
        Some("ED023")
    );
}

#[test]
fn a_continuous_integer_field_read_inside_a_function_is_refused() {
    assert_eq!(
        rejection(OBSERVED_PHASE, "Obs.ReadThroughFunction").as_deref(),
        Some("ED023")
    );
}

#[test]
fn a_continuous_variable_read_through_its_binding_is_continuous_time() {
    // `v` is continuous through its binding, which reads `w`, whose binding
    // reads `time`; `f` hides the relation in a function, so `n` would change
    // without an event.
    let source = r#"
model BoundRead
  function f
    input Real u;
    output Integer n;
  algorithm
    n := if u > 0.5 then 1 else 0;
  end f;
  Real w = time;
  Real v = 2*w;
  Integer n = f(v);
  Real y = n;
end BoundRead;
"#;
    assert_eq!(rejection(source, "BoundRead").as_deref(), Some("ED023"));
}

#[test]
fn an_algorithm_read_makes_a_continuous_time_discrete_definition_observed() {
    // `n` is defined from a relation hidden in a function, so it is only
    // admitted while nothing reads it; the algorithm reads it.
    let source = r#"
model AlgorithmRead
  function f
    input Real u;
    output Integer n;
  algorithm
    n := if u > 0.5 then 1 else 0;
  end f;
  Integer n = f(time);
  Real z;
algorithm
  z := 2*n;
end AlgorithmRead;
"#;
    assert_eq!(rejection(source, "AlgorithmRead").as_deref(), Some("ED023"));
}
