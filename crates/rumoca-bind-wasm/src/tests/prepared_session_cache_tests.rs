use super::*;

const SOURCE: &str = r#"
model Cached
  input Real u = 0;
  Real x(start=1, fixed=true);
equation
  der(x) = u;
end Cached;
"#;
const OPTIONS: &str = r#"{"solver":"rk-like","dt":0.01,"execution_policy":"interpreter"}"#;

fn create() -> WasmSimulationSession {
    WasmSimulationSession::with_interactive_configuration(SOURCE, "Cached", OPTIONS).unwrap()
}

fn value(session: &mut WasmSimulationSession, name: &str) -> f64 {
    session.get(name).unwrap().unwrap()
}

#[test]
fn prepared_session_cache_restarts_defaults_and_replays_the_same_trajectory() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let mut first = create();
    assert!(!first.preparation_reused());
    let initial = value(&mut first, "x");
    first.set_input("u", 2.0).unwrap();
    first.advance_to(0.25).unwrap();
    let expected = value(&mut first, "x");
    assert!(expected > initial);
    drop(first);

    let mut repeat = create();
    assert!(repeat.preparation_reused());
    assert_eq!(repeat.time().to_bits(), 0.0_f64.to_bits());
    assert_eq!(value(&mut repeat, "x").to_bits(), initial.to_bits());
    assert_eq!(value(&mut repeat, "u").to_bits(), 0.0_f64.to_bits());
    repeat.set_input("u", 2.0).unwrap();
    repeat.advance_to(0.25).unwrap();
    assert_eq!(value(&mut repeat, "x").to_bits(), expected.to_bits());
    drop(repeat);
    clear_source_root_cache().unwrap();
}

#[test]
fn prepared_session_cache_never_shares_a_live_instances_state() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let mut first = create();
    let mut second = create();
    assert!(!first.preparation_reused());
    assert!(!second.preparation_reused());
    first.set_input("u", 7.0).unwrap();
    first.advance_to(0.1).unwrap();
    assert_eq!(second.time(), 0.0);
    assert_eq!(value(&mut second, "x"), 1.0);
    assert_eq!(value(&mut second, "u"), 0.0);
    drop(first);
    let mut third = create();
    assert!(third.preparation_reused());
    assert_eq!(value(&mut third, "x"), 1.0);
    third.set_input("u", 3.0).unwrap();
    third.advance_to(0.1).unwrap();
    assert_eq!(value(&mut second, "x"), 1.0);
    assert_eq!(second.time(), 0.0);
    drop((second, third));
    clear_source_root_cache().unwrap();
}

#[test]
fn prepared_session_cache_refuses_stale_options_clears_and_failed_instances() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    drop(create());
    let changed = OPTIONS.replace("0.01", "0.02");
    let different =
        WasmSimulationSession::with_interactive_configuration(SOURCE, "Cached", &changed).unwrap();
    assert!(!different.preparation_reused());
    drop(different);

    let mut failed = create();
    assert!(!failed.preparation_reused());
    assert!(failed.set_input("missing_input", 1.0).is_err());
    drop(failed);
    let active = create();
    assert!(!active.preparation_reused());
    crate::simulation_session_api::clear_prepared_session_cache();
    drop(active);
    let next = create();
    assert!(!next.preparation_reused());
    drop(next);
    clear_source_root_cache().unwrap();
}

#[test]
fn prepared_session_cache_retains_compiled_execution_and_resets_event_latches() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let source = r#"
model EventCached
  Real x(start=0, fixed=true);
  discrete Integer n(start=0, fixed=true);
equation
  der(x) = 1;
  when time >= 0.05 then
    n = pre(n) + 1;
  end when;
end EventCached;
"#;
    let options = r#"{"solver":"rk-like","dt":0.01,"execution_policy":"native_required"}"#;
    let mut first =
        WasmSimulationSession::with_interactive_configuration(source, "EventCached", options)
            .unwrap();
    assert!(!first.preparation_reused());
    let receipt = first.execution_receipt_json().unwrap();
    let engine: serde_json::Value = serde_json::from_str(&receipt).unwrap();
    assert_eq!(engine["engine"], "cranelift");
    first.advance_to(0.1).unwrap();
    assert_eq!(value(&mut first, "n"), 1.0);
    let expected_x = value(&mut first, "x");
    drop(first);
    let mut repeat =
        WasmSimulationSession::with_interactive_configuration(source, "EventCached", options)
            .unwrap();
    assert!(repeat.preparation_reused());
    assert_eq!(repeat.execution_receipt_json().unwrap(), receipt);
    assert_eq!(value(&mut repeat, "n"), 0.0);
    repeat.advance_to(0.1).unwrap();
    assert_eq!(value(&mut repeat, "n"), 1.0);
    assert_eq!(value(&mut repeat, "x").to_bits(), expected_x.to_bits());
    drop(repeat);
    clear_source_root_cache().unwrap();
}

#[test]
fn prepared_session_cache_distinguishes_signed_zero_initial_inputs() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let positive =
        r#"{"solver":"rk-like","execution_policy":"interpreter","initial_inputs":[["u",0.0]]}"#;
    let negative = positive.replace("0.0", "-0.0");
    drop(
        WasmSimulationSession::with_interactive_configuration(SOURCE, "Cached", positive).unwrap(),
    );
    let different =
        WasmSimulationSession::with_interactive_configuration(SOURCE, "Cached", &negative).unwrap();
    assert!(!different.preparation_reused());
    drop(different);
    clear_source_root_cache().unwrap();
}
