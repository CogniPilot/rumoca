use super::*;

const SOURCE: &str = r#"
model Readout
  input Real u = 2;
  Real x(start=1, fixed=true);
  output Real y;
equation
  der(x) = u;
  y = 2*x;
end Readout;
"#;

fn create() -> WasmSimulationSession {
    WasmSimulationSession::with_interactive_configuration(
        SOURCE,
        "Readout",
        r#"{"solver":"rk-like","dt":0.01,"execution_policy":"interpreter"}"#,
    )
    .unwrap()
}

#[test]
fn typed_readout_preserves_order_duplicates_and_all_visible_values() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let mut session = create();
    for time in [0.0, 0.1, 0.25] {
        session.advance_to(time).unwrap();
        let state: serde_json::Value =
            serde_json::from_str(&session.state_json().unwrap()).unwrap();
        let mut names: Vec<String> = state["values"]
            .as_object()
            .unwrap()
            .keys()
            .cloned()
            .collect();
        names.reverse();
        names.extend(["u".into(), "x".into(), "u".into()]);
        let values = session
            .read_values(&serde_json::to_string(&names).unwrap())
            .unwrap();
        assert_eq!(values.len(), names.len());
        for (name, value) in names.iter().zip(values) {
            assert_eq!(
                value.to_bits(),
                state["values"][name].as_f64().unwrap().to_bits()
            );
        }
        assert_eq!(session.time().to_bits(), time.to_bits());
        assert_eq!(session.state_json().unwrap(), state.to_string());
    }
    clear_source_root_cache().unwrap();
}

#[test]
fn typed_readout_observes_changed_inputs_and_original_reset() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let mut session = create();
    session.set_input("u", -3.0).unwrap();
    assert_eq!(
        session.read_values(r#"["u","x","y"]"#).unwrap(),
        [-3.0, 1.0, 2.0]
    );
    session.advance_to(0.1).unwrap();
    let before = session.state_json().unwrap();
    let values = session.read_values(r#"["y","u","x"]"#).unwrap();
    assert_eq!(values[1], -3.0);
    assert_eq!(values[0].to_bits(), (2.0 * values[2]).to_bits());
    assert_eq!(session.state_json().unwrap(), before);
    session.reset().unwrap();
    assert_eq!(
        session.read_values(r#"["u","x","y"]"#).unwrap(),
        [2.0, 1.0, 2.0]
    );
    clear_source_root_cache().unwrap();
}

#[test]
fn typed_readout_refuses_unknown_names_and_invalid_requests_without_mutation() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let mut session = create();
    let before = session.state_json().unwrap();
    for request in [r#"["x","missing","u"]"#, "[", "null", "{}", "[1]"] {
        let error = session.read_values(request).unwrap_err();
        assert!(error.message().starts_with("[EX012]"), "{error}");
        assert_eq!(session.state_json().unwrap(), before);
    }
    assert!(session.read_values("[]").unwrap().is_empty());
    assert_eq!(session.read_values(r#"["x"]"#).unwrap(), [1.0]);
    clear_source_root_cache().unwrap();
}
