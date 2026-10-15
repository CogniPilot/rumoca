use super::*;

const SOURCE: &str = "model PeriodicControllerClock parameter Real period = 0.01; \
    discrete output Real ticks(start=0, fixed=true); algorithm \
    when sample(0, period) then ticks := pre(ticks)+1; end when; end PeriodicControllerClock;";

#[test]
fn an_interactive_session_keeps_ticking_past_one_second() {
    let _guard = session_test_guard();
    let mut session = crate::WasmSimulationSession::with_interactive_options(
        SOURCE,
        "PeriodicControllerClock",
        0.005,
        "rk-like",
        1e-8,
        1e-6,
        "[]",
    )
    .unwrap();
    assert_eq!(session.get("ticks").unwrap(), Some(1.0));
    for time in [0.1, 0.5, 1.0, 1.1, 2.0, 3.0] {
        session.advance_to(time).unwrap();
        assert_eq!(
            session.get("ticks").unwrap(),
            Some((time / 0.01_f64).round() + 1.0),
            "periodic events through {time}s"
        );
    }
    session.reset_at(0.0).unwrap();
    session.advance_to(3.0).unwrap();
    assert_eq!(session.get("ticks").unwrap(), Some(301.0));
}

#[test]
fn the_execution_receipt_names_the_engine_and_any_refusal() {
    let _guard = session_test_guard();
    let compiled_engine = if cfg!(target_arch = "wasm32") {
        "wasm_program"
    } else {
        "cranelift"
    };
    for (policy, engine, refusal) in [
        (None, compiled_engine, None),
        (Some("native_required"), compiled_engine, None),
        (Some("auto"), "interpreter", Some("no_continuous_states")),
        (Some("interpreter"), "interpreter", None),
    ] {
        let mut options = serde_json::json!({"dt": 0.005, "solver": "rk-like"});
        if let Some(policy) = policy {
            options["execution_policy"] = policy.into();
        }
        let mut session = crate::WasmSimulationSession::with_interactive_configuration(
            SOURCE,
            "PeriodicControllerClock",
            &options.to_string(),
        )
        .unwrap();
        let receipt: serde_json::Value =
            serde_json::from_str(&session.execution_receipt_json().unwrap()).unwrap();
        assert_eq!(
            receipt,
            serde_json::json!({"engine": engine, "refusal": refusal, "declined": []}),
            "policy {policy:?}"
        );
        session.advance_to(0.1).unwrap();
        assert_eq!(session.get("ticks").unwrap(), Some(11.0));
    }
}
