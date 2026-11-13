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

const DECAY: &str = "model Decay Real x(start=1, fixed=true); equation der(x) = -x; end Decay;";

/// `x(0.5)` of the decay and the accepted steps taken, for one options
/// document.
fn decay_at_half(options: &str) -> Result<(f64, u64), WasmError> {
    rumoca_sim::reset_step_counts();
    let mut session =
        WasmSimulationSession::with_interactive_configuration(DECAY, "Decay", options)?;
    session.advance_to(0.5)?;
    let counts = rumoca_sim::step_counts();
    assert_eq!(counts.integrator, Some("fixed-step"));
    Ok((session.read_values(r#"["x"]"#)?[0], counts.solver_steps))
}

/// The options document selects `fixed-step` by name and its `fixed_step`
/// object reaches the plugin: the step sets the step count and the order sets
/// the method's error (`h/2 * t * exp(-t)` for explicit Euler at `h = 1/32`,
/// 4.7e-3; `h^4/120 * t * exp(-t)` for the classical method, 2.4e-9).
#[test]
fn fixed_step_options_document_reaches_the_plugin() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let exact = (-0.5_f64).exp();
    let (euler, steps) = decay_at_half(
        r#"{"solver":"fixed-step","fixed_step":{"step":0.03125,"order":1},"execution_policy":"interpreter"}"#,
    )
    .unwrap();
    assert_eq!(steps, 16);
    assert!((1.0e-3..7.0e-3).contains(&(euler - exact).abs()), "{euler}");
    let (classical, steps) = decay_at_half(
        r#"{"solver":"fixed-step","fixed_step":{"step":0.015625,"order":4},"execution_policy":"interpreter"}"#,
    )
    .unwrap();
    assert_eq!(steps, 32);
    assert!((classical - exact).abs() < 1.0e-9, "{classical}");
    // Without the object the named solver runs its 1 ms classical default.
    let (default, steps) =
        decay_at_half(r#"{"solver":"Fixed_Step","execution_policy":"interpreter"}"#).unwrap();
    assert_eq!(steps, 500);
    assert!((default - exact).abs() < 1.0e-12, "{default}");
    clear_source_root_cache().unwrap();
}

#[test]
fn fixed_step_options_document_is_refused_when_invalid_or_misdirected() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    for (options, expected) in [
        (
            r#"{"solver":"fixed-step","fixed_step":{"step":0.005,"order":3}}"#,
            "invalid FixedStep option `order`",
        ),
        (
            r#"{"solver":"fixed-step","fixed_step":{"step":0,"order":4}}"#,
            "invalid FixedStep option `step`",
        ),
        (
            r#"{"solver":"rk-like","fixed_step":{"step":0.005,"order":4}}"#,
            "Rk45 options requested, but the payload belongs to FixedStep",
        ),
        (
            r#"{"solver":"fixed-step","fixed_step":{"step":0.005,"order":4,"tolerance":1}}"#,
            "unknown field `tolerance`",
        ),
    ] {
        let error = decay_at_half(options).expect_err("the request is refused");
        assert!(error.message().contains(expected), "{error}");
    }
    clear_source_root_cache().unwrap();
}
