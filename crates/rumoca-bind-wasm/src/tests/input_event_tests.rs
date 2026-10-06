use super::*;

#[test]
fn changed_input_events_update_conditional_outputs_and_state_derivatives() {
    let _guard = session_test_guard();
    for stateful in [false, true] {
        let state = if stateful {
            "Real x(start=0, fixed=true);"
        } else {
            ""
        };
        let derivative = if stateful { "der(x)=y;" } else { "" };
        let source = format!(
            "model ChangedInput input Real u=1; output Real y; {state} equation y=if u>0 then 2 else 1; {derivative} end ChangedInput;"
        );
        let mut session = crate::WasmSimulationSession::with_interactive_options(
            &source,
            "ChangedInput",
            0.1,
            "rk-like",
            1e-10,
            1e-10,
            "[]",
        )
        .unwrap();
        let mut expected_state = 0.0;
        for (index, u) in [1.0, 0.0, 0.5, -1.0, 2.0, 0.0].into_iter().enumerate() {
            let expected = if u > 0.0 { 2.0 } else { 1.0 };
            session.set_input("u", u).unwrap();
            assert_eq!(session.get("y").unwrap(), Some(expected));
            session.advance_to((index + 1) as f64 * 0.1).unwrap();
            assert_eq!(session.get("u").unwrap(), Some(u));
            assert_eq!(session.get("y").unwrap(), Some(expected));
            expected_state += 0.1 * expected;
            if stateful {
                assert!((session.get("x").unwrap().unwrap() - expected_state).abs() < 1e-8);
            }
        }
    }
}

#[test]
fn changed_input_events_fire_when_edges_once_and_preserve_invalid_batches() {
    let _guard = session_test_guard();
    let source = "model InputEdge input Real u=1; discrete Real sample(start=-3, fixed=true); equation when u>0 then sample=u; end when; end InputEdge;";
    let mut session = crate::WasmSimulationSession::with_interactive_options(
        source,
        "InputEdge",
        0.1,
        "rk-like",
        1e-10,
        1e-10,
        "[]",
    )
    .unwrap();
    for (u, sample) in [(0.0, -3.0), (0.5, 0.5), (2.0, 0.5), (-1.0, 0.5), (3.0, 3.0)] {
        session.set_input("u", u).unwrap();
        assert_eq!(session.get("sample").unwrap(), Some(sample));
    }
    assert!(session.set_inputs(r#"[["u",-1],["missing",0]]"#).is_err());
    assert_eq!(session.get("u").unwrap(), Some(3.0));
    assert_eq!(session.get("sample").unwrap(), Some(3.0));
    // One batch is one input event: its intermediate negative write cannot
    // manufacture a new false-to-true edge in an already positive condition.
    session.set_inputs(r#"[["u",-1],["u",4]]"#).unwrap();
    assert_eq!(session.get("u").unwrap(), Some(4.0));
    assert_eq!(session.get("sample").unwrap(), Some(3.0));
    session.set_input("u", 4.0).unwrap();
    assert_eq!(session.get("sample").unwrap(), Some(3.0));
}
