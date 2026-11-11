//! A selected read answers exactly what the keyed visible map holds.

use super::*;
use crate::fmi_me::session::{MeOutputCursor, MeSimulationSession};

/// The oscillator with two declared inputs: `v` shares its name with the
/// velocity output, `u` names no output at all.
fn oscillator_with_inputs() -> solve::SolveModel {
    let mut model = harmonic_oscillator();
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.input_scalar_names = vec!["v".to_string(), "u".to_string()].into();
    model.parameters = vec![3.0, 5.0].into();
    refresh_owned(model)
}

fn session(component: &mut MeRetainedComponent) -> MeSimulationSession<'_, 'static> {
    component
        .lease(options(Some(1.0)))
        .expect("the sole lease")
        .into_session(Some(Box::new(HermiteStepIntegrator::new(
            2,
            SamplerQuality::Native,
        ))))
        .expect("a state-carrying component admits a state-carrying plugin")
}

fn bits(values: &[Option<f64>]) -> Vec<Option<u64>> {
    values.iter().map(|value| value.map(f64::to_bits)).collect()
}

/// The selected read of `names`, checked entry by entry against the keyed map.
fn selected_like_visible(
    session: &MeSimulationSession<'_, '_>,
    names: &[&str],
) -> Vec<Option<f64>> {
    let visible = session.visible_values().expect("the keyed map reads");
    let selected = session
        .selected_values(names)
        .expect("the selected read reads");
    let keyed: Vec<_> = names
        .iter()
        .map(|name| visible.get(*name).copied())
        .collect();
    assert_eq!(bits(&selected), bits(&keyed), "{names:?}");
    selected
}

#[test]
fn a_selected_read_equals_the_keyed_lookup_bit_for_bit() {
    let model = oscillator_with_inputs();
    let mut component = retained(&model);
    let mut session = session(&mut component);
    let mut cursor = MeOutputCursor::empty();
    let names = session.output_names().unwrap();
    assert_eq!(names, ["x", "v"]);

    // The index is built at the first point and reused at every later one.
    for time in [0.0, 0.25, 0.4] {
        session
            .advance_to(time, &mut cursor)
            .expect("a healthy plugin advances");
        let selected = selected_like_visible(&session, &["x", "v"]);
        let outputs: Vec<_> = session
            .output_values()
            .unwrap()
            .into_iter()
            .map(Some)
            .collect();
        assert_eq!(bits(&selected), bits(&outputs), "declared order at {time}");
        assert_eq!(
            bits(&session.selected_values(&names).unwrap()),
            bits(&selected),
            "owned names read like borrowed ones"
        );
    }
}

#[test]
fn an_applied_input_overrides_its_output_and_one_without_an_output_is_returned() {
    let model = oscillator_with_inputs();
    let mut component = retained(&model);
    let mut session = session(&mut component);
    let mut cursor = MeOutputCursor::empty();
    session.advance_to(0.25, &mut cursor).unwrap();
    assert_eq!(
        bits(&selected_like_visible(&session, &["u"])),
        [None],
        "an input the host has not applied is no visible value"
    );

    session
        .set_inputs(&[("v", 7.5), ("u", -0.0)])
        .expect("both inputs are declared");
    let velocity = session.output_values().unwrap()[1];
    assert_ne!(velocity.to_bits(), 7.5_f64.to_bits());
    let selected = selected_like_visible(&session, &["v", "u", "x"]);
    assert_eq!(
        bits(&selected[..2]),
        [Some(7.5_f64.to_bits()), Some((-0.0_f64).to_bits())]
    );
}

#[test]
fn unknown_names_are_none_and_repeats_keep_request_order() {
    let model = oscillator_with_inputs();
    let mut component = retained(&model);
    let mut session = session(&mut component);
    let mut cursor = MeOutputCursor::empty();
    session.advance_to(0.25, &mut cursor).unwrap();
    session.set_input("u", 2.0).unwrap();
    let outputs = session.output_values().unwrap();
    let (x, v) = (Some(outputs[0].to_bits()), Some(outputs[1].to_bits()));
    let u = Some(2.0_f64.to_bits());

    let selected = selected_like_visible(&session, &["v", "missing", "x", "u", "", "x", "v", "u"]);
    assert_eq!(bits(&selected), [v, None, x, u, None, x, v, u]);
    assert!(selected_like_visible(&session, &[]).is_empty());
}

#[test]
fn a_selected_read_is_still_correct_after_reset() {
    let model = oscillator_with_inputs();
    let mut component = retained(&model);
    let mut session = session(&mut component);
    let mut cursor = MeOutputCursor::empty();
    let names = ["x", "v", "u", "missing", "x"];
    let initial = selected_like_visible(&session, &names);
    session.advance_to(0.25, &mut cursor).unwrap();
    session.set_inputs(&[("v", 7.5), ("u", 2.0)]).unwrap();
    assert_ne!(
        bits(&selected_like_visible(&session, &names)),
        bits(&initial)
    );

    session
        .reset(0.0)
        .expect("the restart rebuilds the lifecycle");
    let restarted = selected_like_visible(&session, &names);
    assert_eq!(
        bits(&restarted),
        bits(&initial),
        "the restart discarded the applied inputs and kept the output positions"
    );
    assert_eq!(bits(&restarted[2..4]), [None, None]);

    session.advance_to(0.25, &mut cursor).unwrap();
    session.set_input("v", -1.5).unwrap();
    let selected = selected_like_visible(&session, &names);
    assert_eq!(selected[1].map(f64::to_bits), Some((-1.5_f64).to_bits()));
}

#[test]
fn a_terminated_session_refuses_a_selected_read_like_the_keyed_one() {
    let model = oscillator_with_inputs();
    let mut component = retained(&model);
    let mut session = session(&mut component);
    selected_like_visible(&session, &["x"]);
    session
        .terminate()
        .expect("the session terminates its component");

    let keyed = session.visible_values().expect_err("no getter is legal");
    let selected = session
        .selected_values(&["x"])
        .expect_err("no getter is legal");
    assert_eq!(selected.to_string(), keyed.to_string());
}
