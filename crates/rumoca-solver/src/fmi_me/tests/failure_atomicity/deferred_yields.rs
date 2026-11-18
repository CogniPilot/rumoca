//! A deferred live yield (issue #513) is the caller's logical point: refused
//! writes, a new output schedule, an expired budget and termination all treat
//! it as the caller's coordinate, never as the step start behind it, and only
//! a real discontinuity (an admitted input change, termination) cuts the step.

use super::*;

/// Open live options (no stop time), so an empty cursor defers its yields.
fn live_session(
    retained: &mut me::session::MeRetainedComponent,
    plugin: FaultyPlugin,
    records_trace: bool,
) -> me::session::MeSimulationSession<'_, 'static> {
    use me::session::{MeSessionOptions, MeSessionOptionsInput};

    let options = MeSessionOptions::new(MeSessionOptionsInput {
        start_time: 0.0,
        stop_time: None,
        relative_tolerance: 1.0e-8,
        absolute_tolerance: 1.0e-8,
        output_interval: 0.1,
        root_scan_resolution: 0.04,
        root_location_tolerance: 1.0e-10,
        max_wall_seconds: None,
        records_trace,
    })
    .expect("open live options are admissible");
    retained
        .lease(options)
        .expect("the sole lease is granted")
        .into_session(Some(Box::new(plugin)))
        .expect("a one-state component admits a one-state plugin")
}

/// Advance to `yield_time` and prove it is served from inside a natural step
/// whose accepted point still stands strictly before it.
fn defer_at(session: &mut me::session::MeSimulationSession<'_, 'static>, yield_time: f64) {
    let mut cursor = me::session::MeOutputCursor::empty();
    session
        .advance_to(yield_time, &mut cursor)
        .expect("the live yield is served");
    assert_eq!(session.time().to_bits(), yield_time.to_bits());
    let (accepted, _) = session.verification_session_point();
    assert!(
        f64::from_bits(accepted) < yield_time,
        "the yield is deferred inside a natural step, not a step end"
    );
}

#[test]
fn a_refused_input_batch_keeps_the_deferred_yield_and_its_step() {
    let model = branch_component_with_input();
    let mut retained = retained_branch_component(&model);
    let plugin = FaultyPlugin::new(PluginFault::Healthy);
    let initializations = Rc::clone(&plugin.initializations);
    let mut session = live_session(&mut retained, plugin, false);
    defer_at(&mut session, 0.25);
    let anchor = session.verification_session_point();
    let observed: Vec<u64> = session
        .output_values()
        .unwrap()
        .iter()
        .map(|value| value.to_bits())
        .collect();
    let restarts = initializations.get();

    for invalid in [
        vec![("u", f64::NAN)],
        vec![("u", f64::INFINITY)],
        vec![("u", f64::NEG_INFINITY)],
        vec![("u", 9.0), ("u", f64::NAN)],
        vec![("u", 9.0), ("missing", 5.0)],
    ] {
        assert!(session.set_inputs(&invalid).is_err());
        assert_eq!(session.time().to_bits(), 0.25_f64.to_bits());
        assert_eq!(session.verification_session_point(), anchor);
        assert_eq!(
            session.verification_component_point(),
            session.verification_session_point()
        );
        let values: Vec<u64> = session
            .output_values()
            .expect("a refused batch leaves the session live")
            .iter()
            .map(|value| value.to_bits())
            .collect();
        assert_eq!(values, observed);
        assert_eq!(initializations.get(), restarts, "no history was reset");
    }

    // The same pending step still serves a later yield inside it.
    let later: f64 = 0.250_001;
    defer_at(&mut session, later);
    assert_eq!(session.verification_session_point(), anchor);
    assert_eq!(initializations.get(), restarts);

    // Control: an admitted change truncates exactly at the visible yield.
    session.set_input("u", 2.0).unwrap();
    assert!(initializations.get() > restarts);
    let (accepted, _) = session.verification_session_point();
    assert_eq!(accepted, later.to_bits());
    assert_eq!(session.time().to_bits(), later.to_bits());
}

#[test]
fn a_new_output_schedule_is_served_from_the_deferred_step_without_a_boundary() {
    let model = branch_component_with_input();
    let mut observed_retained = retained_branch_component(&model);
    let observed_plugin = FaultyPlugin::new(PluginFault::Healthy);
    let observed_restarts = Rc::clone(&observed_plugin.initializations);
    let mut observed = live_session(&mut observed_retained, observed_plugin, true);
    let mut control_retained = retained_branch_component(&model);
    let control_plugin = FaultyPlugin::new(PluginFault::Healthy);
    let control_restarts = Rc::clone(&control_plugin.initializations);
    let mut control = live_session(&mut control_retained, control_plugin, true);
    defer_at(&mut observed, 0.25);
    defer_at(&mut control, 0.25);

    // 0.3 lies inside the pending step, 0.5 coincides with the yield, 0.9 is later.
    let mut cursor =
        me::session::MeOutputCursor::new(vec![0.3, 0.5, 0.9]).expect("the schedule is sorted");
    let outcome = observed
        .advance_to(0.5, &mut cursor)
        .expect("the schedule is served from the retained extension");
    assert!(matches!(outcome, me::MeAdvanceOutcome::Yielded));
    assert_eq!(cursor.peek(), Some(0.9), "0.3 and 0.5 were consumed");
    let mut empty = me::session::MeOutputCursor::empty();
    control.advance_to(0.5, &mut empty).unwrap();

    // Same held-input trajectory and history: no truncation, no plugin reset.
    assert_eq!(observed.time().to_bits(), control.time().to_bits());
    assert_eq!(
        observed.verification_session_point(),
        control.verification_session_point()
    );
    assert_eq!(observed_restarts.get(), control_restarts.get());
    let bits = |values: Vec<f64>| values.iter().map(|v| v.to_bits()).collect::<Vec<_>>();
    assert_eq!(
        bits(observed.output_values().unwrap()),
        bits(control.output_values().unwrap())
    );

    // A schedule entry behind the visible yield is refused before anything moves.
    let mut behind = me::session::MeOutputCursor::new(vec![0.4]).expect("one coordinate");
    assert!(observed.advance_to(0.6, &mut behind).is_err());
    assert_eq!(observed.time().to_bits(), 0.5_f64.to_bits());
    assert_eq!(
        observed.verification_session_point(),
        control.verification_session_point()
    );

    let result = observed.finish();
    for requested in [0.3_f64, 0.5] {
        assert!(
            result
                .times
                .iter()
                .any(|time| time.to_bits() == requested.to_bits()),
            "the requested observation {requested} was published: {:?}",
            result.times
        );
    }
}

#[test]
fn an_expired_budget_refuses_a_deferred_yield_before_serving_it() {
    let model = branch_component_with_input();
    let mut retained = retained_branch_component(&model);
    let mut session = live_session(
        &mut retained,
        FaultyPlugin::new(PluginFault::Healthy),
        false,
    );
    defer_at(&mut session, 0.25);
    let anchor = session.verification_session_point();
    session.verification_expire_budget();
    let mut cursor = me::session::MeOutputCursor::empty();
    for target in [0.25, 0.5] {
        let failure = session
            .advance_to(target, &mut cursor)
            .expect_err("an expired budget refuses even a served yield");
        assert!(failure.is_timeout(), "{failure:?}");
        assert_eq!(session.time().to_bits(), 0.25_f64.to_bits());
        assert_eq!(session.verification_session_point(), anchor);
    }

    // Control: with nothing pending the master loop refuses the same way.
    let mut fresh_retained = retained_branch_component(&model);
    let mut fresh = live_session(
        &mut fresh_retained,
        FaultyPlugin::new(PluginFault::Healthy),
        false,
    );
    fresh.verification_expire_budget();
    let failure = fresh
        .advance_to(0.25, &mut cursor)
        .expect_err("the loop checks the budget");
    assert!(failure.is_timeout(), "{failure:?}");
}

#[test]
fn termination_at_a_deferred_yield_stands_on_that_yield() {
    let model = branch_component_with_input();
    let mut retained = retained_branch_component(&model);
    let mut session = live_session(
        &mut retained,
        FaultyPlugin::new(PluginFault::Healthy),
        false,
    );
    defer_at(&mut session, 0.25);
    session.terminate().expect("the session terminates");
    let (accepted, _) = session.verification_session_point();
    assert_eq!(accepted, 0.25_f64.to_bits());
    assert_eq!(
        session.verification_component_point(),
        session.verification_session_point()
    );
    assert!(session.verification_component_is_terminated());
}

/// The input component with every kind of output: `x`, `a`, the expression
/// `(x + a) * u` issued twice, and the input `u` read straight from storage.
fn branch_component_with_expression_outputs() -> solve::SolveModel {
    use solve::BinaryOp::{Add, Mul};
    use solve::LinearOp::{Binary, LoadP, LoadY, StoreOutput};

    let state = |index| vec![LoadY { dst: 0, index }, StoreOutput { src: 0 }];
    let gain = vec![
        LoadY { dst: 0, index: 0 },
        LoadY { dst: 1, index: 1 },
        Binary {
            dst: 2,
            op: Add,
            lhs: 0,
            rhs: 1,
        },
        LoadP { dst: 3, index: 0 },
        Binary {
            dst: 4,
            op: Mul,
            lhs: 2,
            rhs: 3,
        },
        StoreOutput { src: 4 },
    ];
    let input = vec![LoadP { dst: 0, index: 0 }, StoreOutput { src: 0 }];
    refresh_owned(crate::fixture_layout::with_visible_expressions(
        branch_component_with_input(),
        ["x", "a", "gain", "gain_again", "u"]
            .map(String::from)
            .to_vec(),
        block(
            vec![state(0), state(1), gain.clone(), gain, input],
            "fmi_me_deferred_selected_read.mo",
        ),
        Vec::new(),
    ))
}

/// A selected read at a deferred yield observes the caller's coordinate like
/// the full read, direct and expression outputs alike, and moves nothing.
#[test]
fn a_selected_read_at_a_deferred_yield_equals_the_full_read() {
    let model = branch_component_with_expression_outputs();
    let mut retained = retained_branch_component(&model);
    let mut session = live_session(
        &mut retained,
        FaultyPlugin::new(PluginFault::Healthy),
        false,
    );
    let names = ["gain_again", "x", "missing", "u", "gain", "a", "x"];
    let agrees = |session: &me::session::MeSimulationSession<'_, 'static>| {
        let full = session.output_values().expect("the full read observes");
        let output = |index: usize| Some(full[index].to_bits());
        let selected: Vec<_> = session
            .selected_values(&names)
            .expect("the selected read observes")
            .iter()
            .map(|value| value.map(f64::to_bits))
            .collect();
        assert_eq!(
            selected,
            [
                output(3),
                output(0),
                None,
                output(4),
                output(2),
                output(1),
                output(0)
            ]
        );
        assert_eq!(full[2].to_bits(), ((full[0] + full[1]) * full[4]).to_bits());
        full
    };

    let at_start = agrees(&session);
    defer_at(&mut session, 0.25);
    let anchor = session.verification_session_point();
    let at_yield = agrees(&session);
    assert_ne!(
        at_yield[2].to_bits(),
        at_start[2].to_bits(),
        "the expression is evaluated at the yield, not at the accepted point"
    );
    assert_eq!(session.time().to_bits(), 0.25_f64.to_bits());
    assert_eq!(session.verification_session_point(), anchor);
    assert_eq!(
        session.verification_component_point(),
        session.verification_session_point(),
        "the excursion returned the component to the accepted point"
    );

    // An admitted input change makes the yield the accepted point; the
    // expression row reads the new input there.
    session.set_input("u", 2.0).unwrap();
    let after_input = agrees(&session);
    assert_eq!(after_input[4].to_bits(), 2.0_f64.to_bits());
    assert_ne!(after_input[2].to_bits(), at_yield[2].to_bits());
}
