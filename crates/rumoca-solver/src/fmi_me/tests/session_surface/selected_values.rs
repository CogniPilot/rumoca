//! A selected read answers exactly what the keyed visible map holds.

use super::*;
use crate::fmi_me::session::{MeOutputCursor, MeSimulationSession};

/// Every declared output: two direct state reads, one expression of the
/// states issued twice (so the second is a copy of the first row's group),
/// and one expression of an input and the states.
const OUTPUTS: [&str; 5] = ["x", "v", "energy", "energy_again", "drive"];

/// The oscillator with two declared inputs and expression outputs: input `v`
/// shares its name with the velocity output, input `u` names no output but
/// feeds `drive = u * x + v`; `energy = 4 * x * x + v * v`.
fn oscillator_with_inputs() -> solve::SolveModel {
    use solve::BinaryOp::{Add, Mul};
    use solve::LinearOp::{Binary, Const, LoadP, LoadY, StoreOutput};

    let binary = |dst, op, lhs, rhs| Binary { dst, op, lhs, rhs };
    let state = |index| vec![LoadY { dst: 0, index }, StoreOutput { src: 0 }];
    let energy = vec![
        LoadY { dst: 0, index: 0 },
        LoadY { dst: 1, index: 1 },
        binary(2, Mul, 0, 0),
        Const { dst: 3, value: 4.0 },
        binary(4, Mul, 2, 3),
        binary(5, Mul, 1, 1),
        binary(6, Add, 4, 5),
        StoreOutput { src: 6 },
    ];
    let drive = vec![
        LoadP { dst: 0, index: 1 },
        LoadY { dst: 1, index: 0 },
        binary(2, Mul, 0, 1),
        LoadY { dst: 3, index: 1 },
        binary(4, Add, 2, 3),
        StoreOutput { src: 4 },
    ];
    let mut model = harmonic_oscillator();
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.input_scalar_names = vec!["v".to_string(), "u".to_string()].into();
    model.parameters = vec![3.0, 5.0].into();
    refresh_owned(crate::fixture_layout::with_visible_expressions(
        model,
        OUTPUTS.map(String::from).to_vec(),
        block(
            vec![state(0), state(1), energy.clone(), energy, drive],
            "fmi_me_selected_values.mo",
        ),
        Vec::new(),
    ))
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
fn a_selected_read_equals_the_full_read_bit_for_bit() {
    let model = oscillator_with_inputs();
    let mut component = retained(&model);
    let mut session = session(&mut component);
    let mut cursor = MeOutputCursor::empty();
    let names = session.output_names().unwrap();
    assert_eq!(names, OUTPUTS);

    for time in [0.0, 0.25, 0.4] {
        session
            .advance_to(time, &mut cursor)
            .expect("a healthy plugin advances");
        let selected = selected_like_visible(&session, &OUTPUTS);
        let outputs = session.output_values().unwrap();
        let full: Vec<_> = outputs.iter().copied().map(Some).collect();
        assert_eq!(bits(&selected), bits(&full), "declared order at {time}");
        assert_eq!(
            outputs[2].to_bits(),
            (outputs[0] * outputs[0] * 4.0 + outputs[1] * outputs[1]).to_bits(),
            "the expression row is evaluated, not read from storage"
        );
        assert_eq!(
            bits(&session.selected_values(&names).unwrap()),
            bits(&selected),
            "owned names read like borrowed ones"
        );
        // Each output alone, and the copy without its group's source row.
        for name in OUTPUTS {
            selected_like_visible(&session, &[name]);
        }
        selected_like_visible(&session, &["energy_again", "x"]);
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
    let before = selected_like_visible(&session, &["drive", "energy"]);

    session
        .set_inputs(&[("v", 7.5), ("u", -0.0)])
        .expect("both inputs are declared");
    let velocity = session.output_values().unwrap()[1];
    assert_ne!(velocity.to_bits(), 7.5_f64.to_bits());
    let selected = selected_like_visible(&session, &["v", "u", "x", "drive", "energy"]);
    assert_eq!(
        bits(&selected[..2]),
        [Some(7.5_f64.to_bits()), Some((-0.0_f64).to_bits())]
    );
    assert_ne!(
        bits(&selected[3..4]),
        bits(&before[..1]),
        "the expression row reads the changed input"
    );
    assert_eq!(bits(&selected[4..]), bits(&before[1..]));
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
    let [x, v, energy, _, drive] = [0, 1, 2, 3, 4].map(|output| Some(outputs[output].to_bits()));
    let u = Some(2.0_f64.to_bits());

    let selected = selected_like_visible(
        &session,
        &[
            "v",
            "missing",
            "energy_again",
            "x",
            "u",
            "",
            "drive",
            "energy",
            "x",
            "energy_again",
            "v",
            "u",
        ],
    );
    assert_eq!(
        bits(&selected),
        [v, None, energy, x, u, None, drive, energy, x, energy, v, u]
    );
    assert!(selected_like_visible(&session, &[]).is_empty());
}

#[test]
fn a_selected_read_is_still_correct_after_reset() {
    let model = oscillator_with_inputs();
    let mut component = retained(&model);
    let mut session = session(&mut component);
    let mut cursor = MeOutputCursor::empty();
    let names = [
        "x",
        "v",
        "u",
        "missing",
        "drive",
        "energy_again",
        "x",
        "energy",
    ];
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

/// A selected read reports the violated warning sites like the full read it
/// stands in for, and a reported site stays reported.
#[test]
fn a_selected_read_reports_the_warnings_of_its_observation() {
    // The warning condition `4 * time` is violated only once time has passed,
    // so initialization reports nothing.
    let mut model = with_warning(oscillator_with_inputs());
    model.problem.events.action_conditions = block(
        vec![vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::Const { dst: 1, value: 4.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        "fmi_me_selected_warning_condition.mo",
    );
    let model = refresh_owned(model);
    let mut component = retained(&model);
    let untraced = MeSessionOptions::new(MeSessionOptionsInput {
        start_time: 0.0,
        stop_time: Some(1.0),
        relative_tolerance: 1.0e-8,
        absolute_tolerance: 1.0e-8,
        output_interval: 0.1,
        root_scan_resolution: 0.05,
        root_location_tolerance: 1.0e-10,
        max_wall_seconds: None,
        records_trace: false,
    })
    .expect("the fixture options are admissible");
    let mut session = component
        .lease(untraced)
        .expect("the sole lease")
        .into_session(Some(Box::new(HermiteStepIntegrator::new(
            2,
            SamplerQuality::Native,
        ))))
        .expect("a state-carrying component admits a state-carrying plugin");
    session
        .advance_to(0.25, &mut MeOutputCursor::empty())
        .expect("a healthy plugin advances");
    assert_eq!(
        session.diagnostics_since(0).1,
        0,
        "an untraced advance observes no output"
    );

    session
        .selected_values(&["x"])
        .expect("the selected read reads");
    let (reported, cursor) = session.diagnostics_since(0);
    assert_eq!(cursor, 1);
    assert_eq!(reported[0].code, WARNING_ASSERTION_CODE);
    session
        .selected_values(&["energy", "x"])
        .expect("a reported site stays reported");
    assert_eq!(session.diagnostics_since(cursor), (Vec::new(), 1));
}
