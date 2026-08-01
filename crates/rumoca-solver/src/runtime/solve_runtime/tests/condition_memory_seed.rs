//! MLS §8.3.5.1 activation buffers seeded at the initialization instant.
//!
//! These drive [`SolveRuntime::seed_condition_memory_for_initialization`]
//! directly rather than through a compiled model, because the two properties
//! that matter are properties *of the seed* and are invisible at the trace
//! level once an event has run over them: which parameter snapshot the seed
//! evaluates against, and what `initial()` reads while it does.

use super::*;

/// `p` layout shared by the fixtures below.
const CURRENT: usize = 0;
const PRE_SLOT: usize = 1;
const THRESHOLD_BUFFER: usize = 2;
const INITIAL_FLAG: usize = 3;
const INITIAL_BUFFER: usize = 4;

/// A condition-memory row reading `p[source] > 2`.
fn threshold_row(source: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP {
            dst: 0,
            index: source,
        },
        solve::LinearOp::Const { dst: 1, value: 2.0 },
        solve::LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

/// A condition-memory row reading the `initial()` flag.
fn initial_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP {
            dst: 0,
            index: INITIAL_FLAG,
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

/// Two activation buffers: `pre(s) > 2` and `initial()`.
///
/// `s` has settled to 5 and its lowered `pre` slot still holds `0`, which is the
/// state every backend is in when the seed runs — `commit_pre_params_after_event`
/// advances the `pre` slots only *after* the initial event.
fn seed_fixture() -> solve::SolveModel {
    solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                parameter_count: 5,
                compiled_parameter_len: 5,
                initial_event_parameter_index: Some(INITIAL_FLAG),
                pre_param_bindings: vec![solve::PreParamBinding {
                    dest_p_index: PRE_SLOT,
                    source: solve::PreParamSource::P { index: CURRENT },
                    clock_schedule: None,
                }],
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                condition_memory_parameter_indices: vec![THRESHOLD_BUFFER, INITIAL_BUFFER],
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                rhs: spanned_block(
                    vec![threshold_row(PRE_SLOT), initial_row()],
                    "condition_memory_seed.mo",
                ),
                update_targets: vec![
                    solve::scalar_slot_p(THRESHOLD_BUFFER),
                    solve::scalar_slot_p(INITIAL_BUFFER),
                ],
                row_roles: vec![
                    solve::DiscreteRowRole::ConditionMemory,
                    solve::DiscreteRowRole::ConditionMemory,
                ],
                pre_modes: vec![
                    solve::DiscreteEventPreMode::FollowCurrent,
                    solve::DiscreteEventPreMode::FollowCurrent,
                ],
                observation_refresh: vec![false, false],
                clock_owners: vec![None, None],
                ..Default::default()
            },
            ..Default::default()
        },
        //     s      pre(s)  buf(pre(s)>2)  initial()  buf(initial())
        parameters: vec![5.0, 0.0, 0.0, 1.0, 0.0],
        ..Default::default()
    }
}

/// MLS §8.6: the seed evaluates against `pre(v) = v`, not against a `pre` slot
/// that initialization has not committed yet.
///
/// `pre(s) > 2` with `s = 5` is true at the initialization instant, so §8.3.5.1
/// starts its buffer true and the activation has no edge at the initial event.
/// Reading the raw `pre` slot instead sees `0 > 2`, seeds the buffer false, and
/// manufactures exactly the edge §8.6 says is not there.
#[test]
fn the_seed_reads_pre_variables_as_their_own_current_values() {
    let model = seed_fixture();
    let runtime = SolveRuntime::new(&model).expect("seed fixture should prepare");
    let mut p = model.parameters.clone();

    let seeded = runtime
        .seed_condition_memory_for_initialization(&mut [], &mut p, 0.0, 1.0e-9)
        .expect("condition memory should seed");

    assert_eq!(
        p[THRESHOLD_BUFFER], 1.0,
        "`pre(s) > 2` with s = 5 is true at the initialization instant (MLS §8.6 \
         v = pre(v)), so its activation buffer must start true"
    );
    assert!(
        seeded
            .iter()
            .any(|entry| entry.index == THRESHOLD_BUFFER && entry.value == 1.0),
        "the reported seed must name the slot it wrote, so a caller's event-entry \
         snapshot can carry the same value"
    );
}

/// MLS §8.6 enables a when-clause during initialization *only* with `initial()`,
/// so `initial()` is the one activation whose buffer must seed false.
///
/// The flag is set while the seed runs — the backends raise it before
/// initialization settles — so the seed has to clear it explicitly rather than
/// read `p` verbatim.
#[test]
fn the_seed_leaves_the_initial_activation_its_edge() {
    let model = seed_fixture();
    let runtime = SolveRuntime::new(&model).expect("seed fixture should prepare");
    let mut p = model.parameters.clone();
    assert_eq!(p[INITIAL_FLAG], 1.0, "the fixture runs with initial() true");

    runtime
        .seed_condition_memory_for_initialization(&mut [], &mut p, 0.0, 1.0e-9)
        .expect("condition memory should seed");

    assert_eq!(
        p[INITIAL_BUFFER], 0.0,
        "`initial()` is false everywhere except the initial event, so its buffer \
         seeds false and the initial event is its rising edge"
    );
    assert_eq!(
        p[INITIAL_FLAG], 1.0,
        "and clearing the flag for the seed must not clear it for the event"
    );
}

/// A model with no activation buffers is left alone.
///
/// This is the guard the two `visibility.rs` fixtures happen to take, so it is
/// asserted here rather than assumed there: those models declare no
/// condition-memory slots at all, and would pass this file's other assertions
/// vacuously.
#[test]
fn a_model_without_activation_buffers_seeds_nothing() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![7.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("bare fixture should prepare");
    let mut p = model.parameters.clone();

    let seeded = runtime
        .seed_condition_memory_for_initialization(&mut [], &mut p, 0.0, 1.0e-9)
        .expect("a model with no buffers must not fail");

    assert!(seeded.is_empty());
    assert_eq!(p, vec![7.0], "and must not touch any parameter");
}
