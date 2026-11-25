//! The `rust-fixed-ode` event runtime compiled directly against a
//! hand-written model: `x' = −x` from `x(0) = 1`, a static time event at
//! t = 0.5, and one searched root `x − 0.5`, which crosses at t = ln 2.

#[allow(
    dead_code,
    unreachable_pub,
    clippy::all,
    clippy::too_many_lines,
    clippy::excessive_nesting,
    clippy::too_many_arguments
)]
mod model {
    pub type Scalar = f64;
    pub const Y_LEN: usize = 1;
    pub const P_LEN: usize = 1;
    pub const DERIVATIVE_LEN: usize = 1;
    pub type State = [Scalar; Y_LEN];
    pub type Parameters = [Scalar; P_LEN];
    pub type Derivative = [Scalar; DERIVATIVE_LEN];

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct SolveError;

    pub fn derivative_rhs_into(
        _time: Scalar,
        y: &State,
        _p: &Parameters,
        out: &mut Derivative,
    ) -> Result<(), SolveError> {
        out[0] = -y[0];
        Ok(())
    }

    pub fn refresh_algebraics(
        _time: Scalar,
        _y: &mut State,
        _p: &Parameters,
    ) -> Result<(), SolveError> {
        Ok(())
    }

    pub const TIME_EVENTS: &[f64] = &[0.5];
    const CLOCKS: &[Clock] = &[];
    const TIME_MATCH_TOLERANCE: f64 = 1e-12;

    pub const N_ROOTS: usize = 1;
    const ROOT_SLOTS: usize = 1;
    const ROOT_ZERO: [i8; ROOT_SLOTS] = [0];
    const ROOT_MEMORY: [Option<usize>; ROOT_SLOTS] = [None];
    const ROOT_ALGEBRAIC: [bool; ROOT_SLOTS] = [false];
    const INDICATORS: usize = 1;
    const INDICATOR_SLOTS: usize = 1;
    const INDICATOR_ROOT: [usize; INDICATOR_SLOTS] = [0];

    pub fn eval_roots(
        _time: Scalar,
        y: &State,
        _p: &Parameters,
        out: &mut [f64; ROOT_SLOTS],
    ) -> Result<(), SolveError> {
        out[0] = y[0] - 0.5;
        Ok(())
    }

    fn no_rows(
        _time: Scalar,
        _y: &State,
        _p: &Parameters,
        _out: &mut [f64],
    ) -> Result<(), SolveError> {
        Ok(())
    }
    use self::{
        no_rows as action_values, no_rows as discrete_values, no_rows as guarded_values,
        no_rows as post_commit_values, no_rows as pulse_values, no_rows as runtime_values,
    };

    const ROW_SLOTS: usize = 1;
    const DISCRETE_TARGETS: &[usize] = &[];
    const DISCRETE_MEMORY_ROW: &[bool] = &[];
    const DISCRETE_OBSERVED_ROW: &[bool] = &[];
    const OBSERVATION_READS_Y: bool = false;
    const PULSE_ROWS: &[usize] = &[];
    const GUARDED_TARGETS: &[Slot] = &[];
    const GUARDED_CLOCK: &[Option<usize>] = &[];
    const RUNTIME_TARGETS: &[Slot] = &[];
    const POST_COMMIT_TARGETS: &[Slot] = &[];
    const PRE_LANES: &[PreLane] = &[];
    const ITERATION_LANES: &[usize] = &[];
    const PRE_LANE_SLOTS: usize = 1;
    const INITIAL_EVENT_P: Option<usize> = None;
    const ACTIONS: &[ActionKind] = &[];
    const ACTION_SLOTS: usize = 1;
    const CONDITION_MEMORIES: &[usize] = &[];
    const DISCRETE_INPUTS: &[(usize, usize)] = &[];
    const INPUT_SLOTS: usize = 1;

    const FIXED_POINT_CAP: usize = 32;
    const EVENT_PASS: &[EventStep] = &[
        EventStep::RelationOverrides,
        EventStep::RuntimeAssignments,
        EventStep::AlgebraicProjection,
        EventStep::RuntimeAssignments,
        EventStep::RelationRefresh,
        EventStep::RelationSettle,
    ];
    const RELATION_PASS: &[RelationStep] = &[
        RelationStep::DiscreteSettle,
        RelationStep::SettledIfRelationsUnchanged,
        RelationStep::AlgebraicProjection,
        RelationStep::RuntimeAssignments,
    ];
    const SETTLE_PASS: &[SettleStep] = &[
        SettleStep::DiscreteOwners,
        SettleStep::RuntimeAssignments,
        SettleStep::SettledIfUnchanged,
        SettleStep::AlgebraicProjection,
        SettleStep::RuntimeAssignments,
    ];
    const SURFACE_WINDOW: usize = 8;
    const SURFACE_SLOTS: usize = 8;
    const MODE_LIMIT: usize = 4;
    const MODE_SLOTS: usize = 4;
    const NEIGHBOR_OFFSET: &[usize] = &[0, 1];
    const NEIGHBOR: &[usize] = &[0];

    include!("../src/templates/rust-fixed-ode/events.rs");
}

use model::{EventLocation, EventSettings, Method, Simulator};

/// Events per step of ten 0.1 s RK4 steps, and `x` at the end.
fn run(location: EventLocation) -> (Vec<u32>, f64) {
    let mut sim = Simulator::new(
        Method::Rk4,
        location,
        EventSettings::default(),
        0.0,
        [1.0],
        [0.0],
    );
    sim.initialize().unwrap();
    let events = (0..10).map(|_| sim.do_step(0.1).unwrap()).collect();
    (events, sim.y()[0])
}

/// The step that ends at t = 0.5 applies the time event; the step across
/// t = ln 2 applies the state event; no other step applies one.
#[test]
fn time_and_state_events_land_in_their_steps() {
    for location in [
        EventLocation::StepEnd,
        EventLocation::Secant,
        EventLocation::Bracketed,
    ] {
        let (events, _) = run(location);
        assert_eq!(events, [0, 0, 0, 0, 1, 0, 1, 0, 0, 0], "{location:?}");
    }
}

/// Events that change nothing leave the decay intact: `x(1) = e^(−1)` to
/// RK4 accuracy, whether the state event splits its step or not.
#[test]
fn events_that_change_nothing_leave_the_decay_intact() {
    for location in [
        EventLocation::StepEnd,
        EventLocation::Secant,
        EventLocation::Bracketed,
    ] {
        let (_, x) = run(location);
        assert!((x - (-1.0f64).exp()).abs() < 1e-6, "{location:?}: x = {x}");
    }
}

/// `max_events_per_step` caps state events only: with a cap of zero the time
/// event at t = 0.5 still applies, and the step across t = ln 2 is refused.
#[test]
fn cap_state_events_per_step_from_the_settings() {
    let settings = EventSettings {
        max_events_per_step: 0,
        ..EventSettings::default()
    };
    let mut sim = Simulator::new(
        Method::Rk4,
        EventLocation::Bracketed,
        settings,
        0.0,
        [1.0],
        [0.0],
    );
    sim.initialize().unwrap();
    let steps: Vec<_> = (0..7).map(|_| sim.do_step(0.1)).collect();
    assert_eq!(
        steps,
        [
            Ok(0),
            Ok(0),
            Ok(0),
            Ok(0),
            Ok(1),
            Ok(0),
            Err(model::SimError::TooManyEvents)
        ]
    );
}
