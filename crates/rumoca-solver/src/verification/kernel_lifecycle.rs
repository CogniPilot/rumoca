//! Harness B: the FMI 3 ME component lifecycle, driven over bounded generated
//! operation sequences against a one-state model (`dx/dt = x`).
//!
//! Three of the five properties below are invariants. The other two —
//! [`property_set_time_is_not_monotonic`] and
//! [`property_the_event_boundary_clamps_time_upwards`] — are *pins*: they
//! record what the component does today, and their doc comments say plainly
//! that this is current behaviour and not an enforced invariant. Between them
//! they say why "the component's time is monotonic" is false: the only clamp
//! anywhere is on the event-boundary path, and `set_time` has none.

use super::model_fixture::{divergent_initialization_model, single_state_model};
use crate::fmi_me::{
    MeError, MeEventCause, MeEventEntry, MeInstanceConfig, MeModelSource, MeStepCompletion, MeTime,
    ModelExchangeKernel, SolveMeKernel,
};

const START_TIME: f64 = 0.0;
const STOP_TIME: f64 = 1.0;
const TOLERANCE: f64 = 1.0e-8;

/// The smallest gap the ordered-time generators leave between `earlier` and
/// `later`.
///
/// Both time pins need a strictly ordered pair. The gap that produced it used to
/// be one `f64::EPSILON`, which is an absolute constant and therefore not a gap
/// at all: it is smaller than the ULP of any horizon past `2.0`, so raising
/// `STOP_TIME` would silently start admitting `later == earlier` and fail the
/// strict-ordering pins for a reason that has nothing to do with the component.
/// At the other end it was too *large* to be safe — see the fallback driver's
/// `any_ordered_times` for the range collapse it caused at the top of the
/// horizon. An explicit gap, orders of magnitude above the ULP anywhere inside
/// `[0, 1]` and orders of magnitude below the horizon itself, has neither
/// problem. The proof assumes the same gap so that both drivers explore the same
/// pairs.
const MIN_TIME_GAP: f64 = 1.0e-9;

/// The longest generated operation sequence the harnesses explore.
const MAX_OPS: usize = 8;

/// The number of `update_discrete_states` calls a conforming host loop is
/// allowed before this harness calls the component non-terminating.
const HOST_ITERATION_CEILING: usize = 4;

/// Every `ModelExchangeKernel` entry point the harnesses drive.
///
/// `set_time`, `set_continuous_states`, `enter_event_mode` and
/// `next_event_stop` each carry one generated `f64` so a sequence can put the
/// component at an arbitrary time and state rather than only at the ones its
/// own lifecycle would produce. The generated value ranges over small finite
/// magnitudes *and* over NaN and both infinities, because nothing on the way in
/// rejects them: `set_time` returns `()` and so cannot report a rejection at
/// all, and `set_continuous_states` checks only the buffer length.
///
/// Admitted is not the same as pinned. The only property driven over non-finite
/// payloads is [`property_no_operation_sequence_panics`], and all it asserts of
/// them is that every call returns and the reported model description does not
/// move. No property here asserts what the component *holds* after a NaN or
/// infinite time: the two time pins below are driven over finite ordered pairs
/// only.
#[derive(Clone, Copy, Debug)]
enum KernelOp {
    SetTime(f64),
    SetContinuousStates(f64),
    GetContinuousStates,
    GetDerivatives,
    GetEventIndicators,
    GetNominals,
    Observe,
    ProjectContinuousStates,
    CompletedStep,
    EnterInitializationMode,
    ExitInitializationMode,
    EnterEventMode(f64),
    UpdateDiscreteStates,
    EnterContinuousTimeMode,
    NextEventStop(f64),
    FmuStateRoundTrip,
    Terminate,
}

/// The number of distinct operations [`KernelOp::from_index`] can produce.
const OP_COUNT: u8 = 17;

impl KernelOp {
    /// Decode one generated `(index, value)` pair into an operation.
    ///
    /// Both drivers go through this so the proof and the proptest explore the
    /// same operation alphabet.
    fn from_index(index: u8, value: f64) -> Self {
        match index % OP_COUNT {
            0 => Self::SetTime(value),
            1 => Self::SetContinuousStates(value),
            2 => Self::GetContinuousStates,
            3 => Self::GetDerivatives,
            4 => Self::GetEventIndicators,
            5 => Self::GetNominals,
            6 => Self::Observe,
            7 => Self::ProjectContinuousStates,
            8 => Self::CompletedStep,
            9 => Self::EnterInitializationMode,
            10 => Self::ExitInitializationMode,
            11 => Self::EnterEventMode(value),
            12 => Self::UpdateDiscreteStates,
            13 => Self::EnterContinuousTimeMode,
            14 => Self::NextEventStop(value),
            15 => Self::FmuStateRoundTrip,
            16 => Self::Terminate,
            // `index % OP_COUNT` is below `OP_COUNT`, and every value below it
            // is named above; this arm exists only because the match is over
            // `u8`. It panics rather than falling back to a real operation
            // because a fallback is invisible: while the last arm was `_ =>
            // Self::Terminate`, an alphabet raised past 17 would have folded
            // every surplus index onto `Terminate` with every property still
            // green.
            _ => unreachable!("`index % OP_COUNT` names one of the operations above"),
        }
    }
}

/// Every operation [`KernelOp::from_index`] produces, at the index it produces
/// it.
///
/// Declared with length `OP_COUNT` so that the count and the alphabet are
/// checked against each other by the compiler: lowering `OP_COUNT` without
/// deleting a row, or deleting a row without lowering `OP_COUNT`, does not
/// build. That link is the point — `OP_COUNT` is the modulus every generated
/// index passes through, so lowering it alone used to shrink the explored
/// alphabet silently, dropping whole operations out of every generated sequence
/// while all five properties stayed green.
#[cfg(test)]
const OP_ALPHABET: [KernelOp; OP_COUNT as usize] = [
    KernelOp::SetTime(0.0),
    KernelOp::SetContinuousStates(0.0),
    KernelOp::GetContinuousStates,
    KernelOp::GetDerivatives,
    KernelOp::GetEventIndicators,
    KernelOp::GetNominals,
    KernelOp::Observe,
    KernelOp::ProjectContinuousStates,
    KernelOp::CompletedStep,
    KernelOp::EnterInitializationMode,
    KernelOp::ExitInitializationMode,
    KernelOp::EnterEventMode(0.0),
    KernelOp::UpdateDiscreteStates,
    KernelOp::EnterContinuousTimeMode,
    KernelOp::NextEventStop(0.0),
    KernelOp::FmuStateRoundTrip,
    KernelOp::Terminate,
];

/// Every index below `OP_COUNT` decodes to the operation [`OP_ALPHABET`] names
/// at that position, no two indices decode to the same operation, and the
/// decode wraps at `OP_COUNT`.
///
/// Operations are compared by discriminant rather than by value: `KernelOp`
/// carries generated `f64` payloads — NaN included — and the alphabet is a claim
/// about which operation an index names, not about the payload it carries.
#[cfg(test)]
#[test]
fn every_generated_index_names_its_operation() {
    for (position, expected) in OP_ALPHABET.iter().enumerate() {
        let index = u8::try_from(position).expect("the alphabet is far shorter than u8::MAX");
        let decoded = KernelOp::from_index(index, 1.0);
        assert!(
            std::mem::discriminant(&decoded) == std::mem::discriminant(expected),
            "index {index} decoded to {decoded:?}, not the operation the \
             alphabet names at that position ({expected:?})"
        );
        assert!(
            !OP_ALPHABET[..position]
                .iter()
                .any(|earlier| std::mem::discriminant(earlier) == std::mem::discriminant(expected)),
            "{expected:?} appears twice in the alphabet, so the generated index \
             reaches fewer operations than its range suggests"
        );
    }
    // The generated index ranges over the whole `u8`, so `OP_COUNT` is a
    // modulus: index `OP_COUNT` has to be index 0 again.
    assert!(
        std::mem::discriminant(&KernelOp::from_index(OP_COUNT, 1.0))
            == std::mem::discriminant(&OP_ALPHABET[0]),
        "the alphabet has to wrap at OP_COUNT"
    );
}

fn instantiate(model: &rumoca_ir_solve::SolveModel) -> SolveMeKernel {
    SolveMeKernel::instantiate(
        MeModelSource::new(model),
        &MeInstanceConfig {
            instance_name: "solve-verification",
            tolerance: TOLERANCE,
            start_time: START_TIME,
            stop_time: STOP_TIME,
        },
    )
    .expect("the bounded fixture is a well-formed ME model")
}

/// Apply one operation and hand back whatever the component answered.
///
/// Nothing here inspects the answer: an operation issued out of lifecycle
/// order is expected to fail, and the properties are about *how* it fails.
fn apply(kernel: &mut SolveMeKernel, op: KernelOp) -> Result<(), MeError> {
    let mut values = Vec::new();
    let mut states = [0.0_f64; 1];
    match op {
        KernelOp::SetTime(time) => {
            kernel.set_time(MeTime::at(time));
            Ok(())
        }
        KernelOp::SetContinuousStates(value) => kernel.set_continuous_states(&[value]),
        KernelOp::GetContinuousStates => kernel.get_continuous_states(&mut states),
        KernelOp::GetDerivatives => kernel.get_continuous_state_derivatives(&mut values),
        KernelOp::GetEventIndicators => kernel.get_event_indicators(&mut values),
        KernelOp::GetNominals => kernel.get_nominals_of_continuous_states(&mut states),
        KernelOp::Observe => kernel.observe().map(|_| ()),
        KernelOp::ProjectContinuousStates => {
            kernel.project_continuous_states(&mut states).map(|_| ())
        }
        KernelOp::CompletedStep => kernel.completed_integrator_step(MeStepCompletion::Continuous {
            accepted_derivatives: None,
        }),
        KernelOp::EnterInitializationMode => kernel.enter_initialization_mode(),
        KernelOp::ExitInitializationMode => kernel.exit_initialization_mode(),
        KernelOp::EnterEventMode(time) => kernel.enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: time,
            horizon: STOP_TIME,
        }),
        KernelOp::UpdateDiscreteStates => kernel.update_discrete_states().map(|_| ()),
        KernelOp::EnterContinuousTimeMode => kernel.enter_continuous_time_mode(),
        KernelOp::NextEventStop(horizon) => kernel.next_event_stop(horizon).map(|_| ()),
        KernelOp::FmuStateRoundTrip => {
            let saved = kernel.fmu_state();
            kernel.reset_to_fmu_state(&saved, START_TIME, &[1.0])
        }
        KernelOp::Terminate => kernel.terminate(),
    }
}

/// Walk the component through initialization to Continuous-Time Mode.
fn run_to_continuous_time_mode(kernel: &mut SolveMeKernel) {
    kernel
        .enter_initialization_mode()
        .expect("initialization mode should be enterable from Instantiated");
    kernel
        .exit_initialization_mode()
        .expect("the one-state fixture should settle its initialization system");
    kernel
        .update_discrete_states()
        .expect("the initial event boundary should run");
    kernel
        .enter_continuous_time_mode()
        .expect("continuous-time mode should be enterable after the initial event");
}

/// # Property
///
/// A conforming FMI host's `while discreteStatesNeedUpdate` loop terminates
/// after exactly one `update_discrete_states` call, whatever operations
/// preceded it.
///
/// FMI 3.0 lets a component request further event iterations through
/// `fmi3UpdateDiscreteStates`'s `discreteStatesNeedUpdate` out-parameter, which
/// is why a host writes the call as a loop at all.
/// [`crate::fmi_me::MeDiscreteStates::discrete_states_need_update`] documents
/// rumoca's answer as always `false` — "The rumoca component runs the whole
/// discrete-state fixed point inside one call, so this is always `false`; a
/// conforming host loop still terminates correctly." This harness is what
/// makes that comment checkable.
fn property_the_host_event_iteration_terminates_after_one_call(prefix: &[KernelOp]) {
    let model = single_state_model();
    let mut kernel = instantiate(&model);
    run_to_continuous_time_mode(&mut kernel);
    for op in prefix {
        // A prefix operation may legitimately fail out of lifecycle order; the
        // property is about the host loop that follows it either way.
        let _ = apply(&mut kernel, *op);
    }
    // Arm the boundary explicitly. Without this, a prefix that left the
    // component outside event mode would make every `update_discrete_states`
    // return "called outside event mode" and the loop below would terminate
    // vacuously, proving nothing about the fixed point.
    kernel
        .enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: START_TIME,
            horizon: STOP_TIME,
        })
        .expect("entering event mode never fails on this component");
    // The host's `while discreteStatesNeedUpdate` loop, written the way a
    // conforming FMI importer writes it: keep calling until the component stops
    // asking. The ceiling is the harness's own escape hatch — a component that
    // kept asking would otherwise hang the run instead of failing it.
    let mut iterations = 0_usize;
    loop {
        iterations += 1;
        assert!(
            iterations <= HOST_ITERATION_CEILING,
            "the component asked a conforming host to iterate the discrete \
             fixed point again, which its own contract says it never does"
        );
        match kernel.update_discrete_states() {
            Ok(states) => {
                if !states.discrete_states_need_update {
                    break;
                }
            }
            Err(_) => break,
        }
    }
    assert!(
        iterations == 1,
        "the whole discrete-state fixed point runs inside one call, so a \
         conforming host must never enter a second iteration"
    );
}

/// # Property
///
/// No sequence of ME kernel operations, issued in any order on a well-formed
/// instantiation, panics: every operation returns a `Result` its host can act
/// on, and none of them changes the model description the component reports.
///
/// SPEC_0038 makes [`ModelExchangeKernel`] the only way into the model, so an
/// out-of-order host call has to surface as an [`MeError`] rather than as an
/// unwind through the FMI boundary. "Any order" is meant literally: the
/// generated alphabet includes every lifecycle transition in every position,
/// and its `f64` payloads include NaN and both infinities.
fn property_no_operation_sequence_panics(ops: &[KernelOp]) {
    let model = single_state_model();
    let mut kernel = instantiate(&model);
    let states_before = kernel.model_description().continuous_state_count;
    let indicators_before = kernel.model_description().event_indicator_count;
    let mut returned = 0_usize;
    for op in ops {
        let _ = apply(&mut kernel, *op);
        // Reaching here means the call returned rather than unwinding; the
        // count below is what turns that into a checkable postcondition.
        returned += 1;
    }
    assert!(
        returned == ops.len(),
        "every operation in the sequence must return control to its host"
    );
    let after = kernel.model_description();
    assert!(
        after.continuous_state_count == states_before
            && after.event_indicator_count == indicators_before,
        "no host operation may change the shape the component describes itself \
         with; a host sizes its buffers once"
    );
}

/// # Property (PIN, not an invariant)
///
/// `set_time` applies whatever time it is given: the component neither rejects
/// nor clamps a time earlier than the one it already holds, so a host that
/// sets time backwards moves the component backwards.
///
/// This records CURRENT BEHAVIOUR, not a requirement met. FMI 3.0 gives Model
/// Exchange a state machine in which `fmi3SetTime` is legal only in some
/// states; rumoca's component enforces none of it. `SolveMeKernel::set_time`
/// (`fmi_me/kernel.rs:749`) performs no validation at all: it returns `()`, so
/// it cannot report a rejection, and it writes `self.time` and
/// `self.event_boundary` straight out of the `MeTime` it is handed —
/// `MeTime::at` does not check its argument either. The component's own
/// `MeState` field is assigned in five places and read in none, so no operation
/// is gated on the lifecycle position it records. The only monotonic clamp
/// anywhere in the component is `self.time = event_time.max(self.time)` on the
/// event-boundary path (`fmi_me/kernel.rs:594`), pinned separately below. Do
/// not "fix" this harness by making it assert monotonicity: it would then
/// assert something the component does not do.
///
/// Scope: both drivers feed this property finite pairs ordered inside the
/// fixture's horizon, so what it pins is the backwards *finite* write. Because
/// `set_time` validates nothing, NaN and infinite times are written just as
/// unconditionally — but that is a reading of the implementation, not something
/// asserted here. Those payloads reach `set_time` only through
/// [`property_no_operation_sequence_panics`], which asserts no more than that
/// the call returns; nothing in this harness pins what the component holds
/// afterwards.
fn property_set_time_is_not_monotonic(earlier: f64, later: f64) {
    let model = single_state_model();
    let mut kernel = instantiate(&model);
    kernel.set_time(MeTime::at(later));
    let at_later = kernel
        .observe()
        .expect("a bounded finite time should be observable")
        .time();
    kernel.set_time(MeTime::at(earlier));
    let at_earlier = kernel
        .observe()
        .expect("a bounded finite time should be observable")
        .time();
    assert!(
        at_later == later && at_earlier == earlier,
        "set_time is applied verbatim in both directions"
    );
    assert!(
        at_earlier < at_later,
        "current behaviour: the component's time went backwards and nothing \
         rejected it"
    );
}

/// # Property (PIN, not an invariant)
///
/// The component's one and only monotonic clamp is on the event-boundary path:
/// entering event mode at an instant *earlier* than the component's current
/// time applies the event at the current time instead.
///
/// This records CURRENT BEHAVIOUR — `RuntimeEventBoundaryHandler::on_event_time`
/// opens with `self.time = event_time.max(self.time)`
/// (`fmi_me/kernel.rs:594`). It is the counterpart to
/// [`property_set_time_is_not_monotonic`]: the clamp exists here and nowhere
/// else, which is why "time is monotonic across the component" does not hold.
fn property_the_event_boundary_clamps_time_upwards(earlier: f64, later: f64) {
    let model = single_state_model();
    let mut kernel = instantiate(&model);
    run_to_continuous_time_mode(&mut kernel);
    kernel.set_time(MeTime::at(later));
    kernel
        .enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: earlier,
            horizon: STOP_TIME,
        })
        .expect("event mode should be enterable from continuous-time mode");
    let states = kernel
        .update_discrete_states()
        .expect("a state event on the one-state fixture should apply");
    assert!(
        states.time == later,
        "the event instant is clamped up to the component's current time, so \
         an earlier requested instant does not move it backwards"
    );
}

/// # Property
///
/// A model whose MLS §8.6 initialization updates can never reach a fixed point
/// makes the component stop at its own iteration ceiling and report
/// `MeError::Evaluation`, rather than looping forever or panicking.
///
/// `UPDATE_MAX_ITERS = 32` (`fmi_me/kernel.rs:38`) is the bound every internal
/// fixed point in the component is given; `eval_and_apply_update_rows` returns
/// `EvalSolveError::UpdateDidNotConverge` when it is reached, which the FMI
/// boundary maps to `MeError::Evaluation`.
fn property_a_non_convergent_fixed_point_returns_an_error(increment: f64) {
    let model = divergent_initialization_model(increment);
    let mut kernel = instantiate(&model);
    kernel
        .enter_initialization_mode()
        .expect("initialization mode should be enterable from Instantiated");
    let outcome = kernel.exit_initialization_mode();
    assert!(
        matches!(outcome, Err(MeError::Evaluation { .. })),
        "a fixed point that cannot settle must be reported to the host as an \
         evaluation failure, not run forever"
    );
}

#[cfg(kani)]
mod proof {
    use super::{KernelOp, MAX_OPS};

    /// A bounded operation sequence: at most `MAX_OPS` operations, each
    /// carrying one `f64` that is either small and finite or non-finite.
    ///
    /// The bound is on magnitude only, so NaN and both infinities stay in the
    /// alphabet — the fallback strategy admits exactly the same set.
    fn any_ops() -> Vec<KernelOp> {
        let len: usize = kani::any();
        kani::assume(len <= MAX_OPS);
        let mut ops = Vec::with_capacity(len);
        for _ in 0..len {
            let index: u8 = kani::any();
            let value: f64 = kani::any();
            kani::assume(!value.is_finite() || value.abs() <= 2.0);
            ops.push(KernelOp::from_index(index, value));
        }
        ops
    }

    /// An ordered finite pair `earlier < later` inside the fixture's horizon,
    /// separated by at least `MIN_TIME_GAP`.
    fn any_ordered_times() -> (f64, f64) {
        let earlier: f64 = kani::any();
        let later: f64 = kani::any();
        kani::assume(earlier.is_finite() && later.is_finite());
        kani::assume(earlier >= super::START_TIME && later <= super::STOP_TIME);
        // See `MIN_TIME_GAP`: the fallback cannot draw a closer pair without an
        // empty range, so the proof does not explore one either.
        kani::assume(earlier + super::MIN_TIME_GAP <= later);
        (earlier, later)
    }

    /// A conforming FMI host's `while discreteStatesNeedUpdate` loop terminates
    /// after exactly one `update_discrete_states` call, whatever operations
    /// preceded it.
    #[kani::proof]
    #[kani::unwind(16)]
    fn the_host_event_iteration_terminates_after_one_call() {
        super::property_the_host_event_iteration_terminates_after_one_call(&any_ops());
    }

    /// No sequence of ME kernel operations, issued in any order on a
    /// well-formed instantiation, panics.
    #[kani::proof]
    #[kani::unwind(16)]
    fn no_operation_sequence_panics() {
        super::property_no_operation_sequence_panics(&any_ops());
    }

    /// PIN: `set_time` applies whatever time it is given, so a host that sets
    /// time backwards moves the component backwards.
    #[kani::proof]
    #[kani::unwind(16)]
    fn set_time_is_not_monotonic() {
        let (earlier, later) = any_ordered_times();
        super::property_set_time_is_not_monotonic(earlier, later);
    }

    /// PIN: the component's one monotonic clamp is on the event-boundary path.
    #[kani::proof]
    #[kani::unwind(16)]
    fn the_event_boundary_clamps_time_upwards() {
        let (earlier, later) = any_ordered_times();
        super::property_the_event_boundary_clamps_time_upwards(earlier, later);
    }

    /// A fixed point that cannot settle stops at the component's iteration
    /// ceiling and reports `MeError::Evaluation`.
    #[kani::proof]
    #[kani::unwind(64)]
    fn a_non_convergent_fixed_point_returns_an_error() {
        let increment: f64 = kani::any();
        kani::assume(increment.is_finite() && increment >= 1.0 && increment <= 4.0);
        super::property_a_non_convergent_fixed_point_returns_an_error(increment);
    }
}

#[cfg(all(test, not(kani)))]
mod fallback {
    // Fallback driver: identical property under proptest because Kani is not in
    // the dev shell yet. Graduating means deleting this module.
    use proptest::prelude::*;

    use super::{KernelOp, MAX_OPS, MIN_TIME_GAP, START_TIME, STOP_TIME};

    /// One `f64` payload: small and finite, or non-finite. Mirrors the proof's
    /// `!value.is_finite() || value.abs() <= 2.0` assumption.
    fn any_value() -> impl Strategy<Value = f64> {
        prop_oneof![
            6 => -2.0f64..=2.0f64,
            1 => Just(f64::NAN),
            1 => Just(f64::INFINITY),
            1 => Just(f64::NEG_INFINITY),
        ]
    }

    /// The same operation alphabet the proof explores, over the same bounds.
    fn any_ops() -> impl Strategy<Value = Vec<KernelOp>> {
        prop::collection::vec(
            (any::<u8>(), any_value())
                .prop_map(|(index, value)| KernelOp::from_index(index, value)),
            0..=MAX_OPS,
        )
    }

    /// An ordered finite pair `earlier < later` inside the fixture's horizon,
    /// separated by at least `MIN_TIME_GAP`.
    ///
    /// Both components are drawn from ranges fixed at compile time and `later`
    /// is then computed, rather than drawing `later` from a range anchored at
    /// `earlier`. That is deliberate: proptest's `f64` sampler panics on a
    /// range whose ends coincide (`proptest-1.11.0/src/num/float_samplers.rs`),
    /// and an anchored range collapses to exactly that at the top of its outer
    /// range — with `earlier` at the largest double below `1.0`, `earlier +
    /// f64::EPSILON` rounds to exactly `STOP_TIME` and the inner range becomes
    /// `1.0..=1.0`. Computing `later` and clamping it removes the data-dependent
    /// range altogether, so no draw can degenerate.
    fn any_ordered_times() -> impl Strategy<Value = (f64, f64)> {
        (
            START_TIME..=(STOP_TIME - MIN_TIME_GAP),
            MIN_TIME_GAP..=(STOP_TIME - START_TIME),
        )
            .prop_map(|(earlier, gap)| {
                // `earlier` is capped a full gap below the horizon and `gap` is
                // whole orders of magnitude above the ULP anywhere in `[0, 1]`,
                // so the sum is strictly greater than `earlier` and the clamp
                // can only bring it back to `STOP_TIME`, which is greater still.
                (earlier, (earlier + gap).min(STOP_TIME))
            })
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(64))]

        /// A conforming FMI host's `while discreteStatesNeedUpdate` loop
        /// terminates after exactly one `update_discrete_states` call, whatever
        /// operations preceded it.
        #[test]
        fn the_host_event_iteration_terminates_after_one_call(ops in any_ops()) {
            super::property_the_host_event_iteration_terminates_after_one_call(&ops);
        }

        /// No sequence of ME kernel operations, issued in any order on a
        /// well-formed instantiation, panics.
        #[test]
        fn no_operation_sequence_panics(ops in any_ops()) {
            super::property_no_operation_sequence_panics(&ops);
        }

        /// PIN: `set_time` applies whatever time it is given, so a host that
        /// sets time backwards moves the component backwards.
        #[test]
        fn set_time_is_not_monotonic((earlier, later) in any_ordered_times()) {
            super::property_set_time_is_not_monotonic(earlier, later);
        }

        /// PIN: the component's one monotonic clamp is on the event-boundary
        /// path.
        #[test]
        fn the_event_boundary_clamps_time_upwards((earlier, later) in any_ordered_times()) {
            super::property_the_event_boundary_clamps_time_upwards(earlier, later);
        }

        /// A fixed point that cannot settle stops at the component's iteration
        /// ceiling and reports `MeError::Evaluation`.
        #[test]
        fn a_non_convergent_fixed_point_returns_an_error(increment in 1.0f64..=4.0f64) {
            super::property_a_non_convergent_fixed_point_returns_an_error(increment);
        }
    }
}
