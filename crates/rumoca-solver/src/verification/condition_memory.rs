//! Harness A: MLS §8.3.5.1 activation buffers seeded at the initialization
//! instant, over a model with 1..=3 generated conditions.
//!
//! These properties drive the production seed implementation that
//! `SolveRuntime::seed_condition_memory_for_initialization` delegates to. The
//! implementation is factored from `SolveRuntime` so verification does not
//! construct unrelated solver, projection, delay, and cache state.

use rumoca_eval_solve::{PreparedScalarProgramBlock, RowEvalContext};

use crate::runtime::solve_runtime::{
    ConditionMemorySeedInput, seed_condition_memory_for_initialization_core,
};

#[cfg(not(kani))]
use super::model_fixture::MAX_CONDITIONS;
use super::model_fixture::{CONDITION_THRESHOLD, ConditionLayout, condition_memory_model};

/// The tolerance every slot write in these harnesses uses.
const SEED_TOL: f64 = 1.0e-9;

/// The initialization instant the harnesses seed at.
const T_START: f64 = 0.0;

/// One seed applied to a fresh parameter vector.
struct SeedRun {
    layout: ConditionLayout,
    params: Vec<f64>,
    /// The `(slot, value)` pairs the seed reported writing.
    reported: Vec<(usize, f64)>,
}

fn seed(
    model: &rumoca_ir_solve::SolveModel,
    discrete_rhs: &PreparedScalarProgramBlock,
    params: &mut [f64],
) -> Vec<(usize, f64)> {
    seed_condition_memory_for_initialization_core(ConditionMemorySeedInput {
        model,
        discrete_rhs,
        row_eval_context: RowEvalContext::default(),
        y: &mut [],
        p: params,
        t: T_START,
        tol: SEED_TOL,
    })
    .expect("condition memory should seed")
    .iter()
    .map(|entry| (entry.index, entry.value))
    .collect()
}

#[cfg(not(kani))]
fn seed_once(starts: &[f64]) -> SeedRun {
    let model = condition_memory_model(starts);
    let discrete_rhs = PreparedScalarProgramBlock::new(model.problem.discrete.rhs.clone())
        .expect("bounded fixture should prepare discrete rows");
    let mut params = model.parameters.clone();
    let reported = seed(&model, &discrete_rhs, &mut params);
    SeedRun {
        layout: ConditionLayout::new(starts.len()),
        params,
        reported,
    }
}

/// The value condition `index` evaluates to at the initialization instant.
///
/// The fixture's conditions are `s_i > CONDITION_THRESHOLD` over parameters
/// nothing else writes, so this is also their value at every later instant
/// until an event moves `s_i`.
fn condition_value_at_start(starts: &[f64], index: usize) -> f64 {
    f64::from(starts[index] > CONDITION_THRESHOLD)
}

/// `edge(b) = b and not pre(b)` exactly as lowering emits it.
///
/// `buffered_edge` in `rumoca-phase-solve` builds `current and not
/// p[condition_memory_slot]`, so the edge a seeded buffer presents is decided
/// by the buffer slot alone.
fn buffered_edge(current: f64, buffer: f64) -> bool {
    current != 0.0 && buffer == 0.0
}

/// # Property
///
/// After seeding, every activation buffer holds the value its own condition
/// evaluates to at the initialization instant.
///
/// Registry row FS-EQN-001: "A when-clause activates on the rising edge of a
/// Boolean activation buffer whose start value is the activation condition
/// evaluated at the initialization instant, so a condition already true there
/// presents no edge." (MLS 3.6 §8.3.5.1)
#[cfg(not(kani))]
fn property_every_buffer_holds_its_condition_value_at_the_start(starts: &[f64]) {
    let run = seed_once(starts);
    assert_every_buffer_matches_start(starts, &run);
}

fn assert_every_buffer_matches_start(starts: &[f64], run: &SeedRun) {
    for index in 0..run.layout.count() {
        let expected = condition_value_at_start(starts, index);
        assert!(
            run.params[run.layout.buffer(index)] == expected,
            "MLS §8.3.5.1 starts the buffer at the condition evaluated on the \
             initial values, so the seed must write that value into every slot"
        );
        assert!(
            run.reported.contains(&(run.layout.buffer(index), expected)),
            "the reported seed must name every slot it wrote, so a caller's \
             event-entry snapshot can carry the same value"
        );
    }
}

/// # Property (deliberately weaker restatement, kept as a regression guard)
///
/// No seeded activation presents a rising edge at the initial event: for every
/// condition, `edge(b) = b and not pre(b)` computed from the buffer the seed
/// wrote and the condition's own unchanged value is false.
///
/// Registry row FS-EQN-002: "Before the start of integration every variable
/// satisfies v = pre(v), so an activation buffer cannot present a rising edge
/// at the initial event unless the condition itself rises there." (MLS 3.6 §8.6)
///
/// This is implied by
/// [`property_every_buffer_holds_its_condition_value_at_the_start`] and adds no
/// strength over it: that property pins every buffer to its condition's own
/// value, and `current and not buffer` over a buffer equal to `current` is false
/// by arithmetic. Read it as a guard, not as independent evidence — it is the
/// only place the FS-EQN-002 consequence is stated through [`buffered_edge`],
/// the `current and not p[slot]` shape lowering actually emits, so a seed that
/// stayed correct while that shape moved would fail here first. Deleting it
/// costs coverage of nothing except that pairing.
#[cfg(not(kani))]
fn property_no_seeded_activation_has_a_rising_edge(starts: &[f64]) {
    let run = seed_once(starts);
    assert_no_seeded_rising_edge(starts, &run);
}

fn assert_no_seeded_rising_edge(starts: &[f64], run: &SeedRun) {
    for index in 0..run.layout.count() {
        let current = condition_value_at_start(starts, index);
        assert!(
            !buffered_edge(current, run.params[run.layout.buffer(index)]),
            "a condition unchanged from the initialization instant cannot rise \
             there, so its lowered edge must be false — including for a \
             condition that is already true"
        );
    }
}

/// # Property
///
/// Seeding is idempotent: a second seed over an already-seeded parameter vector
/// writes the same buffer values and leaves every other slot untouched.
///
/// Registry row FS-EQN-002: "Before the start of integration every variable
/// satisfies v = pre(v), so an activation buffer cannot present a rising edge
/// at the initial event unless the condition itself rises there." (MLS 3.6 §8.6)
/// A seed that moved on its second application would mean the buffers it wrote
/// are not the fixed point §8.6 requires the pre-integration state to be at.
#[cfg(not(kani))]
fn property_seeding_is_idempotent(starts: &[f64]) {
    let model = condition_memory_model(starts);
    let discrete_rhs = PreparedScalarProgramBlock::new(model.problem.discrete.rhs.clone())
        .expect("bounded fixture should prepare discrete rows");
    let mut params = model.parameters.clone();
    seed(&model, &discrete_rhs, &mut params);
    let after_first = params.clone();
    seed(&model, &discrete_rhs, &mut params);
    assert!(
        params == after_first,
        "the seed is a fixed point: reapplying it must not move any parameter"
    );
}

/// # Property
///
/// The seed leaves the caller's `initial()` flag slot exactly as it found it,
/// and seeds the `initial()` activation's own buffer false so the initial event
/// remains that activation's rising edge.
///
/// Registry row FS-EQN-001: "A when-clause activates on the rising edge of a
/// Boolean activation buffer whose start value is the activation condition
/// evaluated at the initialization instant, so a condition already true there
/// presents no edge." (MLS 3.6 §8.3.5.1) `initial()` is false everywhere except
/// the initial event, so the value it "has at the initialization instant" for
/// buffering purposes is false — which is why the seed must clear the flag for
/// its own evaluation without clearing it for the event that follows.
#[cfg(not(kani))]
fn property_the_seed_preserves_the_initial_flag(starts: &[f64]) {
    let run = seed_once(starts);
    assert_initial_activation_preserved(&run);
}

fn assert_initial_activation_preserved(run: &SeedRun) {
    assert!(
        run.params[run.layout.initial_flag()] == 1.0,
        "the fixture runs with initial() raised, and clearing the flag for the \
         seed must not clear it for the event"
    );
    assert!(
        run.params[run.layout.initial_buffer()] == 0.0,
        "MLS §8.6 enables a when-clause during initialization only with \
         initial(), so that one activation must keep its edge"
    );
    assert!(
        buffered_edge(1.0, run.params[run.layout.initial_buffer()]),
        "and the edge it keeps is the initial event itself: with the buffer \
         false and `initial()` true at that instant, the lowered edge fires — \
         the one activation MLS §8.6 allows during initialization"
    );
}

/// All condition-memory seed obligations over one shared production run.
///
/// The exhaustive finite-quotient test uses this composition so the Solve IR
/// fixture, prepared evaluator, and first seed are built once per valuation.
/// The individual properties remain separate as focused regression tests.
fn property_complete_condition_memory_seed_contract(starts: &[f64]) {
    let model = condition_memory_model(starts);
    let discrete_rhs = PreparedScalarProgramBlock::new(model.problem.discrete.rhs.clone())
        .expect("bounded fixture should prepare discrete rows");
    let mut params = model.parameters.clone();
    let reported = seed(&model, &discrete_rhs, &mut params);
    let after_first = params.clone();
    seed(&model, &discrete_rhs, &mut params);
    assert!(
        params == after_first,
        "the seed is a fixed point: reapplying it must not move any parameter"
    );
    let run = SeedRun {
        layout: ConditionLayout::new(starts.len()),
        params: after_first,
        reported,
    };
    assert_every_buffer_matches_start(starts, &run);
    assert_no_seeded_rising_edge(starts, &run);
    assert_initial_activation_preserved(&run);
}

#[cfg(all(test, not(kani)))]
mod fallback {
    // Conventional fallback driver: the same properties exercise the exact
    // production core under proptest without launching the verifier.
    use proptest::prelude::*;

    use super::{CONDITION_THRESHOLD, MAX_CONDITIONS};

    /// 1..=3 finite start values, weighted so the threshold itself and both
    /// sides of it are all reachable.
    fn any_starts() -> impl Strategy<Value = Vec<f64>> {
        prop::collection::vec(
            prop_oneof![
                4 => -4.0f64..8.0f64,
                1 => Just(CONDITION_THRESHOLD),
                1 => Just(0.0f64),
            ],
            1..=MAX_CONDITIONS,
        )
    }

    fn threshold_quotient_starts(count: usize, valuation: usize) -> Vec<f64> {
        (0..count)
            .map(|index| {
                if valuation & (1_usize << index) != 0 {
                    CONDITION_THRESHOLD + 1.0
                } else {
                    CONDITION_THRESHOLD - 1.0
                }
            })
            .collect()
    }

    #[test]
    fn exhaustive_threshold_quotient_satisfies_the_complete_seed_contract() {
        for count in 1..=MAX_CONDITIONS {
            for valuation in 0..(1_usize << count) {
                let starts = threshold_quotient_starts(count, valuation);
                super::property_complete_condition_memory_seed_contract(&starts);
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(64))]

        /// After seeding, every activation buffer holds the value its own
        /// condition evaluates to at the initialization instant.
        /// (FS-EQN-001, MLS §8.3.5.1)
        #[test]
        fn every_buffer_holds_its_condition_value_at_the_start(starts in any_starts()) {
            super::property_every_buffer_holds_its_condition_value_at_the_start(&starts);
        }

        /// WEAKER RESTATEMENT of
        /// `every_buffer_holds_its_condition_value_at_the_start`: no seeded
        /// activation presents a rising edge at the initial event.
        /// (FS-EQN-002, MLS §8.6)
        #[test]
        fn no_seeded_activation_has_a_rising_edge(starts in any_starts()) {
            super::property_no_seeded_activation_has_a_rising_edge(&starts);
        }

        /// Seeding is idempotent: a second seed writes the same buffer values
        /// and leaves every other slot untouched. (FS-EQN-002, MLS §8.6)
        #[test]
        fn seeding_is_idempotent(starts in any_starts()) {
            super::property_seeding_is_idempotent(&starts);
        }

        /// The seed leaves the caller's `initial()` flag slot exactly as it
        /// found it, and seeds the `initial()` buffer false.
        /// (FS-EQN-001, MLS §8.3.5.1)
        #[test]
        fn the_seed_preserves_the_initial_flag(starts in any_starts()) {
            super::property_the_seed_preserves_the_initial_flag(&starts);
        }
    }
}
