//! MLS §8.5 time events owned by a `when` activation over `time`.
//!
//! # Why this exists
//!
//! `when time > 0.5 then ...` did not fire. Not late, not twice — never.
//!
//! MLS 3.6 §8.5 calls the schedule out by name: *"It is a quality of
//! implementation issue that the following special relations `time >= discrete
//! expression`, `time < discrete expression` trigger a time event at
//! `time = discrete expression`"*. rumoca took that freedom for a relation
//! written in a model equation but not for one written as a `when` activation:
//! `lower_condition_tree` gave every relational leaf of an activation a zero
//! crossing, so the instant `t = 0.5` was searched for instead of scheduled.
//!
//! Searching for it is what broke it. §8.5 also fixes what a relation reads
//! between events — *"An event generating expression has an internal buffer, and
//! the value of the expression can only be changed at event instants"*, and
//! *"during continuous integration event generation expression has the constant
//! value of the expression from the last event instant"* — and the located
//! crossing lands **on** `t = 0.5`, where the strict relation still reads false.
//! The crossing was therefore consumed with the activation never true, and no
//! later crossing existed to retry it.
//!
//! Whether the bug was visible depended on details with no semantic content —
//! which solver path the model took, and whether the located crossing happened
//! to coincide with an output sample. A coincident crossing is applied *at* the
//! sample (`runtime_root_event_application_time` snaps to the target), where the
//! strict relation still reads false; a non-coincident one is applied at the
//! sample's right limit, where it finally reads true. `when time > 0.5` on its
//! own escaped; `when time > 0.5 and x < 2` — the same relation, one extra
//! conjunct — never fired at all, on either solver.
//!
//! # OpenModelica falsification
//!
//! Every expectation below was first run through `omc` (dassl, `stopTime=1.0`,
//! `numberOfIntervals=20`), which emits three rows at an event instant:
//! pre-event, event-left, event-right. The `t = 0.5` column below is that
//! triple.
//!
//! | model                              | omc `t = 0.45` | omc `t = 0.5` | omc `t = 0.55` |
//! | ---------------------------------- | -------------- | ------------- | -------------- |
//! | `when time > 0.5 then y = 1`       | 0              | 0, 0, 1       | 1              |
//! | `when time >= 0.5 then y = 1`      | 0              | 0, 0, 1       | 1              |
//! | `when time > 0.5 and x < 2`        | 0              | 0, 0, 1       | 1              |
//! | `when x < 2 and time > 0.5`        | 0              | 0, 0, 1       | 1              |
//! | `when time > 2 * p`, `p = 0.25`    | 0              | 0, 0, 1       | 1              |
//! | `when time > 0.5 then reinit(x,1)` | 0              | 0, 0, 1       | 1              |
//! | `when time < 0.5 then y = 1`       | 0              | 0, 0, 0       | 0              |
//! | `when {initial(), time > 0.5}`     | 1              | 1, 1, 2       | 2              |
//!
//! Two further OpenModelica runs back the multi-instance and start-instant
//! expectations below. A class instantiated as `early(threshold = 0.33)` and
//! `late(threshold = 0.73)` fires `early.y` at `0.33` and `late.y` at `0.73`
//! (array instantiation `t[2](threshold = {0.33, 0.73})` gives the same two
//! instants), and `when time > 0` fires once at the start's right limit while
//! `when time >= 0` never fires at all.
//!
//! The falling row is the one that says strictness is not the axis: `time < 0.5`
//! owns the very same instant, and the body still does not run there, because a
//! `when` activates on a rising edge only.
//!
//! One presentational difference remains and is asserted around rather than
//! away. Both tools apply the activation at the event's *right limit* — where
//! §8.5's buffered relation first reads true — and both stamp the row with that
//! right limit rather than with the instant; they differ only in how wide the
//! limit is. OpenModelica's is about 4e-10 relative (`0.3300000001330113` for an
//! instant at `0.33`, `1.000000000124e-10` for one at `0`); rumoca's is
//! `2 * atol`, i.e. `0.500002` for `t = 0.5` at the default `atol = 1e-6`. The
//! values agree; the stamps differ by 2e-6 out of a 0.05 output interval.
//!
//! # Divergences these tests deliberately do not claim
//!
//! One of the five below is a regression this change introduced and chose to
//! land; the rest were there before it and are untouched by it. Each says which
//! it is, because a recorded divergence that misdescribes its own origin is
//! worse than an unrecorded one.
//!
//! * **Activation at the initial event, on the state-free solver path.** MLS
//!   §8.5: *"The equations of a when-clause are active during initialization, if
//!   and only if they are explicitly enabled with `initial()`"*. `omc` leaves
//!   `y = 0` for `when time < 0.5 then y = 1`. rumoca agrees whenever the model
//!   carries a continuous state, and disagrees when it does not: the same model
//!   with `der(x) = 1` added leaves `y = 0`, and without it fires at `t = 0`.
//!   The divergence is therefore solver-path dependent — it belongs to the
//!   state-free session, not to the event schedule — and it is shared with
//!   `when x < 2 then y = 1`, so it is not a property of `time` either.
//!   [`falling_time_activation_does_not_fire_at_its_instant`] accordingly asserts
//!   only what the instant itself does, on a model that has a state.
//! * **A scheduled instant coinciding with a state crossing — REGRESSION
//!   INTRODUCED HERE.** `when time > 0.5 then a = 1` beside
//!   `when x > 0.5 then b = pre(a) + 10`, with `der(x) = 1`. `omc` handles both
//!   in one event iteration at the instant, so `b = 10` with `pre(a) = 0`, as
//!   §8.5 requires. Before this change rumoca also reached `b = 10`, one output
//!   interval late, because both activations were zero crossings and the
//!   rk-like session located them together. Giving the `time` relation a
//!   scheduled instant splits them into two iterations, and the second one now
//!   reads a `pre(a)` the first has already advanced: **`b` regressed from 10 to
//!   11 relative to the pre-change tree.** The regression was measured and
//!   accepted rather than overlooked — no MSL `when` gains a scheduled instant
//!   (every `time`-relational activation in the library has a threshold
//!   `time_event_instant` rejects), the trigger needs a state crossing to land
//!   exactly on an instant, and five other probes moved from wrong to
//!   OpenModelica-matching — but it is a regression and is recorded as one.
//!
//!   The fix belongs to the rk-like session, which applies a scheduled right
//!   limit without carrying the continuous state across it, so a crossing that
//!   lands on the instant is not yet true when the instant is handled. The
//!   equivalent carry *is* in place on the backend-neutral driver kernel that
//!   the diffsol session uses; that does not rescue this model, because on
//!   `SimSolverMode::Bdf` neither `a` nor `b` fires at all, before or after this
//!   change — a separate, pre-existing diffsol defect.
//! * **The diffsol session at the start instant** (pre-existing). The same
//!   models on `SimSolverMode::Bdf` leave `when time > 0` unfired and fire
//!   `when time < 0.5` at `t = 0` — the mirror image of what omc does, and of
//!   what the rk-like session asserted here does. That session applies a
//!   crossing located at the start differently, which is its own defect and not
//!   the schedule's; this file therefore names its solver mode explicitly rather
//!   than asserting whatever the default happens to be.
//!   [`the_diffsol_session_agrees_on_a_scheduled_instant`] pins the part of that
//!   session this change *does* reach, so the divergence above stays a statement
//!   about the start instant rather than about scheduling generally.
//! * **A negated `time` relation** (pre-existing). `when not (time > 0.5)` is
//!   true from the start and only ever falls, so `omc` never runs it; rumoca
//!   runs it at `t = 0.5`. The instant is scheduled correctly — the activation
//!   is the negation, whose buffered value has no seeded edge at the first event
//!   it sees. Measured before and after this change on the rk-like session, the
//!   trace is identical; the diffsol session instead fires it at `t = 0` under
//!   the start-instant defect above.
//! * **`elsewhen` priority.** MLS §8.3.5: *"The equations within a
//!   when-equation are activated only at the instant when the scalar expression
//!   or any of the elements of the vector expression becomes true"*, and the
//!   earlier branch's higher priority *"can be used to resolve assignment
//!   conflicts"* — i.e. priority ranks branches that activate together, and
//!   activation is a rising edge. `omc` follows that: it runs the second branch
//!   of `when time > 0.3 then y = 1; elsewhen time > 0.7 then y = 2;` at
//!   `t = 0.7` (`y = 2`), because only that branch's condition becomes true
//!   there. rumoca keeps `y = 1`, because its branch guard subtracts the
//!   earlier branch's *level* rather than its edge. The same disagreement
//!   appears for a state-relation chain (`when x > 0.3 … elsewhen x > 0.7`,
//!   `der(x) = 1`: omc reaches `y = 2` at `t = 0.7`), so it is a branch-guard
//!   bug and not a `time` one. Both instants are scheduled correctly, which is
//!   all this file claims — see [`else_when_schedules_every_branch_instant`].

use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};

/// Compile `source` and run it over `[0, 1]` with a 0.05 output interval — the
/// grid the OpenModelica runs in the module table used.
///
/// Every model here carries a `der(x) = 1` integrator. It is not decoration:
/// the rk-like session refuses a model with no state equations, and the
/// state-free session it would otherwise use has its own unrelated
/// initial-activation defect (module header). An unused integrator cannot move
/// any `y`, so the OpenModelica values in the table are unaffected by it.
///
/// The entry point matters: plain `simulate_dae` is the diffsol module's own
/// function and runs diffsol whatever `solver_mode` says, so a test that used it
/// would silently assert one backend while claiming the other. This dispatches
/// on the mode, and the mode is the explicit rk-like session — the path the
/// OpenModelica comparison in the module header was measured against.
fn simulate(name: &str, source: &str) -> (rumoca::CompilationResult, SimResult) {
    let compiled = rumoca::Compiler::new()
        .model(name)
        .compile_str(source, "time_event_when_activation.mo")
        .unwrap_or_else(|error| panic!("`{name}` should compile: {error}"));
    let sim = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.05),
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("`{name}` should simulate: {error}"));
    (compiled, sim)
}

/// The last recorded sample at or before `t`.
///
/// An event instant contributes several samples with the same time stamp, and
/// the last of them is the post-event value — the same row OpenModelica writes
/// last for that instant.
fn value_at(sim: &SimResult, name: &str, t: f64) -> f64 {
    let index = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace should contain `{name}`; names={:?}", sim.names));
    let values = &sim.data[index];
    let mut result = values[0];
    for (sample, &time) in sim.times.iter().enumerate() {
        if time <= t + 1.0e-9 {
            result = values[sample];
        } else {
            break;
        }
    }
    result
}

/// `(root_count, time_event_count)` of the compiled model.
fn event_owners(compiled: &rumoca::CompilationResult) -> (usize, usize) {
    compiled
        .dae
        .inspect(|view| (view.root_count(), view.time_event_count()))
}

/// The event instant under test, and the probe just past its right limit.
///
/// rumoca time-stamps the post-event sample at `INSTANT + 2 * atol` where
/// OpenModelica stamps it `INSTANT` exactly (see the module header). The probe
/// is two orders of magnitude past that offset and two orders of magnitude short
/// of the 0.05 output interval, so it reads the post-event value and nothing
/// else.
const INSTANT: f64 = 0.5;
const AFTER_INSTANT: f64 = 0.5 + 1.0e-4;

fn assert_fires_at_the_instant(sim: &SimResult, name: &str, model: &str) {
    assert_eq!(
        value_at(sim, name, 0.45),
        0.0,
        "`{model}`: the body must not run before the instant"
    );
    assert_eq!(
        value_at(sim, name, AFTER_INSTANT),
        1.0,
        "`{model}`: the body must run at t = {INSTANT}, as omc does"
    );
    assert_eq!(
        value_at(sim, name, 0.55),
        1.0,
        "`{model}`: and must have run by the next output sample"
    );
    assert_eq!(
        value_at(sim, name, 1.0),
        1.0,
        "`{model}`: the body's effect must persist"
    );
}

const STRICT: &str = "model Strict
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time > 0.5 then
    y = 1;
  end when;
end Strict;
";

const NON_STRICT: &str = "model NonStrict
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time >= 0.5 then
    y = 1;
  end when;
end NonStrict;
";

const FALLING: &str = "model Falling
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time < 0.5 then
    y = 1;
  end when;
end Falling;
";

const COMPOUND_TIME_FIRST: &str = "model CompoundTimeFirst
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time > 0.5 and x < 2 then
    y = 1;
  end when;
end CompoundTimeFirst;
";

const COMPOUND_STATE_FIRST: &str = "model CompoundStateFirst
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when x < 2 and time > 0.5 then
    y = 1;
  end when;
end CompoundStateFirst;
";

const PARAMETER_THRESHOLD: &str = "model ParameterThreshold
  parameter Real p = 0.25;
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time > 2 * p then
    y = 1;
  end when;
end ParameterThreshold;
";

const STRICT_REINIT: &str = "model StrictReinit
  Real x(start = 0, fixed = true);
equation
  der(x) = 0;
  when time > 0.5 then
    reinit(x, 1);
  end when;
end StrictReinit;
";

const ELSE_WHEN: &str = "model ElseWhen
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time > 0.3 then
    y = 1;
  elsewhen time > 0.7 then
    y = 2;
  end when;
end ElseWhen;
";

const INITIAL_ALTERNATIVE: &str = "model InitialAlternative
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when {initial(), time > 0.5} then
    y = pre(y) + 1;
  end when;
end InitialAlternative;
";

/// A strict `time >` activation fires, and fires at its instant.
///
/// omc: `y = 0` through `t = 0.45`, `0, 0, 1` across the three `t = 0.5` rows,
/// `y = 1` from `t = 0.55`. The classification assertion is what keeps the
/// regression from returning by a different route: an owner count of one root
/// and no instant is exactly the state in which the body never ran.
#[test]
fn strict_time_activation_fires_at_its_instant() {
    let (compiled, sim) = simulate("Strict", STRICT);
    assert_eq!(
        event_owners(&compiled),
        (0, 1),
        "`when time > 0.5` owns one scheduled instant and no zero crossing"
    );
    assert_fires_at_the_instant(&sim, "y", "when time > 0.5");
}

/// The non-strict spelling fires at the same instant.
///
/// omc: identical trace to the strict spelling. MLS §8.5 names
/// `time >= discrete expression` explicitly; `time > discrete expression` is its
/// complement about the same point, so the instant cannot differ.
#[test]
fn non_strict_time_activation_fires_at_the_same_instant() {
    let (compiled, sim) = simulate("NonStrict", NON_STRICT);
    assert_eq!(event_owners(&compiled), (0, 1));
    assert_fires_at_the_instant(&sim, "y", "when time >= 0.5");
}

/// The regression itself: a `time` relation conjoined with a state relation.
///
/// omc: `y = 0` through `t = 0.45`, `0, 0, 1` at `t = 0.5`, `y = 1` after.
/// rumoca produced `y = 0` for the whole run. The owner counts pin the fix:
/// the `time` leaf owns the instant, the state leaf keeps its own crossing, and
/// neither takes the other's owner.
#[test]
fn compound_activation_with_a_state_conjunct_fires_at_the_time_instant() {
    let (compiled, sim) = simulate("CompoundTimeFirst", COMPOUND_TIME_FIRST);
    assert_eq!(
        event_owners(&compiled),
        (1, 1),
        "one crossing for `x < 2`, one instant for `time > 0.5`"
    );
    assert_fires_at_the_instant(&sim, "y", "when time > 0.5 and x < 2");
}

/// Operand order in the conjunction is not part of the semantics.
///
/// omc: same trace as the other conjunction order.
#[test]
fn compound_activation_is_independent_of_conjunct_order() {
    let (compiled, sim) = simulate("CompoundStateFirst", COMPOUND_STATE_FIRST);
    assert_eq!(event_owners(&compiled), (1, 1));
    assert_fires_at_the_instant(&sim, "y", "when x < 2 and time > 0.5");
}

/// A threshold that is an expression over parameters is still an instant.
///
/// MLS §8.5 says *discrete expression*, not *literal*, so `2 * p` with
/// `p = 0.25` schedules `t = 0.5` exactly as a literal would. omc fires at
/// `t = 0.5`.
#[test]
fn parameter_expression_threshold_schedules_an_exact_instant() {
    let (compiled, sim) = simulate("ParameterThreshold", PARAMETER_THRESHOLD);
    assert_eq!(event_owners(&compiled), (0, 1));
    assert_fires_at_the_instant(&sim, "y", "when time > 2 * p");
}

/// The instant carries a state action, not only a discrete assignment.
///
/// `reinit` is the shape the original report used. omc: `x = 0` through the
/// event-left row at `t = 0.5`, `x = 1` from the event-right row on.
#[test]
fn strict_time_activation_applies_a_reinit_at_its_instant() {
    let (compiled, sim) = simulate("StrictReinit", STRICT_REINIT);
    assert_eq!(event_owners(&compiled), (0, 1));
    assert_fires_at_the_instant(&sim, "x", "when time > 0.5 then reinit(x, 1)");
}

/// A falling `time` relation owns the same instant and still does not activate.
///
/// omc leaves `y = 0` for the whole run: `time < 0.5` is true from the start and
/// only ever falls, and a `when` runs on a rising edge. The model carries a
/// continuous state deliberately — without one the state-free session fires
/// every already-true activation at `t = 0` (see the module header), which is a
/// different defect and not the one under test here.
#[test]
fn falling_time_activation_does_not_fire_at_its_instant() {
    let (compiled, sim) = simulate("Falling", FALLING);
    assert_eq!(
        event_owners(&compiled),
        (0, 1),
        "`when time < 0.5` owns the same instant as the rising spellings"
    );
    assert_eq!(
        value_at(&sim, "y", 0.45),
        0.0,
        "`time < 0.5` is true from the start, so there is no rising edge to run"
    );
    assert_eq!(
        value_at(&sim, "y", AFTER_INSTANT),
        0.0,
        "a falling `time` relation must not activate its `when` at t = 0.5"
    );
    assert_eq!(
        value_at(&sim, "y", 1.0),
        0.0,
        "and nothing later may activate it either, exactly as omc leaves it"
    );
}

/// Every branch of a `when`/`elsewhen` chain contributes its own instant.
///
/// omc schedules events at `t = 0.3` and `t = 0.7` for this chain. Which branch
/// *runs* at `t = 0.7` is a branch-priority question this file does not settle
/// (rumoca and omc disagree there — see the module header); that both instants
/// are scheduled, and that the first branch runs at its own instant, is the
/// event-owner property under test.
#[test]
fn else_when_schedules_every_branch_instant() {
    let (compiled, sim) = simulate("ElseWhen", ELSE_WHEN);
    assert_eq!(
        event_owners(&compiled),
        (0, 2),
        "each branch relation over `time` owns its own instant"
    );
    assert_eq!(value_at(&sim, "y", 0.25), 0.0);
    assert_eq!(
        value_at(&sim, "y", 0.3 + 1.0e-4),
        1.0,
        "the first branch runs at its own instant, as omc does"
    );
}

/// The MLS §8.5 vector activation `when {initial(), time > 0.5}` compiles and
/// keeps both of its owners.
///
/// §8.5 gives exactly two spellings for enabling a `when` at initialization:
/// *"`when initial() then` or `when {…, initial(), …} then`"*. The second was
/// rejected outright — condition validation walked `and`/`or`/`not` but handed a
/// vector to ordinary expression validation, which has no owner for `initial()`
/// — so the standard idiom did not compile at all.
///
/// omc: `y = 1` from `t = 0` (the `initial()` element), `1, 1, 2` across the
/// three `t = 0.5` rows, `y = 2` after. rumoca reproduces both steps.
#[test]
fn initial_alternative_in_a_vector_activation_keeps_both_owners() {
    let (compiled, sim) = simulate("InitialAlternative", INITIAL_ALTERNATIVE);
    assert_eq!(
        event_owners(&compiled),
        (0, 1),
        "the `time` element owns an instant; `initial()` owns no crossing"
    );
    assert_eq!(
        value_at(&sim, "y", 0.45),
        1.0,
        "the `initial()` element runs the body once at t = 0, as omc does"
    );
    assert_eq!(
        value_at(&sim, "y", AFTER_INSTANT),
        2.0,
        "and the `time` element runs it again at its instant, as omc does"
    );
    assert_eq!(value_at(&sim, "y", 1.0), 2.0);
}

const TWO_INSTANCES: &str = "model Trigger
  parameter Real threshold = 0.5;
  discrete Real y(start = 0, fixed = true);
equation
  when time > threshold then
    y = 1;
  end when;
end Trigger;

model TwoInstances
  Real x(start = 0, fixed = true);
  Trigger early(threshold = 0.33);
  Trigger late(threshold = 0.73);
equation
  der(x) = 1;
end TwoInstances;
";

const ARRAY_INSTANCES: &str = "model ArrayTrigger
  parameter Real threshold = 0.5;
  discrete Real y(start = 0, fixed = true);
equation
  when time > threshold then
    y = 1;
  end when;
end ArrayTrigger;

model ArrayInstances
  Real x(start = 0, fixed = true);
  ArrayTrigger t[2](threshold = {0.33, 0.73});
equation
  der(x) = 1;
end ArrayInstances;
";

const START_INSTANT: &str = "model StartInstant
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time > 0 then
    y = pre(y) + 1;
  end when;
end StartInstant;
";

/// Each instance of a replicated activation keeps its own instant.
///
/// Flattening gives both instances of `Trigger` the one written
/// `time > threshold`, so both occurrences carry the same source span and are
/// told apart only by the threshold each resolved. An owner map addressed by
/// span alone keeps the first instant, drops the second, and then suppresses the
/// zero crossing of *both* — so the late instance ends up with no event owner at
/// all and never fires, however long the run.
///
/// omc: `early.y` steps to 1 at `t = 0.33` (its right-limit row is stamped
/// `0.3300000001330113`), `late.y` at `t = 0.73`; both hold to `t = 1`.
#[test]
fn two_instances_of_one_class_each_fire_at_their_own_instant() {
    let (compiled, sim) = simulate("TwoInstances", TWO_INSTANCES);
    assert_eq!(
        event_owners(&compiled),
        (0, 2),
        "two occurrences of one span own two instants and no crossing"
    );
    assert_eq!(value_at(&sim, "early.y", 0.3), 0.0);
    assert_eq!(
        value_at(&sim, "early.y", 0.35),
        1.0,
        "the early instance fires at 0.33, as omc does"
    );
    assert_eq!(
        value_at(&sim, "late.y", 0.7),
        0.0,
        "and the late instance has not fired yet"
    );
    assert_eq!(
        value_at(&sim, "late.y", 0.75),
        1.0,
        "the late instance fires at 0.73, as omc does"
    );
    assert_eq!(value_at(&sim, "late.y", 1.0), 1.0);
}

/// The same, for an array instantiation.
///
/// `t[2](threshold = {0.33, 0.73})` is the denser form of the same replication,
/// and it is how MSL writes a bank of identical blocks.
///
/// omc: `t[1].y` steps to 1 at `t = 0.33`, `t[2].y` at `t = 0.73`.
#[test]
fn array_instances_each_fire_at_their_own_instant() {
    let (compiled, sim) = simulate("ArrayInstances", ARRAY_INSTANCES);
    assert_eq!(event_owners(&compiled), (0, 2));
    assert_eq!(value_at(&sim, "t[1].y", 0.3), 0.0);
    assert_eq!(value_at(&sim, "t[1].y", 0.35), 1.0);
    assert_eq!(
        value_at(&sim, "t[2].y", 0.7),
        0.0,
        "the second element must not inherit the first's instant"
    );
    assert_eq!(
        value_at(&sim, "t[2].y", 0.75),
        1.0,
        "the second element fires at its own 0.73, as omc does"
    );
}

/// An activation whose instant is the start of the interval still fires — once.
///
/// The start is not an instant at which anything *changes*: initialization has
/// already fixed every relation's buffered value there, so scheduling a stop at
/// it produces a stop at which `time > 0` still reads false, with no later event
/// to retry. Left to its crossing instead, the change is located at the start
/// and applied at the right limit.
///
/// omc: `y = 0` in the `t = 0` row, `y = 1` from the right-limit row at
/// `t = 1.000000000124e-10` onward — one activation, not none and not two. The
/// state must also come through unscathed: `x` reaches exactly `1` at `t = 1`,
/// with no interval of integration dropped at the boundary.
#[test]
fn an_activation_at_the_start_instant_fires_exactly_once() {
    let (compiled, sim) = simulate("StartInstant", START_INSTANT);
    assert_eq!(
        event_owners(&compiled),
        (1, 0),
        "the start instant is owned by a crossing, not by a scheduled stop"
    );
    assert_eq!(
        value_at(&sim, "y", 0.05),
        1.0,
        "`when time > 0` must fire, as omc does"
    );
    assert_eq!(
        value_at(&sim, "y", 1.0),
        1.0,
        "and must fire exactly once, not once per later event"
    );
    let x_end = value_at(&sim, "x", 1.0);
    assert!(
        (x_end - 1.0).abs() < 1.0e-9,
        "the boundary must not discard an interval of integration: x(1) = {x_end}"
    );
}

const STEP_AT_START: &str = "model StepAtStart
  parameter Real startTime = 0;
  Real x(start = 0, fixed = true);
  Real y;
equation
  der(x) = 1;
  y = if time < startTime then 0 else 1;
end StepAtStart;
";

const STEP_LATER: &str = "model StepLater
  parameter Real startTime = 0.5;
  Real x(start = 0, fixed = true);
  Real y;
equation
  der(x) = 1;
  y = if time < startTime then 0 else 1;
end StepLater;
";

/// The start-instant rule holds for a relation written in an equation too.
///
/// `an_activation_at_the_start_instant_fires_exactly_once` pins the rule for a
/// `when` activation; this pins it for the other collector, the one that walks
/// equation residuals. The two share `time_event_instant`'s bound but reach it
/// by different paths, and the shape that reaches it through the residual walk
/// is the one MSL writes everywhere: `Modelica.Blocks.Sources.Step` is
/// `y = offset + (if time < startTime then 0 else height)`, and its default
/// `startTime = 0` puts the crossing exactly on the start.
///
/// Both directions of the bound are asserted because the bound *is* the
/// behaviour. A threshold at the start is not an instant at which anything
/// changes — §8.5 defines an event as the instant an event generating expression
/// *changes* value, and initialization has already fixed this one there — so it
/// stays an ordinary crossing and owns a root. A threshold after the start is
/// the §8.5 special relation `time < discrete expression` and is scheduled
/// exactly, owning no root. Scheduling the start instant instead was live for
/// the whole of `Modelica.Magnetic.FluxTubes.Examples.BasicExamples`
/// `SaturatedInductor`, which reaches this relation through its voltage source.
///
/// omc: `y = 1` from the `t = 0` row on for `StepAtStart` — the source is past
/// its start time for the entire run — and `y = 0` through `t = 0.45` then `1`
/// from the `t = 0.5` right-limit row on for `StepLater`.
#[test]
fn an_equation_relation_at_the_start_instant_keeps_its_crossing() {
    let (compiled, sim) = simulate("StepAtStart", STEP_AT_START);
    assert_eq!(
        event_owners(&compiled),
        (1, 0),
        "a threshold at the start is a crossing, not a scheduled stop"
    );
    assert_eq!(
        value_at(&sim, "y", 0.0),
        1.0,
        "`time < 0` is false from the start, so the else branch holds"
    );
    assert_eq!(value_at(&sim, "y", 1.0), 1.0, "and holds for the whole run");

    let (compiled, sim) = simulate("StepLater", STEP_LATER);
    assert_eq!(
        event_owners(&compiled),
        (0, 1),
        "a threshold after the start is scheduled exactly, owning no root"
    );
    assert_eq!(value_at(&sim, "y", 0.45), 0.0);
    assert_eq!(
        value_at(&sim, "y", AFTER_INSTANT),
        1.0,
        "the scheduled instant must switch the branch, as omc does"
    );
}

const BDF_SCHEDULED: &str = "model BdfScheduled
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time > 0.5 then
    y = 1;
  end when;
end BdfScheduled;
";

const RESCHEDULING_VECTOR: &str = "model ReschedulingVector
  Real x(start = 0, fixed = true);
  discrete Real nextEvent(start = 0.25, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when {time >= pre(nextEvent), initial()} then
    nextEvent = pre(nextEvent) + 0.25;
    y = pre(y) + 1;
  end when;
end ReschedulingVector;
";

/// The scheduled instant is the diffsol session's too, and it costs no state.
///
/// The module header records two ways that session diverges, both of them about
/// the *start* instant. This pins the part it gets right, so neither record is
/// read as a claim that scheduling is broken there: the activation fires at the
/// instant's right limit, and `x` arrives at that limit having actually been
/// integrated to it.
///
/// The second assertion is the one with teeth. Before this change the session
/// applied a scheduled event at the right limit while leaving the continuous
/// state at the instant, silently discarding `2 * atol` of integration at every
/// scheduled event: `x` read `0.5` in the `t = 0.500002` row and stayed `2e-6`
/// short for the rest of the run.
///
/// omc: `y = 0` through `t = 0.45`, `y = 1` from the right-limit row on, and
/// `x = t` exactly throughout.
#[test]
fn the_diffsol_session_agrees_on_a_scheduled_instant() {
    let compiled = rumoca::Compiler::new()
        .model("BdfScheduled")
        .compile_str(BDF_SCHEDULED, "time_event_when_activation.mo")
        .expect("BdfScheduled should compile");
    let sim = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.05),
            solver_mode: SimSolverMode::Bdf,
            ..SimOptions::default()
        },
    )
    .expect("BdfScheduled should simulate");

    assert_eq!(event_owners(&compiled), (0, 1));
    assert_eq!(value_at(&sim, "y", 0.45), 0.0);
    assert_eq!(
        value_at(&sim, "y", AFTER_INSTANT),
        1.0,
        "the diffsol session must run the body at the instant too"
    );
    let x_end = value_at(&sim, "x", 1.0);
    assert!(
        (x_end - 1.0).abs() < 1.0e-9,
        "a scheduled event must not discard the interval it steps over: x(1) = {x_end}"
    );
}

/// A vector activation whose `time` threshold reschedules itself is rejected.
///
/// `when {time >= pre(nextEvent), initial()} then nextEvent := …` is the shape
/// `Modelica.Blocks.Sources.TimeTable` and `CombiTimeTable` are written in: the
/// event the activation fires sets the instant it next fires at. rumoca has no
/// checked owner for that reschedule, and what it produces is not a missing
/// trace but a wrong one — the threshold stalls, so the `when` fires a few times
/// at the wrong instants and stops (`y` reaching 3 where omc counts 1, 2, 3, 4
/// at `t = 0`, `0.5`, `0.75`, `1`).
///
/// Until the vector form was validated element-wise, it was the `initial()`
/// element that made these models fail. That was an accident, and validating the
/// elements properly — which is what buys `{initial(), time > 0.5}` its
/// agreement with omc — removed it. This test pins the reason that replaced it,
/// so the gap stays loud instead of becoming a silently wrong trace.
///
/// The rejection is gated on `initial()` and reaches no further. A vector
/// activation of plain relations, `when {time >= (pre(count) + 1) * period,
/// false}`, simulates correctly and is covered by
/// `periodic_source_counter_regression`; the same accumulating lag is reachable
/// through the scalar `when time >= pre(next)` with or without this module. Only
/// the `initial()`-bearing form was newly reachable, and only it is rejected.
#[test]
fn a_rescheduling_vector_activation_is_rejected_rather_than_mis_simulated() {
    let Err(error) = rumoca::Compiler::new()
        .model("ReschedulingVector")
        .compile_str(RESCHEDULING_VECTOR, "time_event_when_activation.mo")
    else {
        panic!("a rescheduled `initial()` vector activation has no checked owner yet");
    };
    let text = error.to_string();
    assert!(
        text.contains("moved by the event itself"),
        "the rejection must name the reschedule gap, got: {text}"
    );
}
