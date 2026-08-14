//! When a `when` activation fires: the §8.5 instant it owns, the §8.3.5.1 edge
//! that runs its body, and the §8.3.5 priority between the branches of a chain.
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
//! # Activation at the initial instant
//!
//! MLS §8.6: *"The equations of a when-clause are active during initialization,
//! if and only if they are explicitly enabled with `initial()`"*, in one of the
//! two forms `when initial() then` or `when {…, initial(), …} then`. Everything
//! else must have a rising edge to run, and §8.3.5.1 says where the edge comes
//! from: a `when` is conceptually
//!
//! ```text
//! Boolean b(start = x.start > 2);
//! b  = x > 2;
//! v1 = if edge(b) then expr1 else pre(v1);
//! ```
//!
//! — the activation buffer *starts at the condition's own value*, and §8.6's
//! *"Before the start of the integration, it must be guaranteed that for all
//! variables `v`, `v = pre(v)`"* keeps `pre(b) = b` there. A condition already
//! true at `t = 0` therefore has no edge at the initial event, and one that is
//! true at `t = 0` and only ever falls has no edge for the whole run.
//!
//! rumoca left that buffer at `false`, which manufactured an edge at the initial
//! event for every already-true activation, on both solver paths. The buffer is
//! now seeded from the settled initialization values, with the `initial()` flag
//! cleared — `initial()` is false everywhere except the initial event, so it is
//! the one activation that keeps its edge there.
//!
//! | model                                    | omc | rumoca before          | rumoca after |
//! | ---------------------------------------- | --- | ---------------------- | ------------ |
//! | `when time < 0.5 then y = 1`             | 0   | 1 at `t = 0` (bdf)     | 0            |
//! | `when x < 2 then y = 1`, `der(x) = 1`    | 0   | 1 at `t = 0` (bdf)     | 0            |
//! | `when time < 0.5 and x < 2 then y = 1`   | 0   | 1 at `t = 0` (bdf)     | 0            |
//! | the two above, beside a `when initial()` | 0   | 1 at `t = 0` (both)    | 0            |
//! | `when not (time > 0.5) then y = 1`       | 0   | 1 from `t = 0.5` (rk)  | 0            |
//! | `when initial() then y = 1`              | 1   | 1                      | 1            |
//! | `when {initial(), time > 0.5}`           | 1→2 | 1→2                    | 1→2          |
//!
//! The `when initial()` row in that table is what makes the rest safe to read:
//! the seed removes spurious activations without removing the §8.6 one. The
//! `and`/`beside a when initial()` rows are what identify the defect as *one*
//! bug rather than two: before the fix the rk-like session happened to agree on
//! the first two models only because they own no initial event at all, and
//! adding a `when initial()` — which creates one — made it disagree exactly as
//! the diffsol session did.
//!
//! # Vector activations: one buffer per element
//!
//! §8.3.5 activates on *"any of the elements … becomes true"*, and §8.3.5.1
//! realises that as one `Boolean bi` per element with the activation
//! `edge(b1) or … or edge(bn)`. That is not the edge of the disjunction, and
//! `omc` is where the difference is visible rather than merely arguable:
//!
//! | model                              | omc                     |
//! | ---------------------------------- | ----------------------- |
//! | `when {true, time > 0.5}`          | fires at `t = 0.5`      |
//! | `when true or time > 0.5`          | never fires             |
//! | `when {u, not u}`, `u = time > 0.3`| fires at `t = 0.3`      |
//! | `when u or not u`                  | never fires             |
//!
//! rumoca folded a vector into one `Or` with one buffer, which makes the two
//! rows of each pair indistinguishable — and once the buffer is seeded, a
//! tautological disjunction seeds *true* and the activation is deleted outright.
//!
//! Three MSL blocks are written in that shape:
//! `Modelica.Blocks.Logical.TriggeredTrapezoid` and `LogicalDelay`
//! (`when {u, not u}`), and `Modelica.Blocks.Math.ContinuousSignalExtrema`
//! (`when {u <= x, u >= x, terminal()}`). **None of the three compiles in rumoca
//! today**, for reasons unrelated to activation — `initial()` inside an
//! expression and `terminal()` (ED018), `pre` of a continuous variable (ED019),
//! and an initialization-projection failure — so no claim is made here about
//! what they do. They are the reason the shape matters, not evidence about it.
//! The evidence is the OpenModelica table above and the tests below, measured on
//! the shape itself.
//!
//! # `when <literal>` is a `when`
//!
//! A constant activation still has an activation buffer. §8.3.5.1 starts it at
//! the condition's own value and §8.6 holds `pre(b) = b` before integration, so
//! `when true then` never has a rising edge and never runs — `omc` leaves
//! `y = 0` for `when true then y = pre(y) + 1`, and the same for a
//! `parameter Boolean b = true`. What must *not* carry a buffer is the
//! activation rumoca synthesises for an unguarded algorithm section and for a
//! section-level `assert`, neither of which is a `when` at all; those are a
//! distinct DAE node (`ConditionOperation::Always`) rather than a literal, so
//! the two can no longer be confused by shape.
//!
//! # Divergences these tests deliberately do not claim
//!
//! Two of the five below are regressions this line of work introduced and chose
//! to land; the rest were there before it and are untouched. Each says which it
//! is, because a recorded divergence that misdescribes its own origin is worse
//! than an unrecorded one.
//!
//! * **Two branches of one chain whose instants coincide — REGRESSION
//!   INTRODUCED HERE.** `when time > 0.5 then y = 1; elsewhen x > 0.5 then
//!   y = 2;` with `der(x) = 1`. Both conditions become true at `t = 0.5`, and
//!   `omc` handles them in one event iteration, where §8.3.5's branch priority
//!   picks the first: `y = 1` for the rest of the run. On the rk-like session the
//!   first branch is a *scheduled instant* and the second a *located crossing*,
//!   and the two land in different event iterations — so the second branch finds
//!   its own edge at an instant where the first no longer has one, and `y` steps
//!   to `2` at the next output sample. Before the branch guards were corrected,
//!   the level subtraction masked this: `not (time > 0.5)` was false, so the
//!   second branch was suppressed and `y` stayed `1` by accident. **`y` regressed
//!   from 1 to 2 relative to the pre-change tree** on this shape. The cause is
//!   the iteration split, not the guard — `omc` reaches the same answer as
//!   rumoca whenever the two instants really are distinct — and it is the same
//!   rk-like defect as the record below. The diffsol session leaves `y = 0` here,
//!   before and after.
//! * **A scheduled instant coinciding with a state crossing — REGRESSION
//!   INTRODUCED EARLIER, same rk-like cause.** `when time > 0.5 then a = 1` beside
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
//! * **The diffsol session and a located state crossing** (pre-existing). On
//!   `SimSolverMode::Bdf` a crossing over a continuous state is located far from
//!   where `omc` puts it: `when x > 0.3 then y = 1` with `der(x) = 1` first
//!   reads `y = 1` at `t = 0.7` rather than `0.3`, and the second branch of a
//!   chain over the same state never arrives inside a `[0, 1]` run. Measured
//!   identical before and after this change, so it is that session's root
//!   location and not the activation semantics; this file therefore names its
//!   solver mode explicitly rather than asserting whatever the default happens
//!   to be. [`the_diffsol_session_agrees_on_a_scheduled_instant`] pins the part
//!   of that session the schedule *does* reach.
//! * **The diffsol session applies the initial event more than once**
//!   (pre-existing). Every `when` that legitimately runs at `t = 0` runs two or
//!   three times there on `SimSolverMode::Bdf`: `when true then y = pre(y) + 1`
//!   reads `y = 3` at `t = 0` before the activation buffers existed and `y = 2`
//!   after, where `omc` leaves `0`. The count moved because the buffer removes
//!   one of the applications, not because the multiple application was addressed
//!   — it is the diffsol initial-event boundary running its update more than
//!   once, and it is why every assertion below that reads `t = 0` on both
//!   sessions reads a *level* (fired / did not fire) rather than a count.
//! * **A scalar `initial()` inside a larger condition enables the whole
//!   condition** (pre-existing). MLS §8.6 enables a when-clause during
//!   initialization in exactly two spellings, `when initial() then` and
//!   `when {…, initial(), …} then`; `omc` honours that and does not enable
//!   `when initial() or time > 0.5`, nor `when not initial()`. rumoca treats any
//!   condition containing `initial()` as enabling. The vector spelling is
//!   correct (see [`initial_alternative_in_a_vector_activation_keeps_both_owners`]
//!   and [`an_initial_element_does_not_enable_an_already_true_element`]); the
//!   scalar one is a separate §8.6 admissibility question and no test below
//!   asserts it.
//! * **A `when` body reading a variable the same body assigns — SPEC VIOLATION**
//!   (pre-existing). MLS §8.3.5.1 expands a `when` body to *simultaneous
//!   equations*, one `v = if edge(b) then expr else pre(v)` per assigned
//!   variable. They are equations, not statements, so a body that reads `a`
//!   after assigning it must see the **new** `a`. rumoca evaluates the rows of
//!   an event pass sequentially against the event-entry snapshot, so it sees the
//!   old one.
//!
//!   ```modelica
//!   when time > 0.5 then
//!     a = pre(a) + 1;
//!     b = if a > pre(a) then 100 else -100;
//!   end when;
//!   ```
//!
//!   `omc` gives `b = 100`; rumoca gives `b = -100`, on both solver sessions and
//!   both before and after this change (`a` itself is correct in all four). The
//!   same defect in its cross-`when` form is `when pre(b) then …` beside
//!   `when time > 0.7 then b = false;`, where the two `when`s disagree about
//!   which `b` the instant `t = 0.7` sees.
//!
//!   This is a §8.3.5.1 violation rather than a presentation difference, and it
//!   is the whole of what still separates rumoca from `omc` on
//!   `Modelica.Blocks.Math.ContinuousSignalExtrema`'s shape: its `t_min`/`t_max`
//!   bodies are `if y_min < pre(y_min) then time else pre(t_min)`, read after
//!   `y_min` is assigned in the same body, and they stay at their start values
//!   for exactly this reason. Its `y_min`/`y_max` — which read nothing the body
//!   assigns — track `omc` exactly. Fixing it is a separate change to
//!   event-pass row scheduling; no test below asserts the violated shape.
//! * **A `discrete Boolean` assigned by a relation equation** (pre-existing, and
//!   not about `when` at all). `discrete Boolean b; b = time < 0.5; when b then
//!   y = 1;` — `omc` gives `b = 1` until `t = 0.5` and `0` after. rumoca never
//!   updates `b` at all on the rk-like session (`b = 0` throughout) and pins it
//!   at `1` for the whole run on the diffsol one. Both `y` traces follow from
//!   `b`, so nothing here can be read as an activation claim, and no test below
//!   asserts this shape.

use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};

/// Compile `source` and run it over `[0, 1]` with a 0.05 output interval — the
/// grid the OpenModelica runs in the module table used.
///
/// Every model here carries a `der(x) = 1` integrator: the rk-like session
/// refuses a model with no state equations. An unused integrator cannot move any
/// `y`, so the OpenModelica values in the table are unaffected by it.
///
/// The entry point matters: plain `simulate_dae` is the diffsol module's own
/// function and runs diffsol whatever `solver_mode` says, so a test that used it
/// would silently assert one backend while claiming the other. This dispatches
/// on the mode, and the mode is the explicit rk-like session — the path the
/// OpenModelica comparison in the module header was measured against.
fn simulate(name: &str, source: &str) -> (rumoca::CompilationResult, SimResult) {
    simulate_on(SimSolverMode::RkLike, name, source)
}

/// The same run on a named solver session.
///
/// Activation is a property of the model, not of the integrator, so the
/// initial-instant tests below assert both sessions from one model rather than
/// letting one path's answer stand for both. That distinction is not
/// theoretical here: the two sessions disagreed about `when time < 0.5` before
/// the activation buffer was seeded, and the rk-like one was right only for
/// models that own no initial event.
fn simulate_on(
    mode: SimSolverMode,
    name: &str,
    source: &str,
) -> (rumoca::CompilationResult, SimResult) {
    let compiled = rumoca::Compiler::new()
        .model(name)
        .compile_str(source, "time_event_when_activation.mo")
        .unwrap_or_else(|error| panic!("`{name}` should compile: {error}"));
    let sim = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.05),
            solver_mode: mode,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("`{name}` should simulate on {mode:?}: {error}"));
    (compiled, sim)
}

/// Both solver sessions, so an assertion cannot pass by naming the lucky one.
const SESSIONS: [SimSolverMode; 2] = [SimSolverMode::RkLike, SimSolverMode::Bdf];

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
/// only ever falls, and a `when` runs on a rising edge.
#[test]
fn falling_time_activation_does_not_fire_at_its_instant() {
    let (compiled, sim) = simulate("Falling", FALLING);
    assert_eq!(
        event_owners(&compiled),
        (0, 1),
        "`when time < 0.5` owns the same instant as the rising spellings"
    );
    assert_eq!(
        value_at(&sim, "y", 0.0),
        0.0,
        "`time < 0.5` is true at t = 0, and MLS §8.3.5.1 starts its activation \
         buffer there, so the initial event has no edge to run"
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

const STATE_CONDITION_TRUE_AT_START: &str = "model StateConditionTrueAtStart
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when x < 2 then
    y = 1;
  end when;
end StateConditionTrueAtStart;
";

const COMPOUND_TRUE_AT_START: &str = "model CompoundTrueAtStart
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time < 0.5 and x < 2 then
    y = 1;
  end when;
end CompoundTrueAtStart;
";

const INITIAL_ONLY: &str = "model InitialOnly
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when initial() then
    y = 1;
  end when;
end InitialOnly;
";

const INITIAL_BESIDE_FALLING: &str = "model InitialBesideFalling
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
  discrete Real z(start = 0, fixed = true);
equation
  der(x) = 1;
  when initial() then
    z = 1;
  end when;
  when time < 0.5 then
    y = 1;
  end when;
end InitialBesideFalling;
";

const INITIAL_BESIDE_STATE_CONDITION: &str = "model InitialBesideStateCondition
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
  discrete Real z(start = 0, fixed = true);
equation
  der(x) = 1;
  when initial() then
    z = 1;
  end when;
  when x < 2 then
    y = 1;
  end when;
end InitialBesideStateCondition;
";

const NEGATED_TIME: &str = "model NegatedTime
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when not (time > 0.5) then
    y = 1;
  end when;
end NegatedTime;
";

/// `y` never leaves zero, on either session.
///
/// The whole run is asserted, not only `t = 0`: a condition true at the start
/// must neither run at the initial event nor pick up a spurious edge at some
/// later instant it happens to own.
fn assert_never_activates(name: &str, source: &str, model: &str) {
    for mode in SESSIONS {
        let (_, sim) = simulate_on(mode, name, source);
        for t in [0.0, 0.25, 0.45, AFTER_INSTANT, 0.55, 1.0] {
            assert_eq!(
                value_at(&sim, "y", t),
                0.0,
                "`{model}` on {mode:?}: omc leaves y = 0 for the whole run; \
                 t = {t} must not activate an already-true condition"
            );
        }
    }
}

/// A condition already true at `t = 0` does not run its body at the initial
/// event, on either session.
///
/// MLS §8.6 activates a when-clause during initialization *"if and only if"* it
/// is enabled with `initial()`, and §8.3.5.1 says why `time < 0.5` is not
/// enabled: its activation buffer starts at the condition's own value, so
/// `edge = b and not pre(b)` is false at the initialization instant. omc leaves
/// `y = 0` for the whole run.
#[test]
fn a_falling_time_condition_never_activates_on_either_session() {
    assert_never_activates("Falling", FALLING, "when time < 0.5");
}

/// The same for a relation over a continuous state, which is what rules `time`
/// out as the axis.
///
/// `x` starts at `0`, so `x < 2` is true at the initialization instant and stays
/// true past `t = 1`. omc leaves `y = 0`.
#[test]
fn a_state_condition_true_at_the_start_never_activates_on_either_session() {
    assert_never_activates(
        "StateConditionTrueAtStart",
        STATE_CONDITION_TRUE_AT_START,
        "when x < 2",
    );
}

/// And for a conjunction of the two, so the seed is shown to reach a compound
/// activation and not only a leaf.
///
/// omc leaves `y = 0`.
#[test]
fn a_compound_condition_true_at_the_start_never_activates_on_either_session() {
    assert_never_activates(
        "CompoundTrueAtStart",
        COMPOUND_TRUE_AT_START,
        "when time < 0.5 and x < 2",
    );
}

/// A negated `time` relation never activates either — including at the instant
/// it owns.
///
/// `not (time > 0.5)` is true from the start and falls at `t = 0.5`. Its buffer
/// is seeded true, so the scheduled instant it owns finds no rising edge; with
/// the buffer left at `false` the rk-like session ran the body *at* `t = 0.5`,
/// where §8.5's buffered `time > 0.5` still reads false and the negation reads
/// true. omc leaves `y = 0` for the whole run.
#[test]
fn a_negated_time_condition_never_activates_on_either_session() {
    assert_never_activates("NegatedTime", NEGATED_TIME, "when not (time > 0.5)");
}

/// `when initial()` still runs at the initial event, on either session.
///
/// This is the assertion that keeps the seed honest. §8.6 names `initial()` as
/// the one enabler of a when-clause during initialization, and the seed is taken
/// with the `initial()` flag cleared precisely so that this activation keeps the
/// edge every other one loses. omc: `y = 1` from the `t = 0` row on.
#[test]
fn an_initial_activation_still_runs_at_the_initial_event_on_either_session() {
    for mode in SESSIONS {
        let (_, sim) = simulate_on(mode, "InitialOnly", INITIAL_ONLY);
        assert_eq!(
            value_at(&sim, "y", 0.0),
            1.0,
            "`when initial()` must run at the initial event on {mode:?}, as omc does"
        );
        assert_eq!(
            value_at(&sim, "y", 1.0),
            1.0,
            "and must run exactly once, not once per later event"
        );
    }
}

/// An already-true condition standing *beside* a `when initial()` still does not
/// run.
///
/// This pair is what proves the two sessions had one bug rather than two. Before
/// the seed, the rk-like session left `y = 0` for
/// [`a_falling_time_condition_never_activates_on_either_session`]'s model — not
/// because it applied §8.6, but because that model owns no initial event at all.
/// Adding a `when initial()` creates one, and the rk-like session then ran the
/// falling body at `t = 0` exactly as the diffsol session did.
///
/// omc: `z = 1` at `t = 0`, `y = 0` for the whole run, in both models.
#[test]
fn an_initial_event_does_not_activate_the_conditions_beside_it() {
    for (name, source, model) in [
        (
            "InitialBesideFalling",
            INITIAL_BESIDE_FALLING,
            "when time < 0.5",
        ),
        (
            "InitialBesideStateCondition",
            INITIAL_BESIDE_STATE_CONDITION,
            "when x < 2",
        ),
    ] {
        for mode in SESSIONS {
            let (_, sim) = simulate_on(mode, name, source);
            assert_eq!(
                value_at(&sim, "z", 0.0),
                1.0,
                "`{name}` on {mode:?}: the `when initial()` beside `{model}` must run"
            );
            for t in [0.0, 0.45, 1.0] {
                assert_eq!(
                    value_at(&sim, "y", t),
                    0.0,
                    "`{name}` on {mode:?}: the initial event `when initial()` creates \
                     must not activate `{model}` at t = {t}, as omc does not"
                );
            }
        }
    }
}

const WHEN_LITERAL_TRUE: &str = "model WhenLiteralTrue
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
  discrete Real ticks(start = 0, fixed = true);
equation
  der(x) = 1;
  when sample(0.1, 0.1) then
    ticks = pre(ticks) + 1;
  end when;
  when true then
    y = pre(y) + 1;
  end when;
end WhenLiteralTrue;
";

const WHEN_PARAMETER_TRUE: &str = "model WhenParameterTrue
  parameter Boolean b = true;
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when b then
    y = pre(y) + 1;
  end when;
end WhenParameterTrue;
";

const WHEN_LITERAL_TRUE_BARE: &str = "model WhenLiteralTrueBare
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when true then
    y = pre(y) + 1;
  end when;
end WhenLiteralTrueBare;
";

const VECTOR_TAUTOLOGY: &str = "model VectorTautology
  Real x(start = 0, fixed = true);
  Boolean u;
  discrete Real n(start = 0, fixed = true);
equation
  der(x) = 1;
  u = time > 0.3;
  when {u, not u} then
    n = pre(n) + 1;
  end when;
end VectorTautology;
";

const SCALAR_TAUTOLOGY: &str = "model ScalarTautology
  Real x(start = 0, fixed = true);
  Boolean u;
  discrete Real n(start = 0, fixed = true);
equation
  der(x) = 1;
  u = time > 0.3;
  when u or not u then
    n = pre(n) + 1;
  end when;
end ScalarTautology;
";

const VECTOR_LITERAL_ELEMENT: &str = "model VectorLiteralElement
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when {true, time > 0.5} then
    y = pre(y) + 1;
  end when;
end VectorLiteralElement;
";

const SCALAR_LITERAL_DISJUNCT: &str = "model ScalarLiteralDisjunct
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when true or time > 0.5 then
    y = pre(y) + 1;
  end when;
end ScalarLiteralDisjunct;
";

const VECTOR_INITIAL_WITH_TRUE_ELEMENT: &str = "model VectorInitialWithTrueElement
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when {initial(), time < 0.5} then
    y = pre(y) + 1;
  end when;
end VectorInitialWithTrueElement;
";

/// A model author's `when true then` never runs — on either session.
///
/// MLS §8.3.5.1 starts the activation buffer at the condition's own value and
/// §8.6 keeps `pre(b) = b` before integration, so a constant condition has no
/// rising edge, ever. `omc` leaves `y = 0` for the whole run.
///
/// The `sample` beside it is what gives the model events to run over: without
/// one, "never fired" is indistinguishable from "was never asked". The buffer
/// for the synthesised algorithm-section activation is deliberately *not* built
/// this way (it is `ConditionOperation::Always`), and this test is what stops
/// the two being confused again — a structural "is this a literal?" test would
/// give this `when` the level activation of an algorithm section and count every
/// sample tick.
#[test]
fn a_source_when_over_a_literal_never_runs() {
    let (_, sim) = simulate("WhenLiteralTrue", WHEN_LITERAL_TRUE);
    assert_eq!(
        value_at(&sim, "ticks", 1.0),
        10.0,
        "the sample beside it must have produced ten events, so `y = 0` below is \
         \"never fired\" and not \"never asked\""
    );
    for t in [0.0, 0.25, 0.55, 1.0] {
        assert_eq!(
            value_at(&sim, "y", t),
            0.0,
            "`when true` has no rising edge at t = {t}, as omc has none"
        );
    }
}

/// A literal condition and a `parameter Boolean` condition agree.
///
/// Neither can change, so §8.3.5.1 gives both a buffer that starts true, and
/// `omc` leaves `y = 0` for both. Asserting them together is what keeps the
/// answer a statement about constancy rather than about syntax: a rule that
/// looked at expression shape would separate them.
#[test]
fn a_literal_and_a_parameter_boolean_activation_agree() {
    for (name, source) in [
        ("WhenLiteralTrueBare", WHEN_LITERAL_TRUE_BARE),
        ("WhenParameterTrue", WHEN_PARAMETER_TRUE),
    ] {
        let (_, sim) = simulate(name, source);
        for t in [0.0, 0.45, 1.0] {
            assert_eq!(
                value_at(&sim, "y", t),
                0.0,
                "`{name}` must not run at t = {t}: a constant activation never rises, \
                 and omc leaves y = 0"
            );
        }
    }
}

/// `when {u, not u}` fires on the element that rises; `when u or not u` never
/// fires.
///
/// This pair is the whole of MLS §8.3.5's vector rule. The two models have the
/// same operands and differ only in the spelling, and `omc` separates them:
/// `n` steps to 1 at `t = 0.3` for the vector and stays 0 for the scalar
/// disjunction, which is a tautology and so never becomes true.
///
/// The vector spelling is not a curiosity — it is how
/// `Modelica.Blocks.Logical.TriggeredTrapezoid` and `LogicalDelay` are written.
/// Neither compiles in rumoca yet (see the module header), so this asserts the
/// shape rather than those blocks; folding a vector into one buffered `Or` makes
/// both models here produce the scalar answer, which is what would reach them.
#[test]
fn a_vector_activation_rises_where_its_disjunction_cannot() {
    let (_, sim) = simulate("VectorTautology", VECTOR_TAUTOLOGY);
    assert_eq!(value_at(&sim, "n", 0.25), 0.0);
    assert_eq!(
        value_at(&sim, "n", 0.3 + 1.0e-4),
        1.0,
        "the element `u` becomes true at t = 0.3, so the vector activates there, \
         as omc does"
    );
    assert_eq!(
        value_at(&sim, "n", 1.0),
        1.0,
        "and exactly once: `not u` only ever falls"
    );

    let (_, sim) = simulate("ScalarTautology", SCALAR_TAUTOLOGY);
    for t in [0.0, 0.25, 0.3 + 1.0e-4, 1.0] {
        assert_eq!(
            value_at(&sim, "n", t),
            0.0,
            "`u or not u` is true from the start and never becomes true, so it never \
             activates at t = {t} — omc leaves n = 0"
        );
    }
}

/// The same separation with a literal element, which is where `omc`'s two
/// answers are furthest apart.
///
/// `when {true, time > 0.5}` fires at `t = 0.5` — the *second* element becomes
/// true there — while `when true or time > 0.5` never fires at all. One buffer
/// for the vector cannot produce both.
#[test]
fn a_literal_vector_element_does_not_suppress_the_element_beside_it() {
    let (_, sim) = simulate("VectorLiteralElement", VECTOR_LITERAL_ELEMENT);
    assert_eq!(value_at(&sim, "y", 0.45), 0.0);
    assert_eq!(
        value_at(&sim, "y", AFTER_INSTANT),
        1.0,
        "the `time > 0.5` element rises at its own instant, as omc does"
    );
    assert_eq!(value_at(&sim, "y", 1.0), 1.0);

    let (_, sim) = simulate("ScalarLiteralDisjunct", SCALAR_LITERAL_DISJUNCT);
    for t in [0.0, 0.45, AFTER_INSTANT, 1.0] {
        assert_eq!(
            value_at(&sim, "y", t),
            0.0,
            "`true or time > 0.5` is true from the start, so it never rises — omc \
             leaves y = 0 at t = {t}"
        );
    }
}

/// `when {initial(), <already true>}` runs once, at the initial event.
///
/// The `initial()` element is the §8.6 enabler and has its edge there; the
/// `time < 0.5` element beside it is already true at `t = 0`, so §8.3.5.1 starts
/// *its* buffer true and it contributes no activation — at `t = 0` or when it
/// falls at `t = 0.5`. omc: `y = 1` from the `t = 0` row and never again.
///
/// Per-element buffers are what make that reachable: with one buffer for the
/// vector, the disjunction is true at `t = 0` from the already-true element and
/// seeding it deletes the `initial()` activation as well.
#[test]
fn an_initial_element_does_not_enable_an_already_true_element() {
    let (_, sim) = simulate(
        "VectorInitialWithTrueElement",
        VECTOR_INITIAL_WITH_TRUE_ELEMENT,
    );
    assert_eq!(
        value_at(&sim, "y", 0.0),
        1.0,
        "the `initial()` element runs the body at the initial event, as omc does"
    );
    for t in [0.45, AFTER_INSTANT, 1.0] {
        assert_eq!(
            value_at(&sim, "y", t),
            1.0,
            "and nothing else in the vector may run it again at t = {t}"
        );
    }
}

/// Every branch of a `when`/`elsewhen` chain owns an instant *and runs at it*.
///
/// MLS §8.3.5 activates the equations of a when-equation *"only at the instant
/// when the scalar expression or any of the elements of the vector expression
/// becomes true"*, and §8.3.5.1 writes the chain as an if-expression over one
/// `edge(bi)` per branch condition. Each branch therefore runs on its own edge.
/// The earlier branch's higher priority *"can be used to resolve assignment
/// conflicts"* (§8.3.5) — it ranks branches that activate *together*, which is
/// what [`else_when_priority_resolves_simultaneous_edges`] pins.
///
/// rumoca guarded branch `i` with `cond_i and not (cond_1 or …)` — the earlier
/// branch's *level* — so a first condition that stays true suppressed every
/// later branch forever, and `y` held `1` for the whole run.
///
/// omc: `y = 0` through `t = 0.25`, `1` from the `t = 0.3` right-limit row, `2`
/// from the `t = 0.7` right-limit row, holding to `t = 1`.
#[test]
fn else_when_runs_every_branch_at_its_own_instant() {
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
    assert_eq!(
        value_at(&sim, "y", 0.65),
        1.0,
        "and holds until the second branch's own instant"
    );
    assert_eq!(
        value_at(&sim, "y", 0.7 + 1.0e-4),
        2.0,
        "the elsewhen branch runs at t = 0.7, as omc does: its own condition \
         becomes true there"
    );
    assert_eq!(value_at(&sim, "y", 1.0), 2.0);
}

const ELSE_WHEN_STATE: &str = "model ElseWhenState
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when x > 0.3 then
    y = 1;
  elsewhen x > 0.7 then
    y = 2;
  end when;
end ElseWhenState;
";

const ELSE_WHEN_THREE: &str = "model ElseWhenThree
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time > 0.3 then
    y = 1;
  elsewhen time > 0.5 then
    y = 2;
  elsewhen time > 0.7 then
    y = 3;
  end when;
end ElseWhenThree;
";

const ELSE_WHEN_SIMULTANEOUS: &str = "model ElseWhenSimultaneous
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time > 0.5 then
    y = 1;
  elsewhen time > 0.5 then
    y = 2;
  end when;
end ElseWhenSimultaneous;
";

const ELSE_WHEN_FIRST_TRUE_AT_START: &str = "model ElseWhenFirstTrueAtStart
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when time < 0.5 then
    y = 1;
  elsewhen time > 0.7 then
    y = 2;
  end when;
end ElseWhenFirstTrueAtStart;
";

const ELSE_WHEN_ALGORITHM: &str = "model ElseWhenAlgorithm
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
algorithm
  when time > 0.3 then
    y := 1;
  elsewhen time > 0.7 then
    y := 2;
  end when;
end ElseWhenAlgorithm;
";

const ELSE_WHEN_AFTER_INITIAL: &str = "model ElseWhenAfterInitial
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when initial() then
    y = 1;
  elsewhen time > 0.7 then
    y = 2;
  end when;
end ElseWhenAfterInitial;
";

/// The same chain over a continuous state, which is what identifies the defect
/// as the branch guard rather than the `time` schedule.
///
/// Both branches here are located zero crossings, not scheduled instants, so
/// nothing about §8.5's special `time` relations is involved — and the answer
/// still used to be `y = 1` forever.
///
/// omc: `y = 0` through `t = 0.25`, `1` from `t = 0.3`, `2` from `t = 0.7`. The
/// probes below sit one output interval past each crossing because a located
/// crossing is only as accurate as its root solve, which is a separate
/// property from which branch runs.
#[test]
fn else_when_over_a_state_reaches_its_second_branch() {
    let (compiled, sim) = simulate("ElseWhenState", ELSE_WHEN_STATE);
    assert_eq!(
        event_owners(&compiled),
        (2, 0),
        "a chain over a state owns two crossings and no scheduled instant"
    );
    assert_eq!(value_at(&sim, "y", 0.25), 0.0);
    assert_eq!(
        value_at(&sim, "y", 0.35),
        1.0,
        "the first branch runs at its own crossing, as omc does"
    );
    assert_eq!(
        value_at(&sim, "y", 0.75),
        2.0,
        "and the elsewhen branch at its own, as omc does"
    );
    assert_eq!(value_at(&sim, "y", 1.0), 2.0);
}

/// A three-branch chain reaches every branch, in order.
///
/// Two branches would leave open whether the fix merely swapped which single
/// branch wins. omc: `1` from `t = 0.3`, `2` from `t = 0.5`, `3` from `t = 0.7`.
#[test]
fn a_three_branch_chain_reaches_every_branch() {
    let (compiled, sim) = simulate("ElseWhenThree", ELSE_WHEN_THREE);
    assert_eq!(event_owners(&compiled), (0, 3));
    assert_eq!(value_at(&sim, "y", 0.25), 0.0);
    assert_eq!(value_at(&sim, "y", 0.3 + 1.0e-4), 1.0);
    assert_eq!(value_at(&sim, "y", INSTANT + 1.0e-4), 2.0);
    assert_eq!(value_at(&sim, "y", 0.7 + 1.0e-4), 3.0);
    assert_eq!(value_at(&sim, "y", 1.0), 3.0);
}

/// When two branch edges *are* simultaneous, the earlier branch wins.
///
/// This is the half of MLS §8.3.5 the fix must not throw away: the chain form
/// *"can be used to resolve assignment conflicts since the first of the
/// when/elsewhen parts are given higher priority than later ones"*. Both
/// branches here are written `time > 0.5`, so both edges land at the same
/// instant and only priority can decide.
///
/// omc: `y = 0` through `t = 0.45`, `1` from the `t = 0.5` right-limit row on —
/// never `2`.
#[test]
fn else_when_priority_resolves_simultaneous_edges() {
    let (compiled, sim) = simulate("ElseWhenSimultaneous", ELSE_WHEN_SIMULTANEOUS);
    assert_eq!(
        event_owners(&compiled),
        (0, 2),
        "the two spellings of one threshold still own one instant each"
    );
    assert_eq!(value_at(&sim, "y", 0.45), 0.0);
    assert_eq!(
        value_at(&sim, "y", AFTER_INSTANT),
        1.0,
        "the higher-priority branch takes the instant, as omc does"
    );
    assert_eq!(
        value_at(&sim, "y", 1.0),
        1.0,
        "and the later branch must never overwrite it"
    );
}

/// A chain whose *first* branch is already true at `t = 0` still reaches its
/// second.
///
/// This is where the two fixes meet. §8.3.5.1 starts the first branch's
/// activation buffer at `time.start < 0.5`, i.e. `true`, so that branch has no
/// edge for the whole run — and the second branch must therefore be free to run
/// at its own instant. Guarding it with the first branch's *level* would have
/// suppressed it right through `t = 0.7`, and seeding its buffer to `false`
/// would have run the first branch at `t = 0` instead.
///
/// omc: `y = 0` through `t = 0.65`, `2` from the `t = 0.7` right-limit row on.
#[test]
fn a_chain_whose_first_branch_is_true_at_the_start_reaches_its_second() {
    let (compiled, sim) = simulate("ElseWhenFirstTrueAtStart", ELSE_WHEN_FIRST_TRUE_AT_START);
    assert_eq!(event_owners(&compiled), (0, 2));
    assert_eq!(
        value_at(&sim, "y", 0.0),
        0.0,
        "`time < 0.5` is true at t = 0, so its branch has no edge to run"
    );
    assert_eq!(value_at(&sim, "y", 0.65), 0.0);
    assert_eq!(
        value_at(&sim, "y", 0.7 + 1.0e-4),
        2.0,
        "and the elsewhen branch runs at its own instant, as omc does"
    );
    assert_eq!(value_at(&sim, "y", 1.0), 2.0);
}

/// The same chain written as a `when` *statement* behaves identically.
///
/// MLS §11.2.7 gives a when-statement the same activation as a when-equation,
/// so a chain that reaches its second branch in an equation section and holds at
/// its first in an algorithm section would be two semantics for one construct.
/// The two guards are built in different functions, which is exactly why this is
/// asserted rather than assumed.
///
/// omc: `y = 0` through `t = 0.25`, `1` from `t = 0.3`, `2` from `t = 0.7`.
#[test]
fn an_algorithm_section_chain_reaches_its_second_branch_too() {
    let (_, sim) = simulate("ElseWhenAlgorithm", ELSE_WHEN_ALGORITHM);
    assert_eq!(value_at(&sim, "y", 0.25), 0.0);
    assert_eq!(value_at(&sim, "y", 0.3 + 1.0e-4), 1.0);
    assert_eq!(
        value_at(&sim, "y", 0.7 + 1.0e-4),
        2.0,
        "a `when`/`elsewhen` statement must reach its second branch, as omc does"
    );
    assert_eq!(value_at(&sim, "y", 1.0), 2.0);
}

/// `when initial() … elsewhen …` runs both, each at its own instant.
///
/// The §8.6 initial activation and a later `elsewhen` are the shape MSL sources
/// are written in, and it is the one chain where the first branch legitimately
/// fires at `t = 0`. Its edge must not suppress the second branch afterwards.
///
/// omc: `y = 1` from the `t = 0` row, `2` from the `t = 0.7` right-limit row.
#[test]
fn an_initial_branch_does_not_suppress_its_else_when() {
    let (compiled, sim) = simulate("ElseWhenAfterInitial", ELSE_WHEN_AFTER_INITIAL);
    assert_eq!(
        event_owners(&compiled),
        (0, 1),
        "`initial()` owns no crossing; the `time` branch owns its instant"
    );
    assert_eq!(
        value_at(&sim, "y", 0.0),
        1.0,
        "the `initial()` branch runs at the initial event, as omc does"
    );
    assert_eq!(value_at(&sim, "y", 0.65), 1.0);
    assert_eq!(
        value_at(&sim, "y", 0.7 + 1.0e-4),
        2.0,
        "and the elsewhen branch runs at its own instant, as omc does"
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

/// A vector activation whose `time` threshold reschedules itself owns a
/// checked dynamic deadline.
///
/// `when {time >= pre(nextEvent), initial()} then nextEvent := …` is the shape
/// `Modelica.Blocks.Sources.TimeTable` and `CombiTimeTable` are written in: the
/// event the activation fires sets the instant it next fires at. The deadline is
/// therefore not a static scheduled instant or a continuously searched root:
/// it is fixed during integration and re-evaluated after each event boundary.
///
/// The checked DAE owns that distinction explicitly. This test pins the
/// compiler contract independently of a solver implementation: one dynamic
/// time-event owner, no generic zero crossing, and no static instant.
#[test]
fn a_rescheduling_vector_activation_owns_a_dynamic_time_event() {
    let compiled = rumoca::Compiler::new()
        .model("ReschedulingVector")
        .compile_str(RESCHEDULING_VECTOR, "time_event_when_activation.mo")
        .expect("a rescheduled activation now has a checked dynamic owner");

    compiled.dae.inspect(|view| {
        assert_eq!(view.root_count(), 0, "the deadline is not a root search");
        assert_eq!(view.time_event_count(), 1);
        let event = view
            .time_event(view.time_event_id(0).expect("one time-event identity"))
            .expect("checked time-event identity resolves");
        assert!(event.instant().is_none(), "the deadline is not static");
        assert!(event.deadline().is_some(), "the dynamic deadline is owned");
    });
}
