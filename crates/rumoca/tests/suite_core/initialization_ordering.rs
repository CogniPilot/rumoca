//! MLS §8.6 ordering of the initialization instant.
//!
//! Two orderings are pinned here, both quoted from MLS 3.6 §8.6:
//!
//! * "Before the start of the integration, it must be guaranteed that for all
//!   variables `v`, `v = pre(v)`. If this is not the case for some variables
//!   `vi`, `pre(vi) := vi` must be set and an event iteration at the initial
//!   time must follow, so the model is re-evaluated, until this condition is
//!   fulfilled." — the `pre()` an initial event reads is the value the
//!   *settled* initialization result holds, not the declared `start`.
//!
//! * "All variables declared as parameter having `fixed = false` are treated as
//!   unknowns during the initialization phase, i.e., there must be additional
//!   equations for them — and the start-value can be used as a guess-value
//!   during initialization." — a parameter binding that reads such a parameter
//!   therefore cannot be finished by the parameter set, whose only available
//!   number is that guess.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae, simulate_dae_with_diagnostics};

fn simulate(source: &str, model: &str, t_end: f64) -> rumoca_sim::SimResult {
    simulate_over(source, model, 0.0, t_end)
}

fn simulate_over(source: &str, model: &str, t_start: f64, t_end: f64) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "initialization_ordering.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error:?}"));
    simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_start,
            t_end,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} should simulate: {error:?}"))
}

fn simulate_on(source: &str, model: &str, solver_mode: SimSolverMode) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "initialization_ordering.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.05,
            dt: Some(0.01),
            solver_mode,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} should simulate on {solver_mode:?}: {error:?}"))
}

fn channel_at_start(result: &rumoca_sim::SimResult, name: &str) -> f64 {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("missing channel {name}; have {:?}", result.names));
    *result.data[index]
        .first()
        .unwrap_or_else(|| panic!("channel {name} recorded no sample"))
}

/// MLS §8.6: the `pre()` an initial event reads is the settled initialization
/// value, not the declared `start`.
///
/// The `Modelica.Blocks.Sources.Pulse` period counter, started after its own
/// `startTime`. The `initial algorithm` is the §8.6 owner of the values `count`
/// and `T_start` hold when initialization finishes: at `time = 0` with
/// `startTime = -1` and `period = 0.5` that is `count = 2`, `T_start = 0`.
///
/// The equation section's `when` then re-evaluates at the initial event. Its
/// trigger and its held branch both read `pre(count)`. Taking that `pre` from
/// the *pre-settle* state (`count.start = 0`) makes the trigger `2 > 0` fire
/// and rewrites `count` to `pre(count) + 1 = 1`, discarding the whole
/// initialization result. With `pre(count) := count` set from the settled state
/// the trigger is `2 > 2`, the counter holds, and the §8.6 result survives.
const SETTLED_PRE: &str = r#"
model SettledPreAtInitialEvent
  parameter Real period = 0.5;
  parameter Real startTime = -1.0;
  Real x(start = 0, fixed = true);
  discrete Real T_start;
  Integer count;
  discrete Real seenPre(start = -1.0);
initial algorithm
  count := integer((time - startTime)/period);
  T_start := startTime + count*period;
equation
  when initial() then
    seenPre = pre(count);
  end when;
  when integer((time - startTime)/period) > pre(count) then
    count = pre(count) + 1;
    T_start = time;
  end when;
  der(x) = 0;
end SettledPreAtInitialEvent;
"#;

#[test]
fn an_initial_event_reads_pre_from_the_settled_initialization_result() {
    let result = simulate(SETTLED_PRE, "SettledPreAtInitialEvent", 0.2);
    assert_eq!(
        channel_at_start(&result, "count"),
        2.0,
        "MLS §8.6 settles count = integer((0 - -1)/0.5) = 2; the initial event must not \
         rewrite it from a stale pre(count)"
    );
    assert_eq!(
        channel_at_start(&result, "seenPre"),
        2.0,
        "MLS §8.6: pre(vi) := vi is set from the settled value before the event iteration \
         at the initial time"
    );
    assert_eq!(
        channel_at_start(&result, "T_start"),
        0.0,
        "T_start settles to startTime + count*period = 0"
    );
}

/// MLS §8.6: "Before the start of the integration, it must be guaranteed
/// that for all variables `v`, `v = pre(v)`. If this is not the case for some
/// variables `vi`, `pre(vi) := vi` must be set and an event iteration at the
/// initial time must follow, so the model is re-evaluated, until this condition
/// is fulfilled."
///
/// This state starts at zero, but the initialization system settles it to five.
/// Both hosts must therefore expose five through `pre(x)` to the initial event;
/// reading zero would silently substitute the declared start for the settled
/// initialization result.
const SETTLED_STATE_PRE: &str = r#"
model SettledStatePreAtInitialEvent
  Real x(start = 0, fixed = false);
  discrete Real seenPre(start = -1, fixed = true);
initial equation
  x = 5;
equation
  der(x) = 0;
  when initial() then
    seenPre = pre(x);
  end when;
end SettledStatePreAtInitialEvent;
"#;

#[test]
fn both_hosts_seed_initial_event_pre_from_the_settled_state() {
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_on(
            SETTLED_STATE_PRE,
            "SettledStatePreAtInitialEvent",
            solver_mode,
        );
        assert_eq!(
            channel_at_start(&result, "x"),
            5.0,
            "the initialization system must settle x on {solver_mode:?}"
        );
        assert_eq!(
            channel_at_start(&result, "seenPre"),
            5.0,
            "the initial event must read pre(x) from the settled value on {solver_mode:?}"
        );
    }
}

/// MLS §8.6: a `parameter` declared `fixed = false` has no value of its own;
/// its `start` is the guess. A parameter binding that reads it is therefore not
/// finished by the parameter set, which only has the guess.
///
/// `Ipar` is solved by the initialization equation to `5`. `gain` reads it, so
/// `gain = 2*Ipar = 10` and `der(x) = 10`. Evaluating `gain` against
/// `Ipar.start = 1` instead produces the numerically plausible `2`.
const GUESS_READ: &str = r#"
model CalculatedParameterReadsSolvedParameter
  parameter Real target = 10.0;
  parameter Real Ipar(start = 1.0, fixed = false);
  parameter Real gain = 2*Ipar;
  parameter Real doubled = 2*gain;
  Real x(start = 0, fixed = true);
initial equation
  2*Ipar = target;
equation
  der(x) = gain + doubled;
end CalculatedParameterReadsSolvedParameter;
"#;

#[test]
fn a_parameter_binding_reading_a_fixed_false_parameter_uses_its_solved_value() {
    let result = simulate(GUESS_READ, "CalculatedParameterReadsSolvedParameter", 0.2);
    let x_index = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("x is recorded");
    let x_end = *result.data[x_index].last().expect("x has samples");
    // Solved: Ipar = 5, gain = 10, doubled = 20, der(x) = 30.
    // Against the guess: Ipar = 1, gain = 2, doubled = 4, der(x) = 6.
    assert!(
        (x_end - 30.0 * 0.2).abs() <= 1.0e-6,
        "der(x) = gain + doubled = 2*Ipar + 4*Ipar = 30 for the solved Ipar = 5; \
         x(0.2) = {x_end}, which is {} against the start guess",
        x_end / 0.2
    );
}

/// The same read through the `initial algorithm` calculated-parameter owner.
///
/// `gain` is a `fixed = false` parameter the section determines, so it becomes a
/// calculated parameter whose binding is the replayed `3*Ipar`. That binding
/// reads `Ipar`, which the initialization equation solves — so the replayed
/// value is *not* value-equivalent at parameter-set time, and the acceptance
/// contract holds only because the binding is re-applied after the solve.
const ALGORITHM_GUESS_READ: &str = r#"
model InitialAlgorithmParameterReadsSolvedParameter
  parameter Real target = 10.0;
  parameter Real Ipar(start = 1.0, fixed = false);
  parameter Real gain(fixed = false);
  Real x(start = 0, fixed = true);
initial algorithm
  gain := 3*Ipar;
initial equation
  2*Ipar = target;
equation
  der(x) = gain;
end InitialAlgorithmParameterReadsSolvedParameter;
"#;

#[test]
fn an_initial_algorithm_parameter_reading_a_solved_parameter_uses_its_solved_value() {
    let result = simulate(
        ALGORITHM_GUESS_READ,
        "InitialAlgorithmParameterReadsSolvedParameter",
        0.2,
    );
    let x_index = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("x is recorded");
    let x_end = *result.data[x_index].last().expect("x has samples");
    // Solved: Ipar = 5, gain = 15. Against the guess: Ipar = 1, gain = 3.
    assert!(
        (x_end - 15.0 * 0.2).abs() <= 1.0e-6,
        "der(x) = gain = 3*Ipar = 15 for the solved Ipar = 5; x(0.2) = {x_end}, \
         which is {} against the start guess",
        x_end / 0.2
    );
}

/// MLS §8.6 solves the initialization unknowns *and* the bindings that read
/// them as one system, not as a sweep between them.
///
/// `q` is the §8.6 unknown and `g = 2*q` is a calculated parameter that reads
/// it, so the residual `g + q = 30` couples them. Evaluating that residual
/// against the *stored* `g` — the number the parameter set derived from
/// `q.start` — and then refreshing `g` from the new `q` is a Gauss-Seidel sweep
/// with iteration map `q ← 30 - 2q`. Its loop gain is `-2`, so it diverges even
/// though the system is well posed: substituting the binding gives `3q = 30`,
/// `q = 10`, `g = 20`.
///
/// OMC 4.1.0 (`omc` from the pinned flake) simulates this model and reports
/// `der(x) = q = 10` at `t = 0`.
const DIVERGENT_DEPENDENT: &str = r#"
model DivergentDependent
  parameter Real q(start = 1.0, fixed = false);
  parameter Real g = 2*q;
  Real x(start = 0, fixed = true);
initial equation
  g + q = 30;
equation
  der(x) = q;
end DivergentDependent;
"#;

#[test]
fn a_calculated_parameter_binding_is_solved_with_the_unknown_it_reads() {
    let result = simulate(DIVERGENT_DEPENDENT, "DivergentDependent", 0.2);
    let x_index = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("x is recorded");
    let x_end = *result.data[x_index].last().expect("x has samples");
    // g + q = 30 with g = 2*q is 3q = 30, so q = 10 and der(x) = 10.
    assert!(
        (x_end - 10.0 * 0.2).abs() <= 1.0e-9,
        "der(x) = q = 10 solves `g + q = 30` with `g = 2*q`; x(0.2) = {x_end} is {} instead, \
         which is what a fixed-point sweep around the projection reaches when it converges \
         at all",
        x_end / 0.2
    );
}

/// The same coupling with a loop gain a sweep *could* survive.
///
/// `0.25*g + q = 30` with `g = 2*q` has gain `-0.5`, so the sweep converges —
/// but only to the update tolerance. Solving the substituted residual
/// `1.5q = 30` reaches `q = 20`, `g = 40`, `der(x) = 60` exactly, so this pins
/// that the gain-below-one case did not merely keep working but became exact.
///
/// OMC 4.1.0 reports `der(x) = 60` at `t = 0`.
const CONVERGENT_DEPENDENT: &str = r#"
model ConvergentDependent
  parameter Real q(start = 1.0, fixed = false);
  parameter Real g = 2*q;
  Real x(start = 0, fixed = true);
initial equation
  0.25*g + q = 30;
equation
  der(x) = q + g;
end ConvergentDependent;
"#;

#[test]
fn a_calculated_parameter_binding_with_a_convergent_loop_gain_is_solved_exactly() {
    let result = simulate(CONVERGENT_DEPENDENT, "ConvergentDependent", 0.2);
    let x_index = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("x is recorded");
    let x_end = *result.data[x_index].last().expect("x has samples");
    // 0.25*(2q) + q = 30 is 1.5q = 30, so q = 20, g = 40, der(x) = 60.
    assert!(
        (x_end - 60.0 * 0.2).abs() <= 1.0e-9,
        "der(x) = q + g = 60 solves `0.25*g + q = 30` with `g = 2*q`; x(0.2) = {x_end} is {}, \
         and a sweep that only reaches the update tolerance lands short of it",
        x_end / 0.2
    );
}

/// A `String` parameter whose binding reads a solved parameter is not an
/// initialization update row, and asking it to be one rejected a legal model.
///
/// MLS §8.6 adds no initialization equation for it, and a `String` has no
/// numeric Solve coordinate to update, so the parameter set's ordinary
/// evaluation of the binding is its whole determination. The ordering only ever
/// applied to it because `String(Ipar)` mentions a solved parameter.
///
/// OMC 4.1.0 compiles and simulates this model, reporting `der(x) = Ipar = 5`.
const STRING_DEPENDENT: &str = r#"
model DepString
  parameter Real target = 10.0;
  parameter Real Ipar(start=1.0, fixed = false);
  parameter String label = "Ipar=" + String(Ipar);
  Real x(start = 0, fixed = true);
equation
  der(x) = Ipar;
initial equation
  2*Ipar = target;
end DepString;
"#;

#[test]
fn a_string_parameter_binding_reading_a_solved_parameter_is_not_an_update_row() {
    let result = simulate(STRING_DEPENDENT, "DepString", 0.2);
    let x_index = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("x is recorded");
    let x_end = *result.data[x_index].last().expect("x has samples");
    assert!(
        (x_end - 5.0 * 0.2).abs() <= 1.0e-9,
        "der(x) = Ipar = 5 solves `2*Ipar = target`; x(0.2) = {x_end}"
    );
}

/// The real-source case: `Modelica.Blocks.Math.ContinuousMean`.
///
/// MSL 4.1.0 `Modelica/Blocks/Math.mo` declares, verbatim:
///
/// ```modelica
/// parameter Real t_0(fixed=false) "Start time";
/// parameter Real actualStartTime=max(t_0, startTime);
/// initial equation
///   t_0 = time;
/// ```
///
/// `t_0` is a parameter the initialization equation solves to the initial
/// instant, and a second parameter's binding reads it. Its declared `start` is
/// the default `0.0`, so a parameter set that finishes `actualStartTime` before
/// the solve pins the block's whole time origin to `0` no matter when the
/// simulation begins — the mean is then divided by the wrong elapsed time.
/// Simulating from `t_start = 3` separates the guess from the solved value.
const CONTINUOUS_MEAN: &str = r#"
model ContinuousMeanStartTime
  parameter Real startTime = -1e30;
  parameter Real t_eps = 1e-7;
  parameter Real t_0(fixed=false);
  parameter Real actualStartTime = max(t_0, startTime);
  Real mu(start = 0, fixed = true);
  Real u;
  Real y;
initial equation
  t_0 = time;
equation
  u = 2.0;
  der(mu) = if time >= actualStartTime then u else 0;
  y = noEvent(if time > actualStartTime + t_eps then mu/(time - actualStartTime) else u);
end ContinuousMeanStartTime;
"#;

#[test]
fn the_msl_continuous_mean_start_time_reads_its_solved_parameter() {
    let result = simulate_over(CONTINUOUS_MEAN, "ContinuousMeanStartTime", 3.0, 4.0);
    let y_index = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("y is recorded");
    let y_end = *result.data[y_index].last().expect("y has samples");
    // Solved: t_0 = actualStartTime = 3, mu = 2*(4-3) = 2, y = 2/1 = 2.
    // Against the `t_0` start guess 0: actualStartTime = 0, y = 2/4 = 0.5.
    assert!(
        (y_end - 2.0).abs() <= 1.0e-6,
        "the continuous mean of a constant 2 is 2; y(4) = {y_end}, which is the \
         guess-derived mu/(t - 0) if the parameter set finished actualStartTime early"
    );
}
