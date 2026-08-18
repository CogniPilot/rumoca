//! MLS 3.6 §8.6 initial equations declared on an aliased coordinate.
//!
//! §8.6: "For every Real variable vc with fixed = true, the equation
//! vc = startExpression is added to the initialization equations." A model
//! states that equation wherever the quantity is most natural to talk about —
//! the relative position of a spring, say — and the equations then carry it to
//! whatever the simulator integrates. Every value asserted here was produced by
//! OpenModelica 4.1.0 on the same source, so a regression shows up as a
//! disagreement with a reference implementation, not merely with this test.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};

/// `a = b` with the initial condition stated on the non-state member.
const ALIAS_PIN: &str = r#"
model AliasPin
  Real a;
  Real b(start = 1, fixed = true);
  Real v;
equation
  a = b;
  der(a) = v;
  v = -a;
end AliasPin;
"#;

/// `a = -b`: the transfer has to carry the sign the equality states.
const ALIAS_PIN_OPPOSITE: &str = r#"
model AliasPinOpposite
  Real a;
  Real b(start = 1, fixed = true);
  Real v;
equation
  a + b = 0;
  der(a) = v;
  v = -a;
end AliasPinOpposite;
"#;

/// `a = b + L`: the transfer has to carry the displacement too.
const ALIAS_PIN_DISPLACED: &str = r#"
model AliasPinDisplaced
  parameter Real L = 2;
  Real a;
  Real b(start = 1, fixed = true);
  Real v;
equation
  a = b + L;
  der(a) = v;
  v = -a;
end AliasPinDisplaced;
"#;

/// Two members of one alias class stating different initial values.
const ALIAS_BOTH_PINNED: &str = r#"
model AliasBothPinned
  Real a(start = 2, fixed = true);
  Real b(start = 1, fixed = true);
  Real v;
equation
  a = b;
  der(a) = v;
  v = -a;
end AliasBothPinned;
"#;

/// A support, a spring and a mass, connected exactly as the translational
/// library connects them: the pinned relative position reaches the integrated
/// position through a connector chain, a support held at a parameter, and the
/// body's own half-length displacement.
const SPRING_MASS: &str = r#"
connector Flange
  Real s;
  flow Real f;
end Flange;

model Fixed
  parameter Real s0 = 0;
  Flange flange;
equation
  flange.s = s0;
end Fixed;

model Spring
  parameter Real c = 100;
  Real s_rel(start = 1, fixed = true);
  Real f;
  Flange flange_a;
  Flange flange_b;
equation
  s_rel = flange_b.s - flange_a.s;
  f = c*s_rel;
  flange_b.f = f;
  flange_a.f = -f;
end Spring;

model Mass
  parameter Real m = 1;
  parameter Real L = 0.5;
  Real s(start = 1.5);
  Real v(start = 0, fixed = true);
  Real a;
  Flange flange_a;
  Flange flange_b;
equation
  flange_a.s = s - L/2;
  flange_b.s = s + L/2;
  v = der(s);
  a = der(v);
  m*a = flange_a.f + flange_b.f;
end Mass;

model SpringMass
  Fixed fixed1;
  Spring spring1;
  Mass m1;
equation
  connect(fixed1.flange, spring1.flange_a);
  connect(spring1.flange_b, m1.flange_a);
end SpringMass;

model SpringMassBothPinned
  Fixed fixed1;
  Spring spring1;
  Mass m1(s(start = 1.25, fixed = true));
equation
  connect(fixed1.flange, spring1.flange_a);
  connect(spring1.flange_b, m1.flange_a);
end SpringMassBothPinned;
"#;

/// The same chain with the pin on a coordinate the model asks to *integrate*,
/// which index reduction then has to demote.
///
/// This is the `Translational.Components.SpringDamper` shape: MSL declares
/// `s_rel(start = 0, stateSelect = stateSelect)` with `stateSelect` defaulting
/// to `StateSelect.prefer` (`PartialCompliantWithRelativeStates`), so `s_rel` is
/// a state, and `s_rel = flange_b.s - flange_a.s` makes it one state too many
/// once the body it holds is one as well. `StateSelect.always` on the body
/// position settles which of the two the reduction may take (MLS 3.6 §4.8.9),
/// leaving the demotion that drops the stated `s_rel(0)` as the only one on
/// offer. MLS 3.6 §8.6 states that equation about the *quantity* `s_rel` names,
/// so the reduction is free to take it as long as the value lands on the
/// coordinate that survives.
const SPRING_DAMPER_PREFER: &str = r#"
connector Flange
  Real s;
  flow Real f;
end Flange;

model Fixed
  parameter Real s0 = 0;
  Flange flange;
equation
  flange.s = s0;
end Fixed;

model SpringDamper
  parameter Real c = 100;
  parameter Real d = 5;
  Real s_rel(start = 1, fixed = true, stateSelect = StateSelect.prefer);
  Real v_rel(start = 0, stateSelect = StateSelect.prefer);
  Real f;
  Flange flange_a;
  Flange flange_b;
equation
  s_rel = flange_b.s - flange_a.s;
  v_rel = der(s_rel);
  f = c*s_rel + d*v_rel;
  flange_b.f = f;
  flange_a.f = -f;
end SpringDamper;

model Mass
  parameter Real m = 1;
  parameter Real L = 0.5;
  Real s(start = 1.5, stateSelect = StateSelect.always);
  Real v(start = 0, stateSelect = StateSelect.always);
  Real a;
  Flange flange_a;
  Flange flange_b;
equation
  flange_a.s = s - L/2;
  flange_b.s = s + L/2;
  v = der(s);
  a = der(v);
  m*a = flange_a.f + flange_b.f;
end Mass;

model SpringDamperPrefer
  Fixed fixed1;
  SpringDamper spring1;
  Mass m1;
equation
  connect(fixed1.flange, spring1.flange_a);
  connect(spring1.flange_b, m1.flange_a);
end SpringDamperPrefer;
"#;

/// One four-terminal flow node, written the three ways a modeller writes it.
///
/// `s_rel = x - f4` carries the pin on `s_rel` to `x` only if the node is read,
/// and the node is the same equation in all three spellings. `f4 = -6`, so
/// `s_rel(0) = 7` puts `x(0) = 1` in every one of them.
const FOUR_TERMINAL_NODE: &str = r#"
model NodeSumEqualsZero
  parameter Real k1 = 1;
  parameter Real k2 = 2;
  parameter Real k3 = 3;
  Real f1; Real f2; Real f3; Real f4;
  Real s_rel(start = 7, fixed = true);
  Real x; Real v;
equation
  f1 + f2 + f3 + f4 = 0;
  f1 = k1;
  f2 = k2;
  f3 = k3;
  s_rel = x - f4;
  der(x) = v;
  v = -x;
end NodeSumEqualsZero;

model NodeZeroEqualsSum
  parameter Real k1 = 1;
  parameter Real k2 = 2;
  parameter Real k3 = 3;
  Real f1; Real f2; Real f3; Real f4;
  Real s_rel(start = 7, fixed = true);
  Real x; Real v;
equation
  0 = f1 + f2 + f3 + f4;
  f1 = k1;
  f2 = k2;
  f3 = k3;
  s_rel = x - f4;
  der(x) = v;
  v = -x;
end NodeZeroEqualsSum;

model NodeReassociated
  parameter Real k1 = 1;
  parameter Real k2 = 2;
  parameter Real k3 = 3;
  Real f1; Real f2; Real f3; Real f4;
  Real s_rel(start = 7, fixed = true);
  Real x; Real v;
equation
  f4 = -(f1 + f2 + f3);
  f1 = k1;
  f2 = k2;
  f3 = k3;
  s_rel = x - f4;
  der(x) = v;
  v = -x;
end NodeReassociated;
"#;

/// Two stated initial values whose agreement turns on a parameter.
const PARAMETER_AGREEMENT: &str = r#"
model ParamDisplacedAgree
  parameter Real L = 2;
  Real a(start = 3, fixed = true);
  Real b(start = 1, fixed = true);
  Real v;
equation
  a = b + L;
  der(a) = v;
  v = -a;
end ParamDisplacedAgree;

model ParamStartsAgree
  parameter Real L = 2;
  Real a(start = L + 1, fixed = true);
  Real b(start = 1, fixed = true);
  Real v;
equation
  a = b + L;
  der(a) = v;
  v = -a;
end ParamStartsAgree;

model ParamDisplacedDisagree
  parameter Real L = 2;
  Real a(start = 3, fixed = true);
  Real b(start = 1, fixed = true);
  Real v;
equation
  a = b + L + 1;
  der(a) = v;
  v = -a;
end ParamDisplacedDisagree;
"#;

/// Two contradicting stated values in a class the runtime seeds no state for:
/// `b` is driven by `time`, so neither member is a state.
const NO_STATE_CONFLICT: &str = r#"
model NoStateConflict
  Real a(start = 2, fixed = true);
  Real b(start = 1, fixed = true);
  Real x;
equation
  a = b;
  b = sin(time);
  der(x) = -x;
end NoStateConflict;
"#;

/// A carried §8.6 equation that reads a parameter the initialization system is
/// still solving, in the two shapes the seam allows.
///
/// `PinReadsSolvedParameter` restates the class value through `L`, a *bound*
/// parameter whose binding reads the `fixed = false` unknown `q`: the row has to
/// recompute that binding, because the number the parameter set stored for `L`
/// is `2 * q.start` — a guess, not a value. `PinDeterminesParameter` goes the
/// other way: the restated value is the only equation that reads `q`, so the row
/// has to be visible to the projection planner or `q` can never be determined at
/// all.
const PIN_AND_PARAMETER: &str = r#"
model PinReadsSolvedParameter
  parameter Real q(start = 5.0, fixed = false);
  parameter Real L = 2*q;
  Real a(start = 3, fixed = true);
  Real b(start = 1, fixed = true);
  Real v;
initial equation
  L + q = 3;
equation
  a = b + L;
  der(a) = v;
  v = -a;
end PinReadsSolvedParameter;

model PinReadsSolvedParameterFromGoodSeed
  parameter Real q(start = 1.0, fixed = false);
  parameter Real L = 2*q;
  Real a(start = 3, fixed = true);
  Real b(start = 1, fixed = true);
  Real v;
initial equation
  L + q = 3;
equation
  a = b + L;
  der(a) = v;
  v = -a;
end PinReadsSolvedParameterFromGoodSeed;

model PinDeterminesParameter
  parameter Real q(start = 5.0, fixed = false);
  Real a(start = 3, fixed = true);
  Real b(start = 1, fixed = true);
  Real v;
equation
  a = b + q;
  der(a) = v;
  v = -a;
end PinDeterminesParameter;
"#;

fn simulate(source: &str, model: &str) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} should compile: {error}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.05),
            solver_mode: SimSolverMode::Bdf,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} should simulate: {error}"))
}

fn initial_value(result: &SimResult, name: &str) -> f64 {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("missing `{name}` in {:?}", result.names));
    *result.data[index]
        .first()
        .unwrap_or_else(|| panic!("`{name}` has no samples"))
}

fn assert_initial(result: &SimResult, name: &str, expected: f64) {
    let value = initial_value(result, name);
    assert!(
        (value - expected).abs() <= 1.0e-9,
        "{name} started at {value}, expected {expected}"
    );
}

/// OMC 4.1.0: `a(0) = 1`, `b(0) = 1`.
#[test]
fn a_pin_on_an_alias_member_initializes_the_state_it_names() {
    let result = simulate(ALIAS_PIN, "AliasPin");
    assert_initial(&result, "a", 1.0);
    assert_initial(&result, "b", 1.0);
}

/// OMC 4.1.0: `a(0) = -1`, `b(0) = 1`.
#[test]
fn an_opposite_signed_alias_transfers_the_negated_initial_value() {
    let result = simulate(ALIAS_PIN_OPPOSITE, "AliasPinOpposite");
    assert_initial(&result, "a", -1.0);
    assert_initial(&result, "b", 1.0);
}

/// OMC 4.1.0: `a(0) = 3`, `b(0) = 1`.
#[test]
fn a_displaced_alias_transfers_the_initial_value_through_its_offset() {
    let result = simulate(ALIAS_PIN_DISPLACED, "AliasPinDisplaced");
    assert_initial(&result, "a", 3.0);
    assert_initial(&result, "b", 1.0);
}

/// OMC 4.1.0 refuses this model: "The model contains alias variables with
/// conflicting fixed start values." MLS 3.6 §8.6 adds both equations, and the
/// initialization system they describe has no solution.
#[test]
fn two_members_of_one_class_may_not_pin_different_initial_values() {
    let compiled = Compiler::new()
        .model("AliasBothPinned")
        .compile_str(ALIAS_BOTH_PINNED, "AliasBothPinned.mo")
        .expect("the DAE itself is well formed; the contradiction is structural");
    let error = simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default())
        .expect_err("conflicting stated initial values are not a choice to make silently");
    let report = error.to_string();
    assert!(
        report.contains("conflicting stated initial values"),
        "unexpected diagnostic: {report}"
    );
    assert!(
        report.contains("`a`") && report.contains("`b`"),
        "the diagnostic must name both declarations: {report}"
    );
}

/// OMC 4.1.0: `spring1.s_rel(0) = 1` (as stated) and `m1.s(0) = 1.25`, which is
/// `s_rel + L/2 + fixed1.s0` — the state the connector chain carries the pin to,
/// not the `start = 1.5` guess it was declared with.
#[test]
fn a_connector_chain_carries_a_pinned_relative_position_to_the_body_state() {
    let result = simulate(SPRING_MASS, "SpringMass");
    assert_initial(&result, "spring1.s_rel", 1.0);
    assert_initial(&result, "m1.s", 1.25);
    assert_initial(&result, "m1.flange_a.s", 1.0);
}

/// The same chain with the body position *also* pinned, at the very value the
/// chain implies. The two agree only because `m1.L` and `fixed1.s0` hold the
/// values they do, which is a question about numbers, so nothing here may
/// reject the model: OMC 4.1.0 simulates it with `s_rel(0) = 1`, `m1.s(0) = 1.25`.
#[test]
fn a_second_stated_value_that_agrees_through_parameters_is_not_a_conflict() {
    let result = simulate(SPRING_MASS, "SpringMassBothPinned");
    assert_initial(&result, "spring1.s_rel", 1.0);
    assert_initial(&result, "m1.s", 1.25);
}

/// OMC 4.1.0 on `SpringDamperPrefer`: `spring1.s_rel(0) = 1`, `m1.s(0) = 1.25`,
/// `m1.v(0) = 0`, `spring1.v_rel(0) = 0` — the same answer as the chain above,
/// which is the point: whether the pinned coordinate is integrated or reduced
/// away is a decision about *storage*, and MLS 3.6 §8.6 states the initial
/// equation about the quantity either way.
#[test]
fn a_pinned_state_index_reduction_demotes_still_states_its_initial_value() {
    let result = simulate(SPRING_DAMPER_PREFER, "SpringDamperPrefer");
    assert_initial(&result, "spring1.s_rel", 1.0);
    assert_initial(&result, "m1.s", 1.25);
    assert_initial(&result, "m1.v", 0.0);
    assert_initial(&result, "spring1.v_rel", 0.0);
}

/// One four-terminal flow node, three spellings, one answer.
///
/// OMC 4.1.0 gives `s_rel(0) = 7` and `x(0) = 1` for all three. Which of them a
/// compiler can read must be a property of the equation, not of where the
/// parser put the parentheses (SPEC_0036).
#[test]
fn a_four_terminal_node_reads_the_same_however_it_is_written() {
    for model in ["NodeSumEqualsZero", "NodeZeroEqualsSum", "NodeReassociated"] {
        let result = simulate(FOUR_TERMINAL_NODE, model);
        assert_initial(&result, "s_rel", 7.0);
        assert_initial(&result, "x", 1.0);
        assert_initial(&result, "f4", -6.0);
    }
}

/// `a(start = 3)` and `b(start = 1)` under `a = b + L` state one value exactly
/// when `L = 2`. OMC 4.1.0 simulates both spellings with `a(0) = 3`, `b(0) = 1`;
/// a compiler that cannot evaluate `L` may not call either one a conflict.
#[test]
fn stated_values_that_agree_through_a_parameter_are_not_a_conflict() {
    for model in ["ParamDisplacedAgree", "ParamStartsAgree"] {
        let result = simulate(PARAMETER_AGREEMENT, model);
        assert_initial(&result, "a", 3.0);
        assert_initial(&result, "b", 1.0);
    }
}

/// The same shape with `a = b + L + 1`, where the two starts disagree for every
/// value of `L`. Nothing structural proves that here — the difference still
/// reads `L` — so the restated equation is left for the initialization instant,
/// which fails loudly on the numbers rather than accepting a wrong start.
#[test]
fn stated_values_that_disagree_for_every_parameter_value_fail_loudly() {
    let compiled = Compiler::new()
        .model("ParamDisplacedDisagree")
        .compile_str(PARAMETER_AGREEMENT, "ParamDisplacedDisagree.mo")
        .expect("the contradiction is numeric, not a construction failure");
    let error = simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default())
        .expect_err("an initial value no solution satisfies must not pass silently");
    let report = error.to_string();
    assert!(
        report.contains("initial variable projection did not satisfy"),
        "unexpected diagnostic: {report}"
    );
}

/// OMC 4.1.0 refuses this model too ("alias variables with conflicting fixed
/// start values"), and it holds no state at all: the contradiction is a defect
/// of the two declarations, not of whatever coordinate a runtime happens to
/// seed, so the class shape must not decide whether it is reported.
#[test]
fn contradicting_pins_are_rejected_in_a_class_that_holds_no_state() {
    let compiled = Compiler::new()
        .model("NoStateConflict")
        .compile_str(NO_STATE_CONFLICT, "NoStateConflict.mo")
        .expect("the DAE itself is well formed; the contradiction is structural");
    let error = simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default())
        .expect_err("two stated values that differ by 1 have no common solution");
    let report = error.to_string();
    assert!(
        report.contains("conflicting stated initial values"),
        "unexpected diagnostic: {report}"
    );
    assert!(
        report.contains("`a`") && report.contains("`b`"),
        "the diagnostic must name both declarations: {report}"
    );
}

/// OMC 4.1.0 on `PinReadsSolvedParameter`: `q = 1`, `L = 2`, so `a(0) = 3` and
/// `b(0) = 1`.
///
/// The restated value reads `L`, whose stored number is `2 * q.start`. Reading
/// that seed instead of recomputing the binding makes the row demand
/// `3 - 1 - 10 = 0`, which is why the seed must not be able to decide the
/// outcome: the same model with a different `start` is asserted to give the same
/// answer.
#[test]
fn a_carried_value_reading_a_solved_parameter_does_not_depend_on_its_seed() {
    let far = simulate(PIN_AND_PARAMETER, "PinReadsSolvedParameter");
    let near = simulate(PIN_AND_PARAMETER, "PinReadsSolvedParameterFromGoodSeed");
    assert_initial(&far, "a", 3.0);
    assert_initial(&far, "b", 1.0);
    for name in ["a", "b", "v"] {
        let far_value = initial_value(&far, name);
        let near_value = initial_value(&near, name);
        assert!(
            (far_value - near_value).abs() <= 1.0e-9,
            "`{name}` started at {far_value} from one guess and {near_value} from another; \
             a guess is not an answer"
        );
    }
}

/// OMC 4.1.0 on `PinDeterminesParameter`: `q = 2`, `a(0) = 3`, `b(0) = 1`.
///
/// Nothing else in the model reads `q`, so the carried §8.6 equation is the only
/// row that can determine it. `b(0) = 1` is what proves it was determined: with
/// `q` left at its `start = 5` guess the same row would put `b` at `-2`, and
/// with the row unplanned it could only fail.
#[test]
fn a_carried_value_can_determine_the_parameter_its_displacement_reads() {
    let result = simulate(PIN_AND_PARAMETER, "PinDeterminesParameter");
    assert_initial(&result, "a", 3.0);
    assert_initial(&result, "b", 1.0);
}
