//! MLS §8.6 `initial algorithm` acceptance contract and its bounded rejections.
//!
//! Accepted: sequential scalar assignments to a `parameter` declared
//! `fixed = false`, `if`/`elseif`/`else` conditionals over them, and `assert`
//! statements, which keep every enclosing branch condition as a guard. Such a
//! section lowers to the calculated-parameter and assertion owners that already
//! exist, so the replayed value reaches the trajectory as an exact number.
//!
//! Rejected, each naming the owner that is absent rather than a consequence of
//! it: a call statement other than `assert` (the MSL case is the checking call
//! `Modelica.Blocks.Sources.BooleanTable.isValidTable`), a target the
//! initialization system cannot determine, a loop or `when` carrying implicit
//! memory, and a value that reads a runtime coordinate.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at, simulate_dae};

fn rejection(source: &str, model: &str) -> String {
    let error = Compiler::new()
        .model(model)
        .compile_str(source, "initial_algorithm.mo")
        .err()
        .unwrap_or_else(|| panic!("{model} has no checked initial-algorithm owner"));
    format!("{error:?}")
}

const CALCULATED_PARAMETERS: &str = r#"
model InitialAlgorithmCalculatedParameters
  parameter Real V0 = 60.0;
  parameter Real I0 = 50.0;
  parameter Real P0 = 100.0;
  parameter Real Rcv = 0.1;
  parameter Real Gcc = 0.01;
  parameter Real vLim(fixed=false);
  parameter Real iLim(fixed=false);
  parameter Boolean cp(fixed=false);
  Real x(start = 0, fixed = true);
initial algorithm
  assert(Rcv < (V0/I0), "Rcv too high!");
  assert(Gcc < (I0/V0), "Gcc too high!");
  vLim := (V0 - Rcv*I0)/(1 - Rcv*Gcc);
  iLim := (I0 - Gcc*V0)/(1 - Gcc*Rcv);
  cp := false;
  if vLim*iLim > P0 then
    cp := true;
    vLim := (I0 - sqrt(I0^2 - 4*Gcc*P0))/(2*Gcc);
    iLim := (V0 - sqrt(V0^2 - 4*Rcv*P0))/(2*Rcv);
  end if;
equation
  der(x) = vLim + iLim + (if cp then 1.0 else 0.0);
end InitialAlgorithmCalculatedParameters;
"#;

/// The `Modelica.Electrical.Analog.Sources.DCPowerSupply` shape: an `assert`
/// pair, three deferred parameters, and a conditional that rewrites two of them
/// from the values the earlier statements assigned.
///
/// The expected derivative is the sequential result: the conditional reads the
/// first pair of assignments, takes its branch, and the second pair replaces
/// them. A replay that dropped the sequential read, or that merged the branch
/// against the wrong entry value, produces a different finite number here.
#[test]
fn deferred_parameters_are_determined_by_their_initial_algorithm() {
    let compiled = Compiler::new()
        .model("InitialAlgorithmCalculatedParameters")
        .compile_str(CALCULATED_PARAMETERS, "initial_algorithm.mo")
        .expect("an initial algorithm over deferred parameters has a checked owner");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the calculated parameters evaluate with the parameter set");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    let derivative = probe
        .report
        .derivatives
        .iter()
        .find(|slot| slot.name == "der(x)")
        .unwrap_or_else(|| {
            panic!(
                "missing der(x); have {:?}",
                probe
                    .report
                    .derivatives
                    .iter()
                    .map(|slot| slot.name.clone())
                    .collect::<Vec<_>>()
            )
        })
        .value;
    let v_lim = (50.0 - (50.0_f64.powi(2) - 4.0 * 0.01 * 100.0).sqrt()) / (2.0 * 0.01);
    let i_lim = (60.0 - (60.0_f64.powi(2) - 4.0 * 0.1 * 100.0).sqrt()) / (2.0 * 0.1);
    let expected = v_lim + i_lim + 1.0;
    assert!(
        (derivative - expected).abs() <= 1.0e-9,
        "der(x) = {derivative}, expected the replayed branch value {expected}"
    );
}

/// A guarded `assert` owns `guard implies condition`, not the bare condition:
/// with the guard false the section is silent, and the `else` branch value is
/// the one the trajectory reads.
#[test]
fn a_guarded_assertion_stays_guarded() {
    const SOURCE: &str = r#"
model GuardedInitialAssertion
  parameter Real threshold = 1000.0;
  parameter Real a = 2.0;
  parameter Real k(fixed=false);
  Real x(start = 0, fixed = true);
initial algorithm
  k := a*3;
  if k > threshold then
    assert(false, "guard must not fire");
    k := -1;
  else
    k := k + 1;
  end if;
equation
  der(x) = k;
end GuardedInitialAssertion;
"#;
    let compiled = Compiler::new()
        .model("GuardedInitialAssertion")
        .compile_str(SOURCE, "guarded_initial_assertion.mo")
        .expect("a guarded assertion has a checked owner");
    let simulation = simulate_dae(&compiled.dae, &SimOptions::default())
        .expect("an unreached guard leaves the assertion silent");
    let x = simulation
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state x is visible");
    let last = *simulation.data[x]
        .last()
        .expect("the trajectory has samples");
    let stop = *simulation
        .times
        .last()
        .expect("the trajectory has a final time");
    assert!(
        (last - 7.0 * stop).abs() <= 1.0e-6,
        "der(x) must be the else-branch value 7, got x({stop}) = {last}"
    );
}

/// The same section with a reachable guard fails at the assertion it owns,
/// which is what proves the guard was folded rather than dropped.
#[test]
fn a_reached_guard_fires_its_assertion() {
    const SOURCE: &str = r#"
model ReachedInitialAssertion
  parameter Real threshold = 1.0;
  parameter Real a = 2.0;
  parameter Real k(fixed=false);
  Real x(start = 0, fixed = true);
initial algorithm
  k := a*3;
  if k > threshold then
    assert(false, "guard must fire");
    k := -1;
  else
    k := k + 1;
  end if;
equation
  der(x) = k;
end ReachedInitialAssertion;
"#;
    let compiled = Compiler::new()
        .model("ReachedInitialAssertion")
        .compile_str(SOURCE, "reached_initial_assertion.mo")
        .expect("a guarded assertion has a checked owner");
    let error = simulate_dae(&compiled.dae, &SimOptions::default())
        .err()
        .expect("a reached guard fails its assertion");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("guard must fire"),
        "the assertion must carry its source message, got: {rendered}"
    );
}

/// `Modelica.Blocks.Sources.BooleanTable.isValidTable` is a checking call with
/// no outputs, so the Flat function table never registers it. The rejection
/// names the missing call owner instead of reporting the callee as an
/// unresolved reference.
#[test]
fn a_checking_call_statement_names_the_missing_call_owner() {
    const SOURCE: &str = r#"
within;
function isValidTable
  input Real t[:];
algorithm
  assert(size(t, 1) > 0, "empty table");
end isValidTable;

model InitialAlgorithmCheckingCall
  parameter Real t[2] = {1.0, 2.0};
  Real x(start = 0, fixed = true);
initial algorithm
  isValidTable(t);
equation
  der(x) = 1.0;
end InitialAlgorithmCheckingCall;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmCheckingCall");
    assert!(
        rendered.contains("ED013") && rendered.contains("isValidTable"),
        "the rejection must name the unowned call statement, got: {rendered}"
    );
    assert!(
        !rendered.contains("unresolved Flat reference"),
        "a call statement inside the rejected section must not be reported as an \
         unresolved callee, got: {rendered}"
    );
}

/// A statement-form `assert` reaches Flat as a call to the predefined operator.
/// Neither the accepted nor the rejected path may read it as a user callee.
#[test]
fn a_statement_assert_is_never_read_as_an_unresolved_callee() {
    const SOURCE: &str = r#"
model InitialAlgorithmAssertThenLoop
  parameter Real v0 = 60.0;
  parameter Real y[2](each fixed=false);
  Real x(start = 0, fixed = true);
initial algorithm
  assert(v0 > 0, "v0 must be positive");
  for i in 1:2 loop
    y[i] := v0;
  end for;
equation
  der(x) = y[1] + y[2];
end InitialAlgorithmAssertThenLoop;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmAssertThenLoop");
    assert!(
        rendered.contains("ED013") && rendered.contains("implicit memory"),
        "a loop in an initial algorithm must name its missing owner, got: {rendered}"
    );
    assert!(
        !rendered.contains("unresolved Flat reference"),
        "the statement-form `assert` must not be reported as an unresolved callee, \
         got: {rendered}"
    );
}

/// A discrete coordinate has no checked initialization owner, so the rejection
/// names the role instead of lowering a residual the runtime can only check.
#[test]
fn a_discrete_target_names_the_missing_initialization_owner() {
    const SOURCE: &str = r#"
model InitialAlgorithmDiscreteTarget
  parameter Real period = 1.0;
  discrete Real T_start;
  Real x(start = 0, fixed = true);
initial algorithm
  T_start := period;
equation
  when time > 0.5 then
    T_start = time;
  end when;
  der(x) = T_start;
end InitialAlgorithmDiscreteTarget;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmDiscreteTarget");
    assert!(
        rendered.contains("ED013") && rendered.contains("DiscreteReal"),
        "a discrete target must name its missing initialization owner, got: {rendered}"
    );
}

/// A deferred parameter is computed with the parameter set, before the
/// trajectory exists, so reading a runtime coordinate is rejected rather than
/// silently evaluated against a seed.
#[test]
fn a_runtime_read_in_a_deferred_parameter_is_rejected() {
    const SOURCE: &str = r#"
model InitialAlgorithmRuntimeRead
  parameter Real k(fixed=false);
  Real x(start = 1.0, fixed = true);
initial algorithm
  k := x + 1.0;
equation
  der(x) = k;
end InitialAlgorithmRuntimeRead;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmRuntimeRead");
    assert!(
        rendered.contains("ED013") && rendered.contains("not settled"),
        "a runtime read must name the coordinate that is not settled, got: {rendered}"
    );
}

/// A parameter whose declaration already fixes its value keeps its single
/// determining owner.
#[test]
fn a_fixed_parameter_target_is_rejected() {
    const SOURCE: &str = r#"
model InitialAlgorithmFixedTarget
  parameter Real k = 2.0;
  Real x(start = 0, fixed = true);
initial algorithm
  k := 3.0;
equation
  der(x) = k;
end InitialAlgorithmFixedTarget;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmFixedTarget");
    assert!(
        rendered.contains("ED013") && rendered.contains("fixed = false"),
        "a parameter that is not deferred must name that requirement, got: {rendered}"
    );
}

/// An initial equation over parameters is itself a determining owner for the
/// deferred parameters it reads, so it may not compete with the algorithm.
#[test]
fn a_competing_initial_equation_is_rejected() {
    const SOURCE: &str = r#"
model InitialAlgorithmCompetingOwner
  parameter Real a = 2.0;
  parameter Real k(fixed=false);
  Real x(start = 0, fixed = true);
initial algorithm
  k := a*3;
initial equation
  k = a*4;
equation
  der(x) = k;
end InitialAlgorithmCompetingOwner;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmCompetingOwner");
    assert!(
        rendered.contains("ED013") && rendered.contains("exactly one determining owner"),
        "two determining owners must be named, got: {rendered}"
    );
}
