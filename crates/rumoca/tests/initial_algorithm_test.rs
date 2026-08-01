//! MLS §8.6 `initial algorithm` acceptance contract and its bounded rejections.
//!
//! Accepted: sequential scalar assignments to a `parameter` declared
//! `fixed = false`, `if`/`elseif`/`else` conditionals over them, and `assert`
//! statements, which keep every enclosing branch condition as a guard. Such a
//! section lowers to the calculated-parameter and assertion owners that already
//! exist, so the replayed value reaches the trajectory as an exact number.
//!
//! Also accepted: a zero-output call statement whose callee is proven to have
//! no effect other than raising assertions — the MSL case is the checking call
//! `Modelica.Blocks.Sources.BooleanTable.isValidTable`. It is replaced by
//! exactly the assertions its body raises, one per unrolled loop iteration,
//! under the same guard, so it reaches the assertion owner above.
//!
//! Also accepted: a scalar discrete-time target whose replayed value reads only
//! `time`, parameters, and constants — the
//! `Modelica.Blocks.Sources.Pulse`/`SawTooth`/`Trapezoid` period counter. It
//! becomes an initialization-partition definition of the value the coordinate
//! holds when initialization finishes, and of its `pre` value at that instant.
//! The equation section keeps its own owner for every later instant.
//!
//! Rejected, each naming the owner that is absent rather than a consequence of
//! it: a call statement that binds an output or whose body has any other
//! effect, a state/algebraic/output/input target the initialization system
//! solves from residual rows instead, a loop or `when` carrying implicit
//! memory, and a value that reads a coordinate with no proven value where it is
//! evaluated.

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

/// The `Modelica.Blocks.Sources.BooleanTable.isValidTable` shape: a protected
/// function declared inside the model that owns the section, called with no
/// outputs purely to check a parameter array.
///
/// The declaration path is what the Flat function table is keyed by, while the
/// call spells one segment, so this also proves the callee reaches the DAE at
/// all. Every iteration of the body's loop must reach the trajectory as its own
/// assertion owner: the table below is monotonic, so the model runs.
const CHECKING_CALL: &str = r#"
model InitialAlgorithmCheckingCall
  parameter Real t[:] = {1.0, 2.0, 3.0};
protected
  function isValidTable
    input Real table[:];
  protected
    Integer n = size(table, 1);
  algorithm
    if n > 0 then
      for i in 2:n loop
        assert(table[i] > table[i - 1], "table not strict monotonically increasing");
      end for;
    end if;
  end isValidTable;
public
  Real x(start = 0, fixed = true);
initial algorithm
  isValidTable(t);
equation
  der(x) = 1.0;
end InitialAlgorithmCheckingCall;
"#;

#[test]
fn a_checking_call_replays_as_the_assertions_its_body_raises() {
    let compiled = Compiler::new()
        .model("InitialAlgorithmCheckingCall")
        .compile_str(CHECKING_CALL, "initial_algorithm_checking_call.mo")
        .expect("a zero-output checking call has a checked initialization owner");
    simulate_dae(&compiled.dae, &SimOptions::default())
        .expect("a monotonic table satisfies every assertion the call raises");
}

/// The same call over a table that breaks monotonicity between its second and
/// third entries. Only the unrolled iteration for `i = 3` can catch that, so a
/// replay that dropped an iteration — or that folded the loop into one check —
/// would let this model run.
#[test]
fn every_unrolled_iteration_of_a_checking_call_owns_its_own_assertion() {
    let source = CHECKING_CALL.replace("{1.0, 2.0, 3.0}", "{1.0, 2.0, 1.5}");
    let compiled = Compiler::new()
        .model("InitialAlgorithmCheckingCall")
        .compile_str(&source, "initial_algorithm_checking_call.mo")
        .expect("a zero-output checking call has a checked initialization owner");
    let error = simulate_dae(&compiled.dae, &SimOptions::default())
        .err()
        .expect("a table that is not monotonic fails the assertion the call raises");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("table not strict monotonically increasing"),
        "the replayed assertion must carry its source message, got: {rendered}"
    );
}

/// A checking call is admitted by proving its body has no effect other than
/// raising assertions. A callee that writes an output has one, so it keeps a
/// typed rejection naming the call statement rather than being replayed.
#[test]
fn a_call_statement_that_binds_an_output_names_the_missing_owner() {
    const SOURCE: &str = r#"
model InitialAlgorithmOutputCall
  parameter Real t[2] = {1.0, 2.0};
  parameter Real lo(fixed=false);
  parameter Real hi(fixed=false);
protected
  function bounds
    input Real table[:];
    output Real low;
    output Real high;
  algorithm
    low := table[1];
    high := table[size(table, 1)];
  end bounds;
public
  Real x(start = 0, fixed = true);
initial algorithm
  (lo, hi) := bounds(t);
equation
  der(x) = lo + hi;
end InitialAlgorithmOutputCall;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmOutputCall");
    assert!(
        rendered.contains("ED013") && rendered.contains("bounds"),
        "the rejection must name the unowned call statement, got: {rendered}"
    );
    assert!(
        !rendered.contains("unresolved Flat reference"),
        "a call statement inside the rejected section must not be reported as an \
         unresolved callee, got: {rendered}"
    );
}

/// A checking call replays only the statement forms whose assertions it can
/// state exactly. A `while` carries implicit memory and has no bound the
/// replay can unroll, so the call keeps a typed rejection naming it rather than
/// being replayed as some finite number of checks.
#[test]
fn a_checking_call_with_an_unbounded_loop_names_the_missing_owner() {
    const SOURCE: &str = r#"
model InitialAlgorithmUnboundedCall
  parameter Real t[2] = {1.0, 2.0};
protected
  function scan
    input Real table[:];
  protected
    Integer i = 1;
  algorithm
    while i < size(table, 1) loop
      assert(table[i] < table[i + 1], "table not increasing");
      i := i + 1;
    end while;
  end scan;
public
  Real x(start = 0, fixed = true);
initial algorithm
  scan(t);
equation
  der(x) = 1.0;
end InitialAlgorithmUnboundedCall;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmUnboundedCall");
    assert!(
        rendered.contains("ED013") && rendered.contains("scan"),
        "the rejection must name the call whose body has no replay owner, got: {rendered}"
    );
    assert!(
        rendered.contains("while"),
        "the rejection must name the statement form that is missing an owner, got: {rendered}"
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

/// The `Modelica.Blocks.Sources.Pulse`/`SawTooth`/`Trapezoid` period counter:
/// an initial algorithm determines a discrete Real and a discrete Integer from
/// `time` and parameters, and the equation section keeps its own `when` owner
/// for every later instant.
///
/// The replay is sequential, so `T_start` reads the `count` the first statement
/// assigned rather than that coordinate's declared start. Both the coordinate
/// and its `pre` value must carry the determined number at `t = 0`: MLS §8.6
/// holds `pre(v) = v` at the initialization instant, and here the `when`
/// trigger reads `pre(count)`, so a `pre` left at the declared start would
/// schedule the first period boundary at the wrong time.
const DISCRETE_INITIAL_VALUES: &str = r#"
model InitialAlgorithmDiscreteTarget
  parameter Real period = 0.1;
  parameter Real startTime = -0.35;
  Real y;
protected
  discrete Real T_start;
  discrete Integer count;
initial algorithm
  count := integer((time - startTime)/period);
  T_start := startTime + count*period;
equation
  when time >= (pre(count) + 1)*period + startTime then
    count = pre(count) + 1;
    T_start = time;
  end when;
  y = time - T_start;
end InitialAlgorithmDiscreteTarget;
"#;

fn discrete_trace<'a>(result: &'a rumoca_sim::SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("missing `{name}` in {:?}", result.names));
    &result.data[index]
}

fn discrete_initial_value_trace(t_end: f64) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model("InitialAlgorithmDiscreteTarget")
        .compile_str(DISCRETE_INITIAL_VALUES, "initial_algorithm.mo")
        .expect("an initial algorithm over discrete coordinates has a checked owner");
    let options = SimOptions {
        t_end,
        ..SimOptions::default()
    };
    simulate_dae(&compiled.dae, &options).expect("the determined discrete values simulate")
}

/// The DAE wire is how the MSL simulation worker receives a compiled model, so
/// the discrete initial-value definitions must survive replay through the same
/// checked owner rather than being dropped or defaulted on decode.
#[test]
fn discrete_initial_values_survive_the_dae_wire() {
    let compiled = Compiler::new()
        .model("InitialAlgorithmDiscreteTarget")
        .compile_str(DISCRETE_INITIAL_VALUES, "initial_algorithm.mo")
        .expect("an initial algorithm over discrete coordinates has a checked owner");
    let encoded = serde_json::to_value(&compiled.dae).expect("compiled DAE serializes");
    let decoded: rumoca_ir_dae::Dae =
        serde_json::from_value(encoded.clone()).expect("compiled DAE reconstructs");
    assert_eq!(
        serde_json::to_value(&decoded).expect("round-tripped DAE serializes"),
        encoded,
        "the discrete initial-value definitions must replay unchanged"
    );
    decoded.inspect(|view| {
        assert_eq!(
            view.initial_discrete_value_count(),
            2,
            "T_start and count each keep their initialization-instant definition"
        );
    });
}

#[test]
fn a_discrete_target_is_determined_by_its_initial_algorithm() {
    let result = discrete_initial_value_trace(0.01);
    // integer((0 - (-0.35))/0.1) = 3, so T_start = -0.35 + 3*0.1 and y = -T_start.
    let expected_t_start = -0.35 + 3.0 * 0.1;
    assert_eq!(discrete_trace(&result, "count").first().copied(), Some(3.0));
    let t_start = discrete_trace(&result, "T_start")[0];
    assert!(
        (t_start - expected_t_start).abs() <= 1.0e-12,
        "T_start = {t_start}, expected the replayed value {expected_t_start}"
    );
    let y = discrete_trace(&result, "y")[0];
    assert!(
        (y + expected_t_start).abs() <= 1.0e-12,
        "y = {y}, expected {}",
        -expected_t_start
    );
}

/// The first period boundary is `(pre(count) + 1)*period + startTime`. With the
/// determined `count = 3` that instant is `0.05`; with a `pre` left at the
/// declared start it would be `-0.25`, already true at `t = 0`, and the `when`
/// would never see a rising edge again.
#[test]
fn a_determined_discrete_value_is_the_pre_value_the_first_event_is_scheduled_from() {
    let result = discrete_initial_value_trace(0.16);
    let counts = discrete_trace(&result, "count");
    assert_eq!(
        counts.first().copied(),
        Some(3.0),
        "the initial algorithm determines the first period count"
    );
    assert_eq!(
        counts.last().copied(),
        Some(5.0),
        "the boundaries at 0.05 and 0.15 advance the counter twice by t = 0.16: {counts:?}"
    );
}

/// The runtime applies an algorithm-determined discrete value until it stops
/// changing, so a value that answers differently each time it runs has no fixed
/// point. MLS §12.3 permits the impure call in an initial section; what is
/// missing is an owner for a discrete initial value built from one.
#[test]
fn an_impure_call_in_a_discrete_initial_value_is_rejected() {
    const SOURCE: &str = r#"
model InitialAlgorithmImpureDiscreteValue
  discrete Real T_start;
  Real x(start = 0, fixed = true);
initial algorithm
  T_start := ticks();
equation
  when time > 0.5 then
    T_start = time;
  end when;
  der(x) = T_start;
protected
  impure function ticks
    output Real y;
    external "C" y = rumoca_test_ticks() annotation(Library="rumoca_test");
  end ticks;
end InitialAlgorithmImpureDiscreteValue;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmImpureDiscreteValue");
    assert!(
        rendered.contains("ED013") && rendered.contains("impure function"),
        "an impure determining call must name the missing owner, got: {rendered}"
    );
}

/// A state, algebraic, or output coordinate is solved from residual rows, so an
/// algorithm assignment to one still names the owner that is absent.
#[test]
fn a_continuous_target_names_the_missing_initialization_owner() {
    const SOURCE: &str = r#"
model InitialAlgorithmContinuousTarget
  parameter Real period = 1.0;
  Real w;
  Real x(start = 0, fixed = true);
initial algorithm
  w := period;
equation
  w = 2*x + 1;
  der(x) = w;
end InitialAlgorithmContinuousTarget;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmContinuousTarget");
    assert!(
        rendered.contains("ED013") && rendered.contains("solved from residual rows"),
        "a continuous target must name its missing initialization owner, got: {rendered}"
    );
}

/// The initialization instant settles `time`, parameters, and constants and
/// nothing else, so a discrete initial value that reads a state is rejected
/// rather than evaluated against that state's seed.
#[test]
fn an_unsettled_read_in_a_discrete_initial_value_is_rejected() {
    const SOURCE: &str = r#"
model InitialAlgorithmDiscreteRuntimeRead
  discrete Real T_start;
  Real x(start = 1.0, fixed = true);
initial algorithm
  T_start := x + 1.0;
equation
  when time > 0.5 then
    T_start = time;
  end when;
  der(x) = T_start;
end InitialAlgorithmDiscreteRuntimeRead;
"#;
    let rendered = rejection(SOURCE, "InitialAlgorithmDiscreteRuntimeRead");
    assert!(
        rendered.contains("ED013") && rendered.contains("no proven value"),
        "an unsettled read must name the coordinate that has no value, got: {rendered}"
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
