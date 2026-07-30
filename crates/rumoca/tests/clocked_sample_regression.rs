//! Regression tests for clocked sample flattening.
//!
//! MLS §16.5.1: `sample(u)` samples the value of `u` on an inferred clock. It is
//! not a structural Boolean event indicator and must not be folded to `true`.

use rumoca_core::{BuiltinFunction, Expression, ExpressionVisitor, OpBinary};
use rumoca_phase_flatten::flatten_ref;
use rumoca_phase_instantiate::instantiate_model;
use rumoca_phase_resolve::resolve;
use rumoca_phase_typecheck::typecheck_instanced;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae};

const SAMPLE_TIME_SOURCE: &str = r#"
model SampleTime
  connector ClockInput = input Clock;
  connector ClockOutput = output Clock;
  connector RealInput = input Real;
  connector RealOutput = output Real;

  block PeriodicClock
    parameter Real period = 0.1;
    ClockOutput y;
  equation
    y = Clock(period);
  end PeriodicClock;

  block AssignClock
    RealInput u;
    RealOutput y;
    ClockInput clock;
  equation
    when clock then
      y = u;
    end when;
  end AssignClock;

  block Ramp
    RealOutput y;
    Real simTime;
  equation
    simTime = sample(time);
    y = if simTime < 1.0 then simTime else 1.0;
  end Ramp;

  Ramp ramp;
  PeriodicClock periodicClock;
  AssignClock assignClock;
equation
  connect(periodicClock.y, assignClock.clock);
  connect(ramp.y, assignClock.u);
end SampleTime;
"#;

#[test]
fn real_sample_time_equation_stays_runtime_sample_after_flatten() {
    let source = r#"
package Types
  type Time = Real(unit = "s");
end Types;

model SampleTime
  connector ClockInput = input Clock;
  connector ClockOutput = output Clock;
  connector RealInput = input Real;
  connector RealOutput = output Real;

  block PeriodicClock
    parameter Real period = 0.1;
    ClockOutput y;
  equation
    y = Clock(period);
  end PeriodicClock;

  block AssignClock
    RealInput u;
    RealOutput y;
    ClockInput clock;
  equation
    when clock then
      y = u;
    end when;
  end AssignClock;

  partial block PartialClockedSO
    RealOutput y;
  end PartialClockedSO;

  block Ramp
    extends PartialClockedSO;
    Types.Time simTime;
  equation
    simTime = sample(time);
    y = if simTime < 1.0 then simTime else 1.0;
  end Ramp;

  Ramp ramp;
  PeriodicClock periodicClock;
  AssignClock assignClock;
  Real y;
equation
  connect(periodicClock.y, assignClock.clock);
  connect(ramp.y, assignClock.u);
  assignClock.y = y;
end SampleTime;
"#;

    let def = rumoca_phase_parse::parse_to_ast(source, "sample_time.mo").unwrap();
    let mut tree = rumoca_ir_ast::ClassTree::from_parsed(def);
    tree.source_map.add("sample_time.mo", source);
    let parsed = rumoca_ir_ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let model = "SampleTime";
    let tree = resolved.inner();

    let mut overlay = instantiate_model(tree, model).expect("instantiate should succeed");
    typecheck_instanced(tree, &mut overlay, model).expect("typecheck should succeed");
    let flat = flatten_ref(tree, &overlay, model).expect("flatten should succeed");

    let sample_rhs = flat
        .equations
        .iter()
        .find_map(sample_time_rhs)
        .expect("ramp.simTime equation should stay in the flat model");

    assert!(
        matches!(
            sample_rhs,
            Expression::BuiltinCall {
                function: BuiltinFunction::Sample,
                ..
            }
        ),
        "simTime must be assigned from runtime sample(time), got {sample_rhs:?}"
    );
}

#[test]
fn synchronous_identity_is_exact_from_resolve_through_checked_dae() {
    let source = r#"
model ExactClockIdentity
  type ClockAlias = Clock;
  ClockAlias base = Clock(0.1);
  Clock slow = subSample(base, 2);
  Clock fast = superSample(slow, 2);
  Clock shifted = shiftSample(fast, 1, 2);
  Clock back = backSample(shifted, 1, 2);
  Real x(start = 0);
  Real sampled;
  Real held;
equation
  when back then
    x = previous(x) + interval();
    sampled = sample(time);
  end when;
  held = hold(x);
end ExactClockIdentity;
"#;
    let (tree, flat) = flatten_source(source, "exact_clock_identity.mo", "ExactClockIdentity");
    let canonical_clock = tree
        .type_table
        .lookup("Clock")
        .expect("resolved tree owns the predefined Clock identity");
    assert_eq!(flat.predefined_types.clock, canonical_clock);

    for name in ["base", "slow", "fast", "shifted", "back"] {
        let key = rumoca_core::VarName::new(name);
        let variable = flat
            .variables
            .get(&key)
            .unwrap_or_else(|| panic!("missing exact Clock coordinate `{name}`"));
        assert_eq!(
            flat.effective_types[&variable.type_id].canonical_type(),
            canonical_clock,
            "`{name}` must retain the canonical predefined Clock identity"
        );
    }
    assert_ne!(
        flat.variables[&rumoca_core::VarName::new("base")].type_id,
        canonical_clock,
        "the regression must exercise nominal Clock alias canonicalization"
    );

    let expected_bindings = [
        ("base", BuiltinFunction::Clock),
        ("slow", BuiltinFunction::SubSample),
        ("fast", BuiltinFunction::SuperSample),
        ("shifted", BuiltinFunction::ShiftSample),
        ("back", BuiltinFunction::BackSample),
    ];
    for (name, expected) in expected_bindings {
        let binding = flat.variables[&rumoca_core::VarName::new(name)]
            .binding
            .as_ref()
            .unwrap_or_else(|| panic!("`{name}` has no typed binding"));
        assert!(
            matches!(
                binding,
                Expression::BuiltinCall { function, .. } if *function == expected
            ),
            "`{name}` must use exact typed builtin identity, got {binding:?}"
        );
    }

    let mut synchronous = SynchronousBuiltinCollector::default();
    for equation in &flat.equations {
        synchronous.visit_expression(&equation.residual);
    }
    for chain in &flat.when_chains {
        collect_when_chain_builtins(&mut synchronous, chain);
    }
    for expected in [
        BuiltinFunction::Hold,
        BuiltinFunction::Previous,
        BuiltinFunction::Interval,
        BuiltinFunction::Sample,
    ] {
        assert!(
            synchronous.functions.contains(&expected),
            "missing exact typed `{}` call",
            expected.name()
        );
    }

    let dae = rumoca_phase_dae::to_dae(&flat, tree.source_map.clone())
        .expect("exact clock identities must construct a checked DAE");
    dae.inspect(|view| {
        assert_eq!(view.previous_value_count(), 1);
        let previous = view
            .previous(view.previous_id(0).expect("one previous identity"))
            .expect("previous identity resolves");
        assert_eq!(view.source_text(previous.provenance()), Some("previous(x)"));
    });
}

#[test]
fn shadowed_synchronous_spelling_remains_a_user_function() {
    let source = r#"
model ShadowedSubSample
  function subSample
    input Real u;
    input Integer factor;
    output Real y;
  algorithm
    y := u + factor;
  end subSample;
  Real x = subSample(1.0, 2);
end ShadowedSubSample;
"#;
    let (tree, flat) = flatten_source(source, "shadowed_sub_sample.mo", "ShadowedSubSample");
    let binding = flat.variables[&rumoca_core::VarName::new("x")]
        .binding
        .as_ref()
        .expect("x retains its user-function binding");
    assert!(
        matches!(binding, Expression::FunctionCall { name, .. } if name.as_str().ends_with("subSample")),
        "shadowing user declaration must not become a synchronous builtin: {binding:?}"
    );
    rumoca_phase_dae::to_dae(&flat, tree.source_map.clone())
        .expect("shadowed user function must remain an ordinary checked function call");
}

#[test]
fn no_clock_is_typed_only_from_its_predefined_identity() {
    let source = r#"
model ExactNoClock
  Clock sourceClock = Clock(0.1);
  Clock detached = noClock(sourceClock);
end ExactNoClock;
"#;
    let (_, flat) = flatten_source(source, "exact_no_clock.mo", "ExactNoClock");
    let binding = flat.variables[&rumoca_core::VarName::new("detached")]
        .binding
        .as_ref()
        .expect("detached clock retains its conversion binding");
    assert!(
        matches!(
            binding,
            Expression::BuiltinCall {
                function: BuiltinFunction::NoClock,
                ..
            }
        ),
        "predefined noClock must lower to its typed identity: {binding:?}"
    );
}

#[test]
fn native_simulation_updates_condition_memory_after_clocked_sample_time() {
    let compiled = rumoca::Compiler::new()
        .model("SampleTime")
        .compile_str(SAMPLE_TIME_SOURCE, "sample_time.mo")
        .expect("clocked sample(time) model should compile");
    assert_canonical_clock_ownership(&compiled.dae);
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..SimOptions::default()
        },
    )
    .expect("clocked sample(time) model should simulate");

    let y = trace_values(&sim, "ramp.y");
    assert!(
        (y[0] - 0.0).abs() <= 1.0e-12,
        "MLS Appendix B B.1d condition memory should select the true branch at initialization; got {}",
        y[0]
    );
    assert!(
        (y[1] - 0.1).abs() <= 1.0e-12,
        "MLS §16.5.1 sample(time) should refresh before dependent if-expression projection at the first clock tick; got {}",
        y[1]
    );
    assert_first_clock_assignment_reads_same_tick_input(&sim, "native");
}

#[test]
fn rk_like_simulation_updates_condition_memory_after_clocked_sample_time() {
    let compiled = rumoca::Compiler::new()
        .model("SampleTime")
        .compile_str(SAMPLE_TIME_SOURCE, "sample_time.mo")
        .expect("clocked sample(time) model should compile");
    assert_canonical_clock_ownership(&compiled.dae);
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.2,
            dt: Some(0.1),
            ..SimOptions::default()
        },
    )
    .expect("clocked sample(time) model should simulate with RK-like solver");

    let y = trace_values(&sim, "ramp.y");
    assert!(
        (y[0] - 0.0).abs() <= 1.0e-12,
        "RK-like condition memory should select the true branch at initialization; got {}",
        y[0]
    );
    assert!(
        (y[1] - 0.1).abs() <= 1.0e-12,
        "RK-like sample(time) should refresh before dependent projection at the first clock tick; got {}",
        y[1]
    );
    assert_first_clock_assignment_reads_same_tick_input(&sim, "RK-like");
}

#[test]
fn equal_period_independent_clocks_do_not_supply_an_ambiguous_sample_owner() {
    let source = r#"
model AmbiguousClockOwner
  Clock a;
  Clock b;
  discrete Real sampledTime;
equation
  a = Clock(0.1);
  b = Clock(0.1);
  sampledTime = sample(time);
end AmbiguousClockOwner;
"#;
    let error = rumoca::Compiler::new()
        .model("AmbiguousClockOwner")
        .compile_str(source, "ambiguous_clock_owner.mo")
        .expect_err("two independent clock constructors must not be conflated by period");

    assert!(
        error
            .to_string()
            .contains("more than one possible inferred clock"),
        "ambiguous sample ownership should fail at DAE construction: {error}"
    );
}

#[test]
fn previous_is_clock_owned_provenance_bearing_and_simulatable() {
    let source = r#"
model PreviousCounter
  Clock c = Clock(0.1);
  Real x(start = 0);
equation
  when c then
    x = previous(x) + 1;
  end when;
end PreviousCounter;
"#;
    let compiled = rumoca::Compiler::new()
        .model("PreviousCounter")
        .compile_str(source, "previous_counter.mo")
        .expect("typed previous history should compile through Solve IR");
    compiled.dae.inspect(|view| {
        assert_eq!(view.previous_value_count(), 1);
        let previous = view
            .previous(view.previous_id(0).expect("one branded previous identity"))
            .expect("previous identity resolves");
        let clock = view
            .clock(previous.clock())
            .expect("previous retains its owning clock");
        assert!(matches!(
            clock.operation(),
            rumoca_ir_dae::ClockOperation::Periodic(_)
        ));
        assert!(
            view.source_text(previous.provenance())
                .is_some_and(|text| text.starts_with("previous(")),
            "previous provenance must resolve to its source call occurrence"
        );
    });

    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..SimOptions::default()
        },
    )
    .expect("clock-owned previous history should be runtime computable");
    assert_eq!(trace_values(&result, "x"), &[1.0, 2.0, 3.0]);
}

#[test]
fn sampled_algorithm_target_is_owned_before_its_guarded_value_is_constructed() {
    let source = r#"
model SampledAlgorithmCounter
  discrete Real x(start = 0);
algorithm
  when sample(0.0, 0.1) then
    x := pre(x) + 1;
  end when;
end SampledAlgorithmCounter;
"#;
    let compiled = rumoca::Compiler::new()
        .model("SampledAlgorithmCounter")
        .compile_str(source, "sampled_algorithm_counter.mo")
        .expect("sampled algorithm history should be valid by construction");
    compiled.dae.inspect(|view| {
        assert_eq!(view.clock_count(), 1);
        assert_eq!(view.clock_ownership_count(), 1);
        let ownership = view
            .clock_ownership(
                view.clock_ownership_id(0)
                    .expect("one branded clock-ownership identity"),
            )
            .expect("clock-ownership identity resolves");
        assert_eq!(
            view.source_text(ownership.provenance()),
            Some("x"),
            "ownership provenance must resolve to the algorithm target occurrence"
        );
    });

    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..SimOptions::default()
        },
    )
    .expect("scheduled algorithm history should be runtime computable");
    assert_eq!(trace_values(&result, "x"), &[1.0, 2.0, 3.0]);
}

/// MLS §16.3 `Clock(intervalCounter, resolution)`: the rational constructor is
/// the only form that keeps a sub-millisecond period free of binary rounding,
/// and `Modelica.Clocked.ClockSignals.Clocks.PeriodicExactClock` uses it.
#[test]
fn rational_clock_constructor_keeps_its_exact_interval_counter_period() {
    let source = r#"
model ExactRationalClock
  Clock c = Clock(20, 1000);
  Real x(start = 0);
equation
  when c then
    x = previous(x) + 1;
  end when;
end ExactRationalClock;
"#;
    let compiled = rumoca::Compiler::new()
        .model("ExactRationalClock")
        .compile_str(source, "exact_rational_clock.mo")
        .expect("Clock(intervalCounter, resolution) must resolve to a static schedule");
    assert_periodic_clock_period(&compiled.dae, 1, 50);
}

/// MLS §16.7: clock partitioning is static, so a `Clock` coordinate defined by
/// an `if`-equation over parameter values resolves to the selected branch —
/// exactly the shape `PeriodicExactClock` uses to switch between `subSample`
/// and the rational constructor.
#[test]
fn parameter_if_equation_selects_the_clock_branch_it_defines() {
    let source = r#"
model ParameterSelectedClock
  parameter Integer factor = 20;
  parameter Integer resolutionFactor = 1000;
  parameter Boolean subSampled = true;
  Clock c;
  Real x(start = 0);
equation
  if subSampled then
    c = subSample(Clock(factor), resolutionFactor);
  else
    c = Clock(factor, resolutionFactor);
  end if;
  when c then
    x = previous(x) + 1;
  end when;
end ParameterSelectedClock;
"#;
    let compiled = rumoca::Compiler::new()
        .model("ParameterSelectedClock")
        .compile_str(source, "parameter_selected_clock.mo")
        .expect("a parameter `if` equation must resolve its clock coordinate");
    assert_periodic_clock_period(&compiled.dae, 20000, 1);
}

/// MLS §16.3 `sample(u, c)`: the named clock proves the partition owner instead
/// of leaving it to §16.5.1 inference, which is what makes a model with several
/// independent clock constructors (a `SampleClocked` block) resolvable.
#[test]
fn named_sample_clock_owns_its_partition_without_inference() {
    let source = r#"
model NamedSampleOwner
  Clock fast = Clock(0.05);
  Clock slow = Clock(0.2);
  Real u;
  Real sampled;
  Real held;
equation
  u = time;
  sampled = sample(u, slow);
  held = hold(sampled);
  when fast then
    // keeps the second constructor reachable so inference stays ambiguous
  end when;
end NamedSampleOwner;
"#;
    let compiled = rumoca::Compiler::new()
        .model("NamedSampleOwner")
        .compile_str(source, "named_sample_owner.mo")
        .expect("sample(u, c) must own its partition through the named clock");
    compiled.dae.inspect(|view| {
        let ownership = view
            .clock_ownership(
                view.clock_ownership_id(0)
                    .expect("the sampled coordinate is clock owned"),
            )
            .expect("clock-ownership identity resolves");
        let clock = view
            .clock(ownership.clock())
            .expect("the ownership names a constructed clock");
        let rumoca_ir_dae::ClockOperation::Periodic(lattice) = clock.operation() else {
            panic!("a named sample clock must be periodic");
        };
        assert_eq!(
            lattice.period().numerator(),
            1,
            "sample(u, slow) must adopt the named 0.2 s clock, not the 0.05 s one"
        );
        assert_eq!(lattice.period().denominator(), 5);
    });
}

/// MLS §16.5.1 `when Clock() then`: the inferred-clock form takes its owner
/// from the clocked partition it is connected to. This is the shape
/// `Modelica.Clocked.RealSignals.NonPeriodic.PI` uses.
#[test]
fn inferred_clock_when_branch_adopts_its_connected_partition_owner() {
    let source = r#"
model InferredWhenClock
  Clock c = Clock(0.1);
  Real u;
  Real sampled;
  Real x(start = 0);
  Real held;
equation
  u = time;
  sampled = sample(u, c);
  when Clock() then
    x = previous(x) + sampled;
  end when;
  held = hold(x);
end InferredWhenClock;
"#;
    let compiled = rumoca::Compiler::new()
        .model("InferredWhenClock")
        .compile_str(source, "inferred_when_clock.mo")
        .expect("when Clock() must infer its owner from the connected partition");
    assert_periodic_clock_period(&compiled.dae, 1, 10);
    compiled.dae.inspect(|view| {
        assert_eq!(
            view.clock_count(),
            1,
            "the inferred branch must reuse the partition's single clock identity"
        );
    });
}

/// MLS §16.5 Operator 16.5: `previous(u)` names a clocked discrete-time
/// variable, so a clocked partition written without a `when` clause — the shape
/// `Modelica.Clocked.BooleanSignals.TimeBasedSources.Pulse` uses — classifies
/// its undeclared state as discrete rather than continuous.
#[test]
fn previous_operand_in_a_bare_clocked_partition_is_a_discrete_coordinate() {
    let source = r#"
model BareClockedPartition
  Clock c = Clock(0.1);
  Real simTime;
  Real next(start = 0.1, fixed = true);
  Real held;
equation
  simTime = sample(time);
  next = if simTime >= previous(next) then previous(next) + 0.4 else previous(next);
  held = hold(next);
end BareClockedPartition;
"#;
    let compiled = rumoca::Compiler::new()
        .model("BareClockedPartition")
        .compile_str(source, "bare_clocked_partition.mo")
        .expect("previous(...) outside a when clause must classify its operand as clocked");
    compiled.dae.inspect(|view| {
        assert_eq!(
            view.previous_value_count(),
            1,
            "the three `previous(next)` occurrences share one history identity"
        );
        let previous = view
            .previous(view.previous_id(0).expect("one previous identity"))
            .expect("previous identity resolves");
        assert!(
            matches!(
                view.clock(previous.clock())
                    .expect("previous retains its owning clock")
                    .operation(),
                rumoca_ir_dae::ClockOperation::Periodic(_)
            ),
            "an inferred clocked partition must own its history through a periodic clock"
        );
    });
}

/// MLS §16.5.2: the clock-conversion operators also have a *value* form that
/// moves the value to a derived clock. The canonical DAE owns one clock per
/// discrete coordinate, so that form is rejected at its own occurrence with the
/// section it comes from rather than as an opaque lowering failure.
#[test]
fn value_clock_conversion_is_rejected_at_its_own_occurrence() {
    let source = r#"
model ValueShiftSample
  Clock c = Clock(0.1);
  Real u;
  Real y;
  Real held;
equation
  u = sample(time, c);
  y = shiftSample(u, 1, 2);
  held = hold(y);
end ValueShiftSample;
"#;
    let error = rumoca::Compiler::new()
        .model("ValueShiftSample")
        .compile_str(source, "value_shift_sample.mo")
        .expect_err("a value-level shiftSample has no checked canonical owner");
    let message = error.to_string();
    assert!(
        message.contains("16.5.2") && message.contains("shiftSample"),
        "the rejection must name the operator and its MLS section: {message}"
    );
}

/// A connection row between two clocked coordinates defines *both* of them.
///
/// The solved form names only the side it assigns, so counting the row without
/// counting the coordinate on its other side leaves the model one equation
/// heavy — the shape `Modelica.Clocked.RealSignals.NonPeriodic.UnitDelay`
/// produces when its input is fed by a clocked block.
#[test]
fn clocked_connection_row_counts_the_coordinate_it_defines() {
    let source = r#"
model ClockedRelayBalance
  connector ClockInput = input Clock;
  connector ClockOutput = output Clock;
  connector RealInput = input Real;
  connector RealOutput = output Real;

  block PeriodicClock
    ClockOutput y;
  equation
    y = Clock(0.1);
  end PeriodicClock;

  block Source
    RealOutput y;
  equation
    y = sample(time);
  end Source;

  block AssignClock
    RealInput u;
    RealOutput y;
    ClockInput clock;
  equation
    when clock then
      y = u;
    end when;
  end AssignClock;

  block UnitDelay
    RealInput u;
    RealOutput y;
  equation
    y = previous(u);
  end UnitDelay;

  PeriodicClock periodicClock;
  Source src;
  AssignClock assignClock;
  UnitDelay unitDelay;
  Real held;
equation
  connect(periodicClock.y, assignClock.clock);
  connect(src.y, assignClock.u);
  connect(assignClock.y, unitDelay.u);
  held = hold(unitDelay.y);
end ClockedRelayBalance;
"#;
    rumoca::Compiler::new()
        .model("ClockedRelayBalance")
        .compile_str(source, "clocked_relay_balance.mo")
        .expect("a clocked connection row must count the coordinate it defines");
}

fn assert_periodic_clock_period(model: &rumoca_ir_dae::Dae, numerator: i128, denominator: i128) {
    model.inspect(|view| {
        let clock = view
            .clock(view.clock_id(0).expect("one branded clock identity"))
            .expect("clock identity resolves");
        let rumoca_ir_dae::ClockOperation::Periodic(lattice) = clock.operation() else {
            panic!("the model's clock must be periodic");
        };
        assert_eq!(lattice.period().numerator(), numerator);
        assert_eq!(lattice.period().denominator(), denominator);
    });
}

fn assert_canonical_clock_ownership(model: &rumoca_ir_dae::Dae) {
    model.inspect(|view| {
        assert_eq!(
            view.clock_count(),
            1,
            "clock aliases must share one canonical semantic owner"
        );
        assert_eq!(
            view.clock_ownership_count(),
            2,
            "sampled time and the clocked assignment each require explicit ownership"
        );
        let first = view
            .clock_ownership(view.clock_ownership_id(0).unwrap())
            .unwrap();
        let second = view
            .clock_ownership(view.clock_ownership_id(1).unwrap())
            .unwrap();
        assert_eq!(
            first.clock(),
            second.clock(),
            "both clocked coordinates must reference the same branded clock identity"
        );
    });
}

fn flatten_source(
    source: &str,
    file_name: &str,
    model_name: &str,
) -> (rumoca_ir_ast::ClassTree, rumoca_ir_flat::Model) {
    let def = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = rumoca_ir_ast::ClassTree::from_parsed(def);
    tree.source_map.add(file_name, source);
    let parsed = rumoca_ir_ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("source resolves");
    let mut overlay = instantiate_model(resolved.inner(), model_name).expect("source instantiates");
    typecheck_instanced(resolved.inner(), &mut overlay, model_name).expect("source typechecks");
    let flat = flatten_ref(resolved.inner(), &overlay, model_name).expect("source flattens");
    (resolved.into_inner(), flat)
}

#[derive(Default)]
struct SynchronousBuiltinCollector {
    functions: Vec<BuiltinFunction>,
}

impl ExpressionVisitor for SynchronousBuiltinCollector {
    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        self.functions.push(*function);
        self.walk_builtin_call(function, args);
    }
}

fn collect_when_chain_builtins(
    synchronous: &mut SynchronousBuiltinCollector,
    chain: &rumoca_ir_flat::WhenChain,
) {
    for branch in chain.branches() {
        synchronous.visit_expression(&branch.condition);
        for equation in &branch.equations {
            if let rumoca_ir_flat::WhenEquation::Assign { value, .. } = equation {
                synchronous.visit_expression(value);
            }
        }
    }
}

fn assert_first_clock_assignment_reads_same_tick_input(sim: &rumoca_sim::SimResult, backend: &str) {
    let y = trace_values(sim, "assignClock.y");
    assert!(
        (y[1] - 0.1).abs() <= 1.0e-12,
        "{backend} clocked assignments must read upstream values propagated during the same tick; got {}",
        y[1]
    );
}

fn sample_time_rhs(equation: &rumoca_ir_flat::Equation) -> Option<&Expression> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return None;
    };
    let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return None;
    };
    if name.as_str() == "ramp.simTime" && subscripts.is_empty() {
        Some(rhs.as_ref())
    } else {
        None
    }
}

fn trace_values<'a>(sim: &'a rumoca_sim::SimResult, name: &str) -> &'a [f64] {
    let idx = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace should contain `{name}`; names={:?}", sim.names));
    &sim.data[idx]
}
