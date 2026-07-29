//! Regression tests for clocked sample flattening.
//!
//! MLS §16.5.1: `sample(u)` samples the value of `u` on an inferred clock. It is
//! not a structural Boolean event indicator and must not be folded to `true`.

use rumoca_core::{BuiltinFunction, Expression, OpBinary};
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
    let tree = &resolved.0;

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
