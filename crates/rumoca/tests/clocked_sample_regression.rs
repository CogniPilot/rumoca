//! Regression tests for clocked sample flattening.
//!
//! MLS §16.5.1: `sample(u)` samples the value of `u` on an inferred clock. It is
//! not a structural Boolean event indicator and must not be folded to `true`.

use rumoca_core::{BuiltinFunction, Expression, OpBinary};
use rumoca_phase_flatten::flatten_ref;
use rumoca_phase_instantiate::instantiate_model;
use rumoca_phase_resolve::resolve;
use rumoca_phase_typecheck::typecheck_instanced;
use rumoca_sim::{SimOptions, simulate_dae};

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
    let tree = rumoca_ir_ast::ClassTree::from_parsed(def);
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
