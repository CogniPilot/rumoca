//! An equation-section `pre()` of a variable written by a sampled algorithm
//! (MLS 3.7.5): the GALEC projection collects its previous value like any
//! other `pre()` read. Event iteration settles the equation outside the
//! `when`, where `pre(x)` equals the value the tick wrote, while a sampled
//! action keeps the value it read on entry.

use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};

/// The reduced controls from the RDD2 flight-computer report, and the reduced
/// public-output coordinate from the same controller: a 100 Hz state read
/// through an equation-section pre() by a 1600 Hz sampled motor.
const PRE_CONTROLS: &str = "\
package PreControls
  model PreCoordinate
    discrete output Real state(start=0, fixed=true);
    output Real feedback;
    discrete output Real motor(start=0, fixed=true);
  equation
    feedback = pre(state);
  algorithm
    when sample(0, 0.01) then
      state := pre(state) + 1.0;
    end when;
    when sample(0, 0.000625) then
      motor := feedback;
    end when;
  end PreCoordinate;
  model Chain
    discrete output Real state(start=0, fixed=true);
    output Real doubled;
    output Real step;
    discrete output Real latched(start=0, fixed=true);
  protected
    Real settled;
  equation
    settled = pre(state);
    doubled = 2 * settled + 1;
    step = state - pre(state);
  algorithm
    when sample(0, 0.01) then
      state := pre(state) + 1.0;
      latched := doubled;
    end when;
  end Chain;
  model IntegerOwner
    discrete output Real state(start=0, fixed=true);
    output Integer count;
    output Real lagged;
  equation
    count = integer(state) + 1;
    lagged = pre(count);
  algorithm
    when sample(0, 0.01) then
      state := pre(state) + 1.0;
    end when;
  end IntegerOwner;
  block Vector
    input Real u[3];
    output Real previous[3];
    discrete Real held[3](each start=0, each fixed=true);
  equation
    previous = pre(held);
  algorithm
    when sample(0,0.01) then held := u; end when;
  end Vector;
  block Scalar
    input Real u;
    output Real previous;
    discrete Real held(start=0, fixed=true);
  equation
    previous = pre(held);
  algorithm
    when sample(0,0.01) then held := u; end when;
  end Scalar;
end PreControls;
";

fn evaluator_for(model: &str, run: impl FnOnce(&mut Evaluator<'_>)) {
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(PRE_CONTROLS, "PreControls.mo")
        .expect("pre controls compile");
    let package = lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, model),
        &GalecOptions::default(),
    )
    .unwrap_or_else(|errors| panic!("{model} must project to GALEC: {errors:?}"));
    let mut evaluator = Evaluator::new(package.checked_block(), IntegerDomain::signed_32())
        .expect("checked GALEC evaluator");
    run(&mut evaluator);
}

fn real(value: &Value) -> f64 {
    match value {
        Value::Real(value) => *value,
        other => panic!("expected a Real, got {other:?}"),
    }
}

#[test]
fn an_equation_pre_of_a_sampled_scalar_publishes_the_settled_tick() {
    evaluator_for("PreControls.Scalar", |evaluator| {
        evaluator.startup().expect("Startup");
        for (tick, u) in [3.0, -1.5, 7.25, 0.5].into_iter().enumerate() {
            evaluator.set_state("u", Value::Real(u)).expect("set u");
            evaluator.do_step().expect("DoStep");
            assert_eq!(
                real(evaluator.state("previous").unwrap()),
                u,
                "tick {tick}: the settled pre(held) is the value this tick held"
            );
            assert_eq!(real(evaluator.state("held").unwrap()), u, "tick {tick}");
        }
    });
}

#[test]
fn an_equation_pre_of_a_sampled_vector_publishes_the_settled_tick() {
    evaluator_for("PreControls.Vector", |evaluator| {
        evaluator.startup().expect("Startup");
        for (tick, base) in [1.0, -2.0, 4.5].into_iter().enumerate() {
            let u = [base, base + 0.25, base - 0.5];
            evaluator
                .set_state(
                    "u",
                    Value::Array(u.iter().copied().map(Value::Real).collect()),
                )
                .expect("set u");
            evaluator.do_step().expect("DoStep");
            let Value::Array(previous) = evaluator.state("previous").unwrap() else {
                panic!("previous is an array");
            };
            assert_eq!(
                previous.iter().map(real).collect::<Vec<_>>(),
                u,
                "tick {tick}: the settled pre(held) is the value this tick held"
            );
        }
    });
}

/// At a 100 Hz tick the state advances and the public feedback publishes it,
/// while the 1600 Hz motor keeps the feedback it sampled on entry: after tick
/// 0 state=1, feedback=1, motor=0; after tick 16 state=2, feedback=2, motor=1.
#[test]
fn a_slow_state_read_through_pre_publishes_the_settled_value_and_the_fast_sample_its_entry() {
    evaluator_for("PreControls.PreCoordinate", |evaluator| {
        evaluator.startup().expect("Startup");
        for tick in 0..49_u32 {
            evaluator.do_step().expect("DoStep");
            let state = f64::from(tick / 16 + 1);
            let motor = if tick == 0 {
                0.0
            } else {
                f64::from((tick - 1) / 16 + 1)
            };
            assert_eq!(
                [
                    real(evaluator.state("state").unwrap()),
                    real(evaluator.state("feedback").unwrap()),
                    real(evaluator.state("motor").unwrap()),
                ],
                [state, state, motor],
                "tick {tick}: state, feedback, motor"
            );
        }
    });
}

/// A protected local read through pre() feeds an output, another output takes
/// the difference with pre(), and a sampled action latches the output. The
/// outputs settle on the tick's state; the latch keeps its entry value.
#[test]
fn a_local_definition_chain_through_pre_settles_and_the_sampled_latch_keeps_its_entry() {
    evaluator_for("PreControls.Chain", |evaluator| {
        evaluator.startup().expect("Startup");
        for tick in 0..4_u32 {
            evaluator.do_step().expect("DoStep");
            let state = f64::from(tick + 1);
            assert_eq!(
                [
                    real(evaluator.state("state").unwrap()),
                    real(evaluator.state("doubled").unwrap()),
                    real(evaluator.state("step").unwrap()),
                    real(evaluator.state("latched").unwrap()),
                ],
                [state, 2.0 * state + 1.0, 0.0, 2.0 * (state - 1.0) + 1.0],
                "tick {tick}: state, doubled, step, latched"
            );
        }
    });
}

/// An Integer defined outside any `when` is a discrete value the event
/// assigns, so an equation-section pre() of it settles on the value this tick
/// assigned.
#[test]
fn an_equation_pre_of_an_event_assigned_integer_publishes_the_settled_tick() {
    evaluator_for("PreControls.IntegerOwner", |evaluator| {
        evaluator.startup().expect("Startup");
        for tick in 0..4_i32 {
            evaluator.do_step().expect("DoStep");
            let Value::Integer(count) = *evaluator.state("count").unwrap() else {
                panic!("count is an Integer");
            };
            assert_eq!(count, i64::from(tick + 2), "tick {tick}: count");
            assert_eq!(
                real(evaluator.state("lagged").unwrap()),
                f64::from(tick + 2),
                "tick {tick}: lagged is the settled pre(count)"
            );
        }
    });
}
