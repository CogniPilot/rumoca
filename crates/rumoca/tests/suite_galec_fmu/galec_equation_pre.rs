//! An equation-section `pre()` of a variable written by a sampled algorithm
//! (MLS 3.7.5): the GALEC projection collects its previous value like any
//! other `pre()` read, so the reading output sees the value from before the
//! current tick, not the value the tick wrote.

use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};

/// The reduced controls from the RDD2 flight-computer report.
const PRE_CONTROLS: &str = "\
package PreControls
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
fn an_equation_pre_of_a_sampled_scalar_reads_the_previous_tick() {
    evaluator_for("PreControls.Scalar", |evaluator| {
        evaluator.startup().expect("Startup");
        let mut before = 0.0;
        for (tick, u) in [3.0, -1.5, 7.25, 0.5].into_iter().enumerate() {
            evaluator.set_state("u", Value::Real(u)).expect("set u");
            evaluator.do_step().expect("DoStep");
            assert_eq!(
                real(evaluator.state("previous").unwrap()),
                before,
                "tick {tick}: previous must be held from before this tick"
            );
            assert_eq!(real(evaluator.state("held").unwrap()), u, "tick {tick}");
            before = u;
        }
    });
}

#[test]
fn an_equation_pre_of_a_sampled_vector_reads_the_previous_tick() {
    evaluator_for("PreControls.Vector", |evaluator| {
        evaluator.startup().expect("Startup");
        let mut before = [0.0; 3];
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
                before,
                "tick {tick}: previous must be held from before this tick"
            );
            before = u;
        }
    });
}
