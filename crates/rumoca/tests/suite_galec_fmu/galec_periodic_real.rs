//! Boolean periodic events retain their issued schedule without becoming
//! synchronous clocked variables (MLS 3.7.5 and 16.7.3).

use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_ir_dae as dae;
use rumoca_phase_galec::{GalecInput, GalecOptions, GalecTargetError, lower_to_algorithm_code};

const COUNTER: &str = include_str!("../../../../examples/models/GalecCounter.mo");

#[test]
fn authored_counter_periodic_real_uses_the_issued_boolean_event() {
    let compiled = rumoca::Compiler::new()
        .model("GalecCounter")
        .compile_str(COUNTER, "GalecCounter.mo")
        .expect("unchanged authored counter compiles");
    compiled.dae.inspect(|view| {
        assert_eq!(view.clock_count(), 1);
        assert!(
            view.clock_ownerships()
                .all(|(_, owner)| { owner.kind() != dae::ClockedVariableKind::DiscreteReal })
        );
        let equations = view.discrete_real_equations().collect::<Vec<_>>();
        assert_eq!(equations.len(), 1);
        let dae::DiscreteRealActivation::When { trigger, .. } = equations[0].activation() else {
            panic!("authored Real is owned by a Boolean when event");
        };
        let dae::ConditionOperation::Or(tick, never) = view.condition(trigger).unwrap().operation()
        else {
            panic!("Boolean periodic event retains its unowned activation");
        };
        assert!(matches!(
            view.condition(tick).unwrap().operation(),
            dae::ConditionOperation::Clock(_)
        ));
        let dae::ConditionOperation::Not(always) = view.condition(never).unwrap().operation()
        else {
            panic!("unowned activation has a never-active disjunct");
        };
        assert!(matches!(
            view.condition(always).unwrap().operation(),
            dae::ConditionOperation::Always
        ));
    });
    let package = lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, "GalecCounter"),
        &GalecOptions::default(),
    )
    .expect("periodic Boolean event projects without synchronous ownership");
    let mut evaluator = Evaluator::new(package.checked_block(), IntegerDomain::signed_32())
        .expect("checked GALEC evaluator");
    evaluator.startup().expect("Startup");
    assert_counter(&evaluator, 0);
    evaluator.recalibrate().expect("Recalibrate");
    assert_counter(&evaluator, 0);
    for tick in 1..=5 {
        evaluator.do_step().expect("DoStep");
        assert_counter(&evaluator, tick);
        assert!(evaluator.active_signals().is_empty());
    }
}

fn assert_counter(evaluator: &Evaluator<'_>, count: i64) {
    assert_eq!(evaluator.state("count").unwrap(), &Value::Integer(count));
    assert_eq!(
        evaluator.state("y").unwrap(),
        &Value::Real(2.0 * count as f64)
    );
}

fn rejected_periodic_real(source: &str, feature: &str) {
    let compiled = rumoca::Compiler::new()
        .model("Refused")
        .compile_str(source, "Refused.mo")
        .expect("valid Modelica negative control compiles");
    let errors = lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, "Refused"),
        &GalecOptions::default(),
    )
    .expect_err("unsupported event schedule stays refused");
    assert!(
        errors.iter().any(|error| matches!(error,
            GalecTargetError::UnsupportedFeature { feature: found, span: Some(_), .. }
                if found == feature
        )),
        "expected {feature}, got {errors:?}"
    );
}

#[test]
fn periodic_real_shifted_event_stays_refused() {
    rejected_periodic_real(
        "model Refused discrete Real y(start=0); equation when sample(0.05,0.1) then y=pre(y)+1; end when; end Refused;",
        "clock-phase",
    );
}

#[test]
fn periodic_real_off_tick_disjunction_stays_refused() {
    rejected_periodic_real(
        "model Refused input Boolean b; discrete Real y(start=0); equation when sample(0,0.1) or b then y=pre(y)+1; end when; end Refused;",
        "runtime-event-trigger",
    );
}

#[test]
fn periodic_real_mixed_tick_activation_stays_refused() {
    // MLS vector when syntax is valid; the current checked DAE profile refuses
    // one condition carrying distinct clock owners before target projection.
    let source = "model Refused discrete Real y(start=0); equation when {sample(0,0.1),sample(0,0.2)} then y=pre(y)+1; end when; end Refused;";
    let error = rumoca::Compiler::new()
        .model("Refused")
        .compile_str(source, "Refused.mo")
        .expect_err("mixed clock owners remain outside the checked source profile");
    let rumoca::CompilerError::CompileDiagnosticsError { ref failures, .. } = error else {
        panic!("expected checked source diagnostics, got {error}");
    };
    assert!(
        failures.iter().any(|failure| {
            failure.error_code.as_deref() == Some("ED020")
                && failure
                    .error
                    .contains("duplicate condition clock owner key")
                && failure.primary_label.is_some()
        }),
        "expected the checked mixed-clock refusal, got {error}"
    );
}

#[test]
fn periodic_real_nonintegral_clock_lattice_stays_refused() {
    rejected_periodic_real(
        "model Refused discrete Real y(start=0); discrete Real z(start=0); equation when sample(0,0.2) then y=pre(y)+1; end when; when sample(0,0.3) then z=pre(z)+1; end when; end Refused;",
        "incommensurate-clock",
    );
}
