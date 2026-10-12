//! MLS §8.5 treats `when` bodies with implicit `noEvent`; body `if` guards
//! read the values solved at that instant, while the activation retains its edge.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};
use rumoca_solver::SimExecutionPolicy;
use rumoca_solver::fmi_me::{MeError, session::MeSessionError};

const SOURCE: &str =
    include_str!("../../../../docs/verification/pr427-workshop-ci/sampled-extrema-reproduction.mo");

#[test]
fn a_sampled_body_condition_does_not_create_a_crossing_root() {
    let compiled = Compiler::new()
        .model("SampledExtremaReproduction")
        .compile_str(SOURCE, "SampledExtremaReproduction.mo")
        .unwrap();
    compiled
        .dae
        .inspect(|view| assert_eq!(view.root_count(), 0));
}

#[test]
fn a_sampled_body_condition_reads_the_same_event_extremum() {
    assert_sampled_timestamps(SOURCE);
}

#[test]
fn explicit_noevent_keeps_the_same_sampled_body_values() {
    let source = SOURCE.replace(
        "if y_max > pre(y_max) then",
        "if noEvent(y_max > pre(y_max)) then",
    );
    assert_sampled_timestamps(&source);
}

#[test]
fn a_when_body_assertion_has_no_crossing_root_and_preserves_its_trace() {
    let compiled = compile_body_assertion(-1.0);
    compiled
        .dae
        .inspect(|view| assert_eq!(view.root_count(), 0));
    for execution_policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let result = simulate_dae(
            &compiled.dae,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.05),
                execution_policy,
                ..Default::default()
            },
        )
        .unwrap();
        let column = result.names.iter().position(|name| name == "x").unwrap();
        for (row, &time) in result.times.iter().enumerate() {
            assert!((result.data[column][row] - (0.15 - time)).abs() < 1e-9);
        }
        assert!((result.times.last().unwrap() - 0.2).abs() < 1e-10);
    }
}

#[test]
fn a_when_body_assertion_still_fails_at_its_first_selected_tick() {
    let compiled = compile_body_assertion(0.1);
    compiled
        .dae
        .inspect(|view| assert_eq!(view.root_count(), 0));
    for execution_policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let error = simulate_dae(
            &compiled.dae,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.05),
                execution_policy,
                ..Default::default()
            },
        )
        .unwrap_err();
        let rumoca_sim::SimError::ModelExchangeSession(MeSessionError::Component(component)) =
            error.kind()
        else {
            panic!("{execution_policy:?}: expected a component assertion, got {error:?}");
        };
        let MeError::Assertion { time, message } = component.kind() else {
            panic!("{execution_policy:?}: expected a selected assertion, got {component:?}");
        };
        assert!((time - 0.1).abs() < 1e-10, "{execution_policy:?}: {time}");
        assert_eq!(message, "BODY_ASSERT_TICK");
    }
}

fn compile_body_assertion(threshold: f64) -> rumoca::CompilationResult {
    let source = format!(
        r#"model WhenBodyAssertion
  Real x(start=0.15, fixed=true);
equation
  der(x) = -1;
  when sample(0.1, 0.1) then
    assert(x > {threshold}, "BODY_ASSERT_TICK");
  end when;
  annotation(experiment(StopTime=0.2, Interval=0.05));
end WhenBodyAssertion;"#
    );
    Compiler::new()
        .model("WhenBodyAssertion")
        .compile_str(&source, "WhenBodyAssertion.mo")
        .unwrap()
}

fn assert_sampled_timestamps(source: &str) {
    let compiled = Compiler::new()
        .model("SampledExtremaReproduction")
        .compile_str(source, "SampledExtremaReproduction.mo")
        .unwrap();
    for execution_policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let result = simulate_dae(
            &compiled.dae,
            &SimOptions {
                t_end: 0.3,
                dt: Some(0.05),
                execution_policy,
                ..Default::default()
            },
        )
        .unwrap();
        for name in ["y_max", "t_max"] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (time, expected) in [(0.05, 0.0), (0.15, 0.1), (0.25, 0.2), (0.3, 0.3)] {
                let row = result
                    .times
                    .iter()
                    .rposition(|value| *value <= time + 1e-10)
                    .unwrap();
                let actual = result.data[column][row];
                assert!(
                    (actual - expected).abs() < 1e-10,
                    "{execution_policy:?}/{name} at {time}: {actual}, expected {expected}"
                );
            }
        }
    }
}
