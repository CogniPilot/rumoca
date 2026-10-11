use super::*;

const SOURCE: &str = r#"
function inc
  input Integer n;
  output Integer y;
algorithm
  y := n + 1;
end inc;

model IntegerCallComparison
  input Integer n(start=0);
  Real x(start=0, fixed=true);
equation
  der(x) = if inc(n) > 0 then 1 else 0;
end IntegerCallComparison;
"#;

#[test]
fn integer_comparison_descendant_call_preserves_live_input_and_full_trace() {
    let compiled = compile(SOURCE, "IntegerCallComparison");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        for (input, slope) in [(0.0, 1.0), (-1.0, 0.0)] {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("n".into(), input)];
            let mut session = SimulationSession::new(&compiled.dae, opts.clone())
                .unwrap_or_else(|error| panic!("{policy:?}, n={input}: {error:?}"));
            for sample in 0..=10 {
                let time = f64::from(sample) / 10.0;
                if sample != 0 {
                    session.advance_to(time).unwrap_or_else(|error| {
                        panic!("{policy:?}, n={input}, t={time}: {error:?}")
                    });
                }
                let state = session.state().expect("actual session output");
                assert_eq!(state.time, time);
                let actual = state.values["x"];
                let expected = slope * time;
                let bound = 20.0 * (opts.atol + opts.rtol * expected.abs());
                assert!(
                    actual.is_finite() && (actual - expected).abs() <= bound,
                    "{policy:?}, n={input}, sample{sample}: x={actual}, expected={expected}"
                );
                eprintln!(
                    "IntegerCallComparison policy={policy:?} n={input} sample={sample} receipt={:?}",
                    session.execution_receipt()
                );
            }
        }
    }
}

const ASSERTING_SOURCE: &str = r#"
function guardedInc
  input Integer n;
  output Integer y;
algorithm
  y := n + 1;
  assert(y > 0, "inc n=" + String(n) + " y=" + String(y));
end guardedInc;

function shifted
  input Integer n;
  output Integer y;
algorithm
  y := guardedInc(n) + 1;
end shifted;

model IntegerCallAssertion
  input Integer n(start=0);
  input Boolean active(start=true);
  Real x(start=0, fixed=true);
equation
  der(x) = if (if active then shifted(n) else 1) > 0 then 1 else 0;
end IntegerCallAssertion;
"#;

#[test]
fn integer_comparison_call_assertions_hold_or_remain_inactive_on_full_trace() {
    let compiled = compile(ASSERTING_SOURCE, "IntegerCallAssertion");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        for (input, active) in [(0.0, 1.0), (-2.0, 0.0)] {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("n".into(), input), ("active".into(), active)];
            let result = rumoca_sim::simulate_dae_with_diagnostics(&compiled.dae, &opts)
                .unwrap_or_else(|error| panic!("{policy:?}, n={input}, active={active}: {error}"));
            assert_eq!(result.times.len(), 11);
            let values = series(&result, "x");
            assert_eq!(values.len(), 11);
            for (sample, (&time, &value)) in result.times.iter().zip(values).enumerate() {
                let expected = sample as f64 / 10.0;
                assert!((time - expected).abs() <= f64::EPSILON);
                assert!(
                    value.is_finite()
                        && (value - expected).abs() <= 20.0 * (opts.atol + opts.rtol * expected),
                    "{policy:?}, n={input}, active={active}: x({time})={value}, expected={expected}"
                );
            }
        }
    }
}

#[test]
fn integer_comparison_active_call_assertion_renders_its_declaring_frame() {
    let compiled = compile(ASSERTING_SOURCE, "IntegerCallAssertion");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.initial_inputs = vec![("n".into(), -2.0), ("active".into(), 1.0)];
        let error = rumoca_sim::simulate_dae_with_diagnostics(&compiled.dae, &opts)
            .expect_err("the selected nested call violates its assertion");
        let message = error.to_string();
        assert!(message.contains("inc n=-2 y=-1"), "{policy:?}: {message}");
    }
}
