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
    for (policy, (input, slope)) in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto]
        .into_iter()
        .flat_map(|policy| [(0.0, 1.0), (-1.0, 0.0)].map(|case| (policy, case)))
    {
        let mut opts = options(policy);
        opts.initial_inputs = vec![("n".into(), input)];
        let mut session = SimulationSession::new(&compiled.dae, opts.clone())
            .unwrap_or_else(|error| panic!("{policy:?}, n={input}: {error:?}"));
        for sample in 0..=10 {
            let time = f64::from(sample) / 10.0;
            if sample != 0 {
                session
                    .advance_to(time)
                    .unwrap_or_else(|error| panic!("{policy:?}, n={input}, t={time}: {error:?}"));
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

// MLS §16.6 / SPEC_0022 CLK-015: use a clocked when-equation.
const CLOCKED_SOURCE: &str = r#"
function clockedInc
  input Integer n;
  output Integer y;
algorithm
  y := n + 1;
  assert(y > 0, "clocked inc rejects");
end clockedInc;

model ClockedIntegerCallAssertion
  parameter Integer n=0 annotation(Evaluate=false);
  output Boolean flag(start=false);
  Clock c=Clock(1, 4);
equation
  when c then
    flag = clockedInc(n) > 0;
  end when;
end ClockedIntegerCallAssertion;
"#;

#[test]
fn clocked_integer_comparison_retains_original_assertion_projection() {
    let compiled = compile(CLOCKED_SOURCE, "ClockedIntegerCallAssertion");
    compiled.dae.inspect(|view| {
        assert_eq!(view.model_event_transaction_count(), 0);
    });
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.param_overrides = vec![("n".into(), 0.0)];
        let lowered = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts)
            .expect("the clocked equation retains its issued assertion action projection");
        assert!(lowered.problem.events.root_conditions.is_empty());
        assert_eq!(lowered.problem.events.actions.len(), 1);
        assert!(lowered.problem.events.actions[0].clock_owner.is_some());
        assert!(lowered.problem.discrete.event_transactions.is_empty());
        let mut session = SimulationSession::new(&compiled.dae, opts.clone())
            .expect("the holding clock-owned assertion initializes");
        for sample in 0..=10 {
            let time = f64::from(sample) / 10.0;
            if sample != 0 {
                session
                    .advance_to(time)
                    .expect("the clock-owned assertion holds");
            }
            let state = session.state().expect("actual clocked output");
            assert_eq!(state.time, time);
            assert_eq!(state.values["flag"], 1.0, "{policy:?}, t={time}");
        }
        opts.param_overrides = vec![("n".into(), -2.0)];
        let error = rumoca_sim::simulate_dae_with_diagnostics(&compiled.dae, &opts)
            .expect_err("the active clock-owned call still asserts at its first tick");
        assert!(
            error.to_string().contains("clocked inc rejects"),
            "{policy:?}: {error}"
        );
    }
}

// MLS §8.3.5 / §11.2.7: sample() owns an unclocked periodic when-algorithm.
const PERIODIC_SOURCE: &str = r#"
function periodicInc
  input Integer n;
  output Integer y;
algorithm
  y := n + 1;
  assert(y > 0, "periodic inc rejects");
end periodicInc;

model PeriodicIntegerCallAssertion
  parameter Integer n=0 annotation(Evaluate=false);
  output Boolean flag(start=false);
algorithm
  when sample(0, 0.25) then
    flag := periodicInc(n) > 0;
  end when;
end PeriodicIntegerCallAssertion;
"#;

#[test]
fn periodic_integer_comparison_retains_original_assertion_projection() {
    let compiled = compile(PERIODIC_SOURCE, "PeriodicIntegerCallAssertion");
    compiled.dae.inspect(|view| {
        assert_eq!(view.model_event_transaction_count(), 1);
    });
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.param_overrides = vec![("n".into(), 0.0)];
        let lowered = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts)
            .expect("the periodic transaction retains its issued assertion action projection");
        assert!(lowered.problem.events.root_conditions.is_empty());
        assert_eq!(lowered.problem.events.actions.len(), 1);
        assert!(lowered.problem.events.actions[0].clock_owner.is_some());
        assert_eq!(lowered.problem.discrete.event_transactions.len(), 1);
        let mut session = SimulationSession::new(&compiled.dae, opts.clone())
            .expect("the holding periodic assertion initializes");
        for sample in 0..=10 {
            let time = f64::from(sample) / 10.0;
            if sample != 0 {
                session
                    .advance_to(time)
                    .expect("the periodic assertion holds");
            }
            let state = session
                .state()
                .expect("actual periodically assigned output");
            assert_eq!(state.time, time);
            assert_eq!(state.values["flag"], 1.0, "{policy:?}, t={time}");
        }
        opts.param_overrides = vec![("n".into(), -2.0)];
        let error = rumoca_sim::simulate_dae_with_diagnostics(&compiled.dae, &opts)
            .expect_err("the active periodic call still asserts at its first sample");
        assert!(
            error.to_string().contains("periodic inc rejects"),
            "{policy:?}: {error}"
        );
    }
}
