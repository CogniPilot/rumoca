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
