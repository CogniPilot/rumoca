use super::*;

const SOURCE: &str = r#"
model QuotientComparison
  parameter Integer n = 0;
  input Integer m(start=0);
  Real x(start=0, fixed=true);
equation
  der(x) = if div(63050394783186944+n+m, 7)+1 > div(63050394783186944+n+m, 7)
           then 1 else 0;
end QuotientComparison;
"#;

#[test]
fn integer_quotient_enclosing_comparison_preserves_exact_branch_and_full_trace() {
    let compiled = compile(SOURCE, "QuotientComparison");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        // Independently: the two quotients are 2^53-1 and 2^53 respectively.
        // In Integer arithmetic q+1>q is true in all cases. Each -7 control
        // changes a different runtime slot before (0,0) crosses Binary64.
        for (parameter, input) in [(-7.0, 0.0), (0.0, -7.0), (0.0, 0.0)] {
            let mut opts = options(policy);
            opts.param_overrides = vec![("n".into(), parameter)];
            opts.initial_inputs = vec![("m".into(), input)];
            let result = simulate_dae(&compiled.dae, &opts)
                .unwrap_or_else(|error| panic!("{policy:?}, n={parameter}, m={input}: {error:?}"));
            assert_eq!(result.times.first().copied(), Some(0.0));
            assert_eq!(result.times.last().copied(), Some(1.0));
            assert_eq!(result.times.len(), 11, "complete 0.1 output grid");
            assert_eq!(result.names.iter().filter(|name| *name == "x").count(), 1);
            let values = series(&result, "x");
            assert_eq!(values.len(), 11);
            assert_eq!(values[0], 0.0);
            for (sample, (&time, &value)) in result.times.iter().zip(values).enumerate() {
                let expected_time = sample as f64 / 10.0;
                assert!((time - expected_time).abs() <= f64::EPSILON);
                assert!(
                    (value - expected_time).abs() <= opts.atol,
                    "{policy:?}, n={parameter}, m={input}: sample{sample}, x={value}, expected={expected_time}"
                );
            }
        }
    }
}
