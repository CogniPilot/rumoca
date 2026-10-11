use super::*;

const SOURCE: &str = r#"
model QuotientComparison
  parameter Integer n = 0;
  Real x(start=0, fixed=true);
equation
  der(x) = if div(63050394783186944+n, 7)+1 > div(63050394783186944+n, 7)
           then 1 else 0;
end QuotientComparison;
"#;

#[test]
fn integer_quotient_enclosing_comparison_preserves_exact_branch_and_full_trace() {
    let compiled = compile(SOURCE, "QuotientComparison");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        // Independently: the two quotients are 2^53-1 and 2^53 respectively.
        // In Integer arithmetic q+1>q is true in both cases. n=-7 is the
        // representable-addition control; n=0 crosses the Binary64 boundary.
        for parameter in [-7.0, 0.0] {
            let mut opts = options(policy);
            opts.param_overrides = vec![("n".into(), parameter)];
            let result = simulate_dae(&compiled.dae, &opts)
                .unwrap_or_else(|error| panic!("{policy:?}, n={parameter}: {error:?}"));
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
                    "{policy:?}, n={parameter}: sample{sample}, x={value}, expected={expected_time}"
                );
            }
        }
    }
}
