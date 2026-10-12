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

#[test]
fn integer_comparison_operators_and_mixed_real_promotion_keep_their_types() {
    for (replacement, slope) in [
        ("+1 <", 0.0),
        ("+1 <=", 0.0),
        ("+1 >=", 1.0),
        ("+1 ==", 0.0),
        ("+1 <>", 1.0),
        // MLS §6.7: adding Real zero converts the quotient before adding one.
        // Binary64 then rounds q+1 to q; this mixed comparison is false.
        ("+0.0+1 >", 0.0),
    ] {
        assert_eq!(SOURCE.matches("+1 >").count(), 1);
        let source = SOURCE.replacen("+1 >", replacement, 1);
        let compiled = compile(&source, "QuotientComparison");
        for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("m".into(), 0.0)];
            let result = simulate_dae(&compiled.dae, &opts)
                .unwrap_or_else(|error| panic!("{replacement}, {policy:?}: {error:?}"));
            assert_eq!(result.times.len(), 11);
            assert_eq!(result.times.first().copied(), Some(0.0));
            assert_eq!(result.times.last().copied(), Some(1.0));
            let values = series(&result, "x");
            assert_eq!(values.len(), 11);
            for (&time, &value) in result.times.iter().zip(values) {
                assert!(
                    value.is_finite() && (value - slope * time).abs() <= opts.atol,
                    "{replacement}, {policy:?}: x({time})={value}, slope={slope}"
                );
            }
        }
    }
}

#[test]
fn integer_comparison_native_owner_and_full_session_do_not_fall_back() {
    let compiled = compile(SOURCE, "QuotientComparison");
    let mut opts = options(SimExecutionPolicy::Auto);
    opts.initial_inputs = vec![("m".into(), 0.0)];
    let lowered = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts)
        .expect("exact Integer predicate lowers through the canonical owner");
    assert!(!lowered.pure_calls.owners().is_empty());
    let _native_calls = rumoca_exec_cranelift::compile_pure_call_table(&lowered.pure_calls)
        .expect("the complete Integer predicate compiles through the native typed owner");
    let mut session = SimulationSession::new(&compiled.dae, opts.clone())
        .expect("native Integer predicate initializes");
    for sample in 0..=10 {
        let time = f64::from(sample) / 10.0;
        if sample != 0 {
            session.advance_to(time).expect("native predicate advances");
        }
        let state = session.state().expect("actual native state");
        assert_eq!(state.time, time);
        let actual = state.values["x"];
        assert!(actual.is_finite() && (actual - time).abs() <= opts.atol);
        let receipt = session.execution_receipt();
        assert_eq!(receipt.engine, SimExecutionEngine::Cranelift, "{receipt:?}");
        assert_eq!(receipt.refusal, None, "{receipt:?}");
        assert!(receipt.declined.is_empty(), "{receipt:?}");
    }
}
