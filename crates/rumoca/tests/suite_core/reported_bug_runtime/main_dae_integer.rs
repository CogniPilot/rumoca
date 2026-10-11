use super::*;

const OWNED_INTEGER_SOURCE: &str = "model OwnedInteger input Integer n(start=0); output Integer y; equation y=mod(sum({9007199254740993+n+i for i in 1:2}),7); end OwnedInteger;";

#[test]
fn main_dae_integer_quotients_preserve_negative_operand_rules() {
    // Independently tabulated MLS truncating div/rem and divisor-signed mod.
    for (numerator, divisor, expected) in [
        (
            -9_007_199_254_740_993_i64,
            7,
            [-1_286_742_750_677_284_i64, 2, -5],
        ),
        (9_007_199_254_740_993, -7, [-1_286_742_750_677_284, -2, 5]),
        (-9_007_199_254_740_993, -7, [1_286_742_750_677_284, -5, -5]),
    ] {
        for (operator, expected) in ["div", "mod", "rem"].into_iter().zip(expected) {
            let source = format!(
                "model SignedQuotient input Integer n(start=0); output Integer y; equation y={operator}({numerator}+n,{divisor}); end SignedQuotient;"
            );
            let compiled = compile(&source, "SignedQuotient");
            for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
                let mut opts = options(policy);
                opts.initial_inputs = vec![("n".into(), 0.0)];
                let result = simulate_dae(&compiled.dae, &opts).unwrap();
                assert_eq!(
                    last(&result, "y"),
                    expected as f64,
                    "{policy:?}: {operator}({numerator},{divisor})"
                );
            }
        }
    }
}

fn integer_fault(
    source: &str,
    control: &[(&str, f64)],
    expected: f64,
    invalid: &[(&str, f64)],
    parameter: Option<(&str, f64, f64)>,
) {
    let compiled = compile(source, "IntegerFault");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.initial_inputs = control
            .iter()
            .map(|(name, value)| ((*name).into(), *value))
            .collect();
        if let Some((name, valid, _)) = parameter {
            opts.param_overrides = vec![(name.into(), valid)];
        }
        let result =
            simulate_dae(&compiled.dae, &opts).expect("same compiled model has a valid control");
        assert_eq!(last(&result, "y"), expected, "{policy:?}");
        opts.initial_inputs = invalid
            .iter()
            .map(|(name, value)| ((*name).into(), *value))
            .collect();
        if let Some((name, _, invalid)) = parameter {
            opts.param_overrides = vec![(name.into(), invalid)];
        }
        let error =
            simulate_dae(&compiled.dae, &opts).expect_err("selected Integer arithmetic must fault");
        let diagnostic = format!("{error:?}");
        if parameter.is_some_and(|(_, _, invalid)| invalid == 0.0) {
            // The quotient's generated boundary indicator can observe the
            // undefined parameter reciprocal before the Integer publication.
            assert!(
                diagnostic.contains("domain")
                    || diagnostic.contains("zero")
                    || diagnostic.contains("non-finite"),
                "{policy:?}: {diagnostic}"
            );
        } else {
            assert!(diagnostic.contains("Integer"), "{policy:?}: {diagnostic}");
            assert!(
                diagnostic.contains("domain") || diagnostic.contains("zero"),
                "{policy:?}: {diagnostic}"
            );
        }
    }
}

#[test]
fn main_dae_integer_quotients_refuse_zero_divisors_and_checked_arithmetic_overflow() {
    // Model-level dynamic divisors need a different discontinuity owner;
    // retain their typed admission refusal instead of pretending they run.
    let dynamic = "model IntegerFault input Integer n(start=0); input Integer d(start=7); output Integer y; equation y=mod(9007199254740993+n,d); end IntegerFault;";
    let error = Compiler::new()
        .model("IntegerFault")
        .compile_str(dynamic, "ReportedBug.mo")
        .expect_err("a changing model divisor is outside the checked DAE profile");
    let rumoca::CompilerError::CompileDiagnosticsError { failures, .. } = error else {
        panic!("dynamic divisor must retain a typed phase diagnostic: {error:?}");
    };
    assert!(
        failures
            .iter()
            .any(|failure| failure.error_code.as_deref() == Some("ED018")),
        "{failures:?}"
    );
    integer_fault(
        "model IntegerFault input Integer n(start=0); parameter Integer d=7 annotation(Evaluate=false); output Integer y; equation y=mod(9007199254740993+n,d); end IntegerFault;",
        &[("n", 0.0)],
        5.0,
        &[("n", 0.0)],
        Some(("d", 7.0, 0.0)),
    );
    integer_fault(
        "model IntegerFault input Integer n(start=0); output Integer y; equation y=mod(9223372036854775807+n,7); end IntegerFault;",
        &[("n", -1.0)],
        6.0,
        &[("n", 1.0)],
        None,
    );
    integer_fault(
        "model IntegerFault input Integer n(start=0); output Integer y; equation y=mod(9223372036854775807*n,7); end IntegerFault;",
        &[("n", 0.0)],
        0.0,
        &[("n", 2.0)],
        None,
    );
    integer_fault(
        "model IntegerFault input Integer n(start=1); parameter Integer d=1 annotation(Evaluate=false); output Integer y; equation y=div(-9223372036854775807-n,d); end IntegerFault;",
        &[("n", 1.0)],
        i64::MIN as f64,
        &[("n", 1.0)],
        Some(("d", 1.0, -1.0)),
    );
}

#[test]
fn main_dae_integer_quotients_publish_only_exact_scalar_abi_values() {
    let source = "model IntegerPublication input Integer n(start=0); output Integer y; equation y=div(9007199254740993+n,1); end IntegerPublication;";
    let compiled = compile(source, "IntegerPublication");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.initial_inputs = vec![("n".into(), 1.0)];
        let result = simulate_dae(&compiled.dae, &opts)
            .expect("exact even result above 2^53 remains supported");
        assert_eq!(last(&result, "y"), 9_007_199_254_740_994.0);
        opts.initial_inputs = vec![("n".into(), 0.0)];
        let error = simulate_dae(&compiled.dae, &opts)
            .expect_err("odd Integer result must not round at publication");
        assert!(
            format!("{error:?}").contains("scalar ABI"),
            "{policy:?}: {error:?}"
        );
    }
}

#[test]
fn main_dae_integer_quotients_keep_issued_outer_and_owned_comprehension_binders() {
    let outer = compile(
        "model OuterInteger input Integer n(start=0); output Integer y[2]; equation for i in 1:2 loop y[i]=mod(9007199254740993+n+i,7); end for; end OuterInteger;",
        "OuterInteger",
    );
    let owned = compile(OWNED_INTEGER_SOURCE, "OwnedInteger");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        for (input, expected, expected_sum) in [(0.0, [6.0, 0.0], 6.0), (1.0, [0.0, 1.0], 1.0)] {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("n".into(), input)];
            let result = simulate_dae(&outer.dae, &opts).unwrap();
            for (name, value) in ["y[1]", "y[2]"].into_iter().zip(expected) {
                assert_eq!(
                    last(&result, name),
                    value,
                    "{policy:?}: outer binder, n={input}"
                );
            }
            let result = simulate_dae(&owned.dae, &opts).unwrap();
            assert_eq!(
                last(&result, "y"),
                expected_sum,
                "{policy:?}: owned binder, n={input}"
            );
        }
    }
}

#[test]
fn main_dae_integer_quotients_owned_comprehension_executes_independently() {
    let compiled = compile(OWNED_INTEGER_SOURCE, "OwnedInteger");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        // Independent: (2*9007199254740993 + 3 + 2*n) mod 7 is 6 or 1.
        for (input, expected) in [(0.0, 6.0), (1.0, 1.0)] {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("n".into(), input)];
            let result = simulate_dae(&compiled.dae, &opts)
                .unwrap_or_else(|error| panic!("{policy:?}, n={input}: {error:?}"));
            assert_eq!(result.times.len(), 11);
            assert_eq!(result.times.first().copied(), Some(0.0));
            assert_eq!(result.times.last().copied(), Some(1.0));
            let values = series(&result, "y");
            assert_eq!(values.len(), result.times.len());
            for &value in values {
                assert_eq!(value, expected, "{policy:?}, n={input}");
            }
        }
    }
}
