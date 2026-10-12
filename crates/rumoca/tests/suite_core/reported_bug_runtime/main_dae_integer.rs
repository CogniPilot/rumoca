use super::*;
use rumoca_solver::timeline::sample_time_match_with_tol;

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
    // MLS §§3.7.2/3.8.5: Integer inputs change only at events, so their
    // quotient needs no continuous boundary and retains checked domain faults.
    let dynamic = "model IntegerFault input Integer n(start=0); input Integer d(start=7); output Integer y; equation y=mod(9007199254740993+n,d); end IntegerFault;";
    integer_fault(
        dynamic,
        &[("n", 0.0), ("d", 7.0)],
        5.0,
        &[("n", 0.0), ("d", 0.0)],
        None,
    );
    // A continuously varying Real divisor still needs a different checked
    // discontinuity owner; the Integer capability does not admit this form.
    let continuous = "model IntegerFault input Real n(start=0); input Real d(start=7); output Real y; equation y=mod(9007199254740993+n,d); end IntegerFault;";
    let error = Compiler::new()
        .model("IntegerFault")
        .compile_str(continuous, "ReportedBug.mo")
        .expect_err("a continuous Real divisor is outside the checked DAE profile");
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

#[test]
fn structured_real_quotient_keeps_each_domain_point_boundary_and_trace() {
    // MLS §3.7.2/§8.5: crossings at t=5/8 and 6/8 belong to distinct
    // points of the one compact two-element family (SPEC_0032 §§1–3).
    let compiled = compile(
        "model StructuredRealQuotient output Real y[2]; Real x[2](each start=0, each fixed=true); equation for i in 1:2 loop y[i]=mod(8*time+i,7); end for; der(x)=y; end StructuredRealQuotient;",
        "StructuredRealQuotient",
    );
    compiled.dae.inspect(|view| {
        assert_eq!(view.runtime_quotient_owner_count(), 1);
        assert_eq!(view.structured_root_count(), 1);
        assert_eq!(view.root_count(), 0);
    });
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let opts = options(policy);
        let mut session = SimulationSession::new(&compiled.dae, opts.clone()).unwrap();
        for sample in 0..=10 {
            let time = f64::from(sample) / 10.0;
            if sample != 0 {
                session.advance_to(time).unwrap();
            }
            let state = session.state().unwrap();
            assert_eq!(state.time, time);
            for (axis, name, integral_name) in [(1.0, "y[1]", "x[1]"), (2.0, "y[2]", "x[2]")] {
                let expected = (8.0 * time + axis) % 7.0;
                let actual = state.values[name];
                assert!(
                    actual.is_finite()
                        && (actual - expected).abs()
                            <= 20.0 * (opts.atol + opts.rtol * expected.abs()),
                    "{policy:?} {name}({time})={actual}, expected={expected}"
                );
                let crossing = (7.0 - axis) / 8.0;
                let expected_integral =
                    4.0 * time * time + axis * time - 7.0 * (time - crossing).max(0.0);
                let actual_integral = state.values[integral_name];
                assert!(
                    actual_integral.is_finite()
                        && (actual_integral - expected_integral).abs()
                            <= 20.0 * (opts.atol + opts.rtol * expected_integral.abs()),
                    "{policy:?} {integral_name}({time})={actual_integral}, expected={expected_integral}"
                );
            }
        }
    }
}

#[test]
fn structured_real_quotient_initializes_the_same_producer_in_both_domain_directions() {
    // Adding two periods leaves the analytic value/trace unchanged, but the
    // discontinuous step starts at two. Zero-filled hidden memory is wrong.
    for range in ["1:2", "2:-1:1"] {
        let source = format!(
            "model InitializedStructuredQuotient output Real y[2]; Real x[2](each start=0, each fixed=true); equation for i in {range} loop y[i]=mod(8*time+i+14,7); end for; der(x)=y; end InitializedStructuredQuotient;"
        );
        let compiled = compile(&source, "InitializedStructuredQuotient");
        for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
            check_initialized_structured_policy(&compiled, policy, range);
        }
    }
}

#[test]
fn structured_real_quotient_refuses_generated_targets_without_initialization_support() {
    let source = r#"model StructuredQuotientExport
  output Real y[2]; Real x[2](each start=0,each fixed=true);
equation
  for i in 1:2 loop y[i]=mod(8*time+i,7); end for;
  der(x)=y;
end StructuredQuotientExport;"#;
    let compiled = compile(source, "StructuredQuotientExport");
    let error = compiled
        .render_solve_template_str_without_dae("{{ model_name }}", "StructuredQuotientExport")
        .expect_err("a generated target must not omit numeric Map initialization");
    assert!(
        error
            .to_string()
            .contains("structured quotient initialization"),
        "{error:?}"
    );
}

#[test]
fn real_quotient_inactive_arm_does_not_evaluate_an_undefined_ratio() {
    // MLS §3.3 if-expression selection and §8.5 event construction must retain
    // the source activation: this arm never executes on the requested horizon.
    // The exact public model failed root evaluation at t=0 on source 4b3e7c22.
    for numerator in ["1/(time+1)", "1/time"] {
        let source = format!(
            "model InactiveQuotient output Real y; Real x(start=0,fixed=true); equation y=if time<0.5 then 1 else mod({numerator},7.0); der(x)=y; end InactiveQuotient;"
        );
        let compiled = compile(&source, "InactiveQuotient");
        for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
            check_inactive_quotient_policy(&compiled, policy, numerator);
        }
    }
}

#[test]
fn real_quotient_selected_undefined_ratio_remains_a_fault() {
    let compiled = compile(
        "model SelectedQuotient output Real y; Real x(start=0,fixed=true); equation y=if time<0.5 then mod(1/time,7.0) else 1; der(x)=y; end SelectedQuotient;",
        "SelectedQuotient",
    );
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let error = simulate_dae(&compiled.dae, &options(policy))
            .expect_err("an active undefined quotient must retain its evaluation fault");
        assert!(
            error.to_string().contains("non-finite"),
            "{policy:?}: {error:?}"
        );
    }
}

#[test]
fn real_quotient_nested_first_true_arms_protect_later_conditions_and_ratios() {
    let source = "model FirstTrueQuotient output Real y; Real x(start=0,fixed=true); equation y=if time<0.5 then (if time<0.25 then 1 else mod(1/time,7.0)) elseif mod(1/time,7.0)>0 then 2 else 3; der(x)=y; end FirstTrueQuotient;";
    let compiled = compile(source, "FirstTrueQuotient");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.t_end = 0.1;
        opts.dt = Some(0.01);
        let result = simulate_dae(&compiled.dae, &opts)
            .expect("both nested and later-arm undefined ratios are inactive");
        assert_eq!(result.times.len(), 11);
        for (ordinal, (&time, &value)) in result.times.iter().zip(series(&result, "y")).enumerate()
        {
            assert_eq!(time, ordinal as f64 / 100.0);
            assert_eq!(value, 1.0);
            let state = series(&result, "x")[ordinal];
            assert!((state - time).abs() <= 20.0 * (opts.atol + opts.rtol * time.abs()));
        }
    }
}

#[test]
fn real_quotient_structured_inactive_points_keep_their_own_step() {
    let source = "model GuardedStructuredQuotient output Real y[2]; Real x[2](each start=0,each fixed=true); equation for i in 1:2 loop y[i]=if time<0.5 then mod(14*i+8*time+i,7.0) else i; end for; der(x)=y; end GuardedStructuredQuotient;";
    let compiled = compile(source, "GuardedStructuredQuotient");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        check_guarded_structured_policy(&compiled, policy);
    }
}

#[test]
fn model_arm_relation_protects_inactive_arithmetic_without_a_quotient() {
    let compiled = compile(
        "model GuardedRelation output Real y; Real x(start=0,fixed=true); equation y=if time<0.5 then 1 elseif sqrt(time-0.5)>0 then 2 else 3; der(x)=y; end GuardedRelation;",
        "GuardedRelation",
    );
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let opts = options(policy);
        let result = simulate_dae(&compiled.dae, &opts)
            .expect("inactive square root remains lazy; the later arm activates at its boundary");
        let states = series(&result, "x");
        let outputs = series(&result, "y");
        eprintln!(
            "{policy:?}: times={:?}, x={states:?}, y={outputs:?}",
            result.times
        );
        assert_eq!(result.times.first(), Some(&opts.t_start));
        assert_eq!(result.times.last(), Some(&opts.t_end));
        assert!(result.times.iter().all(|time| time.is_finite()));
        assert!(result.times.windows(2).all(|pair| pair[0] <= pair[1]));
        // The common ME session publishes event-left and settled observations.
        // Require the original grid and bound any extra observation to the
        // source event at 0.5, then check every recorded state and output.
        for ordinal in 0..=10 {
            let requested = ordinal as f64 / 10.0;
            let count = result
                .times
                .iter()
                .filter(|time| sample_time_match_with_tol(**time, requested))
                .count();
            assert!(
                count > 0,
                "{policy:?}: requested output time {requested} is missing"
            );
            if ordinal != 5 {
                assert_eq!(
                    count, 1,
                    "{policy:?}: extra observation away from the 0.5 event at {requested}"
                );
            }
        }
        assert_eq!(states.len(), result.times.len());
        assert_eq!(outputs.len(), result.times.len());
        for (ordinal, &time) in result.times.iter().enumerate() {
            assert!(
                (0..=10).any(|sample| sample_time_match_with_tol(time, sample as f64 / 10.0)),
                "{policy:?}: unexpected observation at {time}; only the requested grid and the 0.5 event are valid"
            );
            let expected = time.min(0.5) + 2.0 * (time - 0.5).max(0.0);
            let actual = states[ordinal];
            assert!(
                actual.is_finite()
                    && (actual - expected).abs() <= 20.0 * (opts.atol + opts.rtol * expected.abs()),
                "{policy:?}: x({time})={actual}, expected {expected}"
            );
            assert!(outputs[ordinal].is_finite());
            // The event observation itself may report a left or right limit;
            // on either open interval the selected source branch is exact.
            if !sample_time_match_with_tol(time, 0.5) {
                assert_eq!(outputs[ordinal], guarded_relation_output(time));
            }
        }
    }
}

#[test]
fn compact_model_arm_relations_keep_each_domain_point_activation() {
    // The nested domain-local relations occur in the authored QuadrotorAcro
    // rate controller. This smaller source isolates their event owner.
    let compiled = compile(
        "model StructuredArmRelation Real x[2](start={2,-2},each fixed=true); output Real y[2]; equation for i in 1:2 loop y[i]=if x[i]>1 then -x[i] elseif x[i]<-1 then -2*x[i] else x[i]; end for; der(x)=y; end StructuredArmRelation;",
        "StructuredArmRelation",
    );
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let opts = SimOptions {
            t_end: 0.2,
            ..options(policy)
        };
        let result = simulate_dae(&compiled.dae, &opts)
            .expect("nested structured relations retain their exact model arm");
        assert_eq!(result.times, vec![0.0, 0.1, 0.2]);
        for (name, rate, initial) in [("x[1]", 1.0, 2.0), ("x[2]", 2.0, -2.0)] {
            let states = series(&result, name);
            let output_name = name.replace('x', "y");
            let outputs = series(&result, &output_name);
            assert_eq!(states.len(), result.times.len());
            assert_eq!(outputs.len(), result.times.len());
            for (ordinal, &time) in result.times.iter().enumerate() {
                let expected = initial * (-rate * time).exp();
                assert!(
                    states[ordinal].is_finite()
                        && (states[ordinal] - expected).abs()
                            <= 20.0 * (opts.atol + opts.rtol * expected.abs()),
                    "{policy:?}: {name}({time})={}, expected {expected}",
                    states[ordinal]
                );
                assert!(
                    outputs[ordinal].is_finite()
                        && (outputs[ordinal] + rate * states[ordinal]).abs() <= opts.atol,
                    "{policy:?}: {output_name}({time})={}, expected {}",
                    outputs[ordinal],
                    -rate * states[ordinal]
                );
            }
        }
    }
}

fn check_initialized_structured_policy(
    compiled: &CompilationResult,
    policy: SimExecutionPolicy,
    range: &str,
) {
    let opts = options(policy);
    let mut session = SimulationSession::new(&compiled.dae, opts.clone()).unwrap();
    for sample in 0..=10 {
        let time = f64::from(sample) / 10.0;
        if sample != 0 {
            session.advance_to(time).unwrap();
        }
        let state = session.state().unwrap();
        assert_eq!(state.time, time);
        for (axis, output, integral) in [(1.0, "y[1]", "x[1]"), (2.0, "y[2]", "x[2]")] {
            let expected_output = (8.0 * time + axis) % 7.0;
            let crossing = (7.0 - axis) / 8.0;
            let expected_integral =
                4.0 * time * time + axis * time - 7.0 * (time - crossing).max(0.0);
            for (name, expected) in [(output, expected_output), (integral, expected_integral)] {
                let actual = state.values[name];
                assert!(
                    actual.is_finite()
                        && (actual - expected).abs()
                            <= 20.0 * (opts.atol + opts.rtol * expected.abs()),
                    "{policy:?} range={range} {name}({time})={actual}, expected={expected}"
                );
            }
        }
    }
}

fn check_inactive_quotient_policy(
    compiled: &CompilationResult,
    policy: SimExecutionPolicy,
    numerator: &str,
) {
    let mut opts = options(policy);
    opts.t_end = 0.1;
    opts.dt = Some(0.01);
    let mut session = SimulationSession::new(&compiled.dae, opts.clone())
        .unwrap_or_else(|error| panic!("inactive {numerator}, {policy:?}: {error:?}"));
    for sample in 0..=10 {
        let time = f64::from(sample) / 100.0;
        if sample != 0 {
            session
                .advance_to(time)
                .expect("inactive arm remains protected");
        }
        let state = session.state().unwrap();
        assert_eq!(state.time, time);
        assert_eq!(state.values["y"], 1.0);
        let actual = state.values["x"];
        assert!(
            actual.is_finite()
                && (actual - time).abs() <= 20.0 * (opts.atol + opts.rtol * time.abs()),
            "{policy:?}: x({time})={actual}, expected {time}"
        );
    }
}

fn check_guarded_structured_policy(compiled: &CompilationResult, policy: SimExecutionPolicy) {
    let opts = options(policy);
    let mut session = SimulationSession::new(&compiled.dae, opts.clone())
        .expect("point-specific nonzero initial steps");
    for sample in 0..=10 {
        let time = sample as f64 / 10.0;
        if sample != 0 {
            session.advance_to(time).unwrap();
        }
        let state = session.state().unwrap();
        assert_eq!(state.time, time);
        for (axis, output, integral) in [(1.0, "y[1]", "x[1]"), (2.0, "y[2]", "x[2]")] {
            let expected = if time < 0.5 { 8.0 * time + axis } else { axis };
            let expected_integral = 4.0 * time.min(0.5).powi(2) + axis * time;
            for (name, expected) in [(output, expected), (integral, expected_integral)] {
                let actual = state.values[name];
                assert!(
                    actual.is_finite()
                        && (actual - expected).abs()
                            <= 20.0 * (opts.atol + opts.rtol * expected.abs()),
                    "{policy:?} {name}({time})={actual}, expected {expected}"
                );
            }
        }
    }
}

fn guarded_relation_output(time: f64) -> f64 {
    if time < 0.5 { 1.0 } else { 2.0 }
}
