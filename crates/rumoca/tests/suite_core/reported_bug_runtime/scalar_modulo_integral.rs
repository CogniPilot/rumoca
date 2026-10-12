//! Scalar control for the structured Real quotient integral (MLS §3.7.2/§8.5).

use super::*;

const SOURCE: &str = "model ScalarRealQuotient output Real y; Real x(start=0, fixed=true); equation y=mod(8*time+2,7); der(x)=y; end ScalarRealQuotient;";

fn expected_integral(time: f64) -> f64 {
    4.0 * time * time + 2.0 * time - 7.0 * (time - 5.0 / 8.0).max(0.0)
}

#[test]
fn scalar_real_quotient_preserves_integrated_state_across_its_boundary() {
    let compiled = compile(SOURCE, "ScalarRealQuotient");
    compiled.dae.inspect(|view| {
        assert_eq!(view.runtime_quotient_owner_count(), 1);
        assert_eq!(view.root_count(), 1);
        assert_eq!(view.structured_root_count(), 0);
    });
    let mut failures = Vec::new();
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        check_scalar_quotient_policy(&compiled, policy, &mut failures);
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

fn check_scalar_quotient_policy(
    compiled: &CompilationResult,
    policy: SimExecutionPolicy,
    failures: &mut Vec<String>,
) {
    let opts = options(policy);
    // The public batch trace exposes the actual localized left/right rows;
    // no product tracing knob or manufactured event samples are added.
    let trace = simulate_dae(&compiled.dae, &opts).unwrap();
    let x = series(&trace, "x");
    let y = series(&trace, "y");
    assert_eq!(trace.times.len(), x.len());
    assert_eq!(trace.times.len(), y.len());
    assert!(trace.times.iter().all(|time| time.is_finite()));
    assert!(trace.times.windows(2).all(|pair| pair[0] <= pair[1]));
    assert!(trace.times.iter().any(|time| (time - 0.625).abs() <= 1e-6));
    for (sample, &time) in trace.times.iter().enumerate() {
        if (time - 0.625).abs() <= 1e-6 {
            eprintln!(
                "scalar quotient event trace: {policy:?}, t={time:.17}, x={:.17}, y={:.17}, expected_x={:.17}",
                x[sample],
                y[sample],
                expected_integral(time)
            );
        }
    }
    let mut session = SimulationSession::new(&compiled.dae, opts.clone()).unwrap();
    for sample in 0..=10 {
        let time = f64::from(sample) / 10.0;
        if sample != 0 {
            session.advance_to(time).unwrap();
        }
        let state = session.state().unwrap();
        assert_eq!(state.time, time);
        for (name, expected) in [
            ("y", (8.0 * time + 2.0) % 7.0),
            ("x", expected_integral(time)),
        ] {
            let actual = state.values[name];
            let bound = 20.0 * (opts.atol + opts.rtol * expected.abs());
            eprintln!(
                "scalar quotient sample: {policy:?}, {name}({time})={actual:.17}, expected={expected:.17}, bound={bound:.17}"
            );
            if !actual.is_finite() || (actual - expected).abs() > bound {
                failures.push(format!(
                    "{policy:?} {name}({time})={actual}, expected={expected}, bound={bound}"
                ));
            }
        }
    }
    eprintln!(
        "scalar quotient actual receipt: {policy:?}: {:?}",
        session.execution_receipt()
    );
}
