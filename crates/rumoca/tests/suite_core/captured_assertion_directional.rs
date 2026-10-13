//! Public-path probe for the OneTank captured-message directional refusal.
//! MLS 11 preserves ordered branches; MLS 8.3.7 keeps assertion messages at
//! their failed source point. SPEC_0036 requires the checked owner to retain
//! both rather than scalarizing an ordered function conditional.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, lower_dae_for_simulation, simulate_dae};

fn model(assertion: &str, initial: f64) -> String {
    format!(
        r#"
model CapturedAssertionDirectional
  function f
    input Real u;
    output Real y;
  algorithm
    {assertion}
    if u > 1 then
      y := u*u;
    else
      y := u;
    end if;
  end f;
  Real x(start={initial}, fixed=true);
  Real y;
equation
  der(x) = -1;
  y = f(x);
end CapturedAssertionDirectional;
"#,
    )
}

fn assert_checked_directional_and_trace(assertion: &str) {
    // Exercise both executed branch bodies without changing the function.
    for initial in [0.5, 2.0] {
        let compiled = Compiler::new()
            .model("CapturedAssertionDirectional")
            .compile_str(
                &model(assertion, initial),
                "CapturedAssertionDirectional.mo",
            )
            .expect("public compiler accepts the ordered pure function");
        let options = SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 0.02,
            dt: Some(0.005),
            ..SimOptions::default()
        };
        let lowered = lower_dae_for_simulation(&compiled.dae, &options)
            .expect("Y-dependent ordered call must retain its checked directional owner");
        assert!(
            !lowered.pure_calls.owners().is_empty(),
            "checked call owner"
        );
        assert!(
            lowered
                .pure_calls
                .owners()
                .iter()
                .all(|owner| owner.directional().is_some()),
            "every issued finite call owner has its checked directional relation"
        );
        let trace = simulate_dae(&compiled.dae, &options)
            .expect("captured diagnostics must not prevent a valid continuous trace");
        let settled = trace
            .times
            .iter()
            .rposition(|time| *time == options.t_start)
            .expect("settled exact initial time");
        let columns: Vec<_> = ["x", "y"]
            .iter()
            .map(|name| {
                trace
                    .names
                    .iter()
                    .position(|actual| actual.as_str() == *name)
                    .unwrap_or_else(|| panic!("missing physical observable {name}"))
            })
            .collect();
        assert_eq!(trace.times.last(), Some(&options.t_end));
        assert!(trace.times.iter().all(|time| time.is_finite()));
        assert!(trace.times.windows(2).all(|pair| pair[0] <= pair[1]));
        for (sample, &time) in trace.times.iter().enumerate().skip(settled) {
            let x = initial - time;
            let y = if x > 1.0 { x * x } else { x };
            for (&column, expected) in columns.iter().zip([x, y]) {
                let actual = trace.data[column][sample];
                assert!(
                    time.is_finite()
                        && actual.is_finite()
                        && (actual - expected).abs() <= 1e-7 * (1.0 + expected.abs()),
                    "{} at {time}: {actual}, expected {expected}",
                    trace.names[column]
                );
            }
        }
    }
}

#[test]
fn captured_assertion_message_preserves_ordered_call_directional_and_trace() {
    assert_checked_directional_and_trace(r#"assert(u >= 0, "u=" + String(u));"#);
}

#[test]
fn literal_assertion_control_preserves_ordered_call_directional_and_trace() {
    assert_checked_directional_and_trace(r#"assert(u >= 0, "u must be nonnegative");"#);
}

#[test]
fn no_assertion_control_preserves_ordered_call_directional_and_trace() {
    assert_checked_directional_and_trace("");
}
