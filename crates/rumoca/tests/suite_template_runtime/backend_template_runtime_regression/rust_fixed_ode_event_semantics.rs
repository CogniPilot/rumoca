//! Event semantics of the `rust-fixed-ode` `Simulator`: `reinit` reads the
//! left limit and applies once per event.

use super::rust_fixed_ode_events::{Run, compiled, run, run_body_from, simulate_generated};

/// A ball run: the host writes the start height and speed.
fn ball_run(
    model: &'static str,
    outputs: &'static [&'static str],
    location: &'static str,
    step: f64,
    steps: usize,
) -> Run<'static> {
    Run {
        starts: &[("h", 1.0), ("v", 0.0)],
        ..run(model, outputs, location, step, steps)
    }
}

fn ball(model: &str, speed: &str) -> String {
    format!(
        r#"
model {model}
  Real h(start = 1, fixed = true);
  Real v(start = 0, fixed = true);
equation
  der(h) = v;
  der(v) = -9.81;
  when h <= 0 then
    reinit(v, -0.8 * {speed});
  end when;
end {model};
"#
    )
}

/// `reinit(v, -0.8*v)` reads `v` at the event's left limit, so it runs as
/// `reinit(v, -0.8*pre(v))` under every location policy (MLS 8.3.6).
#[test]
fn rust_fixed_ode_events_reinit_reads_the_left_limit() {
    let plain = compiled("BallPlain", &ball("BallPlain", "v"));
    let pre = compiled("BallPre", &ball("BallPre", "pre(v)"));
    for location in ["StepEnd", "Secant", "Bracketed"] {
        let rows = simulate_generated(
            &plain,
            &ball_run("BallPlain", &["h", "v"], location, 0.01, 200),
        )
        .unwrap_or_else(|error| panic!("{location}: {error}"));
        let expected =
            simulate_generated(&pre, &ball_run("BallPre", &["h", "v"], location, 0.01, 200))
                .expect("the pre() ball runs");
        assert!(
            rows.iter().map(|row| row.events).sum::<u32>() >= 2,
            "{location}"
        );
        for (row, expected) in rows.iter().zip(&expected) {
            assert_eq!(row.values, expected.values, "{location} at {}", row.time);
        }
    }
}

/// A `when` body beside a `reinit` reads the state's left limit.
#[test]
fn rust_fixed_ode_events_when_bodies_read_the_state_before_its_reinit() {
    let source = r#"
model Beside
  Real h(start = 1, fixed = true);
  Real v(start = 0, fixed = true);
  discrete Real vpre(start = 0, fixed = true);
  discrete Real vnow(start = 0, fixed = true);
equation
  der(h) = v;
  der(v) = -9.81;
  when h < 0 then
    reinit(v, -0.8 * pre(v));
    vpre = pre(v);
    vnow = v;
  end when;
end Beside;
"#;
    let result = compiled("Beside", source);
    let rows = simulate_generated(
        &result,
        &ball_run("Beside", &["vpre", "vnow"], "Bracketed", 0.1, 6),
    )
    .expect("the model runs");
    let last = &rows.last().expect("rows").values;
    assert!(last[0] < -4.0, "{rows:?}");
    assert_eq!(last[0], last[1], "{rows:?}");
}

/// A clocked `reinit` reading its own state applies once per tick.
#[test]
fn rust_fixed_ode_events_apply_a_clocked_reinit_once_per_tick() {
    let source = r#"
model Saw
  Real x(start = 0, fixed = true);
equation
  der(x) = 1;
  when sample(0.5, 0.5) then
    reinit(x, x - 1);
  end when;
end Saw;
"#;
    let result = compiled("Saw", source);
    let rows = simulate_generated(&result, &run("Saw", &["x"], "Bracketed", 0.1, 10))
        .expect("the saw runs");
    // x = 0.5 -> -0.5 at t = 0.5, then 0 -> -1 at t = 1.
    assert_eq!(
        rows.iter().map(|row| row.events).sum::<u32>(),
        2,
        "{rows:?}"
    );
    assert!((rows[10].values[0] + 1.0).abs() < 1e-9, "{rows:?}");
}

/// A `reinit` at a static instant applies once: the right-limit iteration
/// starts again from the event entry.
#[test]
fn rust_fixed_ode_events_apply_a_timed_reinit_once() {
    let source = r#"
model Kick
  Real x(start = 0, fixed = true);
equation
  der(x) = 1;
  when time >= 0.5 then
    reinit(x, 2 * x);
  end when;
end Kick;
"#;
    let result = compiled("Kick", source);
    let rows = simulate_generated(&result, &run("Kick", &["x"], "StepEnd", 0.1, 10))
        .expect("the kick runs");
    // x = 0.5 -> 1 at t = 0.5, then 1.5 at t = 1.
    assert!((rows[10].values[0] - 1.5).abs() < 1e-9, "{rows:?}");
}

/// After a static instant the model reads `time` at its right limit, so
/// every method integrates `time > 1` exactly, also when the grid drifts one
/// ulp below the instant.
#[test]
fn rust_fixed_ode_events_read_time_at_the_right_limit_after_an_instant() {
    let source = r#"
model After
  Real x(start = 0, fixed = true);
  Real u;
equation
  u = if time > 1 then 1.0 else 0.0;
  der(x) = u;
end After;
"#;
    let result = compiled("After", source);
    for method in ["Euler", "Heun", "Rk4"] {
        let mut exact = run("After", &["x"], "StepEnd", 0.25, 8);
        exact.method = method;
        let rows = simulate_generated(&result, &exact).expect("the model runs");
        assert!(
            (rows[8].values[0] - 1.0).abs() < 1e-12,
            "{method}: {rows:?}"
        );
    }
    let source = r#"
model Drift
  Real x(start = 0, fixed = true);
  Real u;
equation
  u = if time > 0.55 then 1.0 else 0.0;
  der(x) = u;
end Drift;
"#;
    let result = compiled("Drift", source);
    let rows = simulate_generated(&result, &run("Drift", &["x", "u"], "StepEnd", 0.05, 20))
        .expect("the model runs");
    assert!((rows[20].values[0] - 0.45).abs() < 1e-12, "{rows:?}");
    assert_eq!(rows[11].values[1], 1.0, "{rows:?}");
}

/// A state event located just before a static instant, within its match
/// tolerance, applies with it at the instant: neither is lost.
#[test]
fn rust_fixed_ode_events_apply_a_state_event_and_a_matching_instant_together() {
    let source = r#"
model Merge
  Real x(start = 0, fixed = true);
  Integer nState(start = 0, fixed = true);
  Integer nTime(start = 0, fixed = true);
equation
  der(x) = 1;
  when x >= 1.999999999999 then
    nState = pre(nState) + 1;
  end when;
  when time >= 2 then
    nTime = pre(nTime) + 1;
  end when;
end Merge;
"#;
    let result = compiled("Merge", source);
    for location in ["StepEnd", "Secant", "Bracketed"] {
        for (step, steps) in [(0.1, 30), (0.25, 12), (0.3, 10)] {
            let rows = simulate_generated(
                &result,
                &run("Merge", &["nState", "nTime"], location, step, steps),
            )
            .expect("the model runs");
            assert_eq!(
                rows[steps].values,
                [1.0, 1.0],
                "{location} {step}: {rows:?}"
            );
        }
    }
}

/// At epoch-scale times a step shorter than the time-match tolerance still
/// integrates.
#[test]
fn rust_fixed_ode_events_integrate_short_steps_at_large_times() {
    let source = r#"
model Late
  Real x(start = 0, fixed = true);
  Integer n(start = 0, fixed = true);
equation
  der(x) = 1;
  when x >= 0.5 then
    n = pre(n) + 1;
  end when;
end Late;
"#;
    let result = compiled("Late", source);
    let stdout = run_body_from(
        &result,
        "Late",
        1.7e9,
        r#"
    sim.initialize().unwrap();
    for _ in 0..10 {
        sim.do_step(1e-3).unwrap();
    }
    println!("{}", sim.y()[0]);
"#,
    );
    let x: f64 = stdout.trim().parse().expect("x");
    assert!((x - 0.01).abs() < 1e-5, "x = {x}");
}
