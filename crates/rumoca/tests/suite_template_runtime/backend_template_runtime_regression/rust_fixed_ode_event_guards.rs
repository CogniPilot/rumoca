//! What the `rust-fixed-ode` `Simulator` refuses or reports: invalid steps,
//! non-finite derivatives, assertions violated at an event's left limit, and
//! models or values the target cannot run as written.

use super::rust_fixed_ode_events::{Run, compiled, run, run_body, simulate_generated};

/// A negative, NaN or infinite step is refused and leaves the simulator as
/// it was; a zero step is accepted.
#[test]
fn rust_fixed_ode_events_refuse_invalid_steps() {
    let source = r#"
model Clocked
  Real x(start = 0, fixed = true);
  Integer n(start = 0, fixed = true);
equation
  der(x) = 1;
  when sample(0, 0.1) then
    n = pre(n) + 1;
  end when;
end Clocked;
"#;
    let result = compiled("Clocked", source);
    let stdout = run_body(
        &result,
        "Clocked",
        r#"
    sim.initialize().unwrap();
    sim.do_step(0.3).unwrap();
    for h in [-0.2, f64::NAN, f64::INFINITY] {
        println!("{:?}", sim.do_step(h));
    }
    println!("{} {:?}", sim.time(), sim.do_step(0.0));
"#,
    );
    let lines = stdout.lines().collect::<Vec<_>>();
    assert_eq!(
        lines,
        [
            "Err(InvalidStep)",
            "Err(InvalidStep)",
            "Err(InvalidStep)",
            "0.3 Ok(0)"
        ],
        "{stdout}"
    );
}

/// A derivative that turns non-finite stops the run instead of committing NaN
/// states.
#[test]
fn rust_fixed_ode_events_refuse_non_finite_derivatives() {
    let source = r#"
model Drain
  Real x(start = 1, fixed = true);
equation
  der(x) = -sqrt(x);
end Drain;
"#;
    let result = compiled("Drain", source);
    let outcome = simulate_generated(
        &result,
        &Run {
            starts: &[("x", 1.0)],
            ..run("Drain", &["x"], "Bracketed", 0.25, 12)
        },
    );
    assert_eq!(outcome.expect_err("x leaves the domain of sqrt"), "Model");
}

/// An error-level assertion violated just before an event that restores it
/// stops the run, under every location policy and for state and time events.
#[test]
fn rust_fixed_ode_events_check_assertions_at_event_left_limits() {
    let ball = r#"
model FastBall
  Real h(start = 1, fixed = true);
  Real v(start = 0, fixed = true);
equation
  der(h) = v;
  der(v) = -9.81;
  assert(noEvent(v > -4.2), "too fast");
  when h <= 0 then
    reinit(v, -0.8 * pre(v));
  end when;
end FastBall;
"#;
    let result = compiled("FastBall", ball);
    for location in ["StepEnd", "Secant", "Bracketed"] {
        let outcome = simulate_generated(
            &result,
            &Run {
                starts: &[("h", 1.0), ("v", 0.0)],
                ..run("FastBall", &["h", "v"], location, 0.1, 10)
            },
        );
        assert_eq!(
            outcome.expect_err("v passes -4.2 before the bounce"),
            "Assertion(0)",
            "{location}"
        );
    }
    let timed = r#"
model Brake
  Real v(start = 0, fixed = true);
equation
  der(v) = -9.81;
  assert(noEvent(v > -4.414), "too fast");
  when time >= 0.45 then
    reinit(v, 0);
  end when;
end Brake;
"#;
    let result = compiled("Brake", timed);
    let outcome = simulate_generated(&result, &run("Brake", &["v"], "Bracketed", 0.05, 10));
    assert_eq!(
        outcome.expect_err("v is -4.4145 at the left limit of t = 0.45"),
        "Assertion(0)"
    );
}

/// Clock ticks do not count toward the per-step event cap, which bounds only
/// state events.
#[test]
fn rust_fixed_ode_events_step_over_many_clock_ticks() {
    let source = r#"
model Fast
  Real x(start = 0, fixed = true);
  discrete Real u(start = 0, fixed = true);
equation
  der(x) = 1;
  when sample(0, 0.001) then
    u = 1 - pre(u);
  end when;
end Fast;
"#;
    let result = compiled("Fast", source);
    let rows = simulate_generated(&result, &run("Fast", &["x", "u"], "Bracketed", 0.1, 2))
        .expect("a step may span many ticks");
    assert!(rows[1].events >= 100, "{rows:?}");
    assert!((rows[2].values[0] - 0.2).abs() < 1e-12, "{rows:?}");
}

/// After an event the algebraic slots hold their values with no clock
/// ticking, also in a model with no state-event indicator.
#[test]
fn rust_fixed_ode_events_refresh_algebraics_after_events() {
    let source = r#"
model Pulse
  Real x(start = 0, fixed = true);
  Real s;
equation
  der(x) = 1;
  s = x + (if sample(0, 0.1) then 1.0 else 0.0);
end Pulse;
"#;
    let result = compiled("Pulse", source);
    let rows = simulate_generated(&result, &run("Pulse", &["x", "s"], "Bracketed", 0.1, 3))
        .expect("the pulse model runs");
    for row in &rows {
        assert!((row.values[1] - row.values[0]).abs() < 1e-12, "{rows:?}");
    }
}

/// A model that reads `initial()` outside a checked relation region is
/// refused, as the FMI C component refuses it.
#[test]
fn rust_fixed_ode_events_refuse_authored_initial() {
    let source = r#"
model Init
  Real x(start = 0, fixed = true);
  Integer n(start = 0, fixed = true);
equation
  der(x) = 1;
  when initial() then
    n = 5;
  end when;
end Init;
"#;
    let result = compiled("Init", source);
    let error = rumoca::render_target_files(&result, "Init", "rust-fixed-ode", None)
        .expect_err("rust-fixed-ode refuses authored initial()");
    assert!(
        format!("{error:#}").contains("unsupported-feature:events"),
        "{error:#}"
    );
}

/// An assertion owned by a clock is refused: the runtime checks every
/// assertion at every event, not only at its clock's ticks.
#[test]
fn rust_fixed_ode_events_refuse_clocked_assertions() {
    let source = r#"
model ClockedAssert
  Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when sample(0, 0.1) then
    y = x;
    assert(x < 10, "x too large");
  end when;
end ClockedAssert;
"#;
    let result = compiled("ClockedAssert", source);
    let error = rumoca::render_target_files(&result, "ClockedAssert", "rust-fixed-ode", None)
        .expect_err("rust-fixed-ode refuses clocked assertions");
    assert!(
        format!("{error:#}").contains("assertions owned by a clock"),
        "{error:#}"
    );
}

/// `homotopy(actual, simplified)` integrates `actual`: the homotopy parameter
/// starts at 1.
#[test]
fn rust_fixed_ode_events_run_homotopy_actual() {
    let source = r#"
model Hom
  Real x(start = 1, fixed = true);
equation
  der(x) = homotopy(-x, -2 * x);
end Hom;
"#;
    let result = compiled("Hom", source);
    let rows = simulate_generated(
        &result,
        &Run {
            starts: &[("x", 1.0)],
            ..run("Hom", &["x"], "Bracketed", 0.01, 100)
        },
    )
    .expect("the model runs");
    let x = rows.last().expect("rows").values[0];
    assert!((x - (-1.0_f64).exp()).abs() < 1e-8, "x(1) = {x}");
}

/// Every input scalar, array elements included, defaults to NaN so a missing
/// host write is visible.
#[test]
fn rust_fixed_ode_default_inputs_to_nan() {
    let source = r#"
model In
  input Real w;
  input Real u[2];
  Real x(start = 0, fixed = true);
equation
  der(x) = w + u[1] + u[2];
end In;
"#;
    let result = compiled("In", source);
    let stdout = run_body(
        &result,
        "In",
        r#"
    let _ = &mut sim;
    println!("{}", DEFAULT_P.iter().filter(|v| v.is_nan()).count());
"#,
    );
    assert_eq!(stdout.trim(), "3");
}

/// A Boolean start the static evaluator cannot fold keeps no value instead of
/// failing the DAE projection every Solve target shares.
#[test]
fn solve_targets_render_unfoldable_boolean_starts() {
    let source = r#"
model Flag
  parameter String s = "abc";
  parameter Boolean b(start = s == "abc") = true;
  Real x(start = 1, fixed = true);
equation
  der(x) = if b then -x else x;
end Flag;
"#;
    let result = compiled("Flag", source);
    for target in ["c-ode", "rust-fixed-ode"] {
        rumoca::render_target_files(&result, "Flag", target, None)
            .unwrap_or_else(|error| panic!("{target}: {error:#}"));
    }
}

/// A step before `initialize` is refused.
#[test]
fn rust_fixed_ode_events_refuse_steps_before_initialize() {
    let source = r#"
model Plain
  Real x(start = 1, fixed = true);
equation
  der(x) = -x;
end Plain;
"#;
    let result = compiled("Plain", source);
    let stdout = run_body(
        &result,
        "Plain",
        r#"
    println!("{:?}", sim.do_step(0.1));
"#,
    );
    assert_eq!(stdout.trim(), "Err(NotInitialized)");
}

/// A relation mode whose roots are not finite stops the run, as in
/// `rumoca sim`, instead of being skipped by the mode search.
#[test]
fn rust_fixed_ode_events_refuse_non_finite_roots_in_the_mode_search() {
    let source = r#"
model FlipNaN
  Real x(start = 0, fixed = true);
  Boolean b1;
  Boolean b2;
  Real y1;
  Real y2;
  Real w;
equation
  der(x) = 1;
  y1 = if b2 then x - 5 else x;
  y2 = if b1 then x - 5 else x;
  w = if b1 and not b2 then -1 else 1;
  b1 = y1 > 0.5;
  b2 = sqrt(w) * y2 > 0.5;
end FlipNaN;
"#;
    let result = compiled("FlipNaN", source);
    let error = simulate_generated(&result, &run("FlipNaN", &["x"], "Bracketed", 0.1, 10))
        .expect_err("a non-finite root stops the run");
    assert_eq!(error, "Model");
}
