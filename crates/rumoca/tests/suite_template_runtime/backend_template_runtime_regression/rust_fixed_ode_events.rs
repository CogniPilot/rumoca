//! Execute the fixed Rust ODE target's event API against `rumoca sim`
//! reference values.

use super::*;

fn compiled(model: &str, source: &str) -> rumoca::CompilationResult {
    Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("the compiler accepts the event model")
}

/// A `when ... reinit` wraps a rotor angle into `[0, 2pi)` as a state event
/// at the end of the step that crosses `2pi`; `rumoca sim` gives
/// theta = 1.1504440784612504 at t = 0.02 for w = 1000.
#[test]
fn rust_fixed_ode_events_wrap_an_angle_with_reinit() {
    let source = "model Wrap parameter Real w = 1000; Real theta(start = 0, fixed = true); \
                  equation der(theta) = w; \
                  when theta >= 2 * 3.141592653589793 then \
                  reinit(theta, pre(theta) - 2 * 3.141592653589793); end when; end Wrap;";
    run_fixed_rust_ode_harness(
        &compiled("Wrap", source),
        "Wrap",
        r#"
    assert_eq!(generated::N_ROOTS, 1);
    assert!(generated::TIME_EVENTS.is_empty());
    let theta = generated::slot(generated::Y_SLOTS, "theta").unwrap();
    let mut p = generated::DEFAULT_P;
    generated::init_params(&mut p).unwrap();
    let mut y = [0.0; generated::Y_LEN];
    let mut events = generated::EventState::new();
    assert!(generated::handle_events(&mut events, 0.0, &mut y, &mut p).unwrap());
    let mut t = 0.0;
    let mut wraps = 0;
    for _ in 0..200 {
        let before = y[theta];
        t = generated::step(generated::Method::Rk4, &mut events, t, 1e-4, &mut y, &mut p).unwrap();
        wraps += usize::from(y[theta] < before);
        assert!((0.0..2.0 * std::f64::consts::PI).contains(&y[theta]), "theta = {}", y[theta]);
    }
    assert_eq!(wraps, 3);
    assert!((t - 0.02).abs() < 1e-12);
    assert!((y[theta] - 1.1504440784612504).abs() < 1e-9, "theta = {}", y[theta]);
"#,
        "fixed Rust ODE angle wrap harness",
    );
}

/// A bouncing ball reverses its speed at each floor crossing. Detection at
/// step ends converges on `rumoca sim` (h = 0.26074169044318607,
/// v = -0.1658694813219696 at t = 2) as the step shrinks.
#[test]
fn rust_fixed_ode_events_bounce_a_ball_on_its_state_event() {
    let source = "model Ball Real h(start = 1, fixed = true); Real v(start = 0, fixed = true); \
                  equation der(h) = v; der(v) = -9.81; \
                  when h <= 0 then reinit(v, -0.8 * pre(v)); end when; end Ball;";
    run_fixed_rust_ode_harness(
        &compiled("Ball", source),
        "Ball",
        r#"
    let h = generated::slot(generated::Y_SLOTS, "h").unwrap();
    let v = generated::slot(generated::Y_SLOTS, "v").unwrap();
    let mut p = generated::DEFAULT_P;
    generated::init_params(&mut p).unwrap();
    let mut y = [0.0; generated::Y_LEN];
    y[h] = 1.0;
    let mut events = generated::EventState::new();
    let mut t = 0.0;
    let mut bounces = 0;
    for _ in 0..200_000 {
        let before = y[v];
        t = generated::step(generated::Method::Rk4, &mut events, t, 1e-5, &mut y, &mut p).unwrap();
        bounces += usize::from(before < 0.0 && y[v] > 0.0);
    }
    assert_eq!(bounces, 3);
    assert!((t - 2.0).abs() < 1e-9);
    assert!((y[h] - 0.26074169044318607).abs() < 5e-4, "h = {}", y[h]);
    assert!((y[v] + 0.1658694813219696).abs() < 5e-4, "v = {}", y[v]);
"#,
        "fixed Rust ODE bouncing ball harness",
    );
}

/// `if time >= 0.5` is a time event: `step` lands on it, and the step that
/// ends there reads the relation's left limit, so x(1) = 0.5 as in
/// `rumoca sim`, for both schemes.
#[test]
fn rust_fixed_ode_events_land_steps_on_time_events() {
    let source = "model Step Real x(start = 0, fixed = true); Real u; \
                  equation u = if time >= 0.5 then 1 else 0; der(x) = u; end Step;";
    run_fixed_rust_ode_harness(
        &compiled("Step", source),
        "Step",
        r#"
    assert_eq!(generated::TIME_EVENTS, &[0.5]);
    assert_eq!(generated::N_ROOTS, 0);
    let x = generated::slot(generated::Y_SLOTS, "x").unwrap();
    for method in [generated::Method::Euler, generated::Method::Rk4] {
        let mut p = generated::DEFAULT_P;
        generated::init_params(&mut p).unwrap();
        let mut y = [0.0; generated::Y_LEN];
        let mut events = generated::EventState::new();
        let mut t = 0.0;
        let mut times = Vec::new();
        while t < 1.0 - 1e-12 {
            t = generated::step(method, &mut events, t, (1.0 - t).min(0.3), &mut y, &mut p).unwrap();
            times.push(t);
        }
        assert_eq!(times[1], 0.5, "{method:?} lands on the event: {times:?}");
        assert!((y[x] - 0.5).abs() < 1e-12, "{method:?}: x = {}", y[x]);
    }
"#,
        "fixed Rust ODE time event harness",
    );
}

/// A `when` on a time relation reinitializes a state at its instant; the
/// discrete rows read only `time`, and the module still compiles warning-free.
#[test]
fn rust_fixed_ode_events_reinit_at_a_time_event() {
    let source = "model TimedReset Real x(start = 0, fixed = true); Real u; \
                  equation u = if time >= 1 then 1.0 else 0.0; der(x) = u; \
                  when time >= 2 then reinit(x, 0.0); end when; end TimedReset;";
    run_fixed_rust_ode_harness(
        &compiled("TimedReset", source),
        "TimedReset",
        r#"
    assert_eq!(generated::TIME_EVENTS, &[1.0, 2.0]);
    let x = generated::slot(generated::Y_SLOTS, "x").unwrap();
    for method in [generated::Method::Euler, generated::Method::Rk4] {
        let mut p = generated::DEFAULT_P;
        generated::init_params(&mut p).unwrap();
        let mut y = [0.0; generated::Y_LEN];
        let mut events = generated::EventState::new();
        let mut t = 0.0;
        let mut reset = false;
        while t < 3.0 - 1e-12 {
            let previous = y[x];
            t = generated::step(method, &mut events, t, (3.0 - t).min(0.3), &mut y, &mut p).unwrap();
            if t == 2.0 {
                reset = true;
                assert!((previous - 0.9).abs() < 1e-12, "{method:?}: x before the reset = {previous}");
                assert_eq!(y[x], 0.0, "{method:?} resets x at t = 2");
            }
        }
        assert!(reset, "{method:?} lands on t = 2");
        assert!((y[x] - 1.0).abs() < 1e-12, "{method:?}: x(3) = {}", y[x]);
    }
"#,
        "fixed Rust ODE timed reinit harness",
    );
}

/// An event-free model exposes the same API: no events, and `step`
/// integrates it.
#[test]
fn rust_fixed_ode_events_step_an_event_free_model() {
    run_fixed_rust_ode_harness(
        &checked_decay(),
        "Decay",
        r#"
    assert!(generated::TIME_EVENTS.is_empty());
    assert_eq!(generated::N_ROOTS, 0);
    let mut p = generated::DEFAULT_P;
    let mut y = [1.0; generated::Y_LEN];
    let mut events = generated::EventState::new();
    assert!(!generated::handle_events(&mut events, 0.0, &mut y, &mut p).unwrap());
    let mut t = 0.0;
    for _ in 0..100 {
        t = generated::step(generated::Method::Rk4, &mut events, t, 0.01, &mut y, &mut p).unwrap();
    }
    assert!((y[0] - (-2.0f64).exp()).abs() < 1e-9, "x = {}", y[0]);
"#,
        "fixed Rust ODE event-free step harness",
    );
}

/// Assertions are event actions the target does not execute, so it refuses
/// the model before writing source.
#[test]
fn rust_fixed_ode_events_refuse_assertions() {
    let source = "model Guard Real x(start = 0, fixed = true); \
                  equation der(x) = 1; assert(x < 10, \"x left its range\"); end Guard;";
    let error =
        rumoca::render_target_files(&compiled("Guard", source), "Guard", "rust-fixed-ode", None)
            .expect_err("rust-fixed-ode refuses assertions");
    assert!(
        format!("{error:?}").contains("unsupported-feature:events.actions"),
        "{error:?}"
    );
}
