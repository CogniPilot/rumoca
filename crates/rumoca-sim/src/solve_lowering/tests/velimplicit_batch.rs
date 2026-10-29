use super::{SimOptions, SimSolverMode, compile, simulate_dae_with_diagnostics};

// `simulate_dae` is the BDF prepared simulation and ignores `solver_mode`; the
// dispatching entry below is the one that hosts the requested plugin.

/// The opt-in `velimplicit` solver through the common host: a stiff pair whose
/// fast mode (k h = 100 at the 1 ms default step) would make explicit Euler
/// diverge, plus a time event whose `reinit` restarts the plugin.
#[test]
fn velimplicit_batch_is_stable_on_a_stiff_pair_across_a_reinit() {
    let source = "model StiffPair
                    Real x(start = 1, fixed = true) \"fast state, tracks y\";
                    Real y(start = 0, fixed = true) \"slow state\";
                  equation
                    der(x) = -1.0e5 * (x - y);
                    der(y) = 1 - y;
                    when time >= 0.5 then
                      reinit(y, 0);
                    end when;
                  end StiffPair;";
    let model = compile(source, "StiffPair");
    let result = simulate_dae_with_diagnostics(
        &model,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.01),
            solver_mode: SimSolverMode::VelImplicit,
            ..Default::default()
        },
    )
    .expect("velimplicit batch simulation should succeed");
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .expect("state is recorded");
        &result.data[index]
    };
    let (x, y) = (column("x"), column("y"));
    let last = result.times.len() - 1;
    assert!((result.times[last] - 1.0).abs() < 1.0e-12);
    assert!(x.iter().chain(y.iter()).all(|value| value.is_finite()));
    // After the reinit at t = 0.5, y = 1 - exp(-(t - 0.5)); the first-order
    // fixed step is within a few multiples of h of it.
    let exact = 1.0 - (-0.5_f64).exp();
    assert!((y[last] - exact).abs() < 2.0e-3, "y(1) = {}", y[last]);
    // The stiff state relaxes onto the slow one instead of oscillating.
    assert!((x[last] - y[last]).abs() < 1.0e-3, "x(1) = {}", x[last]);
    let reset = result
        .times
        .iter()
        .position(|time| *time >= 0.5 && *time < 0.5 + 1.0e-9)
        .expect("the event time is recorded");
    assert!(y[reset..].iter().any(|value| value.abs() < 1.0e-9));
}

/// A state event: the host locates `h < 0` on the plugin's continuous
/// extension and applies the root through truncate/reset. A ball dropped from
/// 1 m first lands at sqrt(2/g) with speed sqrt(2 g) and rebounds at e times it.
#[test]
fn velimplicit_batch_locates_a_state_event_and_rebounds() {
    let source = "model Ball
                    parameter Real e = 0.8;
                    Real h(start = 1, fixed = true);
                    Real v(start = 0, fixed = true);
                  equation
                    der(h) = v;
                    der(v) = -9.81;
                    when h < 0 then
                      reinit(v, -e * pre(v));
                    end when;
                  end Ball;";
    let model = compile(source, "Ball");
    let result = simulate_dae_with_diagnostics(
        &model,
        &SimOptions {
            t_end: 0.6,
            dt: Some(0.01),
            solver_mode: SimSolverMode::VelImplicit,
            ..Default::default()
        },
    )
    .expect("velimplicit batch simulation should succeed");
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .expect("state is recorded");
        &result.data[index]
    };
    let (h, v) = (column("h"), column("v"));
    let g = 9.81_f64;
    let impact = (2.0 / g).sqrt();
    let rebound = 0.8 * (2.0 * g).sqrt();
    // The ball never tunnels through the ground by more than a first-order step.
    assert!(
        h.iter().all(|value| *value > -1.0e-2),
        "min h = {:?}",
        h.iter().cloned().fold(f64::INFINITY, f64::min)
    );
    // It moves up again only after the impact.
    let first_up = result
        .times
        .iter()
        .zip(v)
        .position(|(_, velocity)| *velocity > 0.0)
        .expect("the ball rebounds");
    assert!(
        (result.times[first_up] - impact).abs() < 2.0e-2,
        "rebound at {}",
        result.times[first_up]
    );
    let last = result.times.len() - 1;
    let after = result.times[last] - impact;
    let exact_h = rebound * after - 0.5 * g * after * after;
    assert!(
        (h[last] - exact_h).abs() < 2.0e-2,
        "h(0.6) = {}, exact {exact_h}",
        h[last]
    );
}
