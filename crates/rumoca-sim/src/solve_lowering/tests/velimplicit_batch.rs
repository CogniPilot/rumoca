use super::{SimOptions, SimSolverMode, compile, simulate_dae};

/// The opt-in `velimplicit` solver through the common host: a stiff pair whose
/// fast mode (k h = 100 at the 1 ms default step) would make explicit Euler
/// diverge, plus a time event whose `reinit` exercises truncate/reset.
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
    let result = simulate_dae(
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
