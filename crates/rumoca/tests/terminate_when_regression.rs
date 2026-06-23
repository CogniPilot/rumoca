use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const BALL_WITH_TERMINATE: &str = r#"
model BallTerminate
  Real x(start=10);
  Real v(start=1);
  parameter Real g = 9.81;
equation
  der(x) = v;
  der(v) = -g;
  when x < 0 then
    terminate("Ball has hit the ground");
    reinit(v, -0.8*pre(v));
  end when;
end BallTerminate;
"#;

#[test]
fn terminate_inside_when_stops_at_root_event() {
    let compiled = Compiler::new()
        .model("BallTerminate")
        .compile_str(BALL_WITH_TERMINATE, "ball_terminate.mo")
        .expect("compile BallTerminate");
    let sim = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 10.0,
            dt: Some(0.02),
            ..Default::default()
        },
    )
    .expect("simulate BallTerminate");

    let termination = sim
        .termination
        .as_ref()
        .expect("terminate() inside a when-clause should stop simulation");
    assert_eq!(termination.message, "Ball has hit the ground");
    assert!(
        termination.time > 1.0 && termination.time < 2.0,
        "expected first ground hit near 1.54s, got {}",
        termination.time
    );
    let last_time = *sim.times.last().expect("simulation should record samples");
    assert!(
        last_time < 2.0,
        "simulation should stop at terminate event, not continue to t_end; last time was {last_time}"
    );
}
