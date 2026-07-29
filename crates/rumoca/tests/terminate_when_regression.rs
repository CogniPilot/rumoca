use rumoca::Compiler;
use rumoca_ir_dae as dae;
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

const BALL_WITH_PRE_REINIT: &str = r#"
model BallReinit
  Real x(start=1, fixed=true);
  Real v(start=0, fixed=true);
  parameter Real g = 9.81;
equation
  der(x) = v;
  der(v) = -g;
  when x < 0 then
    reinit(v, -pre(v));
  end when;
end BallReinit;
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

#[test]
fn pre_state_in_reinit_reads_the_pre_action_state() {
    let compiled = Compiler::new()
        .model("BallReinit")
        .compile_str(BALL_WITH_PRE_REINIT, "ball_reinit.mo")
        .expect("compile BallReinit");
    let pre_value_nodes = compiled.dae.inspect(|view| {
        (0..view.expression_count())
            .filter_map(|index| {
                let expression = view
                    .expression(view.expression_id(index).expect("dense expression id"))
                    .expect("dense expression resolves");
                (expression.provenance().origin()
                    == dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::PreValueLowering))
                .then(|| view.source_text(expression.provenance()).map(str::to_owned))
            })
            .collect::<Vec<_>>()
    });
    assert_eq!(
        pre_value_nodes,
        vec![Some("pre(v)".to_owned())],
        "the contextual state read keeps typed generated provenance"
    );

    let sim = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.6,
            dt: Some(0.005),
            ..Default::default()
        },
    )
    .expect("simulate BallReinit");
    let velocity = sim
        .names
        .iter()
        .position(|name| name == "v")
        .expect("velocity output");
    let final_velocity = *sim.data[velocity].last().expect("velocity samples");
    assert!(
        final_velocity > 2.0,
        "reinit must reverse the pre-impact velocity, got {final_velocity}"
    );
}
