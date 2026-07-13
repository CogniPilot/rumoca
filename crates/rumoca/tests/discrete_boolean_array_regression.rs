use rumoca_compile::{Session, SessionConfig};
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const BOOLEAN_ARRAY_FANOUT: &str = r#"
model BooleanArrayFanout
  Boolean trigger;
  Boolean fanout[2];
  discrete Integer hits[2](each start = 0, each fixed = true);
  Real x(start = 0, fixed = true);
equation
  der(x) = 0;
  trigger = time >= 0.5;
  fanout = fill(trigger, 2);
  when edge(fanout[1]) then
    hits[1] = pre(hits[1]) + 1;
  end when;
  when edge(fanout[2]) then
    hits[2] = pre(hits[2]) + 1;
  end when;
end BooleanArrayFanout;
"#;

#[test]
fn scalarized_boolean_array_fanout_reaches_both_event_consumers() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("boolean_array_fanout.mo", BOOLEAN_ARRAY_FANOUT)
        .expect("Boolean array fanout fixture should parse");
    let compiled = session
        .compile_model("BooleanArrayFanout")
        .expect("Boolean array fanout fixture should compile");

    let sim = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        },
    )
    .expect("Boolean array fanout fixture should simulate");

    for name in ["hits[1]", "hits[2]"] {
        let index = sim
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("missing trace for {name}: {:?}", sim.names));
        assert_eq!(
            sim.data[index].last().copied(),
            Some(1.0),
            "both scalarized Boolean lanes must settle in the same event"
        );
    }
}
