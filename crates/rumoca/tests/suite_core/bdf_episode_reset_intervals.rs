//! SPEC_0044 accepted-interval sampler contract under BDF: a sampled
//! controller and a state-event episode reset restart the method at order one,
//! so it changes order inside sample intervals. Every accepted interval must
//! still start at the previous accepted point, and the run must not depend on
//! how it is split into advance calls (CanyonRun.Run under `sim bench`).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, SimulationSession};

const SOURCE: &str = r#"
model SampledEpisodes
  Real x(start=0, fixed=true);
  Real y(start=0, fixed=true);
  Real v(start=0, fixed=true);
  discrete Real u(start=1, fixed=true);
  discrete Real ticks(start=0, fixed=true);
  discrete Real episodes(start=0, fixed=true);
  discrete Real lastReset(start=0, fixed=true);
equation
  der(x) = 15;
  der(y) = v;
  der(v) = u - 0.5*v - 4*y;
  when sample(0, 0.05) then
    ticks = pre(ticks) + 1;
    u = if mod(pre(ticks), 7) < 3 then 6 else -6;
  end when;
  when x > 63 then
    episodes = pre(episodes) + 1;
    lastReset = time;
    reinit(x, 0);
    reinit(y, 0);
    reinit(v, 0);
  end when;
end SampledEpisodes;
"#;

const T_END: f64 = 60.0;

fn session() -> SimulationSession {
    let dae = Compiler::new()
        .model("SampledEpisodes")
        .compile_str(SOURCE, "bdf_episode_reset_intervals.mo")
        .unwrap()
        .dae;
    SimulationSession::new(
        &dae,
        SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: T_END,
            dt: Some(0.01),
            rtol: 1e-6,
            atol: 1e-6,
            ..SimOptions::default()
        },
    )
    .unwrap()
}

fn get(session: &SimulationSession, name: &str) -> f64 {
    session.get(name).unwrap().unwrap()
}

/// Advance through `targets`, reporting the first failing call by time.
fn run(targets: impl IntoIterator<Item = f64>) -> SimulationSession {
    let mut session = session();
    for target in targets {
        if let Err(error) = session.advance_to(target) {
            panic!("advance_to({target}) failed: {error}");
        }
    }
    session
}

fn fixed_grid() -> impl Iterator<Item = f64> {
    (1..=6000).map(|step| step as f64 * 0.01)
}

#[test]
fn bdf_episode_resets_keep_every_interval_anchored() {
    // Before the accepted-extension correction to Diffsol this failed with
    // "accepted-interval left endpoint sampler state …" once BDF changed order
    // and step size together.
    let session = run(fixed_grid());
    // Ticks at 0, 0.05, …, 60; x reaches 63 every 4.2 s.
    assert_eq!(get(&session, "ticks"), 1201.0);
    assert_eq!(get(&session, "episodes"), 14.0);
}

#[test]
fn bdf_results_do_not_depend_on_advance_call_sizes() {
    let fine = run(fixed_grid());
    let ragged = run([
        0.003, 0.0101, 0.7, 0.7000001, 4.2, 4.2000001, 9.99, 20.083, 20.1, 33.3, 47.0, T_END,
    ]);
    // Predeclared: the event schedule is exact; the continuous states agree to
    // the integration tolerance, not bit for bit, because the step sequence
    // depends on where each call stops.
    for name in ["ticks", "episodes", "u"] {
        assert_eq!(get(&fine, name), get(&ragged, name), "{name}");
    }
    assert!((get(&fine, "lastReset") - get(&ragged, "lastReset")).abs() <= 1e-6);
    for name in ["x", "y", "v"] {
        let (a, b) = (get(&fine, name), get(&ragged, name));
        assert!(
            (a - b).abs() <= 1e-4 * a.abs().max(1.0),
            "{name}: fixed grid {a}, ragged {b}"
        );
    }
}
