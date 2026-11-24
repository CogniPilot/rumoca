//! `delay(u, d)` read by a session stepped on an output grid, as an
//! application drives it (`advance_to` every 0.1 s, explicit RK).
//!
//! A source step at a state event is recorded as a left/right pair. With the
//! output grid the event's left limit and its right limit land a few ulps
//! apart instead of at one time, and the cubic stencil read them as two
//! distinct points: weights of order 1/ulp turned a 0 → 1 step into values of
//! 1e15 one delay later. The delay times are also Integer literals here, which
//! MLS §10.6.13 (Integer → Real for a Real formal) admits. A smooth state's
//! delay, sampled at the integrator's accepted steps, reads back across the
//! event pair.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, SimulationSession};

const SOURCE: &str = r#"
model SteppedDelay
  Real u = if time > 1 then 1 else 0;
  Real s(start = 0, fixed = true);
  Real y = delay(u, 2.0);
  Real yi = delay(u, 2);
  Real ym = delay(u, 2, 3);
  Real ys = delay(s, 2.0);
equation
  der(s) = cos(time);
end SteppedDelay;
"#;

fn stepped(solver_mode: SimSolverMode) -> Vec<(f64, f64, f64, f64, f64)> {
    let dae = Compiler::new()
        .model("SteppedDelay")
        .compile_str(SOURCE, "SteppedDelay.mo")
        .unwrap_or_else(|error| panic!("SteppedDelay compiles: {error:?}"))
        .dae;
    let mut session = SimulationSession::new(
        &dae,
        SimOptions {
            solver_mode,
            t_end: 6.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("SteppedDelay starts: {error}"));
    let get = |session: &SimulationSession, name: &str| session.get(name).unwrap().unwrap();
    (1..=60)
        .map(|step| {
            let time = f64::from(step) * 0.1;
            session
                .advance_to(time)
                .unwrap_or_else(|error| panic!("advance_to({time}): {error}"));
            (
                time,
                get(&session, "y"),
                get(&session, "yi"),
                get(&session, "ym"),
                get(&session, "ys"),
            )
        })
        .collect()
}

fn check(solver_mode: SimSolverMode) {
    for (time, y, yi, ym, ys) in stepped(solver_mode) {
        // the step reaches y at 3 s; at the grid point 3.0 itself either side is right
        let expected = if time < 2.999 {
            0.0
        } else if time > 3.001 {
            1.0
        } else {
            y
        };
        assert!(
            (0.0..=1.0).contains(&y) && (y - expected).abs() < 1e-9,
            "{solver_mode:?}: delay(u, 2.0) at {time} is {y}, expected {expected}"
        );
        assert_eq!(
            y.to_bits(),
            yi.to_bits(),
            "{solver_mode:?}: delay(u, 2) at {time}"
        );
        assert_eq!(
            y.to_bits(),
            ym.to_bits(),
            "{solver_mode:?}: delay(u, 2, 3) at {time}"
        );
        // (the history holds the accepted steps, up to a second long for
        // this easy state, read back cubically: up to 1e-2 off sin under RK,
        // never garbage)
        let smooth = if time <= 2.0 { 0.0 } else { (time - 2.0).sin() };
        assert!(
            (ys - smooth).abs() < 2e-2,
            "{solver_mode:?}: delay(s, 2.0) of s = sin(time) at {time} is {ys}, expected {smooth}"
        );
    }
}

#[test]
fn a_stepped_delay_of_an_event_step_stays_a_step_rk() {
    check(SimSolverMode::RkLike);
}

#[test]
fn a_stepped_delay_of_an_event_step_stays_a_step_bdf() {
    check(SimSolverMode::Bdf);
}
