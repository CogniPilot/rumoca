//! Run the `rust-fixed-ode` target's `Simulator` and compare it with
//! `rumoca sim` (fixed-step RK4, which locates every state event) and with
//! closed-form answers.

use rumoca_sim::{
    SimOptions, SimSolverMode, fixed_step_backend_options, simulate_dae_with_diagnostics,
};

use super::*;

pub(super) fn compiled(model: &str, source: &str) -> rumoca::CompilationResult {
    Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("the compiler accepts the event model")
}

/// One `Simulator` run of the generated module.
pub(super) struct Run<'a> {
    pub(super) model: &'a str,
    /// Start values the host writes into `y` (or `p`) before `initialize`.
    pub(super) starts: &'a [(&'a str, f64)],
    pub(super) outputs: &'a [&'a str],
    pub(super) method: &'a str,
    pub(super) location: &'a str,
    pub(super) step: f64,
    pub(super) steps: usize,
    /// Host writes `(before step k, name, value)` between steps.
    pub(super) writes: &'a [(usize, &'a str, f64)],
}

pub(super) fn run(
    model: &'static str,
    outputs: &'static [&'static str],
    location: &'static str,
    step: f64,
    steps: usize,
) -> Run<'static> {
    Run {
        model,
        starts: &[],
        outputs,
        method: "Rk4",
        location,
        step,
        steps,
        writes: &[],
    }
}

/// The stdout of the module compiled with a harness whose `main` builds an
/// RK4 `Simulator` at t = 0 from the defaults and then runs `body`.
pub(super) fn run_body(result: &rumoca::CompilationResult, model: &str, body: &str) -> String {
    run_body_from(result, model, 0.0, body)
}

/// `run_body` with the `Simulator` built at `start`.
pub(super) fn run_body_from(
    result: &rumoca::CompilationResult,
    model: &str,
    start: f64,
    body: &str,
) -> String {
    let (directory, files) = rendered_model_target(result, model, "rust-fixed-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".rs"))
        .expect("fixed Rust ODE target emits a Rust module");
    fs::copy(
        directory.path().join(&module.path),
        directory.path().join("generated.rs"),
    )
    .expect("copy the module beside its harness");
    let harness = format!(
        r#"mod generated;
use generated::*;

fn main() {{
    let mut p = DEFAULT_P;
    init_params(&mut p).unwrap();
    let mut y = [0.0; Y_LEN];
    init_state(&p, &mut y).unwrap();
    let mut sim = Simulator::new(Method::Rk4, EventLocation::Bracketed, EventSettings::default(), {start:?}, y, p);
{body}
}}
"#
    );
    fs::write(directory.path().join("main.rs"), harness).expect("write the harness");
    let executable = directory.path().join("rust-fixed-ode-guards");
    run_checked(
        Command::new("rustc")
            .args(["--edition=2024", "-Dwarnings"])
            .arg(directory.path().join("main.rs"))
            .arg("-o")
            .arg(&executable)
            .current_dir(directory.path()),
        &format!("compile the {model} harness"),
    );
    let output = Command::new(&executable).output().expect("run the harness");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("utf-8 output")
}

/// One printed row: time, events applied by the step, then the outputs.
#[derive(Debug)]
pub(super) struct Row {
    pub(super) time: f64,
    pub(super) events: u32,
    pub(super) values: Vec<f64>,
}

/// The harness program of a run: it prints one row per step, or the
/// `SimError` it stopped with.
fn harness_source(run: &Run<'_>) -> String {
    let starts = run
        .starts
        .iter()
        .map(|(name, value)| format!("    set(&mut y, &mut p, {name:?}, {value:?});\n"))
        .collect::<String>();
    let writes = run
        .writes
        .iter()
        .map(|(at, name, value)| format!("({at}, {name:?}, {value:?}), "))
        .collect::<String>();
    let outputs = run
        .outputs
        .iter()
        .map(|name| format!("{name:?}, "))
        .collect::<String>();
    format!(
        r#"mod generated;
use generated::*;

const OUTPUTS: &[&str] = &[{outputs}];
const WRITES: &[(usize, &str, f64)] = &[{writes}];

#[allow(dead_code)]
fn set_p(sim: &mut Simulator, name: &str, value: f64) {{
    sim.p_mut()[slot(P_SLOTS, name).expect(name)] = value;
}}

#[allow(dead_code)]
fn set(y: &mut State, p: &mut Parameters, name: &str, value: f64) {{
    match slot(Y_SLOTS, name) {{
        Some(index) => y[index] = value,
        None => p[slot(P_SLOTS, name).expect(name)] = value,
    }}
}}

fn print(sim: &Simulator, events: u32) {{
    print!("{{}} {{}}", sim.time(), events);
    for name in OUTPUTS {{
        let value = match slot(Y_SLOTS, name) {{
            Some(index) => sim.y()[index],
            None => sim.p()[slot(P_SLOTS, name).expect(name)],
        }};
        print!(" {{}}", value);
    }}
    println!();
}}

fn main() {{
    let mut p = DEFAULT_P;
    init_params(&mut p).unwrap();
    let mut y = [0.0; Y_LEN];
{starts}    init_state(&p, &mut y).unwrap();
    let mut sim = Simulator::new(Method::{method}, EventLocation::{location}, EventSettings::default(), 0.0, y, p);
    if let Err(error) = sim.initialize() {{
        println!("error {{error:?}}");
        return;
    }}
    print(&sim, 0);
    for k in 0..{steps} {{
        for &(at, name, value) in WRITES {{
            if at == k {{
                set_p(&mut sim, name, value);
            }}
        }}
        match sim.do_step({step:?}) {{
            Ok(events) => print(&sim, events),
            Err(error) => {{
                println!("error {{error:?}}");
                return;
            }}
        }}
    }}
}}
"#,
        method = run.method,
        location = run.location,
        steps = run.steps,
        step = run.step,
    )
}

/// The rows of a run, or the `SimError` it stopped with.
pub(super) fn simulate_generated(
    result: &rumoca::CompilationResult,
    run: &Run<'_>,
) -> Result<Vec<Row>, String> {
    let (directory, files) = rendered_model_target(result, run.model, "rust-fixed-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".rs"))
        .expect("fixed Rust ODE target emits a Rust module");
    fs::copy(
        directory.path().join(&module.path),
        directory.path().join("generated.rs"),
    )
    .expect("copy the module beside its harness");
    fs::write(directory.path().join("main.rs"), harness_source(run)).expect("write the harness");
    let executable = directory.path().join("rust-fixed-ode-events");
    run_checked(
        Command::new("rustc")
            .args(["--edition=2024", "-Dwarnings", "-O"])
            .arg(directory.path().join("main.rs"))
            .arg("-o")
            .arg(&executable)
            .current_dir(directory.path()),
        &format!("compile the {} harness", run.model),
    );
    let output = Command::new(&executable).output().expect("run the harness");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    parse_rows(&String::from_utf8(output.stdout).expect("utf-8 rows"))
}

fn parse_rows(stdout: &str) -> Result<Vec<Row>, String> {
    let mut rows = Vec::new();
    for line in stdout.lines() {
        if let Some(error) = line.strip_prefix("error ") {
            return Err(error.to_string());
        }
        let mut fields = line.split_whitespace();
        let time = fields.next().expect("time").parse().expect("a time");
        let events = fields
            .next()
            .expect("events")
            .parse()
            .expect("an event count");
        let values = fields
            .map(|field| field.parse().expect("a value"))
            .collect();
        rows.push(Row {
            time,
            events,
            values,
        });
    }
    Ok(rows)
}

/// `rumoca sim` with the fixed-step RK4 solver at `step`, sampled at the same
/// grid: the right-limit row at each time.
fn simulate_reference(result: &rumoca::CompilationResult, run: &Run<'_>) -> Vec<Row> {
    let t_end = run.step * run.steps as f64;
    let options = SimOptions {
        solver_mode: SimSolverMode::FixedStep,
        backend_options: Some(fixed_step_backend_options(run.step, 4)),
        t_end,
        dt: Some(run.step),
        ..SimOptions::default()
    };
    let reference =
        simulate_dae_with_diagnostics(&result.dae, &options).expect("rumoca sim runs the model");
    let columns = run
        .outputs
        .iter()
        .map(|name| {
            reference
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap_or_else(|| panic!("rumoca sim records {name}: {:?}", reference.names))
        })
        .collect::<Vec<_>>();
    (0..=run.steps)
        .map(|k| {
            let time = run.step * k as f64;
            // The nearest row; of equally near ones (an event's two limits)
            // the last, its right limit.
            let row = (0..reference.times.len())
                .min_by(|&a, &b| {
                    let (da, db) = (
                        (reference.times[a] - time).abs(),
                        (reference.times[b] - time).abs(),
                    );
                    da.total_cmp(&db).then(b.cmp(&a))
                })
                .filter(|&row| (reference.times[row] - time).abs() <= 1e-9 * (1.0 + time.abs()))
                .unwrap_or_else(|| panic!("rumoca sim has a row at {time}"));
            Row {
                time,
                events: 0,
                values: columns
                    .iter()
                    .map(|&column| reference.data[column][row])
                    .collect(),
            }
        })
        .collect()
}

/// The largest difference per output between two runs on the same grid.
fn worst_difference(rows: &[Row], reference: &[Row]) -> Vec<f64> {
    assert_eq!(rows.len(), reference.len());
    let mut worst = vec![0.0_f64; rows[0].values.len()];
    for (row, expected) in rows.iter().zip(reference) {
        assert!(
            (row.time - expected.time).abs() <= 1e-9,
            "{} vs {}",
            row.time,
            expected.time
        );
        for (k, (value, expected)) in row.values.iter().zip(&expected.values).enumerate() {
            worst[k] = worst[k].max((value - expected).abs());
        }
    }
    worst
}

const BALL: &str = r#"
model Ball
  Real h(start = 1, fixed = true);
  Real v(start = 0, fixed = true);
equation
  der(h) = v;
  der(v) = -9.81;
  when h <= 0 then
    reinit(v, -0.8 * pre(v));
  end when;
end Ball;
"#;

const SAMPLED_PI: &str = r#"
model SampledPI
  parameter Real Ts = 0.1;
  Real x(start = 0, fixed = true);
  discrete Real u(start = 0, fixed = true);
  discrete Real I(start = 0, fixed = true);
equation
  der(x) = -x + u;
  when sample(0, Ts) then
    I = pre(I) + Ts * (1 - x);
    u = 2 * (1 - x) + I;
  end when;
end SampledPI;
"#;

/// The bouncing ball in closed form: height and speed at `t`, and the impact
/// times before it.
fn ball_exact(t: f64) -> (f64, f64, usize) {
    let g = 9.81_f64;
    let (mut start, mut height, mut speed, mut impacts) = (0.0, 1.0, 0.0, 0);
    loop {
        // Time to the floor from `height` going up at `speed`.
        let flight = (speed + (speed * speed + 2.0 * g * height).sqrt()) / g;
        if start + flight > t {
            let s = t - start;
            return (height + speed * s - 0.5 * g * s * s, speed - g * s, impacts);
        }
        start += flight;
        speed = 0.8 * (g * flight - speed);
        height = 0.0;
        impacts += 1;
    }
}

fn ball_run(location: &'static str, step: f64, steps: usize) -> Run<'static> {
    Run {
        starts: &[("h", 1.0), ("v", 0.0)],
        ..run("Ball", &["h", "v"], location, step, steps)
    }
}

/// Bounces in a run: the speed turning from falling to rising.
fn bounces(rows: &[Row]) -> usize {
    rows.windows(2)
        .filter(|pair| pair[0].values[1] < 0.0 && pair[1].values[1] > 0.0)
        .count()
}

/// RK4 and its Hermite interpolant are exact for the ball's quadratic flight,
/// so a bracketed impact is exact to roundoff and so is every later state;
/// a step-end impact is late by up to a step.
#[test]
fn rust_fixed_ode_events_locate_ball_impacts_by_policy() {
    let result = compiled("Ball", BALL);
    let step = 1e-2;
    let steps = 200;
    let error = |location| {
        let rows =
            simulate_generated(&result, &ball_run(location, step, steps)).expect("the ball runs");
        let (h, v, exact_impacts) = ball_exact(2.0);
        assert_eq!(bounces(&rows), exact_impacts, "{location}: impacts");
        let last = rows.last().expect("rows");
        (last.values[0] - h).abs().max((last.values[1] - v).abs())
    };
    let bracketed = error("Bracketed");
    let secant = error("Secant");
    let step_end = error("StepEnd");
    assert!(bracketed < 1e-9, "Bracketed: {bracketed:e}");
    assert!(secant < 1e-3, "Secant: {secant:e}");
    assert!(step_end > 1e-3 && step_end < 0.2, "StepEnd: {step_end:e}");
    assert!(bracketed < secant && secant < step_end);
}

/// A step-end event is first order: halving the step halves its error.
#[test]
fn rust_fixed_ode_events_step_end_error_is_first_order() {
    let result = compiled("Ball", BALL);
    let (h, _, _) = ball_exact(2.0);
    let error = |step: f64| {
        let steps = (2.0 / step).round() as usize;
        let rows =
            simulate_generated(&result, &ball_run("StepEnd", step, steps)).expect("the ball runs");
        (rows.last().expect("rows").values[0] - h).abs()
    };
    let (coarse, fine) = (error(4e-3), error(1e-3));
    assert!(
        fine < coarse / 2.0,
        "StepEnd: {coarse:e} at 4 ms, {fine:e} at 1 ms"
    );
}

/// The bracketed ball matches `rumoca sim` at every grid point.
#[test]
fn rust_fixed_ode_events_ball_matches_rumoca_sim() {
    let result = compiled("Ball", BALL);
    let run = ball_run("Bracketed", 1e-3, 2000);
    let rows = simulate_generated(&result, &run).expect("the ball runs");
    let worst = worst_difference(&rows, &simulate_reference(&result, &run));
    assert!(worst.iter().all(|w| *w < 1e-9), "{worst:?}");
}

/// Hysteresis: `on` and `switches` change at the two thresholds exactly as
/// in `rumoca sim`.
#[test]
fn rust_fixed_ode_events_thermostat_matches_rumoca_sim() {
    let source = r#"
model Thermostat
  Real T(start = 15, fixed = true);
  Boolean on(start = true, fixed = true);
  Integer switches(start = 0, fixed = true);
equation
  der(T) = (if on then 2 else 0) - 0.1 * (T - 10);
algorithm
  when T > 21 then
    on := false;
    switches := pre(switches) + 1;
  elsewhen T < 19 then
    on := true;
    switches := pre(switches) + 1;
  end when;
end Thermostat;
"#;
    let result = compiled("Thermostat", source);
    let run = Run {
        starts: &[("T", 15.0)],
        ..run(
            "Thermostat",
            &["T", "on", "switches"],
            "Bracketed",
            1e-2,
            4000,
        )
    };
    let rows = simulate_generated(&result, &run).expect("the thermostat runs");
    let reference = simulate_reference(&result, &run);
    let worst = worst_difference(&rows, &reference);
    assert!(worst[0] < 1e-6, "T: {worst:?}");
    assert_eq!(worst[1], 0.0, "on: {worst:?}");
    assert_eq!(worst[2], 0.0, "switches: {worst:?}");
    assert!(
        rows.last().expect("rows").values[2] >= 4.0,
        "the thermostat switches"
    );
}

/// A sampled PI controller: every tick of `sample(0, 0.1)` is a time event
/// the step lands on, so all three policies agree with `rumoca sim`.
#[test]
fn rust_fixed_ode_events_sampled_controller_matches_rumoca_sim() {
    let result = compiled("SampledPI", SAMPLED_PI);
    for location in ["StepEnd", "Secant", "Bracketed"] {
        let run = run("SampledPI", &["x", "u", "I"], location, 0.025, 200);
        let rows = simulate_generated(&result, &run).expect("the controller runs");
        let ticks: u32 = rows.iter().map(|row| row.events).sum();
        assert_eq!(ticks, 50, "{location}: ticks after t = 0");
        let worst = worst_difference(&rows, &simulate_reference(&result, &run));
        assert!(worst.iter().all(|w| *w < 1e-9), "{location}: {worst:?}");
    }
}

/// Two indicators crossing in the same step are applied together.
#[test]
fn rust_fixed_ode_events_apply_simultaneous_crossings_together() {
    let source = r#"
model Pair
  Real x(start = 0, fixed = true);
  Integer a(start = 0, fixed = true);
  Integer b(start = 0, fixed = true);
equation
  der(x) = 1;
  when x >= 0.5 then
    a = pre(a) + 1;
  end when;
  when x >= 0.5 + 1e-15 then
    b = pre(b) + 1;
  end when;
end Pair;
"#;
    let result = compiled("Pair", source);
    let run = run("Pair", &["x", "a", "b"], "Bracketed", 0.3, 4);
    let rows = simulate_generated(&result, &run).expect("the pair runs");
    assert_eq!(rows[2].events, 1, "{rows:?}");
    assert_eq!(&rows[2].values[1..], &[1.0, 1.0]);
    let worst = worst_difference(&rows, &simulate_reference(&result, &run));
    assert!(worst.iter().all(|w| *w < 1e-12), "{worst:?}");
}

/// A `reinit` wraps an angle into `[0, 2 pi)` as in `rumoca sim`.
#[test]
fn rust_fixed_ode_events_wrap_an_angle_with_reinit() {
    let source = r#"
model Wrap
  parameter Real w = 1000;
  Real theta(start = 0, fixed = true);
equation
  der(theta) = w;
  when theta >= 2 * 3.141592653589793 then
    reinit(theta, pre(theta) - 2 * 3.141592653589793);
  end when;
end Wrap;
"#;
    let result = compiled("Wrap", source);
    let run = run("Wrap", &["theta"], "Bracketed", 1e-4, 200);
    let rows = simulate_generated(&result, &run).expect("the angle wraps");
    assert_eq!(rows.iter().map(|row| row.events).sum::<u32>(), 3);
    let theta = rows.last().expect("rows").values[0];
    assert!(
        (theta - (20.0 - 6.0 * std::f64::consts::PI)).abs() < 1e-9,
        "theta = {theta}"
    );
    let worst = worst_difference(&rows, &simulate_reference(&result, &run));
    assert!(worst[0] < 1e-9, "{worst:?}");
}

/// A time relation and a timed `reinit`: steps land on both instants, for
/// every method.
#[test]
fn rust_fixed_ode_events_land_on_time_events() {
    let source = r#"
model TimedReset
  Real x(start = 0, fixed = true);
  Real u;
equation
  u = if time >= 1 then 1.0 else 0.0;
  der(x) = u;
  when time >= 2 then
    reinit(x, 0.0);
  end when;
end TimedReset;
"#;
    let result = compiled("TimedReset", source);
    for method in ["Euler", "Heun", "Rk4"] {
        let run = Run {
            method,
            ..run("TimedReset", &["x"], "StepEnd", 0.3, 10)
        };
        let rows = simulate_generated(&result, &run).expect("the model runs");
        assert_eq!(
            rows.iter().map(|row| row.events).sum::<u32>(),
            2,
            "{method}"
        );
        // x = 0 until 1, rises to 1 at 2, resets, and reaches 1 at 3.
        assert!(
            (rows[10].values[0] - 1.0).abs() < 1e-12,
            "{method}: {rows:?}"
        );
    }
}

/// A relay switching on both sides of `x = 0` reaches the surface at t = 1
/// and then slides on it. Located events refine the first crossings until
/// the sliding-mode streak forms, then apply at the step end, as `rumoca sim`
/// does: both hold `x` within one step of the surface.
#[test]
fn rust_fixed_ode_events_slide_a_relay_like_rumoca_sim() {
    let source = r#"
model Relay
  Real x(start = 1, fixed = true);
  discrete Real u(start = -1, fixed = true);
equation
  der(x) = u;
  when x <= 0 then
    u = 1;
  elsewhen x > 0 then
    u = -1;
  end when;
end Relay;
"#;
    let result = compiled("Relay", source);
    let step = 0.01;
    for location in ["StepEnd", "Secant", "Bracketed"] {
        let run = Run {
            starts: &[("x", 1.0)],
            ..run("Relay", &["x"], location, step, 200)
        };
        let rows = simulate_generated(&result, &run).expect("the relay slides");
        let reference = simulate_reference(&result, &run);
        let before = worst_difference(&rows[..100], &reference[..100]);
        assert!(before[0] < 1e-9, "{location}: {before:?}");
        for (row, expected) in rows[100..].iter().zip(&reference[100..]) {
            assert!(row.values[0].abs() <= step + 1e-9, "{location}: {row:?}");
            assert!(
                expected.values[0].abs() <= step + 1e-9,
                "rumoca sim: {expected:?}"
            );
        }
    }
}

/// Two relations that read each other's alias cycle under simultaneous
/// updates at x = 0.5; the mode search pins the first consistent joint mode,
/// the one `rumoca sim` pins.
#[test]
fn rust_fixed_ode_events_resolve_a_relation_cycle_like_rumoca_sim() {
    let source = r#"
model Flip
  Real x(start = 0, fixed = true);
  Boolean b1;
  Boolean b2;
  Real y1;
  Real y2;
equation
  der(x) = 1;
  y1 = if b2 then x - 5 else x;
  y2 = if b1 then x - 5 else x;
  b1 = y1 > 0.5;
  b2 = y2 > 0.5;
end Flip;
"#;
    let result = compiled("Flip", source);
    let run = run("Flip", &["x", "b1", "b2", "y1", "y2"], "Bracketed", 0.25, 8);
    let rows = simulate_generated(&result, &run).expect("the cycle resolves");
    assert_eq!(&rows[3].values[1..3], &[1.0, 0.0], "{rows:?}");
    let worst = worst_difference(&rows, &simulate_reference(&result, &run));
    assert!(worst.iter().all(|w| *w < 1e-9), "{worst:?}");
}

/// A relation cycle with no consistent joint mode stops both executors at
/// the same event.
#[test]
fn rust_fixed_ode_events_refuse_an_inconsistent_cycle_like_rumoca_sim() {
    let source = r#"
model Inverter
  Real x(start = -1, fixed = true);
  Boolean b1;
  Boolean b2;
  Real y1;
  Real y2;
equation
  der(x) = 1;
  y1 = if b2 then x + 1 else x - 1;
  y2 = if b1 then x - 1 else x + 1;
  b1 = y1 > 0.5;
  b2 = y2 > 0.5;
end Inverter;
"#;
    let result = compiled("Inverter", source);
    let run = |steps| Run {
        starts: &[("x", -1.0)],
        ..run("Inverter", &["x"], "Bracketed", 0.25, steps)
    };
    assert!(simulate_generated(&result, &run(1)).is_ok());
    assert_eq!(
        simulate_generated(&result, &run(4)).expect_err("no consistent mode"),
        "EventIteration"
    );
    let reference = |t_end| {
        let options = SimOptions {
            solver_mode: SimSolverMode::FixedStep,
            backend_options: Some(fixed_step_backend_options(0.25, 4)),
            t_end,
            dt: Some(0.25),
            ..SimOptions::default()
        };
        simulate_dae_with_diagnostics(&result.dae, &options)
    };
    assert!(reference(0.25).is_ok());
    assert!(reference(1.0).is_err());
}

/// An error-level assertion stops the run at the step that violates it, as
/// `rumoca sim` stops; a warning-level one never stops it.
#[test]
fn rust_fixed_ode_events_check_assertions_like_rumoca_sim() {
    let guard = |level: &str| {
        format!(
            r#"
model Guard
  Real x(start = 0, fixed = true);
equation
  der(x) = 1;
  assert(x < 2.5, "x left its range"{level});
end Guard;
"#
        )
    };
    let run = |steps| run("Guard", &["x"], "Bracketed", 0.25, steps);
    let error = compiled("Guard", &guard(""));
    assert!(simulate_generated(&error, &run(9)).is_ok());
    assert_eq!(
        simulate_generated(&error, &run(16)).expect_err("x reaches 2.5"),
        "Assertion(0)"
    );
    let reference = |result: &rumoca::CompilationResult, t_end| {
        let options = SimOptions {
            solver_mode: SimSolverMode::FixedStep,
            backend_options: Some(fixed_step_backend_options(0.25, 4)),
            t_end,
            dt: Some(0.25),
            ..SimOptions::default()
        };
        simulate_dae_with_diagnostics(&result.dae, &options)
    };
    assert!(reference(&error, 2.25).is_ok());
    assert!(reference(&error, 4.0).is_err());
    let warning = compiled("Guard", &guard(", AssertionLevel.warning"));
    let rows = simulate_generated(&warning, &run(16)).expect("a warning never stops the run");
    let worst = worst_difference(&rows, &simulate_reference(&warning, &run(16)));
    assert!(worst[0] < 1e-12, "{worst:?}");
}

/// A discrete-valued input changes only at events: each host write of
/// `enable` is an event at the next step's first instant, so `when enable`
/// counts every rising edge and `x` integrates exactly while enabled.
#[test]
fn rust_fixed_ode_events_turn_discrete_input_writes_into_events() {
    let source = r#"
model Counter
  input Boolean enable(start = false);
  Real x(start = 0, fixed = true);
  Integer count(start = 0, fixed = true);
equation
  der(x) = if enable then 1 else 0;
  when enable then
    count = pre(count) + 1;
  end when;
end Counter;
"#;
    let result = compiled("Counter", source);
    let run = Run {
        writes: &[(4, "enable", 1.0), (8, "enable", 0.0), (12, "enable", 1.0)],
        ..run("Counter", &["x", "count"], "Bracketed", 0.25, 16)
    };
    let rows = simulate_generated(&result, &run).expect("the counter runs");
    for (k, row) in rows.iter().enumerate() {
        let enabled_steps = match k {
            0..=4 => 0,
            5..=8 => k - 4,
            9..=12 => 4,
            _ => k - 8,
        };
        let count = match k {
            0..=4 => 0.0,
            5..=12 => 1.0,
            _ => 2.0,
        };
        assert!(
            (row.values[0] - 0.25 * enabled_steps as f64).abs() < 1e-12,
            "{k}: {row:?}"
        );
        assert_eq!(row.values[1], count, "{k}: {row:?}");
    }
    assert!(rows[5].events >= 1 && rows[9].events >= 1 && rows[13].events >= 1);
}

/// A stick-slip state machine (`stuck = pre(stuck) and ...`) reads `pre` at
/// the event entry in every pass. The joint decelerates, sticks while the
/// drive torque stays under the breakaway torque, breaks away, and sticks no
/// more until t = 1.75, all as in `rumoca sim`. (Later, `w` grazes the 1e-3
/// sticking window in microseconds, where the outcome is ill-conditioned and
/// OpenModelica and `rumoca sim` disagree with each other as well.)
#[test]
fn rust_fixed_ode_events_run_a_stick_slip_state_machine_like_rumoca_sim() {
    let source = r#"
model Stiction
  parameter Real J = 0.01;
  parameter Real Fc = 0.3;
  parameter Real Fs = 0.4;
  Real w(start = 2, fixed = true);
  Real tau;
  Boolean stuck(start = false, fixed = true);
equation
  tau = 0.6 * sin(2 * time);
  stuck = pre(stuck) and abs(tau) <= Fs
    or not pre(stuck) and abs(w) < 1e-3 and abs(tau) <= Fs;
  J * der(w) = if stuck then 0 else tau - Fc * sign(w);
end Stiction;
"#;
    let result = compiled("Stiction", source);
    let run = Run {
        starts: &[("w", 2.0)],
        ..run("Stiction", &["w", "stuck"], "Bracketed", 1e-3, 1750)
    };
    let rows = simulate_generated(&result, &run).expect("the joint sticks and slips");
    assert!(
        rows.iter().any(|row| row.values[1] == 1.0),
        "the joint sticks"
    );
    assert_eq!(rows.last().expect("rows").values[1], 0.0, "and breaks away");
    let worst = worst_difference(&rows, &simulate_reference(&result, &run));
    assert!(worst[0] < 1e-9, "w: {worst:?}");
    assert_eq!(worst[1], 0.0, "stuck: {worst:?}");
}

/// A `when` body reads a state's left limit through `pre(x)` while another
/// `when` resets it: `held` accumulates the value `x` had on entry.
#[test]
fn rust_fixed_ode_events_read_left_limits_like_rumoca_sim() {
    let source = r#"
model Capture
  Real x(start = 0, fixed = true);
  discrete Real held(start = 0, fixed = true);
  Integer n(start = 0, fixed = true);
equation
  der(x) = 1;
  when x > 0.5 then
    reinit(x, 0);
  end when;
  when x > 0.5 then
    held = pre(x) + pre(held);
    n = pre(n) + 1;
  end when;
end Capture;
"#;
    let result = compiled("Capture", source);
    let run = run("Capture", &["x", "held", "n"], "Bracketed", 0.25, 8);
    let rows = simulate_generated(&result, &run).expect("the model runs");
    assert_eq!(rows.last().expect("rows").values[2], 3.0);
    assert!((rows.last().expect("rows").values[1] - 1.5).abs() < 1e-12);
    let worst = worst_difference(&rows, &simulate_reference(&result, &run));
    assert!(worst.iter().all(|w| *w < 1e-12), "{worst:?}");
}

/// A model whose only non-state slot is an output still refreshes it: the
/// derivative reads `y = 2 u`, so `x` rises at twice the input.
#[test]
fn rust_fixed_ode_events_refresh_output_only_slots() {
    let source = r#"
model OutputOnly
  input Real u(start = 0);
  Real x(start = 0, fixed = true);
  output Real y;
equation
  y = 2 * u;
  der(x) = y;
end OutputOnly;
"#;
    let result = compiled("OutputOnly", source);
    let run = Run {
        writes: &[(0, "u", 1.0)],
        ..run("OutputOnly", &["x", "y"], "Bracketed", 0.25, 4)
    };
    let rows = simulate_generated(&result, &run).expect("the model runs");
    let last = rows.last().expect("rows");
    assert!((last.values[0] - 2.0).abs() < 1e-12, "{rows:?}");
}

/// An event-free model steps through the same API.
#[test]
fn rust_fixed_ode_events_step_an_event_free_model() {
    let result = checked_decay();
    let run = run("Decay", &[], "Bracketed", 0.01, 100);
    let rows = simulate_generated(&result, &run).expect("the decay runs");
    assert_eq!(rows.len(), 101);
    assert!(rows.iter().all(|row| row.events == 0));
}

/// `initialize` and `do_step` perform no heap allocation, through state
/// events (the ball), clock ticks and pulses (the controller), under every
/// location policy.
#[test]
fn rust_fixed_ode_events_step_without_heap_allocation() {
    for (model, source, starts) in [
        ("Ball", BALL, "y[0] = 1.0;"),
        ("SampledPI", SAMPLED_PI, "y[0] = 0.0;"),
    ] {
        let (directory, files) =
            rendered_model_target(&compiled(model, source), model, "rust-fixed-ode");
        let module = files
            .iter()
            .find(|file| file.path.ends_with(".rs"))
            .expect("fixed Rust ODE target emits a Rust module");
        fs::copy(
            directory.path().join(&module.path),
            directory.path().join("generated.rs"),
        )
        .expect("copy the module beside its harness");
        let harness = format!(
            r#"use std::alloc::{{GlobalAlloc, Layout, System}};
use std::sync::atomic::{{AtomicUsize, Ordering}};

struct CountingAllocator;
static ALLOCATIONS: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for CountingAllocator {{
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {{
        ALLOCATIONS.fetch_add(1, Ordering::SeqCst);
        {unsafe_block} {{ System.alloc(layout) }}
    }}

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {{
        {unsafe_block} {{ System.dealloc(pointer, layout) }}
    }}
}}

#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

mod generated;
use generated::*;

fn main() {{
    for location in [EventLocation::StepEnd, EventLocation::Secant, EventLocation::Bracketed] {{
        let mut p = DEFAULT_P;
        init_params(&mut p).unwrap();
        let mut y = [0.0; Y_LEN];
        {starts}
        let mut sim = Simulator::new(Method::Rk4, location, EventSettings::default(), 0.0, y, p);
        let before = ALLOCATIONS.load(Ordering::SeqCst);
        sim.initialize().unwrap();
        let mut events = 0;
        for _ in 0..150 {{
            events += sim.do_step(0.01).unwrap();
        }}
        let after = ALLOCATIONS.load(Ordering::SeqCst);
        assert!(events >= 4, "{{location:?}}: {{events}} events");
        assert_eq!(before, after, "{{location:?}}: stepping allocated");
    }}
}}
"#,
            unsafe_block = concat!("un", "safe"),
        );
        fs::write(directory.path().join("main.rs"), harness).expect("write the harness");
        let executable = directory.path().join("rust-fixed-ode-no-allocation");
        run_checked(
            Command::new("rustc")
                .args(["--edition=2024", "-Dwarnings"])
                .arg(directory.path().join("main.rs"))
                .arg("-o")
                .arg(&executable)
                .current_dir(directory.path()),
            &format!("compile the {model} allocation harness"),
        );
        run_checked(
            &mut Command::new(executable),
            &format!("step {model} without allocating"),
        );
    }
}
