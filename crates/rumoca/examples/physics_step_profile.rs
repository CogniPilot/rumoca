//! Native profile of an interactive physics step.
//!
//! Mirrors the browser's `WasmSimulationSession.withInteractiveOptions` call
//! (rk-like solver, `dt` as the initial step hint, explicit tolerances, one
//! input batch before every `advance_to`) so wall time and allocation counts
//! per advance can be attributed on the native build with `perf`. The browser
//! advances the physics worker once per sensor event, so `--advance` defaults
//! to the 10 Hz IMU interval of the slam_web scoreboard (two advances per
//! 200 ms camera frame); a smaller advance over-weights per-call overhead.
//!
//! ```text
//! physics_step_profile <model.mo> <ModelName> --source-root <dir> \
//!     [--advance 0.1] [--steps 500] [--warmup 50] [--policy auto|interpreter] \
//!     [--solver rk-like|bdf|auto] [--atol 1e-8] [--rtol 1e-6] [--dt 0.01] \
//!     [--input name=value ...] [--warehouse]
//! ```
//!
//! `--warehouse` replays the slam_web warehouse circuit: the four inputs
//! `forward`, `left`, `up`, `yaw` follow 6 s legs over a 24 s cycle,
//! `autopilot` and `indoorTour` stay 0, and `commandTime` is held at the start
//! of each 0.2 s camera interval, as the physics worker sets them before every
//! `advance_to`.
//!
//! Prints one JSON object with the per-advance wall-time distribution, the
//! allocation count and gross allocated bytes per advance (a realloc counts
//! once at its new size), and the execution receipt.

// SPEC_0021: Exception - a counting `GlobalAlloc` wrapper is the only way to
// attribute allocations per advance, and `GlobalAlloc` is an unsafe trait.
#![allow(unsafe_code)]

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

use rumoca_sim::{SimExecutionPolicy, SimOptions, SimSolverMode, SimulationSession};

struct CountingAllocator;

static ALLOCATIONS: AtomicU64 = AtomicU64::new(0);
static ALLOCATED_BYTES: AtomicU64 = AtomicU64::new(0);

unsafe impl GlobalAlloc for CountingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Ordering::Relaxed);
        ALLOCATED_BYTES.fetch_add(layout.size() as u64, Ordering::Relaxed);
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Ordering::Relaxed);
        ALLOCATED_BYTES.fetch_add(new_size as u64, Ordering::Relaxed);
        unsafe { System.realloc(ptr, layout, new_size) }
    }
}

#[global_allocator]
static GLOBAL: CountingAllocator = CountingAllocator;

struct Args {
    model_file: String,
    model_name: String,
    source_roots: Vec<String>,
    advance: f64,
    steps: usize,
    warmup: usize,
    policy: SimExecutionPolicy,
    solver: SimSolverMode,
    atol: f64,
    rtol: f64,
    dt: f64,
    inputs: Vec<(String, f64)>,
    warehouse: bool,
}

/// The slam_web warehouse circuit input batch before advance number `step`
/// of `advance` seconds each. The schedule is derived from the integer step
/// count, as the browser's sensor clock derives interval starts from an
/// integer sequence, so accumulated floating-point time never lands an
/// interval boundary one advance late.
fn warehouse_inputs(step: u64, advance: f64) -> Vec<(&'static str, f64)> {
    let t = step as f64 * advance;
    let leg = ((t % 24.0) / 6.0).floor() as usize % 4;
    let forward = [0.5, 0.0, -0.5, 0.0][leg];
    let left = [0.0, 0.25, 0.0, -0.25][leg];
    let steps_per_interval = (0.2 / advance).round().max(1.0) as u64;
    let command_time = (step / steps_per_interval * steps_per_interval) as f64 * advance;
    vec![
        ("forward", forward),
        ("left", left),
        ("up", 0.0),
        ("yaw", 0.0),
        ("autopilot", 0.0),
        ("indoorTour", 0.0),
        ("commandTime", command_time),
    ]
}

fn parse_args() -> anyhow::Result<Args> {
    let mut raw = std::env::args().skip(1);
    let model_file = raw
        .next()
        .ok_or_else(|| anyhow::anyhow!("missing <model.mo>"))?;
    let model_name = raw
        .next()
        .ok_or_else(|| anyhow::anyhow!("missing <ModelName>"))?;
    let mut args = Args {
        model_file,
        model_name,
        source_roots: Vec::new(),
        advance: 0.1,
        steps: 500,
        warmup: 50,
        policy: SimExecutionPolicy::Auto,
        solver: SimSolverMode::RkLike,
        atol: 1e-8,
        rtol: 1e-6,
        dt: 0.01,
        inputs: Vec::new(),
        warehouse: false,
    };
    while let Some(flag) = raw.next() {
        if flag == "--warehouse" {
            args.warehouse = true;
            continue;
        }
        let value = raw
            .next()
            .ok_or_else(|| anyhow::anyhow!("{flag} needs a value"))?;
        match flag.as_str() {
            "--source-root" => args.source_roots.push(value),
            "--advance" => args.advance = value.parse()?,
            "--steps" => args.steps = value.parse()?,
            "--warmup" => args.warmup = value.parse()?,
            "--policy" => {
                args.policy = match value.as_str() {
                    "auto" => SimExecutionPolicy::Auto,
                    "interpreter" => SimExecutionPolicy::Interpreter,
                    other => anyhow::bail!("unknown policy `{other}`"),
                }
            }
            "--solver" => {
                args.solver = match value.as_str() {
                    "rk-like" => SimSolverMode::RkLike,
                    "bdf" => SimSolverMode::Bdf,
                    "auto" => SimSolverMode::Auto,
                    other => anyhow::bail!("unknown solver `{other}`"),
                }
            }
            "--atol" => args.atol = value.parse()?,
            "--rtol" => args.rtol = value.parse()?,
            "--dt" => args.dt = value.parse()?,
            "--input" => {
                let (name, number) = value
                    .split_once('=')
                    .ok_or_else(|| anyhow::anyhow!("--input expects name=value"))?;
                args.inputs.push((name.to_owned(), number.parse()?));
            }
            other => anyhow::bail!("unknown flag `{other}`"),
        }
    }
    Ok(args)
}

fn percentile(sorted: &[f64], fraction: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let index = ((sorted.len() - 1) as f64 * fraction).round() as usize;
    sorted[index]
}

/// Per-advance samples of the timed loop.
struct Measurements {
    wall_ms: Vec<f64>,
    allocations: Vec<u64>,
    bytes: Vec<u64>,
    total_seconds: f64,
}

/// Advance the session `count` times from advance number `*step`, recording
/// wall time and allocation deltas when `record` is set.
fn advance(
    session: &mut SimulationSession,
    args: &Args,
    inputs_at: &dyn Fn(u64) -> Vec<(&'static str, f64)>,
    step: &mut u64,
    count: usize,
    record: bool,
) -> anyhow::Result<Measurements> {
    let mut measurements = Measurements {
        wall_ms: Vec::with_capacity(count),
        allocations: Vec::with_capacity(count),
        bytes: Vec::with_capacity(count),
        total_seconds: 0.0,
    };
    let total_start = Instant::now();
    for _ in 0..count {
        let inputs = inputs_at(*step);
        *step += 1;
        let target = (*step as f64) * args.advance;
        let allocations_before = ALLOCATIONS.load(Ordering::Relaxed);
        let bytes_before = ALLOCATED_BYTES.load(Ordering::Relaxed);
        let started = Instant::now();
        session.set_inputs(&inputs)?;
        session.advance_to(target)?;
        if record {
            measurements
                .wall_ms
                .push(started.elapsed().as_secs_f64() * 1e3);
            measurements
                .allocations
                .push(ALLOCATIONS.load(Ordering::Relaxed) - allocations_before);
            measurements
                .bytes
                .push(ALLOCATED_BYTES.load(Ordering::Relaxed) - bytes_before);
        }
    }
    measurements.total_seconds = total_start.elapsed().as_secs_f64();
    Ok(measurements)
}

fn summarize(measurements: &Measurements, simulated_seconds: f64) -> serde_json::Value {
    let mut sorted = measurements.wall_ms.clone();
    sorted.sort_by(|a, b| a.total_cmp(b));
    let count = measurements.wall_ms.len().max(1) as f64;
    serde_json::json!({
        "wall_ms_per_advance": {
            "mean": measurements.wall_ms.iter().sum::<f64>() / count,
            "median": percentile(&sorted, 0.5),
            "p95": percentile(&sorted, 0.95),
            "max": percentile(&sorted, 1.0),
            "min": percentile(&sorted, 0.0),
        },
        "real_time_factor": simulated_seconds / measurements.total_seconds,
        "allocations_per_advance": measurements.allocations.iter().sum::<u64>() as f64 / count,
        "allocated_bytes_per_advance": measurements.bytes.iter().sum::<u64>() as f64 / count,
    })
}

fn main() -> anyhow::Result<()> {
    let args = parse_args()?;

    let compile_start = Instant::now();
    let compiler = rumoca::Compiler::new()
        .model(&args.model_name)
        .source_roots(&args.source_roots);
    let result = compiler.compile_file(&args.model_file)?;
    let compile_seconds = compile_start.elapsed().as_secs_f64();

    let opts = SimOptions {
        solver_mode: args.solver,
        dt: Some(args.dt),
        atol: args.atol,
        rtol: args.rtol,
        initial_inputs: if args.warehouse {
            warehouse_inputs(0, args.advance)
                .into_iter()
                .map(|(name, value)| (name.to_owned(), value))
                .collect()
        } else {
            args.inputs.clone()
        },
        execution_policy: args.policy,
        ..SimOptions::default()
    };
    let prepare_start = Instant::now();
    let mut session = SimulationSession::new(&result.dae, opts)?;
    let prepare_seconds = prepare_start.elapsed().as_secs_f64();
    let input_names = session.input_names().to_vec();

    if args.warehouse && !args.inputs.is_empty() {
        eprintln!("[physics_step_profile] --warehouse replaces the --input values");
    }
    let fixed_inputs: Vec<(&'static str, f64)> = args
        .inputs
        .iter()
        .map(|(name, value)| (Box::leak(name.clone().into_boxed_str()) as &str, *value))
        .collect();
    let inputs_at = |step: u64| -> Vec<(&'static str, f64)> {
        if args.warehouse {
            warehouse_inputs(step, args.advance)
        } else {
            fixed_inputs.clone()
        }
    };

    let mut step: u64 = 0;
    advance(
        &mut session,
        &args,
        &inputs_at,
        &mut step,
        args.warmup,
        false,
    )?;
    let measurements = advance(&mut session, &args, &inputs_at, &mut step, args.steps, true)?;

    let mut report = serde_json::json!({
        "model": args.model_name,
        "model_file": args.model_file,
        "policy": format!("{:?}", args.policy),
        "solver": format!("{:?}", args.solver),
        "atol": args.atol,
        "rtol": args.rtol,
        "dt_hint": args.dt,
        "advance_seconds": args.advance,
        "steps": args.steps,
        "warmup": args.warmup,
        "inputs_set_per_step": inputs_at(0).len(),
        "warehouse_circuit": args.warehouse,
        "input_names": input_names,
        "compile_seconds": compile_seconds,
        "prepare_seconds": prepare_seconds,
        "final_time": session.time(),
        "execution_receipt": format!("{:?}", session.execution_receipt()),
    });
    let summary = summarize(&measurements, args.advance * args.steps as f64);
    for (key, value) in summary.as_object().into_iter().flatten() {
        report[key] = value.clone();
    }
    println!("{}", serde_json::to_string_pretty(&report)?);
    Ok(())
}
