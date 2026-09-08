//! RDD2 simulation hot-loop performance non-regression guard (ledger #32).
//!
//! The RDD2 vehicle stack is the repo's realistic end-to-end workload, and its
//! hot-loop cost is the number every Solve/codegen change is judged against.
//! Until now that number lived only in agent transcripts, so a regression could
//! only be noticed by someone who happened to re-run the bench. This test turns
//! it into a machine-checked gate over the canonical command:
//!
//! ```text
//! rumoca sim bench <root>/Vehicles/package.mo \
//!   --model Vehicles.Rdd2.Test.WaypointMission --source-root <root> \
//!   --t-end 0.5 --dt 0.005 --solver rk-like --iterations 5 --warmups 2 --json
//! ```
//!
//! **Release, not debug.** `hot_average_seconds` is meaningless across
//! profiles: the debug binary is roughly an order of magnitude slower, so a
//! debug measurement would either always fail or force a uselessly loose
//! baseline. The guard therefore drives `target/release/rumoca` explicitly
//! rather than `CARGO_BIN_EXE_rumoca` (which is the *test's* profile, i.e.
//! debug under a plain `cargo test`), and fails with build instructions when
//! that binary is absent. Nothing here is timed inside the test process, so it
//! does not matter that the test harness itself is a debug build.
//!
//! **Why it is feature-gated.** It costs seconds of wall clock, needs a release
//! build, and needs the out-of-tree `modelica_models` library, so it cannot run
//! in a default `cargo test`. A Cargo feature is how this repo gates heavy
//! suites; `architecture_hardening_test` bans the attribute-based alternative
//! outright. Run it deliberately:
//!
//! ```text
//! cargo build --release -p rumoca
//! cargo test -p rumoca --features rdd2-metric-gates --test suite_gates -- rdd2_perf_guard
//! ```
//!
//! Run it from the same shell you built in: the release binary is spawned as a
//! subprocess and inherits this process's dynamic-linker environment, so a
//! stripped-down `LD_LIBRARY_PATH` surfaces as a loader error in the assertion
//! that reports the subprocess's stderr.
//!
//! **Two falsified designs, and what actually works.** This gate's shape was
//! not chosen on principle; it is what survived measurement.
//!
//! *Attempt 1 -- single run, mean, 25 % tolerance.* The claim was that 25 % was
//! wide enough to ride out a busy machine. Tested with all 32 cores saturated:
//! +77.1 %, +73.7 %, +68.8 % over baseline. Three hard failures, no regression
//! present. Concurrent agent fan-outs are this box's *normal* condition, so
//! that design is a false-positive generator, and a tolerance wide enough to
//! absorb the interference (~+100 %) would no longer catch anything worth
//! catching.
//!
//! *Attempt 2 -- minimum of >= 3 invocations.* The reasoning was that
//! contention is one-sided (it can only make a sample slower), so a minimum
//! should discard it. That reasoning is sound for *scheduling* noise and wrong
//! for this workload. Re-tested under the same saturation: +67.3 % and +67.6 %.
//! Saturation does not just add a slow tail here -- it raises the floor,
//! because the bench competes for memory bandwidth and cache rather than merely
//! for a timeslice. Every sample is inflated, so the minimum is inflated too.
//!
//! *What ships.* The estimator is still the minimum across `invocations`
//! (>= 3) runs of each invocation's `hot_best_seconds` -- that part is free and
//! does strip scheduling jitter. The actual fix is that the gate now measures
//! its own operating conditions: it samples `/proc/stat` across its measurement
//! window and asserts ONLY when the machine was less than
//! `max_cpu_busy_fraction` busy. Above that threshold it prints every number
//! and asserts nothing, saying so loudly. The result is a gate that cannot
//! produce a false failure from contention while still asserting a tight 25 %
//! bound everywhere the wall-clock number carries information.
//!
//! The threshold is the measured knee, which is sharp and sits close to fully
//! saturated. Inflation of the gated statistic against machine load:
//!
//! ```text
//!   busy 0.098 -> baseline      busy 0.751 -> - 7.4 %   (enforced, passed)
//!   busy 0.539 -> + 2.7 %       busy 0.915 -> +15.6 %
//!   busy 0.667 -> +10.6 %       busy 0.997 -> +59.7 %   (not enforced)
//!                               busy 0.999 -> +68.4 %   (not enforced)
//! ```
//!
//! Everything below ~0.92 stays well inside the 25 % tolerance -- at 0.751 the
//! measurement was not inflated at all. Only near-total saturation blows out.
//!
//! An earlier revision set this to 0.25 on the *assumption* that the
//! uncharacterised middle was dangerous. It is not, and 0.25 was actively
//! harmful: the gate's own bench load contributes roughly 11 points of busy to
//! its own window, so on a normal box it enforced in 0 of 8 trials even with a
//! 10x regression injected -- it never enforced where it lives. A gate that
//! declines to fire is not conservative, it is decorative.
//!
//! The honest cost: on a near-saturated box this degrades to reporting-only,
//! so it is not a substitute for a dedicated perf machine. It is a tripwire for
//! the developer who runs it, not a wall.
//!
//! Every sample is printed on every run, pass, fail, or unenforced. A single
//! number cannot be sanity checked, and the spread plus the busy fraction are
//! what let a reader tell a regression from a pathological machine.
//!
//! **Ratchet.** `hot_best_seconds` in the baseline JSON is a ceiling that may
//! be LOWERED whenever the workload gets faster, and must never be RAISED
//! without a recorded decision saying why the regression is accepted.
//!
//! **NOT WIRED INTO CI -- this is a manual-invocation gate.** No workflow
//! passes `--features rdd2-metric-gates`, so nothing runs this on a push, and
//! saying otherwise would oversell it. That is deliberate rather than pending:
//! the baseline is a wall-clock number calibrated to one specific machine, and
//! GitHub's shared runners have different, unannounced, and varying hardware,
//! so the same baseline there would measure the runner rather than the
//! compiler. Wiring it up needs a dedicated, pinned, non-shared machine and a
//! baseline recorded on that machine; until such a runner exists, run it by
//! hand before and after work that touches Solve lowering or codegen.

use std::fs;
use std::path::Path;
use std::process::Command;

// Corpus/binary resolution remains shared with any future authenticated RDD2
// artifact gates so they cannot disagree about what they are measuring.
use super::rdd2_gate_environment::{
    baseline_f64, baseline_path, baseline_str, corpus_provenance, read_baseline, release_binary,
    require_model_library,
};

/// One bench invocation's timings, in seconds.
struct BenchSample {
    average: f64,
    best: f64,
}

/// Run the canonical bench and return its parsed `--json` report.
fn run_bench(baseline: &serde_json::Value, source_root: &Path) -> serde_json::Value {
    let package = source_root.join(baseline_str(baseline, "package_relative_path"));
    assert!(
        package.is_file(),
        "model library at {} has no {} -- is this really a modelica_models checkout?",
        source_root.display(),
        baseline_str(baseline, "package_relative_path")
    );
    let extra_args: Vec<&str> = baseline["bench_args"]
        .as_array()
        .expect("baseline key `bench_args` must be an array")
        .iter()
        .map(|arg| {
            arg.as_str()
                .expect("every `bench_args` entry must be a string")
        })
        .collect();

    let output = Command::new(release_binary())
        .arg("sim")
        .arg("bench")
        .arg(&package)
        .arg("--model")
        .arg(baseline_str(baseline, "model"))
        .arg("--source-root")
        .arg(source_root)
        .args(&extra_args)
        .arg("--json")
        .output()
        .unwrap_or_else(|error| panic!("spawn `rumoca sim bench`: {error}"));

    assert!(
        output.status.success(),
        "`rumoca sim bench` failed ({}):\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    serde_json::from_str(&stdout)
        .unwrap_or_else(|error| panic!("parse bench --json report: {error}\nstdout was:\n{stdout}"))
}

/// Collect `invocations` independent bench runs.
fn collect_samples(
    baseline: &serde_json::Value,
    source_root: &Path,
    invocations: usize,
) -> Vec<BenchSample> {
    (0..invocations)
        .map(|_| {
            let report = run_bench(baseline, source_root);
            BenchSample {
                average: report["hot_average_seconds"]
                    .as_f64()
                    .expect("bench report must carry `hot_average_seconds`"),
                best: report["hot_best_seconds"]
                    .as_f64()
                    .expect("bench report must carry `hot_best_seconds`"),
            }
        })
        .collect()
}

/// Smallest value in a non-empty slice.
fn minimum(values: impl Iterator<Item = f64>) -> f64 {
    values.fold(f64::INFINITY, f64::min)
}

/// Cumulative (idle_jiffies, total_jiffies) across all CPUs, from the aggregate
/// `cpu` line of `/proc/stat`. `None` when that file is unavailable (non-Linux).
fn cpu_totals() -> Option<(u64, u64)> {
    let stat = fs::read_to_string("/proc/stat").ok()?;
    let line = stat.lines().next()?;
    let fields: Vec<u64> = line
        .split_whitespace()
        .skip(1)
        .filter_map(|field| field.parse().ok())
        .collect();
    // user nice system idle iowait irq softirq steal ...
    let idle = fields.get(3)? + fields.get(4).copied().unwrap_or(0);
    let total: u64 = fields.iter().sum();
    (total > 0).then_some((idle, total))
}

/// Fraction of all CPU capacity that was busy between two [`cpu_totals`]
/// samples, i.e. how contended the machine was while the bench ran.
///
/// This is measured over the gate's own measurement window rather than read
/// from `/proc/loadavg`, because load average is a decaying one-minute mean: it
/// still read 6.0 three seconds after 32 spinners were launched, which is
/// exactly the moment a perf gate must not trust it.
fn busy_fraction(before: (u64, u64), after: (u64, u64)) -> Option<f64> {
    let idle_delta = after.0.checked_sub(before.0)?;
    let total_delta = after.1.checked_sub(before.1)?;
    (total_delta > 0).then(|| 1.0 - (idle_delta as f64 / total_delta as f64))
}

#[test]
fn test_rdd2_waypoint_mission_hot_loop_stays_within_the_perf_baseline() {
    let baseline = read_baseline();
    let source_root = require_model_library(&baseline);

    let invocations = baseline["invocations"]
        .as_u64()
        .expect("baseline key `invocations` must be an integer") as usize;
    assert!(
        invocations >= 3,
        "`invocations` must be at least 3: the gate's schedule-robustness rests on taking a \
         minimum across independent runs, and fewer than 3 does not earn that claim"
    );
    let cpu_before = cpu_totals();
    let samples = collect_samples(&baseline, &source_root, invocations);
    let busy = cpu_before
        .zip(cpu_totals())
        .and_then(|(before, after)| busy_fraction(before, after));

    // The reported statistic is the minimum `hot_best_seconds` across
    // invocations, i.e. the fastest of `invocations * iterations` timed runs.
    // A minimum strips the one-sided tail that a mean absorbs -- but see the
    // enforcement decision below, because on this workload it does NOT by
    // itself survive a saturated machine.
    let measured = minimum(samples.iter().map(|sample| sample.best));
    let recorded = baseline_f64(&baseline, "hot_best_seconds");
    let tolerance = baseline_f64(&baseline, "tolerance_fraction");
    let ceiling = recorded * (1.0 + tolerance);
    let delta_percent = (measured / recorded - 1.0) * 100.0;

    // Report every sample, always: a single printed number cannot be sanity
    // checked, and when this does fail the reader needs the spread to tell a
    // real regression from a machine that was busy.
    println!("{}", corpus_provenance(&baseline, &source_root));
    println!("RDD2 hot-loop bench (release), {invocations} invocations:");
    for (index, sample) in samples.iter().enumerate() {
        println!(
            "  run {}: average {:.4} s, best {:.4} s",
            index + 1,
            sample.average,
            sample.best
        );
    }
    println!(
        "  reported statistic (min best) {measured:.4} s vs baseline {recorded:.4} s \
         ({delta_percent:+.1} %), ceiling {ceiling:.4} s at +{:.0} %",
        tolerance * 100.0
    );

    // Enforcement decision. A wall-clock ceiling only means something when the
    // machine had capacity to spare; see the module docs for the measurements
    // that forced this design. Rather than guess, the gate measures how busy
    // the machine actually was over its own sampling window and enforces only
    // below the recorded threshold. Above it, everything is still reported --
    // the numbers stay useful to a human -- but nothing is asserted, because a
    // failure there would carry no information about the compiler.
    let max_busy = baseline_f64(&baseline, "max_cpu_busy_fraction");
    let Some(busy) = busy else {
        println!(
            "  NOT ENFORCED: could not read /proc/stat, so machine contention is unknown. \
             The numbers above are reported for a human; no assertion was made."
        );
        return;
    };
    println!(
        "  machine was {:.1} % busy during measurement (enforcement threshold {:.0} %)",
        busy * 100.0,
        max_busy * 100.0
    );
    if busy > max_busy {
        println!(
            "  NOT ENFORCED: the machine was too contended for a wall-clock ceiling to mean \
             anything. This is a report, not a pass -- re-run on a quiet machine to actually \
             gate the number."
        );
        return;
    }

    let sample_lines = samples
        .iter()
        .enumerate()
        .map(|(index, sample)| {
            format!(
                "    run {}: average {:.4} s, best {:.4} s",
                index + 1,
                sample.average,
                sample.best
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        measured <= ceiling,
        "RDD2 hot-loop performance regressed: the FASTEST of {invocations} independent bench \
         invocations ran {measured:.4} s, baseline = {recorded:.4} s, allowed ceiling = \
         {ceiling:.4} s (baseline +{:.0} %), i.e. {delta_percent:+.1} % vs baseline.\n\
         Samples:\n{sample_lines}\n\
         The machine was only {busy_percent:.1} % busy while this ran, under the \
         {max_busy_percent:.0} % threshold at which this gate refuses to assert, and the \
         statistic is the FASTEST of {invocations} runs -- so this is not machine noise. \
         Treat it as a real regression (an accidental per-step allocation, a lost fast path, \
         a re-introduced interpretive fallback).\n\
         Model {model} via {package}.\n\
         If the workload legitimately got more expensive, raising `hot_best_seconds` in \
         {baseline_path} needs a recorded decision -- the number is a ratchet: lower it \
         freely when things get faster, never raise it silently.",
        tolerance * 100.0,
        busy_percent = busy * 100.0,
        max_busy_percent = max_busy * 100.0,
        model = baseline_str(&baseline, "model"),
        package = source_root
            .join(baseline_str(&baseline, "package_relative_path"))
            .display(),
        baseline_path = baseline_path().display(),
    );
}
