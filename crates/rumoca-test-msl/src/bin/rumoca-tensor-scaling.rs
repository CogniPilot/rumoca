//! Full-pipeline compile-scaling ratchet for regular array models.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::fs::File;
use std::io::BufWriter;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use anyhow::{Context, Result, bail};
use clap::Parser;
use rumoca_compile::compile::{CompilationResult, Session, SessionConfig};
use serde::Serialize;

const DEFAULT_SIZES: &[usize] = &[128, 512, 2048];
const DEFAULT_MAX_EXPONENT: f64 = 1.25;

#[derive(Debug, Parser)]
#[command(name = "rumoca-tensor-scaling")]
#[command(about = "Measure and ratchet full-pipeline array compile scaling")]
struct Args {
    /// Array cardinalities to measure, from smallest to largest.
    #[arg(
        long,
        value_delimiter = ',',
        default_values_t = DEFAULT_SIZES.iter().copied()
    )]
    sizes: Vec<usize>,

    /// Timed fresh-session compilations at each cardinality.
    #[arg(long, default_value_t = 3)]
    repetitions: usize,

    /// Largest allowed log-log compile-time slope.
    #[arg(long, default_value_t = DEFAULT_MAX_EXPONENT)]
    max_exponent: f64,

    /// Fail when a workload exceeds the scaling exponent ratchet.
    #[arg(long)]
    enforce: bool,

    /// Machine-readable benchmark report.
    #[arg(long, default_value = "target/tensor-scaling/report.json")]
    output: PathBuf,
}

#[derive(Clone, Copy, Debug)]
enum Workload {
    WholeArrayFirstOrder,
    CascadedFirstOrder,
}

impl Workload {
    const ALL: [Self; 2] = [Self::WholeArrayFirstOrder, Self::CascadedFirstOrder];

    const fn name(self) -> &'static str {
        match self {
            Self::WholeArrayFirstOrder => "whole-array-first-order",
            Self::CascadedFirstOrder => "cascaded-first-order",
        }
    }

    const fn model_name(self) -> &'static str {
        match self {
            Self::WholeArrayFirstOrder => "WholeArrayFirstOrder",
            Self::CascadedFirstOrder => "CascadedFirstOrder",
        }
    }

    fn source(self, size: usize) -> String {
        match self {
            Self::WholeArrayFirstOrder => whole_array_source(size),
            Self::CascadedFirstOrder => cascaded_source(size),
        }
    }
}

#[derive(Debug, Serialize)]
struct Report {
    schema_version: u16,
    repetitions: usize,
    max_exponent: f64,
    passed: bool,
    workloads: Vec<WorkloadReport>,
}

#[derive(Debug, Serialize)]
struct WorkloadReport {
    name: &'static str,
    exponent: f64,
    passed: bool,
    measurements: Vec<Measurement>,
}

#[derive(Clone, Debug, Serialize)]
struct Measurement {
    size: usize,
    median_ms: f64,
    samples_ms: Vec<f64>,
    states: usize,
    algebraics: usize,
    equations: usize,
    structured_families: usize,
    compact_domain_points: usize,
}

#[derive(Clone, Copy, Debug)]
struct CompileInventory {
    states: usize,
    algebraics: usize,
    equations: usize,
    structured_families: usize,
    compact_domain_points: usize,
}

fn whole_array_source(size: usize) -> String {
    format!(
        "model WholeArrayFirstOrder\n\
         \x20 constant Integer N = {size};\n\
         \x20 Real x[N](each start = 1.0);\n\
         equation\n\
         \x20 der(x) = -x;\n\
         end WholeArrayFirstOrder;\n"
    )
}

fn cascaded_source(size: usize) -> String {
    format!(
        "model CascadedFirstOrder\n\
         \x20 constant Integer N = {size};\n\
         \x20 Real x[N](each start = 1.0);\n\
         equation\n\
         \x20 der(x[1]) = 1.0 - x[1];\n\
         \x20 for i in 2:N loop\n\
         \x20   der(x[i]) = x[i - 1] - x[i];\n\
         \x20 end for;\n\
         end CascadedFirstOrder;\n"
    )
}

fn compile_once(workload: Workload, size: usize) -> Result<(Duration, CompileInventory)> {
    let source = workload.source(size);
    let mut session = Session::new(SessionConfig::default());
    let started = Instant::now();
    session
        .add_document("tensor-scaling.mo", &source)
        .with_context(|| format!("failed to parse {} at N={size}", workload.name()))?;
    let result = session
        .compile_model(workload.model_name())
        .with_context(|| format!("failed to compile {} at N={size}", workload.name()))?;
    let elapsed = started.elapsed();
    Ok((elapsed, inventory(&result)?))
}

fn inventory(result: &CompilationResult) -> Result<CompileInventory> {
    let families = &result.dae.continuous.structured_equations;
    let mut compact_domain_points = 0usize;
    for family in families {
        let points = family
            .domain
            .scalar_count()
            .context("invalid compact structured-family domain")?;
        compact_domain_points = compact_domain_points
            .checked_add(points)
            .context("compact structured-family point count overflowed")?;
    }
    Ok(CompileInventory {
        states: result.dae.variables.states.len(),
        algebraics: result.dae.variables.algebraics.len(),
        equations: result.dae.continuous.equations.len(),
        structured_families: families.len(),
        compact_domain_points,
    })
}

fn measure_workload(
    workload: Workload,
    sizes: &[usize],
    repetitions: usize,
) -> Result<WorkloadReport> {
    let _ = compile_once(workload, sizes[0])?;
    let mut measurements = sizes
        .iter()
        .map(|size| Measurement {
            size: *size,
            median_ms: 0.0,
            samples_ms: Vec::with_capacity(repetitions),
            states: 0,
            algebraics: 0,
            equations: 0,
            structured_families: 0,
            compact_domain_points: 0,
        })
        .collect::<Vec<_>>();
    collect_samples(workload, &mut measurements, repetitions)?;
    for measurement in &mut measurements {
        measurement.samples_ms.sort_by(f64::total_cmp);
        measurement.median_ms = median(&measurement.samples_ms);
    }
    let exponent = scaling_exponent(&measurements)?;
    Ok(WorkloadReport {
        name: workload.name(),
        exponent,
        passed: false,
        measurements,
    })
}

fn collect_samples(
    workload: Workload,
    measurements: &mut [Measurement],
    repetitions: usize,
) -> Result<()> {
    for repetition in 0..repetitions {
        if repetition % 2 == 0 {
            collect_sample_range(workload, measurements, 0..measurements.len())?;
        } else {
            collect_sample_range(workload, measurements, (0..measurements.len()).rev())?;
        }
    }
    Ok(())
}

fn collect_sample_range(
    workload: Workload,
    measurements: &mut [Measurement],
    indices: impl Iterator<Item = usize>,
) -> Result<()> {
    for index in indices {
        let measurement = &mut measurements[index];
        let (elapsed, inventory) = compile_once(workload, measurement.size)?;
        record_sample(measurement, elapsed, inventory);
    }
    Ok(())
}

fn record_sample(measurement: &mut Measurement, elapsed: Duration, inventory: CompileInventory) {
    measurement.samples_ms.push(elapsed.as_secs_f64() * 1_000.0);
    measurement.states = inventory.states;
    measurement.algebraics = inventory.algebraics;
    measurement.equations = inventory.equations;
    measurement.structured_families = inventory.structured_families;
    measurement.compact_domain_points = inventory.compact_domain_points;
}

fn median(sorted: &[f64]) -> f64 {
    let middle = sorted.len() / 2;
    if sorted.len().is_multiple_of(2) {
        (sorted[middle - 1] + sorted[middle]) / 2.0
    } else {
        sorted[middle]
    }
}

fn scaling_exponent(measurements: &[Measurement]) -> Result<f64> {
    let samples = measurements
        .iter()
        .map(|measurement| (measurement.size as f64, measurement.median_ms))
        .collect::<Vec<_>>();
    log_log_slope(&samples)
}

fn log_log_slope(samples: &[(f64, f64)]) -> Result<f64> {
    if samples.len() < 2 {
        bail!("scaling exponent requires at least two measurements");
    }
    let log_samples = samples
        .iter()
        .map(|(size, elapsed)| (size.ln(), elapsed.max(f64::MIN_POSITIVE).ln()))
        .collect::<Vec<_>>();
    let count = log_samples.len() as f64;
    let mean_x = log_samples.iter().map(|(x, _)| x).sum::<f64>() / count;
    let mean_y = log_samples.iter().map(|(_, y)| y).sum::<f64>() / count;
    let covariance = log_samples
        .iter()
        .map(|(x, y)| (x - mean_x) * (y - mean_y))
        .sum::<f64>();
    let variance = log_samples
        .iter()
        .map(|(x, _)| (x - mean_x).powi(2))
        .sum::<f64>();
    if variance <= f64::EPSILON {
        bail!("scaling sizes must not all be equal");
    }
    Ok(covariance / variance)
}

fn validate_args(args: &mut Args) -> Result<()> {
    args.sizes.sort_unstable();
    args.sizes.dedup();
    if args.sizes.len() < 2 || args.sizes.contains(&0) {
        bail!("--sizes requires at least two distinct positive cardinalities");
    }
    if args.repetitions == 0 {
        bail!("--repetitions must be positive");
    }
    if !args.max_exponent.is_finite() || args.max_exponent < 0.0 {
        bail!("--max-exponent must be a finite non-negative number");
    }
    Ok(())
}

fn write_report(path: &Path, report: &Report) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    let file =
        File::create(path).with_context(|| format!("failed to create {}", path.display()))?;
    serde_json::to_writer_pretty(BufWriter::new(file), report)
        .with_context(|| format!("failed to write {}", path.display()))
}

fn print_workload(report: &WorkloadReport, max_exponent: f64) {
    println!(
        "{}: exponent {:.3} (limit {:.3})",
        report.name, report.exponent, max_exponent
    );
    for measurement in &report.measurements {
        println!(
            "  N={:<5} median={:>9.3} ms equations={} families={} domain_points={}",
            measurement.size,
            measurement.median_ms,
            measurement.equations,
            measurement.structured_families,
            measurement.compact_domain_points
        );
    }
}

fn run(mut args: Args) -> Result<()> {
    validate_args(&mut args)?;
    let mut workloads = Vec::with_capacity(Workload::ALL.len());
    for workload in Workload::ALL {
        let mut report = measure_workload(workload, &args.sizes, args.repetitions)?;
        report.passed = report.exponent <= args.max_exponent;
        print_workload(&report, args.max_exponent);
        workloads.push(report);
    }
    let passed = workloads.iter().all(|workload| workload.passed);
    let report = Report {
        schema_version: 1,
        repetitions: args.repetitions,
        max_exponent: args.max_exponent,
        passed,
        workloads,
    };
    write_report(&args.output, &report)?;
    println!("Report: {}", args.output.display());
    if args.enforce && !passed {
        bail!(
            "tensor compile scaling exceeded the {:.3} exponent ratchet",
            args.max_exponent
        );
    }
    Ok(())
}

fn main() -> Result<()> {
    run(Args::parse())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn log_log_slope_recovers_linear_scaling() {
        let slope = log_log_slope(&[(10.0, 2.0), (100.0, 20.0), (1_000.0, 200.0)]).unwrap();
        assert!((slope - 1.0).abs() < 1.0e-12);
    }

    #[test]
    fn log_log_slope_recovers_constant_scaling() {
        let slope = log_log_slope(&[(10.0, 4.0), (100.0, 4.0), (1_000.0, 4.0)]).unwrap();
        assert!(slope.abs() < 1.0e-12);
    }
}
