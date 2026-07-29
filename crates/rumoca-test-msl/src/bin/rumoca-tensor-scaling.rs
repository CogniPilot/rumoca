//! Full-pipeline compile-scaling ratchet for regular array models.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::fs::File;
use std::io::BufWriter;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use anyhow::{Context, Result, bail};
use clap::Parser;
use rumoca_compile::compile::{CompilationResult, Session, SessionConfig, VariableRole};
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
    timing_passed: bool,
    structural_integrity_passed: bool,
    spec_0032_compact_storage: bool,
    passed: bool,
    structural_failures: Vec<String>,
    measurements: Vec<Measurement>,
}

#[derive(Clone, Debug, Default, Serialize)]
struct Measurement {
    size: usize,
    median_ms: f64,
    samples_ms: Vec<f64>,
    states: usize,
    algebraics: usize,
    equations: usize,
    structured_families: usize,
    compact_domain_points: usize,
    row_major_families: usize,
    binder_substitution_families: usize,
    non_materialized_families: usize,
    dae_scalar_residual_view_available: bool,
    solve_map_nodes: usize,
    solve_affine_stencil_nodes: usize,
}

#[derive(Clone, Copy, Debug)]
struct CompileInventory {
    states: usize,
    algebraics: usize,
    equations: usize,
    structured_families: usize,
    compact_domain_points: usize,
    row_major_families: usize,
    binder_substitution_families: usize,
    non_materialized_families: usize,
    dae_scalar_residual_view_available: bool,
    solve_map_nodes: usize,
    solve_affine_stencil_nodes: usize,
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
    let inventory = inventory(&result)?;
    let elapsed = started.elapsed();
    Ok((elapsed, inventory))
}

fn inventory(result: &CompilationResult) -> Result<CompileInventory> {
    let (
        states,
        algebraics,
        equations,
        structured_families,
        compact_domain_points,
        row_major_families,
        binder_substitution_families,
    ) = result.dae.inspect(|view| {
        let states = view
            .variables()
            .filter(|(_, variable)| variable.role() == VariableRole::State)
            .count();
        let algebraics = view
            .variables()
            .filter(|(_, variable)| variable.role() == VariableRole::Algebraic)
            .count();
        let mut compact_domain_points = 0usize;
        let mut row_major_families = 0usize;
        let mut binder_substitution_families = 0usize;
        for index in 0..view.continuous_family_count() {
            let family = view
                .continuous_family(index)
                .expect("finalized continuous family resolves");
            compact_domain_points = compact_domain_points
                .checked_add(
                    view.domain(family.domain())
                        .expect("branded family domain resolves")
                        .scalar_count() as usize,
                )
                .expect("checked family domain total fits usize");
            match family.scalar_view() {
                rumoca_compile::compile::core::ComprehensionScalarView::RowMajorProjection => {
                    row_major_families += 1;
                }
                rumoca_compile::compile::core::ComprehensionScalarView::BinderSubstitution => {
                    binder_substitution_families += 1;
                }
            }
        }
        (
            states,
            algebraics,
            view.continuous_equation_count(),
            view.continuous_family_count(),
            compact_domain_points,
            row_major_families,
            binder_substitution_families,
        )
    });
    let non_materialized_families = structured_families;
    let dae_scalar_residual_view_available = structured_families == 0;
    let solve = rumoca_sim::lower_solve_problem(&result.dae)
        .context("tensor workload failed Solve-IR lowering")?;
    let solve_counts = solve.compute_node_counts();
    Ok(CompileInventory {
        states,
        algebraics,
        equations,
        structured_families,
        compact_domain_points,
        row_major_families,
        binder_substitution_families,
        non_materialized_families,
        dae_scalar_residual_view_available,
        solve_map_nodes: solve_counts.map,
        solve_affine_stencil_nodes: solve_counts.affine_stencil,
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
            row_major_families: 0,
            binder_substitution_families: 0,
            non_materialized_families: 0,
            dae_scalar_residual_view_available: false,
            solve_map_nodes: 0,
            solve_affine_stencil_nodes: 0,
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
        timing_passed: false,
        structural_integrity_passed: false,
        spec_0032_compact_storage: false,
        passed: false,
        structural_failures: Vec::new(),
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
    measurement.row_major_families = inventory.row_major_families;
    measurement.binder_substitution_families = inventory.binder_substitution_families;
    measurement.non_materialized_families = inventory.non_materialized_families;
    measurement.dae_scalar_residual_view_available = inventory.dae_scalar_residual_view_available;
    measurement.solve_map_nodes = inventory.solve_map_nodes;
    measurement.solve_affine_stencil_nodes = inventory.solve_affine_stencil_nodes;
}

struct StructuralAssessment {
    integrity_passed: bool,
    spec_0032_compact_storage: bool,
    failures: Vec<String>,
}

fn assess_structure(workload: Workload, measurements: &[Measurement]) -> StructuralAssessment {
    let mut failures = Vec::new();
    let mut compact_storage = true;
    for measurement in measurements {
        let size = measurement.size;
        match workload {
            Workload::WholeArrayFirstOrder => {
                require_structure(
                    measurement.equations == 1,
                    &mut failures,
                    format!("N={size}: expected one aggregate DAE equation"),
                );
                require_structure(
                    measurement.structured_families == 1
                        && measurement.compact_domain_points == size
                        && measurement.row_major_families == 1,
                    &mut failures,
                    format!(
                        "N={size}: expected one row-major compact family covering exactly N points"
                    ),
                );
                require_structure(
                    measurement.dae_scalar_residual_view_available,
                    &mut failures,
                    format!("N={size}: aggregate DAE equation must be directly consumable"),
                );
                require_structure(
                    measurement.solve_map_nodes >= 1,
                    &mut failures,
                    format!("N={size}: expected a native Solve Map node"),
                );
                compact_storage &= measurement.equations == 1;
            }
            Workload::CascadedFirstOrder => {
                let expected_points = size.saturating_sub(1);
                require_structure(
                    measurement.structured_families == 1
                        && measurement.compact_domain_points == expected_points
                        && measurement.binder_substitution_families == 1
                        && measurement.non_materialized_families == 1,
                    &mut failures,
                    format!(
                        "N={size}: expected one non-materialized binder-substitution family \
                         covering N-1 points"
                    ),
                );
                require_structure(
                    !measurement.dae_scalar_residual_view_available,
                    &mut failures,
                    format!(
                        "N={size}: placeholder scalar residual view must fail loudly at DAE codegen"
                    ),
                );
                require_structure(
                    measurement.solve_affine_stencil_nodes >= 1,
                    &mut failures,
                    format!("N={size}: expected a native Solve AffineStencil node"),
                );
                // One boundary row plus one compact family owner is the largest
                // O(1) DAE equation inventory this workload may claim. The current
                // pipeline stores N placeholder rows, so this intentionally reports
                // and gates the remaining SPEC_0032 violation.
                compact_storage &= measurement.equations <= 2;
            }
        }
    }
    StructuralAssessment {
        integrity_passed: failures.is_empty(),
        spec_0032_compact_storage: compact_storage,
        failures,
    }
}

fn require_structure(condition: bool, failures: &mut Vec<String>, message: String) {
    if !condition {
        failures.push(message);
    }
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
        "{}: exponent {:.3} (limit {:.3}), structural_integrity={}, \
         spec_0032_compact_storage={}",
        report.name,
        report.exponent,
        max_exponent,
        report.structural_integrity_passed,
        report.spec_0032_compact_storage
    );
    for measurement in &report.measurements {
        println!(
            "  N={:<5} median={:>9.3} ms equations={} families={} domain_points={} \
             row_major={} binder_substitution={} non_materialized={} dae_scalar_view={} \
             solve_maps={} solve_stencils={}",
            measurement.size,
            measurement.median_ms,
            measurement.equations,
            measurement.structured_families,
            measurement.compact_domain_points,
            measurement.row_major_families,
            measurement.binder_substitution_families,
            measurement.non_materialized_families,
            measurement.dae_scalar_residual_view_available,
            measurement.solve_map_nodes,
            measurement.solve_affine_stencil_nodes,
        );
    }
    for failure in &report.structural_failures {
        println!("  structural failure: {failure}");
    }
    if !report.spec_0032_compact_storage {
        println!(
            "  SPEC_0032 failure: DAE equation storage scales with domain cardinality \
             instead of retaining only the compact owner"
        );
    }
}

fn run(mut args: Args) -> Result<()> {
    validate_args(&mut args)?;
    let mut workloads = Vec::with_capacity(Workload::ALL.len());
    for workload in Workload::ALL {
        let mut report = measure_workload(workload, &args.sizes, args.repetitions)?;
        let assessment = assess_structure(workload, &report.measurements);
        report.timing_passed = report.exponent <= args.max_exponent;
        report.structural_integrity_passed = assessment.integrity_passed;
        report.spec_0032_compact_storage = assessment.spec_0032_compact_storage;
        report.structural_failures = assessment.failures;
        report.passed = report.timing_passed
            && report.structural_integrity_passed
            && report.spec_0032_compact_storage;
        print_workload(&report, args.max_exponent);
        workloads.push(report);
    }
    let passed = workloads.iter().all(|workload| workload.passed);
    let report = Report {
        schema_version: 2,
        repetitions: args.repetitions,
        max_exponent: args.max_exponent,
        passed,
        workloads,
    };
    write_report(&args.output, &report)?;
    println!("Report: {}", args.output.display());
    if args.enforce && !passed {
        bail!(
            "tensor compile-scaling or structural-ownership ratchet failed \
             (timing limit {:.3}); inspect the report for workload invariants",
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

    fn cascaded_measurement(equations: usize) -> Measurement {
        Measurement {
            size: 128,
            equations,
            structured_families: 1,
            compact_domain_points: 127,
            binder_substitution_families: 1,
            non_materialized_families: 1,
            dae_scalar_residual_view_available: false,
            solve_affine_stencil_nodes: 1,
            ..Measurement::default()
        }
    }

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

    #[test]
    fn scalarized_whole_array_cannot_pass_structural_ratchet() {
        let measurement = Measurement {
            size: 128,
            equations: 128,
            dae_scalar_residual_view_available: true,
            ..Measurement::default()
        };

        let assessment = assess_structure(Workload::WholeArrayFirstOrder, &[measurement]);

        assert!(!assessment.integrity_passed);
        assert!(!assessment.spec_0032_compact_storage);
    }

    #[test]
    fn cascaded_placeholder_rows_are_reported_as_spec_violation() {
        let assessment =
            assess_structure(Workload::CascadedFirstOrder, &[cascaded_measurement(128)]);

        assert!(assessment.integrity_passed);
        assert!(!assessment.spec_0032_compact_storage);
    }

    #[test]
    fn compact_cascaded_owner_can_satisfy_structural_ratchet() {
        let assessment = assess_structure(Workload::CascadedFirstOrder, &[cascaded_measurement(2)]);

        assert!(assessment.integrity_passed);
        assert!(assessment.spec_0032_compact_storage);
    }
}
