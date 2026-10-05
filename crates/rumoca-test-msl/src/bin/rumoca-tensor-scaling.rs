//! Full-pipeline compile-scaling ratchet for regular array models.

#[global_allocator]
static GLOBAL: rumoca_allocator::ProcessAllocator = rumoca_allocator::ProcessAllocator;

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
    GridTwoBodyInterior,
    InitialGridFamily,
}

impl Workload {
    const ALL: [Self; 4] = [
        Self::WholeArrayFirstOrder,
        Self::CascadedFirstOrder,
        Self::GridTwoBodyInterior,
        Self::InitialGridFamily,
    ];

    const fn name(self) -> &'static str {
        match self {
            Self::WholeArrayFirstOrder => "whole-array-first-order",
            Self::CascadedFirstOrder => "cascaded-first-order",
            Self::GridTwoBodyInterior => "grid-two-body-interior",
            Self::InitialGridFamily => "initial-grid-family",
        }
    }

    const fn model_name(self) -> &'static str {
        match self {
            Self::WholeArrayFirstOrder => "WholeArrayFirstOrder",
            Self::CascadedFirstOrder => "CascadedFirstOrder",
            Self::GridTwoBodyInterior => "GridTwoBodyInterior",
            Self::InitialGridFamily => "InitialGridFamily",
        }
    }

    fn source(self, size: usize) -> String {
        match self {
            Self::WholeArrayFirstOrder => whole_array_source(size),
            Self::CascadedFirstOrder => cascaded_source(size),
            Self::GridTwoBodyInterior => grid_source(grid_side(size)),
            Self::InitialGridFamily => initial_grid_source(grid_side(size)),
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
    native_kernels: usize,
    native_compiled_rows: usize,
    initial_map_nodes: usize,
    initial_native_kernels: usize,
    initial_native_rows: usize,
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
    native_kernels: usize,
    native_compiled_rows: usize,
    initial_map_nodes: usize,
    initial_native_kernels: usize,
    initial_native_rows: usize,
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

/// The side of the square grid whose cell count is closest to `size`.
fn grid_side(size: usize) -> usize {
    ((size as f64).sqrt().round() as usize).max(3)
}

/// A clamped 2-D method-of-lines grid whose interior nest holds two bodies:
/// eight edge families and two interior families over the whole nest.
fn grid_source(side: usize) -> String {
    format!(
        "model GridTwoBodyInterior\n\
         \x20 constant Integer N = {side};\n\
         \x20 Real u[N, N](each start = 1.0);\n\
         \x20 Real w[N, N](each start = 0.0);\n\
         equation\n\
         \x20 for i in 1:N loop\n\
         \x20   der(u[i, 1]) = 0.0;\n\
         \x20   der(u[i, N]) = 0.0;\n\
         \x20   der(w[i, 1]) = 0.0;\n\
         \x20   der(w[i, N]) = 0.0;\n\
         \x20 end for;\n\
         \x20 for j in 2:N - 1 loop\n\
         \x20   der(u[1, j]) = 0.0;\n\
         \x20   der(u[N, j]) = 0.0;\n\
         \x20   der(w[1, j]) = 0.0;\n\
         \x20   der(w[N, j]) = 0.0;\n\
         \x20 end for;\n\
         \x20 for i in 2:N - 1 loop\n\
         \x20   for j in 2:N - 1 loop\n\
         \x20     der(u[i, j]) = w[i, j];\n\
         \x20     der(w[i, j]) = u[i + 1, j] + u[i - 1, j] + u[i, j + 1] + u[i, j - 1]\n\
         \x20       - 4.0 * u[i, j];\n\
         \x20   end for;\n\
         \x20 end for;\n\
         end GridTwoBodyInterior;\n"
    )
}

/// A 2-D state grid whose start values come from a structured initial-equation
/// nest over cell coordinates, as a PDE model seeds its geometry.
fn initial_grid_source(side: usize) -> String {
    format!(
        "model InitialGridFamily\n\
         \x20 constant Integer N = {side};\n\
         \x20 parameter Real a = 0.3;\n\
         \x20 Real s[N, N];\n\
         initial equation\n\
         \x20 for i in 1:N loop\n\
         \x20   for j in 1:N loop\n\
         \x20     s[i, j] = ((i - 0.5) * a - 1.0) * cos(a) - (j - 0.5) * a * sin(a);\n\
         \x20   end for;\n\
         \x20 end for;\n\
         equation\n\
         \x20 der(s) = -s;\n\
         end InitialGridFamily;\n"
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
                rumoca_core::ComprehensionScalarView::RowMajorProjection => {
                    row_major_families += 1;
                }
                rumoca_core::ComprehensionScalarView::BinderSubstitution => {
                    binder_substitution_families += 1;
                }
                rumoca_core::ComprehensionScalarView::BinderPrefixProjection { .. } => {
                    row_major_families += 1;
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
    // The native derivative block: compact nodes run as loop kernels, so the
    // number of rows compiled one by one must not grow with the domain.
    let native = rumoca_sim::native_compute_inventory(&solve.continuous.derivative_rhs)
        .map_err(anyhow::Error::msg)
        .context("tensor workload failed native compilation")?;
    // The initialization residual and its Jacobian, compiled natively: rows
    // compiled one by one for either count against both.
    let artifacts = rumoca_sim::lower_solve_artifacts(&solve)
        .context("tensor workload failed Solve artifact lowering")?;
    let (initial_residual, initial_jacobian) =
        rumoca_sim::native_initialization_inventory(&solve, &artifacts)
            .map_err(anyhow::Error::msg)
            .context("tensor workload failed native initialization compilation")?;
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
        native_kernels: native.kernels,
        native_compiled_rows: native.compiled_rows,
        initial_map_nodes: solve.initialization.residual().compute_node_counts().map,
        initial_native_kernels: initial_residual.kernels + initial_jacobian.kernels,
        initial_native_rows: initial_residual.compiled_rows + initial_jacobian.compiled_rows,
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
            native_kernels: 0,
            native_compiled_rows: 0,
            initial_map_nodes: 0,
            initial_native_kernels: 0,
            initial_native_rows: 0,
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
    measurement.native_kernels = inventory.native_kernels;
    measurement.native_compiled_rows = inventory.native_compiled_rows;
    measurement.initial_map_nodes = inventory.initial_map_nodes;
    measurement.initial_native_kernels = inventory.initial_native_kernels;
    measurement.initial_native_rows = inventory.initial_native_rows;
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
                    measurement.equations == 0,
                    &mut failures,
                    format!(
                        "N={size}: compact DAE family must have no parallel scalar equation owner"
                    ),
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
                    !measurement.dae_scalar_residual_view_available,
                    &mut failures,
                    format!(
                        "N={size}: authoritative DAE family must require an explicit scalar view"
                    ),
                );
                require_structure(
                    measurement.solve_map_nodes >= 1,
                    &mut failures,
                    format!("N={size}: expected a native Solve Map node"),
                );
                require_native_kernels(measurement, 1, 0, &mut failures);
                compact_storage &= measurement.equations == 0;
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
                require_native_kernels(measurement, 1, 1, &mut failures);
                require_structure(
                    measurement.equations == 1,
                    &mut failures,
                    format!("N={size}: expected exactly one scalar boundary equation"),
                );
                compact_storage &= measurement.equations == 1;
            }
            Workload::GridTwoBodyInterior => {
                assess_grid(measurement, &mut failures);
                compact_storage &= measurement.equations == 0;
            }
            Workload::InitialGridFamily => {
                assess_initial_grid(measurement, &mut failures);
            }
        }
    }
    StructuralAssessment {
        integrity_passed: failures.is_empty(),
        spec_0032_compact_storage: compact_storage,
        failures,
    }
}

/// Every derivative row of the grid belongs to one of ten families (eight
/// edges, two interior bodies over the whole nest), and each family lowers to
/// one Solve tensor node: the inventory is independent of the grid size.
fn assess_grid(measurement: &Measurement, failures: &mut Vec<String>) {
    let size = measurement.size;
    let side = grid_side(size);
    require_structure(
        measurement.structured_families == 10
            && measurement.compact_domain_points == 2 * side * side,
        failures,
        format!(
            "N={size}: expected ten compact families covering all {} derivative rows",
            2 * side * side
        ),
    );
    require_structure(
        measurement.solve_map_nodes + measurement.solve_affine_stencil_nodes
            >= measurement.structured_families,
        failures,
        format!("N={size}: expected one native Solve tensor node per grid family"),
    );
    require_native_kernels(measurement, measurement.structured_families, 0, failures);
}

/// The initial-equation nest stays one `Map` node through Solve, and both the
/// initialization residual and its Jacobian run as native loop kernels with no
/// per-cell compiled row, at every size.
fn assess_initial_grid(measurement: &Measurement, failures: &mut Vec<String>) {
    let size = measurement.size;
    require_structure(
        measurement.initial_map_nodes == 1
            && measurement.initial_native_kernels == 2
            && measurement.initial_native_rows == 0,
        failures,
        format!(
            "N={size}: expected one initialization Map node compiled as two native kernels \
             (residual and Jacobian) with no per-row code, found {} nodes, {} kernels, {} rows",
            measurement.initial_map_nodes,
            measurement.initial_native_kernels,
            measurement.initial_native_rows
        ),
    );
    require_native_kernels(measurement, 1, 0, failures);
}

/// The native derivative block runs at least `kernels` loop kernels and
/// compiles exactly `rows` scalar rows one by one, at every size: a runtime
/// that re-expanded the compact nodes into per-row code fails here.
fn require_native_kernels(
    measurement: &Measurement,
    kernels: usize,
    rows: usize,
    failures: &mut Vec<String>,
) {
    let size = measurement.size;
    require_structure(
        measurement.native_kernels >= kernels && measurement.native_compiled_rows == rows,
        failures,
        format!(
            "N={size}: expected at least {kernels} native loop kernels and {rows} per-row \
             compiled rows, found {} kernels and {} rows",
            measurement.native_kernels, measurement.native_compiled_rows
        ),
    );
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
             solve_maps={} solve_stencils={} native_kernels={} native_rows={} \
             initial_maps={} initial_kernels={} initial_rows={}",
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
            measurement.native_kernels,
            measurement.native_compiled_rows,
            measurement.initial_map_nodes,
            measurement.initial_native_kernels,
            measurement.initial_native_rows,
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
            native_kernels: 1,
            native_compiled_rows: 1,
            ..Measurement::default()
        }
    }

    #[test]
    fn per_row_grid_families_cannot_pass_structural_ratchet() {
        // An unrolled outer binder leaves one family per interior row, and a
        // strided interior lowered as scalar rows has no tensor node.
        let side = grid_side(128);
        let measurement = Measurement {
            size: 128,
            structured_families: 8 + 2 * (side - 2),
            compact_domain_points: 2 * side * side,
            solve_map_nodes: 4,
            ..Measurement::default()
        };

        let assessment = assess_structure(Workload::GridTwoBodyInterior, &[measurement]);

        assert!(!assessment.integrity_passed);
        assert_eq!(assessment.failures.len(), 3);
    }

    #[test]
    fn a_scalarized_initialization_family_cannot_pass_structural_ratchet() {
        let measurement = Measurement {
            size: 128,
            native_kernels: 1,
            initial_map_nodes: 0,
            initial_native_rows: 2 * 128,
            ..Measurement::default()
        };

        let assessment = assess_structure(Workload::InitialGridFamily, &[measurement]);

        assert!(!assessment.integrity_passed);
        assert_eq!(assessment.failures.len(), 1);
    }

    #[test]
    fn per_row_native_compilation_cannot_pass_structural_ratchet() {
        // Compact Solve nodes whose runtime compiles every scalar row.
        let side = grid_side(128);
        let measurement = Measurement {
            size: 128,
            structured_families: 10,
            compact_domain_points: 2 * side * side,
            solve_map_nodes: 4,
            solve_affine_stencil_nodes: 6,
            native_kernels: 0,
            native_compiled_rows: 2 * side * side,
            ..Measurement::default()
        };

        let assessment = assess_structure(Workload::GridTwoBodyInterior, &[measurement]);

        assert!(!assessment.integrity_passed);
        assert_eq!(assessment.failures.len(), 1);
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

        // Placeholder rows break both the one-boundary-row invariant and the
        // compact-storage contract.
        assert!(!assessment.integrity_passed);
        assert!(!assessment.spec_0032_compact_storage);
    }

    #[test]
    fn compact_cascaded_owner_can_satisfy_structural_ratchet() {
        let assessment = assess_structure(Workload::CascadedFirstOrder, &[cascaded_measurement(1)]);

        assert!(assessment.integrity_passed);
        assert!(assessment.spec_0032_compact_storage);
    }
}
