//! MSL/OMC reference, baseline, and parity-maintenance tooling.
//!
//! This bin owns the full `msl` command surface. `xtask`'s `repo msl` subcommand
//! is a thin passthrough that shells out to `cargo run -p rumoca-test-msl --bin
//! rumoca-msl-tools -- <args>`, so the heavy compiler stack these tools link is
//! never compiled for a plain `cargo xtask` invocation.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use anyhow::Result;
use clap::{Parser, Subcommand};

use rumoca_test_msl::{msl_flamegraph, msl_tools, repo_root};

#[derive(Debug, Parser)]
#[command(name = "rumoca-msl-tools")]
#[command(version)]
#[command(about = "MSL/OMC reference, baseline, and parity-maintenance tooling")]
struct Cli {
    #[command(subcommand)]
    command: MslCommand,
}

#[derive(Debug, Subcommand)]
// Variants are grouped (and the help wording is prefixed) so `--help` reads as
// clusters you can navigate without external docs. clap lists variants in
// declaration order; match arms are by name, so the order is purely for help
// readability.
enum MslCommand {
    // -- Generate OMC baselines (the reference data the gates compare against) --
    /// Generate baseline: OMC simulation + rumoca-vs-OMC trace & compile-speed comparison
    OmcSimulationReference(msl_tools::omc_simulation_reference::Args),

    // -- Reports built from existing results (read-only analysis) --
    /// Report: rank compile/balance/sim/trace failures to fix next
    Triage(msl_tools::triage::Args),
    /// Report: machine-readable rumoca-vs-OMC simulation parity failure buckets
    ParityManifest(msl_tools::parity_manifest::Args),
    /// Report: per-package MSL release compatibility summary
    CompatibilityReport(msl_tools::compatibility_report::Args),
    /// Report: diff rumoca balance output against the OMC reference
    CompareBalance(msl_tools::compare_balance::Args),
    /// Report: compare per-model phase/sim/trace transitions between two MSL result dirs
    TransitionDiff(msl_tools::transition_diff::Args),
    /// Report: assemble the MSL quality summary posted as the CI PR comment
    PrComment(msl_tools::pr_comment::Args),

    // -- Inspect one model (debug a single failure) --
    /// One model: dump JSON IR from every compiler stage
    DebugModel(msl_tools::debug_model::Args),
    /// One model: regenerate OMC+rumoca traces and open the uPlot overlay
    PlotCompare(msl_tools::plot_compare::Args),
    /// One model: diff rumoca's BLT/coupled-block/tearing against OMC's
    OmcStructure(msl_tools::omc_structure_diff::Args),
    /// One model (or a triage bucket): re-run the focused parity pipeline
    Rerun(msl_tools::rerun::Args),
    /// One model: cargo-flamegraph profile of compile or simulation
    Flamegraph(msl_flamegraph::MslFlamegraphArgs),

    // -- Catalog & baseline maintenance --
    /// Catalog MSL-shipped ModelicaTest cases by semantic feature area
    ModelicaTestCatalog(msl_tools::modelica_test_catalog::Args),
    /// Promote a quality snapshot to the checked-in fallback baseline JSON
    PromoteQualityBaseline(msl_tools::promote_quality_baseline::Args),
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        MslCommand::OmcSimulationReference(args) => msl_tools::omc_simulation_reference::run(args),
        MslCommand::Triage(args) => msl_tools::triage::run(args),
        MslCommand::ParityManifest(args) => msl_tools::parity_manifest::run(args),
        MslCommand::CompatibilityReport(args) => msl_tools::compatibility_report::run(args),
        MslCommand::CompareBalance(args) => msl_tools::compare_balance::run(args),
        MslCommand::TransitionDiff(args) => msl_tools::transition_diff::run(args),
        MslCommand::PrComment(args) => msl_tools::pr_comment::run(args),
        MslCommand::DebugModel(args) => msl_tools::debug_model::run(args),
        MslCommand::PlotCompare(args) => msl_tools::plot_compare::run(args),
        MslCommand::OmcStructure(args) => msl_tools::omc_structure_diff::run(args),
        MslCommand::Rerun(args) => msl_tools::rerun::run(args),
        MslCommand::Flamegraph(args) => msl_flamegraph::run(args, &repo_root()),
        MslCommand::ModelicaTestCatalog(args) => msl_tools::modelica_test_catalog::run(args),
        MslCommand::PromoteQualityBaseline(args) => msl_tools::promote_quality_baseline::run(args),
    }
}
