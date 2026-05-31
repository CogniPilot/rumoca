#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use anyhow::Result;
use clap::{Parser, Subcommand};

pub(crate) use xtask::msl_tools::{
    compare_balance, omc_simulation_reference, parity_manifest, plot_compare,
    promote_quality_baseline, rerun, triage,
};

#[derive(Debug, Parser)]
#[command(name = "rumoca-msl-tools")]
#[command(version)]
#[command(about = "MSL/OMC reference tooling for rumoca test infrastructure")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Generate OMC simulation reference data and trace comparisons
    OmcSimulationReference(omc_simulation_reference::Args),
    /// Compare rumoca balance output against OMC reference data
    CompareBalance(compare_balance::Args),
    /// Build rumoca-vs-OMC simulation parity failure manifest
    ParityManifest(parity_manifest::Args),
    /// Build ranked MSL compile/balance/simulation/trace triage reports
    Triage(triage::Args),
    /// Rerun one focused MSL model or triage reason-code bucket
    Rerun(rerun::Args),
    /// Regenerate OMC+Rumoca traces and plot them for one model
    PlotCompare(plot_compare::Args),
    /// Promote target quality snapshot to committed baseline JSON
    PromoteQualityBaseline(promote_quality_baseline::Args),
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::OmcSimulationReference(args) => omc_simulation_reference::run(args),
        Commands::CompareBalance(args) => compare_balance::run(args),
        Commands::ParityManifest(args) => parity_manifest::run(args),
        Commands::Triage(args) => triage::run(args),
        Commands::Rerun(args) => rerun::run(args),
        Commands::PlotCompare(args) => plot_compare::run(args),
        Commands::PromoteQualityBaseline(args) => promote_quality_baseline::run(args),
    }
}
