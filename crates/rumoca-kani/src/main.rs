use anyhow::Result;
use clap::Parser;
use rumoca_kani::KaniArgs;

#[derive(Debug, Parser)]
#[command(name = "rumoca-kani", version)]
#[command(about = "Validate and run Rumoca's manifest-defined Kani proofs")]
struct Cli {
    #[command(flatten)]
    args: KaniArgs,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let root = std::env::current_dir()?;
    rumoca_kani::run(&root, &cli.args)
}
