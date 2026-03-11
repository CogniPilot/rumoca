//! Rumoca Language Server Protocol (LSP) binary.

use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "rumoca-lsp")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "Rumoca Modelica Language Server", long_about = None)]
struct Cli {
    /// Increase logging verbosity (reserved for future use)
    #[arg(short, long, action = clap::ArgAction::Count)]
    _verbose: u8,
}

#[tokio::main]
async fn main() {
    let _ = Cli::parse();
    rumoca_tool_lsp::run_server().await;
}
