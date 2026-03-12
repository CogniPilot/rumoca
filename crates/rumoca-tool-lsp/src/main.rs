//! Rumoca Language Server Protocol (LSP) binary.

use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "rumoca-lsp")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "Rumoca Modelica Language Server", long_about = None)]
struct Cli {
    /// Compatibility no-op for clients that explicitly pass --stdio.
    #[arg(long, hide = true)]
    _stdio: bool,

    /// Increase logging verbosity (reserved for future use)
    #[arg(short, long, action = clap::ArgAction::Count)]
    _verbose: u8,
}

#[tokio::main]
async fn main() {
    let _ = Cli::parse();
    rumoca_tool_lsp::run_server().await;
}

#[cfg(test)]
mod tests {
    use super::Cli;
    use clap::Parser;

    #[test]
    fn parse_accepts_stdio_flag() {
        let cli = Cli::try_parse_from(["rumoca-lsp", "--stdio"]).expect("stdio flag should parse");
        assert!(cli._stdio);
    }

    #[test]
    fn parse_accepts_stdio_with_verbosity() {
        let cli = Cli::try_parse_from(["rumoca-lsp", "--stdio", "-v"])
            .expect("stdio+verbose should parse");
        assert!(cli._stdio);
        assert_eq!(cli._verbose, 1);
    }
}
