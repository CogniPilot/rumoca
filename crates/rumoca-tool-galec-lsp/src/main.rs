//! GALEC (`.alg`) Language Server binary — speaks LSP over stdio.
//!
//! Mirrors `rumoca-lsp`: a thin clap wrapper so editors can probe `--version`
//! and pass a compatibility `--stdio` flag, then serve LSP over stdio.

use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "rumoca-galec-lsp")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "Rumoca GALEC (.alg) Language Server", long_about = None)]
struct Cli {
    /// Compatibility no-op for clients that explicitly pass --stdio.
    #[arg(long, hide = true)]
    _stdio: bool,

    /// Increase logging verbosity (reserved for future use).
    #[arg(short, long, action = clap::ArgAction::Count)]
    _verbose: u8,
}

#[tokio::main]
async fn main() {
    // Parsing gives `--version`/`--help` (for the editor's server probe) and
    // accepts `--stdio`; stdio is the only transport, so the flags are no-ops.
    let _cli = Cli::parse();
    rumoca_tool_galec_lsp::run_server().await;
}

#[cfg(test)]
mod tests {
    use super::Cli;
    use clap::Parser;

    #[test]
    fn parses_the_stdio_flag() {
        let cli = Cli::try_parse_from(["rumoca-galec-lsp", "--stdio"]).expect("stdio flag parses");
        assert!(cli._stdio);
    }

    #[test]
    fn reports_a_version() {
        // `--version` must exit cleanly (clap handles it) so the editor's
        // `--version` server probe succeeds instead of starting the server.
        let err = Cli::try_parse_from(["rumoca-galec-lsp", "--version"])
            .expect_err("--version exits via clap");
        assert_eq!(err.kind(), clap::error::ErrorKind::DisplayVersion);
    }
}
