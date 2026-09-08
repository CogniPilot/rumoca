//! GALEC (`.alg`) Language Server binary — speaks LSP over stdio.
//!
//! Thin clap wrapper so editors can probe `--version`, then serve LSP over
//! the sole supported transport: stdio.

use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "rumoca-lsp-galec")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "Rumoca GALEC (.alg) Language Server", long_about = None)]
struct Cli {}

#[tokio::main]
async fn main() {
    // Parsing gives `--version`/`--help` for the editor's server probe.
    let _cli = Cli::parse();
    rumoca_tool_lsp_galec::run_server().await;
}

#[cfg(test)]
mod tests {
    use super::Cli;
    use clap::Parser;

    #[test]
    fn retired_no_op_flags_remain_rejected() {
        for flag in ["--stdio", "--verbose", "-v"] {
            let error = Cli::try_parse_from(["rumoca-lsp-galec", flag])
                .expect_err("a retired no-op flag must not return");
            assert_eq!(error.kind(), clap::error::ErrorKind::UnknownArgument);
        }
    }

    #[test]
    fn reports_a_version() {
        // `--version` must exit cleanly (clap handles it) so the editor's
        // `--version` server probe succeeds instead of starting the server.
        let err = Cli::try_parse_from(["rumoca-lsp-galec", "--version"])
            .expect_err("--version exits via clap");
        assert_eq!(err.kind(), clap::error::ErrorKind::DisplayVersion);
    }
}
