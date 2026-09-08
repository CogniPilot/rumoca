//! Rumoca Language Server Protocol (LSP) binary.

use clap::Parser;
use rumoca_tool_lsp::LspTimingPaths;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "rumoca-lsp")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "Rumoca Modelica Language Server", long_about = None)]
struct Cli {
    /// Append completion timing records (JSONL) to this file (benchmark use).
    #[arg(long, hide = true)]
    completion_timing_file: Option<PathBuf>,

    /// Append completion progress records (JSONL) to this file (benchmark use).
    #[arg(long, hide = true)]
    completion_progress_file: Option<PathBuf>,

    /// Append diagnostics timing records (JSONL) to this file (benchmark use).
    #[arg(long, hide = true)]
    diagnostics_timing_file: Option<PathBuf>,

    /// Append navigation timing records (JSONL) to this file (benchmark use).
    #[arg(long, hide = true)]
    navigation_timing_file: Option<PathBuf>,

    /// Append startup timing records (JSONL) to this file (benchmark use).
    #[arg(long, hide = true)]
    startup_timing_file: Option<PathBuf>,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();
    let timing_paths = LspTimingPaths {
        completion: cli.completion_timing_file,
        completion_progress: cli.completion_progress_file,
        diagnostics: cli.diagnostics_timing_file,
        navigation: cli.navigation_timing_file,
        startup: cli.startup_timing_file,
    };
    rumoca_tool_lsp::run_server_with_timing_paths(timing_paths).await;
}

#[cfg(test)]
mod tests {
    use super::Cli;
    use clap::Parser;

    #[test]
    fn retired_no_op_flags_remain_rejected() {
        for flag in ["--stdio", "--verbose", "-v"] {
            let error = Cli::try_parse_from(["rumoca-lsp", flag])
                .expect_err("a retired no-op flag must not return");
            assert_eq!(error.kind(), clap::error::ErrorKind::UnknownArgument);
        }
    }
}
