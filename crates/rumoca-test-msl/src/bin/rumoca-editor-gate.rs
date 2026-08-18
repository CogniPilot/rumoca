use anyhow::Result;
use clap::{Parser, Subcommand};
use std::path::PathBuf;

use rumoca_test_msl::{editor_gate, repo_root};

#[derive(Debug, Parser)]
#[command(name = "rumoca-editor-gate", version)]
#[command(about = "Run Rumoca's LSP and editor integration gates")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Run full-MSL LSP timings plus VS Code and browser runtime smokes.
    LspMslCompletionTimings {
        #[arg(long)]
        install_prereqs: bool,
        #[arg(long)]
        require_runtimes: bool,
        #[arg(long, default_value = "target/msl/ModelicaStandardLibrary-4.1.0")]
        msl_root: PathBuf,
    },
    /// Run the VS Code extension-host MSL smoke.
    VscodeMsl {
        #[arg(long)]
        install_prereqs: bool,
        #[arg(long, default_value = "target/msl/ModelicaStandardLibrary-4.1.0")]
        msl_root: PathBuf,
    },
    /// Run the browser-hosted WASM editor MSL smoke.
    WasmEditorMsl {
        #[arg(long, default_value = "target/msl/ModelicaStandardLibrary-4.1.0")]
        msl_root: PathBuf,
    },
    #[command(hide = true)]
    ServePlayground {
        #[arg(long)]
        port: u16,
    },
}

fn main() -> Result<()> {
    let root = repo_root();
    match Cli::parse().command {
        Command::LspMslCompletionTimings {
            install_prereqs,
            require_runtimes,
            msl_root,
        } => editor_gate::run_lsp_msl_completion_timings(
            &root,
            &root.join(msl_root),
            install_prereqs,
            require_runtimes,
        ),
        Command::VscodeMsl {
            install_prereqs,
            msl_root,
        } => editor_gate::run_vscode_msl_smoke(&root, &root.join(msl_root), install_prereqs),
        Command::WasmEditorMsl { msl_root } => {
            editor_gate::run_wasm_msl_smoke(&root, &root.join(msl_root))
        }
        Command::ServePlayground { port } => editor_gate::serve_playground(&root, port),
    }
}
