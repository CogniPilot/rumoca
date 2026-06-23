use std::path::PathBuf;

use anyhow::{Context, Result};

use crate::cli::{Cli, CompletionShell};

pub(crate) fn discover_workspace_root_for_model_file(model_file: &str) -> Option<PathBuf> {
    let input_path = PathBuf::from(model_file);
    let absolute = if input_path.is_absolute() {
        input_path
    } else {
        std::env::current_dir().ok()?.join(input_path)
    };
    let start_dir = if absolute.is_dir() {
        absolute
    } else {
        absolute.parent()?.to_path_buf()
    };
    for ancestor in start_dir.ancestors() {
        if ancestor.join("modelica_dependencies.toml").is_file() {
            return Some(ancestor.to_path_buf());
        }
    }
    None
}

/// Generate a shell completion script directly from the clap command tree, so
/// completions can never drift from the real command/flag set (no hand-
/// maintained list to keep in sync — see the CLI review's completions finding).
pub(crate) fn completion_script(shell: CompletionShell) -> Result<String> {
    use clap::CommandFactory;

    let shell = match shell {
        CompletionShell::Bash => clap_complete::Shell::Bash,
        CompletionShell::Zsh => clap_complete::Shell::Zsh,
        CompletionShell::Fish => clap_complete::Shell::Fish,
        CompletionShell::PowerShell => clap_complete::Shell::PowerShell,
    };
    let mut command = Cli::command();
    let mut buffer = Vec::new();
    clap_complete::generate(shell, &mut command, "rumoca", &mut buffer);
    String::from_utf8(buffer).context("clap_complete emitted invalid UTF-8")
}
