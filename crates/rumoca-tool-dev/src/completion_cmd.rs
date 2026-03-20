use anyhow::{Context, Result};
use clap::Args;
use clap::ValueEnum;
use clap_complete::generate;
use std::io;

#[derive(Debug, Args, Clone)]
pub(crate) struct CompletionsArgs {
    /// Target shell
    #[arg(value_enum)]
    pub(crate) shell: ShellKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub(crate) enum ShellKind {
    Bash,
    Zsh,
    Fish,
    PowerShell,
}

impl From<ShellKind> for clap_complete::Shell {
    fn from(value: ShellKind) -> Self {
        match value {
            ShellKind::Bash => Self::Bash,
            ShellKind::Zsh => Self::Zsh,
            ShellKind::Fish => Self::Fish,
            ShellKind::PowerShell => Self::PowerShell,
        }
    }
}

pub(crate) fn run(args: CompletionsArgs, command: &mut clap::Command) -> Result<()> {
    let bin_name = command.get_name().to_string();
    let shell: clap_complete::Shell = args.shell.into();
    generate(shell, command, bin_name, &mut io::stdout());
    Ok(())
}

pub(crate) fn render(shell: ShellKind, command: &mut clap::Command) -> Result<String> {
    let mut output = Vec::new();
    let bin_name = command.get_name().to_string();
    let shell: clap_complete::Shell = shell.into();
    generate(shell, command, bin_name, &mut output);
    String::from_utf8(output).context("generated completion script was not valid UTF-8")
}
