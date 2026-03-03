use anyhow::Result;
use clap::Parser;
use clap::ValueEnum;
use std::io::{self, Write};

#[derive(Debug, Parser, Clone)]
pub(crate) struct CompletionsArgs {
    /// Target shell
    #[arg(value_enum)]
    pub(crate) shell: ShellKind,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum ShellKind {
    Bash,
    Zsh,
    Fish,
    PowerShell,
}

pub(crate) fn run(args: CompletionsArgs, command_name: &str, subcommands: &[&str]) -> Result<()> {
    let script = match args.shell {
        ShellKind::Bash => render_bash(command_name, subcommands),
        ShellKind::Zsh => render_zsh(command_name, subcommands),
        ShellKind::Fish => render_fish(command_name, subcommands),
        ShellKind::PowerShell => render_powershell(command_name, subcommands),
    };
    io::stdout().write_all(script.as_bytes())?;
    Ok(())
}

fn render_bash(command_name: &str, subcommands: &[&str]) -> String {
    let words = subcommands.join(" ");
    format!(
        r#"_{}_completions() {{
  local cur
  cur="${{COMP_WORDS[COMP_CWORD]}}"
  if [[ $COMP_CWORD -eq 1 ]]; then
    COMPREPLY=($(compgen -W "{}" -- "$cur"))
  fi
}}
complete -F _{}_completions {}
"#,
        command_name.replace('-', "_"),
        words,
        command_name.replace('-', "_"),
        command_name
    )
}

fn render_zsh(command_name: &str, subcommands: &[&str]) -> String {
    let entries = subcommands
        .iter()
        .map(|sub| format!("'{sub}:{sub} command'"))
        .collect::<Vec<_>>()
        .join(" \\\n  ");
    format!(
        r#"#compdef {}
_{}() {{
  _arguments '1: :(({{
  {}
  }}))'
}}
compdef _{} {}
"#,
        command_name,
        command_name.replace('-', "_"),
        entries,
        command_name.replace('-', "_"),
        command_name
    )
}

fn render_fish(command_name: &str, subcommands: &[&str]) -> String {
    subcommands
        .iter()
        .map(|sub| {
            format!(
                "complete -c {command_name} -n '__fish_use_subcommand' -a '{sub}' -d '{sub} command'"
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
        + "\n"
}

fn render_powershell(command_name: &str, subcommands: &[&str]) -> String {
    let entries = subcommands
        .iter()
        .map(|sub| format!("'{sub}'"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        r#"Register-ArgumentCompleter -CommandName {} -ScriptBlock {{
  param($wordToComplete, $commandAst, $cursorPosition)
  @({}) | Where-Object {{ $_ -like "$wordToComplete*" }} | ForEach-Object {{
    [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
  }}
}}
"#,
        command_name, entries
    )
}
