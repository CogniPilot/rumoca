use anyhow::{Context, Result};
use clap::CommandFactory;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::{Cli, RepoCliInstallArgs, completion_cmd, exe_name, repo_root, run_status};

fn rum_cli_install_package_dir(root: &Path) -> PathBuf {
    root.join("crates/rumoca-tool-dev")
}

pub(crate) fn cargo_install_rum_args(root: &Path) -> Vec<String> {
    vec![
        "install".to_string(),
        "--path".to_string(),
        rum_cli_install_package_dir(root).display().to_string(),
        "--locked".to_string(),
        "--bin".to_string(),
        "rum".to_string(),
        "--force".to_string(),
    ]
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ShellKind {
    Bash,
    Zsh,
    Fish,
    PowerShell,
    Posix,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PathUpdateGuidance {
    pub(crate) current_command: String,
    pub(crate) persist_intro: String,
    pub(crate) persist_action: String,
    pub(crate) reload_hint: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ShellProfileUpdate {
    pub(crate) path: PathBuf,
    pub(crate) snippet: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CompletionInstallPlan {
    pub(crate) script_path: PathBuf,
    pub(crate) script_contents: String,
    pub(crate) profile_update: Option<ShellProfileUpdate>,
}

fn paths_equivalent(lhs: &Path, rhs: &Path) -> bool {
    if lhs == rhs {
        return true;
    }
    match (lhs.canonicalize(), rhs.canonicalize()) {
        (Ok(lhs), Ok(rhs)) => lhs == rhs,
        _ => false,
    }
}

pub(crate) fn path_var_contains_dir(path_env: Option<&OsStr>, dir: &Path) -> bool {
    path_env.is_some_and(|value| env::split_paths(value).any(|entry| paths_equivalent(&entry, dir)))
}

pub(crate) fn detect_shell_kind(shell_env: Option<&OsStr>) -> ShellKind {
    let Some(shell_env) = shell_env else {
        return ShellKind::Unknown;
    };
    let shell_name = Path::new(shell_env)
        .file_name()
        .and_then(OsStr::to_str)
        .or_else(|| shell_env.to_str())
        .unwrap_or_default();
    match shell_name {
        "bash" => ShellKind::Bash,
        "zsh" => ShellKind::Zsh,
        "fish" => ShellKind::Fish,
        "pwsh" | "powershell" | "pwsh.exe" | "powershell.exe" => ShellKind::PowerShell,
        "sh" | "dash" | "ash" | "ksh" => ShellKind::Posix,
        _ => ShellKind::Unknown,
    }
}

fn current_shell_kind() -> ShellKind {
    let shell_env = env::var_os("SHELL").or_else(|| env::var_os("COMSPEC"));
    detect_shell_kind(shell_env.as_deref())
}

fn shell_single_quote(value: &str) -> String {
    format!("'{}'", value.replace('\'', "'\\''"))
}

fn powershell_single_quote(value: &str) -> String {
    format!("'{}'", value.replace('\'', "''"))
}

pub(crate) fn shell_path_update_guidance(shell: ShellKind, bin_dir: &Path) -> PathUpdateGuidance {
    let bin_dir = bin_dir.display().to_string();
    match shell {
        ShellKind::Bash => {
            let line = format!("export PATH={}:\"$PATH\"", shell_single_quote(&bin_dir));
            PathUpdateGuidance {
                current_command: line.clone(),
                persist_intro: "Persist for future bash shells by adding this line to ~/.bashrc:"
                    .to_string(),
                persist_action: line,
                reload_hint: Some("source ~/.bashrc".to_string()),
            }
        }
        ShellKind::Zsh => {
            let line = format!("export PATH={}:\"$PATH\"", shell_single_quote(&bin_dir));
            PathUpdateGuidance {
                current_command: line.clone(),
                persist_intro: "Persist for future zsh shells by adding this line to ~/.zshrc:"
                    .to_string(),
                persist_action: line,
                reload_hint: Some("source ~/.zshrc".to_string()),
            }
        }
        ShellKind::Fish => PathUpdateGuidance {
            current_command: format!("set -gx PATH {} $PATH", shell_single_quote(&bin_dir)),
            persist_intro: "Persist for future fish shells by running:".to_string(),
            persist_action: format!("fish_add_path {}", shell_single_quote(&bin_dir)),
            reload_hint: None,
        },
        ShellKind::PowerShell => {
            let line = format!(
                "$env:Path = {} + ';' + $env:Path",
                powershell_single_quote(&bin_dir)
            );
            PathUpdateGuidance {
                current_command: line.clone(),
                persist_intro:
                    "Persist for future PowerShell sessions by adding this line to $PROFILE:"
                        .to_string(),
                persist_action: line,
                reload_hint: Some(". $PROFILE".to_string()),
            }
        }
        ShellKind::Posix => {
            let line = format!("export PATH={}:\"$PATH\"", shell_single_quote(&bin_dir));
            PathUpdateGuidance {
                current_command: line.clone(),
                persist_intro: "Persist for future shells by adding this line to ~/.profile:"
                    .to_string(),
                persist_action: line,
                reload_hint: Some("source ~/.profile".to_string()),
            }
        }
        ShellKind::Unknown => {
            let line = format!("export PATH={}:\"$PATH\"", shell_single_quote(&bin_dir));
            PathUpdateGuidance {
                current_command: line.clone(),
                persist_intro:
                    "Persist for future shells by adding this line to your shell startup file:"
                        .to_string(),
                persist_action: line,
                reload_hint: None,
            }
        }
    }
}

pub(crate) fn shell_profile_update(
    shell: ShellKind,
    home_dir: &Path,
    bin_dir: &Path,
) -> Option<ShellProfileUpdate> {
    let bin_dir = bin_dir.display().to_string();
    match shell {
        ShellKind::Bash => Some(ShellProfileUpdate {
            path: home_dir.join(".bashrc"),
            snippet: format!("export PATH={}:\"$PATH\"", shell_single_quote(&bin_dir)),
        }),
        ShellKind::Zsh => Some(ShellProfileUpdate {
            path: home_dir.join(".zshrc"),
            snippet: format!("export PATH={}:\"$PATH\"", shell_single_quote(&bin_dir)),
        }),
        ShellKind::Posix | ShellKind::Unknown => Some(ShellProfileUpdate {
            path: home_dir.join(".profile"),
            snippet: format!("export PATH={}:\"$PATH\"", shell_single_quote(&bin_dir)),
        }),
        ShellKind::Fish => Some(ShellProfileUpdate {
            path: home_dir.join(".config/fish/conf.d/rum-path.fish"),
            snippet: format!(
                "if not contains -- {} $PATH\n    set -gx PATH {} $PATH\nend",
                shell_single_quote(&bin_dir),
                shell_single_quote(&bin_dir)
            ),
        }),
        ShellKind::PowerShell => Some(ShellProfileUpdate {
            path: home_dir.join("Documents/PowerShell/Microsoft.PowerShell_profile.ps1"),
            snippet: format!(
                "$env:Path = {} + ';' + $env:Path",
                powershell_single_quote(&bin_dir)
            ),
        }),
    }
}

fn completion_shell_kind(shell: ShellKind) -> Option<completion_cmd::ShellKind> {
    match shell {
        ShellKind::Bash => Some(completion_cmd::ShellKind::Bash),
        ShellKind::Zsh => Some(completion_cmd::ShellKind::Zsh),
        ShellKind::Fish => Some(completion_cmd::ShellKind::Fish),
        ShellKind::PowerShell => Some(completion_cmd::ShellKind::PowerShell),
        ShellKind::Posix | ShellKind::Unknown => None,
    }
}

pub(crate) fn completion_install_plan(
    shell: ShellKind,
    home_dir: &Path,
) -> Option<CompletionInstallPlan> {
    let completion_shell = completion_shell_kind(shell)?;
    let mut command = Cli::command();
    let script_contents = completion_cmd::render(completion_shell, &mut command).ok()?;
    match shell {
        ShellKind::Bash => {
            let script_path = home_dir.join(".local/share/bash-completion/completions/rum");
            let quoted = shell_single_quote(&script_path.display().to_string());
            Some(CompletionInstallPlan {
                script_path: script_path.clone(),
                script_contents,
                profile_update: Some(ShellProfileUpdate {
                    path: home_dir.join(".bashrc"),
                    snippet: format!("if [ -f {quoted} ]; then\n  source {quoted}\nfi"),
                }),
            })
        }
        ShellKind::Zsh => {
            let completions_dir = home_dir.join(".zfunc");
            let quoted_dir = shell_single_quote(&completions_dir.display().to_string());
            Some(CompletionInstallPlan {
                script_path: completions_dir.join("_rum"),
                script_contents,
                profile_update: Some(ShellProfileUpdate {
                    path: home_dir.join(".zshrc"),
                    snippet: format!(
                        "fpath=({quoted_dir} $fpath)\nif ! (( $+functions[compdef] )); then\n  autoload -Uz compinit && compinit\nfi"
                    ),
                }),
            })
        }
        ShellKind::Fish => Some(CompletionInstallPlan {
            script_path: home_dir.join(".config/fish/completions/rum.fish"),
            script_contents,
            profile_update: None,
        }),
        ShellKind::PowerShell => {
            let script_path = home_dir.join("Documents/PowerShell/Completions/rum.ps1");
            let quoted = powershell_single_quote(&script_path.display().to_string());
            Some(CompletionInstallPlan {
                script_path,
                script_contents,
                profile_update: Some(ShellProfileUpdate {
                    path: home_dir.join("Documents/PowerShell/Microsoft.PowerShell_profile.ps1"),
                    snippet: format!(". {quoted}"),
                }),
            })
        }
        ShellKind::Posix | ShellKind::Unknown => None,
    }
}

fn user_home_dir() -> Option<PathBuf> {
    env::var_os("HOME")
        .or_else(|| env::var_os("USERPROFILE"))
        .map(PathBuf::from)
}

fn write_file_if_changed(path: &Path, contents: &str) -> Result<bool> {
    let existing = if path.is_file() {
        Some(
            fs::read_to_string(path)
                .with_context(|| format!("failed to read {}", path.display()))?,
        )
    } else {
        None
    };
    if existing.as_deref() == Some(contents) {
        return Ok(false);
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(path, contents).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(true)
}

fn append_unique_snippet(path: &Path, snippet: &str) -> Result<bool> {
    let existing = if path.is_file() {
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?
    } else {
        String::new()
    };
    if existing.contains(snippet) {
        return Ok(false);
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    let mut file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .with_context(|| format!("failed to open {}", path.display()))?;
    if !existing.is_empty() && !existing.ends_with('\n') {
        writeln!(file).with_context(|| format!("failed to update {}", path.display()))?;
    }
    writeln!(file, "{snippet}").with_context(|| format!("failed to update {}", path.display()))?;
    Ok(true)
}

fn cargo_bin_dir_hint() -> Option<PathBuf> {
    env::var_os("CARGO_HOME")
        .map(PathBuf::from)
        .map(|path| path.join("bin"))
        .or_else(|| {
            env::var_os("HOME")
                .map(PathBuf::from)
                .map(|path| path.join(".cargo/bin"))
        })
}

fn print_path_update_status(changed: bool, updated_label: &str, existing_label: &str, path: &Path) {
    let label = if changed {
        updated_label
    } else {
        existing_label
    };
    println!("{label} {}.", path.display());
}

pub(crate) fn cmd_install_rum_cli(args: RepoCliInstallArgs) -> Result<()> {
    let root = repo_root();
    let install_args = cargo_install_rum_args(&root);
    let mut cmd = Command::new("cargo");
    cmd.args(&install_args).current_dir(&root);
    run_status(cmd)?;

    println!("Installed rum with `cargo {}`.", install_args.join(" "));
    let shell = current_shell_kind();
    let home_dir = user_home_dir();
    if let Some(home_dir) = home_dir.as_deref() {
        if let Some(plan) = completion_install_plan(shell, home_dir) {
            let changed = write_file_if_changed(&plan.script_path, &plan.script_contents)?;
            print_path_update_status(
                changed,
                "Installed shell completions at",
                "Shell completions already up to date at",
                &plan.script_path,
            );
            if let Some(profile_update) = plan.profile_update {
                let changed = append_unique_snippet(&profile_update.path, &profile_update.snippet)?;
                print_path_update_status(
                    changed,
                    "Enabled shell completion loading in",
                    "Shell completion loading already configured in",
                    &profile_update.path,
                );
            }
        } else {
            println!(
                "Could not auto-install shell completions for this shell. Use `rum repo completions <shell>`."
            );
        }
    } else {
        println!(
            "Could not determine home directory to install shell completions. Use `rum repo completions <shell>`."
        );
    }
    if let Some(bin_dir) = cargo_bin_dir_hint() {
        let path_is_ready = path_var_contains_dir(env::var_os("PATH").as_deref(), &bin_dir);
        if args.path && !path_is_ready {
            let home_dir = home_dir
                .clone()
                .context("could not determine home directory for PATH update")?;
            let update = shell_profile_update(shell, &home_dir, &bin_dir)
                .context("automatic PATH updates are not supported for this shell")?;
            let changed = append_unique_snippet(&update.path, &update.snippet)?;
            if changed {
                println!("Persisted PATH update in {}.", update.path.display());
            } else {
                println!("PATH update already present in {}.", update.path.display());
            }
        }
        if path_var_contains_dir(env::var_os("PATH").as_deref(), &bin_dir) {
            println!(
                "{} is already on PATH. You can run `rum verify quick` now.",
                bin_dir.display()
            );
        } else {
            let guidance = shell_path_update_guidance(shell, &bin_dir);
            let rum_bin = bin_dir.join(exe_name("rum"));
            println!("{} is not on PATH in this shell.", bin_dir.display());
            println!("You can run rum immediately via {}.", rum_bin.display());
            println!("Run now in this shell:\n  {}", guidance.current_command);
            println!("{}\n  {}", guidance.persist_intro, guidance.persist_action);
            if let Some(reload_hint) = guidance.reload_hint {
                println!("Reload your shell or run:\n  {reload_hint}");
            }
            if !args.path {
                println!("To write the persistent PATH update for you, rerun:");
                println!("  cargo run --bin rum -- repo cli install --path");
            }
        }
    } else {
        println!("Ensure your cargo bin directory is on PATH, then run `rum verify quick`.");
    }
    Ok(())
}
