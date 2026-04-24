use anyhow::{Context, Result};
use clap::CommandFactory;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io::Write;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::{
    Cli, RepoCliInstallArgs, RepoCompletionsInstallArgs, RepoUbuntuInstallArgs, command_exists,
    completion_cmd, repo_root, run_status,
};

fn rum_cli_install_package_dir(root: &Path) -> PathBuf {
    root.join("crates/rumoca-tool-dev")
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ShellCommandPlan {
    pub(crate) program: String,
    pub(crate) args: Vec<String>,
}

pub(crate) fn rum_cli_launcher_path(bin_dir: &Path) -> PathBuf {
    #[cfg(windows)]
    {
        bin_dir.join("rum.cmd")
    }
    #[cfg(not(windows))]
    {
        bin_dir.join("rum")
    }
}

pub(crate) fn ubuntu_vscode_smoke_prereq_install_plan(
    no_update: bool,
    use_sudo: bool,
) -> Vec<ShellCommandPlan> {
    let mut plans = Vec::new();
    if !no_update {
        plans.push(apt_command_plan(use_sudo, ["update"]));
    }
    plans.push(apt_command_plan(
        use_sudo,
        ["install", "-y", "xvfb", "xauth"],
    ));
    plans
}

fn apt_command_plan<const N: usize>(use_sudo: bool, args: [&str; N]) -> ShellCommandPlan {
    if use_sudo {
        let mut full_args = Vec::with_capacity(N + 1);
        full_args.push("apt-get".to_string());
        full_args.extend(args.into_iter().map(str::to_string));
        ShellCommandPlan {
            program: "sudo".to_string(),
            args: full_args,
        }
    } else {
        ShellCommandPlan {
            program: "apt-get".to_string(),
            args: args.into_iter().map(str::to_string).collect(),
        }
    }
}

fn render_shell_command(plan: &ShellCommandPlan) -> String {
    std::iter::once(plan.program.as_str())
        .chain(plan.args.iter().map(String::as_str))
        .collect::<Vec<_>>()
        .join(" ")
}

pub(crate) fn install_ubuntu_vscode_smoke_prereqs(no_update: bool) -> Result<()> {
    anyhow::ensure!(
        cfg!(target_os = "linux"),
        "`rum repo ubuntu install-vscode-smoke-prereqs` only supports Linux hosts"
    );
    anyhow::ensure!(
        command_exists("apt-get"),
        "missing `apt-get`; this helper currently targets Ubuntu/Debian-style package managers"
    );

    let plans = ubuntu_vscode_smoke_prereq_install_plan(no_update, command_exists("sudo"));
    println!("Installing headless VS Code smoke prerequisites: xvfb xauth");
    for plan in plans {
        println!("Running: {}", render_shell_command(&plan));
        let mut command = Command::new(&plan.program);
        command.args(&plan.args);
        run_status(command)?;
    }
    println!("Installed headless VS Code smoke prerequisites.");
    Ok(())
}

pub(crate) fn rum_cli_launcher_contents(root: &Path) -> String {
    let root = root.display().to_string();
    #[cfg(windows)]
    {
        format!(
            "@echo off\r\nsetlocal\r\nset \"RUM_REPO_ROOT={root}\"\r\ncd /d \"%RUM_REPO_ROOT%\" || exit /b %errorlevel%\r\ncargo run -q -p rumoca-tool-dev --bin rum -- %*\r\n"
        )
    }
    #[cfg(not(windows))]
    {
        format!(
            "#!/bin/sh\nset -eu\ncd {}\ntarget_dir=${{CARGO_TARGET_DIR:-target}}\ncase \"$target_dir\" in\n  /*) rum_bin=\"$target_dir/debug/rum\" ;;\n  *) rum_bin=\"$PWD/$target_dir/debug/rum\" ;;\nesac\nbuild_log=$(mktemp \"${{TMPDIR:-/tmp}}/rum-build.XXXXXX\")\nshow_flag=\"$build_log.show\"\ncleanup() {{ rm -f \"$build_log\" \"$show_flag\"; }}\ntrap cleanup EXIT INT TERM\ncargo build -p rumoca-tool-dev --bin rum >\"$build_log\" 2>&1 &\nbuild_pid=$!\n(\n  sleep 0.5\n  if kill -0 \"$build_pid\" 2>/dev/null; then\n    touch \"$show_flag\"\n    tail -n +1 -f \"$build_log\" >&2 &\n    tail_pid=$!\n    while kill -0 \"$build_pid\" 2>/dev/null; do\n      sleep 0.1\n    done\n    kill \"$tail_pid\" 2>/dev/null || true\n  fi\n) &\nnotice_pid=$!\nif ! wait \"$build_pid\"; then\n  kill \"$notice_pid\" 2>/dev/null || true\n  if [ ! -f \"$show_flag\" ]; then\n    cat \"$build_log\" >&2\n  fi\n  exit 1\nfi\nkill \"$notice_pid\" 2>/dev/null || true\nexec \"$rum_bin\" \"$@\"\n",
            shell_single_quote(&root),
        )
    }
}

#[cfg(unix)]
fn ensure_launcher_executable(path: &Path) -> Result<()> {
    let metadata =
        fs::metadata(path).with_context(|| format!("failed to stat {}", path.display()))?;
    let mut permissions = metadata.permissions();
    let mode = permissions.mode();
    let desired_mode = mode | 0o755;
    if desired_mode != mode {
        permissions.set_mode(desired_mode);
        fs::set_permissions(path, permissions)
            .with_context(|| format!("failed to chmod {}", path.display()))?;
    }
    Ok(())
}

#[cfg(not(unix))]
fn ensure_launcher_executable(_path: &Path) -> Result<()> {
    Ok(())
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

fn completion_shell_to_install_shell(shell: completion_cmd::ShellKind) -> ShellKind {
    match shell {
        completion_cmd::ShellKind::Bash => ShellKind::Bash,
        completion_cmd::ShellKind::Zsh => ShellKind::Zsh,
        completion_cmd::ShellKind::Fish => ShellKind::Fish,
        completion_cmd::ShellKind::PowerShell => ShellKind::PowerShell,
    }
}

pub(crate) fn cmd_install_shell_completions(args: RepoCompletionsInstallArgs) -> Result<()> {
    let shell = args
        .shell
        .map(completion_shell_to_install_shell)
        .unwrap_or_else(current_shell_kind);
    let home_dir =
        user_home_dir().context("could not determine home directory to install completions")?;
    let plan = completion_install_plan(shell, &home_dir).with_context(|| {
        format!(
            "shell completion installation is not supported for {:?}; use `rum repo completions print <shell>` instead",
            shell
        )
    })?;
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
    Ok(())
}

pub(crate) fn cmd_install_ubuntu_vscode_smoke_prereqs(args: RepoUbuntuInstallArgs) -> Result<()> {
    install_ubuntu_vscode_smoke_prereqs(args.no_update)
}

fn user_home_dir() -> Option<PathBuf> {
    env::var_os("HOME")
        .or_else(|| env::var_os("USERPROFILE"))
        .map(PathBuf::from)
}

fn write_file_if_changed(path: &Path, contents: &str) -> Result<bool> {
    let existing = if path.is_file() {
        Some(fs::read(path).with_context(|| format!("failed to read {}", path.display()))?)
    } else {
        None
    };
    if existing.as_deref() == Some(contents.as_bytes()) {
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
    let bin_dir = cargo_bin_dir_hint().context("could not determine cargo bin directory")?;
    let launcher_path = rum_cli_launcher_path(&bin_dir);
    let launcher_contents = rum_cli_launcher_contents(&root);
    let changed = write_file_if_changed(&launcher_path, &launcher_contents)?;
    ensure_launcher_executable(&launcher_path)?;
    print_path_update_status(
        changed,
        "Installed rum launcher at",
        "Rum launcher already up to date at",
        &launcher_path,
    );
    println!(
        "The launcher quietly builds `rum` when needed, then runs it from {}.",
        rum_cli_install_package_dir(&root).display()
    );
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
                "Could not auto-install shell completions for this shell. Use `rum repo completions print <shell>`."
            );
        }
    } else {
        println!(
            "Could not determine home directory to install shell completions. Use `rum repo completions print <shell>`."
        );
    }
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
        println!("{} is not on PATH in this shell.", bin_dir.display());
        println!(
            "You can run rum immediately via {}.",
            launcher_path.display()
        );
        println!("Run now in this shell:\n  {}", guidance.current_command);
        println!("{}\n  {}", guidance.persist_intro, guidance.persist_action);
        if let Some(reload_hint) = guidance.reload_hint {
            println!("Reload your shell or run:\n  {reload_hint}");
        }
        if !args.path {
            println!("To write the persistent PATH update for you, rerun:");
            println!("  {} repo cli install --path", launcher_path.display());
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{ubuntu_vscode_smoke_prereq_install_plan, write_file_if_changed};
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn new_temp_dir(name: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("rumoca-tool-dev-{name}-{unique}"));
        fs::create_dir_all(&dir).expect("mkdir temp dir");
        dir
    }

    #[test]
    fn ubuntu_vscode_smoke_prereq_install_plan_includes_update_and_install() {
        let plan = ubuntu_vscode_smoke_prereq_install_plan(false, true);
        assert_eq!(plan.len(), 2);
        assert_eq!(plan[0].program, "sudo");
        assert_eq!(
            plan[0].args,
            vec!["apt-get".to_string(), "update".to_string()]
        );
        assert_eq!(
            plan[1].args,
            vec![
                "apt-get".to_string(),
                "install".to_string(),
                "-y".to_string(),
                "xvfb".to_string(),
                "xauth".to_string(),
            ]
        );
    }

    #[test]
    fn ubuntu_vscode_smoke_prereq_install_plan_can_skip_update() {
        let plan = ubuntu_vscode_smoke_prereq_install_plan(true, false);
        assert_eq!(plan.len(), 1);
        assert_eq!(plan[0].program, "apt-get");
        assert_eq!(
            plan[0].args,
            vec![
                "install".to_string(),
                "-y".to_string(),
                "xvfb".to_string(),
                "xauth".to_string(),
            ]
        );
    }

    #[test]
    fn write_file_if_changed_replaces_non_utf8_existing_file() {
        let temp = new_temp_dir("binary-launcher-replace");
        let path = temp.join("rum");
        fs::write(&path, [0xff, 0x00, 0xfe]).expect("seed binary file");

        let changed =
            write_file_if_changed(&path, "#!/bin/sh\nexec cargo run -- \"$@\"\n").expect("write");

        assert!(changed);
        assert_eq!(
            fs::read_to_string(&path).expect("launcher text"),
            "#!/bin/sh\nexec cargo run -- \"$@\"\n"
        );
    }
}
