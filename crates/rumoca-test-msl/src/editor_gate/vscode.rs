//! VS Code extension-host smoke harness support.

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use tempfile::TempDir;

use crate::proc::{command_exists, exe_name, run_capture, run_status, run_status_quiet};
use crate::web_assets;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VscodeNpmDependencyMode {
    RefreshLocked,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VscodeNpmInstallPlan {
    Ci,
    Install,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) struct VscodeSmokeOptions {
    pub(crate) install_prereqs: bool,
}

struct PreparedVscodeSmokeCommand {
    command: Command,
    _stage_dir: TempDir,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub(crate) struct VscodeStageCacheDeltaSummary {
    #[serde(alias = "fileItemIndexQueryHits")]
    pub(crate) file_item_index_query_hits: Option<u64>,
    #[serde(alias = "fileItemIndexQueryMisses")]
    pub(crate) file_item_index_query_misses: Option<u64>,
    #[serde(alias = "declarationIndexQueryHits")]
    pub(crate) declaration_index_query_hits: Option<u64>,
    #[serde(alias = "declarationIndexQueryMisses")]
    pub(crate) declaration_index_query_misses: Option<u64>,
    #[serde(alias = "scopeQueryHits")]
    pub(crate) scope_query_hits: Option<u64>,
    #[serde(alias = "scopeQueryMisses")]
    pub(crate) scope_query_misses: Option<u64>,
    #[serde(alias = "sourceSetPackageMembershipQueryHits")]
    pub(crate) source_set_package_membership_query_hits: Option<u64>,
    #[serde(alias = "sourceSetPackageMembershipQueryMisses")]
    pub(crate) source_set_package_membership_query_misses: Option<u64>,
    #[serde(alias = "orphanPackageMembershipQueryHits")]
    pub(crate) orphan_package_membership_query_hits: Option<u64>,
    #[serde(alias = "orphanPackageMembershipQueryMisses")]
    pub(crate) orphan_package_membership_query_misses: Option<u64>,
    #[serde(alias = "libraryCompletionCacheHits")]
    pub(crate) namespace_completion_cache_hits: Option<u64>,
    #[serde(alias = "libraryCompletionCacheMisses")]
    pub(crate) namespace_completion_cache_misses: Option<u64>,
    #[serde(alias = "libraryFilesParsed")]
    pub(crate) source_root_files_parsed: Option<u64>,
    #[serde(alias = "standardResolvedBuilds")]
    pub(crate) standard_resolved_builds: Option<u64>,
    #[serde(alias = "semanticNavigationBuilds")]
    pub(crate) semantic_navigation_builds: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct VscodeStageTimingSummary {
    pub(crate) uri: Option<String>,
    pub(crate) source_root_load_ms: Option<u64>,
    pub(crate) completion_source_root_load_ms: Option<u64>,
    pub(crate) namespace_completion_prime_ms: Option<u64>,
    pub(crate) needs_resolved_session: Option<bool>,
    pub(crate) ast_fast_path_matched: Option<bool>,
    pub(crate) query_fast_path_check_ms: Option<u64>,
    pub(crate) query_fast_path_matched: Option<bool>,
    pub(crate) resolved_build_ms: Option<u64>,
    pub(crate) completion_handler_ms: Option<u64>,
    pub(crate) total_ms: Option<u64>,
    pub(crate) built_resolved_tree: Option<bool>,
    pub(crate) namespace_index_query_hits: Option<u64>,
    pub(crate) namespace_index_query_misses: Option<u64>,
    pub(crate) file_item_index_query_hits: Option<u64>,
    pub(crate) file_item_index_query_misses: Option<u64>,
    pub(crate) declaration_index_query_hits: Option<u64>,
    pub(crate) declaration_index_query_misses: Option<u64>,
    pub(crate) scope_query_hits: Option<u64>,
    pub(crate) scope_query_misses: Option<u64>,
    pub(crate) source_set_package_membership_query_hits: Option<u64>,
    pub(crate) source_set_package_membership_query_misses: Option<u64>,
    pub(crate) orphan_package_membership_query_hits: Option<u64>,
    pub(crate) orphan_package_membership_query_misses: Option<u64>,
    pub(crate) class_name_count_after_ensure: Option<u64>,
    pub(crate) session_cache_delta: Option<VscodeStageCacheDeltaSummary>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct VscodeMslSmokeSummary {
    pub(crate) activate_ms: Option<u64>,
    pub(crate) open_ms: Option<u64>,
    pub(crate) code_lens_ms: Option<u64>,
    pub(crate) code_lens_count: Option<u64>,
    pub(crate) source_root_load_ms: Option<u64>,
    pub(crate) source_root_load_completion_count: Option<u64>,
    pub(crate) source_root_expected_completion_present: Option<bool>,
    pub(crate) source_root_stage_timings: Option<VscodeStageTimingSummary>,
    pub(crate) completion_ms: Option<u64>,
    pub(crate) completion_count: Option<u64>,
    pub(crate) expected_completion_present: Option<bool>,
    pub(crate) warm_completion_ms: Option<u64>,
    pub(crate) warm_completion_count: Option<u64>,
    pub(crate) warm_expected_completion_present: Option<bool>,
    pub(crate) hover_ms: Option<u64>,
    pub(crate) hover_count: Option<u64>,
    pub(crate) expected_hover_present: Option<bool>,
    pub(crate) definition_ms: Option<u64>,
    pub(crate) definition_count: Option<u64>,
    pub(crate) expected_definition_present: Option<bool>,
    pub(crate) cross_file_definition_present: Option<bool>,
    pub(crate) cold_stage_timings: Option<VscodeStageTimingSummary>,
    pub(crate) warm_stage_timings: Option<VscodeStageTimingSummary>,
    pub(crate) latest_stage_timings: Option<VscodeStageTimingSummary>,
}

pub(crate) fn run_vscode_msl_smoke(
    root: &Path,
    msl_root: &Path,
    install_prereqs: bool,
) -> Result<()> {
    let output_dir = root.join("target/editor-msl-smoke");
    let _ = run_vscode_msl_smoke_report(
        root,
        msl_root,
        &output_dir,
        VscodeSmokeOptions { install_prereqs },
    )?;
    Ok(())
}

pub(crate) fn can_launch_vscode_msl_smoke() -> bool {
    let environment = current_vscode_smoke_environment();
    command_available("node")
        && command_available("npm")
        && select_vscode_smoke_launch_mode(environment).is_ok()
}

pub(crate) fn run_vscode_msl_smoke_report(
    root: &Path,
    msl_root: &Path,
    output_dir: &Path,
    options: VscodeSmokeOptions,
) -> Result<VscodeMslSmokeSummary> {
    fs::create_dir_all(output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;
    let summary_path = output_dir.join("vscode-msl-smoke-summary.json");
    let timing_path = output_dir.join("vscode-msl-completion-timings.jsonl");
    let PreparedVscodeSmokeCommand {
        command: smoke,
        _stage_dir,
    } = prepare_vscode_msl_smoke_command(
        root,
        msl_root,
        Some(&summary_path),
        Some(&timing_path),
        options,
    )?;
    run_status_quiet(smoke)?;
    let raw = fs::read_to_string(&summary_path)
        .with_context(|| format!("failed to read {}", summary_path.display()))?;
    let summary = serde_json::from_str(&raw)
        .with_context(|| format!("failed to parse {}", summary_path.display()))?;
    run_vscode_failed_start_command_smoke(root, output_dir, options)?;
    Ok(summary)
}

fn prepare_vscode_msl_smoke_command(
    root: &Path,
    msl_root: &Path,
    summary_output_path: Option<&Path>,
    timing_output_path: Option<&Path>,
    options: VscodeSmokeOptions,
) -> Result<PreparedVscodeSmokeCommand> {
    let source_vscode_dir = resolve_vscode_dir(root)?;
    let smoke_stage = prepare_vscode_smoke_stage(root)?;
    let staged_vscode_dir = smoke_stage.path();
    let smoke_executable = resolve_vscode_smoke_executable(
        staged_vscode_dir,
        &source_vscode_dir.join(".vscode-test"),
    )?;

    let mut smoke = new_vscode_smoke_command("node", options)?;
    smoke
        .arg("tests/run_msl_extension_smoke.mjs")
        .arg("--msl-root")
        .arg(msl_root)
        .arg("--smoke-executable")
        .arg(&smoke_executable)
        .current_dir(staged_vscode_dir);
    if let Some(path) = summary_output_path {
        smoke
            .arg("--summary-out")
            .arg(path)
            .arg("--artifact-result")
            .arg(path);
    }
    if let Some(path) = timing_output_path {
        smoke.arg("--artifact-timings").arg(path);
    }
    Ok(PreparedVscodeSmokeCommand {
        command: smoke,
        _stage_dir: smoke_stage,
    })
}

fn run_vscode_failed_start_command_smoke(
    root: &Path,
    output_dir: &Path,
    options: VscodeSmokeOptions,
) -> Result<()> {
    fs::create_dir_all(output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;
    let summary_path = output_dir.join("vscode-failed-start-command-smoke-summary.json");
    let PreparedVscodeSmokeCommand {
        command: smoke,
        _stage_dir,
    } = prepare_vscode_failed_start_smoke_command(root, Some(&summary_path), options)?;
    run_status_quiet(smoke)?;
    let _ = fs::read_to_string(&summary_path)
        .with_context(|| format!("failed to read {}", summary_path.display()))?;
    Ok(())
}

fn prepare_vscode_failed_start_smoke_command(
    root: &Path,
    summary_output_path: Option<&Path>,
    options: VscodeSmokeOptions,
) -> Result<PreparedVscodeSmokeCommand> {
    let source_vscode_dir = resolve_vscode_dir(root)?;
    let smoke_stage = prepare_vscode_smoke_stage(root)?;
    let staged_vscode_dir = smoke_stage.path();
    let smoke_executable = resolve_vscode_smoke_executable(
        staged_vscode_dir,
        &source_vscode_dir.join(".vscode-test"),
    )?;

    let mut smoke = new_vscode_smoke_command("node", options)?;
    smoke
        .arg("tests/run_failed_start_command_smoke.mjs")
        .arg("--smoke-executable")
        .arg(&smoke_executable)
        .current_dir(staged_vscode_dir);
    if let Some(path) = summary_output_path {
        smoke.arg("--artifact-result").arg(path);
    }
    Ok(PreparedVscodeSmokeCommand {
        command: smoke,
        _stage_dir: smoke_stage,
    })
}

fn prepare_vscode_smoke_stage(root: &Path) -> Result<TempDir> {
    let source_vscode_dir = resolve_vscode_dir(root)?;
    let smoke_stage = stage_vscode_smoke_workspace(&source_vscode_dir)?;
    let staged_vscode_dir = smoke_stage.path();
    mirror_cached_vscode_smoke_install(&source_vscode_dir, staged_vscode_dir)?;

    // SPEC_0025: smoke verification must not mutate the live extension tree or
    // interfere with a concurrent local watch session under packages/vscode.
    build_and_stage_vscode_lsp(root, staged_vscode_dir, false)?;
    ensure_vscode_npm_dependencies(
        staged_vscode_dir,
        VscodeNpmDependencyMode::RefreshLocked,
        true,
        true,
    )?;
    prepare_vscode_web_assets(root)?;

    println!("Bundling VS Code extension for MSL smoke...");
    // The esbuild npm script vendors shared webview assets from the real repo,
    // but runs from this staged temp copy. The nested npm scripts make argv
    // forwarding unreliable, so hand the repo root over via a marker file the
    // vendor step reads (a libtest-config-style fixed-path channel, not an env
    // variable).
    let repo_root_marker = staged_vscode_dir.join(".rumoca-smoke-repo-root");
    fs::write(&repo_root_marker, root.to_string_lossy().as_bytes())
        .with_context(|| format!("failed to write {}", repo_root_marker.display()))?;
    let mut npm_esbuild = Command::new("npm");
    npm_esbuild
        .arg("run")
        .arg("esbuild")
        .current_dir(staged_vscode_dir);
    run_status_quiet(npm_esbuild)?;
    Ok(smoke_stage)
}

fn stage_vscode_smoke_workspace(source_vscode_dir: &Path) -> Result<TempDir> {
    let stage_dir = tempfile::Builder::new()
        .prefix("rumoca-vscode-smoke-stage-")
        .tempdir()
        .context("failed to create VS Code smoke staging dir")?;
    copy_vscode_smoke_workspace(source_vscode_dir, stage_dir.path())?;
    Ok(stage_dir)
}

fn mirror_cached_vscode_smoke_install(
    source_vscode_dir: &Path,
    staged_vscode_dir: &Path,
) -> Result<()> {
    let source_cache = source_vscode_dir.join(".vscode-test");
    if !source_cache.is_dir() {
        return Ok(());
    }

    let staged_cache = staged_vscode_dir.join(".vscode-test");
    fs::create_dir_all(&staged_cache)
        .with_context(|| format!("failed to create {}", staged_cache.display()))?;
    for entry in fs::read_dir(&source_cache)
        .with_context(|| format!("failed to read {}", source_cache.display()))?
    {
        let entry = entry.with_context(|| format!("failed to read {}", source_cache.display()))?;
        let file_name = entry.file_name();
        let Some(name) = file_name.to_str() else {
            continue;
        };
        if !name.starts_with("vscode-") {
            continue;
        }
        mirror_vscode_smoke_install_dir(&entry.path(), &staged_cache.join(file_name))?;
    }
    Ok(())
}

fn copy_vscode_smoke_workspace(source: &Path, destination: &Path) -> Result<()> {
    for entry in
        fs::read_dir(source).with_context(|| format!("failed to read {}", source.display()))?
    {
        let entry = entry.with_context(|| format!("failed to read {}", source.display()))?;
        let file_name = entry.file_name();
        if !should_copy_vscode_smoke_root_entry(&file_name) {
            continue;
        }
        copy_vscode_smoke_entry(&entry.path(), &destination.join(file_name))?;
    }
    Ok(())
}

fn should_copy_vscode_smoke_root_entry(file_name: &std::ffi::OsStr) -> bool {
    match file_name.to_str() {
        Some("bin" | "node_modules" | "out" | ".vscode-test") => false,
        Some(_) | None => true,
    }
}

fn copy_vscode_smoke_entry(source: &Path, destination: &Path) -> Result<()> {
    let file_type = fs::symlink_metadata(source)
        .with_context(|| format!("failed to stat {}", source.display()))?
        .file_type();
    if file_type.is_dir() {
        fs::create_dir_all(destination)
            .with_context(|| format!("failed to create {}", destination.display()))?;
        for entry in
            fs::read_dir(source).with_context(|| format!("failed to read {}", source.display()))?
        {
            let entry = entry.with_context(|| format!("failed to read {}", source.display()))?;
            copy_vscode_smoke_entry(&entry.path(), &destination.join(entry.file_name()))?;
        }
        return Ok(());
    }

    if let Some(parent) = destination.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::copy(source, destination).with_context(|| {
        format!(
            "failed to copy {} to {}",
            source.display(),
            destination.display()
        )
    })?;
    Ok(())
}

fn mirror_vscode_smoke_install_dir(source: &Path, destination: &Path) -> Result<()> {
    if destination.exists() {
        return Ok(());
    }
    if try_symlink_dir(source, destination).is_ok() {
        return Ok(());
    }
    copy_vscode_smoke_entry(source, destination)
}

fn try_symlink_dir(source: &Path, destination: &Path) -> std::io::Result<()> {
    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(source, destination)
    }
    #[cfg(windows)]
    {
        std::os::windows::fs::symlink_dir(source, destination)
    }
    #[cfg(not(any(unix, windows)))]
    {
        let _ = (source, destination);
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "directory symlinks unsupported on this platform",
        ))
    }
}

fn resolve_vscode_smoke_executable(vscode_dir: &Path, cache_path: &Path) -> Result<PathBuf> {
    // Cache the VS Code test runtime under packages/vscode/.vscode-test so every
    // smoke stage reuses the same verified download instead of hitting the CDN again.
    let mut resolve = Command::new("node");
    resolve
        .arg("tests/resolve_vscode_smoke_executable.mjs")
        .arg("--cache-path")
        .arg(cache_path)
        .current_dir(vscode_dir);
    let executable = run_capture(resolve)?.trim().to_string();
    ensure!(
        !executable.is_empty(),
        "failed to resolve VS Code smoke executable path"
    );
    Ok(PathBuf::from(executable))
}

fn prepare_vscode_web_assets(root: &Path) -> Result<()> {
    let vendor_dir = web_assets::build_web_vendor_assets(root)?;
    println!("Prepared VS Code webview assets: {}", vendor_dir.display());
    Ok(())
}

fn resolve_vscode_dir(root: &Path) -> Result<PathBuf> {
    let vscode_dir = root.join("packages/vscode");
    ensure!(
        vscode_dir.is_dir(),
        "missing VS Code extension dir: {}",
        vscode_dir.display()
    );
    Ok(vscode_dir)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VscodeSmokeLaunchMode {
    Direct,
    Xvfb,
}

fn new_vscode_smoke_command(program: &str, options: VscodeSmokeOptions) -> Result<Command> {
    let mut environment = current_vscode_smoke_environment();
    maybe_install_vscode_smoke_prereqs(&mut environment, options)?;
    match select_vscode_smoke_launch_mode(environment)? {
        VscodeSmokeLaunchMode::Direct => Ok(Command::new(program)),
        VscodeSmokeLaunchMode::Xvfb => {
            let mut cmd = Command::new("xvfb-run");
            cmd.arg("-a").arg(program);
            Ok(cmd)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct VscodeSmokeEnvironment {
    is_linux: bool,
    has_display: bool,
    has_xvfb_run: bool,
    has_xauth: bool,
}

fn current_vscode_smoke_environment() -> VscodeSmokeEnvironment {
    VscodeSmokeEnvironment {
        is_linux: cfg!(target_os = "linux"),
        has_display: std::env::var_os("DISPLAY").is_some(),
        has_xvfb_run: command_in_path("xvfb-run"),
        has_xauth: command_in_path("xauth"),
    }
}

fn maybe_install_vscode_smoke_prereqs(
    environment: &mut VscodeSmokeEnvironment,
    options: VscodeSmokeOptions,
) -> Result<()> {
    if !should_install_vscode_smoke_prereqs(*environment, options) {
        return Ok(());
    }
    println!("Installing headless VS Code smoke prerequisites.");
    install_ubuntu_vscode_smoke_prereqs()?;
    environment.has_xvfb_run = command_in_path("xvfb-run");
    environment.has_xauth = command_in_path("xauth");
    Ok(())
}

fn install_ubuntu_vscode_smoke_prereqs() -> Result<()> {
    ensure!(
        cfg!(target_os = "linux"),
        "automatic VS Code smoke prerequisite installation only supports Linux hosts"
    );
    ensure!(
        command_exists("apt-get"),
        "missing `apt-get`; install xvfb and xauth with your system package manager"
    );
    let mut update = if command_exists("sudo") {
        let mut command = Command::new("sudo");
        command.arg("apt-get");
        command
    } else {
        Command::new("apt-get")
    };
    update.arg("update");
    run_status(update)?;

    let mut install = if command_exists("sudo") {
        let mut command = Command::new("sudo");
        command.arg("apt-get");
        command
    } else {
        Command::new("apt-get")
    };
    install.args(["install", "-y", "xvfb", "xauth"]);
    run_status(install)
}

fn should_install_vscode_smoke_prereqs(
    environment: VscodeSmokeEnvironment,
    options: VscodeSmokeOptions,
) -> bool {
    environment.is_linux
        && options.install_prereqs
        && !(environment.has_xvfb_run && environment.has_xauth)
}

fn select_vscode_smoke_launch_mode(
    environment: VscodeSmokeEnvironment,
) -> Result<VscodeSmokeLaunchMode> {
    if !environment.is_linux {
        return Ok(VscodeSmokeLaunchMode::Direct);
    }

    if environment.has_xvfb_run && environment.has_xauth {
        return Ok(VscodeSmokeLaunchMode::Xvfb);
    }

    let missing = missing_headless_vscode_smoke_prereqs(environment);

    anyhow::bail!(
        "VS Code desktop smoke always runs under xvfb on Linux. Missing {}. Run `install xvfb/xauth`, pass `--install-prereqs`, or install xvfb/xauth manually with `sudo apt-get install -y xvfb xauth`.",
        missing.join(", ")
    );
}

fn missing_headless_vscode_smoke_prereqs(environment: VscodeSmokeEnvironment) -> Vec<&'static str> {
    let mut missing = Vec::new();
    if !environment.has_xvfb_run {
        missing.push("xvfb-run");
    }
    if !environment.has_xauth {
        missing.push("xauth");
    }
    missing
}

fn resolve_vscode_npm_install_plan(
    has_lockfile: bool,
    _node_modules_present: bool,
    _npm_toolchain_present: bool,
    _mode: VscodeNpmDependencyMode,
) -> VscodeNpmInstallPlan {
    if has_lockfile {
        VscodeNpmInstallPlan::Ci
    } else {
        VscodeNpmInstallPlan::Install
    }
}

fn ensure_vscode_npm_dependencies(
    vscode_dir: &Path,
    mode: VscodeNpmDependencyMode,
    ignore_scripts: bool,
    quiet: bool,
) -> Result<()> {
    let node_modules = vscode_dir.join("node_modules");
    let bin_dir = node_modules.join(".bin");
    let esbuild_bin = if cfg!(windows) {
        bin_dir.join("esbuild.cmd")
    } else {
        bin_dir.join("esbuild")
    };
    let eslint_bin = if cfg!(windows) {
        bin_dir.join("eslint.cmd")
    } else {
        bin_dir.join("eslint")
    };
    let has_lockfile = vscode_dir.join("package-lock.json").is_file();
    let plan = resolve_vscode_npm_install_plan(
        has_lockfile,
        node_modules.is_dir(),
        esbuild_bin.is_file() && eslint_bin.is_file(),
        mode,
    );

    match plan {
        VscodeNpmInstallPlan::Ci => {
            println!("Refreshing VS Code npm dependencies with npm ci...");
        }
        VscodeNpmInstallPlan::Install if node_modules.is_dir() => {
            println!(
                "Reinstalling npm dependencies (missing toolchain at {} or {})...",
                esbuild_bin.display(),
                eslint_bin.display()
            );
        }
        VscodeNpmInstallPlan::Install => {
            println!("Installing npm dependencies...");
        }
    }

    let mut npm_install = Command::new("npm");
    match plan {
        VscodeNpmInstallPlan::Ci => {
            npm_install.arg("ci");
        }
        VscodeNpmInstallPlan::Install => {
            npm_install.arg("install");
        }
    }
    if ignore_scripts {
        npm_install.arg("--ignore-scripts");
    }
    npm_install.current_dir(vscode_dir);
    let install_result = if quiet {
        run_status_quiet(npm_install)
    } else {
        run_status(npm_install)
    };
    if install_result.is_ok() {
        return Ok(());
    }
    if plan == VscodeNpmInstallPlan::Ci
        && node_modules.is_dir()
        && should_retry_vscode_npm_ci_after_clean(install_result.as_ref().err())
    {
        println!("npm ci left a dirty node_modules tree; clearing and retrying once...");
        fs::remove_dir_all(&node_modules)
            .with_context(|| format!("failed to remove {}", node_modules.display()))?;
        let mut retry = Command::new("npm");
        retry.arg("ci");
        if ignore_scripts {
            retry.arg("--ignore-scripts");
        }
        retry.current_dir(vscode_dir);
        return if quiet {
            run_status_quiet(retry)
        } else {
            run_status(retry)
        };
    }
    install_result
}

fn should_retry_vscode_npm_ci_after_clean(error: Option<&anyhow::Error>) -> bool {
    let Some(error) = error else {
        return false;
    };
    let message = error.to_string();
    message.contains("ENOTEMPTY") || message.contains("EBUSY")
}

fn build_and_stage_vscode_lsp(root: &Path, vscode_dir: &Path, release: bool) -> Result<()> {
    let profile_name = if release { "release" } else { "debug" };
    println!("Building rumoca-lsp ({profile_name})...");

    let mut cargo_build = Command::new("cargo");
    cargo_build
        .arg("build")
        .arg("--bin")
        .arg("rumoca-lsp")
        .arg("--bin")
        .arg("rumoca-lsp-galec")
        .arg("--bin")
        .arg("rumoca");
    if release {
        cargo_build.arg("--release");
    }
    cargo_build.current_dir(root);
    run_status(cargo_build)?;

    let bin_dir = vscode_dir.join("bin");
    fs::create_dir_all(&bin_dir)
        .with_context(|| format!("failed to create {}", bin_dir.display()))?;
    let stage_bin = |name: &str| -> Result<()> {
        let source = root.join("target").join(profile_name).join(exe_name(name));
        let target = bin_dir.join(exe_name(name));
        replace_staged_binary(&source, &target).with_context(|| {
            format!(
                "failed to copy {name} from {} to {}",
                source.display(),
                target.display()
            )
        })?;
        Ok(())
    };

    stage_bin("rumoca-lsp")?;
    stage_bin("rumoca-lsp-galec")?;
    stage_bin("rumoca")?;
    Ok(())
}

fn replace_staged_binary(source: &Path, target: &Path) -> Result<()> {
    let temp_target = staged_temp_path(target);
    fs::copy(source, &temp_target).with_context(|| {
        format!(
            "failed to copy staged binary from {} to {}",
            source.display(),
            temp_target.display()
        )
    })?;
    if let Err(error) = fs::rename(&temp_target, target) {
        #[cfg(windows)]
        {
            if target.exists() {
                fs::remove_file(target)
                    .with_context(|| format!("failed to remove {}", target.display()))?;
                fs::rename(&temp_target, target).with_context(|| {
                    format!(
                        "failed to replace staged binary {} with {}",
                        temp_target.display(),
                        target.display()
                    )
                })?;
                return Ok(());
            }
        }
        let _ = fs::remove_file(&temp_target);
        return Err(error).with_context(|| {
            format!(
                "failed to replace staged binary {} with {}",
                temp_target.display(),
                target.display()
            )
        });
    }
    Ok(())
}

fn staged_temp_path(target: &Path) -> PathBuf {
    let file_name = target
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("rumoca-stage");
    target.with_file_name(format!(".{file_name}.{}.tmp", std::process::id()))
}

fn command_available(program: &str) -> bool {
    Command::new(program)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

fn command_in_path(program: &str) -> bool {
    std::env::var_os("PATH").is_some_and(|paths| {
        std::env::split_paths(&paths).any(|dir| {
            let candidate = dir.join(program);
            if candidate.is_file() {
                return true;
            }
            cfg!(windows) && dir.join(format!("{program}.exe")).is_file()
        })
    })
}
