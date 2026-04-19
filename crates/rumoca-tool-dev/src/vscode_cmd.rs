use anyhow::{Context, Result, ensure};
use clap::ValueEnum;
use serde::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::{Duration, UNIX_EPOCH};
use tempfile::TempDir;
use walkdir::WalkDir;

use crate::{
    VscodeBuildArgs, VscodeHostArgs, VscodeInstallCheckArgs, VscodePackageArgs, exe_name,
    newest_prefixed_file, repo_cli_cmd, repo_root, run_capture, run_status, run_status_quiet,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VscodeNpmDependencyMode {
    IfMissing,
    RefreshLocked,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VscodeNpmInstallPlan {
    Skip,
    Ci,
    Install,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub(crate) enum VscodePackageTarget {
    LinuxX64,
    LinuxArm64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) struct VscodeSmokeOptions {
    pub(crate) install_prereqs: bool,
}

struct PreparedVscodeSmokeCommand {
    command: Command,
    _stage_dir: TempDir,
}

impl VscodePackageTarget {
    fn vsce_target(self) -> &'static str {
        match self {
            Self::LinuxX64 => "linux-x64",
            Self::LinuxArm64 => "linux-arm64",
        }
    }

    fn rust_target(self) -> &'static str {
        match self {
            Self::LinuxX64 => "x86_64-unknown-linux-musl",
            Self::LinuxArm64 => "aarch64-unknown-linux-musl",
        }
    }

    fn linker(self) -> &'static str {
        match self {
            Self::LinuxX64 | Self::LinuxArm64 => "musl-gcc",
        }
    }
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

pub(crate) fn build_vscode_ext(args: VscodeBuildArgs) -> Result<()> {
    let root = repo_root();
    let vsix = build_vscode_dev_vsix(&root, args.system)?;
    println!("Built VSIX: {}", vsix.display());

    if !args.no_install {
        println!("Installing extension in VSCode...");
        install_vscode_vsix(&vsix, None, None)?;
    }

    Ok(())
}

pub(crate) fn install_check_vscode_ext(args: VscodeInstallCheckArgs) -> Result<()> {
    let root = repo_root();
    let vscode_dir = resolve_vscode_dir(&root)?;
    ensure!(
        command_available("code"),
        "missing `code` CLI in PATH; install VS Code's Shell Command and retry"
    );

    let profile_root = resolve_install_check_profile_root(&root, args.profile_root.as_deref())?;
    let document = resolve_install_check_document(&root, args.document.as_deref())?;
    let smoke_workspace = prepare_install_check_workspace(&profile_root, &document)?;
    let user_data_dir = profile_root.join("user-data");
    let extensions_dir = profile_root.join("extensions");
    let artifacts_dir = profile_root.join("artifacts");
    fs::create_dir_all(&artifacts_dir)
        .with_context(|| format!("failed to create {}", artifacts_dir.display()))?;

    let vsix = if args.no_build {
        newest_prefixed_file(&vscode_dir, "rumoca-modelica-", "vsix")?.context(
            "failed to locate packaged VSCode extension (*.vsix); rerun without --no-build",
        )?
    } else {
        build_vscode_dev_vsix(&root, args.system)?
    };
    println!("Using VSIX: {}", vsix.display());

    println!(
        "Installing VSIX into isolated profile at {}",
        profile_root.display()
    );
    install_vscode_vsix(&vsix, Some(&user_data_dir), Some(&extensions_dir))?;

    println!("Running installed-extension smoke against packaged VSIX...");
    let summary_path = artifacts_dir.join("installed-extension-smoke-summary.json");
    run_installed_vscode_extension_smoke(
        &vscode_dir,
        &smoke_workspace,
        &document,
        &user_data_dir,
        &extensions_dir,
        &summary_path,
    )?;
    println!(
        "Installed-extension smoke passed: {}",
        summary_path.display()
    );

    if args.no_open {
        return Ok(());
    }

    println!(
        "Opening VS Code with the isolated install-check profile on {}...",
        document.display()
    );
    launch_vscode_install_check_profile(&document, &user_data_dir, &extensions_dir)
}

pub(crate) fn package_vscode_ext(args: VscodePackageArgs) -> Result<()> {
    let root = repo_root();
    let vscode_dir = resolve_vscode_dir(&root)?;

    ensure_vscode_npm_dependencies(
        &vscode_dir,
        VscodeNpmDependencyMode::IfMissing,
        false,
        false,
    )?;
    ensure_vscode_package_target_prereqs(args.target, args.install_musl_tools)?;
    build_vscode_release_binaries(&root, args.target)?;
    stage_vscode_release_binaries(&root, &vscode_dir, args.target)?;
    package_vscode_target(&vscode_dir, args.target)
}

pub(crate) fn run_vscode_ci(root: &Path) -> Result<()> {
    let vscode_dir = resolve_vscode_dir(root)?;
    // Keep the local and hosted gates aligned. We intentionally skip install scripts everywhere
    // because the esbuild postinstall validation fails under the Node 24 toolchain on this repo,
    // while the bundled binary still works for test/lint/bundle verification.
    ensure_vscode_npm_dependencies(
        &vscode_dir,
        VscodeNpmDependencyMode::RefreshLocked,
        true,
        false,
    )?;

    println!("Running VSCode extension tests...");
    let mut npm_test = Command::new("npm");
    npm_test.arg("test").current_dir(&vscode_dir);
    run_status(npm_test)?;

    println!("Running VSCode extension lint...");
    let mut npm_lint = Command::new("npm");
    npm_lint.arg("run").arg("lint").current_dir(&vscode_dir);
    run_status(npm_lint)?;

    println!("Bundling VSCode extension...");
    let mut npm_esbuild = Command::new("npm");
    npm_esbuild
        .arg("run")
        .arg("esbuild")
        .current_dir(&vscode_dir);
    run_status(npm_esbuild)
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
        .env("RUMOCA_VSCODE_MSL_ROOT", msl_root)
        .env("RUMOCA_VSCODE_SMOKE_EXECUTABLE", &smoke_executable)
        .current_dir(staged_vscode_dir);
    if let Some(path) = summary_output_path {
        smoke.env("RUMOCA_VSCODE_SMOKE_SUMMARY_OUT", path);
        smoke.env("RUMOCA_VSCODE_SMOKE_ARTIFACT_RESULT", path);
    }
    if let Some(path) = timing_output_path {
        smoke.env("RUMOCA_VSCODE_SMOKE_ARTIFACT_TIMINGS", path);
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

fn run_installed_vscode_extension_smoke(
    vscode_dir: &Path,
    workspace_file: &Path,
    document_path: &Path,
    user_data_dir: &Path,
    extensions_dir: &Path,
    summary_output_path: &Path,
) -> Result<()> {
    ensure_vscode_npm_dependencies(vscode_dir, VscodeNpmDependencyMode::IfMissing, false, false)?;
    let smoke_executable =
        resolve_vscode_smoke_executable(vscode_dir, &vscode_dir.join(".vscode-test"))?;
    let mut smoke = new_vscode_smoke_command("node", VscodeSmokeOptions::default())?;
    smoke
        .arg("tests/run_installed_extension_check.mjs")
        .env("RUMOCA_VSCODE_INSTALL_CHECK_WORKSPACE", workspace_file)
        .env("RUMOCA_VSCODE_INSTALL_CHECK_DOCUMENT", document_path)
        .env("RUMOCA_VSCODE_INSTALL_CHECK_USER_DATA_DIR", user_data_dir)
        .env("RUMOCA_VSCODE_INSTALL_CHECK_EXTENSIONS_DIR", extensions_dir)
        .env("RUMOCA_VSCODE_INSTALL_CHECK_RESULT", summary_output_path)
        .env("RUMOCA_VSCODE_SMOKE_EXECUTABLE", &smoke_executable)
        .current_dir(vscode_dir);
    run_status_quiet(smoke)
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
        .env("RUMOCA_VSCODE_SMOKE_EXECUTABLE", &smoke_executable)
        .current_dir(staged_vscode_dir);
    if let Some(path) = summary_output_path {
        smoke.env("RUMOCA_VSCODE_FAILED_START_ARTIFACT_RESULT", path);
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
    // interfere with a concurrent local watch session under editors/vscode.
    build_and_stage_vscode_lsp(root, staged_vscode_dir, false)?;
    ensure_vscode_npm_dependencies(
        staged_vscode_dir,
        VscodeNpmDependencyMode::RefreshLocked,
        true,
        true,
    )?;

    println!("Bundling VSCode extension for MSL smoke...");
    let mut npm_esbuild = Command::new("npm");
    npm_esbuild
        .arg("run")
        .arg("esbuild")
        .env("RUMOCA_REPO_ROOT", root)
        .current_dir(staged_vscode_dir);
    run_status_quiet(npm_esbuild)?;
    Ok(smoke_stage)
}

fn build_vscode_dev_vsix(root: &Path, system: bool) -> Result<PathBuf> {
    let vscode_dir = resolve_vscode_dir(root)?;

    if !system {
        build_and_stage_vscode_lsp(root, &vscode_dir, true)?;
    }

    ensure_vscode_npm_dependencies(
        &vscode_dir,
        VscodeNpmDependencyMode::IfMissing,
        false,
        false,
    )?;

    println!("Compiling extension TypeScript...");
    let mut npm_esbuild = Command::new("npm");
    npm_esbuild
        .arg("run")
        .arg("esbuild")
        .current_dir(&vscode_dir);
    run_status(npm_esbuild)?;

    println!("Packaging extension...");
    let mut npm_package = Command::new("npm");
    npm_package
        .arg("run")
        .arg("package")
        .current_dir(&vscode_dir);
    run_status(npm_package)?;

    newest_prefixed_file(&vscode_dir, "rumoca-modelica-", "vsix")?
        .context("failed to locate packaged VSCode extension (*.vsix)")
}

fn install_vscode_vsix(
    vsix: &Path,
    user_data_dir: Option<&Path>,
    extensions_dir: Option<&Path>,
) -> Result<()> {
    let mut code = Command::new("code");
    if let Some(dir) = user_data_dir {
        fs::create_dir_all(dir).with_context(|| format!("failed to create {}", dir.display()))?;
        code.arg("--user-data-dir").arg(dir);
    }
    if let Some(dir) = extensions_dir {
        fs::create_dir_all(dir).with_context(|| format!("failed to create {}", dir.display()))?;
        code.arg("--extensions-dir").arg(dir);
    }
    code.arg("--install-extension").arg(vsix).arg("--force");
    run_status(code)
}

fn resolve_install_check_profile_root(root: &Path, requested: Option<&Path>) -> Result<PathBuf> {
    let profile_root = match requested {
        Some(path) if path.is_absolute() => path.to_path_buf(),
        Some(path) => root.join(path),
        None => root.join("target").join("vscode-install-check"),
    };
    if profile_root.exists() {
        fs::remove_dir_all(&profile_root)
            .with_context(|| format!("failed to remove {}", profile_root.display()))?;
    }
    fs::create_dir_all(&profile_root)
        .with_context(|| format!("failed to create {}", profile_root.display()))?;
    Ok(profile_root)
}

fn resolve_install_check_document(root: &Path, requested: Option<&Path>) -> Result<PathBuf> {
    let candidate = match requested {
        Some(path) if path.is_absolute() => path.to_path_buf(),
        Some(path) => root.join(path),
        None => root.join("examples").join("Ball.mo"),
    };
    let resolved = candidate
        .canonicalize()
        .unwrap_or_else(|_| candidate.clone());
    ensure!(
        resolved.is_file(),
        "install-check document does not exist or is not a file: {}",
        resolved.display()
    );
    ensure!(
        resolved.extension().and_then(|ext| ext.to_str()) == Some("mo"),
        "install-check document must be a .mo file: {}",
        resolved.display()
    );
    Ok(resolved)
}

fn prepare_install_check_workspace(profile_root: &Path, document: &Path) -> Result<PathBuf> {
    let workspace_dir = profile_root.join("workspace");
    fs::create_dir_all(&workspace_dir)
        .with_context(|| format!("failed to create {}", workspace_dir.display()))?;
    let workspace_file = workspace_dir.join("install-check.code-workspace");
    let workspace_folder = document.parent().with_context(|| {
        format!(
            "install-check document has no parent directory: {}",
            document.display()
        )
    })?;
    let missing_server_path = workspace_dir.join(exe_name("missing-rumoca-lsp"));
    let workspace = serde_json::json!({
        "folders": [{ "path": workspace_folder }],
        "settings": {
            "rumoca.debug": false,
            "rumoca.serverPath": missing_server_path,
            "rumoca.sourceRootPaths": [],
        }
    });
    fs::write(
        &workspace_file,
        serde_json::to_string_pretty(&workspace)
            .context("failed to serialize install-check workspace")?,
    )
    .with_context(|| format!("failed to write {}", workspace_file.display()))?;
    Ok(workspace_file)
}

fn launch_vscode_install_check_profile(
    document_file: &Path,
    user_data_dir: &Path,
    extensions_dir: &Path,
) -> Result<()> {
    let mut code = Command::new("code");
    code.arg("--user-data-dir")
        .arg(user_data_dir)
        .arg("--extensions-dir")
        .arg(extensions_dir)
        .arg("--new-window")
        .arg("--disable-workspace-trust")
        .arg("--wait")
        .arg(document_file);
    run_status(code)
}

fn stage_vscode_smoke_workspace(source_vscode_dir: &Path) -> Result<TempDir> {
    let stage_dir = tempfile::Builder::new()
        .prefix("rumoca-vscode-smoke-stage-")
        .tempdir()
        .context("failed to create VSCode smoke staging dir")?;
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
    // Cache the VS Code test runtime under editors/vscode/.vscode-test so every
    // smoke stage reuses the same verified download instead of hitting the CDN again.
    let mut resolve = Command::new("node");
    resolve
        .arg("tests/resolve_vscode_smoke_executable.mjs")
        .env("RUMOCA_VSCODE_SMOKE_CACHE_PATH", cache_path)
        .current_dir(vscode_dir);
    let executable = run_capture(resolve)?.trim().to_string();
    ensure!(
        !executable.is_empty(),
        "failed to resolve VS Code smoke executable path"
    );
    Ok(PathBuf::from(executable))
}

pub(crate) fn vscode_dev(args: VscodeHostArgs) -> Result<()> {
    let root = repo_root();
    let vscode_dir = resolve_vscode_dir(&root)?;
    let workspace_dir = resolve_workspace_dir(&root, args.workspace_dir.as_deref())?;

    if !args.skip_lsp_build {
        build_and_stage_vscode_lsp(&root, &vscode_dir, false)?;
    }
    ensure_vscode_npm_dependencies(
        &vscode_dir,
        VscodeNpmDependencyMode::IfMissing,
        false,
        false,
    )?;

    let mut rust_watch_stop: Option<Arc<AtomicBool>> = None;
    let mut rust_watch_handle: Option<thread::JoinHandle<()>> = None;
    if !args.skip_lsp_build {
        let stop = Arc::new(AtomicBool::new(false));
        rust_watch_handle = Some(spawn_rust_lsp_watch_loop(
            root.clone(),
            vscode_dir.clone(),
            stop.clone(),
        ));
        rust_watch_stop = Some(stop);
    }

    let mut ts_watch: Option<Child> = None;
    if !args.no_ts_watch {
        println!("Starting TypeScript watch (esbuild --watch=forever)...");
        let mut watch_cmd = Command::new("npm");
        watch_cmd
            .arg("run")
            .arg("esbuild-base")
            .arg("--")
            .arg("--sourcemap")
            .arg("--watch=forever")
            .current_dir(&vscode_dir)
            .stdin(Stdio::null())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit());
        let child = watch_cmd
            .spawn()
            .with_context(|| "failed to start TypeScript watch process".to_string())?;
        println!("TypeScript watch pid={}", child.id());
        ts_watch = Some(child);
    }

    let launch_result = launch_vscode_extension_host(&vscode_dir, &workspace_dir);

    if let Some(stop) = rust_watch_stop {
        stop.store(true, Ordering::Relaxed);
    }
    if let Some(handle) = rust_watch_handle {
        let _ = handle.join();
    }

    if let Some(mut child) = ts_watch {
        match child.try_wait() {
            Ok(Some(_)) => {}
            Ok(None) => {
                let _ = child.kill();
                let _ = child.wait();
            }
            Err(_) => {}
        }
    }

    launch_result
}

fn spawn_rust_lsp_watch_loop(
    root: PathBuf,
    vscode_dir: PathBuf,
    stop: Arc<AtomicBool>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let mut last_fingerprint = match rust_watch_fingerprint(&root) {
            Ok(value) => value,
            Err(error) => {
                eprintln!("[rum vscode edit] failed to initialize Rust watcher: {error:#}");
                0
            }
        };
        while !stop.load(Ordering::Relaxed) {
            thread::sleep(Duration::from_millis(800));
            let current_fingerprint = match rust_watch_fingerprint(&root) {
                Ok(value) => value,
                Err(error) => {
                    eprintln!("[rum vscode edit] Rust watcher scan failed: {error:#}");
                    continue;
                }
            };
            if current_fingerprint == last_fingerprint {
                continue;
            }
            last_fingerprint = current_fingerprint;
            println!("[rum vscode edit] Rust change detected; rebuilding rumoca-lsp...");
            match build_and_stage_vscode_lsp(&root, &vscode_dir, false) {
                Ok(()) => println!("[rum vscode edit] rumoca-lsp rebuild complete."),
                Err(error) => {
                    eprintln!("[rum vscode edit] rumoca-lsp rebuild failed: {error:#}")
                }
            }
        }
    })
}

fn rust_watch_fingerprint(root: &Path) -> Result<u64> {
    let mut hasher = DefaultHasher::new();
    for entry in WalkDir::new(root)
        .follow_links(false)
        .into_iter()
        .filter_entry(|entry| rust_watch_descend(entry.path()))
    {
        let entry = match entry {
            Ok(entry) => entry,
            Err(_) => continue,
        };
        if !entry.file_type().is_file() || !is_rust_watch_file(entry.path()) {
            continue;
        }
        let path = entry.path();
        path.strip_prefix(root).unwrap_or(path).hash(&mut hasher);
        if let Ok(metadata) = entry.metadata() {
            metadata.len().hash(&mut hasher);
            if let Ok(modified) = metadata.modified()
                && let Ok(duration) = modified.duration_since(UNIX_EPOCH)
            {
                duration.as_secs().hash(&mut hasher);
                duration.subsec_nanos().hash(&mut hasher);
            }
        }
    }
    Ok(hasher.finish())
}

fn rust_watch_descend(path: &Path) -> bool {
    if !path.is_dir() {
        return true;
    }
    let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
        return true;
    };
    !matches!(
        name,
        ".git" | "target" | "node_modules" | ".venv" | ".rumoca" | ".vscode" | ".idea"
    )
}

fn is_rust_watch_file(path: &Path) -> bool {
    if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
        return true;
    }
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some("Cargo.toml")
            | Some("Cargo.lock")
            | Some("rust-toolchain.toml")
            | Some("rustfmt.toml")
            | Some("clippy.toml")
    )
}

fn resolve_vscode_dir(root: &Path) -> Result<PathBuf> {
    let vscode_dir = root.join("editors/vscode");
    ensure!(
        vscode_dir.is_dir(),
        "missing VSCode extension dir: {}",
        vscode_dir.display()
    );
    Ok(vscode_dir)
}

fn resolve_workspace_dir(root: &Path, requested: Option<&Path>) -> Result<PathBuf> {
    let candidate = match requested {
        Some(path) if path.is_absolute() => path.to_path_buf(),
        Some(path) => root.join(path),
        None => root.to_path_buf(),
    };
    let resolved = candidate
        .canonicalize()
        .unwrap_or_else(|_| candidate.clone());
    ensure!(
        resolved.is_dir(),
        "workspace directory does not exist or is not a directory: {}",
        resolved.display()
    );
    Ok(resolved)
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
    repo_cli_cmd::install_ubuntu_vscode_smoke_prereqs(false)?;
    environment.has_xvfb_run = command_in_path("xvfb-run");
    environment.has_xauth = command_in_path("xauth");
    Ok(())
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
        "VS Code desktop smoke always runs under xvfb on Linux. Missing {}. Run `rum repo ubuntu install-vscode-smoke-prereqs`, pass `--install-prereqs`, or install xvfb/xauth manually with `sudo apt-get install -y xvfb xauth`.",
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
    node_modules_present: bool,
    npm_toolchain_present: bool,
    mode: VscodeNpmDependencyMode,
) -> VscodeNpmInstallPlan {
    match mode {
        VscodeNpmDependencyMode::IfMissing if node_modules_present && npm_toolchain_present => {
            VscodeNpmInstallPlan::Skip
        }
        VscodeNpmDependencyMode::IfMissing | VscodeNpmDependencyMode::RefreshLocked
            if has_lockfile =>
        {
            VscodeNpmInstallPlan::Ci
        }
        VscodeNpmDependencyMode::IfMissing | VscodeNpmDependencyMode::RefreshLocked => {
            VscodeNpmInstallPlan::Install
        }
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
        VscodeNpmInstallPlan::Skip => return Ok(()),
        VscodeNpmInstallPlan::Ci => {
            println!("Refreshing VSCode npm dependencies with npm ci...");
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
        VscodeNpmInstallPlan::Skip => {}
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

fn launch_vscode_extension_host(vscode_dir: &Path, workspace_dir: &Path) -> Result<()> {
    println!("Launching VSCode extension development mode...");
    let mut code = Command::new("code");
    code.arg(format!(
        "--extensionDevelopmentPath={}",
        vscode_dir.display()
    ))
    .arg("--wait")
    .arg(workspace_dir)
    .current_dir(workspace_dir);
    run_status(code)
}

fn ensure_vscode_package_target_prereqs(
    target: VscodePackageTarget,
    install_musl_tools: bool,
) -> Result<()> {
    ensure!(
        cfg!(target_os = "linux"),
        "rum vscode package currently supports Linux hosts only"
    );

    let mut rustup_target = Command::new("rustup");
    rustup_target
        .arg("target")
        .arg("add")
        .arg(target.rust_target());
    run_status(rustup_target)?;

    if command_available(target.linker()) {
        return Ok(());
    }

    if install_musl_tools {
        let mut apt_update = Command::new("sudo");
        apt_update.arg("apt-get").arg("update");
        run_status(apt_update)?;

        let mut apt_install = Command::new("sudo");
        apt_install
            .arg("apt-get")
            .arg("install")
            .arg("-y")
            .arg("musl-tools");
        run_status(apt_install)?;
    }

    ensure!(
        command_available(target.linker()),
        "missing {} for {}. Install musl-tools or rerun `rum vscode package --target {} --install-musl-tools`",
        target.linker(),
        target.rust_target(),
        target.vsce_target()
    );

    Ok(())
}

fn build_vscode_release_binaries(root: &Path, target: VscodePackageTarget) -> Result<()> {
    println!(
        "Building bundled VSCode binaries for {} ({})...",
        target.vsce_target(),
        target.rust_target()
    );

    let mut cargo_build = Command::new("cargo");
    cargo_build
        .arg("build")
        .arg("--release")
        .arg("--target")
        .arg(target.rust_target())
        .arg("--bin")
        .arg("rumoca-lsp")
        .arg("--bin")
        .arg("rumoca")
        .current_dir(root)
        .env(
            format!("CC_{}", cargo_target_cc_env_suffix(target.rust_target())),
            target.linker(),
        )
        .env(
            format!(
                "CARGO_TARGET_{}_LINKER",
                cargo_target_linker_env_suffix(target.rust_target())
            ),
            target.linker(),
        );
    run_status(cargo_build)
}

fn stage_vscode_release_binaries(
    root: &Path,
    vscode_dir: &Path,
    target: VscodePackageTarget,
) -> Result<()> {
    let bin_dir = vscode_dir.join("bin");
    fs::create_dir_all(&bin_dir)
        .with_context(|| format!("failed to create {}", bin_dir.display()))?;

    for entry in
        fs::read_dir(&bin_dir).with_context(|| format!("failed to read {}", bin_dir.display()))?
    {
        let entry = entry.with_context(|| format!("failed to inspect {}", bin_dir.display()))?;
        if entry
            .file_type()
            .map(|kind| kind.is_file())
            .unwrap_or(false)
        {
            fs::remove_file(entry.path())
                .with_context(|| format!("failed to remove {}", entry.path().display()))?;
        }
    }

    for vsix in fs::read_dir(vscode_dir)
        .with_context(|| format!("failed to read {}", vscode_dir.display()))?
    {
        let vsix = vsix.with_context(|| format!("failed to inspect {}", vscode_dir.display()))?;
        let path = vsix.path();
        if path.extension().and_then(|ext| ext.to_str()) == Some("vsix") {
            fs::remove_file(&path)
                .with_context(|| format!("failed to remove {}", path.display()))?;
        }
    }

    let release_dir = root
        .join("target")
        .join(target.rust_target())
        .join("release");
    stage_named_binary(&release_dir, &bin_dir, "rumoca-lsp", "rumoca-lsp")?;
    stage_named_binary(&release_dir, &bin_dir, "rumoca", "rumoca")?;
    Ok(())
}

fn stage_named_binary(
    release_dir: &Path,
    bin_dir: &Path,
    source_name: &str,
    target_name: &str,
) -> Result<()> {
    let source = release_dir.join(exe_name(source_name));
    let target = bin_dir.join(exe_name(target_name));
    ensure!(
        source.is_file(),
        "missing bundled binary: {}",
        source.display()
    );
    replace_staged_binary(&source, &target).with_context(|| {
        format!(
            "failed to copy bundled binary from {} to {}",
            source.display(),
            target.display()
        )
    })?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = fs::metadata(&target)
            .with_context(|| format!("failed to stat {}", target.display()))?;
        let mut perms = metadata.permissions();
        perms.set_mode(perms.mode() | 0o111);
        fs::set_permissions(&target, perms)
            .with_context(|| format!("failed to chmod +x {}", target.display()))?;
    }
    Ok(())
}

fn package_vscode_target(vscode_dir: &Path, target: VscodePackageTarget) -> Result<()> {
    println!("Bundling VSCode extension for {}...", target.vsce_target());

    let mut npm_esbuild = Command::new("npm");
    npm_esbuild
        .arg("run")
        .arg("esbuild-base")
        .arg("--")
        .arg("--minify")
        .current_dir(vscode_dir);
    run_status(npm_esbuild)?;

    let mut vsce_package = Command::new("npx");
    vsce_package
        .arg("@vscode/vsce")
        .arg("package")
        .arg("--target")
        .arg(target.vsce_target())
        .arg("--no-dependencies")
        .arg("--no-yarn")
        .current_dir(vscode_dir);
    run_status(vsce_package)?;

    let vsix = newest_prefixed_file(vscode_dir, "rumoca-modelica-", "vsix")?
        .context("failed to locate packaged VSCode extension (*.vsix)")?;
    println!("Built VSIX: {}", vsix.display());
    Ok(())
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

fn cargo_target_linker_env_suffix(target: &str) -> String {
    target.to_ascii_uppercase().replace('-', "_")
}

fn cargo_target_cc_env_suffix(target: &str) -> String {
    target.replace('-', "_")
}

#[cfg(test)]
mod tests {
    use super::{
        VscodeMslSmokeSummary, VscodeNpmDependencyMode, VscodeNpmInstallPlan, VscodePackageTarget,
        VscodeSmokeEnvironment, VscodeSmokeLaunchMode,
        cargo_target_cc_env_suffix, cargo_target_linker_env_suffix,
        mirror_cached_vscode_smoke_install, prepare_install_check_workspace, replace_staged_binary,
        resolve_install_check_document, resolve_install_check_profile_root,
        resolve_vscode_npm_install_plan, select_vscode_smoke_launch_mode,
        should_copy_vscode_smoke_root_entry, should_retry_vscode_npm_ci_after_clean,
        stage_vscode_smoke_workspace,
    };
    #[cfg(target_os = "linux")]
    use super::{VscodeSmokeOptions, should_install_vscode_smoke_prereqs};
    use anyhow::anyhow;
    use serde_json::json;

    fn smoke_environment(
        is_linux: bool,
        has_display: bool,
        has_xvfb_run: bool,
        has_xauth: bool,
    ) -> VscodeSmokeEnvironment {
        VscodeSmokeEnvironment {
            is_linux,
            has_display,
            has_xvfb_run,
            has_xauth,
        }
    }

    #[test]
    fn if_missing_mode_skips_when_toolchain_is_present() {
        let plan =
            resolve_vscode_npm_install_plan(true, true, true, VscodeNpmDependencyMode::IfMissing);
        assert_eq!(plan, VscodeNpmInstallPlan::Skip);
    }

    #[test]
    fn if_missing_mode_runs_npm_ci_when_lockfile_exists_but_toolchain_missing() {
        let plan =
            resolve_vscode_npm_install_plan(true, true, false, VscodeNpmDependencyMode::IfMissing);
        assert_eq!(plan, VscodeNpmInstallPlan::Ci);
    }

    #[test]
    fn refresh_locked_mode_forces_npm_ci_with_lockfile() {
        let plan = resolve_vscode_npm_install_plan(
            true,
            true,
            true,
            VscodeNpmDependencyMode::RefreshLocked,
        );
        assert_eq!(plan, VscodeNpmInstallPlan::Ci);
    }

    #[test]
    fn refresh_locked_mode_uses_npm_install_without_lockfile() {
        let plan = resolve_vscode_npm_install_plan(
            false,
            true,
            true,
            VscodeNpmDependencyMode::RefreshLocked,
        );
        assert_eq!(plan, VscodeNpmInstallPlan::Install);
    }

    #[test]
    fn npm_ci_retry_detector_recovers_from_directory_busy_errors() {
        assert!(should_retry_vscode_npm_ci_after_clean(Some(&anyhow!(
            "npm error ENOTEMPTY: directory not empty"
        ))));
        assert!(should_retry_vscode_npm_ci_after_clean(Some(&anyhow!(
            "npm error EBUSY: resource busy or locked"
        ))));
        assert!(!should_retry_vscode_npm_ci_after_clean(Some(&anyhow!(
            "npm error EACCES: permission denied"
        ))));
        assert!(!should_retry_vscode_npm_ci_after_clean(None));
    }

    #[test]
    fn vscode_smoke_staging_skips_mutable_build_artifacts() {
        let source = tempfile::tempdir().expect("source tempdir");
        let staged = source.path().join("editors").join("vscode");
        std::fs::create_dir_all(&staged).expect("create source tree");
        std::fs::write(staged.join("package.json"), "{}").expect("write package");
        std::fs::create_dir_all(staged.join("src")).expect("create src");
        std::fs::write(staged.join("src/extension.ts"), "export {}").expect("write source");
        std::fs::create_dir_all(staged.join("node_modules")).expect("create node_modules");
        std::fs::write(staged.join("node_modules/keep.txt"), "ignore").expect("write dep");
        std::fs::create_dir_all(staged.join("out")).expect("create out");
        std::fs::write(staged.join("out/extension.js"), "ignore").expect("write out");
        std::fs::create_dir_all(staged.join("bin")).expect("create bin");
        std::fs::write(staged.join("bin/rumoca-lsp"), "ignore").expect("write bin");
        std::fs::create_dir_all(staged.join(".vscode-test")).expect("create cache");
        std::fs::write(staged.join(".vscode-test/code"), "ignore").expect("write cache");

        let temp = stage_vscode_smoke_workspace(&staged).expect("stage workspace");
        let copied = temp.path();

        assert!(copied.join("package.json").is_file());
        assert!(copied.join("src/extension.ts").is_file());
        assert!(!copied.join("node_modules").exists());
        assert!(!copied.join("out").exists());
        assert!(!copied.join("bin").exists());
        assert!(!copied.join(".vscode-test").exists());
    }

    #[test]
    fn vscode_smoke_staging_filter_excludes_live_dependency_dirs() {
        assert!(should_copy_vscode_smoke_root_entry(std::ffi::OsStr::new(
            "src"
        )));
        assert!(!should_copy_vscode_smoke_root_entry(std::ffi::OsStr::new(
            "node_modules"
        )));
        assert!(!should_copy_vscode_smoke_root_entry(std::ffi::OsStr::new(
            "out"
        )));
        assert!(!should_copy_vscode_smoke_root_entry(std::ffi::OsStr::new(
            "bin"
        )));
        assert!(!should_copy_vscode_smoke_root_entry(std::ffi::OsStr::new(
            ".vscode-test"
        )));
    }

    #[test]
    fn mirror_cached_vscode_smoke_install_links_only_downloaded_editor() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source_vscode_dir = temp.path().join("source");
        let staged_vscode_dir = temp.path().join("stage");
        let cached_dir = source_vscode_dir
            .join(".vscode-test")
            .join("vscode-linux-x64-1.111.0");
        std::fs::create_dir_all(&cached_dir).expect("create cached dir");
        std::fs::write(cached_dir.join("code"), "").expect("write cached executable");
        std::fs::create_dir_all(source_vscode_dir.join(".vscode-test").join("user-data"))
            .expect("create user-data");
        std::fs::create_dir_all(&staged_vscode_dir).expect("create staged dir");

        mirror_cached_vscode_smoke_install(&source_vscode_dir, &staged_vscode_dir)
            .expect("mirror cached install");

        assert!(
            staged_vscode_dir
                .join(".vscode-test")
                .join("vscode-linux-x64-1.111.0")
                .exists()
        );
        assert!(
            !staged_vscode_dir
                .join(".vscode-test")
                .join("user-data")
                .exists()
        );
    }

    #[test]
    fn vscode_package_target_linux_x64_maps_to_expected_release_targets() {
        assert_eq!(VscodePackageTarget::LinuxX64.vsce_target(), "linux-x64");
        assert_eq!(
            VscodePackageTarget::LinuxX64.rust_target(),
            "x86_64-unknown-linux-musl"
        );
    }

    #[test]
    fn vscode_package_target_linux_arm64_maps_to_expected_release_targets() {
        assert_eq!(VscodePackageTarget::LinuxArm64.vsce_target(), "linux-arm64");
        assert_eq!(
            VscodePackageTarget::LinuxArm64.rust_target(),
            "aarch64-unknown-linux-musl"
        );
    }

    #[test]
    fn replace_staged_binary_overwrites_existing_target() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source = temp.path().join("source-bin");
        let target = temp.path().join("target-bin");
        std::fs::write(&source, b"new").expect("write source");
        std::fs::write(&target, b"old").expect("write target");

        replace_staged_binary(&source, &target).expect("replace staged binary");

        let contents = std::fs::read(&target).expect("read target");
        assert_eq!(contents, b"new");
    }

    #[test]
    fn vscode_smoke_summary_parses_snake_case_cache_delta() {
        let summary: VscodeMslSmokeSummary = serde_json::from_value(json!({
            "warmStageTimings": {
                "sourceRootLoadMs": 38,
                "completionSourceRootLoadMs": 37,
                "resolvedBuildMs": null,
                "completionHandlerMs": 0,
                "totalMs": 20,
                "sessionCacheDelta": {
                    "namespace_completion_cache_hits": 1,
                    "namespace_completion_cache_misses": 0,
                    "source_root_files_parsed": 0
                }
            }
        }))
        .expect("summary should parse");

        let warm = summary
            .warm_stage_timings
            .expect("warm stage timings should be present");
        assert_eq!(warm.source_root_load_ms, Some(38));
        assert_eq!(warm.completion_source_root_load_ms, Some(37));
        assert_eq!(warm.completion_handler_ms, Some(0));
        let delta = warm
            .session_cache_delta
            .expect("session cache delta should be present");
        assert_eq!(delta.namespace_completion_cache_hits, Some(1));
        assert_eq!(delta.namespace_completion_cache_misses, Some(0));
        assert_eq!(delta.source_root_files_parsed, Some(0));
    }

    #[test]
    fn cargo_target_env_suffixes_match_cargo_conventions() {
        assert_eq!(
            cargo_target_cc_env_suffix("x86_64-unknown-linux-musl"),
            "x86_64_unknown_linux_musl"
        );
        assert_eq!(
            cargo_target_linker_env_suffix("x86_64-unknown-linux-musl"),
            "X86_64_UNKNOWN_LINUX_MUSL"
        );
    }

    #[test]
    fn install_check_profile_root_defaults_under_target() {
        let temp = tempfile::tempdir().expect("tempdir");
        let profile_root =
            resolve_install_check_profile_root(temp.path(), None).expect("resolve profile root");
        assert_eq!(
            profile_root,
            temp.path().join("target/vscode-install-check")
        );
        assert!(profile_root.is_dir());
    }

    #[test]
    fn install_check_document_defaults_to_ball_example() {
        let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("workspace root");
        let document =
            resolve_install_check_document(&root, None).expect("resolve default install document");
        assert!(document.ends_with("examples/Ball.mo"));
        assert!(document.is_file());
    }

    #[test]
    fn install_check_workspace_writes_missing_server_override() {
        let temp = tempfile::tempdir().expect("tempdir");
        let document = temp.path().join("Example.mo");
        std::fs::write(&document, "model Example end Example;").expect("write model file");

        let workspace =
            prepare_install_check_workspace(temp.path(), &document).expect("prepare workspace");
        let raw = std::fs::read_to_string(&workspace).expect("read workspace file");
        let value: serde_json::Value = serde_json::from_str(&raw).expect("parse workspace json");
        assert_eq!(
            value
                .get("folders")
                .and_then(|folders| folders.get(0))
                .and_then(|folder| folder.get("path"))
                .and_then(serde_json::Value::as_str),
            document.parent().and_then(|path| path.to_str())
        );
        let server_path = value
            .get("settings")
            .and_then(|settings| settings.get("rumoca.serverPath"))
            .and_then(serde_json::Value::as_str)
            .expect("missing server path override");
        let expected_name = if cfg!(windows) {
            "missing-rumoca-lsp.exe"
        } else {
            "missing-rumoca-lsp"
        };
        assert!(server_path.ends_with(expected_name));
    }

    #[test]
    fn vscode_smoke_uses_xvfb_on_linux_even_with_display() {
        let mode = select_vscode_smoke_launch_mode(smoke_environment(true, true, true, true))
            .expect("Linux smoke should always use xvfb");
        assert_eq!(mode, VscodeSmokeLaunchMode::Xvfb);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn vscode_smoke_uses_xvfb_when_linux_has_no_display() {
        let mode = select_vscode_smoke_launch_mode(smoke_environment(true, false, true, true))
            .expect("xvfb should satisfy Linux headless smoke");
        assert_eq!(mode, VscodeSmokeLaunchMode::Xvfb);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn vscode_smoke_errors_when_linux_has_no_display_or_xauth() {
        let error = select_vscode_smoke_launch_mode(smoke_environment(true, false, true, false))
            .expect_err("Linux headless smoke should fail without xvfb");
        assert!(
            error.to_string().contains("Missing xauth"),
            "unexpected error: {error:#}"
        );
    }

    #[cfg(not(target_os = "linux"))]
    #[test]
    fn vscode_smoke_runs_direct_off_linux_without_display() {
        let mode = select_vscode_smoke_launch_mode(smoke_environment(false, false, false, false))
            .expect("non-Linux smoke should launch directly");
        assert_eq!(mode, VscodeSmokeLaunchMode::Direct);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn install_prereqs_flag_only_applies_to_linux_missing_tools() {
        assert!(should_install_vscode_smoke_prereqs(
            smoke_environment(true, true, true, false),
            VscodeSmokeOptions {
                install_prereqs: true
            }
        ));
        assert!(!should_install_vscode_smoke_prereqs(
            smoke_environment(true, false, true, true),
            VscodeSmokeOptions {
                install_prereqs: true
            }
        ));
        assert!(should_install_vscode_smoke_prereqs(
            smoke_environment(true, true, false, false),
            VscodeSmokeOptions {
                install_prereqs: true
            }
        ));
        assert!(!should_install_vscode_smoke_prereqs(
            smoke_environment(true, false, false, false),
            VscodeSmokeOptions {
                install_prereqs: false
            }
        ));
    }
}
