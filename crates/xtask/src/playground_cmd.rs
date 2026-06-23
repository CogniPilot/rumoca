use anyhow::{Context, Result, bail};
use std::fs;
use std::path::Path;
use std::process::Command;
use xtask::web_assets;

use crate::{
    WasmBuildProfile, WasmVariant, build_wasm, ensure_wasm_deps, run_status, wasm_build_subdir_name,
};

pub(crate) fn stage_playground_vendor_assets(root: &Path) -> Result<()> {
    let source = web_assets::build_web_vendor_assets(root)?;
    let target = root.join("packages/playground/vendor");
    fs::remove_dir_all(&target).or_else(|error| {
        if error.kind() == std::io::ErrorKind::NotFound {
            Ok(())
        } else {
            Err(error)
        }
    })?;
    copy_dir_recursive(&source, &target)
}

pub(crate) fn run_playground_test_suite(root: &Path) -> Result<()> {
    let mut wasm_tests = Command::new("cargo");
    wasm_tests
        .arg("test")
        .arg("-p")
        .arg("rumoca-bind-wasm")
        .arg("--all-features")
        .arg("--verbose")
        .current_dir(root);
    run_status(wasm_tests)?;

    run_playground_smoke_check(root)
}

pub(crate) fn run_playground_smoke_check(root: &Path) -> Result<()> {
    stage_playground_vendor_assets(root)?;
    check_playground_js_syntax(root)?;
    check_playground_expected_sources(root)?;
    run_single_threaded_wasm_smoke(root)
}

fn check_playground_js_syntax(root: &Path) -> Result<()> {
    let js_checks = [
        "packages/playground/src/main.js",
        "packages/playground/src/modules/command_palette.js",
        "packages/playground/src/modules/default_workspace.js",
        "packages/playground/src/modules/diagnostics_panel.js",
        "packages/playground/src/modules/file_actions.js",
        "packages/playground/src/modules/monaco_setup.js",
        "packages/playground/src/modules/result_file_editor.js",
        "packages/playground/src/modules/scenario_config_editor.js",
        "packages/playground/src/modules/workspace_fs.js",
        "packages/rumoca-web/runtime/rumoca_worker.js",
        "packages/rumoca-web/runtime/rumoca_runtime.js",
        "packages/rumoca-web/runtime/rumoca_interactive.js",
        "packages/rumoca-web/runtime/modelica_language.js",
        "packages/rumoca-web/runtime/parse_worker.js",
        "packages/rumoca-web/runtime/rumoca_gpu.js",
        "packages/rumoca-web/runtime/rumoca_diffsol.js",
        "packages/rumoca-web/viz/visualization_shared.js",
        "packages/rumoca-web/viz/results_app.js",
        "packages/rumoca-web/viz/results_mount.js",
        "docs/user-guide/live/rumoca-live.js",
    ];
    for file in js_checks {
        let mut cmd = Command::new("node");
        cmd.arg("--check").arg(file).current_dir(root);
        run_status(cmd)?;
    }
    Ok(())
}

fn check_playground_expected_sources(root: &Path) -> Result<()> {
    ensure_any_file_contains(
        root,
        &[
            "packages/playground/src/modules/diagnostics_panel.js",
            "packages/playground/index.html",
        ],
        "diagnostic-quick-fix",
    )?;
    ensure_any_file_contains(
        root,
        &[
            "packages/playground/src/main.js",
            "packages/playground/src/modules/diagnostics_panel.js",
        ],
        "triggerModelicaQuickFix",
    )?;
    ensure_any_file_contains(
        root,
        &[
            "packages/playground/src/main.js",
            "packages/playground/src/modules/diagnostics_panel.js",
        ],
        "triggerQuickFixAtCursor",
    )?;
    ensure_any_file_contains(
        root,
        &["packages/playground/src/modules/package_archive_controller.js"],
        "rumoca-cache/package-archives/v2/",
    )?;
    ensure_any_file_contains(
        root,
        &["packages/playground/src/modules/package_archive_controller.js"],
        "Discarding stale package-archive binary cache",
    )
}

fn run_single_threaded_wasm_smoke(root: &Path) -> Result<()> {
    let smoke_rayon = false;
    ensure_wasm_deps(root)?;
    build_wasm(
        root,
        WasmBuildProfile::Release,
        WasmVariant::FullWeb,
        smoke_rayon,
        false,
        false,
    )?;
    let pkg_subdir =
        wasm_build_subdir_name(WasmBuildProfile::Release, WasmVariant::FullWeb, smoke_rayon);
    run_wasm_simulation_smoke(root, &pkg_subdir)?;
    run_wasm_source_root_smoke(root, &pkg_subdir)
}

fn copy_dir_recursive(source: &Path, target: &Path) -> Result<()> {
    fs::create_dir_all(target).with_context(|| format!("failed to create {}", target.display()))?;
    for entry in
        fs::read_dir(source).with_context(|| format!("failed to read {}", source.display()))?
    {
        let entry =
            entry.with_context(|| format!("failed to read entry in {}", source.display()))?;
        let source_path = entry.path();
        let target_path = target.join(entry.file_name());
        if source_path.is_dir() {
            copy_dir_recursive(&source_path, &target_path)?;
        } else {
            fs::copy(&source_path, &target_path).with_context(|| {
                format!(
                    "failed to copy {} to {}",
                    source_path.display(),
                    target_path.display()
                )
            })?;
        }
    }
    Ok(())
}

fn run_wasm_simulation_smoke(root: &Path, pkg_subdir: &str) -> Result<()> {
    let mut wasm_smoke = Command::new("node");
    wasm_smoke
        .arg("packages/playground/tests/simulate_smoke.mjs")
        .arg("--pkg-subdir")
        .arg(pkg_subdir)
        .current_dir(root);
    run_status(wasm_smoke)
}

fn run_wasm_source_root_smoke(root: &Path, pkg_subdir: &str) -> Result<()> {
    let mut wasm_smoke = Command::new("node");
    wasm_smoke
        .arg("packages/playground/tests/source_root_smoke.mjs")
        .arg("--pkg-subdir")
        .arg(pkg_subdir)
        .current_dir(root);
    run_status(wasm_smoke)
}

fn ensure_any_file_contains(root: &Path, files: &[&str], needle: &str) -> Result<()> {
    for file in files {
        let path = root.join(file);
        let contents = fs::read_to_string(&path)
            .with_context(|| format!("failed to read smoke-check file {}", path.display()))?;
        if contents.contains(needle) {
            return Ok(());
        }
    }

    bail!(
        "expected to find `{needle}` in one of: {}",
        files.join(", ")
    )
}
