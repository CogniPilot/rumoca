//! Browser vendor-asset builder used by the MSL plot/speed-report tooling.
//!
//! This is a copy of `xtask`'s `web_assets` module. `xtask` keeps its own copy
//! for the light web/playground commands that stay there; duplicating this
//! self-contained (no compiler deps) helper is the sanctioned alternative to a
//! shared util crate, which `xtask` must not depend on (SPEC_0029 §7).

use anyhow::{Context, Result, bail, ensure};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

const WEB_VENDOR_REQUIRED_FILES: &[&str] = &[
    "markdown_renderer.js",
    "modelica_language.js",
    "results_app.css",
    "results_report_inline.js",
    "three_global.js",
    "three_viewer.js",
    "uplot.min.css",
    "uplot_global.js",
    "visualization_shared.js",
    "web_deps.js",
];

pub fn ensure_web_vendor_assets(root: &Path) -> Result<PathBuf> {
    let vendor_dir = root.join("packages/rumoca-web/vendor");
    if web_vendor_assets_ready(&vendor_dir) {
        return Ok(vendor_dir);
    }
    build_web_vendor_assets(root)
}

pub fn build_web_vendor_assets(root: &Path) -> Result<PathBuf> {
    let web_root = root.join("packages/rumoca-web");
    let vendor_dir = web_root.join("vendor");
    ensure!(
        web_root.join("package.json").is_file(),
        "missing {}; run this command from a Rumoca checkout",
        web_root.join("package.json").display()
    );
    ensure_web_toolchain_available()?;
    ensure_web_package_deps(&web_root).with_context(|| {
        format!(
            "failed to prepare npm dependencies in {}",
            web_root.display()
        )
    })?;

    let mut build = Command::new("npm");
    build.arg("run").arg("build").current_dir(&web_root);
    run_status(build).with_context(|| {
        format!(
            "failed to build browser assets in {}; verify Node 20/npm are installed",
            web_root.display()
        )
    })?;

    ensure!(
        web_vendor_assets_ready(&vendor_dir),
        "web asset build completed but required files are missing from {}",
        vendor_dir.display()
    );
    Ok(vendor_dir)
}

fn web_vendor_assets_ready(vendor_dir: &Path) -> bool {
    vendor_dir.is_dir()
        && WEB_VENDOR_REQUIRED_FILES
            .iter()
            .all(|relative| vendor_dir.join(relative).is_file())
}

fn ensure_web_toolchain_available() -> Result<()> {
    ensure!(
        command_exists("node"),
        "web/package workflows require Node.js 20 and npm; install Node, then retry the selected command"
    );
    ensure!(
        command_exists("npm"),
        "web/package workflows require npm from Node.js 20; install Node/npm, then retry the selected command"
    );
    Ok(())
}

fn ensure_web_package_deps(web_root: &Path) -> Result<()> {
    let node_modules = web_root.join("node_modules");
    let esbuild_bin = node_modules.join(".bin").join(if cfg!(windows) {
        "esbuild.cmd"
    } else {
        "esbuild"
    });
    let package_lock = web_root.join("package-lock.json");
    let required_packages = ["jszip", "marked", "monaco-editor", "three", "uplot"];
    if node_modules.is_dir()
        && esbuild_bin.is_file()
        && required_packages
            .iter()
            .all(|package| node_modules.join(package).is_dir())
    {
        return Ok(());
    }

    let mut install = Command::new("npm");
    if package_lock.is_file() {
        install.arg("ci");
    } else {
        install.arg("install");
    }
    install.current_dir(web_root);
    run_status(install)
}

fn command_exists(program: &str) -> bool {
    Command::new(program)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

fn run_status(mut command: Command) -> Result<()> {
    let rendered = format!("{command:?}");
    let status = command
        .status()
        .with_context(|| format!("failed to run command: {rendered}"))?;
    if !status.success() {
        bail!("command failed (status={status}): {rendered}");
    }
    Ok(())
}
