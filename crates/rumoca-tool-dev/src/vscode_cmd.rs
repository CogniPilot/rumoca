use anyhow::{Context, Result, ensure};
use clap::ValueEnum;
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::{Duration, UNIX_EPOCH};
use walkdir::WalkDir;

use crate::{
    VscodeBuildArgs, VscodeHostArgs, VscodePackageArgs, exe_name, newest_prefixed_file, repo_root,
    run_status,
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

pub(crate) fn build_vscode_ext(args: VscodeBuildArgs) -> Result<()> {
    let root = repo_root();
    let vscode_dir = resolve_vscode_dir(&root)?;

    if !args.system {
        build_and_stage_vscode_lsp(&root, &vscode_dir, true)?;
    }

    ensure_vscode_npm_dependencies(&vscode_dir, VscodeNpmDependencyMode::IfMissing, false)?;

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

    let vsix = newest_prefixed_file(&vscode_dir, "rumoca-modelica-", "vsix")?
        .context("failed to locate packaged VSCode extension (*.vsix)")?;
    println!("Built VSIX: {}", vsix.display());

    if !args.no_install {
        println!("Installing extension in VSCode...");
        let mut code = Command::new("code");
        code.arg("--install-extension")
            .arg(&vsix)
            .arg("--force")
            .current_dir(&vscode_dir);
        run_status(code)?;
    }

    Ok(())
}

pub(crate) fn package_vscode_ext(args: VscodePackageArgs) -> Result<()> {
    let root = repo_root();
    let vscode_dir = resolve_vscode_dir(&root)?;

    ensure_vscode_npm_dependencies(&vscode_dir, VscodeNpmDependencyMode::IfMissing, false)?;
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
    ensure_vscode_npm_dependencies(&vscode_dir, VscodeNpmDependencyMode::RefreshLocked, true)?;

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

pub(crate) fn vscode_dev(args: VscodeHostArgs) -> Result<()> {
    let root = repo_root();
    let vscode_dir = resolve_vscode_dir(&root)?;
    let workspace_dir = resolve_workspace_dir(&root, args.workspace_dir.as_deref())?;

    if !args.skip_lsp_build {
        build_and_stage_vscode_lsp(&root, &vscode_dir, false)?;
    }
    ensure_vscode_npm_dependencies(&vscode_dir, VscodeNpmDependencyMode::IfMissing, false)?;

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

fn resolve_vscode_npm_install_plan(
    has_lockfile: bool,
    node_modules_present: bool,
    esbuild_present: bool,
    mode: VscodeNpmDependencyMode,
) -> VscodeNpmInstallPlan {
    match mode {
        VscodeNpmDependencyMode::IfMissing if node_modules_present && esbuild_present => {
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
) -> Result<()> {
    let node_modules = vscode_dir.join("node_modules");
    let esbuild_bin = if cfg!(windows) {
        node_modules.join(".bin").join("esbuild.cmd")
    } else {
        node_modules.join(".bin").join("esbuild")
    };
    let has_lockfile = vscode_dir.join("package-lock.json").is_file();
    let plan = resolve_vscode_npm_install_plan(
        has_lockfile,
        node_modules.is_dir(),
        esbuild_bin.is_file(),
        mode,
    );

    match plan {
        VscodeNpmInstallPlan::Skip => return Ok(()),
        VscodeNpmInstallPlan::Ci => {
            println!("Refreshing VSCode npm dependencies with npm ci...");
        }
        VscodeNpmInstallPlan::Install if node_modules.is_dir() => {
            println!(
                "Reinstalling npm dependencies (missing toolchain at {})...",
                esbuild_bin.display()
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
    run_status(npm_install)
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
        fs::copy(&source, &target).with_context(|| {
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
    fs::copy(&source, &target).with_context(|| {
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

fn cargo_target_linker_env_suffix(target: &str) -> String {
    target.to_ascii_uppercase().replace('-', "_")
}

fn cargo_target_cc_env_suffix(target: &str) -> String {
    target.replace('-', "_")
}

#[cfg(test)]
mod tests {
    use super::{
        VscodeNpmDependencyMode, VscodeNpmInstallPlan, VscodePackageTarget,
        cargo_target_cc_env_suffix, cargo_target_linker_env_suffix,
        resolve_vscode_npm_install_plan,
    };

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
}
