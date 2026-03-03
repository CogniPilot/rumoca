use anyhow::{Context, Result, ensure};
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
    BuildVscodeExtArgs, VscodeDevArgs, exe_name, newest_prefixed_file, repo_root, run_status,
};

pub(crate) fn build_vscode_ext(args: BuildVscodeExtArgs) -> Result<()> {
    let root = repo_root();
    let vscode_dir = resolve_vscode_dir(&root)?;

    if !args.system {
        build_and_stage_vscode_lsp(&root, &vscode_dir, true)?;
    }

    ensure_vscode_npm_dependencies(&vscode_dir)?;

    println!("Compiling extension TypeScript...");
    let mut npm_esbuild = Command::new("npm");
    npm_esbuild
        .arg("run")
        .arg("esbuild")
        .current_dir(&vscode_dir);
    run_status(npm_esbuild)?;

    if args.dev {
        launch_vscode_extension_host(&vscode_dir, &root)?;
        return Ok(());
    }

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

    if !args.build {
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

pub(crate) fn vscode_dev(args: VscodeDevArgs) -> Result<()> {
    let root = repo_root();
    let vscode_dir = resolve_vscode_dir(&root)?;
    let workspace_dir = resolve_workspace_dir(&root, args.workspace_dir.as_deref())?;

    if !args.skip_lsp_build {
        build_and_stage_vscode_lsp(&root, &vscode_dir, false)?;
    }
    ensure_vscode_npm_dependencies(&vscode_dir)?;

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
                eprintln!("[rum vscode-dev] failed to initialize Rust watcher: {error:#}");
                0
            }
        };
        while !stop.load(Ordering::Relaxed) {
            thread::sleep(Duration::from_millis(800));
            let current_fingerprint = match rust_watch_fingerprint(&root) {
                Ok(value) => value,
                Err(error) => {
                    eprintln!("[rum vscode-dev] Rust watcher scan failed: {error:#}");
                    continue;
                }
            };
            if current_fingerprint == last_fingerprint {
                continue;
            }
            last_fingerprint = current_fingerprint;
            println!("[rum vscode-dev] Rust change detected; rebuilding rumoca-lsp...");
            match build_and_stage_vscode_lsp(&root, &vscode_dir, false) {
                Ok(()) => println!("[rum vscode-dev] rumoca-lsp rebuild complete."),
                Err(error) => eprintln!("[rum vscode-dev] rumoca-lsp rebuild failed: {error:#}"),
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

fn ensure_vscode_npm_dependencies(vscode_dir: &Path) -> Result<()> {
    let node_modules = vscode_dir.join("node_modules");
    let esbuild_bin = if cfg!(windows) {
        node_modules.join(".bin").join("esbuild.cmd")
    } else {
        node_modules.join(".bin").join("esbuild")
    };

    if node_modules.is_dir() && esbuild_bin.is_file() {
        return Ok(());
    }

    if node_modules.is_dir() {
        println!(
            "Reinstalling npm dependencies (missing toolchain at {})...",
            esbuild_bin.display()
        );
    } else {
        println!("Installing npm dependencies...");
    }

    let mut npm_install = Command::new("npm");
    if vscode_dir.join("package-lock.json").is_file() {
        npm_install.arg("ci");
    } else {
        npm_install.arg("install");
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
