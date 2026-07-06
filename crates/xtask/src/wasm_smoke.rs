//! WASM browser-smoke server, assets, and result types for the MSL editor/LSP
//! gates. Extracted from `verify_cmd.rs` to keep that file within the source
//! size budget; the logic is unchanged.

use anyhow::{Context, Result};
use std::fs;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

use crate::{command_exists, exe_name, run_status, run_status_quiet, vscode_cmd};

const WASM_SMOKE_SERVER_READY_PATH: &str = "/rumoca/";
const WASM_SMOKE_SERVER_START_ATTEMPTS: usize = 3;
const WASM_SMOKE_SERVER_START_TIMEOUT: Duration = Duration::from_secs(20);

pub(crate) fn can_launch_wasm_browser_msl_smoke() -> bool {
    command_exists("node") && detect_browser_binary().is_ok() && can_bind_local_browser_port()
}

pub(crate) fn run_wasm_browser_msl_smoke(root: &Path, msl_root: &Path) -> Result<()> {
    let output_dir = root.join("target/editor-msl-smoke");
    let _ = run_wasm_browser_msl_smoke_report(root, msl_root, &output_dir)?;
    Ok(())
}

pub(crate) fn run_wasm_browser_msl_smoke_report(
    root: &Path,
    msl_root: &Path,
    output_dir: &Path,
) -> Result<WasmSmokeSummary> {
    prepare_wasm_browser_smoke_assets(root, msl_root)?;
    fs::create_dir_all(output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;
    let (port, _child_guard) = start_wasm_smoke_server(root)?;

    let wasm_dir = root.join("packages/playground");
    ensure_wasm_browser_smoke_npm_dependencies(&wasm_dir)?;
    let browser = detect_browser_binary()?;
    let result_path = output_dir.join("wasm-browser-result.json");
    let smoke_model = "SmokeHarness";
    let smoke_url = format!(
        "http://127.0.0.1:{port}/rumoca/?rumoca_smoke=1&smoke_model={smoke_model}&smoke_source_url=/target/editor-msl-smoke/SmokeHarness.mo&smoke_package_archive_url=/target/editor-msl-smoke/msl-slice.zip&smoke_compile_timeout_ms=300000"
    );
    let mut smoke = Command::new("node");
    smoke
        .arg("tests/run_browser_msl_smoke.mjs")
        .arg("--browser-binary")
        .arg(&browser)
        .arg("--smoke-url")
        .arg(&smoke_url)
        .arg("--smoke-result")
        .arg(&result_path)
        .current_dir(&wasm_dir);
    run_status_quiet(smoke)
        .with_context(|| "failed to launch Playwright-driven playground smoke".to_string())?;
    let callback = fs::read_to_string(&result_path)
        .with_context(|| format!("failed to read wasm smoke result {}", result_path.display()))
        .and_then(|raw| {
            serde_json::from_str::<WasmSmokeCallback>(&raw)
                .context("failed to parse wasm smoke result JSON")
        })?;
    anyhow::ensure!(
        callback.status == "pass",
        "wasm browser smoke reported '{}' with payload {}",
        callback.status,
        serde_json::to_string_pretty(&callback.payload)
            .unwrap_or_else(|_| String::from("<unserializable>"))
    );
    Ok(callback.payload)
}

pub(crate) fn resolve_xtask_cli_executable(root: &Path) -> Result<PathBuf> {
    let repo_debug_bin = root.join("target/debug").join(exe_name("xtask"));
    if repo_debug_bin.is_file() {
        return Ok(repo_debug_bin);
    }
    std::env::current_exe().context("failed to resolve current xtask executable")
}

fn ensure_wasm_browser_smoke_npm_dependencies(wasm_dir: &Path) -> Result<()> {
    let playwright_package = wasm_dir.join("node_modules/playwright-core/package.json");
    if playwright_package.is_file() {
        return Ok(());
    }

    println!("Installing WASM browser smoke npm dependencies...");
    let mut npm = Command::new("npm");
    if wasm_dir.join("package-lock.json").is_file() {
        npm.arg("ci");
    } else {
        npm.arg("install");
    }
    npm.current_dir(wasm_dir);
    run_status(npm)
}

fn prepare_wasm_browser_smoke_assets(root: &Path, msl_root: &Path) -> Result<PathBuf> {
    let smoke_dir = root.join("target/editor-msl-smoke");
    if smoke_dir.exists() {
        fs::remove_dir_all(&smoke_dir)
            .with_context(|| format!("failed to remove {}", smoke_dir.display()))?;
    }
    let modelica_dir = smoke_dir.join("Modelica");
    let services_dir = smoke_dir.join("ModelicaServices");
    copy_dir_recursive(&msl_root.join("Modelica 4.1.0"), &modelica_dir)?;
    copy_dir_recursive(&msl_root.join("ModelicaServices 4.1.0"), &services_dir)?;
    fs::copy(msl_root.join("Complex.mo"), smoke_dir.join("Complex.mo"))
        .with_context(|| "failed to stage Complex.mo for wasm smoke".to_string())?;
    fs::write(smoke_dir.join("SmokeHarness.mo"), WASM_BROWSER_SMOKE_SOURCE)
        .with_context(|| "failed to stage browser smoke source".to_string())?;

    let mut zip = Command::new("zip");
    zip.arg("-qr")
        .arg("msl-slice.zip")
        .arg("Modelica")
        .arg("ModelicaServices")
        .arg("Complex.mo")
        .current_dir(&smoke_dir);
    run_status(zip)?;

    Ok(smoke_dir)
}

const WASM_BROWSER_SMOKE_SOURCE: &str = r#"model SmokeHarness
  Modelica.Electrical.Analog.Sources.ConstantVoltage source(V=1);
  Modelica.Electrical.Analog.Basic.Resistor resistor(R=1);
  Modelica.Electrical.Analog.Basic.Ground ground;
equation
  connect(source.p, resistor.p);
  connect(resistor.n, ground.p);
  connect(source.n, ground.p);
end SmokeHarness;
"#;

fn copy_dir_recursive(source: &Path, dest: &Path) -> Result<()> {
    anyhow::ensure!(
        source.is_dir(),
        "missing directory required for wasm smoke: {}",
        source.display()
    );
    for entry in walkdir::WalkDir::new(source) {
        let entry = entry
            .with_context(|| format!("failed to walk source directory {}", source.display()))?;
        let relative = entry.path().strip_prefix(source).with_context(|| {
            format!(
                "failed to strip source prefix {} from {}",
                source.display(),
                entry.path().display()
            )
        })?;
        let target = dest.join(relative);
        if entry.file_type().is_dir() {
            fs::create_dir_all(&target)
                .with_context(|| format!("failed to create {}", target.display()))?;
            continue;
        }
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        fs::copy(entry.path(), &target).with_context(|| {
            format!(
                "failed to copy '{}' to '{}'",
                entry.path().display(),
                target.display()
            )
        })?;
    }
    Ok(())
}

fn reserve_local_port() -> Result<u16> {
    let listener = TcpListener::bind(("127.0.0.1", 0)).context("failed to bind local port")?;
    let port = listener
        .local_addr()
        .context("failed to inspect reserved port")?
        .port();
    drop(listener);
    Ok(port)
}

fn can_bind_local_browser_port() -> bool {
    TcpListener::bind(("127.0.0.1", 0)).is_ok()
}

fn start_wasm_smoke_server(root: &Path) -> Result<(u16, ChildGuard)> {
    let xtask_exe = resolve_xtask_cli_executable(root)?;
    println!("Prebuilding WASM module for browser smoke...");
    let mut build = Command::new(&xtask_exe);
    build
        .arg("playground")
        .arg("build")
        // Browser smoke validates the default editor/runtime flow on the
        // non-threaded package (the build default, no --rayon).
        .current_dir(root);
    run_status(build).with_context(|| {
        "failed to prebuild `cargo xtask playground build` for browser smoke".to_string()
    })?;
    let mut last_error = None;

    for attempt in 1..=WASM_SMOKE_SERVER_START_ATTEMPTS {
        let port = reserve_local_port()?;
        let mut server = Command::new(&xtask_exe);
        server
            .arg("playground")
            .arg("edit")
            // The `playground build` above already produced the bundle; serve it
            // as-is so the server binds the port promptly instead of repeating a
            // ~2 min wasm-bindgen/wasm-opt rebuild that overruns the readiness
            // window below.
            .arg("--skip-build")
            .arg("--port")
            .arg(port.to_string())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .current_dir(root);
        let child = server
            .spawn()
            .context("failed to launch `cargo xtask playground edit` for browser smoke")?;
        let mut child_guard = ChildGuard::new(child);

        match wait_for_http_ready(
            &mut child_guard,
            port,
            WASM_SMOKE_SERVER_READY_PATH,
            WASM_SMOKE_SERVER_START_TIMEOUT,
        ) {
            Ok(()) => return Ok((port, child_guard)),
            Err(error) => last_error = Some(format!("attempt {attempt}: {error:#}")),
        }
    }

    anyhow::bail!(
        "failed to start wasm smoke server after {} attempts: {}",
        WASM_SMOKE_SERVER_START_ATTEMPTS,
        last_error.unwrap_or_else(|| "unknown startup failure".to_string())
    )
}

fn wait_for_http_ready(
    child_guard: &mut ChildGuard,
    port: u16,
    path: &str,
    timeout: Duration,
) -> Result<()> {
    let started = Instant::now();
    while started.elapsed() < timeout {
        if http_get_ok(port, path) {
            return Ok(());
        }
        if child_guard.has_exited()? {
            anyhow::bail!(
                "wasm smoke server exited before responding on http://127.0.0.1:{port}{path}"
            );
        }
        thread::sleep(Duration::from_millis(250));
    }
    anyhow::bail!("timed out waiting for wasm smoke server on http://127.0.0.1:{port}{path}")
}

fn http_get_ok(port: u16, path: &str) -> bool {
    let Ok(mut stream) = TcpStream::connect(("127.0.0.1", port)) else {
        return false;
    };
    let request =
        format!("GET {path} HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nConnection: close\r\n\r\n");
    if stream.write_all(request.as_bytes()).is_err() {
        return false;
    }
    let mut response = String::new();
    if stream.read_to_string(&mut response).is_err() {
        return false;
    }
    response.starts_with("HTTP/1.1 200")
}

fn detect_browser_binary() -> Result<String> {
    ["google-chrome", "chromium", "chromium-browser"]
        .into_iter()
        .find(|program| {
            Command::new(program)
                .arg("--version")
                .output()
                .is_ok_and(|output| output.status.success())
        })
        .map(ToOwned::to_owned)
        .context("missing browser for wasm smoke (expected google-chrome/chromium)")
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct WasmSmokeSummary {
    pub(crate) model_name: Option<String>,
    pub(crate) source_root_count: Option<u64>,
    pub(crate) status_text: Option<String>,
    pub(crate) open_ms: Option<u64>,
    pub(crate) code_lens_ms: Option<u64>,
    pub(crate) code_lens_count: Option<u64>,
    pub(crate) archive_load_ms: Option<u64>,
    pub(crate) source_root_load_ms: Option<u64>,
    pub(crate) source_root_load_completion_count: Option<u64>,
    pub(crate) source_root_expected_completion_present: Option<bool>,
    pub(crate) source_root_stage_timings: Option<vscode_cmd::VscodeStageTimingSummary>,
    pub(crate) compile_ms: Option<u64>,
    pub(crate) completion_ms: Option<u64>,
    pub(crate) completion_count: Option<u64>,
    pub(crate) expected_completion_present: Option<bool>,
    pub(crate) cold_stage_timings: Option<vscode_cmd::VscodeStageTimingSummary>,
    pub(crate) warm_completion_ms: Option<u64>,
    pub(crate) warm_completion_count: Option<u64>,
    pub(crate) warm_expected_completion_present: Option<bool>,
    pub(crate) warm_stage_timings: Option<vscode_cmd::VscodeStageTimingSummary>,
    pub(crate) hover_ms: Option<u64>,
    pub(crate) hover_count: Option<u64>,
    pub(crate) expected_hover_present: Option<bool>,
    pub(crate) definition_ms: Option<u64>,
    pub(crate) definition_count: Option<u64>,
    pub(crate) expected_definition_present: Option<bool>,
    pub(crate) cross_file_definition_present: Option<bool>,
    pub(crate) error: Option<String>,
}

#[derive(Debug, serde::Deserialize)]
struct WasmSmokeCallback {
    status: String,
    payload: WasmSmokeSummary,
}

struct ChildGuard {
    child: Option<Child>,
}

impl ChildGuard {
    fn new(child: Child) -> Self {
        Self { child: Some(child) }
    }

    fn has_exited(&mut self) -> Result<bool> {
        let Some(child) = self.child.as_mut() else {
            return Ok(true);
        };
        Ok(child.try_wait()?.is_some())
    }
}

impl Drop for ChildGuard {
    fn drop(&mut self) {
        let Some(mut child) = self.child.take() else {
            return;
        };
        let _ = child.kill();
        let _ = child.wait();
    }
}
