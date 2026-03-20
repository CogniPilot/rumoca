use anyhow::{Context, Result, ensure};
use clap::{Args, Subcommand};
use std::ffi::OsStr;
use std::fs;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use crate::{
    CheckRustFileLinesArgs, cmd_check_rust_file_lines, command_exists, exe_name, lsp_benchmark_cmd,
    msl_flamegraph_cmd, run_status, run_status_quiet, test_cmd, vscode_cmd,
};

#[derive(Debug, Args, Clone)]
pub(crate) struct VerifyArgs {
    #[command(subcommand)]
    pub(crate) command: VerifyCommand,
}

#[derive(Debug, Args, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) struct VerifyEditorRuntimeArgs {
    /// On Ubuntu/Debian Linux hosts, install missing headless VS Code smoke prerequisites (`xvfb`, `xauth`) before running
    #[arg(long)]
    pub(crate) install_prereqs: bool,
}

#[derive(Debug, Subcommand, Clone, PartialEq, Eq)]
pub(crate) enum VerifyCommand {
    /// Rust formatting, line-count policy, traversal policy, and clippy
    Lint,
    /// Workspace tests that mirror the main test matrix
    Workspace,
    /// Environment-dependent example template runtime checks
    TemplateRuntimes,
    /// Real VS Code extension-host MSL smoke check
    VscodeMsl(VerifyEditorRuntimeArgs),
    /// Full-MSL LSP timings plus headless VS Code and WASM runtime smoke
    LspMslCompletionTimings(VerifyEditorRuntimeArgs),
    /// Real browser-hosted WASM editor MSL smoke check
    WasmEditorMsl,
    /// Real VS Code and WASM editor MSL smoke checks
    EditorMsl(VerifyEditorRuntimeArgs),
    /// Full verification suite mirrored by GitHub CI
    Full,
    /// Full local verification suite, excluding the slow 180-model MSL parity gate
    Quick,
    /// Verify the primary binaries build
    Binaries,
    /// Rustdoc/docs gate with warnings denied
    Docs,
    /// Full MSL/OMC parity gate harness
    MslParity,
    /// Generate real flamegraph SVGs for the hottest compile and sim models from the latest MSL run
    MslHotspots,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct VerifyStep {
    label: &'static str,
    args: &'static [&'static str],
    include_in_quick: bool,
}

const VERIFY_SUITE_STEPS: &[VerifyStep] = &[
    VerifyStep {
        label: "lint",
        args: &["verify", "lint"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "workspace tests",
        args: &["verify", "workspace"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "binary build",
        args: &["verify", "binaries"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "coverage run",
        args: &["coverage", "run"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "coverage report",
        args: &["coverage", "report"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "coverage gate",
        args: &[
            "coverage",
            "gate",
            "--allowed-workspace-line-coverage-drop",
            "6.0",
        ],
        include_in_quick: true,
    },
    VerifyStep {
        label: "docs",
        args: &["verify", "docs"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "VS Code gate",
        args: &["vscode", "test"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "WASM gate",
        args: &["wasm", "test"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "full-MSL LSP/editor gate",
        args: &["verify", "lsp-msl-completion-timings"],
        include_in_quick: false,
    },
    VerifyStep {
        label: "MSL parity",
        args: &["verify", "msl-parity"],
        include_in_quick: false,
    },
];

const WASM_SMOKE_SERVER_READY_PATH: &str = "/editors/wasm/index.html";
const WASM_SMOKE_SERVER_START_ATTEMPTS: usize = 3;
const WASM_SMOKE_SERVER_START_TIMEOUT: Duration = Duration::from_secs(20);
const MSL_RESOURCE_CPU_SAMPLE_INTERVAL: Duration = Duration::from_secs(30);

#[derive(Debug, Clone, Copy)]
enum VerifySuite {
    Full,
    Quick,
}

impl VerifySuite {
    fn label(self) -> &'static str {
        match self {
            Self::Full => "verify full",
            Self::Quick => "verify quick",
        }
    }

    fn includes(self, step: &VerifyStep) -> bool {
        match self {
            Self::Full => true,
            Self::Quick => step.include_in_quick,
        }
    }
}

pub(crate) fn run(args: VerifyArgs, root: &Path) -> Result<()> {
    match args.command {
        VerifyCommand::Lint => run_lint_job(root),
        VerifyCommand::Workspace => test_cmd::run_workspace_tests(root),
        VerifyCommand::TemplateRuntimes => run_template_runtime_checks(root),
        VerifyCommand::VscodeMsl(args) => run_vscode_editor_msl_smoke(root, args.install_prereqs),
        VerifyCommand::LspMslCompletionTimings(args) => {
            run_lsp_msl_completion_timings(root, args.install_prereqs)
        }
        VerifyCommand::WasmEditorMsl => run_wasm_editor_msl_smoke(root),
        VerifyCommand::EditorMsl(args) => run_editor_msl_smoke(root, args.install_prereqs),
        VerifyCommand::Full => run_verify_suite(root, VerifySuite::Full),
        VerifyCommand::Quick => run_verify_suite(root, VerifySuite::Quick),
        VerifyCommand::Binaries => test_cmd::run_workspace_binary_build(root),
        VerifyCommand::Docs => test_cmd::run_workspace_docs(root),
        VerifyCommand::MslParity => run_msl_quality_gate(root),
        VerifyCommand::MslHotspots => run_msl_hotspot_flamegraphs(root),
    }
}

#[derive(Debug, serde::Deserialize)]
struct MslHotspotSummary {
    model_results: Vec<MslHotspotModelResult>,
}

#[derive(Debug, serde::Deserialize)]
struct MslHotspotModelResult {
    model_name: String,
    #[serde(default)]
    compile_seconds: Option<f64>,
    #[serde(default)]
    sim_wall_seconds: Option<f64>,
}

fn hottest_compile_model(summary: &MslHotspotSummary) -> Option<(&str, f64)> {
    summary
        .model_results
        .iter()
        .filter_map(|result| {
            result
                .compile_seconds
                .map(|seconds| (result.model_name.as_str(), seconds))
        })
        .max_by(|(_, lhs), (_, rhs)| lhs.total_cmp(rhs))
}

fn hottest_sim_model(summary: &MslHotspotSummary) -> Option<(&str, f64)> {
    summary
        .model_results
        .iter()
        .filter_map(|result| {
            result
                .sim_wall_seconds
                .map(|seconds| (result.model_name.as_str(), seconds))
        })
        .max_by(|(_, lhs), (_, rhs)| lhs.total_cmp(rhs))
}

fn latest_msl_results_path(root: &Path) -> PathBuf {
    root.join("target/msl/results/msl_results.json")
}

fn load_latest_msl_hotspot_summary(root: &Path) -> Result<MslHotspotSummary> {
    let results_path = latest_msl_results_path(root);
    let raw = fs::read_to_string(&results_path)
        .with_context(|| format!("failed to read {}", results_path.display()))?;
    serde_json::from_str(&raw)
        .with_context(|| format!("failed to parse {}", results_path.display()))
}

fn run_msl_hotspot_flamegraphs(root: &Path) -> Result<()> {
    let summary = load_latest_msl_hotspot_summary(root).with_context(|| {
        format!(
            "missing hotspot source data. Run the MSL 180-model test first so {} exists.",
            latest_msl_results_path(root).display()
        )
    })?;
    let library = cached_msl_library_root(root)?;

    let Some((compile_model, compile_seconds)) = hottest_compile_model(&summary) else {
        anyhow::bail!("latest MSL results did not contain per-model compile timings");
    };
    println!(
        "Generating compile flamegraph for hottest model: {} ({:.2}s)",
        compile_model, compile_seconds
    );
    msl_flamegraph_cmd::run(
        msl_flamegraph_cmd::MslFlamegraphArgs {
            model: compile_model.to_string(),
            mode: msl_flamegraph_cmd::MslFlamegraphMode::Compile,
            library: Some(library.clone()),
            output: None,
            freq: 99,
            no_inline: false,
            stop_time: None,
        },
        root,
    )?;

    let Some((sim_model, sim_seconds)) = hottest_sim_model(&summary) else {
        anyhow::bail!("latest MSL results did not contain per-model simulation timings");
    };
    println!(
        "Generating simulation flamegraph for hottest model: {} ({:.2}s)",
        sim_model, sim_seconds
    );
    msl_flamegraph_cmd::run(
        msl_flamegraph_cmd::MslFlamegraphArgs {
            model: sim_model.to_string(),
            mode: msl_flamegraph_cmd::MslFlamegraphMode::Simulate,
            library: Some(library),
            output: None,
            freq: 99,
            no_inline: false,
            stop_time: None,
        },
        root,
    )
}

fn run_template_runtime_checks(root: &Path) -> Result<()> {
    let mut cmd = Command::new("cargo");
    cmd.arg("test")
        .arg("--verbose")
        .arg("-p")
        .arg("rumoca")
        .arg("--test")
        .arg("sympy_template_regression")
        .arg("--")
        .arg("--ignored")
        .arg("--nocapture")
        .current_dir(root);
    run_status(cmd)?;

    let mut cmd = Command::new("cargo");
    cmd.arg("test")
        .arg("--verbose")
        .arg("-p")
        .arg("rumoca")
        .arg("--test")
        .arg("backend_template_runtime_regression")
        .arg("--")
        .arg("--ignored")
        .arg("--nocapture")
        .current_dir(root);
    run_status(cmd)
}

fn run_editor_msl_smoke(root: &Path, install_prereqs: bool) -> Result<()> {
    run_vscode_editor_msl_smoke(root, install_prereqs)?;
    run_wasm_editor_msl_smoke(root)
}

fn run_vscode_editor_msl_smoke(root: &Path, install_prereqs: bool) -> Result<()> {
    let msl_root = cached_msl_library_root(root)?;
    vscode_cmd::run_vscode_msl_smoke(root, &msl_root, install_prereqs)
}

fn run_wasm_editor_msl_smoke(root: &Path) -> Result<()> {
    let msl_root = cached_msl_library_root(root)?;
    run_wasm_browser_msl_smoke(root, &msl_root)
}

fn run_verify_suite(root: &Path, suite: VerifySuite) -> Result<()> {
    println!("Running `{}` suite...", suite.label());
    for step in VERIFY_SUITE_STEPS
        .iter()
        .filter(|step| suite.includes(step))
    {
        run_rum_step(root, step)?;
    }
    println!("`{}` suite passed.", suite.label());
    Ok(())
}

fn run_rum_step(root: &Path, step: &VerifyStep) -> Result<()> {
    println!("Running {}: `rum {}`", step.label, step.args.join(" "));
    let rum_exe = resolve_rum_cli_executable(root)?;
    let mut cmd = Command::new(rum_exe);
    cmd.args(step.args).current_dir(root);
    run_status(cmd)
}

fn cached_msl_library_root(root: &Path) -> Result<PathBuf> {
    let msl_root = root.join("target/msl/ModelicaStandardLibrary-4.1.0");
    anyhow::ensure!(
        msl_root.is_dir(),
        "missing cached MSL at {}. Populate target/msl first.",
        msl_root.display()
    );
    Ok(msl_root)
}

fn run_lsp_msl_completion_timings(root: &Path, install_prereqs: bool) -> Result<()> {
    let msl_root = cached_msl_library_root(root)?;
    lsp_benchmark_cmd::run_lsp_msl_completion_timings(root, &msl_root, install_prereqs)
}

pub(crate) fn can_launch_wasm_browser_msl_smoke() -> bool {
    command_exists("node") && detect_browser_binary().is_ok() && can_bind_local_browser_port()
}

fn run_wasm_browser_msl_smoke(root: &Path, msl_root: &Path) -> Result<()> {
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

    let wasm_dir = root.join("editors/wasm");
    ensure_wasm_browser_smoke_npm_dependencies(&wasm_dir)?;
    let browser = detect_browser_binary()?;
    let result_path = output_dir.join("wasm-browser-result.json");
    let smoke_url = format!(
        "http://127.0.0.1:{port}/editors/wasm/index.html?rumoca_smoke=1&smoke_model=Resistor&smoke_source_url=/target/editor-msl-smoke/Resistor.mo&smoke_library_zip_url=/target/editor-msl-smoke/msl-slice.zip&smoke_compile_timeout_ms=300000"
    );
    let mut smoke = Command::new("node");
    smoke
        .arg("tests/run_browser_msl_smoke.mjs")
        .env("RUMOCA_BROWSER_BINARY", &browser)
        .env("RUMOCA_WASM_SMOKE_URL", &smoke_url)
        .env("RUMOCA_WASM_SMOKE_RESULT", &result_path)
        .current_dir(&wasm_dir);
    run_status_quiet(smoke)
        .with_context(|| "failed to launch Playwright-driven wasm editor smoke".to_string())?;
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
    enforce_wasm_smoke_latency_budget(&callback.payload)?;
    Ok(callback.payload)
}

fn resolve_rum_cli_executable(root: &Path) -> Result<PathBuf> {
    let repo_debug_bin = root.join("target/debug").join(exe_name("rum"));
    if repo_debug_bin.is_file() {
        return Ok(repo_debug_bin);
    }
    std::env::current_exe().context("failed to resolve current rum executable")
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
    fs::copy(
        msl_root.join("Modelica 4.1.0/Electrical/Analog/Examples/Resistor.mo"),
        smoke_dir.join("Resistor.mo"),
    )
    .with_context(|| "failed to stage MSL example source for wasm smoke".to_string())?;

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
    let rum_exe = resolve_rum_cli_executable(root)?;
    println!("Prebuilding WASM module for browser smoke...");
    let mut build = Command::new(&rum_exe);
    build.arg("wasm").arg("build").current_dir(root);
    run_status(build)
        .with_context(|| "failed to prebuild `rum wasm build` for browser smoke".to_string())?;
    let mut last_error = None;

    for attempt in 1..=WASM_SMOKE_SERVER_START_ATTEMPTS {
        let port = reserve_local_port()?;
        let mut server = Command::new(&rum_exe);
        server
            .arg("wasm")
            .arg("edit")
            .arg("--skip-build")
            .arg("--port")
            .arg(port.to_string())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .current_dir(root);
        let child = server
            .spawn()
            .context("failed to launch `rum wasm edit` for browser smoke")?;
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
    if let Some(configured) = std::env::var_os("RUMOCA_BROWSER_BINARY") {
        let configured = configured.to_string_lossy().into_owned();
        ensure!(
            Command::new(&configured)
                .arg("--version")
                .output()
                .is_ok_and(|output| output.status.success()),
            "configured browser '{}' failed `--version` check",
            configured
        );
        return Ok(configured);
    }

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
    pub(crate) library_count: Option<u64>,
    pub(crate) status_text: Option<String>,
    pub(crate) open_ms: Option<u64>,
    pub(crate) code_lens_ms: Option<u64>,
    pub(crate) code_lens_count: Option<u64>,
    pub(crate) archive_load_ms: Option<u64>,
    pub(crate) library_load_ms: Option<u64>,
    pub(crate) library_load_completion_count: Option<u64>,
    pub(crate) library_expected_completion_present: Option<bool>,
    pub(crate) library_stage_timings: Option<vscode_cmd::VscodeStageTimingSummary>,
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

fn enforce_wasm_smoke_latency_budget(summary: &WasmSmokeSummary) -> Result<()> {
    if let Some(max_ms) = env_var_nonzero_u64("RUMOCA_WASM_EDITOR_SMOKE_LIBRARY_LOAD_MAX_MS")
        && let Some(measured_ms) = summary.archive_load_ms.or(summary.library_load_ms)
    {
        anyhow::ensure!(
            measured_ms <= max_ms,
            "wasm editor smoke library load took {measured_ms}ms (budget {max_ms}ms)"
        );
    }

    if let Some(max_ms) = env_var_nonzero_u64("RUMOCA_WASM_EDITOR_SMOKE_COMPILE_MAX_MS")
        && let Some(measured_ms) = summary.compile_ms
    {
        anyhow::ensure!(
            measured_ms <= max_ms,
            "wasm editor smoke compile took {measured_ms}ms (budget {max_ms}ms)"
        );
    }

    Ok(())
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

fn run_lint_job(root: &Path) -> Result<()> {
    test_cmd::run_workspace_fmt_check(root)?;
    cmd_check_rust_file_lines(CheckRustFileLinesArgs {
        max_lines: 2000,
        all_files: true,
    })?;

    let mut traversal = Command::new("cargo");
    traversal
        .arg("run")
        .arg("--quiet")
        .arg("--bin")
        .arg("rumoca-traversal-policy-check")
        .current_dir(root);
    run_status(traversal)?;

    test_cmd::run_workspace_clippy(root)
}

fn run_msl_quality_gate(root: &Path) -> Result<()> {
    let ci_env = MslCiEnvironment::from_env(root);
    ci_env.print_notice();
    ci_env.clean_stale_results()?;
    let _cleanup = MslResultsCleanupGuard::new(ci_env.results_dir.clone(), ci_env.clean_results);
    let _monitor = MslResourceMonitor::start(ci_env.clone());

    let mut build_sim_worker = Command::new("cargo");
    build_sim_worker
        .arg("build")
        .arg("--verbose")
        .arg("--release")
        .arg("--package")
        .arg("rumoca-test-msl")
        .arg("--bin")
        .arg("rumoca-sim-worker")
        .current_dir(root);
    run_status_logged(build_sim_worker)?;

    let mut build_msl_tools = Command::new("cargo");
    build_msl_tools
        .arg("build")
        .arg("--verbose")
        .arg("--release")
        .arg("--package")
        .arg("rumoca-tool-dev")
        .arg("--bin")
        .arg("rumoca-msl-tools")
        .current_dir(root);
    run_status_logged(build_msl_tools)?;

    let mut gate = Command::new("cargo");
    gate.arg("test")
        .arg("--verbose")
        .arg("--release")
        .arg("--package")
        .arg("rumoca-test-msl")
        .arg("--test")
        .arg("msl_tests")
        .arg("balance_pipeline::balance_pipeline_core::test_msl_all")
        .arg("--")
        .arg("--ignored")
        .arg("--nocapture")
        .env(
            "RUMOCA_SIM_WORKER_EXE",
            root.join("target/release/rumoca-sim-worker"),
        )
        .env(
            "RUMOCA_MSL_TOOLS_EXE",
            root.join("target/release/rumoca-msl-tools"),
        )
        .env("RUST_BACKTRACE", "full")
        .env("RUMOCA_MSL_CACHE_DIR", root.join("target/msl"))
        .current_dir(root);
    run_status_logged(gate)
}

fn run_status_logged(command: Command) -> Result<()> {
    println!("Running command: {command:?}");
    run_status(command)
}

#[derive(Debug, Clone)]
struct MslCiEnvironment {
    root: std::path::PathBuf,
    results_dir: std::path::PathBuf,
    monitor_interval: Option<Duration>,
    clean_results: bool,
    github_actions: bool,
}

const MSL_RESULTS_PRESERVED_DIRS: &[&str] = &["omc_parity_cache"];

fn should_preserve_msl_results_entry(entry_path: &Path) -> bool {
    entry_path.is_dir()
        && entry_path
            .file_name()
            .and_then(OsStr::to_str)
            .is_some_and(|name| MSL_RESULTS_PRESERVED_DIRS.contains(&name))
}

fn clean_msl_results_dir(results_dir: &Path) -> std::io::Result<()> {
    if !results_dir.is_dir() {
        return Ok(());
    }

    for entry in fs::read_dir(results_dir)? {
        let entry = entry?;
        let path = entry.path();
        if should_preserve_msl_results_entry(&path) {
            continue;
        }
        if path.is_dir() {
            fs::remove_dir_all(&path)?;
        } else {
            fs::remove_file(&path)?;
        }
    }

    if fs::read_dir(results_dir)?.next().is_none() {
        fs::remove_dir(results_dir)?;
    }

    Ok(())
}

impl MslCiEnvironment {
    fn from_env(root: &Path) -> Self {
        let results_dir = std::env::var_os("RUMOCA_CI_MSL_RESULTS_DIR")
            .filter(|value| !value.is_empty())
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|| root.join("target/msl/results"));
        Self {
            root: root.to_path_buf(),
            results_dir,
            monitor_interval: env_var_nonzero_u64("RUMOCA_CI_RESOURCE_MONITOR_SECS")
                .map(Duration::from_secs),
            clean_results: env_var_bool("RUMOCA_CI_CLEAN_MSL_RESULTS"),
            github_actions: std::env::var_os("GITHUB_ACTIONS").is_some(),
        }
    }

    fn print_notice(&self) {
        if !self.github_actions {
            return;
        }
        println!(
            "GitHub Actions note: workflow concurrency may cancel this job when a newer push or force-push reaches the same branch/PR, even if CPU, disk, and memory look healthy."
        );
    }

    fn clean_stale_results(&self) -> Result<()> {
        if !self.clean_results || !self.results_dir.is_dir() {
            return Ok(());
        }
        eprintln!(
            "Removing stale MSL results directory before run: {}",
            self.results_dir.display()
        );
        print_results_dir_summary("cleanup-start", &self.results_dir);
        clean_msl_results_dir(&self.results_dir).with_context(|| {
            format!(
                "failed to remove stale MSL results directory '{}'",
                self.results_dir.display()
            )
        })
    }
}

struct MslResultsCleanupGuard {
    results_dir: std::path::PathBuf,
    enabled: bool,
}

impl MslResultsCleanupGuard {
    fn new(results_dir: std::path::PathBuf, enabled: bool) -> Self {
        Self {
            results_dir,
            enabled,
        }
    }
}

impl Drop for MslResultsCleanupGuard {
    fn drop(&mut self) {
        if !self.enabled || !self.results_dir.is_dir() {
            return;
        }
        eprintln!(
            "Cleaning MSL results directory: {}",
            self.results_dir.display()
        );
        print_results_dir_summary("cleanup-before", &self.results_dir);
        if let Err(error) = clean_msl_results_dir(&self.results_dir) {
            eprintln!(
                "WARNING: failed to remove MSL results directory '{}': {error}",
                self.results_dir.display()
            );
            return;
        }
        eprintln!(
            "Removed MSL results directory: {}",
            self.results_dir.display()
        );
    }
}

struct MslResourceMonitor {
    config: MslCiEnvironment,
    done: Arc<AtomicBool>,
    worker: Option<JoinHandle<()>>,
}

impl MslResourceMonitor {
    fn start(config: MslCiEnvironment) -> Self {
        print_resource_snapshot("initial", &config, true);
        let Some(interval) = config.monitor_interval else {
            return Self {
                config,
                done: Arc::new(AtomicBool::new(true)),
                worker: None,
            };
        };

        let done = Arc::new(AtomicBool::new(false));
        let done_flag = Arc::clone(&done);
        let config_for_worker = config.clone();
        let worker = thread::spawn(move || {
            run_resource_monitor_loop(done_flag, interval, config_for_worker);
        });
        Self {
            config,
            done,
            worker: Some(worker),
        }
    }
}

impl Drop for MslResourceMonitor {
    fn drop(&mut self) {
        self.done.store(true, Ordering::Relaxed);
        if let Some(worker) = self.worker.take() {
            let _ = worker.join();
        }
        print_resource_snapshot("final", &self.config, true);
    }
}

fn run_resource_monitor_loop(
    done_flag: Arc<AtomicBool>,
    interval: Duration,
    config: MslCiEnvironment,
) {
    let mut last_cpu_sample = Instant::now();
    while !done_flag.load(Ordering::Relaxed) {
        thread::sleep(interval);
        if done_flag.load(Ordering::Relaxed) {
            break;
        }
        let include_cpu = last_cpu_sample.elapsed() >= MSL_RESOURCE_CPU_SAMPLE_INTERVAL;
        print_resource_snapshot("periodic", &config, include_cpu);
        if include_cpu {
            last_cpu_sample = Instant::now();
        }
    }
}

fn env_var_nonzero_u64(key: &str) -> Option<u64> {
    std::env::var(key)
        .ok()
        .as_deref()
        .and_then(parse_nonzero_u64)
}

fn env_var_bool(key: &str) -> bool {
    std::env::var(key)
        .ok()
        .as_deref()
        .is_some_and(parse_truthy_bool)
}

fn parse_nonzero_u64(raw: &str) -> Option<u64> {
    raw.trim().parse::<u64>().ok().filter(|value| *value > 0)
}

fn parse_truthy_bool(raw: &str) -> bool {
    matches!(
        raw.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

fn print_resource_snapshot(phase: &str, config: &MslCiEnvironment, include_cpu: bool) {
    eprintln!("== MSL Resource Snapshot ({phase}) ==");
    log_command_output("date", "date", ["-Is"]);
    eprintln!("workspace_root={}", config.root.display());
    if let Some(interval) = config.monitor_interval {
        eprintln!("resource_monitor_interval_secs={}", interval.as_secs());
    }
    log_command_output("nproc", "nproc", std::iter::empty::<&str>());
    log_command_output("uptime", "uptime", std::iter::empty::<&str>());
    log_command_output("free -h", "free", ["-h"]);
    log_command_output("df -h", "df", ["-h", ".", "/tmp"]);
    log_command_output("df -ih", "df", ["-ih", ".", "/tmp"]);
    log_path_size("target", &config.root.join("target"));
    log_path_size("target/msl", &config.root.join("target/msl"));
    log_path_size("msl_results", &config.results_dir);
    print_results_dir_summary("results-breakdown", &config.results_dir);
    log_top_processes("top-by-mem", "--sort=-%mem");
    if include_cpu {
        log_top_processes("top-by-cpu", "--sort=-%cpu");
    }
}

fn log_path_size(label: &str, path: &Path) {
    if !path.exists() {
        return;
    }
    let output = Command::new("du").arg("-sh").arg(path).output();
    match output {
        Ok(output) if output.status.success() => {
            let summary = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !summary.is_empty() {
                eprintln!("{label}: {summary} ({})", path.display());
            }
        }
        Ok(_) | Err(_) => {
            eprintln!("{label}: {}", path.display());
        }
    }
}

fn print_results_dir_summary(label: &str, results_dir: &Path) {
    if !results_dir.is_dir() {
        return;
    }
    eprintln!("{label}:");
    let entries = match fs::read_dir(results_dir) {
        Ok(entries) => entries,
        Err(error) => {
            eprintln!("  failed to read '{}': {error}", results_dir.display());
            return;
        }
    };
    for entry in entries.flatten() {
        let path = entry.path();
        log_path_size("  entry", &path);
    }
}

fn log_top_processes(label: &str, sort_flag: &str) {
    let output = Command::new("ps")
        .args(["-eo", "pid,ppid,%cpu,%mem,rss,vsz,etime,comm", sort_flag])
        .output();
    let Ok(output) = output else {
        return;
    };
    if !output.status.success() {
        return;
    }
    eprintln!("{label}:");
    for line in String::from_utf8_lossy(&output.stdout).lines().take(15) {
        eprintln!("  {line}");
    }
}

fn log_command_output<I, S>(label: &str, program: &str, args: I)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = Command::new(program).args(args).output();
    let Ok(output) = output else {
        return;
    };
    if !output.status.success() {
        return;
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    if stdout.trim().is_empty() {
        return;
    }
    eprintln!("{label}:");
    for line in stdout.lines() {
        eprintln!("  {line}");
    }
}

#[cfg(test)]
mod tests {
    use super::{
        MslCiEnvironment, MslHotspotModelResult, MslHotspotSummary, VERIFY_SUITE_STEPS,
        VerifySuite, hottest_compile_model, hottest_sim_model, parse_nonzero_u64,
        parse_truthy_bool,
    };
    use std::path::PathBuf;

    fn step_argvs(suite: VerifySuite) -> Vec<Vec<&'static str>> {
        VERIFY_SUITE_STEPS
            .iter()
            .filter(|step| suite.includes(step))
            .map(|step| step.args.to_vec())
            .collect()
    }

    #[test]
    fn quick_suite_skips_msl_parity() {
        let steps = step_argvs(VerifySuite::Quick);
        assert!(!steps.contains(&vec!["verify", "msl-parity"]));
        assert!(!steps.contains(&vec!["verify", "lsp-msl-completion-timings"]));
        assert!(steps.contains(&vec!["coverage", "run"]));
        assert!(steps.contains(&vec!["vscode", "test"]));
        assert!(steps.contains(&vec!["wasm", "test"]));
    }

    #[test]
    fn full_suite_includes_full_msl_parity() {
        let steps = step_argvs(VerifySuite::Full);
        assert!(steps.contains(&vec!["verify", "lsp-msl-completion-timings"]));
        assert!(steps.contains(&vec!["verify", "msl-parity"]));
        assert_eq!(steps.last(), Some(&vec!["verify", "msl-parity"]));
    }

    #[test]
    fn hotspot_selection_uses_max_compile_and_sim_wall_times() {
        let summary = MslHotspotSummary {
            model_results: vec![
                MslHotspotModelResult {
                    model_name: "A".to_string(),
                    compile_seconds: Some(1.5),
                    sim_wall_seconds: Some(8.0),
                },
                MslHotspotModelResult {
                    model_name: "B".to_string(),
                    compile_seconds: Some(3.0),
                    sim_wall_seconds: Some(2.0),
                },
                MslHotspotModelResult {
                    model_name: "C".to_string(),
                    compile_seconds: None,
                    sim_wall_seconds: Some(9.0),
                },
            ],
        };

        assert_eq!(hottest_compile_model(&summary), Some(("B", 3.0)));
        assert_eq!(hottest_sim_model(&summary), Some(("C", 9.0)));
    }

    #[test]
    fn bool_env_parser_accepts_common_truthy_values() {
        assert!(parse_truthy_bool("yes"));
        assert!(parse_truthy_bool("TRUE"));
        assert!(!parse_truthy_bool("no"));
    }

    #[test]
    fn nonzero_u64_env_parser_rejects_zero_and_invalid_values() {
        assert_eq!(parse_nonzero_u64("30"), Some(30));
        assert_eq!(parse_nonzero_u64("0"), None);
        assert_eq!(parse_nonzero_u64("abc"), None);
    }

    #[test]
    fn msl_ci_environment_cleans_stale_results_before_run() {
        let temp = tempfile::tempdir().expect("tempdir");
        let results_dir = temp.path().join("results");
        std::fs::create_dir_all(&results_dir).expect("mkdir");
        std::fs::write(results_dir.join("stale.json"), "{}").expect("write stale file");
        let env = MslCiEnvironment {
            root: PathBuf::from(temp.path()),
            results_dir: results_dir.clone(),
            monitor_interval: None,
            clean_results: true,
            github_actions: false,
        };
        env.clean_stale_results().expect("cleanup should succeed");
        assert!(
            !results_dir.exists(),
            "pre-run cleanup should remove stale results directory"
        );
    }

    #[test]
    fn msl_ci_environment_preserves_keyed_omc_parity_cache() {
        let temp = tempfile::tempdir().expect("tempdir");
        let results_dir = temp.path().join("results");
        let parity_cache_dir = results_dir.join("omc_parity_cache");
        std::fs::create_dir_all(&parity_cache_dir).expect("mkdir parity cache");
        std::fs::write(results_dir.join("stale.json"), "{}").expect("write stale file");
        std::fs::write(parity_cache_dir.join("compile.json"), "{}").expect("write cache file");

        let env = MslCiEnvironment {
            root: PathBuf::from(temp.path()),
            results_dir: results_dir.clone(),
            monitor_interval: None,
            clean_results: true,
            github_actions: false,
        };
        env.clean_stale_results().expect("cleanup should succeed");

        assert!(
            results_dir.is_dir(),
            "results dir should remain when keyed parity cache is preserved"
        );
        assert!(
            parity_cache_dir.join("compile.json").is_file(),
            "cleanup should preserve keyed OMC parity cache contents"
        );
        assert!(
            !results_dir.join("stale.json").exists(),
            "cleanup should remove stale non-cache artifacts"
        );
    }
}
