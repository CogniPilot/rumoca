mod completion_cmd;
mod coverage_analysis;
mod coverage_gate;
mod crate_dag_cmd;
mod lsp_benchmark_cmd;
#[cfg(test)]
mod main_tests;
mod msl_flamegraph_cmd;
#[path = "bin/msl_tools/mod.rs"]
mod msl_tools;
mod release_cmd;
mod repo_cli_cmd;
mod static_server;
mod test_cmd;
mod verify_cmd;
mod vscode_cmd;
mod wasm_tooling;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use anyhow::{Context, Result, bail, ensure};
use clap::{Args, CommandFactory, Parser, Subcommand};
use completion_cmd::CompletionsArgs;
use coverage_analysis::{
    CallsiteIndex, build_workspace_callsite_index, count_callsites_same_file,
    is_opaque_symbol_name, owner_decision_for_label, render_coverage_trim_report,
};
use coverage_gate::CoverageGateArgs;
use crate_dag_cmd::CrateDagArgs;
use rumoca_compile::compile::core::workspace_root_from_manifest_dir;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use verify_cmd::VerifyArgs;

pub(crate) use msl_tools::common;

#[derive(Debug, Parser)]
#[command(name = "rum")]
#[command(about = "Rumoca developer command")]
#[command(version)]
#[command(disable_help_subcommand = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Run verification jobs used locally and in CI
    Verify(VerifyArgs),
    /// VS Code extension workflows
    Vscode(VscodeArgs),
    /// WASM editor workflows
    Wasm(WasmArgs),
    /// Python binding workflows
    Python(PythonArgs),
    /// Coverage generation, reporting, and policy gates
    Coverage(CoverageArgs),
    /// Repository maintenance, packaging, and release workflows
    Repo(RepoArgs),
}

#[derive(Debug, Args, Clone)]
struct CheckRustFileLinesArgs {
    /// Maximum allowed lines in a Rust source file
    #[arg(long, default_value_t = 2000)]
    max_lines: usize,
    /// Check all tracked Rust files instead of only staged files
    #[arg(long)]
    all_files: bool,
}

#[derive(Debug, Args, Clone)]
struct BuildPythonArgs {
    /// Build release wheel and install it
    #[arg(long)]
    release: bool,
}

#[derive(Debug, Args, Clone)]
struct VscodeBuildArgs {
    /// Package only; do not install the extension locally
    #[arg(long)]
    no_install: bool,
    /// Use system rumoca-lsp; skip cargo build/copy
    #[arg(long, short = 's')]
    system: bool,
}

#[derive(Debug, Args, Clone)]
struct VscodePackageArgs {
    /// Release package target
    #[arg(long, value_enum)]
    target: vscode_cmd::VscodePackageTarget,
    /// On Debian/Ubuntu, install musl-tools automatically when required
    #[arg(long)]
    install_musl_tools: bool,
}

#[derive(Debug, Args, Clone)]
struct VscodeInstallCheckArgs {
    /// Reuse the latest existing VSIX instead of rebuilding/packaging
    #[arg(long)]
    no_build: bool,
    /// Use system rumoca-lsp when rebuilding the VSIX
    #[arg(long, short = 's')]
    system: bool,
    /// Root directory for the isolated VS Code profile
    #[arg(long, value_name = "DIR")]
    profile_root: Option<PathBuf>,
    /// Modelica file to open after installing the VSIX
    #[arg(long, value_name = "FILE")]
    document: Option<PathBuf>,
    /// Install the VSIX into the isolated profile without opening VS Code
    #[arg(long)]
    no_open: bool,
}

#[derive(Debug, Args, Clone)]
struct VscodeHostArgs {
    /// Skip rebuilding/copying rumoca-lsp into editors/vscode/bin
    #[arg(long)]
    skip_lsp_build: bool,
    /// Skip TypeScript esbuild watch process
    #[arg(long)]
    no_ts_watch: bool,
    /// Directory to open in the Extension Development Host (defaults to repo root)
    #[arg(long, short = 'w', value_name = "DIR")]
    workspace_dir: Option<PathBuf>,
}

#[derive(Debug, Args, Clone)]
struct VscodeArgs {
    #[command(subcommand)]
    command: VscodeCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum VscodeCommand {
    /// Build/package/install the VS Code extension
    Build(VscodeBuildArgs),
    /// Package a platform-specific VSIX with bundled release binaries
    Package(VscodePackageArgs),
    /// Build/package the VSIX if needed, install it into an isolated profile, and open a .mo file
    InstallCheck(VscodeInstallCheckArgs),
    /// VS Code extension verification gate
    Test,
    /// Watch Rust/TypeScript and launch the Extension Development Host
    Edit(VscodeHostArgs),
}

#[derive(Debug, Args, Clone)]
struct WasmArgs {
    #[command(subcommand)]
    command: WasmCommand,
}

#[derive(Debug, Args, Clone)]
struct WasmBuildArgs {
    /// Build with wasm-pack --dev for faster local iteration
    #[arg(long)]
    dev: bool,
}

#[derive(Debug, Subcommand, Clone)]
enum WasmCommand {
    /// Build the WASM editor bundle
    Build(WasmBuildArgs),
    /// WASM editor verification gate
    Test,
    /// Build and serve the WASM editor
    Edit(WasmEditArgs),
    /// Clean generated WASM artifacts
    Clean,
}

#[derive(Debug, Args, Clone)]
struct WasmEditArgs {
    /// Override serve port (default: PORT env or 8080)
    #[arg(long)]
    port: Option<u16>,
    /// Serve existing WASM assets without rebuilding first
    #[arg(long)]
    skip_build: bool,
}

#[derive(Debug, Args, Clone)]
struct PythonArgs {
    #[command(subcommand)]
    command: PythonCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum PythonCommand {
    /// Build/install Python bindings via maturin
    Build(BuildPythonArgs),
}

#[derive(Debug, Args, Clone)]
struct CoverageArgs {
    #[command(subcommand)]
    command: CoverageCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum CoverageCommand {
    /// Generate standardized unified workspace coverage artifacts in target/llvm-cov
    Run(CoverageRunArgs),
    /// Generate per-package inventory and trim candidates from unified workspace llvm-cov JSON
    Report(CoverageReportArgs),
    /// Enforce coverage-trim regression thresholds against committed baseline
    Gate(CoverageGateArgs),
}

#[derive(Debug, Args, Clone)]
struct CoverageRunArgs {
    /// Reuse llvm-cov artifacts between runs for faster iteration.
    #[arg(long)]
    no_clean: bool,
    /// Also run ignored tests (e.g. slow MSL suites) into the same coverage dataset.
    #[arg(long)]
    include_ignored: bool,
    /// Restrict coverage run to selected workspace package(s).
    #[arg(long = "package", short = 'p')]
    packages: Vec<String>,
}

#[derive(Debug, Args, Clone)]
struct CoverageReportArgs {
    /// Workspace package(s) to report. If omitted, all workspace packages are shown.
    #[arg(long = "package", short = 'p')]
    packages: Vec<String>,
    /// Number of lowest-coverage files to include per package.
    #[arg(long, default_value_t = 10)]
    top_files: usize,
    /// "Near-zero" callsite threshold for dead helper detection.
    #[arg(long, default_value_t = 1)]
    near_zero_callsites: usize,
}

#[derive(Debug, Args, Clone)]
struct ReleaseArgs {
    /// Release version (X.Y.Z)
    version: String,
    /// Dry-run: print actions without writing files
    #[arg(long)]
    dry_run: bool,
    /// Allow dirty working tree
    #[arg(long)]
    allow_dirty: bool,
    /// Create commit after version bump
    #[arg(long)]
    commit: bool,
    /// Create git tag vX.Y.Z
    #[arg(long)]
    tag: bool,
    /// Push main and tag in one git push (implies --commit --tag)
    #[arg(long)]
    push: bool,
}

#[derive(Debug, Args, Clone)]
struct RepoArgs {
    #[command(subcommand)]
    command: RepoCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum RepoCommand {
    /// Install/update the rum CLI and shell completions
    Cli(RepoCliArgs),
    /// Ubuntu/Debian helper commands for local developer prerequisites
    Ubuntu(RepoUbuntuArgs),
    /// Repository git hook workflows
    Hooks(RepoHooksArgs),
    /// Workspace graphing utilities
    Graph(RepoGraphArgs),
    /// MSL/OMC reference, baseline, and parity-maintenance tooling
    Msl(RepoMslArgs),
    /// Shell completion workflows
    Completions(RepoCompletionsArgs),
    /// Release version bump, optional commit/tag/push
    Release(ReleaseArgs),
    /// Repository policy helpers
    Policy(RepoPolicyArgs),
}

#[derive(Debug, Args, Clone)]
struct RepoCliArgs {
    #[command(subcommand)]
    command: RepoCliCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum RepoCliCommand {
    /// Install/update the rum launcher and configure shell completions
    Install(RepoCliInstallArgs),
}

#[derive(Debug, Args, Clone)]
struct RepoCliInstallArgs {
    /// Also persist the cargo bin directory into your shell PATH configuration
    #[arg(long)]
    path: bool,
}

#[derive(Debug, Args, Clone)]
struct RepoCompletionsArgs {
    #[command(subcommand)]
    command: RepoCompletionsCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum RepoCompletionsCommand {
    /// Print a shell completion script to stdout
    Print(CompletionsArgs),
    /// Install shell completions for the current or specified shell
    Install(RepoCompletionsInstallArgs),
}

#[derive(Debug, Args, Clone)]
struct RepoCompletionsInstallArgs {
    /// Shell to install for; defaults to the current shell when omitted
    #[arg(value_enum)]
    shell: Option<completion_cmd::ShellKind>,
}

#[derive(Debug, Args, Clone)]
struct RepoHooksArgs {
    #[command(subcommand)]
    command: RepoHooksCommand,
}

#[derive(Debug, Args, Clone)]
struct RepoUbuntuArgs {
    #[command(subcommand)]
    command: RepoUbuntuCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum RepoUbuntuCommand {
    /// Install headless VS Code smoke prerequisites (`xvfb`, `xauth`) via apt
    InstallVscodeSmokePrereqs(RepoUbuntuInstallArgs),
}

#[derive(Debug, Args, Clone)]
struct RepoUbuntuInstallArgs {
    /// Skip `apt-get update` before installing packages
    #[arg(long)]
    no_update: bool,
}

#[derive(Debug, Subcommand, Clone)]
enum RepoHooksCommand {
    /// Configure repository git hooks
    Install,
}

#[derive(Debug, Args, Clone)]
struct RepoGraphArgs {
    #[command(subcommand)]
    command: RepoGraphCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum RepoGraphCommand {
    /// Generate workspace crate dependency DAG plots (html/dot/svg/png)
    Crates(CrateDagArgs),
}

#[derive(Debug, Args, Clone)]
struct RepoMslArgs {
    #[command(subcommand)]
    command: RepoMslCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum RepoMslCommand {
    /// Generate OMC compile/check reference data
    OmcReference(msl_tools::omc_reference::Args),
    /// Generate OMC simulation reference data and trace comparisons
    OmcSimulationReference(msl_tools::omc_simulation_reference::Args),
    /// Run cargo-flamegraph for one focused MSL compile/simulation model
    Flamegraph(msl_flamegraph_cmd::MslFlamegraphArgs),
    /// Compare rumoca balance output against OMC reference data
    CompareBalance(msl_tools::compare_balance::Args),
    /// Build rumoca-vs-OMC simulation parity failure manifest
    ParityManifest(msl_tools::parity_manifest::Args),
    /// Regenerate OMC+Rumoca traces and plot them for one model
    PlotCompare(msl_tools::plot_compare::Args),
    /// Promote target quality snapshot to committed baseline JSON
    PromoteQualityBaseline(msl_tools::promote_quality_baseline::Args),
}

#[derive(Debug, Args, Clone)]
struct RepoPolicyArgs {
    #[command(subcommand)]
    command: RepoPolicyCommand,
}

#[derive(Debug, Subcommand, Clone)]
enum RepoPolicyCommand {
    /// Check max line count for Rust files (SPEC_0021)
    RustFileLines(CheckRustFileLinesArgs),
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Verify(args) => verify_cmd::run(args, &repo_root()),
        Commands::Vscode(args) => cmd_vscode(args),
        Commands::Wasm(args) => cmd_wasm(args),
        Commands::Python(args) => cmd_python(args),
        Commands::Coverage(args) => cmd_coverage(args),
        Commands::Repo(args) => cmd_repo(args),
    }
}

fn cmd_vscode(args: VscodeArgs) -> Result<()> {
    match args.command {
        VscodeCommand::Build(args) => vscode_cmd::build_vscode_ext(args),
        VscodeCommand::Package(args) => vscode_cmd::package_vscode_ext(args),
        VscodeCommand::InstallCheck(args) => vscode_cmd::install_check_vscode_ext(args),
        VscodeCommand::Test => vscode_cmd::run_vscode_ci(&repo_root()),
        VscodeCommand::Edit(args) => vscode_cmd::vscode_dev(args),
    }
}

fn cmd_wasm(args: WasmArgs) -> Result<()> {
    match args.command {
        WasmCommand::Build(args) => cmd_build_wasm(args),
        WasmCommand::Test => run_wasm_test_suite(&repo_root()),
        WasmCommand::Edit(args) => {
            let root = repo_root();
            if !args.skip_build {
                ensure_wasm_deps(&root)?;
                build_wasm(&root, WasmBuildProfile::Release)?;
            }
            serve_wasm(&root, args.port)
        }
        WasmCommand::Clean => clean_wasm(&repo_root()),
    }
}

fn cmd_python(args: PythonArgs) -> Result<()> {
    match args.command {
        PythonCommand::Build(args) => cmd_build_python(args),
    }
}

fn cmd_coverage(args: CoverageArgs) -> Result<()> {
    match args.command {
        CoverageCommand::Run(args) => cmd_coverage_run(args),
        CoverageCommand::Report(args) => cmd_coverage_report(args),
        CoverageCommand::Gate(args) => cmd_coverage_gate(args),
    }
}

fn cmd_repo(args: RepoArgs) -> Result<()> {
    match args.command {
        RepoCommand::Cli(args) => match args.command {
            RepoCliCommand::Install(args) => repo_cli_cmd::cmd_install_rum_cli(args),
        },
        RepoCommand::Ubuntu(args) => match args.command {
            RepoUbuntuCommand::InstallVscodeSmokePrereqs(args) => {
                repo_cli_cmd::cmd_install_ubuntu_vscode_smoke_prereqs(args)
            }
        },
        RepoCommand::Hooks(args) => match args.command {
            RepoHooksCommand::Install => cmd_install_git_hooks(),
        },
        RepoCommand::Graph(args) => match args.command {
            RepoGraphCommand::Crates(args) => crate_dag_cmd::run(&repo_root(), args),
        },
        RepoCommand::Msl(args) => match args.command {
            RepoMslCommand::OmcReference(args) => msl_tools::omc_reference::run(args),
            RepoMslCommand::OmcSimulationReference(args) => {
                msl_tools::omc_simulation_reference::run(args)
            }
            RepoMslCommand::Flamegraph(args) => msl_flamegraph_cmd::run(args, &repo_root()),
            RepoMslCommand::CompareBalance(args) => msl_tools::compare_balance::run(args),
            RepoMslCommand::ParityManifest(args) => msl_tools::parity_manifest::run(args),
            RepoMslCommand::PlotCompare(args) => msl_tools::plot_compare::run(args),
            RepoMslCommand::PromoteQualityBaseline(args) => {
                msl_tools::promote_quality_baseline::run(args)
            }
        },
        RepoCommand::Completions(args) => match args.command {
            RepoCompletionsCommand::Print(args) => {
                let mut command = Cli::command();
                completion_cmd::run(args, &mut command)
            }
            RepoCompletionsCommand::Install(args) => {
                repo_cli_cmd::cmd_install_shell_completions(args)
            }
        },
        RepoCommand::Release(args) => release_cmd::cmd_release(args),
        RepoCommand::Policy(args) => match args.command {
            RepoPolicyCommand::RustFileLines(args) => cmd_check_rust_file_lines(args),
        },
    }
}

fn repo_root() -> PathBuf {
    let root = workspace_root_from_manifest_dir(env!("CARGO_MANIFEST_DIR"));
    root.canonicalize().unwrap_or(root)
}

fn is_windows() -> bool {
    cfg!(windows)
}

pub(crate) fn exe_name(base: &str) -> String {
    if is_windows() {
        format!("{base}.exe")
    } else {
        base.to_string()
    }
}

pub(crate) fn run_status(mut command: Command) -> Result<()> {
    let rendered = format!("{command:?}");
    let status = command
        .status()
        .with_context(|| format!("failed to run command: {rendered}"))?;
    if !status.success() {
        bail!("command failed (status={status}): {rendered}");
    }
    Ok(())
}

pub(crate) fn run_status_quiet(mut command: Command) -> Result<()> {
    let rendered = format!("{command:?}");
    let output = command
        .output()
        .with_context(|| format!("failed to run command: {rendered}"))?;
    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!(
            "command failed (status={}): {}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            rendered,
            stdout,
            stderr
        );
    }
    Ok(())
}

fn run_capture(mut command: Command) -> Result<String> {
    let rendered = format!("{command:?}");
    let output = command
        .output()
        .with_context(|| format!("failed to run command: {rendered}"))?;
    if !output.status.success() {
        bail!(
            "command failed (status={}): {}\n{}",
            output.status,
            rendered,
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

pub(crate) fn command_exists(program: &str) -> bool {
    Command::new(program)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

fn cmd_check_rust_file_lines(args: CheckRustFileLinesArgs) -> Result<()> {
    let root = repo_root();
    let mut cmd = Command::new("git");
    if args.all_files {
        cmd.arg("ls-files")
            .arg("-z")
            .arg("--")
            .arg("*.rs")
            .current_dir(&root);
    } else {
        cmd.arg("diff")
            .arg("--cached")
            .arg("--name-only")
            .arg("--diff-filter=ACMR")
            .arg("-z")
            .arg("--")
            .arg("*.rs")
            .current_dir(&root);
    }
    let output = cmd.output().context("failed to query Rust files")?;
    ensure!(
        output.status.success(),
        "git diff failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let mut rust_files = output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|chunk| !chunk.is_empty())
        .filter_map(|chunk| std::str::from_utf8(chunk).ok())
        .collect::<Vec<_>>();
    rust_files.sort_unstable();

    if rust_files.is_empty() {
        if args.all_files {
            println!("Rust file line-count check: no tracked Rust files.");
        } else {
            println!("Rust file line-count check: no staged Rust files.");
        }
        return Ok(());
    }

    let mut violations = Vec::new();
    for rel in rust_files {
        if rel.contains("/generated/") || rel.contains("\\generated\\") {
            continue;
        }
        let path = root.join(rel);
        if !path.is_file() {
            continue;
        }
        let file =
            fs::File::open(&path).with_context(|| format!("failed to open {}", path.display()))?;
        let line_count = BufReader::new(file).lines().count();
        let max_allowed = args.max_lines;
        if line_count > max_allowed {
            violations.push((rel.to_string(), line_count, max_allowed));
        }
    }

    if violations.is_empty() {
        println!(
            "Rust file line-count check passed (max {} lines).",
            args.max_lines
        );
        return Ok(());
    }

    for (file, line_count, max_allowed) in &violations {
        eprintln!("ERROR: {file} has {line_count} lines (max allowed: {max_allowed}).");
    }
    bail!("Rust file line-count check failed. Split oversized Rust files before committing.");
}

fn cmd_install_git_hooks() -> Result<()> {
    let root = repo_root();
    for hook in ["pre-commit", "pre-push"] {
        let hook_path = root.join(".githooks").join(hook);
        ensure!(hook_path.is_file(), "missing hook: {}", hook_path.display());
        make_executable(&hook_path)?;
    }

    let mut cmd = Command::new("git");
    cmd.arg("config")
        .arg("core.hooksPath")
        .arg(root.join(".githooks"))
        .current_dir(&root);
    run_status(cmd)?;

    println!("Git hooks installed.");
    println!(
        "Configured core.hooksPath={}",
        root.join(".githooks").display()
    );
    println!("Installed hooks:\n  - pre-commit\n  - pre-push");
    Ok(())
}

fn cmd_build_python(args: BuildPythonArgs) -> Result<()> {
    let root = repo_root();
    let python_dir = root.join("bindings/python");
    ensure!(
        python_dir.is_dir(),
        "missing python bindings dir: {}",
        python_dir.display()
    );
    ensure!(
        command_exists("maturin"),
        "maturin not found. Install with: pip install maturin"
    );

    if args.release {
        println!("Building release Python wheel...");
        let mut build = Command::new("maturin");
        build.arg("build").arg("--release").current_dir(&python_dir);
        run_status(build)?;

        let wheels_dir = root.join("target/wheels");
        let wheel = newest_file_with_ext(&wheels_dir, "whl")?.with_context(|| {
            format!(
                "no wheel found in {} after maturin build --release",
                wheels_dir.display()
            )
        })?;
        println!("Installing wheel {}...", wheel.display());
        let mut pip = Command::new("pip");
        pip.arg("install")
            .arg("--force-reinstall")
            .arg(&wheel)
            .current_dir(&root);
        run_status(pip)?;
    } else {
        println!("Building and installing Python package in development mode...");
        let mut develop = Command::new("maturin");
        develop.arg("develop").current_dir(&python_dir);
        run_status(develop)?;
    }

    let mut verify = Command::new("python");
    verify
        .arg("-c")
        .arg("import rumoca; print(rumoca.__version__)")
        .current_dir(&root);
    match verify.output() {
        Ok(output) if output.status.success() => {
            println!(
                "Installed rumoca Python version: {}",
                String::from_utf8_lossy(&output.stdout).trim()
            );
        }
        _ => {
            println!("Warning: could not verify installed Python package version.");
        }
    }

    Ok(())
}

fn cmd_coverage_run(args: CoverageRunArgs) -> Result<()> {
    let root = repo_root();
    ensure_cargo_llvm_cov_available(&root)?;
    ensure_llvm_tools_preview_available(&root)?;

    let output_dir = root.join("target/llvm-cov");
    fs::create_dir_all(&output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;

    let mut runbook = String::new();
    runbook.push_str("# rumoca coverage runbook\n");
    runbook.push_str("# generated by rum coverage run\n\n");
    let package_args = coverage_package_args(&args.packages);
    let mut full_args = vec!["llvm-cov".to_string()];
    if args.packages.is_empty() {
        full_args.push("--workspace".to_string());
    }
    full_args.extend([
        "--tests".to_string(),
        "--json".to_string(),
        "--output-path".to_string(),
        "target/llvm-cov/workspace-full.json".to_string(),
    ]);
    full_args.extend(package_args.clone());
    if args.no_clean {
        full_args.push("--no-clean".to_string());
    }
    let mut summary_args = vec![
        "llvm-cov".to_string(),
        "report".to_string(),
        "--json".to_string(),
        "--summary-only".to_string(),
        "--output-path".to_string(),
        "target/llvm-cov/workspace-summary.json".to_string(),
    ];
    summary_args.extend(package_args.clone());

    println!("coverage: workspace (tests)");
    runbook.push_str("cargo ");
    runbook.push_str(&full_args.join(" "));
    runbook.push('\n');
    run_cargo_with_args(&root, &full_args)?;

    if args.include_ignored {
        let mut ignored_args = vec!["llvm-cov".to_string()];
        if args.packages.is_empty() {
            ignored_args.push("--workspace".to_string());
        }
        ignored_args.extend([
            "--tests".to_string(),
            "--json".to_string(),
            "--output-path".to_string(),
            "target/llvm-cov/workspace-full.json".to_string(),
        ]);
        ignored_args.extend(package_args.clone());
        ignored_args.extend([
            "--no-clean".to_string(),
            "--".to_string(),
            "--ignored".to_string(),
        ]);
        println!("coverage: workspace (ignored tests)");
        runbook.push_str("cargo ");
        runbook.push_str(&ignored_args.join(" "));
        runbook.push('\n');
        run_cargo_with_args(&root, &ignored_args)?;
    }

    println!("coverage: workspace (summary)");
    runbook.push_str("cargo ");
    runbook.push_str(&summary_args.join(" "));
    runbook.push('\n');
    run_cargo_with_args(&root, &summary_args)?;

    let runbook_path = output_dir.join("coverage-commands.txt");
    fs::write(&runbook_path, runbook)
        .with_context(|| format!("failed to write {}", runbook_path.display()))?;

    println!("Coverage artifacts generated in {}", output_dir.display());
    println!("Runbook: {}", runbook_path.display());
    Ok(())
}

fn coverage_package_args(packages: &[String]) -> Vec<String> {
    packages
        .iter()
        .flat_map(|package| ["--package".to_string(), package.clone()])
        .collect()
}

fn cmd_coverage_gate(args: CoverageGateArgs) -> Result<()> {
    coverage_gate::run(&repo_root(), &args)
}

#[derive(Debug, Clone)]
struct CoverageFileSummary {
    file: String,
    line_percent: f64,
    line_count: u64,
    line_covered: u64,
}

#[derive(Debug, Clone)]
struct CoverageCandidate {
    package: String,
    file: String,
    line: usize,
    demangled_function: String,
    symbol: Option<String>,
    visibility: String,
    has_cfg_attr: bool,
    callsites_same_file: Option<usize>,
    callsites_workspace: Option<usize>,
    callsites_other_crates: Option<usize>,
    triage_label: String,
    owner_decision: String,
}

#[derive(Debug, Clone)]
struct PackageCoverageInventory {
    package: String,
    lowest_files: Vec<CoverageFileSummary>,
    zero_count_functions_total: usize,
    candidates: Vec<CoverageCandidate>,
    triage_counts: BTreeMap<String, usize>,
}

#[derive(Debug, Clone)]
struct WorkspacePackageInfo {
    name: String,
    root_prefix: String,
}

type ZeroCoverageTotalsByPackage = HashMap<String, usize>;
type CandidateListsByPackage = HashMap<String, Vec<CoverageCandidate>>;

fn cmd_coverage_report(args: CoverageReportArgs) -> Result<()> {
    let root = repo_root();
    let output_dir = root.join("target/llvm-cov");
    ensure!(
        output_dir.is_dir(),
        "coverage output directory not found: {}. Run `rum coverage run` first.",
        output_dir.display()
    );
    let summary_path = output_dir.join("workspace-summary.json");
    let full_path = output_dir.join("workspace-full.json");
    ensure!(
        summary_path.is_file() && full_path.is_file(),
        "missing workspace coverage artifacts (expected {} and {}). Run `rum coverage run` first.",
        summary_path.display(),
        full_path.display()
    );

    let package_infos = workspace_package_infos(&root)?;
    let selected_packages = if args.packages.is_empty() {
        package_infos.clone()
    } else {
        let requested = args.packages.into_iter().collect::<BTreeSet<_>>();
        let package_map = package_infos
            .iter()
            .map(|info| (info.name.clone(), info.clone()))
            .collect::<BTreeMap<_, _>>();
        let mut selected = Vec::new();
        for package in requested {
            let Some(info) = package_map.get(&package) else {
                bail!("unknown workspace package in --package filter: {package}");
            };
            selected.push(info.clone());
        }
        selected
    };
    ensure!(
        !selected_packages.is_empty(),
        "no workspace packages available for coverage report"
    );
    let summary_json = read_json_file(&summary_path)?;
    let full_json = read_json_file(&full_path)?;

    let (inventories, all_candidates) = collect_coverage_inventories(
        &root,
        &selected_packages,
        &summary_json,
        &full_json,
        args.top_files,
        args.near_zero_callsites,
    )?;
    ensure!(
        all_candidates
            .iter()
            .all(|candidate| !candidate.owner_decision.is_empty()),
        "coverage report candidate generation produced empty owner decisions"
    );
    ensure!(
        all_candidates
            .iter()
            .all(|candidate| candidate.owner_decision != "untriaged"),
        "coverage report candidate generation produced untriaged owner decisions"
    );
    let (report_path, candidates_path) =
        write_coverage_report_artifacts(&output_dir, &inventories, &all_candidates)?;

    println!("Coverage inventory report: {}", report_path.display());
    println!("Trim candidates JSON: {}", candidates_path.display());
    Ok(())
}

fn collect_coverage_inventories(
    root: &Path,
    package_infos: &[WorkspacePackageInfo],
    summary_json: &serde_json::Value,
    full_json: &serde_json::Value,
    top_files: usize,
    near_zero_callsites: usize,
) -> Result<(Vec<PackageCoverageInventory>, Vec<CoverageCandidate>)> {
    let mut source_cache = HashMap::new();
    let callsite_index = build_workspace_callsite_index(root, package_infos)?;
    let mut inventories = Vec::new();
    let mut all_candidates = Vec::new();
    let mut files_by_package = summary_file_entries_by_package(root, package_infos, summary_json)?;
    let (zero_totals_by_package, mut candidates_by_package) = zero_count_candidates_by_package(
        root,
        package_infos,
        full_json,
        near_zero_callsites,
        &mut source_cache,
        &callsite_index,
    )?;

    for package in package_infos {
        let mut lowest_files = files_by_package.remove(&package.name).unwrap_or_default();
        lowest_files.sort_by(|a, b| {
            a.line_percent
                .total_cmp(&b.line_percent)
                .then(a.file.cmp(&b.file))
        });
        lowest_files.truncate(top_files);

        let mut candidates = candidates_by_package
            .remove(&package.name)
            .unwrap_or_default();
        candidates.sort_by(|a, b| {
            a.file
                .cmp(&b.file)
                .then(a.line.cmp(&b.line))
                .then(a.demangled_function.cmp(&b.demangled_function))
        });
        candidates.dedup_by(|a, b| {
            a.file == b.file
                && a.line == b.line
                && a.symbol == b.symbol
                && a.triage_label == b.triage_label
        });
        all_candidates.extend(candidates.clone());

        let mut triage_counts = BTreeMap::new();
        for candidate in &candidates {
            *triage_counts
                .entry(candidate.triage_label.clone())
                .or_insert(0) += 1;
        }

        inventories.push(PackageCoverageInventory {
            package: package.name.clone(),
            lowest_files,
            zero_count_functions_total: zero_totals_by_package
                .get(&package.name)
                .copied()
                .unwrap_or(0),
            candidates,
            triage_counts,
        });
    }

    inventories.sort_by(|a, b| a.package.cmp(&b.package));
    all_candidates.sort_by(|a, b| {
        a.package
            .cmp(&b.package)
            .then(a.file.cmp(&b.file))
            .then(a.line.cmp(&b.line))
            .then(a.demangled_function.cmp(&b.demangled_function))
    });
    Ok((inventories, all_candidates))
}

fn write_coverage_report_artifacts(
    output_dir: &Path,
    inventories: &[PackageCoverageInventory],
    all_candidates: &[CoverageCandidate],
) -> Result<(PathBuf, PathBuf)> {
    let report_md = render_coverage_trim_report(inventories);
    let report_path = output_dir.join("coverage-trim-report.md");
    fs::write(&report_path, report_md)
        .with_context(|| format!("failed to write {}", report_path.display()))?;

    let candidates_json = build_trim_candidates_json(inventories, all_candidates);
    let candidates_path = output_dir.join("trim-candidates.json");
    let candidates_serialized =
        serde_json::to_string_pretty(&candidates_json).context("failed to serialize candidates")?;
    fs::write(&candidates_path, candidates_serialized)
        .with_context(|| format!("failed to write {}", candidates_path.display()))?;
    Ok((report_path, candidates_path))
}

fn build_trim_candidates_json(
    inventories: &[PackageCoverageInventory],
    all_candidates: &[CoverageCandidate],
) -> serde_json::Value {
    serde_json::json!({
        "generated_by": "rum coverage report",
        "generated_at_unix_secs": unix_timestamp_seconds(),
        "report_version": 1,
        "packages": inventories.iter().map(|inventory| {
            serde_json::json!({
                "package": inventory.package,
                "zero_count_functions_total": inventory.zero_count_functions_total,
                "lowest_coverage_files": inventory.lowest_files.iter().map(|file| {
                    serde_json::json!({
                        "file": file.file,
                        "line_percent": file.line_percent,
                        "line_count": file.line_count,
                        "line_covered": file.line_covered,
                    })
                }).collect::<Vec<_>>(),
                "triage_counts": inventory.triage_counts,
            })
        }).collect::<Vec<_>>(),
        "candidates": all_candidates.iter().map(|candidate| {
            serde_json::json!({
                "package": candidate.package,
                "file": candidate.file,
                "line": candidate.line,
                "demangled_function": candidate.demangled_function,
                "symbol": candidate.symbol,
                "visibility": candidate.visibility,
                "has_cfg_attr": candidate.has_cfg_attr,
                "callsites_same_file": candidate.callsites_same_file,
                "callsites_workspace": candidate.callsites_workspace,
                "callsites_other_crates": candidate.callsites_other_crates,
                "triage_label": candidate.triage_label,
                "owner_decision": candidate.owner_decision,
            })
        }).collect::<Vec<_>>(),
    })
}

fn read_json_file(path: &Path) -> Result<serde_json::Value> {
    let raw =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    serde_json::from_str(&raw).with_context(|| format!("failed to parse JSON {}", path.display()))
}

fn summary_file_entries_by_package(
    root: &Path,
    package_infos: &[WorkspacePackageInfo],
    summary_json: &serde_json::Value,
) -> Result<HashMap<String, Vec<CoverageFileSummary>>> {
    let files = summary_json
        .get("data")
        .and_then(serde_json::Value::as_array)
        .and_then(|data| data.first())
        .and_then(|first| first.get("files"))
        .and_then(serde_json::Value::as_array)
        .context("summary JSON missing data[0].files")?;

    let mut entries_by_package: HashMap<String, Vec<CoverageFileSummary>> = HashMap::new();
    for file in files {
        let filename = match file.get("filename").and_then(serde_json::Value::as_str) {
            Some(value) => value,
            None => continue,
        };
        let Some(package) = package_for_filename(root, package_infos, filename) else {
            continue;
        };

        let line_summary = match file
            .get("summary")
            .and_then(|summary| summary.get("lines"))
            .and_then(serde_json::Value::as_object)
        {
            Some(value) => value,
            None => continue,
        };

        let line_percent = line_summary
            .get("percent")
            .and_then(serde_json::Value::as_f64)
            .unwrap_or(0.0);
        let line_count = line_summary
            .get("count")
            .and_then(serde_json::Value::as_u64)
            .unwrap_or(0);
        let line_covered = line_summary
            .get("covered")
            .and_then(serde_json::Value::as_u64)
            .unwrap_or(0);
        entries_by_package
            .entry(package.name.clone())
            .or_default()
            .push(CoverageFileSummary {
                file: relativize_path(root, filename),
                line_percent,
                line_count,
                line_covered,
            });
    }
    Ok(entries_by_package)
}

fn zero_count_candidates_by_package(
    root: &Path,
    package_infos: &[WorkspacePackageInfo],
    full_json: &serde_json::Value,
    near_zero_callsites: usize,
    source_cache: &mut HashMap<PathBuf, String>,
    callsite_index: &CallsiteIndex,
) -> Result<(ZeroCoverageTotalsByPackage, CandidateListsByPackage)> {
    let functions = full_json
        .get("data")
        .and_then(serde_json::Value::as_array)
        .and_then(|data| data.first())
        .and_then(|first| first.get("functions"))
        .and_then(serde_json::Value::as_array)
        .context("full JSON missing data[0].functions")?;

    let mut zero_totals_by_package: ZeroCoverageTotalsByPackage = HashMap::new();
    let mut candidates_by_package: CandidateListsByPackage = HashMap::new();
    for function in functions {
        if function
            .get("count")
            .and_then(serde_json::Value::as_u64)
            .unwrap_or(0)
            != 0
        {
            continue;
        }
        let Some((package_name, candidate)) = zero_count_candidate_for_function(
            root,
            package_infos,
            function,
            near_zero_callsites,
            source_cache,
            callsite_index,
        ) else {
            continue;
        };
        *zero_totals_by_package
            .entry(package_name.clone())
            .or_insert(0) += 1;
        candidates_by_package
            .entry(package_name)
            .or_default()
            .push(candidate);
    }

    Ok((zero_totals_by_package, candidates_by_package))
}

fn zero_count_candidate_for_function(
    root: &Path,
    package_infos: &[WorkspacePackageInfo],
    function: &serde_json::Value,
    near_zero_callsites: usize,
    source_cache: &mut HashMap<PathBuf, String>,
    callsite_index: &CallsiteIndex,
) -> Option<(String, CoverageCandidate)> {
    let file = function
        .get("filenames")
        .and_then(serde_json::Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(serde_json::Value::as_str)
        .find(|filename| package_for_filename(root, package_infos, filename).is_some())?;
    let package = package_for_filename(root, package_infos, file)?;
    let line = function
        .get("regions")
        .and_then(serde_json::Value::as_array)
        .and_then(|regions| regions.first())
        .and_then(serde_json::Value::as_array)
        .and_then(|region| region.first())
        .and_then(serde_json::Value::as_u64)
        .unwrap_or(0) as usize;
    let raw_function = function
        .get("name")
        .and_then(serde_json::Value::as_str)
        .unwrap_or("<unknown>")
        .to_string();
    let demangled_function = demangle_cov_function_name(&raw_function);
    let symbol = extract_symbol_name(&demangled_function);
    let (declaration_name, has_cfg_attr, visibility, callsites_same_file) =
        candidate_symbol_metadata(root, file, line, symbol, source_cache)?;
    if declaration_name
        .as_deref()
        .is_some_and(is_opaque_symbol_name)
    {
        return None;
    }
    let callsites_workspace = declaration_name
        .as_deref()
        .and_then(|name| callsite_index.workspace_callsites(name));
    let callsites_other_crates = declaration_name
        .as_deref()
        .and_then(|name| callsite_index.other_crate_callsites(&package.name, name));
    let triage_label = classify_candidate(
        visibility.as_str(),
        has_cfg_attr,
        callsites_workspace,
        near_zero_callsites,
    )
    .to_string();
    let owner_decision = owner_decision_for_label(&triage_label).to_string();
    Some((
        package.name.clone(),
        CoverageCandidate {
            package: package.name.clone(),
            file: relativize_path(root, file),
            line,
            demangled_function,
            symbol: declaration_name,
            visibility,
            has_cfg_attr,
            callsites_same_file,
            callsites_workspace,
            callsites_other_crates,
            triage_label,
            owner_decision,
        },
    ))
}

fn package_for_filename<'a>(
    root: &Path,
    package_infos: &'a [WorkspacePackageInfo],
    filename: &str,
) -> Option<&'a WorkspacePackageInfo> {
    let rel = relativize_path(root, filename);
    package_infos
        .iter()
        .find(|package| rel.starts_with(&package.root_prefix))
}

fn candidate_symbol_metadata(
    root: &Path,
    filename: &str,
    line: usize,
    symbol: Option<String>,
    source_cache: &mut HashMap<PathBuf, String>,
) -> Option<(Option<String>, bool, String, Option<usize>)> {
    let source_file = {
        let path = Path::new(filename);
        if path.is_absolute() {
            path.to_path_buf()
        } else {
            root.join(path)
        }
    };
    if !source_cache.contains_key(&source_file) {
        let text = fs::read_to_string(&source_file).ok()?;
        source_cache.insert(source_file.clone(), text);
    }
    let source_text = source_cache.get(&source_file).map(String::as_str)?;
    let declaration = parse_fn_declaration(source_text, line);
    let has_cfg_attr = declaration_has_cfg_attr(source_text, line);
    let declaration_name = declaration
        .as_ref()
        .map(|decl| decl.name.clone())
        .or(symbol);
    let visibility = declaration
        .as_ref()
        .map(|decl| {
            if decl.is_public {
                "public".to_string()
            } else {
                "private".to_string()
            }
        })
        .unwrap_or_else(|| "unknown".to_string());
    let callsites_same_file = declaration_name
        .as_deref()
        .and_then(|name| count_callsites_same_file(source_text, name));
    Some((
        declaration_name,
        has_cfg_attr,
        visibility,
        callsites_same_file,
    ))
}

fn relativize_path(root: &Path, filename: &str) -> String {
    let path = Path::new(filename);
    let rel = path.strip_prefix(root).unwrap_or(path);
    rel.to_string_lossy().replace('\\', "/")
}

#[derive(Debug, Clone)]
struct FnDeclaration {
    name: String,
    is_public: bool,
}

fn parse_fn_declaration(source: &str, line: usize) -> Option<FnDeclaration> {
    if line == 0 {
        return None;
    }
    let line_text = source.lines().nth(line - 1)?;
    let trimmed = line_text.trim_start();
    if trimmed.starts_with("//") {
        return None;
    }
    let fn_index = trimmed.find("fn ")?;
    let prefix = &trimmed[..fn_index];
    let is_public = prefix.contains("pub");
    let after_fn = &trimmed[(fn_index + 3)..];
    let name = after_fn
        .chars()
        .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
        .collect::<String>();
    if name.is_empty() {
        return None;
    }
    Some(FnDeclaration { name, is_public })
}

fn declaration_has_cfg_attr(source: &str, line: usize) -> bool {
    if line == 0 {
        return false;
    }
    let start = line.saturating_sub(4);
    let end = line.saturating_sub(1);
    for (index, line_text) in source.lines().enumerate() {
        let line_no = index + 1;
        if line_no < start || line_no > end {
            continue;
        }
        if line_text.contains("#[cfg(") || line_text.contains("#[cfg_attr(") {
            return true;
        }
    }
    false
}

fn classify_candidate(
    visibility: &str,
    has_cfg_attr: bool,
    callsites_workspace: Option<usize>,
    near_zero_callsites: usize,
) -> &'static str {
    if visibility == "public" {
        return "public_api_review";
    }
    if has_cfg_attr {
        return "rare_path_keep";
    }
    // Keep explicit single-use private helpers: we intentionally use these to
    // keep complex flows readable and avoid deep nesting.
    if callsites_workspace == Some(2) {
        return "single_use_helper_keep";
    }
    if callsites_workspace.is_some_and(|count| count <= near_zero_callsites) {
        return "dead_likely";
    }
    "needs_targeted_test"
}

fn demangle_cov_function_name(name: &str) -> String {
    if let Some(index) = name.rfind("::h") {
        let suffix = &name[(index + 3)..];
        if suffix.len() >= 8 && suffix.chars().all(|ch| ch.is_ascii_hexdigit()) {
            return name[..index].to_string();
        }
    }
    name.to_string()
}

fn extract_symbol_name(name: &str) -> Option<String> {
    for segment in name.rsplit("::") {
        let trimmed = segment.trim();
        if trimmed.is_empty() || trimmed.contains("{{closure}}") {
            continue;
        }
        let symbol = trimmed
            .chars()
            .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
            .collect::<String>();
        if !symbol.is_empty() {
            return Some(symbol);
        }
    }
    None
}

fn unix_timestamp_seconds() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_secs())
        .unwrap_or(0)
}

fn ensure_cargo_llvm_cov_available(root: &Path) -> Result<()> {
    let mut cmd = Command::new("cargo");
    cmd.arg("llvm-cov").arg("--version").current_dir(root);
    let _ = run_capture(cmd)
        .context("cargo-llvm-cov is required. Install with: cargo install cargo-llvm-cov")?;
    Ok(())
}

fn ensure_llvm_tools_preview_available(root: &Path) -> Result<()> {
    let mut rustc_info = Command::new("rustc");
    rustc_info.arg("-vV").current_dir(root);
    let rustc_info =
        run_capture(rustc_info).context("failed to run `rustc -vV` for coverage preflight")?;
    let host = parse_rustc_host(&rustc_info)
        .context("failed to parse host triple from `rustc -vV` output")?;

    let mut rustc_sysroot = Command::new("rustc");
    rustc_sysroot
        .arg("--print")
        .arg("sysroot")
        .current_dir(root);
    let sysroot = run_capture(rustc_sysroot)
        .context("failed to run `rustc --print sysroot` for coverage preflight")?;
    let rustlib_bin = Path::new(sysroot.trim())
        .join("lib")
        .join("rustlib")
        .join(host)
        .join("bin");
    let llvm_cov = rustlib_bin.join(exe_name("llvm-cov"));
    let llvm_profdata = rustlib_bin.join(exe_name("llvm-profdata"));
    if llvm_cov.is_file() && llvm_profdata.is_file() {
        return Ok(());
    }

    let install_cmd = active_toolchain(root)
        .map(|toolchain| format!("rustup component add llvm-tools-preview --toolchain {toolchain}"))
        .unwrap_or_else(|| "rustup component add llvm-tools-preview".to_string());
    bail!(
        "missing rustup component `llvm-tools-preview` for the active toolchain.\n\
expected tools:\n  {}\n  {}\n\
install with:\n  {}\n\
then re-run:\n  rum coverage run",
        llvm_cov.display(),
        llvm_profdata.display(),
        install_cmd
    );
}

fn parse_rustc_host(rustc_info: &str) -> Option<&str> {
    rustc_info
        .lines()
        .find_map(|line| line.strip_prefix("host: "))
        .map(str::trim)
        .filter(|host| !host.is_empty())
}

fn active_toolchain(root: &Path) -> Option<String> {
    let mut cmd = Command::new("rustup");
    cmd.arg("show").arg("active-toolchain").current_dir(root);
    let output = run_capture(cmd).ok()?;
    output
        .split_whitespace()
        .next()
        .map(str::to_string)
        .filter(|toolchain| !toolchain.is_empty())
}

fn workspace_package_infos(root: &Path) -> Result<Vec<WorkspacePackageInfo>> {
    let mut cmd = Command::new("cargo");
    cmd.arg("metadata")
        .arg("--no-deps")
        .arg("--format-version=1")
        .current_dir(root);
    let metadata_json = run_capture(cmd).context("failed to run cargo metadata")?;
    let metadata: serde_json::Value =
        serde_json::from_str(&metadata_json).context("failed to parse cargo metadata JSON")?;

    let workspace_members = metadata
        .get("workspace_members")
        .and_then(serde_json::Value::as_array)
        .context("cargo metadata missing workspace_members")?
        .iter()
        .filter_map(serde_json::Value::as_str)
        .map(str::to_string)
        .collect::<BTreeSet<_>>();

    let mut package_infos = metadata
        .get("packages")
        .and_then(serde_json::Value::as_array)
        .context("cargo metadata missing packages")?
        .iter()
        .filter_map(|pkg| {
            let id = pkg.get("id").and_then(serde_json::Value::as_str)?;
            if !workspace_members.contains(id) {
                return None;
            }
            let name = pkg.get("name").and_then(serde_json::Value::as_str)?;
            let manifest_path = pkg
                .get("manifest_path")
                .and_then(serde_json::Value::as_str)?;
            let manifest_path = Path::new(manifest_path);
            let package_root = manifest_path.parent()?;
            let root_prefix = package_root
                .strip_prefix(root)
                .unwrap_or(package_root)
                .to_string_lossy()
                .replace('\\', "/");
            Some(WorkspacePackageInfo {
                name: name.to_string(),
                root_prefix: format!("{root_prefix}/"),
            })
        })
        .collect::<Vec<_>>();

    package_infos.sort_by(|a, b| {
        b.root_prefix
            .len()
            .cmp(&a.root_prefix.len())
            .then(a.name.cmp(&b.name))
    });
    package_infos.dedup_by(|a, b| a.name == b.name && a.root_prefix == b.root_prefix);
    Ok(package_infos)
}

fn run_cargo_with_args(root: &Path, args: &[String]) -> Result<()> {
    let mut command = Command::new("cargo");
    for arg in args {
        command.arg(arg);
    }
    command.current_dir(root);
    run_status(command)
}

fn cmd_build_wasm(args: WasmBuildArgs) -> Result<()> {
    let root = repo_root();
    ensure_wasm_deps(&root)?;
    let profile = if args.dev {
        WasmBuildProfile::Dev
    } else {
        WasmBuildProfile::Release
    };
    build_wasm(&root, profile)
}

pub(crate) fn run_wasm_test_suite(root: &Path) -> Result<()> {
    let mut wasm_tests = Command::new("cargo");
    wasm_tests
        .arg("test")
        .arg("-p")
        .arg("rumoca-bind-wasm")
        .arg("--all-features")
        .arg("--verbose")
        .current_dir(root);
    run_status(wasm_tests)?;

    run_wasm_editor_smoke_check(root)
}

pub(crate) fn run_wasm_editor_smoke_check(root: &Path) -> Result<()> {
    let js_checks = [
        "editors/wasm/src/main.js",
        "editors/wasm/src/modules/command_palette.js",
        "editors/wasm/src/modules/diagnostics_panel.js",
        "editors/wasm/src/modules/monaco_setup.js",
        "editors/wasm/rumoca_worker.js",
    ];
    for file in js_checks {
        let mut cmd = Command::new("node");
        cmd.arg("--check").arg(file).current_dir(root);
        run_status(cmd)?;
    }

    ensure_any_file_contains(
        root,
        &[
            "editors/wasm/src/modules/diagnostics_panel.js",
            "editors/wasm/index.html",
        ],
        "diagnostic-quick-fix",
    )?;
    ensure_any_file_contains(
        root,
        &[
            "editors/wasm/src/main.js",
            "editors/wasm/src/modules/diagnostics_panel.js",
        ],
        "triggerModelicaQuickFix",
    )?;
    ensure_any_file_contains(
        root,
        &[
            "editors/wasm/src/main.js",
            "editors/wasm/src/modules/diagnostics_panel.js",
        ],
        "triggerQuickFixAtCursor",
    )?;

    ensure_wasm_deps(root)?;
    build_wasm(root, WasmBuildProfile::Release)?;
    run_wasm_simulation_smoke(root)?;
    run_wasm_source_root_smoke(root)?;
    Ok(())
}

fn run_wasm_simulation_smoke(root: &Path) -> Result<()> {
    let mut wasm_smoke = Command::new("node");
    wasm_smoke
        .arg("editors/wasm/tests/simulate_smoke.mjs")
        .current_dir(root);
    run_status(wasm_smoke)
}

fn run_wasm_source_root_smoke(root: &Path) -> Result<()> {
    let mut wasm_smoke = Command::new("node");
    wasm_smoke
        .arg("editors/wasm/tests/source_root_smoke.mjs")
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

fn newest_file_with_ext(dir: &Path, ext: &str) -> Result<Option<PathBuf>> {
    if !dir.is_dir() {
        return Ok(None);
    }
    let mut newest: Option<(std::time::SystemTime, PathBuf)> = None;
    for entry in fs::read_dir(dir).with_context(|| format!("failed to read {}", dir.display()))? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(OsStr::to_str) != Some(ext) {
            continue;
        }
        let modified = entry
            .metadata()
            .and_then(|metadata| metadata.modified())
            .unwrap_or(std::time::SystemTime::UNIX_EPOCH);
        match &newest {
            None => newest = Some((modified, path)),
            Some((current, _)) if modified > *current => newest = Some((modified, path)),
            _ => {}
        }
    }
    Ok(newest.map(|(_, path)| path))
}

fn newest_prefixed_file(dir: &Path, prefix: &str, ext: &str) -> Result<Option<PathBuf>> {
    if !dir.is_dir() {
        return Ok(None);
    }
    let mut newest: Option<(std::time::SystemTime, PathBuf)> = None;
    for entry in fs::read_dir(dir).with_context(|| format!("failed to read {}", dir.display()))? {
        let entry = entry?;
        let path = entry.path();
        let file_name = path.file_name().and_then(OsStr::to_str).unwrap_or_default();
        let ext_match = path.extension().and_then(OsStr::to_str) == Some(ext);
        if !ext_match || !file_name.starts_with(prefix) {
            continue;
        }
        let modified = entry
            .metadata()
            .and_then(|metadata| metadata.modified())
            .unwrap_or(std::time::SystemTime::UNIX_EPOCH);
        match &newest {
            None => newest = Some((modified, path)),
            Some((current, _)) if modified > *current => newest = Some((modified, path)),
            _ => {}
        }
    }
    Ok(newest.map(|(_, path)| path))
}

fn ensure_wasm_deps(root: &Path) -> Result<()> {
    let wasm_bindgen_version = wasm_tooling::ensure_wasm_bindgen_cli(root)?;
    let wasm_pack_version = wasm_tooling::ensure_wasm_pack(root)?;
    println!(
        "WASM tooling ready: wasm-bindgen-cli {wasm_bindgen_version}, wasm-pack {wasm_pack_version}"
    );

    let mut list = Command::new("rustup");
    list.arg("target")
        .arg("list")
        .arg("--installed")
        .current_dir(root);
    let installed = run_capture(list)?;
    if !installed.contains("wasm32-unknown-unknown") {
        println!("Adding wasm32-unknown-unknown target...");
        let mut add = Command::new("rustup");
        add.arg("target")
            .arg("add")
            .arg("wasm32-unknown-unknown")
            .current_dir(root);
        run_status(add)?;
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WasmBuildProfile {
    Dev,
    Release,
}

fn wasm_opt_enabled() -> bool {
    matches!(
        std::env::var("RUMOCA_WASM_OPT").as_deref(),
        Ok("1" | "true" | "TRUE" | "yes" | "YES" | "on" | "ON")
    )
}

fn build_wasm(root: &Path, profile: WasmBuildProfile) -> Result<()> {
    let wasm_threads = std::env::var("RUMOCA_WASM_THREADS").unwrap_or_else(|_| "1".to_string());
    let mut wasm_features = vec!["full-web"];
    let wasm_license = root.join("crates/rumoca-bind-wasm/LICENSE");
    let staged_license = !wasm_license.exists();
    if staged_license {
        let root_license = root.join("LICENSE");
        ensure!(
            root_license.is_file(),
            "missing root LICENSE file at {}",
            root_license.display()
        );
        fs::copy(&root_license, &wasm_license).with_context(|| {
            format!(
                "failed to stage {} into {} for wasm-pack",
                root_license.display(),
                wasm_license.display()
            )
        })?;
    }
    println!("Building WASM module (rumoca-bind-wasm)...");
    let mut build = Command::new("wasm-pack");
    build
        .arg("build")
        .arg("crates/rumoca-bind-wasm")
        .arg("--target")
        .arg("web")
        .arg("--out-dir")
        .arg("../../pkg")
        .current_dir(root);
    build.arg(match profile {
        WasmBuildProfile::Dev => "--dev",
        WasmBuildProfile::Release => "--release",
    });
    if wasm_threads != "0" {
        wasm_features.push("wasm-rayon");
        const THREAD_FLAGS: &str = "-C target-feature=+atomics,+bulk-memory,+mutable-globals";
        let mut existing_rustflags = std::env::var("RUSTFLAGS").unwrap_or_default();
        if !existing_rustflags.contains("target-feature=+atomics") {
            if !existing_rustflags.trim().is_empty() {
                existing_rustflags.push(' ');
            }
            existing_rustflags.push_str(THREAD_FLAGS);
        }
        build.env("RUSTFLAGS", existing_rustflags);
    }
    if !wasm_opt_enabled() {
        build.arg("--no-opt");
    }
    build
        .arg("--")
        .arg("--features")
        .arg(wasm_features.join(","));
    let build_result = run_status(build);
    if staged_license {
        let _ = fs::remove_file(&wasm_license);
    }
    build_result?;

    let pkg_dir = root.join("pkg");
    let js_from = pkg_dir.join("rumoca_bind_wasm.js");
    let wasm_from = pkg_dir.join("rumoca_bind_wasm_bg.wasm");
    let js_to = pkg_dir.join("rumoca.js");
    let wasm_to = pkg_dir.join("rumoca_bg.wasm");
    ensure!(
        js_from.is_file() && wasm_from.is_file(),
        "unexpected wasm-pack output; expected {} and {}",
        js_from.display(),
        wasm_from.display()
    );
    fs::rename(&js_from, &js_to).with_context(|| {
        format!(
            "failed to rename {} to {}",
            js_from.display(),
            js_to.display()
        )
    })?;
    fs::rename(&wasm_from, &wasm_to).with_context(|| {
        format!(
            "failed to rename {} to {}",
            wasm_from.display(),
            wasm_to.display()
        )
    })?;

    let mut js_text = fs::read_to_string(&js_to)
        .with_context(|| format!("failed to read {}", js_to.display()))?;
    js_text = js_text.replace("rumoca_bind_wasm_bg.wasm", "rumoca_bg.wasm");
    fs::write(&js_to, js_text).with_context(|| format!("failed to write {}", js_to.display()))?;

    fs::copy(
        root.join("editors/wasm/rumoca_worker.js"),
        pkg_dir.join("rumoca_worker.js"),
    )
    .context("failed to copy rumoca_worker.js")?;
    fs::copy(
        root.join("editors/wasm/parse_worker.js"),
        pkg_dir.join("parse_worker.js"),
    )
    .context("failed to copy parse_worker.js")?;
    println!("WASM build complete: {}", pkg_dir.display());
    Ok(())
}

fn clean_wasm(root: &Path) -> Result<()> {
    let pkg = root.join("pkg");
    if pkg.exists() {
        fs::remove_dir_all(&pkg).with_context(|| format!("failed to remove {}", pkg.display()))?;
    }
    let target = root.join("target/wasm32-unknown-unknown");
    if target.exists() {
        fs::remove_dir_all(&target)
            .with_context(|| format!("failed to remove {}", target.display()))?;
    }
    println!("WASM artifacts cleaned.");
    Ok(())
}

fn serve_wasm(root: &Path, explicit_port: Option<u16>) -> Result<()> {
    static_server::serve(root, explicit_port)
}

#[cfg(unix)]
fn make_executable(path: &Path) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let metadata =
        fs::metadata(path).with_context(|| format!("failed to stat {}", path.display()))?;
    let mut perms = metadata.permissions();
    perms.set_mode(perms.mode() | 0o111);
    fs::set_permissions(path, perms)
        .with_context(|| format!("failed to chmod +x {}", path.display()))?;
    Ok(())
}

#[cfg(not(unix))]
fn make_executable(_path: &Path) -> Result<()> {
    Ok(())
}
