mod completion_cmd;
mod coverage_analysis;
mod coverage_gate;
mod crate_dag_cmd;
mod msl_cmd;
mod test_cmd;
mod vscode_cmd;
mod wasm_tooling;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use anyhow::{Context, Result, bail, ensure};
use clap::{Parser, Subcommand};
use completion_cmd::CompletionsArgs;
use coverage_analysis::{
    CallsiteIndex, build_workspace_callsite_index, count_callsites_same_file,
    is_opaque_symbol_name, owner_decision_for_label, render_coverage_trim_report,
};
use coverage_gate::CoverageGateArgs;
use crate_dag_cmd::CrateDagArgs;
use msl_cmd::MslArgs;
use rumoca_session::compile::core::workspace_root_from_manifest_dir;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ffi::OsStr;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Component, Path, PathBuf};
use std::process::{Command, Stdio};
use test_cmd::TestArgs;

#[derive(Debug, Parser)]
#[command(name = "rum")]
#[command(version)]
#[command(about = "Rumoca developer command")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Check max line count for staged Rust files (SPEC_0021)
    CheckRustFileLines(CheckRustFileLinesArgs),
    /// Configure repository git hooks
    InstallGitHooks,
    /// Build/install Python bindings via maturin
    BuildPython(BuildPythonArgs),
    /// Build/package/install VSCode extension
    BuildVscodeExt(BuildVscodeExtArgs),
    /// Fast VSCode extension development loop (watch + Extension Host)
    VscodeDev(VscodeDevArgs),
    /// Quick MSL development check
    DevCheck,
    /// Run workspace checks/tests
    Test(TestArgs),
    /// Run local CI-parity checks (fmt+clippy+rustdoc+workspace tests)
    CiParity,
    /// Print shell completion scripts
    Completions(CompletionsArgs),
    /// Run MSL tooling commands via the unified developer CLI
    Msl(MslArgs),
    /// Generate standardized unified workspace coverage artifacts in target/llvm-cov
    Coverage(CoverageArgs),
    /// Generate per-package inventory and trim candidates from unified workspace llvm-cov JSON
    CoverageReport(CoverageReportArgs),
    /// Enforce coverage-trim regression thresholds against committed baseline
    CoverageGate(CoverageGateArgs),
    /// Build/serve/clean WASM editor artifacts
    WasmTest(WasmTestArgs),
    /// WASM editor smoke checks
    WasmEditorSmokeCheck,
    /// Generate workspace crate dependency DAG plots (html/dot/svg/png)
    CrateDag(CrateDagArgs),
    /// Release version bump, optional commit/tag/push
    Release(ReleaseArgs),
}

#[derive(Debug, Parser, Clone)]
struct CheckRustFileLinesArgs {
    /// Maximum allowed lines in a Rust source file
    #[arg(long, default_value_t = 2000)]
    max_lines: usize,
    /// Check all tracked Rust files instead of only staged files
    #[arg(long)]
    all_files: bool,
}

#[derive(Debug, Parser, Clone)]
struct BuildPythonArgs {
    /// Build release wheel and install it
    #[arg(long)]
    release: bool,
}

#[derive(Debug, Parser, Clone)]
struct BuildVscodeExtArgs {
    /// Launch VSCode extension development mode
    #[arg(long, short = 'd')]
    dev: bool,
    /// Build/package only; do not install extension
    #[arg(long, short = 'b')]
    build: bool,
    /// Use system rumoca-lsp; skip cargo build/copy
    #[arg(long, short = 's')]
    system: bool,
}

#[derive(Debug, Parser, Clone)]
struct VscodeDevArgs {
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

#[derive(Debug, Parser, Clone)]
struct CoverageArgs {
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

#[derive(Debug, Parser, Clone)]
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

#[derive(Debug, Parser, Clone)]
struct WasmTestArgs {
    /// Action to run (default: all)
    #[arg(value_enum, default_value_t = WasmAction::All)]
    action: WasmAction,
    /// Override serve port (default: PORT env or 8080)
    #[arg(long)]
    port: Option<u16>,
}

#[derive(Debug, Clone, Copy, clap::ValueEnum, PartialEq, Eq)]
enum WasmAction {
    Build,
    Serve,
    Clean,
    All,
}

#[derive(Debug, Parser, Clone)]
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
    /// Push main and tag (implies --commit --tag)
    #[arg(long)]
    push: bool,
}

#[derive(Debug, Clone)]
struct ReleasePaths {
    cargo_toml: PathBuf,
    pyproject: PathBuf,
    py_version: PathBuf,
    package_json: PathBuf,
}

#[derive(Debug, Clone)]
struct ReleaseEdits {
    cargo_toml: String,
    pyproject: String,
    py_version: String,
    package_json: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::CheckRustFileLines(args) => cmd_check_rust_file_lines(args),
        Commands::InstallGitHooks => cmd_install_git_hooks(),
        Commands::BuildPython(args) => cmd_build_python(args),
        Commands::BuildVscodeExt(args) => vscode_cmd::build_vscode_ext(args),
        Commands::VscodeDev(args) => vscode_cmd::vscode_dev(args),
        Commands::DevCheck => cmd_dev_check(),
        Commands::Test(args) => test_cmd::run(args, &repo_root()),
        Commands::CiParity => test_cmd::run_ci_parity(&repo_root()),
        Commands::Completions(args) => {
            completion_cmd::run(args, "rum", &Commands::subcommand_names())
        }
        Commands::Msl(args) => msl_cmd::run(args, &repo_root()),
        Commands::Coverage(args) => cmd_coverage(args),
        Commands::CoverageReport(args) => cmd_coverage_report(args),
        Commands::CoverageGate(args) => cmd_coverage_gate(args),
        Commands::WasmTest(args) => cmd_wasm_test(args),
        Commands::WasmEditorSmokeCheck => cmd_wasm_editor_smoke_check(),
        Commands::CrateDag(args) => crate_dag_cmd::run(&repo_root(), args),
        Commands::Release(args) => cmd_release(args),
    }
}

impl Commands {
    fn subcommand_names() -> Vec<&'static str> {
        vec![
            "check-rust-file-lines",
            "install-git-hooks",
            "build-python",
            "build-vscode-ext",
            "vscode-dev",
            "dev-check",
            "test",
            "ci-parity",
            "completions",
            "msl",
            "coverage",
            "coverage-report",
            "coverage-gate",
            "wasm-test",
            "wasm-editor-smoke-check",
            "crate-dag",
            "release",
        ]
    }
}

fn repo_root() -> PathBuf {
    let root = workspace_root_from_manifest_dir(env!("CARGO_MANIFEST_DIR"));
    root.canonicalize().unwrap_or(root)
}

fn is_windows() -> bool {
    cfg!(windows)
}

fn exe_name(base: &str) -> String {
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

fn command_exists(program: &str) -> bool {
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
        if line_count > args.max_lines {
            violations.push((rel.to_string(), line_count));
        }
    }

    if violations.is_empty() {
        println!(
            "Rust file line-count check passed (max {} lines).",
            args.max_lines
        );
        return Ok(());
    }

    for (file, line_count) in &violations {
        eprintln!(
            "ERROR: {file} has {line_count} lines (max allowed: {}).",
            args.max_lines
        );
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

fn cmd_dev_check() -> Result<()> {
    let root = repo_root();
    let cache_dir = resolved_msl_cache_dir(&root);
    let results_file = cache_dir.join("results/msl_results.json");

    if !results_file.is_file() {
        println!(
            "No cached MSL results at {}. Running full baseline test first...",
            results_file.display()
        );
        let mut cmd = Command::new("cargo");
        cmd.arg("test")
            .arg("--release")
            .arg("--package")
            .arg("rumoca")
            .arg("--test")
            .arg("msl_tests")
            .arg("test_msl_all")
            .arg("--")
            .arg("--ignored")
            .arg("--nocapture")
            .env("RUMOCA_MSL_CACHE_DIR", &cache_dir)
            .current_dir(&root);
        run_status(cmd)?;
    }

    println!("Running quick-check set...");
    let mut cmd = Command::new("cargo");
    cmd.arg("test")
        .arg("--release")
        .arg("--package")
        .arg("rumoca")
        .arg("--test")
        .arg("msl_quick_check")
        .arg("test_quick_check")
        .arg("--")
        .arg("--ignored")
        .arg("--nocapture")
        .env("RUMOCA_MSL_CACHE_DIR", &cache_dir)
        .current_dir(&root);
    run_status(cmd)?;

    Ok(())
}

fn cmd_coverage(args: CoverageArgs) -> Result<()> {
    let root = repo_root();
    ensure_cargo_llvm_cov_available(&root)?;

    let output_dir = root.join("target/llvm-cov");
    fs::create_dir_all(&output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;

    let mut runbook = String::new();
    runbook.push_str("# rumoca coverage runbook\n");
    runbook.push_str("# generated by rum coverage\n\n");
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
        "coverage output directory not found: {}. Run `rum coverage` first.",
        output_dir.display()
    );
    let summary_path = output_dir.join("workspace-summary.json");
    let full_path = output_dir.join("workspace-full.json");
    ensure!(
        summary_path.is_file() && full_path.is_file(),
        "missing workspace coverage artifacts (expected {} and {}). Run `rum coverage` first.",
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
        "coverage-report candidate generation produced empty owner decisions"
    );
    ensure!(
        all_candidates
            .iter()
            .all(|candidate| candidate.owner_decision != "untriaged"),
        "coverage-report candidate generation produced untriaged owner decisions"
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
        "generated_by": "rum coverage-report",
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

fn cmd_wasm_test(args: WasmTestArgs) -> Result<()> {
    let root = repo_root();
    match args.action {
        WasmAction::Build => {
            ensure_wasm_deps(&root)?;
            build_wasm(&root)?;
            run_wasm_simulation_smoke(&root)?;
        }
        WasmAction::Serve => {
            serve_wasm(&root, args.port)?;
        }
        WasmAction::Clean => {
            clean_wasm(&root)?;
        }
        WasmAction::All => {
            ensure_wasm_deps(&root)?;
            build_wasm(&root)?;
            run_wasm_simulation_smoke(&root)?;
            serve_wasm(&root, args.port)?;
        }
    }
    Ok(())
}

fn cmd_wasm_editor_smoke_check() -> Result<()> {
    let root = repo_root();
    let js_checks = [
        "editors/wasm/src/main.js",
        "editors/wasm/src/modules/command_palette.js",
        "editors/wasm/src/modules/diagnostics_panel.js",
        "editors/wasm/src/modules/monaco_setup.js",
        "editors/wasm/rumoca_worker.js",
    ];
    for file in js_checks {
        let mut cmd = Command::new("node");
        cmd.arg("--check").arg(file).current_dir(&root);
        run_status(cmd)?;
    }

    let mut rg_quickfix = Command::new("rg");
    rg_quickfix
        .arg("-n")
        .arg("diagnostic-quick-fix")
        .arg("editors/wasm/src/modules/diagnostics_panel.js")
        .arg("editors/wasm/index.html")
        .current_dir(&root);
    run_status(rg_quickfix)?;

    let mut rg_trigger = Command::new("rg");
    rg_trigger
        .arg("-n")
        .arg("triggerModelicaQuickFix|triggerQuickFixAtCursor")
        .arg("editors/wasm/src/main.js")
        .arg("editors/wasm/src/modules/diagnostics_panel.js")
        .current_dir(&root);
    run_status(rg_trigger)?;

    let cargo_tests = [
        (
            "rumoca-tool-lsp",
            "handlers::diagnostics::tests::unresolved_component_diagnostic_points_to_equation_site",
        ),
        (
            "rumoca-tool-lsp",
            "handlers::semantic_tokens::tests::highlights_reinit_as_keyword_in_when_equation",
        ),
        (
            "rumoca-bind-wasm",
            "test_lsp_diagnostics_reports_unknown_builtin_modifier_startdt",
        ),
        (
            "rumoca-bind-wasm",
            "test_lsp_code_actions_returns_unknown_modifier_fix",
        ),
        (
            "rumoca-bind-wasm",
            "test_lsp_code_actions_returns_missing_semicolon_fix",
        ),
        (
            "rumoca-bind-wasm",
            "test_lsp_code_actions_returns_did_you_mean_type_fix",
        ),
    ];
    for (package, test_name) in cargo_tests {
        let mut cmd = Command::new("cargo");
        cmd.arg("test")
            .arg("-p")
            .arg(package)
            .arg(test_name)
            .arg("--")
            .arg("--nocapture")
            .current_dir(&root);
        run_status(cmd)?;
    }

    ensure_wasm_deps(&root)?;
    build_wasm(&root)?;
    run_wasm_simulation_smoke(&root)?;
    Ok(())
}

fn run_wasm_simulation_smoke(root: &Path) -> Result<()> {
    let mut wasm_smoke = Command::new("node");
    wasm_smoke
        .arg("editors/wasm/tests/simulate_smoke.mjs")
        .current_dir(root);
    run_status(wasm_smoke)
}

fn cmd_release(mut args: ReleaseArgs) -> Result<()> {
    validate_semver(&args.version)?;
    normalize_release_flags(&mut args);
    let root = repo_root();
    ensure_release_worktree_state(&root, &args)?;
    let paths = release_paths(&root);
    let edits = build_release_edits(&paths, &args.version)?;

    println!("Release version: {}", args.version);
    if args.dry_run {
        println!("Dry run: no files written.");
    } else {
        write_release_edits(&paths, &edits)?;
        run_quiet_cargo_check(&root);
        run_release_git_steps(&root, &args)?;
    }

    Ok(())
}

fn normalize_release_flags(args: &mut ReleaseArgs) {
    if args.push {
        args.commit = true;
        args.tag = true;
    }
}

fn ensure_release_worktree_state(root: &Path, args: &ReleaseArgs) -> Result<()> {
    if args.allow_dirty {
        return Ok(());
    }
    let mut status = Command::new("git");
    status.arg("status").arg("--porcelain").current_dir(root);
    let output = run_capture(status)?;
    ensure!(
        output.trim().is_empty(),
        "working tree is not clean; pass --allow-dirty to override"
    );
    Ok(())
}

fn release_paths(root: &Path) -> ReleasePaths {
    ReleasePaths {
        cargo_toml: root.join("Cargo.toml"),
        pyproject: root.join("bindings/python/pyproject.toml"),
        py_version: root.join("bindings/python/rumoca/version.py"),
        package_json: root.join("editors/vscode/package.json"),
    }
}

fn build_release_edits(paths: &ReleasePaths, version: &str) -> Result<ReleaseEdits> {
    let cargo_text = read_file_string(&paths.cargo_toml)?;
    let pyproject_text = read_file_string(&paths.pyproject)?;
    let py_version_text = read_file_string(&paths.py_version)?;
    let package_text = read_file_string(&paths.package_json)?;

    let cargo_toml = replace_first_line_by_prefix(
        &cargo_text,
        "version = \"",
        &format!("version = \"{version}\""),
    )
    .context("failed to update Cargo.toml version")?;
    let pyproject = replace_first_line_by_prefix(
        &pyproject_text,
        "version = \"",
        &format!("version = \"{version}\""),
    )
    .context("failed to update pyproject.toml version")?;
    let py_version = replace_first_line_by_prefix(
        &py_version_text,
        "__version__ = \"",
        &format!("__version__ = \"{version}\""),
    )
    .context("failed to update Python runtime version")?;

    let mut package_value: serde_json::Value =
        serde_json::from_str(&package_text).context("invalid VSCode package.json")?;
    let version_slot = package_value
        .get_mut("version")
        .context("missing version field in package.json")?;
    *version_slot = serde_json::Value::String(version.to_string());
    let package_json =
        serde_json::to_string_pretty(&package_value).context("failed to render package.json")?;

    Ok(ReleaseEdits {
        cargo_toml,
        pyproject,
        py_version,
        package_json,
    })
}

fn write_release_edits(paths: &ReleasePaths, edits: &ReleaseEdits) -> Result<()> {
    fs::write(&paths.cargo_toml, &edits.cargo_toml)
        .with_context(|| format!("failed to write {}", paths.cargo_toml.display()))?;
    fs::write(&paths.pyproject, &edits.pyproject)
        .with_context(|| format!("failed to write {}", paths.pyproject.display()))?;
    fs::write(&paths.py_version, &edits.py_version)
        .with_context(|| format!("failed to write {}", paths.py_version.display()))?;
    fs::write(&paths.package_json, format!("{}\n", edits.package_json))
        .with_context(|| format!("failed to write {}", paths.package_json.display()))?;
    Ok(())
}

fn run_quiet_cargo_check(root: &Path) {
    let mut cargo_check = Command::new("cargo");
    cargo_check.arg("check").arg("--quiet").current_dir(root);
    let _ = run_status(cargo_check);
}

fn run_release_git_steps(root: &Path, args: &ReleaseArgs) -> Result<()> {
    if args.commit {
        run_release_git_commit(root, &args.version)?;
    }
    if args.tag {
        run_release_git_tag(root, &args.version)?;
    }
    if args.push {
        run_release_git_push(root, &args.version)?;
    }
    Ok(())
}

fn run_release_git_commit(root: &Path, version: &str) -> Result<()> {
    let mut add = Command::new("git");
    add.arg("add")
        .arg("Cargo.toml")
        .arg("Cargo.lock")
        .arg("bindings/python/pyproject.toml")
        .arg("bindings/python/rumoca/version.py")
        .arg("editors/vscode/package.json")
        .current_dir(root);
    run_status(add)?;

    let mut commit = Command::new("git");
    commit
        .arg("commit")
        .arg("-m")
        .arg(format!("Release v{version}"))
        .current_dir(root);
    run_status(commit)
}

fn run_release_git_tag(root: &Path, version: &str) -> Result<()> {
    let mut tag = Command::new("git");
    tag.arg("tag").arg(format!("v{version}")).current_dir(root);
    run_status(tag)
}

fn run_release_git_push(root: &Path, version: &str) -> Result<()> {
    let mut push_main = Command::new("git");
    push_main
        .arg("push")
        .arg("origin")
        .arg("main")
        .current_dir(root);
    run_status(push_main)?;

    let mut push_tag = Command::new("git");
    push_tag
        .arg("push")
        .arg("origin")
        .arg(format!("v{version}"))
        .current_dir(root);
    run_status(push_tag)
}

fn read_file_string(path: &Path) -> Result<String> {
    fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))
}

fn replace_first_line_by_prefix(text: &str, prefix: &str, replacement: &str) -> Option<String> {
    let mut replaced = false;
    let mut out = String::with_capacity(text.len().saturating_add(32));
    for line in text.lines() {
        if !replaced && line.trim_start().starts_with(prefix) {
            out.push_str(replacement);
            out.push('\n');
            replaced = true;
        } else {
            out.push_str(line);
            out.push('\n');
        }
    }
    if replaced { Some(out) } else { None }
}

fn validate_semver(version: &str) -> Result<()> {
    let parts = version.split('.').collect::<Vec<_>>();
    ensure!(
        parts.len() == 3 && parts.iter().all(|part| part.parse::<u64>().is_ok()),
        "invalid version format: {version} (expected X.Y.Z)"
    );
    Ok(())
}

fn resolved_msl_cache_dir(root: &Path) -> PathBuf {
    match std::env::var("RUMOCA_MSL_CACHE_DIR") {
        Ok(value) => {
            let path = PathBuf::from(value);
            if path.is_absolute() {
                path
            } else {
                root.join(path)
            }
        }
        Err(_) => root.join("target/msl"),
    }
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

fn build_wasm(root: &Path) -> Result<()> {
    let wasm_opt = std::env::var("WASM_OPT").unwrap_or_else(|_| "0".to_string());
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
    if wasm_opt != "1" {
        build.arg("--no-opt");
    }
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
    let port = explicit_port
        .or_else(|| std::env::var("PORT").ok().and_then(|raw| raw.parse().ok()))
        .unwrap_or(8080);
    let listener = TcpListener::bind(("0.0.0.0", port))
        .with_context(|| format!("failed to bind 0.0.0.0:{port}"))?;

    println!("Serving on http://localhost:{port}");
    println!("Editor URL: http://localhost:{port}/editors/wasm/index.html");
    println!("Press Ctrl+C to stop.");

    for stream in listener.incoming() {
        match stream {
            Ok(mut stream) => {
                if let Err(error) = handle_http_request(root, &mut stream) {
                    eprintln!("serve error: {error:#}");
                }
            }
            Err(error) => eprintln!("accept error: {error}"),
        }
    }
    Ok(())
}

fn handle_http_request(root: &Path, stream: &mut TcpStream) -> Result<()> {
    let mut reader = BufReader::new(
        stream
            .try_clone()
            .context("failed to clone TCP stream for request parsing")?,
    );
    let mut request_line = String::new();
    if reader
        .read_line(&mut request_line)
        .context("failed to read request line")?
        == 0
    {
        return Ok(());
    }
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).is_err() || line == "\r\n" || line.is_empty() {
            break;
        }
    }

    let mut parts = request_line.split_whitespace();
    let method = parts.next().unwrap_or_default();
    let target = parts.next().unwrap_or("/");
    if method != "GET" {
        return write_http_response(
            stream,
            405,
            "Method Not Allowed",
            "text/plain",
            b"method not allowed",
        );
    }

    let url = target.split('?').next().unwrap_or("/");
    if url.starts_with("/proxy/") {
        return write_http_response(
            stream,
            501,
            "Not Implemented",
            "text/plain",
            b"proxy endpoint is not supported in rum",
        );
    }

    match resolve_static_path(root, url) {
        Some(path) if path.is_file() => {
            let body =
                fs::read(&path).with_context(|| format!("failed to read {}", path.display()))?;
            write_http_response(stream, 200, "OK", mime_for_path(&path), &body)
        }
        _ => write_http_response(stream, 404, "Not Found", "text/plain", b"not found"),
    }
}

fn write_http_response(
    stream: &mut TcpStream,
    code: u16,
    status: &str,
    content_type: &str,
    body: &[u8],
) -> Result<()> {
    let headers = format!(
        "HTTP/1.1 {code} {status}\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\nCross-Origin-Opener-Policy: same-origin\r\nCross-Origin-Embedder-Policy: require-corp\r\nAccess-Control-Allow-Origin: *\r\nCache-Control: no-store, no-cache, must-revalidate, max-age=0\r\nPragma: no-cache\r\nExpires: 0\r\n\r\n",
        body.len()
    );
    stream
        .write_all(headers.as_bytes())
        .context("failed writing HTTP headers")?;
    stream.write_all(body).context("failed writing HTTP body")?;
    stream.flush().context("failed flushing HTTP response")?;
    Ok(())
}

fn resolve_static_path(root: &Path, url: &str) -> Option<PathBuf> {
    if url == "/" {
        return Some(root.join("editors/wasm/index.html"));
    }
    let relative = url.trim_start_matches('/');
    let mut safe = PathBuf::new();
    for component in Path::new(relative).components() {
        if let Component::Normal(segment) = component {
            safe.push(segment);
        }
    }
    if safe.as_os_str().is_empty() {
        return Some(root.join("editors/wasm/index.html"));
    }
    let mut path = root.join(safe);
    if path.is_dir() {
        path = path.join("index.html");
    }
    Some(path)
}

fn mime_for_path(path: &Path) -> &'static str {
    match path.extension().and_then(OsStr::to_str).unwrap_or_default() {
        "html" => "text/html",
        "js" | "mjs" => "application/javascript",
        "json" => "application/json",
        "wasm" => "application/wasm",
        "css" => "text/css",
        "svg" => "image/svg+xml",
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        _ => "application/octet-stream",
    }
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

#[cfg(test)]
mod tests {
    use super::classify_candidate;

    #[test]
    fn classify_keeps_single_use_private_helper() {
        let label = classify_candidate("private", false, Some(2), 2);
        assert_eq!(label, "single_use_helper_keep");
    }

    #[test]
    fn classify_marks_zero_callsite_private_as_dead_likely() {
        let label = classify_candidate("private", false, Some(1), 1);
        assert_eq!(label, "dead_likely");
    }
}
