use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::fs::OpenOptions;
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context as _, Result, bail, ensure};
use clap::Args;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use xtask::golden_registry::{ensure_safe_id, sha256_file};

use crate::golden_coverage::{
    analyze_source, coverage_environment, ensure_coverage_tool, parse_lcov,
    production_relative_path, run_cargo_owned_output,
};

const CAPTURE_SCHEMA_VERSION: u32 = 4;
const OUTPUT_ROOT: &str = "target/golden-transition-coverage";
const UNIT_DERIVATIVE_SOURCE: &str = "crates/rumoca/tests/fixtures/golden/UnitDerivative.mo";
const RUSTC_FLAG_SEPARATOR: char = '\u{1f}';
const GNU_BUILD_ID_LINK_ARG: &str = "link-arg=-Wl,--build-id=sha1";
const REQUIRED_ELF_PROFILE_SECTIONS: [&str; 4] = [
    "__llvm_prf_cnts",
    "__llvm_prf_data",
    "__llvm_covfun",
    "__llvm_covmap",
];

#[derive(Debug, Args, Clone)]
pub(crate) struct GoldenTransitionCoverageArgs {
    /// Exact golden-model identity.
    model: String,
    /// Exact compiler transition identity.
    transition: String,
}

#[derive(Clone, Copy)]
struct TransitionDefinition {
    model: &'static str,
    transition: &'static str,
    predecessor: &'static str,
    successor: &'static str,
    package: &'static str,
    example: &'static str,
    source: &'static str,
    transition_function: &'static str,
    owner_source_prefix: &'static str,
}

impl TransitionDefinition {
    fn checked(args: &GoldenTransitionCoverageArgs) -> Result<Self> {
        ensure_safe_id(&args.model).context("invalid golden transition model identity")?;
        ensure_safe_id(&args.transition).context("invalid golden transition identity")?;
        match (args.model.as_str(), args.transition.as_str()) {
            ("UnitDerivative", "typed-instanced-to-flat") => Ok(Self {
                model: "UnitDerivative",
                transition: "typed-instanced-to-flat",
                predecessor: "TypedInstancedTree",
                successor: "flat::Model",
                package: "rumoca-phase-flatten",
                example: "golden_flatten_transition_capture",
                source: UNIT_DERIVATIVE_SOURCE,
                transition_function: "rumoca_phase_flatten::flatten_typed",
                owner_source_prefix: "crates/rumoca-phase-flatten/",
            }),
            _ => bail!(
                "no exact candidate capture runner is registered for ({}, {}); \
                 unsupported transitions fail rather than using a generic test filter",
                args.model,
                args.transition
            ),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
enum CaptureClaim {
    CandidateOnly,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct TransitionCaptureReport {
    schema_version: u32,
    claim: CaptureClaim,
    model: String,
    transition: String,
    predecessor: String,
    successor: String,
    model_source: SourceIdentity,
    runner: RunnerIdentity,
    replay: ReplayIdentity,
    calibration: CalibrationEvidence,
    summary: TransitionCoverageSummary,
    files: Vec<ProductionFileCoverage>,
    executed_regions: Vec<ExecutedProductionRegion>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct SourceIdentity {
    path: String,
    sha256: String,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct RunnerIdentity {
    package: String,
    example: String,
    executable: String,
    executable_sha256: String,
    rustc_identity: String,
    cargo_llvm_cov_identity: String,
    instrumentation_flags: String,
    profiler_runtime_image: String,
    runtime_images: Vec<RuntimeImageIdentity>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct RuntimeImageIdentity {
    path: String,
    sha256: String,
    defines_profile_dump: bool,
    defines_profile_reset: bool,
    defines_profile_runtime_symbol: bool,
    profiling_sections: Vec<String>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ReplayIdentity {
    normalized_llvm_export_sha256: String,
    first_raw_profile_sha256: String,
    replay_raw_profile_sha256: String,
    first_profile_data_sha256: String,
    replay_profile_data_sha256: String,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct CalibrationEvidence {
    before_reset: u64,
    inside_window: u64,
    after_dump: u64,
    transition_function: String,
    transition_function_count: u64,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct TransitionCoverageSummary {
    production_files: usize,
    covered_production_lines: usize,
    attributable_production_lines: usize,
    ambiguous_macro_or_generic_lines: usize,
    transition_owner_files: usize,
    transition_owner_attributable_lines: usize,
    supporting_files: usize,
    supporting_attributable_lines: usize,
    executed_production_regions: usize,
    attributable_production_regions: usize,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ProductionFileCoverage {
    path: String,
    source_sha256: String,
    covered_lines: Vec<u32>,
    attributable_lines: Vec<u32>,
    ambiguous_macro_or_generic_lines: Vec<u32>,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
enum RegionAttribution {
    Attributable,
    AmbiguousMacroOrGeneric,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ExecutedProductionRegion {
    source_path: String,
    llvm_function: String,
    demangled_function: String,
    function_count: u64,
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
    region_count: u64,
    region_kind: u64,
    attribution: RegionAttribution,
}

struct LlvmTools {
    profdata: PathBuf,
    cov: PathBuf,
    nm: PathBuf,
    readobj: PathBuf,
    rustc_identity: String,
}

struct CapturedProfile {
    raw_path: PathBuf,
    profile_data_path: PathBuf,
    llvm_export: Vec<u8>,
    lcov: Vec<u8>,
    runtime_images: Vec<PathBuf>,
}

struct BuiltRunner {
    environment: BTreeMap<String, String>,
    executable: PathBuf,
    sha256: String,
}

struct CaptureLock {
    path: Option<PathBuf>,
}

impl CaptureLock {
    fn acquire(output_dir: &Path) -> Result<Self> {
        let path = output_dir.join("capture.lock");
        let mut file = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
            .with_context(|| {
                format!(
                    "another capture owns {} or a prior capture did not exit cleanly",
                    path.display()
                )
            })?;
        let lock = Self { path: Some(path) };
        writeln!(file, "pid={}", std::process::id())?;
        file.sync_all()?;
        Ok(lock)
    }

    fn release(mut self) -> Result<()> {
        let path = self.path.take().context("capture lock already released")?;
        fs::remove_file(&path)
            .with_context(|| format!("failed to release capture lock {}", path.display()))
    }
}

impl Drop for CaptureLock {
    fn drop(&mut self) {
        if let Some(path) = self.path.take() {
            drop(fs::remove_file(path));
        }
    }
}

pub(crate) fn run(root: &Path, args: &GoldenTransitionCoverageArgs) -> Result<()> {
    let definition = TransitionDefinition::checked(args)?;
    ensure_coverage_tool(root)?;
    let tools = llvm_tools(root)?;
    let output_dir = root
        .join(OUTPUT_ROOT)
        .join(definition.model)
        .join(definition.transition);
    let build_dir = root.join(OUTPUT_ROOT).join("llvm-target");
    fs::create_dir_all(&output_dir)?;
    fs::create_dir_all(&build_dir)?;
    let capture_lock = CaptureLock::acquire(&output_dir)?;
    invalidate_capture(&output_dir)?;
    let runner = build_exact_runner(root, &build_dir, definition)?;
    let (first, replay) = capture_replay(root, &output_dir, &runner, &tools)?;
    let report = build_transition_report(root, definition, &tools, &runner, &first, &replay)?;
    ensure_file_sha256(
        &runner.executable,
        &report.runner.executable_sha256,
        "immediately before report publication",
    )?;
    write_report(&output_dir, &report, &first.llvm_export, &first.lcov)?;
    ensure_file_sha256(
        &runner.executable,
        &report.runner.executable_sha256,
        "immediately after report publication",
    )?;
    capture_lock.release()?;
    println!(
        "candidate transition coverage: {} / {}: {} attributable production lines",
        definition.model, definition.transition, report.summary.attributable_production_lines
    );
    println!("review packet: {}", output_dir.join("review.md").display());
    println!(
        "machine report: {}",
        output_dir.join("capture.json").display()
    );
    Ok(())
}

fn build_exact_runner(
    root: &Path,
    build_dir: &Path,
    definition: TransitionDefinition,
) -> Result<BuiltRunner> {
    let mut environment = coverage_environment(root, build_dir)?;
    require_runner_build_id(&mut environment)?;
    environment.insert("RAYON_NUM_THREADS".to_string(), "1".to_string());
    environment.insert("RUST_TEST_THREADS".to_string(), "1".to_string());
    let executable = build_runner(root, &environment, definition)?;
    ensure!(
        executable.is_file(),
        "Cargo-issued runner executable is absent"
    );
    let sha256 = sha256_file(&executable)?;
    Ok(BuiltRunner {
        environment,
        executable,
        sha256,
    })
}

fn capture_replay(
    root: &Path,
    output_dir: &Path,
    runner: &BuiltRunner,
    tools: &LlvmTools,
) -> Result<(CapturedProfile, CapturedProfile)> {
    let first = capture_once(
        root,
        output_dir,
        "first",
        &runner.executable,
        &runner.sha256,
        tools,
    )?;
    let replay = capture_once(
        root,
        output_dir,
        "replay",
        &runner.executable,
        &runner.sha256,
        tools,
    )?;
    ensure_file_sha256(&runner.executable, &runner.sha256, "during capture")?;
    ensure!(
        first.llvm_export == replay.llvm_export,
        "normalized LLVM transition profiles differ between exact replays"
    );
    ensure!(
        first.lcov == replay.lcov,
        "LCOV transition profiles differ between exact replays"
    );
    ensure!(
        first.runtime_images == replay.runtime_images,
        "file-backed executable images differ between exact replays"
    );
    Ok((first, replay))
}

fn build_transition_report(
    root: &Path,
    definition: TransitionDefinition,
    tools: &LlvmTools,
    runner: &BuiltRunner,
    first: &CapturedProfile,
    replay: &CapturedProfile,
) -> Result<TransitionCaptureReport> {
    let llvm: Value = serde_json::from_slice(&first.llvm_export)
        .context("LLVM transition export is not valid JSON")?;
    let calibration = validate_calibration(&llvm, definition.transition_function)?;
    let files = production_file_coverage(root, &first.lcov)?;
    validate_transition_file_boundary(&files)?;
    let regions = executed_production_regions(root, &llvm)?;
    let summary = summarize(&files, &regions, definition.owner_source_prefix);
    ensure!(
        summary.attributable_production_lines > 0,
        "exact transition capture contains no attributable production line"
    );
    let runtime_images = runtime_image_identities(tools, &first.runtime_images)?;
    let profiler_runtime_image =
        sole_profiler_runtime_image(&runtime_images, &runner.executable, &runner.sha256)?;
    Ok(TransitionCaptureReport {
        schema_version: CAPTURE_SCHEMA_VERSION,
        claim: CaptureClaim::CandidateOnly,
        model: definition.model.to_string(),
        transition: definition.transition.to_string(),
        predecessor: definition.predecessor.to_string(),
        successor: definition.successor.to_string(),
        model_source: SourceIdentity {
            path: definition.source.to_string(),
            sha256: sha256_file(&root.join(definition.source))?,
        },
        runner: RunnerIdentity {
            package: definition.package.to_string(),
            example: definition.example.to_string(),
            executable: runner.executable.display().to_string(),
            executable_sha256: runner.sha256.clone(),
            rustc_identity: tools.rustc_identity.clone(),
            cargo_llvm_cov_identity: command_text(root, "cargo", &["llvm-cov", "--version"])?,
            instrumentation_flags: runner
                .environment
                .get("__CARGO_LLVM_COV_RUSTC_WRAPPER_RUSTFLAGS")
                .cloned()
                .context("cargo-llvm-cov did not publish instrumentation flags")?,
            profiler_runtime_image,
            runtime_images,
        },
        replay: ReplayIdentity {
            normalized_llvm_export_sha256: sha256_bytes(&first.llvm_export),
            first_raw_profile_sha256: sha256_file(&first.raw_path)?,
            replay_raw_profile_sha256: sha256_file(&replay.raw_path)?,
            first_profile_data_sha256: sha256_file(&first.profile_data_path)?,
            replay_profile_data_sha256: sha256_file(&replay.profile_data_path)?,
        },
        calibration,
        summary,
        files,
        executed_regions: regions,
    })
}

fn build_runner(
    root: &Path,
    environment: &BTreeMap<String, String>,
    definition: TransitionDefinition,
) -> Result<PathBuf> {
    let arguments = vec![
        "build".to_string(),
        "--package".to_string(),
        definition.package.to_string(),
        "--example".to_string(),
        definition.example.to_string(),
        "--jobs".to_string(),
        "4".to_string(),
        "--message-format=json-render-diagnostics".to_string(),
    ];
    let output = run_cargo_owned_output(root, environment, &arguments)?;
    exact_example_executable(&output, definition.example)
}

fn exact_example_executable(cargo_output: &str, example: &str) -> Result<PathBuf> {
    let mut executables = BTreeSet::new();
    for line in cargo_output.lines().filter(|line| !line.is_empty()) {
        let message: Value = serde_json::from_str(line)
            .with_context(|| format!("invalid Cargo JSON message: {line}"))?;
        if message.get("reason").and_then(Value::as_str) != Some("compiler-artifact")
            || message.pointer("/target/name").and_then(Value::as_str) != Some(example)
            || !message
                .pointer("/target/kind")
                .and_then(Value::as_array)
                .is_some_and(|kinds| kinds.iter().any(|kind| kind.as_str() == Some("example")))
        {
            continue;
        }
        if let Some(executable) = message.get("executable").and_then(Value::as_str) {
            executables.insert(PathBuf::from(executable));
        }
    }
    ensure!(
        executables.len() == 1,
        "Cargo reported {} executable artifacts for exact example `{example}`",
        executables.len()
    );
    executables
        .pop_first()
        .context("exact example executable disappeared")
}

fn capture_once(
    root: &Path,
    output_dir: &Path,
    label: &str,
    executable: &Path,
    executable_sha256: &str,
    tools: &LlvmTools,
) -> Result<CapturedProfile> {
    let raw_path = output_dir.join(format!("{label}.profraw"));
    let profile_data_path = output_dir.join(format!("{label}.profdata"));
    ensure!(
        !raw_path.exists() && !profile_data_path.exists(),
        "capture paths must not exist immediately before exact runner spawn"
    );
    ensure_file_sha256(executable, executable_sha256, "before runner spawn")?;
    let output = Command::new(executable)
        .current_dir(root)
        .env("LLVM_PROFILE_FILE", &raw_path)
        .env("RAYON_NUM_THREADS", "1")
        .env("RUST_TEST_THREADS", "1")
        .output()
        .with_context(|| format!("failed to spawn exact runner {}", executable.display()))?;
    ensure!(
        output.status.success(),
        "exact transition runner failed with {}:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    ensure_file_sha256(executable, executable_sha256, "after runner exit")?;
    let runtime_images = parse_runtime_images(&output.stdout)?;
    ensure!(
        raw_path.is_file() && fs::metadata(&raw_path)?.len() > 0,
        "successful exact runner produced no raw LLVM profile"
    );
    run_checked(
        Command::new(&tools.profdata)
            .arg("merge")
            .arg("-sparse")
            .arg(&raw_path)
            .arg("-o")
            .arg(&profile_data_path),
        "llvm-profdata merge",
    )?;
    ensure!(
        profile_data_path.is_file() && fs::metadata(&profile_data_path)?.len() > 0,
        "LLVM profile merge produced no profile data"
    );
    ensure_profile_matches_executable(tools, &profile_data_path, executable)?;
    ensure_file_sha256(executable, executable_sha256, "before LLVM JSON export")?;
    let llvm_export = llvm_cov_export(&tools.cov, &profile_data_path, executable, None)?;
    ensure_file_sha256(executable, executable_sha256, "after LLVM JSON export")?;
    let lcov = export_lcov(tools, &profile_data_path, executable)?;
    ensure_file_sha256(executable, executable_sha256, "after LLVM LCOV export")?;
    Ok(CapturedProfile {
        raw_path,
        profile_data_path,
        llvm_export,
        lcov,
        runtime_images,
    })
}

fn require_runner_build_id(environment: &mut BTreeMap<String, String>) -> Result<()> {
    let flags = environment
        .get_mut("__CARGO_LLVM_COV_RUSTC_WRAPPER_RUSTFLAGS")
        .context("cargo-llvm-cov did not publish instrumentation flags")?;
    ensure!(
        !flags
            .split(RUSTC_FLAG_SEPARATOR)
            .any(|flag| flag == GNU_BUILD_ID_LINK_ARG),
        "cargo-llvm-cov flags already contain the exact runner build-ID link argument"
    );
    flags.push(RUSTC_FLAG_SEPARATOR);
    flags.push_str("-C");
    flags.push(RUSTC_FLAG_SEPARATOR);
    flags.push_str(GNU_BUILD_ID_LINK_ARG);
    Ok(())
}

fn ensure_file_sha256(path: &Path, expected: &str, stage: &str) -> Result<()> {
    let observed = sha256_file(path)?;
    ensure!(
        observed == expected,
        "exact runner executable changed {stage}: expected {expected}, found {observed}"
    );
    Ok(())
}

fn parse_runtime_images(stdout: &[u8]) -> Result<Vec<PathBuf>> {
    let paths: Vec<String> = serde_json::from_slice(stdout)
        .context("exact transition runner did not emit one JSON image witness")?;
    ensure!(!paths.is_empty(), "runner image witness is empty");
    let paths = paths.into_iter().map(PathBuf::from).collect::<Vec<_>>();
    ensure!(
        paths
            .iter()
            .all(|path| path.is_absolute() && path.is_file()),
        "runner image witness contains a missing or non-absolute path"
    );
    ensure!(
        paths.windows(2).all(|pair| pair[0] < pair[1]),
        "runner image witness must be strictly sorted and duplicate-free"
    );
    Ok(paths)
}

fn runtime_image_identities(
    tools: &LlvmTools,
    paths: &[PathBuf],
) -> Result<Vec<RuntimeImageIdentity>> {
    paths
        .iter()
        .map(|path| {
            let symbols = defined_symbols(&tools.nm, path)?;
            let sections = section_names(&tools.readobj, path)?;
            let profiling_sections = REQUIRED_ELF_PROFILE_SECTIONS
                .iter()
                .copied()
                .filter(|section| sections.contains(*section))
                .map(str::to_string)
                .collect();
            Ok(RuntimeImageIdentity {
                path: path.display().to_string(),
                sha256: sha256_file(path)?,
                defines_profile_dump: symbols.contains("__llvm_profile_dump"),
                defines_profile_reset: symbols.contains("__llvm_profile_reset_counters"),
                defines_profile_runtime_symbol: symbols
                    .iter()
                    .any(|symbol| symbol.starts_with("__llvm_profile_")),
                profiling_sections,
            })
        })
        .collect()
}

fn defined_symbols(llvm_nm: &Path, image: &Path) -> Result<BTreeSet<String>> {
    let output = Command::new(llvm_nm)
        .args(["--defined-only", "--format=posix"])
        .arg(image)
        .output()
        .with_context(|| format!("failed to inspect mapped image {}", image.display()))?;
    ensure!(
        output.status.success(),
        "llvm-nm rejected mapped image {} with {}:\n{}",
        image.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = std::str::from_utf8(&output.stdout).context("llvm-nm output is not UTF-8")?;
    Ok(stdout
        .lines()
        .filter_map(|line| line.split_ascii_whitespace().next())
        .map(str::to_string)
        .collect())
}

fn section_names(llvm_readobj: &Path, image: &Path) -> Result<BTreeSet<String>> {
    let output = Command::new(llvm_readobj)
        .arg("--sections")
        .arg(image)
        .output()
        .with_context(|| format!("failed to inspect sections in {}", image.display()))?;
    ensure!(
        output.status.success(),
        "llvm-readobj rejected mapped image {} with {}:\n{}",
        image.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = std::str::from_utf8(&output.stdout).context("llvm-readobj output is not UTF-8")?;
    Ok(stdout
        .lines()
        .filter_map(|line| line.trim().strip_prefix("Name: "))
        .filter_map(|name_and_index| name_and_index.split_ascii_whitespace().next())
        .map(str::to_string)
        .collect())
}

fn ensure_profile_matches_executable(
    tools: &LlvmTools,
    profile: &Path,
    executable: &Path,
) -> Result<()> {
    let profile_ids = profile_binary_ids(&tools.profdata, profile)?;
    let executable_ids = executable_build_ids(&tools.readobj, executable)?;
    ensure!(
        profile_ids.len() == 1,
        "exact transition profile carries {} binary IDs, expected exactly one",
        profile_ids.len()
    );
    ensure!(
        executable_ids.len() == 1,
        "exact transition runner carries {} GNU build IDs, expected exactly one",
        executable_ids.len()
    );
    ensure!(
        profile_ids == executable_ids,
        "transition profile binary ID does not match the exact runner GNU build ID"
    );
    Ok(())
}

fn profile_binary_ids(llvm_profdata: &Path, profile: &Path) -> Result<BTreeSet<String>> {
    let output = Command::new(llvm_profdata)
        .args(["show", "--binary-ids"])
        .arg(profile)
        .output()
        .with_context(|| format!("failed to inspect binary IDs in {}", profile.display()))?;
    ensure!(
        output.status.success(),
        "llvm-profdata rejected {} with {}:\n{}",
        profile.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    parse_profile_binary_ids(std::str::from_utf8(&output.stdout)?)
}

fn executable_build_ids(llvm_readobj: &Path, executable: &Path) -> Result<BTreeSet<String>> {
    let output = Command::new(llvm_readobj)
        .arg("--notes")
        .arg(executable)
        .output()
        .with_context(|| format!("failed to inspect build ID in {}", executable.display()))?;
    ensure!(
        output.status.success(),
        "llvm-readobj rejected {} with {}:\n{}",
        executable.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = std::str::from_utf8(&output.stdout).context("llvm-readobj output is not UTF-8")?;
    stdout
        .lines()
        .filter_map(|line| line.trim().strip_prefix("Build ID: "))
        .map(normalize_binary_id)
        .collect()
}

fn parse_profile_binary_ids(output: &str) -> Result<BTreeSet<String>> {
    let mut lines = output.lines();
    let marker_found = lines.any(|line| line.trim() == "Binary IDs:");
    ensure!(marker_found, "LLVM profile carries no binary-ID section");
    let binary_ids = lines
        .filter(|line| !line.trim().is_empty())
        .map(|line| normalize_binary_id(line.trim()))
        .collect::<Result<BTreeSet<_>>>()?;
    ensure!(!binary_ids.is_empty(), "LLVM profile carries no binary ID");
    Ok(binary_ids)
}

fn normalize_binary_id(value: &str) -> Result<String> {
    let value = value.strip_prefix("0x").unwrap_or(value);
    ensure!(
        !value.is_empty()
            && value.len().is_multiple_of(2)
            && value.bytes().all(|byte| byte.is_ascii_hexdigit()),
        "invalid binary ID `{value}`"
    );
    Ok(value.to_ascii_lowercase())
}

fn sole_profiler_runtime_image(
    images: &[RuntimeImageIdentity],
    executable: &Path,
    executable_sha256: &str,
) -> Result<String> {
    let profiler_images = images
        .iter()
        .filter(|image| {
            image.defines_profile_dump
                || image.defines_profile_reset
                || image.defines_profile_runtime_symbol
                || !image.profiling_sections.is_empty()
        })
        .collect::<Vec<_>>();
    ensure!(
        profiler_images.len() == 1
            && profiler_images[0].defines_profile_dump
            && profiler_images[0].defines_profile_reset
            && profiler_images[0].defines_profile_runtime_symbol
            && profiler_images[0]
                .profiling_sections
                .iter()
                .map(String::as_str)
                .eq(REQUIRED_ELF_PROFILE_SECTIONS),
        "expected exactly one mapped image owning LLVM controls, counters, and mapping, found {}",
        profiler_images.len()
    );
    let mapped = fs::canonicalize(&profiler_images[0].path)?;
    let expected = fs::canonicalize(executable)?;
    ensure!(
        mapped == expected,
        "LLVM profiler runtime belongs to {}, not the exact runner {}",
        mapped.display(),
        expected.display()
    );
    ensure!(
        profiler_images[0].sha256 == executable_sha256,
        "mapped runner SHA-256 does not match the exact captured executable"
    );
    Ok(profiler_images[0].path.clone())
}

fn llvm_cov_export(
    llvm_cov: &Path,
    profile: &Path,
    executable: &Path,
    format: Option<&str>,
) -> Result<Vec<u8>> {
    let mut command = Command::new(llvm_cov);
    command
        .arg("export")
        .arg("--check-binary-ids")
        .arg(format!("--instr-profile={}", profile.display()));
    if let Some(format) = format {
        command.arg(format!("--format={format}"));
    }
    command.arg(executable);
    let output = command.output().context("failed to run llvm-cov export")?;
    ensure!(
        output.status.success(),
        "llvm-cov export failed with {}:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    ensure!(!output.stdout.is_empty(), "llvm-cov export was empty");
    Ok(output.stdout)
}

fn export_lcov(tools: &LlvmTools, profile: &Path, executable: &Path) -> Result<Vec<u8>> {
    llvm_cov_export(&tools.cov, profile, executable, Some("lcov"))
}

fn validate_calibration(llvm: &Value, transition: &str) -> Result<CalibrationEvidence> {
    let before_reset = exact_function_count(
        llvm,
        "golden_flatten_transition_capture::calibration_before_reset",
    )?;
    let inside_window = exact_function_count(
        llvm,
        "golden_flatten_transition_capture::calibration_inside_window",
    )?;
    let after_dump = exact_function_count(
        llvm,
        "golden_flatten_transition_capture::calibration_after_dump",
    )?;
    let transition_function_count = exact_function_count(llvm, transition)?;
    ensure!(
        (before_reset, inside_window, after_dump) == (0, 1, 0),
        "LLVM reset/dump calibration must be exactly 0/1/0, found \
         {before_reset}/{inside_window}/{after_dump}"
    );
    ensure!(
        transition_function_count == 1,
        "production transition `{transition}` must execute exactly once, found {transition_function_count}"
    );
    Ok(CalibrationEvidence {
        before_reset,
        inside_window,
        after_dump,
        transition_function: transition.to_string(),
        transition_function_count,
    })
}

fn exact_function_count(llvm: &Value, expected: &str) -> Result<u64> {
    let functions = llvm_functions(llvm)?;
    let matches = functions
        .iter()
        .filter_map(|function| {
            let name = function.get("name")?.as_str()?;
            let demangled = format!("{:#}", rustc_demangle::demangle(name));
            (demangled == expected).then(|| {
                function
                    .get("count")
                    .and_then(Value::as_u64)
                    .with_context(|| format!("function `{demangled}` has no integer count"))
            })
        })
        .collect::<Result<Vec<_>>>()?;
    ensure!(
        matches.len() == 1,
        "LLVM mapping contains {} functions named `{expected}`, expected exactly one",
        matches.len()
    );
    Ok(matches[0])
}

fn llvm_functions(llvm: &Value) -> Result<&Vec<Value>> {
    ensure!(
        llvm.get("type").and_then(Value::as_str) == Some("llvm.coverage.json.export"),
        "unexpected LLVM coverage export type"
    );
    let data = llvm
        .get("data")
        .and_then(Value::as_array)
        .context("LLVM coverage export has no data array")?;
    ensure!(
        data.len() == 1,
        "LLVM coverage export must contain one object"
    );
    data[0]
        .get("functions")
        .and_then(Value::as_array)
        .context("LLVM coverage export has no function mapping")
}

fn production_file_coverage(root: &Path, lcov: &[u8]) -> Result<Vec<ProductionFileCoverage>> {
    let lcov = std::str::from_utf8(lcov).context("LLVM LCOV export is not UTF-8")?;
    let mut files = Vec::new();
    for (absolute, coverage) in parse_lcov(lcov)? {
        let Some(relative) = production_relative_path(root, &absolute) else {
            continue;
        };
        let analysis = analyze_source(&root.join(&relative))?;
        let covered_lines = coverage
            .lines
            .iter()
            .filter_map(|(line, count)| (*count > 0).then_some(*line))
            .filter(|line| !analysis.test_lines.contains(line))
            .collect::<Vec<_>>();
        if covered_lines.is_empty() {
            continue;
        }
        let ambiguous = covered_lines
            .iter()
            .copied()
            .filter(|line| {
                analysis.macro_lines.contains(line) || analysis.generic_lines.contains(line)
            })
            .collect::<Vec<_>>();
        let ambiguous_set = ambiguous.iter().copied().collect::<BTreeSet<_>>();
        let attributable_lines = covered_lines
            .iter()
            .copied()
            .filter(|line| !ambiguous_set.contains(line))
            .collect();
        files.push(ProductionFileCoverage {
            path: relative.clone(),
            source_sha256: sha256_file(&root.join(relative))?,
            covered_lines,
            attributable_lines,
            ambiguous_macro_or_generic_lines: ambiguous,
        });
    }
    files.sort_by(|left, right| left.path.cmp(&right.path));
    Ok(files)
}

fn executed_production_regions(root: &Path, llvm: &Value) -> Result<Vec<ExecutedProductionRegion>> {
    let mut regions = Vec::new();
    let mut analyses = BTreeMap::new();
    for function in llvm_functions(llvm)? {
        let llvm_function = function
            .get("name")
            .and_then(Value::as_str)
            .context("LLVM function has no name")?;
        let function_count = function
            .get("count")
            .and_then(Value::as_u64)
            .context("LLVM function has no count")?;
        let filenames = function
            .get("filenames")
            .and_then(Value::as_array)
            .context("LLVM function has no filenames")?;
        let function_regions = function
            .get("regions")
            .and_then(Value::as_array)
            .context("LLVM function has no regions")?;
        for region in function_regions {
            let fields = region.as_array().context("LLVM region is not an array")?;
            ensure!(fields.len() >= 8, "LLVM region has fewer than eight fields");
            let region_count = integer_field(fields, 4, "region count")?;
            if region_count == 0 {
                continue;
            }
            let file_index = usize::try_from(integer_field(fields, 5, "file index")?)?;
            let filename = filenames
                .get(file_index)
                .and_then(Value::as_str)
                .context("LLVM region file index is out of range")?;
            let Some(relative) = production_relative_path(root, Path::new(filename)) else {
                continue;
            };
            let analysis = match analyses.entry(relative.clone()) {
                std::collections::btree_map::Entry::Occupied(entry) => entry.into_mut(),
                std::collections::btree_map::Entry::Vacant(entry) => {
                    entry.insert(analyze_source(&root.join(&relative))?)
                }
            };
            let start_line = u32::try_from(integer_field(fields, 0, "start line")?)?;
            let attribution = if analysis.macro_lines.contains(&start_line)
                || analysis.generic_lines.contains(&start_line)
            {
                RegionAttribution::AmbiguousMacroOrGeneric
            } else {
                RegionAttribution::Attributable
            };
            regions.push(ExecutedProductionRegion {
                source_path: relative,
                llvm_function: llvm_function.to_string(),
                demangled_function: format!("{:#}", rustc_demangle::demangle(llvm_function)),
                function_count,
                start_line,
                start_column: u32::try_from(integer_field(fields, 1, "start column")?)?,
                end_line: u32::try_from(integer_field(fields, 2, "end line")?)?,
                end_column: u32::try_from(integer_field(fields, 3, "end column")?)?,
                region_count,
                region_kind: integer_field(fields, 7, "region kind")?,
                attribution,
            });
        }
    }
    regions.sort_by(|left, right| {
        (
            &left.source_path,
            &left.llvm_function,
            left.start_line,
            left.start_column,
            left.end_line,
            left.end_column,
        )
            .cmp(&(
                &right.source_path,
                &right.llvm_function,
                right.start_line,
                right.start_column,
                right.end_line,
                right.end_column,
            ))
    });
    Ok(regions)
}

fn integer_field(fields: &[Value], index: usize, label: &str) -> Result<u64> {
    fields
        .get(index)
        .and_then(Value::as_u64)
        .with_context(|| format!("LLVM region has no integer {label}"))
}

fn validate_transition_file_boundary(files: &[ProductionFileCoverage]) -> Result<()> {
    for forbidden in [
        "crates/rumoca-phase-parse/",
        "crates/rumoca-phase-instantiate/",
    ] {
        ensure!(
            !files.iter().any(|file| file.path.starts_with(forbidden)),
            "predecessor setup leaked into the measured transition through `{forbidden}`"
        );
    }
    ensure!(
        files
            .iter()
            .any(|file| file.path.starts_with("crates/rumoca-phase-flatten/")),
        "measured transition contains no phase-flatten production file"
    );
    Ok(())
}

fn summarize(
    files: &[ProductionFileCoverage],
    regions: &[ExecutedProductionRegion],
    owner_source_prefix: &str,
) -> TransitionCoverageSummary {
    let (owner_files, supporting_files): (Vec<_>, Vec<_>) = files
        .iter()
        .partition(|file| file.path.starts_with(owner_source_prefix));
    TransitionCoverageSummary {
        production_files: files.len(),
        covered_production_lines: files.iter().map(|file| file.covered_lines.len()).sum(),
        attributable_production_lines: files.iter().map(|file| file.attributable_lines.len()).sum(),
        ambiguous_macro_or_generic_lines: files
            .iter()
            .map(|file| file.ambiguous_macro_or_generic_lines.len())
            .sum(),
        transition_owner_files: owner_files.len(),
        transition_owner_attributable_lines: owner_files
            .iter()
            .map(|file| file.attributable_lines.len())
            .sum(),
        supporting_files: supporting_files.len(),
        supporting_attributable_lines: supporting_files
            .iter()
            .map(|file| file.attributable_lines.len())
            .sum(),
        executed_production_regions: regions.len(),
        attributable_production_regions: regions
            .iter()
            .filter(|region| matches!(region.attribution, RegionAttribution::Attributable))
            .count(),
    }
}

fn write_report(
    output_dir: &Path,
    report: &TransitionCaptureReport,
    llvm_export: &[u8],
    lcov: &[u8],
) -> Result<()> {
    let mut markdown = format!(
        "# Candidate transition coverage: {} / {}\n\n\
         This is review-routing evidence only. It contributes zero official golden coverage.\n\n\
         - Predecessor: `{}`\n\
         - Successor: `{}`\n\
         - Exact transition count: `{}`\n\
         - Calibration: `{}/{}/{}`\n\
         - Normalized replay digest: `{}`\n\
         - Profiler runtime image: `{}`\n\
         - Mapped executable images: `{}`\n\
         - Attributable production lines: `{}`\n\
         - Transition-owner attributable lines: `{}` in `{}` files\n\
         - Supporting attributable lines: `{}` in `{}` files\n\
         - Ambiguous macro/generic lines: `{}`\n\n",
        report.model,
        report.transition,
        report.predecessor,
        report.successor,
        report.calibration.transition_function_count,
        report.calibration.before_reset,
        report.calibration.inside_window,
        report.calibration.after_dump,
        report.replay.normalized_llvm_export_sha256,
        report.runner.profiler_runtime_image,
        report.runner.runtime_images.len(),
        report.summary.attributable_production_lines,
        report.summary.transition_owner_attributable_lines,
        report.summary.transition_owner_files,
        report.summary.supporting_attributable_lines,
        report.summary.supporting_files,
        report.summary.ambiguous_macro_or_generic_lines,
    );
    for file in &report.files {
        markdown.push_str(&format!(
            "## `{}`\n\n- SHA-256: `{}`\n- Attributable lines: `{}`\n",
            file.path,
            file.source_sha256,
            compact_ranges(&file.attributable_lines)
        ));
        if !file.ambiguous_macro_or_generic_lines.is_empty() {
            markdown.push_str(&format!(
                "- Excluded ambiguous lines: `{}`\n",
                compact_ranges(&file.ambiguous_macro_or_generic_lines)
            ));
        }
        markdown.push('\n');
    }
    write_atomic(&output_dir.join("llvm-export.json"), llvm_export)?;
    write_atomic(&output_dir.join("phase.lcov"), lcov)?;
    write_atomic(&output_dir.join("review.md"), markdown.as_bytes())?;
    let mut report_json = serde_json::to_vec_pretty(report)?;
    report_json.push(b'\n');
    write_atomic(&output_dir.join("capture.json"), &report_json)?;
    Ok(())
}

fn write_atomic(path: &Path, bytes: &[u8]) -> Result<()> {
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .context("capture output path has no UTF-8 file name")?;
    let temporary = path.with_file_name(format!("{file_name}.tmp"));
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&temporary)
        .with_context(|| format!("refusing to overwrite {}", temporary.display()))?;
    file.write_all(bytes)?;
    file.sync_all()?;
    drop(file);
    fs::rename(&temporary, path).with_context(|| {
        format!(
            "failed to publish {} as {}",
            temporary.display(),
            path.display()
        )
    })
}

fn compact_ranges(lines: &[u32]) -> String {
    let Some((&first, rest)) = lines.split_first() else {
        return String::new();
    };
    let mut rendered = Vec::new();
    let mut start = first;
    let mut end = first;
    for &line in rest {
        if end.checked_add(1) == Some(line) {
            end = line;
        } else {
            rendered.push(render_range(start, end));
            start = line;
            end = line;
        }
    }
    rendered.push(render_range(start, end));
    rendered.join(", ")
}

fn render_range(start: u32, end: u32) -> String {
    if start == end {
        start.to_string()
    } else {
        format!("{start}-{end}")
    }
}

fn llvm_tools(root: &Path) -> Result<LlvmTools> {
    let rustc_identity = command_text(root, "rustc", &["-vV"])?;
    let host = rustc_identity
        .lines()
        .find_map(|line| line.strip_prefix("host: "))
        .context("rustc identity has no host triple")?;
    let sysroot = command_text(root, "rustc", &["--print", "sysroot"])?;
    let bin = Path::new(sysroot.trim())
        .join("lib")
        .join("rustlib")
        .join(host)
        .join("bin");
    let tools = LlvmTools {
        profdata: bin.join(crate::exe_name("llvm-profdata")),
        cov: bin.join(crate::exe_name("llvm-cov")),
        nm: bin.join(crate::exe_name("llvm-nm")),
        readobj: bin.join(crate::exe_name("llvm-readobj")),
        rustc_identity,
    };
    ensure!(
        tools.profdata.is_file()
            && tools.cov.is_file()
            && tools.nm.is_file()
            && tools.readobj.is_file(),
        "active toolchain lacks llvm-tools-preview; enter `nix develop .#full`"
    );
    Ok(tools)
}

fn command_text(root: &Path, program: &str, arguments: &[&str]) -> Result<String> {
    let output = Command::new(program)
        .args(arguments)
        .current_dir(root)
        .output()
        .with_context(|| format!("failed to run `{program} {}`", arguments.join(" ")))?;
    ensure!(
        output.status.success(),
        "`{program} {}` failed with {}",
        arguments.join(" "),
        output.status
    );
    String::from_utf8(output.stdout)
        .context("tool identity output is not UTF-8")
        .map(|text| text.trim().to_string())
}

fn run_checked(command: &mut Command, label: &str) -> Result<()> {
    let output = command
        .output()
        .with_context(|| format!("failed to run {label}"))?;
    ensure!(
        output.status.success(),
        "{label} failed with {}:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(())
}

fn sha256_bytes(bytes: &[u8]) -> String {
    use sha2::Digest as _;
    format!("{:x}", sha2::Sha256::digest(bytes))
}

fn invalidate_capture(output_dir: &Path) -> Result<()> {
    for name in [
        "capture.json",
        "first.profdata",
        "first.profraw",
        "llvm-export.json",
        "phase.lcov",
        "replay.profdata",
        "replay.profraw",
        "review.md",
    ] {
        for path in [
            output_dir.join(name),
            output_dir.join(format!("{name}.tmp")),
        ] {
            match fs::remove_file(path) {
                Ok(()) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(error.into()),
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn llvm_fixture(before: u64, inside: u64, after: u64, transition: u64) -> Value {
        let function = |name: &str, count: u64| {
            serde_json::json!({
                "name": name,
                "count": count,
                "filenames": ["/workspace/crates/example/src/lib.rs"],
                "regions": [],
            })
        };
        serde_json::json!({
            "type": "llvm.coverage.json.export",
            "data": [{
                "functions": [
                    function(
                        "golden_flatten_transition_capture::calibration_before_reset",
                        before,
                    ),
                    function(
                        "golden_flatten_transition_capture::calibration_inside_window",
                        inside,
                    ),
                    function(
                        "golden_flatten_transition_capture::calibration_after_dump",
                        after,
                    ),
                    function(
                        "_RNvCsbl67CnlFoui_20rumoca_phase_flatten13flatten_typed",
                        transition,
                    ),
                ]
            }]
        })
    }

    #[test]
    fn cargo_artifact_selection_requires_one_exact_example() {
        let output = [
            serde_json::json!({
                "reason": "compiler-artifact",
                "target": {"name": "other", "kind": ["example"]},
                "executable": "/target/other"
            }),
            serde_json::json!({
                "reason": "compiler-artifact",
                "target": {"name": "golden", "kind": ["example"]},
                "executable": "/target/golden"
            }),
        ]
        .map(|value| value.to_string())
        .join("\n");
        assert_eq!(
            exact_example_executable(&output, "golden").expect("one exact artifact"),
            PathBuf::from("/target/golden")
        );
        assert!(exact_example_executable(&output, "missing").is_err());
    }

    #[test]
    fn calibration_accepts_only_zero_one_zero_and_one_transition() {
        let accepted = llvm_fixture(0, 1, 0, 1);
        let evidence = validate_calibration(&accepted, "rumoca_phase_flatten::flatten_typed")
            .expect("exact calibration is accepted");
        assert_eq!(evidence.transition_function_count, 1);

        for rejected in [
            llvm_fixture(1, 1, 0, 1),
            llvm_fixture(0, 0, 0, 1),
            llvm_fixture(0, 1, 1, 1),
            llvm_fixture(0, 1, 0, 2),
        ] {
            assert!(
                validate_calibration(&rejected, "rumoca_phase_flatten::flatten_typed").is_err()
            );
        }
    }

    #[test]
    fn review_lines_render_as_compact_ranges() {
        assert_eq!(compact_ranges(&[]), "");
        assert_eq!(compact_ranges(&[2, 3, 4, 9, 11, 12]), "2-4, 9, 11-12");
    }

    #[test]
    fn runtime_image_witness_requires_existing_sorted_paths() {
        let executable = std::env::current_exe().expect("test executable path");
        let witness = serde_json::to_vec(&vec![executable.display().to_string()])
            .expect("serialize image witness");
        assert_eq!(
            parse_runtime_images(&witness).expect("one exact executable image"),
            vec![executable]
        );
        assert!(parse_runtime_images(b"[]").is_err());
        assert!(parse_runtime_images(br#"["/missing"]"#).is_err());
    }

    #[test]
    fn profiler_runtime_requires_one_complete_exact_runner_owner() {
        let executable = std::env::current_exe().expect("test executable path");
        let path = executable.display().to_string();
        let executable_sha256 = sha256_file(&executable).expect("hash test executable");
        let image = |dump, reset, runtime, sections: &[&str]| RuntimeImageIdentity {
            path: path.clone(),
            sha256: executable_sha256.clone(),
            defines_profile_dump: dump,
            defines_profile_reset: reset,
            defines_profile_runtime_symbol: runtime,
            profiling_sections: sections
                .iter()
                .map(|section| (*section).to_string())
                .collect(),
        };
        assert_eq!(
            sole_profiler_runtime_image(
                &[image(true, true, true, &REQUIRED_ELF_PROFILE_SECTIONS)],
                &executable,
                &executable_sha256,
            )
            .expect("one complete exact owner"),
            path
        );
        assert!(
            sole_profiler_runtime_image(
                &[image(true, false, true, &REQUIRED_ELF_PROFILE_SECTIONS)],
                &executable,
                &executable_sha256,
            )
            .is_err()
        );
        assert!(
            sole_profiler_runtime_image(
                &[
                    image(true, true, true, &REQUIRED_ELF_PROFILE_SECTIONS),
                    image(false, false, false, &["__llvm_covmap"]),
                ],
                &executable,
                &executable_sha256,
            )
            .is_err()
        );
        assert!(
            sole_profiler_runtime_image(
                &[image(false, false, false, &[])],
                &executable,
                &executable_sha256,
            )
            .is_err()
        );
        let changed_hash = "00".repeat(32);
        assert!(
            sole_profiler_runtime_image(
                &[image(true, true, true, &REQUIRED_ELF_PROFILE_SECTIONS)],
                &executable,
                &changed_hash,
            )
            .is_err()
        );
    }

    #[test]
    fn profile_binary_id_parser_refuses_absent_or_malformed_identity() {
        assert_eq!(
            parse_profile_binary_ids("summary\nBinary IDs:\n  A0b1c2d3\n")
                .expect("one normalized binary ID"),
            BTreeSet::from(["a0b1c2d3".to_string()])
        );
        assert!(parse_profile_binary_ids("summary only\n").is_err());
        assert!(parse_profile_binary_ids("Binary IDs:\nnot-hex\n").is_err());
        assert!(parse_profile_binary_ids("Binary IDs:\n").is_err());
    }

    #[test]
    fn coverage_runner_flags_require_one_explicit_gnu_build_id() {
        let mut environment = BTreeMap::from([(
            "__CARGO_LLVM_COV_RUSTC_WRAPPER_RUSTFLAGS".to_string(),
            "-C\u{1f}instrument-coverage".to_string(),
        )]);
        require_runner_build_id(&mut environment).expect("append exact build-ID flag");
        assert_eq!(
            environment["__CARGO_LLVM_COV_RUSTC_WRAPPER_RUSTFLAGS"],
            "-C\u{1f}instrument-coverage\u{1f}-C\u{1f}link-arg=-Wl,--build-id=sha1"
        );
        assert!(require_runner_build_id(&mut environment).is_err());
    }

    #[test]
    fn zero_count_mapping_files_are_not_reported_as_executed() {
        let temporary = tempfile::tempdir().expect("temporary source root");
        let root = temporary.path();
        let parse = root.join("crates/rumoca-phase-parse/src/lib.rs");
        let flatten = root.join("crates/rumoca-phase-flatten/src/lib.rs");
        fs::create_dir_all(parse.parent().expect("parse parent")).expect("create parse source");
        fs::create_dir_all(flatten.parent().expect("flatten parent"))
            .expect("create flatten source");
        fs::write(&parse, "pub fn parse() {}\n").expect("write parse source");
        fs::write(&flatten, "pub fn flatten() {}\n").expect("write flatten source");
        let lcov = format!(
            "SF:{}\nDA:1,0\nend_of_record\nSF:{}\nDA:1,1\nend_of_record\n",
            parse.display(),
            flatten.display()
        );
        let files = production_file_coverage(root, lcov.as_bytes()).expect("project LCOV");
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].path, "crates/rumoca-phase-flatten/src/lib.rs");
        validate_transition_file_boundary(&files).expect("only Flatten executed");
    }

    #[test]
    fn capture_lock_refuses_same_pair_concurrency() {
        let temporary = tempfile::tempdir().expect("temporary capture root");
        let first = CaptureLock::acquire(temporary.path()).expect("first capture owns lock");
        assert!(CaptureLock::acquire(temporary.path()).is_err());
        first.release().expect("release first capture lock");
        CaptureLock::acquire(temporary.path())
            .expect("lock can be reacquired after an orderly release")
            .release()
            .expect("release second capture lock");
    }
}
