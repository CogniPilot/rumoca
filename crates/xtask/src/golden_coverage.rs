mod lcov;
mod report;
mod schema;
mod source_analysis;

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Write as _;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, ensure};
use clap::Args;
use serde::{Deserialize, Serialize};
use xtask::golden_registry::{
    self, CheckedCandidateCapture, CoverageTest, GoldenRegistry, ensure_safe_id,
    parse_golden_registry, scenario_identity, sha256_file,
};

pub(crate) use self::lcov::parse_lcov;
use self::report::{
    percentage, read_previous_denominator, write_aggregate_markdown, write_footprint, write_json,
};
pub(crate) use self::source_analysis::{analyze_source, production_relative_path};

const REGISTRY_PATH: &str = "infra/verification/golden-models/registry.toml";
const OUTPUT_ROOT: &str = "target/golden-coverage";
/// The independently versioned schema of the candidate-capture footprint this
/// tool writes. A footprint is review input, not golden proof.
const FOOTPRINT_SCHEMA_VERSION: u32 = 3;
/// The base cargo invocation for a workspace denominator build. A candidate
/// model's scenario-feature union is appended by `denominator_cargo_args`.
///
/// This deliberately omits `--all-features`. The denominator must be a superset
/// of every configuration the scenarios measure, and `--all-features` is not
/// that superset: fourteen production sites gate on feature ABSENCE via
/// `#[cfg(not(feature = ...))]` (connection and array-expansion tracing
/// fallbacks, resolve tracing, and the solver-absent simulation paths). Under
/// `--all-features` those blocks are compiled out of the denominator entirely,
/// yet the scenarios build with default features plus their own listed extras
/// and so compile and execute exactly those absence-gated lines. The union
/// would then hold covered coordinates the denominator could never contain.
/// Default features plus the candidate's declared union remains a genuine
/// superset of each isolated scenario build.
const DENOMINATOR_CARGO_ARGS: [&str; 7] = [
    "test",
    "--workspace",
    "--all-targets",
    "--no-run",
    "--jobs",
    "4",
    "--locked",
];

#[derive(Debug, Args, Clone)]
pub(crate) struct GoldenCoverageArgs {
    /// Exact model id from the checked golden-model registry.
    model: String,
}

#[derive(Debug, Args, Clone)]
pub(crate) struct GoldenCoverageAllArgs {}

struct CoverageSubject {
    id: String,
    claim: CaptureClaim,
    source: String,
    source_sha256: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
enum CaptureClaim {
    Candidate,
}

/// The footprint `coverage golden` writes for a candidate model.
#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct CoverageFootprint {
    schema_version: u32,
    model: String,
    capture_claim: CaptureClaim,
    model_source: String,
    model_source_sha256: String,
    summary: CoverageSummary,
    files: Vec<FileFootprint>,
    scenarios: Vec<ScenarioFootprint>,
}

/// Two different denominators exist for one covered-line count, and reporting a
/// ratio without saying which one produced it is how a density gets mistaken for
/// a coverage fraction. Both are recorded here, each named for its denominator,
/// so no reader has to infer which was used.
///
/// * *Linked* lines are the instrumentable production lines in the files that
///   were linked into this model's scenario binaries. A file the model never
///   reaches is absent from this total entirely, so the linked ratio measures
///   how densely the model covers the code it touches. It rises when a model
///   touches fewer files, which makes it useless as a progress measure.
/// * *Workspace* lines are every instrumentable production line in the
///   workspace, captured by a separate instrumented build
///   (`capture_denominator`) whose coordinate set is asserted to be a superset
///   of the covered coordinates. This is the denominator that answers "what
///   fraction of the compiler does this model execute", and it is the one the
///   golden-coverage campaign tracks.
///
/// For a single-state model the two differ by roughly a factor of three, so the
/// distinction is not academic.
#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
struct CoverageSummary {
    /// Instrumentable production lines in the linked files only. Context for the
    /// linked ratio; never a workspace total.
    linked_instrumentable_production_lines: usize,
    /// Instrumentable production lines in the whole workspace. `None` only for
    /// the denominator capture itself, which has no denominator of its own.
    workspace_instrumentable_production_lines: Option<usize>,
    covered_production_lines: usize,
    /// `covered_production_lines` minus the lines llvm-cov could not attribute to
    /// a function. This is the numerator of both the workspace ratio below and
    /// the golden-coverage union in `AggregateReport`, so the per-model share and
    /// the campaign total agree by construction rather than by coincidence.
    attributed_covered_production_lines: Option<usize>,
    macro_attributed_covered_lines: usize,
    unattributable_covered_lines: usize,
    /// `covered / linked`: density within the files this model touches.
    linked_line_coverage_percent: f64,
    /// `attributed covered / workspace`: the share of the compiler this model
    /// executes. This is the golden-coverage number.
    workspace_line_coverage_percent: Option<f64>,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct FileFootprint {
    path: String,
    source_sha256: String,
    #[serde(
        serialize_with = "report::serialize_line_ranges",
        deserialize_with = "report::deserialize_line_ranges"
    )]
    instrumentable_lines: Vec<u32>,
    #[serde(
        serialize_with = "report::serialize_line_ranges",
        deserialize_with = "report::deserialize_line_ranges"
    )]
    covered_lines: Vec<u32>,
    #[serde(
        serialize_with = "report::serialize_line_ranges",
        deserialize_with = "report::deserialize_line_ranges"
    )]
    macro_attributed_covered_lines: Vec<u32>,
    #[serde(
        serialize_with = "report::serialize_line_ranges",
        deserialize_with = "report::deserialize_line_ranges"
    )]
    unattributable_covered_lines: Vec<u32>,
    macro_attributed_function_identities: Vec<String>,
    executed_functions: Vec<ExecutedFunction>,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ExecutedFunction {
    definition_line: Option<u32>,
    llvm_name: String,
    demangled_name: String,
    execution_count: u64,
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ScenarioFootprint {
    declared_scenario_id: String,
    scenario_id: String,
    exact_test: CoverageTest,
    summary: CoverageSummary,
    files: Vec<FileFootprint>,
}

struct NormalizedCoverage {
    summary: CoverageSummary,
    files: Vec<FileFootprint>,
}

pub(crate) fn run_model(root: &Path, args: &GoldenCoverageArgs) -> Result<()> {
    ensure_coverage_tool(root)?;
    let registry = read_registry(root)?;
    let model = registry
        .models()
        .iter()
        .find(|model| model.id == args.model)
        .with_context(|| format!("golden-model registry has no `{}` record", args.model))?;
    let checked = golden_registry::check_candidate_capture(root, model)?;
    let mut footprint = capture_candidate_model(root, &checked)?;
    // The linked total this capture already has answers only "how densely does
    // this model cover the files it touches". The question the campaign asks is
    // what share of the workspace it executes, and that needs a denominator the
    // model's own run cannot see, so build it separately and bind it in here.
    let denominator = capture_denominator(root, &model_scenario_features(&checked))?;
    attach_workspace_denominator(&mut footprint, &instrumentable_coordinates(&denominator))?;
    let output_dir = model_output_dir(root, checked.id())?;
    write_footprint(&output_dir, &footprint)?;
    println!(
        "golden coverage {:?} `{}`: {} covered lines, {} attributed",
        footprint.capture_claim,
        checked.id(),
        footprint.summary.covered_production_lines,
        footprint
            .summary
            .attributed_covered_production_lines
            .unwrap_or_default(),
    );
    println!(
        "  workspace share: {:.2}% of {} instrumentable production lines  <- golden coverage",
        footprint
            .summary
            .workspace_line_coverage_percent
            .unwrap_or_default(),
        footprint
            .summary
            .workspace_instrumentable_production_lines
            .unwrap_or_default(),
    );
    println!(
        "  linked density:  {:.2}% of {} lines in the {} files it touches (not a workspace share)",
        footprint.summary.linked_line_coverage_percent,
        footprint.summary.linked_instrumentable_production_lines,
        footprint.files.len(),
    );
    println!(
        "review footprint: {}",
        output_dir.join("review.md").display()
    );
    println!(
        "machine footprint: {}",
        output_dir.join("footprint.json").display()
    );
    Ok(())
}

pub(crate) fn run_all(root: &Path, _args: &GoldenCoverageAllArgs) -> Result<()> {
    ensure_coverage_tool(root)?;
    let output_dir = root.join(OUTPUT_ROOT);
    fs::create_dir_all(&output_dir)?;
    let _aggregate_lock = CaptureLock::acquire(&output_dir.join("aggregate.lock"))?;
    let registry = read_registry(root)?;
    // The checked result deliberately carries no model or coordinate data, so
    // a candidate cannot become an aggregate numerator by construction.
    let admission = golden_registry::check_candidate_only_registry(root, registry)?;

    let denominator = capture_denominator(root, &BTreeSet::new())?;
    let denominator_coordinates = instrumentable_coordinates(&denominator);
    let denominator_size = NonZeroUsize::new(denominator_coordinates.len())
        .context("workspace instrumentable production-line denominator is empty")?;
    let previous_denominator = read_previous_denominator(&output_dir.join("aggregate.json"))?;
    let denominator_history = golden_registry::checked_aggregate_denominator_history(
        denominator_size,
        previous_denominator,
    )?;
    let report = golden_registry::candidate_only_aggregate_report(admission, denominator_history);
    write_json(&output_dir.join("aggregate.json"), &report)?;
    write_aggregate_markdown(&output_dir.join("aggregate.md"), &report)?;
    println!(
        "golden coverage: {}/{} production lines ({:.2}%) across {} admitted model(s)",
        report.union_covered_lines(),
        report.workspace_instrumentable_production_lines(),
        report.golden_line_coverage_percent(),
        report.admitted_model_count()
    );
    println!(
        "aggregate report: {}",
        output_dir.join("aggregate.md").display()
    );
    Ok(())
}

fn read_registry(root: &Path) -> Result<GoldenRegistry> {
    let path = root.join(REGISTRY_PATH);
    let source =
        fs::read_to_string(&path).with_context(|| format!("failed to read {}", path.display()))?;
    parse_golden_registry(&source, &path.display().to_string())
}

fn capture_candidate_model(
    root: &Path,
    checked: &CheckedCandidateCapture<'_>,
) -> Result<CoverageFootprint> {
    let output_dir = model_output_dir(root, checked.id())?;
    fs::create_dir_all(&output_dir)?;
    let _capture_lock = CaptureLock::acquire(&output_dir.join("capture.lock"))?;
    invalidate_model_evidence(&output_dir)?;
    let mut scenarios = Vec::new();
    for scenario_request in checked.scenarios() {
        let test = &scenario_request.test;
        let scenario_id = scenario_identity(test)?;
        let scenario_dir = output_dir.join("scenarios").join(&scenario_id);
        let build_dir = scenario_dir.join("llvm-target");
        fs::create_dir_all(&scenario_dir)?;
        prepare_build_dir(root, &build_dir)?;
        let coverage_env = coverage_environment(root, &build_dir)?;
        validate_registered_test(root, &coverage_env, test)?;
        clear_profile_data(&build_dir)?;
        run_exact_test(root, &coverage_env, test)?;
        let lcov_path = scenario_dir.join("raw.lcov");
        export_lcov(root, &coverage_env, &lcov_path)?;
        let normalized = normalize_lcov(root, &lcov_path, false)?;
        let scenario = ScenarioFootprint {
            declared_scenario_id: scenario_request.id.clone(),
            scenario_id,
            exact_test: test.clone(),
            summary: normalized.summary,
            files: normalized.files,
        };
        write_json(&scenario_dir.join("footprint.json"), &scenario)?;
        scenarios.push(scenario);
    }
    let normalized = merge_scenario_footprints(&scenarios)?;
    Ok(model_footprint(
        &CoverageSubject {
            id: checked.id().to_string(),
            claim: CaptureClaim::Candidate,
            source: checked.source().path.clone(),
            source_sha256: checked.source().sha256.clone(),
        },
        normalized,
        scenarios,
    ))
}

/// The features one model's scenarios build with, for its own denominator.
///
/// The checked candidate carrier is the only source: validation is not
/// discarded before denominator construction.
fn model_scenario_features(checked: &CheckedCandidateCapture<'_>) -> BTreeSet<String> {
    checked
        .scenarios()
        .iter()
        .flat_map(|scenario| scenario.test.features.iter().cloned())
        .collect()
}

/// Assemble the denominator invocation as the shared base plus the scenario
/// feature union. The union is appended through `--features`, which enables
/// those features in addition to the defaults; it never disables defaults, so
/// the resulting build is a superset of every scenario build.
fn denominator_cargo_args(scenario_features: &BTreeSet<String>) -> Vec<String> {
    let mut args: Vec<String> = DENOMINATOR_CARGO_ARGS
        .iter()
        .map(|argument| (*argument).to_string())
        .collect();
    if !scenario_features.is_empty() {
        args.push("--features".to_string());
        args.push(
            scenario_features
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .join(","),
        );
    }
    args
}

fn capture_denominator(
    root: &Path,
    scenario_features: &BTreeSet<String>,
) -> Result<CoverageFootprint> {
    let output_dir = root.join(OUTPUT_ROOT).join("workspace-denominator");
    let build_dir = output_dir.join("llvm-target");
    fs::create_dir_all(&output_dir)?;
    let _capture_lock = CaptureLock::acquire(&output_dir.join("capture.lock"))?;
    remove_file_if_present(&output_dir.join("denominator.lcov"))?;
    prepare_build_dir(root, &build_dir)?;
    let coverage_env = coverage_environment(root, &build_dir)?;
    run_cargo(
        root,
        &coverage_env,
        &[
            "test",
            "--package",
            "xtask",
            "--bin",
            "xtask",
            "--jobs",
            "4",
            "--",
            "golden_coverage::tests::covered_lines_render_as_compact_review_ranges",
            "--exact",
        ],
    )?;
    let denominator_args = denominator_cargo_args(scenario_features);
    let denominator_args: Vec<&str> = denominator_args.iter().map(String::as_str).collect();
    run_cargo(root, &coverage_env, &denominator_args)?;
    let lcov_path = output_dir.join("denominator.lcov");
    export_lcov(root, &coverage_env, &lcov_path)?;
    let synthetic = CoverageSubject {
        id: "workspace-denominator".to_string(),
        claim: CaptureClaim::Candidate,
        source: REGISTRY_PATH.to_string(),
        source_sha256: sha256_file(&root.join(REGISTRY_PATH))?,
    };
    let normalized = normalize_lcov(root, &lcov_path, true)?;
    Ok(model_footprint(&synthetic, normalized, Vec::new()))
}

fn run_exact_test(
    root: &Path,
    coverage_env: &BTreeMap<String, String>,
    test: &CoverageTest,
) -> Result<()> {
    let mut args = cargo_test_target_args(test)?;
    args.extend([
        "--".to_string(),
        test.test_name.clone(),
        "--exact".to_string(),
    ]);
    run_cargo_owned(root, coverage_env, &args).with_context(|| {
        format!(
            "exact golden test failed: {} / {} / {}",
            test.package, test.test_target, test.test_name
        )
    })
}

fn validate_registered_test(
    root: &Path,
    coverage_env: &BTreeMap<String, String>,
    test: &CoverageTest,
) -> Result<()> {
    let mut list_args = cargo_test_target_args(test)?;
    list_args.extend([
        "--".to_string(),
        test.test_name.clone(),
        "--exact".to_string(),
        "--list".to_string(),
        "--format".to_string(),
        "terse".to_string(),
    ]);
    let listing = run_cargo_owned_output(root, coverage_env, &list_args)?;
    validate_exact_test_listing(&test.test_name, &listing)
}

fn cargo_test_target_args(test: &CoverageTest) -> Result<Vec<String>> {
    let mut args = vec![
        "test".to_string(),
        "--package".to_string(),
        test.package.clone(),
        "--test".to_string(),
        test.test_target.clone(),
        "--jobs".to_string(),
        "4".to_string(),
    ];
    if !test.features.is_empty() {
        // Every listed feature is enabled ON TOP OF the default set through a
        // single `--features` argument. The workspace denominator is built as
        // default features plus the union of these extras and is therefore only
        // a superset of this scenario while the scenario keeps its defaults. A
        // feature atom that began with a dash (a `--no-default-features` smuggled
        // through the feature list, or any other flag) would break that superset
        // relationship, so refuse it here rather than let the denominator quietly
        // stop measuring the same program the scenario executes.
        for feature in &test.features {
            ensure!(
                !feature.is_empty() && !feature.starts_with('-'),
                "scenario feature `{feature}` may not be empty or disable default features"
            );
        }
        args.push("--features".to_string());
        args.push(test.features.join(","));
    }
    Ok(args)
}

pub(crate) fn run_cargo_owned_output(
    root: &Path,
    coverage_env: &BTreeMap<String, String>,
    args: &[String],
) -> Result<String> {
    let output = Command::new("cargo")
        .args(args)
        .current_dir(root)
        .envs(coverage_env)
        .output()
        .with_context(|| format!("failed to run `cargo {}`", args.join(" ")))?;
    ensure!(
        output.status.success(),
        "`cargo {}` failed with {}:\n{}",
        args.join(" "),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).context("cargo test listing was not UTF-8")
}

fn validate_exact_test_listing(test_name: &str, listing: &str) -> Result<()> {
    let expected = format!("{test_name}: test");
    let listed: Vec<&str> = listing.lines().filter(|line| !line.is_empty()).collect();
    ensure!(
        listed == [expected.as_str()],
        "exact golden test `{test_name}` must list once and only once; observed {listed:?}"
    );
    Ok(())
}

fn export_lcov(root: &Path, coverage_env: &BTreeMap<String, String>, output: &Path) -> Result<()> {
    let args = [
        "--lcov".to_string(),
        "--output-path".to_string(),
        output.display().to_string(),
    ];
    let mut command = Command::new("cargo");
    command
        .arg("llvm-cov")
        .arg("report")
        .args(args)
        .current_dir(root)
        .envs(coverage_env);
    run_command(command, "cargo llvm-cov report")
}

pub(crate) fn coverage_environment(
    root: &Path,
    build_dir: &Path,
) -> Result<BTreeMap<String, String>> {
    let output = Command::new("cargo")
        .args(["llvm-cov", "show-env"])
        .current_dir(root)
        .env("CARGO_LLVM_COV_TARGET_DIR", build_dir)
        .output()
        .context("failed to read cargo-llvm-cov environment")?;
    ensure!(
        output.status.success(),
        "`cargo llvm-cov show-env` failed with {}",
        output.status
    );
    let mut environment = BTreeMap::new();
    for line in String::from_utf8(output.stdout)?.lines() {
        let (key, value) = line
            .split_once('=')
            .with_context(|| format!("malformed cargo-llvm-cov environment row `{line}`"))?;
        environment.insert(key.to_string(), unquote_shell_value(value)?.to_string());
    }
    environment.insert(
        "CARGO_TARGET_DIR".to_string(),
        build_dir.display().to_string(),
    );
    environment.insert(
        "CARGO_LLVM_COV_TARGET_DIR".to_string(),
        build_dir.display().to_string(),
    );
    environment.insert(
        "CARGO_LLVM_COV_BUILD_DIR".to_string(),
        build_dir.display().to_string(),
    );
    environment.insert("CARGO_BUILD_JOBS".to_string(), "4".to_string());
    environment.insert("RUST_TEST_THREADS".to_string(), "4".to_string());
    environment.insert("RAYON_NUM_THREADS".to_string(), "4".to_string());
    Ok(environment)
}

fn unquote_shell_value(value: &str) -> Result<&str> {
    if let Some(value) = value.strip_prefix('\'') {
        return value
            .strip_suffix('\'')
            .context("unterminated quoted cargo-llvm-cov environment value");
    }
    Ok(value)
}

fn run_cargo(root: &Path, coverage_env: &BTreeMap<String, String>, args: &[&str]) -> Result<()> {
    let owned: Vec<String> = args.iter().map(|value| (*value).to_string()).collect();
    run_cargo_owned(root, coverage_env, &owned)
}

fn run_cargo_owned(
    root: &Path,
    coverage_env: &BTreeMap<String, String>,
    args: &[String],
) -> Result<()> {
    let mut command = Command::new("cargo");
    command.args(args).current_dir(root).envs(coverage_env);
    run_command(command, &format!("cargo {}", args.join(" ")))
}

fn run_command(mut command: Command, display: &str) -> Result<()> {
    let status = command
        .status()
        .with_context(|| format!("failed to run `{display}`"))?;
    ensure!(status.success(), "`{display}` failed with {status}");
    Ok(())
}

pub(crate) fn ensure_coverage_tool(root: &Path) -> Result<()> {
    let status = Command::new("cargo")
        .args(["llvm-cov", "--version"])
        .current_dir(root)
        .status()
        .context("failed to run cargo-llvm-cov")?;
    ensure!(
        status.success(),
        "cargo-llvm-cov is unavailable; enter `nix develop .#full` or install it"
    );
    Ok(())
}

fn normalize_lcov(
    root: &Path,
    path: &Path,
    retain_uncovered_files: bool,
) -> Result<NormalizedCoverage> {
    let source =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    let files = parse_lcov(&source)?;
    let mut normalized = Vec::new();
    for (path, coverage) in files {
        let Some(relative) = production_relative_path(root, &path) else {
            continue;
        };
        let absolute = root.join(&relative);
        let analysis = analyze_source(&absolute)?;
        let instrumentable_lines: Vec<u32> = coverage
            .lines
            .keys()
            .copied()
            .filter(|line| !analysis.test_lines.contains(line))
            .collect();
        let covered_lines: Vec<u32> = coverage
            .lines
            .iter()
            .filter(|(line, count)| **count > 0 && !analysis.test_lines.contains(line))
            .map(|(line, _)| *line)
            .collect();
        let macro_attributed_covered_lines: Vec<u32> = covered_lines
            .iter()
            .copied()
            .filter(|line| analysis.macro_lines.contains(line))
            .collect();
        let unattributable_covered_lines: Vec<u32> = covered_lines
            .iter()
            .copied()
            .filter(|line| {
                analysis.macro_lines.contains(line) || analysis.generic_lines.contains(line)
            })
            .collect();
        let executed_functions = coverage
            .function_counts
            .iter()
            .filter(|(name, count)| {
                **count > 0
                    && coverage.function_lines.get(*name).is_none_or(|line| {
                        !analysis.test_lines.contains(line)
                            && !analysis.macro_lines.contains(line)
                            && !analysis.generic_lines.contains(line)
                    })
            })
            .map(|(name, count)| ExecutedFunction {
                definition_line: coverage.function_lines.get(name).copied(),
                llvm_name: name.clone(),
                demangled_name: rustc_demangle::demangle(name).to_string(),
                execution_count: *count,
            })
            .collect();
        let macro_attributed_function_identities = coverage
            .function_counts
            .iter()
            .filter(|(name, count)| {
                **count > 0
                    && coverage.function_lines.get(*name).is_some_and(|line| {
                        !analysis.test_lines.contains(line) && analysis.macro_lines.contains(line)
                    })
            })
            .map(|(name, _)| rustc_demangle::demangle(name).to_string())
            .collect();
        normalized.push(FileFootprint {
            path: relative,
            source_sha256: sha256_file(&absolute)?,
            instrumentable_lines,
            covered_lines,
            macro_attributed_covered_lines,
            unattributable_covered_lines,
            macro_attributed_function_identities,
            executed_functions,
        });
    }
    normalized.sort_by(|left, right| left.path.cmp(&right.path));
    let summary = summarize(&normalized);
    if !retain_uncovered_files {
        normalized.retain(|file| !file.covered_lines.is_empty());
    }
    Ok(NormalizedCoverage {
        summary,
        files: normalized,
    })
}

fn summarize(files: &[FileFootprint]) -> CoverageSummary {
    let instrumentable = files
        .iter()
        .map(|file| file.instrumentable_lines.len())
        .sum();
    let covered = files.iter().map(|file| file.covered_lines.len()).sum();
    let macros = files
        .iter()
        .map(|file| file.macro_attributed_covered_lines.len())
        .sum();
    let unattributable = files
        .iter()
        .map(|file| file.unattributable_covered_lines.len())
        .sum();
    CoverageSummary {
        linked_instrumentable_production_lines: instrumentable,
        workspace_instrumentable_production_lines: None,
        covered_production_lines: covered,
        attributed_covered_production_lines: None,
        macro_attributed_covered_lines: macros,
        unattributable_covered_lines: unattributable,
        linked_line_coverage_percent: percentage(covered, instrumentable),
        workspace_line_coverage_percent: None,
    }
}

/// Attach the workspace denominator to a model footprint.
///
/// `summarize` can only see the linked files, so the workspace share is filled
/// in here from a separate instrumented build of the whole workspace. The
/// denominator must be a superset of what the model covered; if it is not, the
/// two builds disagree about what production code is and the resulting ratio
/// would be meaningless, so this refuses rather than reporting it.
fn attach_workspace_denominator(
    footprint: &mut CoverageFootprint,
    denominator: &BTreeSet<(String, u32)>,
) -> Result<()> {
    ensure!(
        !denominator.is_empty(),
        "workspace instrumentable production-line denominator is empty"
    );
    let covered = attributed_covered_coordinates(footprint);
    let absent: Vec<_> = covered.difference(denominator).take(5).cloned().collect();
    ensure!(
        absent.is_empty(),
        "covered production coordinates absent from the workspace denominator \
         (showing up to 5): {absent:?}"
    );
    footprint.summary.attributed_covered_production_lines = Some(covered.len());
    footprint.summary.workspace_instrumentable_production_lines = Some(denominator.len());
    footprint.summary.workspace_line_coverage_percent =
        Some(percentage(covered.len(), denominator.len()));
    Ok(())
}

fn model_footprint(
    subject: &CoverageSubject,
    normalized: NormalizedCoverage,
    scenarios: Vec<ScenarioFootprint>,
) -> CoverageFootprint {
    CoverageFootprint {
        schema_version: FOOTPRINT_SCHEMA_VERSION,
        model: subject.id.clone(),
        capture_claim: subject.claim,
        model_source: subject.source.clone(),
        model_source_sha256: subject.source_sha256.clone(),
        summary: normalized.summary,
        files: normalized.files,
        scenarios,
    }
}

fn merge_scenario_footprints(scenarios: &[ScenarioFootprint]) -> Result<NormalizedCoverage> {
    let mut files = BTreeMap::<String, FileFootprint>::new();
    for scenario in scenarios {
        for source in &scenario.files {
            if let Some(target) = files.get_mut(&source.path) {
                merge_file_footprint(target, source)?;
            } else {
                files.insert(source.path.clone(), source.clone());
            }
        }
    }
    let files: Vec<FileFootprint> = files.into_values().collect();
    Ok(NormalizedCoverage {
        summary: summarize(&files),
        files,
    })
}

fn merge_file_footprint(target: &mut FileFootprint, source: &FileFootprint) -> Result<()> {
    ensure!(
        target.source_sha256 == source.source_sha256,
        "scenario captures disagree on source digest for `{}`",
        target.path
    );
    merge_sorted_unique(
        &mut target.instrumentable_lines,
        &source.instrumentable_lines,
    );
    merge_sorted_unique(&mut target.covered_lines, &source.covered_lines);
    merge_sorted_unique(
        &mut target.macro_attributed_covered_lines,
        &source.macro_attributed_covered_lines,
    );
    merge_sorted_unique(
        &mut target.unattributable_covered_lines,
        &source.unattributable_covered_lines,
    );
    target
        .macro_attributed_function_identities
        .extend(source.macro_attributed_function_identities.iter().cloned());
    target.macro_attributed_function_identities.sort();
    target.macro_attributed_function_identities.dedup();
    target
        .executed_functions
        .extend(source.executed_functions.iter().cloned());
    target.executed_functions.sort_by(|left, right| {
        left.llvm_name
            .cmp(&right.llvm_name)
            .then_with(|| left.definition_line.cmp(&right.definition_line))
    });
    target.executed_functions.dedup_by(|left, right| {
        left.llvm_name == right.llvm_name && left.definition_line == right.definition_line
    });
    Ok(())
}

fn merge_sorted_unique(target: &mut Vec<u32>, source: &[u32]) {
    target.extend_from_slice(source);
    target.sort_unstable();
    target.dedup();
}

/// A covered line enters the union only when the capture could attribute it to
/// an executed function. Lines it could not attribute are recorded separately
/// and dropped here, so they neither inflate the union nor stand in need of an
/// identity to justify them.
fn line_is_counted(file: &FileFootprint, line: u32) -> bool {
    !file.unattributable_covered_lines.contains(&line)
}

fn attributed_covered_coordinates(footprint: &CoverageFootprint) -> BTreeSet<(String, u32)> {
    footprint
        .files
        .iter()
        .flat_map(|file| {
            file.covered_lines
                .iter()
                .filter(|line| line_is_counted(file, **line))
                .map(|line| (file.path.clone(), *line))
        })
        .collect()
}

fn instrumentable_coordinates(footprint: &CoverageFootprint) -> BTreeSet<(String, u32)> {
    footprint
        .files
        .iter()
        .flat_map(|file| {
            file.instrumentable_lines
                .iter()
                .map(|line| (file.path.clone(), *line))
        })
        .collect()
}

fn model_output_dir(root: &Path, id: &str) -> Result<PathBuf> {
    ensure_safe_id(id)?;
    Ok(root.join(OUTPUT_ROOT).join(id))
}

fn prepare_build_dir(root: &Path, path: &Path) -> Result<()> {
    let expected = root.join(OUTPUT_ROOT);
    ensure!(
        path.starts_with(&expected) && path != expected,
        "refusing to prepare unsafe coverage path {}",
        path.display()
    );
    fs::create_dir_all(path)?;
    clear_profile_data(path)
}

struct CaptureLock {
    path: PathBuf,
}

impl CaptureLock {
    fn acquire(path: &Path) -> Result<Self> {
        let mut file = fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(path)
            .with_context(|| {
                format!(
                    "golden coverage capture is already active or left an unresolved lock at {}",
                    path.display()
                )
            })?;
        writeln!(file, "pid={}", std::process::id())?;
        Ok(Self {
            path: path.to_path_buf(),
        })
    }
}

impl Drop for CaptureLock {
    fn drop(&mut self) {
        if let Err(error) = fs::remove_file(&self.path) {
            eprintln!(
                "warning: failed to release golden coverage lock {}: {error}",
                self.path.display()
            );
        }
    }
}

fn invalidate_model_evidence(output_dir: &Path) -> Result<()> {
    for name in ["raw.lcov", "footprint.json", "review.md"] {
        remove_file_if_present(&output_dir.join(name))?;
    }
    Ok(())
}

fn remove_file_if_present(path: &Path) -> Result<()> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error).with_context(|| format!("failed to remove {}", path.display())),
    }
}

fn clear_profile_data(path: &Path) -> Result<()> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && matches!(
                entry.path().extension().and_then(|value| value.to_str()),
                Some("profraw" | "profdata")
            )
        {
            fs::remove_file(entry.path())?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests;
