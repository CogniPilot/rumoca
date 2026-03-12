use anyhow::{Context, Result, bail, ensure};
use clap::Args as ClapArgs;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

const DEFAULT_BASELINE_FILE_REL: &str = "crates/rumoca-tool-dev/coverage/trim-gate-baseline.json";
const DEFAULT_CANDIDATES_FILE_REL: &str = "target/llvm-cov/trim-candidates.json";
const DEFAULT_DIFF_FILE_REL: &str = "target/llvm-cov/coverage-gate.md";
const DEFAULT_SUMMARY_FILE_NAME: &str = "workspace-summary.json";
const GENERATED_BY: &str = "rum coverage gate";

#[derive(Debug, Clone, ClapArgs)]
pub(crate) struct CoverageGateArgs {
    /// Optional current trim-candidates JSON (default: target/llvm-cov/trim-candidates.json)
    #[arg(long)]
    candidates_file: Option<PathBuf>,
    /// Optional committed baseline JSON (default: crates/rumoca-tool-dev/coverage/trim-gate-baseline.json)
    #[arg(long)]
    baseline_file: Option<PathBuf>,
    /// Optional markdown diff output path (default: target/llvm-cov/coverage-gate.md)
    #[arg(long)]
    diff_file: Option<PathBuf>,
    /// Enforce trim candidate regressions as hard failures.
    #[arg(long, default_value_t = false)]
    enforce_trim_regressions: bool,
    /// Allowed drop in workspace line coverage percentage from baseline.
    #[arg(long, default_value_t = 0.25)]
    allowed_workspace_line_coverage_drop: f64,
    /// Restrict gate to selected package(s). If omitted, baseline package set is used.
    #[arg(long = "package", short = 'p')]
    packages: Vec<String>,
    /// Allowed growth for zero-count function totals per package.
    #[arg(long, default_value_t = 0)]
    allowed_zero_count_growth: usize,
    /// Allowed growth for dead_likely candidate totals per package.
    #[arg(long, default_value_t = 0)]
    allowed_dead_likely_growth: usize,
    /// Allowed growth for total candidate totals per package.
    #[arg(long, default_value_t = 0)]
    allowed_total_candidate_growth: usize,
    /// Allowed growth for needs_targeted_test candidate totals per package.
    #[arg(long, default_value_t = 0)]
    allowed_needs_targeted_test_growth: usize,
    /// Promote current metrics to baseline instead of enforcing the gate.
    #[arg(long, default_value_t = false)]
    promote_baseline: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct PackageGateMetrics {
    zero_count_functions_total: usize,
    candidates_total: usize,
    dead_likely_candidates: usize,
    needs_targeted_test_candidates: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CoverageTrimGateBaseline {
    generated_by: String,
    generated_at_unix_secs: u64,
    source_candidates_file: String,
    #[serde(default)]
    workspace_line_coverage_percent: Option<f64>,
    #[serde(default)]
    workspace_lines_covered: Option<u64>,
    #[serde(default)]
    workspace_lines_total: Option<u64>,
    packages: BTreeMap<String, PackageGateMetrics>,
}

#[derive(Debug, Clone)]
struct GateComparison {
    package: String,
    baseline: PackageGateMetrics,
    current: PackageGateMetrics,
}

#[derive(Debug, Clone, Copy)]
enum MetricKind {
    ZeroCountFunctionsTotal,
    CandidatesTotal,
    DeadLikelyCandidates,
    NeedsTargetedTestCandidates,
}

impl MetricKind {
    fn label(self) -> &'static str {
        match self {
            Self::ZeroCountFunctionsTotal => "zero_count_functions_total",
            Self::CandidatesTotal => "candidates_total",
            Self::DeadLikelyCandidates => "dead_likely_candidates",
            Self::NeedsTargetedTestCandidates => "needs_targeted_test_candidates",
        }
    }
}

pub(crate) fn run(root: &Path, args: &CoverageGateArgs) -> Result<()> {
    let candidates_path = resolve_path(
        root,
        args.candidates_file.as_ref(),
        DEFAULT_CANDIDATES_FILE_REL,
    );
    ensure!(
        candidates_path.is_file(),
        "missing trim candidates JSON '{}'; run `rum coverage report` first",
        candidates_path.display()
    );
    let baseline_path = resolve_path(root, args.baseline_file.as_ref(), DEFAULT_BASELINE_FILE_REL);
    let diff_path = resolve_path(root, args.diff_file.as_ref(), DEFAULT_DIFF_FILE_REL);

    let current_metrics = load_current_metrics(&candidates_path)?;
    let package_filter = selected_packages(&args.packages, None, &current_metrics);

    if args.promote_baseline {
        promote_baseline(
            &baseline_path,
            &candidates_path,
            &current_metrics,
            &package_filter,
        )?;
        println!(
            "Coverage trim baseline updated: {}",
            baseline_path.display()
        );
        return Ok(());
    }

    ensure!(
        baseline_path.is_file(),
        "missing coverage baseline '{}'; run `rum coverage gate --promote-baseline`",
        baseline_path.display()
    );
    let baseline = load_baseline(&baseline_path)?;
    let package_filter = selected_packages(&args.packages, Some(&baseline), &current_metrics);
    let comparisons = build_comparisons(
        &baseline,
        &current_metrics,
        &package_filter,
        !args.packages.is_empty(),
    )?;
    let baseline_workspace = baseline
        .workspace_line_coverage_percent
        .zip(baseline.workspace_lines_covered)
        .zip(baseline.workspace_lines_total)
        .map(|((percent, covered), total)| WorkspaceLineCoverage {
            percent,
            covered,
            total,
        });
    let current_workspace = load_workspace_line_coverage(&candidates_path);
    let workspace_coverage_failure = compare_workspace_line_coverage(
        baseline_workspace,
        current_workspace,
        args.allowed_workspace_line_coverage_drop,
    );

    let (diff_markdown, trim_failures) = build_gate_diff(
        &comparisons,
        args,
        &baseline_path,
        &candidates_path,
        baseline_workspace,
        current_workspace,
    );
    write_text_file(&diff_path, &diff_markdown)?;

    finalize_gate_result(
        comparisons.len(),
        &diff_path,
        trim_failures,
        workspace_coverage_failure,
        args.enforce_trim_regressions,
    )
}

fn resolve_path(root: &Path, user_path: Option<&PathBuf>, default_rel: &str) -> PathBuf {
    let path = user_path
        .cloned()
        .unwrap_or_else(|| PathBuf::from(default_rel));
    if path.is_absolute() {
        return path;
    }
    root.join(path)
}

fn finalize_gate_result(
    package_count: usize,
    diff_path: &Path,
    trim_failures: Vec<String>,
    workspace_coverage_failure: Option<String>,
    enforce_trim_regressions: bool,
) -> Result<()> {
    let has_trim_failures = !trim_failures.is_empty();
    let trim_is_fatal = enforce_trim_regressions && has_trim_failures;
    let has_workspace_failure = workspace_coverage_failure.is_some();

    if !has_workspace_failure && !trim_is_fatal {
        println!(
            "Coverage gate: PASS ({} package(s)). Diff: {}",
            package_count,
            diff_path.display()
        );
        if has_trim_failures {
            println!(
                "Coverage gate: trim regressions detected (non-fatal). Re-run with --enforce-trim-regressions to fail on these."
            );
            for warning in trim_failures {
                println!("- {warning}");
            }
        }
        return Ok(());
    }

    let mut message = format!("coverage gate failed; diff: {}\n", diff_path.display());
    if let Some(workspace_failure) = workspace_coverage_failure {
        message.push_str("- ");
        message.push_str(&workspace_failure);
        message.push('\n');
    }
    if trim_is_fatal {
        message.push_str(&format!(
            "- trim regressions: {} (strict mode enabled)\n",
            trim_failures.len()
        ));
        for failure in trim_failures {
            message.push_str("  - ");
            message.push_str(&failure);
            message.push('\n');
        }
    }
    bail!(message.trim_end().to_string());
}

fn load_current_metrics(path: &Path) -> Result<BTreeMap<String, PackageGateMetrics>> {
    let payload = read_json(path)?;
    let mut metrics = BTreeMap::<String, PackageGateMetrics>::new();

    if let Some(packages) = payload.get("packages").and_then(Value::as_array) {
        for package in packages {
            let Some(name) = package
                .get("package")
                .and_then(Value::as_str)
                .map(str::trim)
            else {
                continue;
            };
            if name.is_empty() {
                continue;
            }
            let Some(zero_count) = package
                .get("zero_count_functions_total")
                .and_then(Value::as_u64)
                .and_then(|count| usize::try_from(count).ok())
            else {
                continue;
            };
            metrics
                .entry(name.to_string())
                .or_default()
                .zero_count_functions_total = zero_count;
        }
    }

    if let Some(candidates) = payload.get("candidates").and_then(Value::as_array) {
        for candidate in candidates {
            let Some(name) = candidate
                .get("package")
                .and_then(Value::as_str)
                .map(str::trim)
            else {
                continue;
            };
            if name.is_empty() {
                continue;
            }
            let entry = metrics.entry(name.to_string()).or_default();
            entry.candidates_total += 1;
            let triage_label = candidate
                .get("triage_label")
                .and_then(Value::as_str)
                .unwrap_or_default();
            match triage_label {
                "dead_likely" => entry.dead_likely_candidates += 1,
                "needs_targeted_test" => entry.needs_targeted_test_candidates += 1,
                _ => {}
            }
        }
    }

    ensure!(
        !metrics.is_empty(),
        "trim candidates payload '{}' did not contain package metrics",
        path.display()
    );
    Ok(metrics)
}

fn read_json(path: &Path) -> Result<Value> {
    let raw = fs::read_to_string(path)
        .with_context(|| format!("failed to read JSON '{}'", path.display()))?;
    serde_json::from_str(&raw).with_context(|| format!("failed to parse JSON '{}'", path.display()))
}

fn selected_packages(
    requested: &[String],
    baseline: Option<&CoverageTrimGateBaseline>,
    current_metrics: &BTreeMap<String, PackageGateMetrics>,
) -> Vec<String> {
    if !requested.is_empty() {
        let selected = requested
            .iter()
            .map(|package| package.trim())
            .filter(|package| !package.is_empty())
            .map(ToString::to_string)
            .collect::<BTreeSet<_>>();
        return selected.iter().cloned().collect();
    }
    if let Some(baseline) = baseline
        && !baseline.packages.is_empty()
    {
        return baseline.packages.keys().cloned().collect();
    }
    current_metrics.keys().cloned().collect()
}

fn promote_baseline(
    baseline_path: &Path,
    candidates_path: &Path,
    current_metrics: &BTreeMap<String, PackageGateMetrics>,
    selected_packages: &[String],
) -> Result<()> {
    let mut packages = BTreeMap::<String, PackageGateMetrics>::new();
    for package in selected_packages {
        let Some(metrics) = current_metrics.get(package) else {
            bail!(
                "cannot promote baseline: selected package '{}' not found in '{}'",
                package,
                candidates_path.display()
            );
        };
        packages.insert(package.clone(), metrics.clone());
    }

    let workspace_line_coverage = load_workspace_line_coverage(candidates_path);
    let baseline = CoverageTrimGateBaseline {
        generated_by: GENERATED_BY.to_string(),
        generated_at_unix_secs: unix_timestamp_seconds(),
        source_candidates_file: path_metadata_string(candidates_path),
        workspace_line_coverage_percent: workspace_line_coverage.map(|stats| stats.percent),
        workspace_lines_covered: workspace_line_coverage.map(|stats| stats.covered),
        workspace_lines_total: workspace_line_coverage.map(|stats| stats.total),
        packages,
    };
    let payload =
        serde_json::to_string_pretty(&baseline).context("failed to serialize baseline JSON")?;
    write_text_file(baseline_path, &payload)
}

#[derive(Debug, Clone, Copy)]
struct WorkspaceLineCoverage {
    percent: f64,
    covered: u64,
    total: u64,
}

fn load_workspace_line_coverage(candidates_path: &Path) -> Option<WorkspaceLineCoverage> {
    let summary_path = candidates_path.parent()?.join(DEFAULT_SUMMARY_FILE_NAME);
    let payload = read_json(&summary_path).ok()?;
    let lines = payload
        .get("data")
        .and_then(Value::as_array)
        .and_then(|data| data.first())
        .and_then(|entry| entry.get("totals"))
        .and_then(|totals| totals.get("lines"))?;
    let covered = lines.get("covered").and_then(Value::as_u64)?;
    let total = lines.get("count").and_then(Value::as_u64)?;
    let percent = lines.get("percent").and_then(Value::as_f64)?;
    Some(WorkspaceLineCoverage {
        percent,
        covered,
        total,
    })
}

fn load_baseline(path: &Path) -> Result<CoverageTrimGateBaseline> {
    let raw = fs::read_to_string(path)
        .with_context(|| format!("failed to read baseline JSON '{}'", path.display()))?;
    serde_json::from_str(&raw)
        .with_context(|| format!("failed to parse baseline '{}'", path.display()))
}

fn write_text_file(path: &Path, payload: &str) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory '{}'", parent.display()))?;
    }
    fs::write(path, payload).with_context(|| format!("failed to write '{}'", path.display()))
}

fn path_metadata_string(path: &Path) -> String {
    let Ok(cwd) = std::env::current_dir() else {
        return path.display().to_string();
    };
    if let Ok(relative) = path.strip_prefix(&cwd) {
        return relative.display().to_string();
    }
    path.display().to_string()
}

fn build_comparisons(
    baseline: &CoverageTrimGateBaseline,
    current_metrics: &BTreeMap<String, PackageGateMetrics>,
    selected_packages: &[String],
    strict_missing_current: bool,
) -> Result<Vec<GateComparison>> {
    let mut comparisons = Vec::new();
    for package in selected_packages {
        let Some(base) = baseline.packages.get(package) else {
            bail!(
                "baseline does not contain package '{}' (update baseline or select valid packages)",
                package
            );
        };
        let Some(current) = current_metrics.get(package) else {
            if strict_missing_current {
                bail!(
                    "current trim candidates are missing package '{}' (check package filters)",
                    package
                );
            }
            continue;
        };
        comparisons.push(GateComparison {
            package: package.clone(),
            baseline: base.clone(),
            current: current.clone(),
        });
    }
    Ok(comparisons)
}

fn build_gate_diff(
    comparisons: &[GateComparison],
    args: &CoverageGateArgs,
    baseline_path: &Path,
    candidates_path: &Path,
    baseline_workspace: Option<WorkspaceLineCoverage>,
    current_workspace: Option<WorkspaceLineCoverage>,
) -> (String, Vec<String>) {
    let mut markdown = String::new();
    markdown.push_str("# Coverage Gate\n\n");
    markdown.push_str(&format!("- baseline: `{}`\n", baseline_path.display()));
    markdown.push_str(&format!("- current: `{}`\n\n", candidates_path.display()));
    markdown.push_str("## Workspace line coverage\n\n");
    markdown.push_str("| baseline (%) | current (%) | delta | allowed drop | status |\n");
    markdown.push_str("| ---: | ---: | ---: | ---: | --- |\n");
    let (baseline_percent, current_percent, delta, status) = workspace_coverage_row(
        baseline_workspace,
        current_workspace,
        args.allowed_workspace_line_coverage_drop,
    );
    markdown.push_str(&format!(
        "| {} | {} | {} | `-{:.2}` | {} |\n\n",
        baseline_percent, current_percent, delta, args.allowed_workspace_line_coverage_drop, status
    ));
    markdown.push_str("## Trim candidate regressions\n\n");
    markdown.push_str("| package | metric | baseline | current | delta | allowance | status |\n");
    markdown.push_str("| --- | --- | ---: | ---: | ---: | ---: | --- |\n");

    let mut failures = Vec::new();
    for comparison in comparisons {
        push_metric_row(
            &mut markdown,
            &mut failures,
            &comparison.package,
            MetricKind::ZeroCountFunctionsTotal,
            comparison.baseline.zero_count_functions_total,
            comparison.current.zero_count_functions_total,
            args.allowed_zero_count_growth,
        );
        push_metric_row(
            &mut markdown,
            &mut failures,
            &comparison.package,
            MetricKind::CandidatesTotal,
            comparison.baseline.candidates_total,
            comparison.current.candidates_total,
            args.allowed_total_candidate_growth,
        );
        push_metric_row(
            &mut markdown,
            &mut failures,
            &comparison.package,
            MetricKind::DeadLikelyCandidates,
            comparison.baseline.dead_likely_candidates,
            comparison.current.dead_likely_candidates,
            args.allowed_dead_likely_growth,
        );
        push_metric_row(
            &mut markdown,
            &mut failures,
            &comparison.package,
            MetricKind::NeedsTargetedTestCandidates,
            comparison.baseline.needs_targeted_test_candidates,
            comparison.current.needs_targeted_test_candidates,
            args.allowed_needs_targeted_test_growth,
        );
    }
    (markdown, failures)
}

fn compare_workspace_line_coverage(
    baseline_workspace: Option<WorkspaceLineCoverage>,
    current_workspace: Option<WorkspaceLineCoverage>,
    allowed_drop: f64,
) -> Option<String> {
    let Some(baseline) = baseline_workspace else {
        return Some("baseline workspace line coverage metrics are missing".to_string());
    };
    let Some(current) = current_workspace else {
        return Some("current workspace line coverage metrics are missing".to_string());
    };
    let minimum_allowed = baseline.percent - allowed_drop;
    if current.percent < minimum_allowed {
        return Some(format!(
            "workspace line coverage regressed: current={:.2}% < baseline={:.2}% - allowed_drop={:.2}%",
            current.percent, baseline.percent, allowed_drop
        ));
    }
    None
}

fn workspace_coverage_row(
    baseline_workspace: Option<WorkspaceLineCoverage>,
    current_workspace: Option<WorkspaceLineCoverage>,
    allowed_drop: f64,
) -> (String, String, String, &'static str) {
    let Some(baseline) = baseline_workspace else {
        return (
            "missing".to_string(),
            current_workspace
                .map(|current| format!("{:.2}", current.percent))
                .unwrap_or_else(|| "missing".to_string()),
            "n/a".to_string(),
            "FAIL",
        );
    };
    let Some(current) = current_workspace else {
        return (
            format!("{:.2}", baseline.percent),
            "missing".to_string(),
            "n/a".to_string(),
            "FAIL",
        );
    };

    let delta = current.percent - baseline.percent;
    let status = if current.percent >= baseline.percent - allowed_drop {
        "PASS"
    } else {
        "FAIL"
    };
    (
        format!("{:.2}", baseline.percent),
        format!("{:.2}", current.percent),
        format!("{delta:+.2}"),
        status,
    )
}

fn push_metric_row(
    markdown: &mut String,
    failures: &mut Vec<String>,
    package: &str,
    metric: MetricKind,
    baseline: usize,
    current: usize,
    allowance: usize,
) {
    let baseline_u64 = u64::try_from(baseline).unwrap_or(u64::MAX);
    let current_u64 = u64::try_from(current).unwrap_or(u64::MAX);
    let delta_i128 = i128::from(current_u64) - i128::from(baseline_u64);
    let allowed_max = baseline.saturating_add(allowance);
    let passed = current <= allowed_max;
    markdown.push_str(&format!(
        "| `{}` | `{}` | `{}` | `{}` | `{}` | `+{}` | `{}` |\n",
        package,
        metric.label(),
        baseline,
        current,
        delta_i128,
        allowance,
        if passed { "PASS" } else { "FAIL" }
    ));
    if !passed {
        failures.push(format!(
            "{} {} regressed: current={} > baseline={} + allowance={}",
            package,
            metric.label(),
            current,
            baseline,
            allowance
        ));
    }
}

fn unix_timestamp_seconds() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_secs())
        .unwrap_or(0)
}
