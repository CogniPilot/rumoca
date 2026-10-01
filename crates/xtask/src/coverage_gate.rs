//! `cargo xtask coverage gate`: the coverage checks CI enforces after
//! `coverage report`.
//!
//! Two checks fail the gate. Every function a change adds (its first line lies
//! in a hunk that `git diff -U0 <base>...HEAD` adds) must execute at least once
//! under the workspace tests; closures are exempt, since an error path's
//! `.with_context(|| ...)` is a closure only a failure runs, and so are
//! functions carrying the one reviewed exemption (see `EXEMPTION_ATTRIBUTE`),
//! which the report lists for the reviewer. And workspace line
//! coverage may not drop more than the allowed margin below the committed
//! baseline. The per-package zero-execution counts are reported for
//! information only: they drift between runs and say nothing about the change
//! under review.

use anyhow::{Context, Result, bail, ensure};
use clap::Args as ClapArgs;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const DEFAULT_BASELINE_FILE_REL: &str = "crates/xtask/coverage/line-coverage-baseline.json";
const DEFAULT_CANDIDATES_FILE_REL: &str = "target/llvm-cov/trim-candidates.json";
const DEFAULT_REPORT_FILE_REL: &str = "target/llvm-cov/coverage-gate.md";
const DEFAULT_SUMMARY_FILE_NAME: &str = "workspace-summary.json";
const GENERATED_BY: &str = "cargo xtask coverage gate";

#[derive(Debug, Clone, ClapArgs)]
pub(crate) struct CoverageGateArgs {
    /// Trim candidates JSON from `coverage report` (default: target/llvm-cov/trim-candidates.json)
    #[arg(long)]
    candidates_file: Option<PathBuf>,
    /// Committed line-coverage baseline (default: crates/xtask/coverage/line-coverage-baseline.json)
    #[arg(long)]
    baseline_file: Option<PathBuf>,
    /// Markdown report output path (default: target/llvm-cov/coverage-gate.md)
    #[arg(long)]
    report_file: Option<PathBuf>,
    /// Base revision of the change: every function `git diff <rev>...HEAD` adds must execute.
    #[arg(
        long,
        conflicts_with = "changed_diff",
        required_unless_present_any = ["changed_diff", "promote_baseline"]
    )]
    changed_since: Option<String>,
    /// A `git diff -U0` of the change, for a tree without history (a `verify gate` snapshot).
    #[arg(long)]
    changed_diff: Option<PathBuf>,
    /// Allowed drop in workspace line coverage percentage from baseline.
    #[arg(long, default_value_t = 0.25)]
    allowed_workspace_line_coverage_drop: f64,
    /// Write the current workspace line coverage to the baseline instead of gating.
    #[arg(long, conflicts_with_all = ["changed_since", "changed_diff"])]
    promote_baseline: bool,
}

/// The committed workspace line-coverage bar.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct LineCoverageBaseline {
    generated_by: String,
    generated_at_unix_secs: u64,
    source_candidates_file: String,
    workspace_line_coverage_percent: f64,
    workspace_lines_covered: u64,
    workspace_lines_total: u64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct WorkspaceLineCoverage {
    percent: f64,
    covered: u64,
    total: u64,
}

/// One function the workspace tests never execute, from the trim candidates.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ZeroExecutionFunction {
    file: String,
    line: u64,
    name: String,
    package: String,
}

/// The lines a change adds, per repository-relative file, as inclusive
/// `(first, last)` ranges.
type AddedLines = BTreeMap<String, Vec<(u64, u64)>>;

/// The one coverage exemption attribute (SPEC_0025 §4), whitespace removed:
/// an exempt function has no coverage record, so the gate never sees it, and
/// the report lists every exemption a change adds.
const EXEMPTION_ATTRIBUTE: &str = "#[cfg_attr(coverage_nightly,coverage(off))]";

/// Whether an added source line is the exemption attribute itself. Only a
/// line that starts with the attribute counts, so a mention in a comment, a
/// string literal, or a test fixture's text is never one.
fn is_exemption_attribute(text: &str) -> bool {
    let compact = text
        .chars()
        .filter(|ch| !ch.is_whitespace())
        .collect::<String>();
    compact.starts_with(EXEMPTION_ATTRIBUTE)
}

pub(crate) fn run(root: &Path, args: &CoverageGateArgs) -> Result<()> {
    let candidates_path = resolve_path(
        root,
        args.candidates_file.as_ref(),
        DEFAULT_CANDIDATES_FILE_REL,
    );
    ensure!(
        candidates_path.is_file(),
        "missing trim candidates JSON '{}'; run `cargo xtask coverage report` first",
        candidates_path.display()
    );
    let baseline_path = resolve_path(root, args.baseline_file.as_ref(), DEFAULT_BASELINE_FILE_REL);
    let current_workspace = load_workspace_line_coverage(&candidates_path);

    if args.promote_baseline {
        let Some(current) = current_workspace else {
            bail!(
                "cannot promote baseline: no workspace line coverage beside '{}'",
                candidates_path.display()
            );
        };
        promote_baseline(&baseline_path, &candidates_path, current)?;
        println!("Coverage baseline updated: {}", baseline_path.display());
        return Ok(());
    }

    let baseline = load_baseline(&baseline_path)?;
    let baseline_workspace = WorkspaceLineCoverage {
        percent: baseline.workspace_line_coverage_percent,
        covered: baseline.workspace_lines_covered,
        total: baseline.workspace_lines_total,
    };
    let functions = load_zero_execution_functions(&candidates_path)?;
    let (diff, change) = changed_code(root, args)?;
    let changed = parse_changed_code(&diff);
    let untested = new_functions_without_executions(&functions, &changed.added);
    let allowed_drop = args.allowed_workspace_line_coverage_drop;
    let line_failure =
        compare_workspace_line_coverage(baseline_workspace, current_workspace, allowed_drop);

    let report_path = resolve_path(root, args.report_file.as_ref(), DEFAULT_REPORT_FILE_REL);
    let mut report = format!(
        "# Coverage Gate\n\n- baseline: `{}`\n- current: `{}`\n- change: `{change}`\n\n",
        baseline_path.display(),
        candidates_path.display()
    );
    report.push_str(&render_line_coverage(
        baseline_workspace,
        current_workspace,
        allowed_drop,
    ));
    report.push_str(&render_new_functions(&untested));
    report.push_str(&render_exemptions(&changed.exemptions));
    report.push_str(&render_package_counts(&functions));
    write_text_file(&report_path, &report)?;
    for exemption in &changed.exemptions {
        println!("Coverage gate: the change adds a coverage exemption at {exemption}");
    }
    gate_verdict(&report_path, &untested, line_failure)
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

/// The change's unified diff and a label naming where it came from.
fn changed_code(root: &Path, args: &CoverageGateArgs) -> Result<(String, String)> {
    if let Some(diff_file) = &args.changed_diff {
        let path = resolve_path(root, Some(diff_file), "");
        let diff = fs::read_to_string(&path)
            .with_context(|| format!("failed to read changed-code diff '{}'", path.display()))?;
        return Ok((diff, path.display().to_string()));
    }
    let Some(base) = args.changed_since.as_deref() else {
        bail!("the coverage gate needs --changed-since <rev> or --changed-diff <file>");
    };
    Ok((
        changed_code_diff(root, base, "HEAD")?,
        format!("{base}...HEAD"),
    ))
}

/// `git diff -U0 <base>...<head>` of the Rust sources in `repo`: the changes
/// `head` makes since its merge base with `base`.
pub(crate) fn changed_code_diff(repo: &Path, base: &str, head: &str) -> Result<String> {
    let range = format!("{base}...{head}");
    let output = Command::new("git")
        .arg("-C")
        .arg(repo)
        .args(["-c", "core.quotePath=false", "diff", "-U0", "--no-color"])
        .args(["--no-ext-diff", &range, "--", "*.rs"])
        .output()
        .context("failed to run git diff")?;
    ensure!(
        output.status.success(),
        "git diff {range} failed in '{}': {}",
        repo.display(),
        String::from_utf8_lossy(&output.stderr).trim()
    );
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

/// What a change adds: the new-side line ranges of each file's hunks, and the
/// `file:line` of every added line carrying the coverage exemption.
#[derive(Debug, Default, PartialEq)]
struct ChangedCode {
    added: AddedLines,
    exemptions: Vec<String>,
}

/// Parse a `git diff -U0`. File headers are read only between `diff --git` and
/// the first hunk, so an added line whose text starts with `++ b/` is never
/// taken for one.
fn parse_changed_code(diff: &str) -> ChangedCode {
    let mut change = ChangedCode::default();
    let mut file: Option<String> = None;
    let mut in_header = false;
    let mut next_line = 0;
    for line in diff.lines() {
        if line.starts_with("diff --git ") {
            in_header = true;
            file = None;
        } else if in_header && let Some(path) = line.strip_prefix("+++ ") {
            // A deleted file's new side is `/dev/null`: it adds nothing.
            file = path.strip_prefix("b/").map(str::to_string);
        } else if let Some(header) = line.strip_prefix("@@ ") {
            in_header = false;
            if let (Some(file), Some(range)) = (&file, added_range(header)) {
                change.added.entry(file.clone()).or_default().push(range);
                next_line = range.0;
            }
        } else if let (Some(file), Some(text)) = (&file, line.strip_prefix('+')) {
            if is_exemption_attribute(text) {
                change.exemptions.push(format!("{file}:{next_line}"));
            }
            next_line += 1;
        }
    }
    change
}

/// The inclusive new-side range of a hunk header `-a,b +c,d @@ ...`, or `None`
/// when the hunk only deletes.
fn added_range(header: &str) -> Option<(u64, u64)> {
    let new_side = header
        .split_whitespace()
        .find_map(|field| field.strip_prefix('+'))?;
    let (start, count) = match new_side.split_once(',') {
        Some((start, count)) => (start.parse::<u64>().ok()?, count.parse::<u64>().ok()?),
        None => (new_side.parse::<u64>().ok()?, 1),
    };
    let last = (start + count).checked_sub(1)?;
    (count > 0).then_some((start, last))
}

/// A closure body (`{closure#N}` in v0 demangling, `{{closure}}` in legacy).
fn is_closure(name: &str) -> bool {
    name.contains("{closure") || name.contains("{{closure}}")
}

/// The non-closure zero-execution functions whose first line the change adds,
/// ordered by file and line.
fn new_functions_without_executions<'a>(
    functions: &'a [ZeroExecutionFunction],
    added: &AddedLines,
) -> Vec<&'a ZeroExecutionFunction> {
    functions
        .iter()
        .filter(|function| !is_closure(&function.name))
        .filter(|function| {
            added.get(&function.file).is_some_and(|ranges| {
                ranges
                    .iter()
                    .any(|&(first, last)| (first..=last).contains(&function.line))
            })
        })
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

fn load_zero_execution_functions(path: &Path) -> Result<Vec<ZeroExecutionFunction>> {
    let payload = read_json(path)?;
    let Some(candidates) = payload.get("candidates").and_then(Value::as_array) else {
        bail!(
            "trim candidates payload '{}' has no candidates array",
            path.display()
        );
    };
    let functions = candidates
        .iter()
        .map(zero_execution_function)
        .collect::<Option<Vec<_>>>();
    let Some(functions) = functions else {
        bail!(
            "trim candidates payload '{}' has a candidate without file, line, demangled_function, and package",
            path.display()
        );
    };
    Ok(functions)
}

fn zero_execution_function(candidate: &Value) -> Option<ZeroExecutionFunction> {
    let text = |key: &str| {
        candidate
            .get(key)
            .and_then(Value::as_str)
            .map(str::to_string)
    };
    Some(ZeroExecutionFunction {
        file: text("file")?,
        line: candidate.get("line").and_then(Value::as_u64)?,
        name: text("demangled_function")?,
        package: text("package")?,
    })
}

fn read_json(path: &Path) -> Result<Value> {
    let raw = fs::read_to_string(path)
        .with_context(|| format!("failed to read JSON '{}'", path.display()))?;
    serde_json::from_str(&raw).with_context(|| format!("failed to parse JSON '{}'", path.display()))
}

fn promote_baseline(
    baseline_path: &Path,
    candidates_path: &Path,
    current: WorkspaceLineCoverage,
) -> Result<()> {
    let baseline = LineCoverageBaseline {
        generated_by: GENERATED_BY.to_string(),
        generated_at_unix_secs: unix_timestamp_seconds(),
        source_candidates_file: path_metadata_string(candidates_path),
        workspace_line_coverage_percent: current.percent,
        workspace_lines_covered: current.covered,
        workspace_lines_total: current.total,
    };
    let payload =
        serde_json::to_string_pretty(&baseline).context("failed to serialize baseline JSON")?;
    write_text_file(baseline_path, &format!("{payload}\n"))
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

fn load_baseline(path: &Path) -> Result<LineCoverageBaseline> {
    let raw = fs::read_to_string(path).with_context(|| {
        format!(
            "failed to read coverage baseline '{}'; run `cargo xtask coverage gate --promote-baseline`",
            path.display()
        )
    })?;
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

fn compare_workspace_line_coverage(
    baseline: WorkspaceLineCoverage,
    current_workspace: Option<WorkspaceLineCoverage>,
    allowed_drop: f64,
) -> Option<String> {
    let Some(current) = current_workspace else {
        return Some("current workspace line coverage metrics are missing".to_string());
    };
    if current.percent < baseline.percent - allowed_drop {
        return Some(format!(
            "workspace line coverage regressed: current={:.2}% < baseline={:.2}% - allowed_drop={:.2}%",
            current.percent, baseline.percent, allowed_drop
        ));
    }
    None
}

fn render_line_coverage(
    baseline: WorkspaceLineCoverage,
    current_workspace: Option<WorkspaceLineCoverage>,
    allowed_drop: f64,
) -> String {
    let (current, delta) = match current_workspace {
        Some(current) => (
            format!("{:.2}", current.percent),
            format!("{:+.2}", current.percent - baseline.percent),
        ),
        None => ("missing".to_string(), "n/a".to_string()),
    };
    let failed = compare_workspace_line_coverage(baseline, current_workspace, allowed_drop);
    format!(
        "## Workspace line coverage\n\n\
         | baseline (%) | current (%) | delta | allowed drop | status |\n\
         | ---: | ---: | ---: | ---: | --- |\n\
         | {:.2} | {current} | {delta} | `-{allowed_drop:.2}` | {} |\n\n",
        baseline.percent,
        if failed.is_some() { "FAIL" } else { "PASS" }
    )
}

fn render_new_functions(untested: &[&ZeroExecutionFunction]) -> String {
    let mut markdown = String::from(
        "## New functions without executions\n\n\
         Functions the change adds that no workspace test executes (closures exempt).\n\n\
         | file:line | function |\n| --- | --- |\n",
    );
    if untested.is_empty() {
        markdown.push_str("| _none_ | - |\n");
    }
    for function in untested {
        markdown.push_str(&format!(
            "| `{}:{}` | `{}` |\n",
            function.file, function.line, function.name
        ));
    }
    markdown.push('\n');
    markdown
}

fn render_exemptions(exemptions: &[String]) -> String {
    let mut markdown = String::from(
        "## Coverage exemptions the change adds\n\n\
         Each needs a comment naming the effect no test can drive (SPEC_0025 §4).\n\n",
    );
    if exemptions.is_empty() {
        markdown.push_str("- _none_\n");
    }
    for exemption in exemptions {
        markdown.push_str(&format!("- `{exemption}`\n"));
    }
    markdown.push('\n');
    markdown
}

/// Per-package zero-execution counts, informational only.
fn render_package_counts(functions: &[ZeroExecutionFunction]) -> String {
    let mut counts = BTreeMap::<&str, (usize, usize)>::new();
    for function in functions {
        let entry = counts.entry(function.package.as_str()).or_default();
        if is_closure(&function.name) {
            entry.1 += 1;
        } else {
            entry.0 += 1;
        }
    }
    let mut markdown = String::from(
        "## Zero-execution functions per package (informational)\n\n\
         | package | functions | closures |\n| --- | ---: | ---: |\n",
    );
    for (package, (named, closures)) in counts {
        markdown.push_str(&format!("| `{package}` | {named} | {closures} |\n"));
    }
    markdown
}

fn gate_verdict(
    report_path: &Path,
    untested: &[&ZeroExecutionFunction],
    line_failure: Option<String>,
) -> Result<()> {
    if untested.is_empty() && line_failure.is_none() {
        println!("Coverage gate: PASS. Report: {}", report_path.display());
        return Ok(());
    }
    let mut message = format!("coverage gate failed; report: {}\n", report_path.display());
    if let Some(line_failure) = line_failure {
        message.push_str(&format!("- {line_failure}\n"));
    }
    if !untested.is_empty() {
        message.push_str(&format!(
            "- {} new function(s) never execute under the workspace tests; add a test that runs each:\n",
            untested.len()
        ));
    }
    for function in untested {
        message.push_str(&format!(
            "  - {}:{} {}\n",
            function.file, function.line, function.name
        ));
    }
    bail!(message.trim_end().to_string());
}

fn unix_timestamp_seconds() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_secs())
        .unwrap_or(0)
}

#[cfg(test)]
mod tests;
