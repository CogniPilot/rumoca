use std::fs;
use std::num::NonZeroUsize;
use std::path::Path;

use anyhow::{Context as _, Result};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use xtask::golden_registry::{
    CandidateOnlyAggregateReport, parse_candidate_only_aggregate_denominator,
};

use super::CoverageFootprint;

pub(super) fn percentage(numerator: usize, denominator: usize) -> f64 {
    if denominator == 0 {
        0.0
    } else {
        100.0 * numerator as f64 / denominator as f64
    }
}

pub(super) fn read_previous_denominator(path: &Path) -> Result<Option<NonZeroUsize>> {
    let source = match fs::read_to_string(path) {
        Ok(source) => source,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => {
            return Err(error).with_context(|| format!("failed to read {}", path.display()));
        }
    };
    parse_candidate_only_aggregate_denominator(&source, &path.display().to_string()).map(Some)
}

pub(super) fn write_footprint(output_dir: &Path, footprint: &CoverageFootprint) -> Result<()> {
    fs::create_dir_all(output_dir)?;
    write_json(&output_dir.join("footprint.json"), footprint)?;
    let mut markdown = format!(
        "# Golden coverage review: {}\n\nCapture claim: `{:?}` (raw candidate review input; candidate coordinates never enter the official union, and reviewed admission is not implemented).\n\n\
         ## Coverage\n\n\
         **Workspace share: {} / {} instrumentable production lines ({:.2}%).** This is the golden-coverage number: the fraction of the whole workspace this model executes, measured against a separate instrumented build of every production line.\n\n\
         Linked density: {} / {} lines ({:.2}%) in the {} files this model links. This is *not* a workspace share. Files the model never reaches are absent from its denominator, so this ratio rises as a model touches fewer files and must never be reported as coverage of the compiler.\n\n\
         - Isolated scenarios: {}\n- Covered production lines: {}\n- Unattributable macro/generic lines excluded from the candidate's attributed count: {}\n- Macro-attributed covered lines: {}\n\n",
        footprint.model,
        footprint.capture_claim,
        footprint
            .summary
            .attributed_covered_production_lines
            .unwrap_or_default(),
        footprint
            .summary
            .workspace_instrumentable_production_lines
            .unwrap_or_default(),
        footprint
            .summary
            .workspace_line_coverage_percent
            .unwrap_or_default(),
        footprint.summary.covered_production_lines,
        footprint.summary.linked_instrumentable_production_lines,
        footprint.summary.linked_line_coverage_percent,
        footprint.files.len(),
        footprint.scenarios.len(),
        footprint.summary.covered_production_lines,
        footprint.summary.unattributable_covered_lines,
        footprint.summary.macro_attributed_covered_lines,
    );
    for file in &footprint.files {
        if file.covered_lines.is_empty() {
            continue;
        }
        markdown.push_str(&format!(
            "## `{}`\n\n- SHA-256: `{}`\n- Covered: `{}`\n",
            file.path,
            file.source_sha256,
            format_ranges(&file.covered_lines)?
        ));
        if !file.macro_attributed_covered_lines.is_empty() {
            markdown.push_str(&format!(
                "- Macro-attributed: `{}`\n- Executed macro function identities: `{}`\n",
                format_ranges(&file.macro_attributed_covered_lines)?,
                file.macro_attributed_function_identities.join("`, `")
            ));
        }
        if !file.unattributable_covered_lines.is_empty() {
            markdown.push_str(&format!(
                "- Excluded as unattributable: `{}`\n",
                format_ranges(&file.unattributable_covered_lines)?
            ));
        }
        markdown.push('\n');
    }
    markdown.push_str("## Exact isolated scenarios\n\n");
    for scenario in &footprint.scenarios {
        markdown.push_str(&format!(
            "- `{}`: `{}` / `{}` / `{}` : {} covered lines\n",
            scenario.scenario_id,
            scenario.exact_test.package,
            scenario.exact_test.test_target,
            scenario.exact_test.test_name,
            scenario.summary.covered_production_lines,
        ));
    }
    fs::write(output_dir.join("review.md"), markdown)?;
    Ok(())
}

pub(super) fn write_aggregate_markdown(
    path: &Path,
    report: &CandidateOnlyAggregateReport,
) -> Result<()> {
    let markdown = format!(
        "# Aggregate golden coverage\n\n**{}/{} production lines ({:.2}%)**\n\n- Admitted models: {}\n- Previous denominator: `{}`\n- Denominator delta: `{}`\n\n| Model | Covered lines | Marginal lines |\n|---|---:|---:|\n",
        report.union_covered_lines(),
        report.workspace_instrumentable_production_lines(),
        report.golden_line_coverage_percent(),
        report.admitted_model_count(),
        report
            .previous_workspace_instrumentable_production_lines()
            .map_or_else(|| "unavailable".to_string(), |value| value.to_string()),
        report
            .denominator_line_delta()
            .map_or_else(|| "unavailable".to_string(), |value| format!("{value:+}")),
    );
    fs::write(path, markdown)?;
    Ok(())
}

pub(super) fn format_ranges(lines: &[u32]) -> Result<String> {
    Ok(compact_line_ranges(lines)?.join(", "))
}

pub(super) fn serialize_line_ranges<S>(
    lines: &[u32],
    serializer: S,
) -> std::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    compact_line_ranges(lines)
        .map_err(serde::ser::Error::custom)?
        .serialize(serializer)
}

pub(super) fn deserialize_line_ranges<'de, D>(
    deserializer: D,
) -> std::result::Result<Vec<u32>, D::Error>
where
    D: Deserializer<'de>,
{
    let ranges = Vec::<String>::deserialize(deserializer)?;
    expand_line_ranges(&ranges).map_err(serde::de::Error::custom)
}

fn expand_line_ranges(ranges: &[String]) -> Result<Vec<u32>> {
    let mut lines = Vec::new();
    for range in ranges {
        let (start, end) = match range.split_once('-') {
            Some((start, end)) => (parse_line(start)?, parse_line(end)?),
            None => {
                let line = parse_line(range)?;
                (line, line)
            }
        };
        anyhow::ensure!(start <= end, "descending line range `{range}`");
        lines.extend(start..=end);
    }
    anyhow::ensure!(
        lines.windows(2).all(|pair| pair[0] < pair[1]),
        "line ranges must be strictly increasing and non-overlapping"
    );
    Ok(lines)
}

fn parse_line(value: &str) -> Result<u32> {
    let line = value.parse::<u32>()?;
    anyhow::ensure!(line > 0, "source line zero is invalid");
    Ok(line)
}

fn compact_line_ranges(lines: &[u32]) -> Result<Vec<String>> {
    let Some((&first, rest)) = lines.split_first() else {
        return Ok(Vec::new());
    };
    let mut ranges = Vec::new();
    let mut start = first;
    let mut end = first;
    for &line in rest {
        anyhow::ensure!(line > end, "line coordinates must be strictly increasing");
        if end.checked_add(1) == Some(line) {
            end = line;
        } else {
            ranges.push(format_range(start, end));
            start = line;
            end = line;
        }
    }
    ranges.push(format_range(start, end));
    Ok(ranges)
}

fn format_range(start: u32, end: u32) -> String {
    if start == end {
        start.to_string()
    } else {
        format!("{start}-{end}")
    }
}

pub(super) fn write_json(path: &Path, value: &impl Serialize) -> Result<()> {
    let mut bytes = serde_json::to_vec_pretty(value)?;
    bytes.push(b'\n');
    fs::write(path, bytes).with_context(|| format!("failed to write {}", path.display()))
}
