//! Rendering of the package pass-rate, trace-accuracy, and MLS-category
//! tables. Each table's columns are one list that renders both the header and
//! the legend under it (`rumoca_test_msl::msl_tools::report_table`), so a
//! column never appears without its definition.

use super::*;
use rumoca_sim::sim_trace_compare::SEVERE_CHANNEL_MAX_THRESHOLD;
use rumoca_test_msl::msl_tools::report_table::{Align, Column, render_markdown_table};

const PACKAGE_LEGEND: &str =
    "the models under `Modelica.<package>.Examples`; Overall is every package";
const N_LEGEND: &str = "models in the row; every percentage is of n";
const TIME_LEGEND: &str =
    "mean seconds in the stage to its left, over the models that passed it or timed out in it";

type RowCount = fn(&MslPackagePassRateRow) -> usize;
type RowSeconds = fn(&MslPackagePassRateRow) -> Option<f64>;

/// One pipeline stage of the pass-rate table.
struct PassRateStage {
    header: &'static str,
    legend: String,
    /// `None` when the run did not measure the stage.
    passed: Option<RowCount>,
    avg_seconds: Option<RowSeconds>,
}

impl PassRateStage {
    fn percent(&self, row: &MslPackagePassRateRow) -> String {
        self.passed.map_or_else(
            || "-".to_string(),
            |passed| percent_cell(passed(row), row.n),
        )
    }

    fn time(&self, row: &MslPackagePassRateRow) -> Option<String> {
        self.avg_seconds.map(|avg| avg_time_cell(avg(row)))
    }
}

fn stage(
    header: &'static str,
    legend: impl Into<String>,
    passed: RowCount,
    avg_seconds: Option<RowSeconds>,
) -> PassRateStage {
    PassRateStage {
        header,
        legend: legend.into(),
        passed: Some(passed),
        avg_seconds,
    }
}

/// The pass-rate stages, in pipeline order, each defined from the predicate
/// that counts it (`add_result_to_pass_rate_counts`).
fn pass_rate_stages(parity_measured: bool) -> Vec<PassRateStage> {
    let high = PassRateStage {
        header: "High",
        legend: format!(
            "at strict-high trace parity with OMC: {}{}",
            high_band_rule(),
            if parity_measured {
                ""
            } else {
                "; `-` because no OMC comparison ran"
            }
        ),
        passed: parity_measured
            .then_some((|row: &MslPackagePassRateRow| row.sim_passed) as RowCount),
        avg_seconds: None,
    };
    vec![
        stage(
            "Ast",
            "parsed; models are drawn from the parsed library, so always 100%",
            |row| row.parse_passed,
            Some(|row| row.parse_avg_seconds),
        ),
        stage(
            "Flat",
            "flattened without error",
            |row| row.flatten_passed,
            Some(|row| row.flatten_avg_seconds),
        ),
        stage(
            "Dae",
            "compiled to a DAE (balance is checked separately)",
            |row| row.dae_passed,
            Some(|row| row.dae_avg_seconds),
        ),
        stage(
            "Solve",
            "lowered to Solve IR",
            |row| row.solve_passed,
            Some(|row| row.solve_avg_seconds),
        ),
        stage(
            "IC",
            "with initial conditions solved; a model whose initial values were compared with OMC \
             must also match them",
            |row| row.ic_passed,
            Some(|row| row.ic_avg_seconds),
        ),
        stage(
            "Simulated",
            "whose simulation ran to completion: a completion count, never a parity rate",
            |row| row.simulated_passed,
            Some(|row| row.sim_avg_seconds),
        ),
        high,
    ]
}

fn high_band_rule() -> String {
    let deviation = if MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE <= 0.0 {
        "no deviation channel".to_string()
    } else {
        format!(
            "at most {:.0}% deviation channels",
            MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE * 100.0
        )
    };
    format!(
        "at least {:.0}% high channels and {deviation}",
        MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE * 100.0
    )
}

fn pass_rate_columns(stages: &[PassRateStage]) -> Vec<Column<'_, MslPackagePassRateRow>> {
    let mut columns = vec![
        Column::new(
            "MSL Package",
            Align::Left,
            PACKAGE_LEGEND,
            |row: &MslPackagePassRateRow| row.package.clone(),
        ),
        Column::new(
            "n",
            Align::Right,
            N_LEGEND,
            |row: &MslPackagePassRateRow| row.n.to_string(),
        ),
    ];
    for stage in stages {
        columns.push(Column::new(
            stage.header,
            Align::Right,
            format!("% of n {}", stage.legend),
            |row: &MslPackagePassRateRow| stage.percent(row),
        ));
        if stage.avg_seconds.is_some() {
            columns.push(Column::new(
                "Time",
                Align::Right,
                TIME_LEGEND,
                |row: &MslPackagePassRateRow| stage.time(row).unwrap_or_default(),
            ));
        }
    }
    columns
}

pub(super) fn format_msl_package_pass_rate_markdown(report: &MslPackagePassRateReport) -> String {
    let stages = pass_rate_stages(report.parity_measured);
    render_markdown_table(
        &pass_rate_columns(&stages),
        report.rows.iter().chain(std::iter::once(&report.overall)),
    )
}

fn avg_time_cell(avg_seconds: Option<f64>) -> String {
    match avg_seconds {
        Some(value) if value.is_finite() => format!("{value:.2}s"),
        _ => "-".to_string(),
    }
}

const PASS_RATE_TERMINAL_MIN_TIME_WIDTH: usize = "10.00s".len();

/// A stage's terminal cell: its percentage, then its time when it has one.
fn terminal_stage_cell(
    percent: &str,
    time: Option<&str>,
    percent_width: usize,
    time_width: usize,
) -> String {
    match time {
        Some(time) => format!("{percent:>percent_width$}  {time:>time_width$}"),
        None => format!("{percent:>percent_width$}"),
    }
}

/// The aligned plain-text pass-rate table printed to the terminal.
pub(super) fn format_msl_package_pass_rate_terminal_table(
    report: &MslPackagePassRateReport,
) -> String {
    let stages = pass_rate_stages(report.parity_measured);
    let rows = report
        .rows
        .iter()
        .chain(std::iter::once(&report.overall))
        .collect::<Vec<_>>();
    let package_width = rows
        .iter()
        .map(|row| row.package.len())
        .chain(std::iter::once("MSL Package".len()))
        .max()
        .unwrap_or_default()
        + 1;
    let count_width = rows
        .iter()
        .map(|row| row.n.to_string().len())
        .chain(std::iter::once(1))
        .max()
        .unwrap_or(1);
    let widths = stages
        .iter()
        .map(|stage| {
            let percent = rows
                .iter()
                .map(|row| stage.percent(row).len())
                .chain(std::iter::once(stage.header.len()))
                .max()
                .unwrap_or_default();
            let time = rows
                .iter()
                .filter_map(|row| stage.time(row).map(|time| time.len()))
                .chain(std::iter::once(PASS_RATE_TERMINAL_MIN_TIME_WIDTH))
                .max()
                .unwrap_or_default();
            (percent, time)
        })
        .collect::<Vec<_>>();
    let line = |package: &str, count: &str, cells: Vec<String>| {
        format!(
            "{package:<package_width$}{count:>count_width$} | {}\n",
            cells.join(" | ")
        )
    };
    let header_cells = stages
        .iter()
        .zip(&widths)
        .map(|(stage, (percent, time))| {
            let label = stage.avg_seconds.map(|_| "Time");
            terminal_stage_cell(stage.header, label, *percent, *time)
        })
        .collect();
    let separator = {
        let cells = stages
            .iter()
            .zip(&widths)
            .map(|(stage, (percent, time))| {
                let width = percent + stage.avg_seconds.map_or(0, |_| 2 + time);
                "-".repeat(width)
            })
            .collect::<Vec<_>>();
        format!(
            "{}-+-{}\n",
            "-".repeat(package_width + count_width),
            cells.join("-+-")
        )
    };
    let row_line = |row: &MslPackagePassRateRow| {
        let cells = stages
            .iter()
            .zip(&widths)
            .map(|(stage, (percent, time))| {
                terminal_stage_cell(
                    &stage.percent(row),
                    stage.time(row).as_deref(),
                    *percent,
                    *time,
                )
            })
            .collect();
        line(&row.package, &row.n.to_string(), cells)
    };
    let mut table = line("MSL Package", "n", header_cells);
    table.push_str(&separator);
    for row in &report.rows {
        table.push_str(&row_line(row));
    }
    table.push_str(&separator);
    table.push_str(&row_line(&report.overall));
    table
}

const PASS_RATE_COMPACT_PACKAGE_WIDTH: usize = 42;
const PASS_RATE_COMPACT_COUNT_WIDTH: usize = 4;
const PASS_RATE_COMPACT_PERCENT_WIDTH: usize = 6;
const PASS_RATE_COMPACT_TIME_WIDTH: usize = 10;

/// The fixed-width pass-rate table, stable across runs for line diffs.
pub(super) fn format_msl_package_pass_rate_compact_table(
    report: &MslPackagePassRateReport,
) -> String {
    let stages = pass_rate_stages(report.parity_measured);
    let line = |package: &str, count: &str, cells: &[(String, Option<String>)], fill: char| {
        let mut text = format!(
            "{package:<PASS_RATE_COMPACT_PACKAGE_WIDTH$} {count:>PASS_RATE_COMPACT_COUNT_WIDTH$}"
        );
        for ((percent, time), stage) in cells.iter().zip(&stages) {
            let width = PASS_RATE_COMPACT_PERCENT_WIDTH.max(stage.header.len());
            text.push(' ');
            text.push_str(&pad_left(percent, width, fill));
            if let Some(time) = time {
                text.push(' ');
                text.push_str(&pad_left(time, PASS_RATE_COMPACT_TIME_WIDTH, fill));
            }
        }
        text.push('\n');
        text
    };
    let header = stages
        .iter()
        .map(|stage| {
            let time = stage.avg_seconds.map(|_| "Time".to_string());
            (stage.header.to_string(), time)
        })
        .collect::<Vec<_>>();
    let blank = stages
        .iter()
        .map(|stage| (String::new(), stage.avg_seconds.map(|_| String::new())))
        .collect::<Vec<_>>();
    let separator = line(
        &"-".repeat(PASS_RATE_COMPACT_PACKAGE_WIDTH),
        &"-".repeat(PASS_RATE_COMPACT_COUNT_WIDTH),
        &blank,
        '-',
    );
    let row_line = |row: &MslPackagePassRateRow| {
        let cells = stages
            .iter()
            .map(|stage| (stage.percent(row), stage.time(row)))
            .collect::<Vec<_>>();
        line(&row.package, &row.n.to_string(), &cells, ' ')
    };
    let mut table = line("MSL Package", "n", &header, ' ');
    table.push_str(&separator);
    for row in &report.rows {
        table.push_str(&row_line(row));
    }
    table.push_str(&separator);
    table.push_str(&row_line(&report.overall));
    table
}

fn pad_left(text: &str, width: usize, fill: char) -> String {
    let padding = width.saturating_sub(text.chars().count());
    std::iter::repeat_n(fill, padding)
        .chain(text.chars())
        .collect()
}

/// Models in the row with at least one severe channel, in any band.
fn severe_models(row: &MslPackageTraceAccuracyRow) -> usize {
    row.compared.saturating_sub(row.no_severe_models)
}

fn trace_accuracy_columns() -> Vec<Column<'static, MslPackageTraceAccuracyRow>> {
    type Row = MslPackageTraceAccuracyRow;
    vec![
        Column::new("MSL Package", Align::Left, PACKAGE_LEGEND, |row: &Row| {
            row.package.clone()
        }),
        Column::new("n", Align::Right, N_LEGEND, |row: &Row| row.n.to_string()),
        Column::new(
            "Compared",
            Align::Right,
            "% of n whose simulation trace was compared with OMC's; High + Near + Deviation = Compared",
            |row: &Row| percent_cell(row.compared, row.n),
        ),
        Column::new(
            "High",
            Align::Right,
            format!("% of n in the strict-high band: {}", high_band_rule()),
            |row: &Row| percent_cell(row.high_agreement, row.n),
        ),
        Column::new(
            "Near",
            Align::Right,
            format!(
                "% of n in the near band: not high, at least {:.0}% high or near channels and at \
                 most {:.0}% deviation channels",
                MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE * 100.0,
                MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE * 100.0
            ),
            |row: &Row| percent_cell(row.near_agreement, row.n),
        ),
        Column::new(
            "Deviation",
            Align::Right,
            "% of n compared but in neither band",
            |row: &Row| percent_cell(row.deviation, row.n),
        ),
        Column::new(
            "Severe",
            Align::Right,
            format!(
                "% of n with at least one channel at bounded normalized L1 error {SEVERE_CHANNEL_MAX_THRESHOLD} \
                 or more, in any band"
            ),
            |row: &Row| percent_cell(severe_models(row), row.n),
        ),
    ]
}

pub(super) fn format_msl_package_trace_accuracy_markdown(
    report: &MslPackageTraceAccuracyReport,
) -> String {
    render_markdown_table(
        &trace_accuracy_columns(),
        report.rows.iter().chain(std::iter::once(&report.overall)),
    )
}

fn mls_contract_coverage_columns() -> Vec<Column<'static, MlsContractCoverageRow>> {
    type Row = MlsContractCoverageRow;
    vec![
        Column::new(
            "MLS Category",
            Align::Left,
            "MLS area assigned from keywords in the model name and its error",
            |row: &Row| row.category.clone(),
        ),
        Column::new("n", Align::Right, N_LEGEND, |row: &Row| {
            row.models.to_string()
        }),
        Column::new(
            "Flat",
            Align::Right,
            "% of n flattened without error",
            |row: &Row| percent_cell(row.compiled, row.models),
        ),
        Column::new(
            "Solve IR",
            Align::Right,
            "% of n lowered to Solve IR",
            |row: &Row| percent_cell(row.solve_ir, row.models),
        ),
        Column::new(
            "Balance",
            Align::Right,
            "% of n whose DAE is balanced",
            |row: &Row| percent_cell(row.balanced, row.models),
        ),
        Column::new(
            "Simulated",
            Align::Right,
            "% of n whose simulation ran to completion, never a parity rate",
            |row: &Row| percent_cell(row.sim_ok, row.models),
        ),
        Column::new(
            "Phases",
            Align::Left,
            "models by last pipeline phase reached (`Success` = compiled to a DAE)",
            |row: &Row| count_map_cell(&row.phase_counts),
        ),
        Column::new(
            "Errors",
            Align::Left,
            "models by error code from compile, Solve lowering, or simulation",
            |row: &Row| count_map_cell(&row.error_code_counts),
        ),
    ]
}

pub(super) fn format_mls_contract_coverage_markdown(report: &MlsContractCoverageReport) -> String {
    render_markdown_table(&mls_contract_coverage_columns(), &report.rows)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every header the three tables render has exactly one non-empty legend
    /// line, so a column added or renamed without a definition fails here.
    #[test]
    fn every_rendered_column_has_one_legend_line() {
        let check = |headers: Vec<&'static str>, legends: Vec<String>, markdown: &str| {
            let header_line = markdown.lines().next().expect("header");
            let missing = headers
                .iter()
                .find(|header| !header_line.contains(**header));
            assert_eq!(missing, None, "header missing in {header_line}");
            let distinct = headers.iter().collect::<std::collections::BTreeSet<_>>();
            assert!(legends.iter().all(|legend| !legend.trim().is_empty()));
            for header in &distinct {
                let prefix = format!("- `{header}`: ");
                assert_eq!(
                    markdown
                        .lines()
                        .filter(|line| line.starts_with(&prefix))
                        .count(),
                    1,
                    "legend for {header} in {markdown}"
                );
            }
            assert_eq!(
                markdown
                    .lines()
                    .filter(|line| line.starts_with("- `"))
                    .count(),
                distinct.len()
            );
        };
        for parity_measured in [false, true] {
            let stages = pass_rate_stages(parity_measured);
            let columns = pass_rate_columns(&stages);
            let report = MslPackagePassRateReport {
                git_commit: String::new(),
                msl_version: String::new(),
                selection_kind: String::new(),
                selection_pattern: String::new(),
                model_count: 0,
                parity_measured,
                rows: Vec::new(),
                overall: pass_rate_row(
                    "Overall".to_string(),
                    MslPackagePassRateCounts::default(),
                    None,
                ),
            };
            check(
                columns.iter().map(|column| column.header).collect(),
                columns.iter().map(|column| column.legend.clone()).collect(),
                &format_msl_package_pass_rate_markdown(&report),
            );
        }
        let columns = trace_accuracy_columns();
        let report = MslPackageTraceAccuracyReport {
            selection_pattern: String::new(),
            model_count: 0,
            rows: Vec::new(),
            overall: trace_accuracy_row(
                "Overall".to_string(),
                MslPackageTraceAccuracyCounts::default(),
            ),
        };
        check(
            columns.iter().map(|column| column.header).collect(),
            columns.iter().map(|column| column.legend.clone()).collect(),
            &format_msl_package_trace_accuracy_markdown(&report),
        );
        let columns = mls_contract_coverage_columns();
        let report = MlsContractCoverageReport {
            git_commit: String::new(),
            msl_version: String::new(),
            selection_kind: String::new(),
            model_count: 0,
            rows: Vec::new(),
        };
        check(
            columns.iter().map(|column| column.header).collect(),
            columns.iter().map(|column| column.legend.clone()).collect(),
            &format_mls_contract_coverage_markdown(&report),
        );
    }
}
