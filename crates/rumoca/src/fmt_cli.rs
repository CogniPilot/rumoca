use std::path::{Path, PathBuf};

use anyhow::Result;
use clap::{Args, ValueEnum};
use rumoca_tool_fmt::PartialFormatOptions;

use crate::{collect_modelica_files, normalize_target_paths, validate_explicit_target_paths};

#[derive(Args, Debug)]
pub(crate) struct FmtArgs {
    /// Files or directories to format. If empty, formats current directory.
    #[arg()]
    pub(crate) paths: Vec<PathBuf>,
    /// Check formatting without writing changes.
    #[arg(long, default_value_t = false)]
    pub(crate) check: bool,
    /// Report formatter rule coverage over eligible AST trivia gaps without
    /// writing changes.
    #[arg(long, default_value_t = false)]
    pub(crate) coverage: bool,
    /// Formatting profile. `dymola` preserves MSL/Dymola-compatible local
    /// whitespace; `canonical` enables stricter spacing and indentation
    /// defaults that can still be overridden by the flags below.
    #[arg(long, value_enum)]
    pub(crate) profile: Option<FmtProfileArg>,
    /// Number of spaces per indentation level when indentation normalization is enabled.
    #[arg(long)]
    pub(crate) indent_size: Option<usize>,
    /// Use tabs instead of spaces when indentation normalization is enabled.
    #[arg(long, num_args = 0..=1, default_missing_value = "true", require_equals = true)]
    pub(crate) use_tabs: Option<bool>,
    /// Normalize structural indentation; enabled by the canonical profile.
    #[arg(long, num_args = 0..=1, default_missing_value = "true", require_equals = true)]
    pub(crate) normalize_indentation: Option<bool>,
    /// Add missing structural indentation only to unindented lines.
    #[arg(long, num_args = 0..=1, default_missing_value = "true", require_equals = true)]
    pub(crate) repair_missing_indentation: Option<bool>,
    /// Normalize spaces around declaration bindings and equation/algorithm
    /// assignments; enabled by the canonical profile.
    #[arg(long, num_args = 0..=1, default_missing_value = "true", require_equals = true)]
    pub(crate) normalize_equation_spacing: Option<bool>,
    /// Normalize spaces around binary expression operators; enabled by the
    /// canonical profile.
    #[arg(long, num_args = 0..=1, default_missing_value = "true", require_equals = true)]
    pub(crate) normalize_operator_spacing: Option<bool>,
    /// Normalize named argument and modification assignments to compact `=`;
    /// enabled by the canonical profile.
    #[arg(long, num_args = 0..=1, default_missing_value = "true", require_equals = true)]
    pub(crate) normalize_argument_assignment_spacing: Option<bool>,
    /// Insert a final newline.
    #[arg(long, num_args = 0..=1, default_missing_value = "true", require_equals = true)]
    pub(crate) insert_final_newline: Option<bool>,
    /// Trim trailing horizontal whitespace.
    #[arg(long, num_args = 0..=1, default_missing_value = "true", require_equals = true)]
    pub(crate) trim_trailing_whitespace: Option<bool>,
    /// Output line-ending style.
    #[arg(long, value_enum)]
    pub(crate) line_ending: Option<FmtLineEndingArg>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum FmtProfileArg {
    Dymola,
    Canonical,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum FmtLineEndingArg {
    Auto,
    Lf,
    Crlf,
}

impl From<FmtProfileArg> for rumoca_tool_fmt::FormatProfile {
    fn from(value: FmtProfileArg) -> Self {
        match value {
            FmtProfileArg::Dymola => rumoca_tool_fmt::FormatProfile::Dymola,
            FmtProfileArg::Canonical => rumoca_tool_fmt::FormatProfile::Canonical,
        }
    }
}

impl From<FmtLineEndingArg> for rumoca_tool_fmt::LineEnding {
    fn from(value: FmtLineEndingArg) -> Self {
        match value {
            FmtLineEndingArg::Auto => rumoca_tool_fmt::LineEnding::Auto,
            FmtLineEndingArg::Lf => rumoca_tool_fmt::LineEnding::Lf,
            FmtLineEndingArg::Crlf => rumoca_tool_fmt::LineEnding::Crlf,
        }
    }
}
pub(crate) fn run_fmt(args: FmtArgs) -> Result<()> {
    validate_explicit_target_paths(&args.paths)?;
    let paths = normalize_target_paths(&args.paths);
    let cli_overrides = PartialFormatOptions {
        profile: args.profile.map(Into::into),
        indent_size: args.indent_size,
        use_tabs: args.use_tabs,
        normalize_indentation: args.normalize_indentation,
        repair_missing_indentation: args.repair_missing_indentation,
        normalize_equation_spacing: args.normalize_equation_spacing,
        normalize_operator_spacing: args.normalize_operator_spacing,
        normalize_argument_assignment_spacing: args.normalize_argument_assignment_spacing,
        insert_final_newline: args.insert_final_newline,
        trim_trailing_whitespace: args.trim_trailing_whitespace,
        line_ending: args.line_ending.map(Into::into),
    };

    let files = collect_modelica_files(&paths);
    if files.is_empty() {
        eprintln!("No .mo files found");
        return Ok(());
    }

    let mut needs_formatting = 0usize;
    let mut errors = 0usize;
    let mut coverage = FmtCoverageSummary::default();
    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("Error reading {}: {e}", file.display());
                errors += 1;
                continue;
            }
        };

        let source_name = file.display().to_string();
        let options = fmt_options_for_file(file, &cli_overrides)?;
        if args.coverage {
            match rumoca_tool_fmt::format_coverage_report_with_source_name(
                &source,
                &options,
                &source_name,
            ) {
                Ok(report) => coverage.record(report),
                Err(e) => {
                    eprintln!(
                        "Error measuring formatter coverage for {}: {e}",
                        file.display()
                    );
                    errors += 1;
                }
            }
            continue;
        }

        let formatted =
            match rumoca_tool_fmt::format_with_source_name(&source, &options, &source_name) {
                Ok(v) => v,
                Err(e) => {
                    eprintln!("Error formatting {}: {e}", file.display());
                    errors += 1;
                    continue;
                }
            };
        if formatted == source {
            continue;
        }
        needs_formatting += 1;
        if args.check {
            // The changed-file list is the result data → stdout (matches `lint`,
            // so `rumoca fmt --check . > changes.txt` captures it).
            println!("Would reformat: {}", file.display());
        } else if let Err(e) = std::fs::write(file, formatted) {
            eprintln!("Error writing {}: {e}", file.display());
            errors += 1;
        } else {
            println!("Formatted: {}", file.display());
        }
    }

    let total = files.len();
    let unchanged = total.saturating_sub(needs_formatting + errors);
    if args.coverage {
        coverage.print(total, errors);
        if errors > 0 {
            std::process::exit(1);
        }
    } else if args.check {
        eprintln!(
            "{total} files checked: {unchanged} ok, {needs_formatting} need formatting, {errors} errors"
        );
        if needs_formatting > 0 || errors > 0 {
            std::process::exit(1);
        }
    } else {
        eprintln!(
            "{total} files processed: {unchanged} unchanged, {needs_formatting} formatted, {errors} errors"
        );
        if errors > 0 {
            std::process::exit(1);
        }
    }

    Ok(())
}

#[derive(Default)]
struct FmtCoverageSummary {
    eligible_gaps: usize,
    covered_gaps: usize,
    categories: Vec<FmtCoverageCategorySummary>,
    unclassified_examples: Vec<String>,
}

impl FmtCoverageSummary {
    fn record(&mut self, report: rumoca_tool_fmt::FormatCoverageReport) {
        self.eligible_gaps += report.eligible_gaps;
        self.covered_gaps += report.covered_gaps;
        for example in report.unclassified_examples {
            self.record_unclassified_example(example);
        }
        for category in report.categories {
            self.record_category(category);
        }
    }

    fn record_unclassified_example(&mut self, example: String) {
        if self.unclassified_examples.len() < 8
            && !self
                .unclassified_examples
                .iter()
                .any(|existing| existing == &example)
        {
            self.unclassified_examples.push(example);
        }
    }

    fn record_category(&mut self, category: rumoca_tool_fmt::FormatCoverageCategoryReport) {
        if let Some(entry) = self
            .categories
            .iter_mut()
            .find(|entry| entry.category == category.category)
        {
            entry.eligible_gaps += category.eligible_gaps;
            entry.covered_gaps += category.covered_gaps;
            return;
        }
        self.categories.push(FmtCoverageCategorySummary {
            category: category.category,
            eligible_gaps: category.eligible_gaps,
            covered_gaps: category.covered_gaps,
        });
    }

    fn print(&mut self, total_files: usize, errors: usize) {
        self.categories.sort_by_key(|entry| entry.category.label());
        eprintln!(
            "{total_files} files measured: {} eligible gaps, {} covered, {:.2}% coverage, {errors} errors",
            self.eligible_gaps,
            self.covered_gaps,
            self.coverage_percent(),
        );
        if self.categories.is_empty() {
            return;
        }
        eprintln!("category,eligible,covered,coverage");
        for category in &self.categories {
            eprintln!(
                "{},{},{},{:.2}%",
                category.category.label(),
                category.eligible_gaps,
                category.covered_gaps,
                category.coverage_percent(),
            );
        }
        if !self.unclassified_examples.is_empty() {
            eprintln!("unclassified examples:");
            for example in &self.unclassified_examples {
                eprintln!("  {example}");
            }
        }
    }

    fn coverage_percent(&self) -> f64 {
        coverage_percent(self.covered_gaps, self.eligible_gaps)
    }
}

struct FmtCoverageCategorySummary {
    category: rumoca_tool_fmt::FormatCoverageCategory,
    eligible_gaps: usize,
    covered_gaps: usize,
}

impl FmtCoverageCategorySummary {
    fn coverage_percent(&self) -> f64 {
        coverage_percent(self.covered_gaps, self.eligible_gaps)
    }
}

fn coverage_percent(covered_gaps: usize, eligible_gaps: usize) -> f64 {
    if eligible_gaps == 0 {
        return 100.0;
    }
    covered_gaps as f64 * 100.0 / eligible_gaps as f64
}

fn fmt_options_for_file(
    file: &Path,
    cli_overrides: &PartialFormatOptions,
) -> Result<rumoca_tool_fmt::FormatOptions> {
    let config_dir = file.parent().unwrap_or(Path::new("."));
    let config_overrides = rumoca_tool_fmt::load_config_overrides_from_dir(config_dir)
        .map_err(|e| anyhow::anyhow!("Failed to load formatter config: {e}"))?
        .unwrap_or_default();
    Ok(rumoca_tool_fmt::FormatOptions::from_partial(
        config_overrides.overlay(cli_overrides.clone()),
    ))
}
