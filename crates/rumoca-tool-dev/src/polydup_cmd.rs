use anyhow::{Context, Result, bail};
use clap::Parser;
use dupe_core::{CloneType, Report, Scanner};
use std::path::{Path, PathBuf};

#[derive(Debug, Parser, Clone)]
pub(crate) struct DupesArgs {
    /// Paths to scan (relative to workspace root). Defaults to `crates`.
    #[arg(value_name = "PATH", num_args = 0..)]
    pub(crate) paths: Vec<PathBuf>,
    /// Minimum duplicate block size in normalized tokens.
    #[arg(long, default_value_t = 50)]
    pub(crate) min_tokens: usize,
    /// Similarity threshold (0.0 - 1.0).
    #[arg(long, default_value_t = 0.85)]
    pub(crate) similarity: f64,
    /// Include test files (*.test.*, *.spec.*, etc.) in scanning.
    #[arg(long)]
    pub(crate) include_tests: bool,
    /// Additional glob exclude patterns (repeatable).
    #[arg(long = "exclude")]
    pub(crate) exclude_patterns: Vec<String>,
    /// Enable Type-3 (near-miss) clone detection.
    #[arg(long)]
    pub(crate) type3: bool,
    /// Type-3 tolerance (0.0 - 1.0).
    #[arg(long, default_value_t = 0.85)]
    pub(crate) type3_tolerance: f64,
    /// Print full report as JSON.
    #[arg(long)]
    pub(crate) json: bool,
    /// Maximum number of duplicate entries to print in text mode.
    #[arg(long, default_value_t = 20)]
    pub(crate) limit: usize,
    /// Exit non-zero when duplicates are found.
    #[arg(long)]
    pub(crate) fail_on_duplicates: bool,
}

pub(crate) fn run(args: DupesArgs, root: &Path) -> Result<()> {
    let scan_paths = resolved_scan_paths(&args, root);

    let mut scanner = Scanner::with_config(args.min_tokens, args.similarity)
        .context("invalid polydup scanner configuration")?
        .with_test_files(args.include_tests);

    if !args.exclude_patterns.is_empty() {
        scanner = scanner.with_exclude_patterns(args.exclude_patterns.clone());
    }

    if args.type3 {
        scanner = scanner
            .with_type3_detection(args.type3_tolerance)
            .context("invalid type-3 tolerance for polydup")?;
    }

    let report = scanner
        .scan(scan_paths)
        .context("polydup scan failed while scanning workspace paths")?;

    if args.json {
        println!(
            "{}",
            serde_json::to_string_pretty(&report).context("failed to serialize polydup report")?
        );
    } else {
        print_report_summary(&report, args.limit);
    }

    if args.fail_on_duplicates && !report.duplicates.is_empty() {
        bail!("duplicate scan found {} matches", report.duplicates.len());
    }

    Ok(())
}

fn resolved_scan_paths(args: &DupesArgs, root: &Path) -> Vec<PathBuf> {
    if args.paths.is_empty() {
        return vec![root.join("crates")];
    }
    args.paths
        .iter()
        .map(|path| {
            if path.is_absolute() {
                path.clone()
            } else {
                root.join(path)
            }
        })
        .collect()
}

fn print_report_summary(report: &Report, limit: usize) {
    println!(
        "polydup: files_scanned={} functions_analyzed={} duplicates={} skipped={}",
        report.files_scanned,
        report.functions_analyzed,
        report.duplicates.len(),
        report.skipped_files.len()
    );

    if report.duplicates.is_empty() {
        println!("No duplicate code matches found.");
        return;
    }

    println!("Top duplicate matches:");
    for (idx, dup) in report.duplicates.iter().take(limit).enumerate() {
        println!(
            "{:>3}. {}:{} <-> {}:{}  len={}  sim={:.3}  type={}",
            idx + 1,
            dup.file1,
            dup.start_line1,
            dup.file2,
            dup.start_line2,
            dup.length,
            dup.similarity,
            clone_type_label(&dup.clone_type)
        );
    }

    if report.duplicates.len() > limit {
        println!(
            "... {} more matches omitted (use --limit or --json).",
            report.duplicates.len() - limit
        );
    }
}

fn clone_type_label(clone_type: &CloneType) -> &'static str {
    match clone_type {
        CloneType::Type1 => "type-1",
        CloneType::Type2 => "type-2",
        CloneType::Type3 => "type-3",
    }
}
