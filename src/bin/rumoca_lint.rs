//! # Rumoca Modelica Linter
//!
//! A command-line tool for checking Modelica code for common issues,
//! similar to `clippy` for Rust.
//!
//! ## Configuration
//!
//! The linter looks for config files in the following order:
//! 1. `.rumoca_lint.toml` in the file's directory or any parent directory
//! 2. `rumoca_lint.toml` in the file's directory or any parent directory
//!
//! Example config file:
//! ```toml
//! min_level = "warning"
//! disabled_rules = ["magic-number", "missing-documentation"]
//! deny_warnings = false
//! ```
//!
//! CLI options override config file settings.
//!
//! ## Usage
//! ```sh
//! # Lint all .mo files in current directory (recursively)
//! rumoca-lint
//!
//! # Lint specific files
//! rumoca-lint file1.mo file2.mo
//!
//! # Lint with specific warning level (overrides config file)
//! rumoca-lint --level warning
//!
//! # Output as JSON
//! rumoca-lint --format json
//!
//! # List available rules
//! rumoca-lint --list-rules
//! ```

use anyhow::{Context, Result};
use clap::{Parser, ValueEnum};
use rumoca::LINT_CONFIG_FILE_NAMES;
use rumoca::lint::{LINT_RULES, LintConfig, LintLevel, LintResult, lint_file};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq)]
enum OutputFormat {
    Text,
    Json,
    Compact,
}

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq)]
enum MinLevel {
    Help,
    Note,
    Warning,
    Error,
}

impl From<MinLevel> for LintLevel {
    fn from(level: MinLevel) -> Self {
        match level {
            MinLevel::Help => LintLevel::Help,
            MinLevel::Note => LintLevel::Note,
            MinLevel::Warning => LintLevel::Warning,
            MinLevel::Error => LintLevel::Error,
        }
    }
}

#[derive(Parser, Debug)]
#[command(
    name = "rumoca-lint",
    version,
    about = "Lint Modelica code for common issues",
    long_about = "A linter for Modelica source files, similar to clippy for Rust."
)]
struct Args {
    /// Files or directories to lint
    #[arg(name = "FILES")]
    files: Vec<PathBuf>,

    /// Minimum severity level to report
    #[arg(short = 'l', long = "level", value_enum, default_value = "help")]
    level: MinLevel,

    /// Output format
    #[arg(short = 'f', long = "format", value_enum, default_value = "text")]
    format: OutputFormat,

    /// Disable specific rules (comma-separated)
    #[arg(short = 'D', long = "disable", value_delimiter = ',')]
    disable: Vec<String>,

    /// Enable only specific rules (comma-separated)
    #[arg(short = 'E', long = "enable", value_delimiter = ',')]
    enable: Vec<String>,

    /// List all available lint rules
    #[arg(long = "list-rules")]
    list_rules: bool,

    /// Print verbose output
    #[arg(short = 'v', long)]
    verbose: bool,

    /// Print less output
    #[arg(short = 'q', long)]
    quiet: bool,

    /// Exit with error code if any warnings are found
    #[arg(long = "deny-warnings")]
    deny_warnings: bool,

    /// Recursively lint directories
    #[arg(long, default_value = "true")]
    recursive: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    if args.list_rules {
        println!("Available lint rules:\n");
        for (name, description, default_level) in LINT_RULES {
            println!("  {:24} [{}] {}", name, default_level, description);
        }
        return Ok(());
    }

    let files = collect_files(&args)?;

    // Determine start directory for config file search
    let start_dir = if !files.is_empty() {
        files[0].clone()
    } else {
        std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
    };

    // Load config from file or use defaults
    let config = load_lint_config(&start_dir, &args);

    if files.is_empty() {
        if !args.quiet {
            eprintln!("No Modelica files found");
        }
        return Ok(());
    }

    let mut all_results = Vec::new();
    for path in &files {
        let result = lint_file(path, &config);
        all_results.push(result);
    }

    match args.format {
        OutputFormat::Text => output_text(&all_results, &args, &config),
        OutputFormat::Json => output_json(&all_results)?,
        OutputFormat::Compact => output_compact(&all_results, &config),
    }

    let total_errors: usize = all_results
        .iter()
        .map(|r| r.count_by_level(LintLevel::Error))
        .sum();
    let total_warnings: usize = all_results
        .iter()
        .map(|r| r.count_by_level(LintLevel::Warning))
        .sum();

    if total_errors > 0 || (config.deny_warnings && total_warnings > 0) {
        std::process::exit(1);
    }

    Ok(())
}

/// Load lint config, merging config file and CLI options
fn load_lint_config(start_dir: &Path, args: &Args) -> LintConfig {
    // Start with file-based config or defaults
    let mut config = if let Some(file_config) = LintConfig::from_config_file(start_dir) {
        if args.verbose {
            // Find which config file was used
            let mut current = start_dir.to_path_buf();
            if current.is_file()
                && let Some(parent) = current.parent()
            {
                current = parent.to_path_buf();
            }
            'outer: loop {
                for config_name in LINT_CONFIG_FILE_NAMES {
                    let config_path = current.join(config_name);
                    if config_path.exists() {
                        eprintln!("Using config: {}", config_path.display());
                        break 'outer;
                    }
                }
                if let Some(parent) = current.parent() {
                    current = parent.to_path_buf();
                } else {
                    break;
                }
            }
        }
        file_config
    } else {
        LintConfig::default()
    };

    // Determine CLI options to merge
    // Only override min_level if it's not the default (help)
    let cli_min_level = if args.level != MinLevel::Help {
        Some(args.level.into())
    } else {
        None
    };

    let cli_deny_warnings = if args.deny_warnings { Some(true) } else { None };

    // Merge CLI options (they take precedence)
    config.merge_cli_options(
        cli_min_level,
        &args.disable,
        &args.enable,
        cli_deny_warnings,
    );

    config
}

fn collect_files(args: &Args) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    let paths = if args.files.is_empty() {
        vec![PathBuf::from(".")]
    } else {
        args.files.clone()
    };

    for path in paths {
        if path.is_dir() {
            collect_mo_files(&path, &mut files, args.recursive)?;
        } else if path.is_file() && path.extension().is_some_and(|ext| ext == "mo") {
            files.push(path);
        }
    }

    Ok(files)
}

fn collect_mo_files(dir: &Path, files: &mut Vec<PathBuf>, recursive: bool) -> Result<()> {
    let entries = fs::read_dir(dir)
        .with_context(|| format!("Failed to read directory: {}", dir.display()))?;

    for entry in entries {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() && recursive {
            let dir_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if !dir_name.starts_with('.') && dir_name != "target" {
                collect_mo_files(&path, files, recursive)?;
            }
        } else if path.is_file() && path.extension().is_some_and(|ext| ext == "mo") {
            files.push(path);
        }
    }

    Ok(())
}

fn output_text(results: &[LintResult], args: &Args, config: &LintConfig) {
    let mut total_by_level = [0usize; 4];

    for result in results {
        let messages: Vec<_> = result
            .messages
            .iter()
            .filter(|m| config.should_report(m))
            .collect();

        if messages.is_empty() && !args.verbose {
            continue;
        }

        for msg in &messages {
            match msg.level {
                LintLevel::Help => total_by_level[0] += 1,
                LintLevel::Note => total_by_level[1] += 1,
                LintLevel::Warning => total_by_level[2] += 1,
                LintLevel::Error => total_by_level[3] += 1,
            }

            println!("{}: {}", msg.level, msg.message);
            println!("  --> {}:{}:{}", msg.file, msg.line, msg.column);
            println!("   = rule: {}", msg.rule);
            if let Some(ref suggestion) = msg.suggestion {
                println!("   = suggestion: {}", suggestion);
            }
            println!();
        }
    }

    if !args.quiet {
        let total: usize = total_by_level.iter().sum();
        if total > 0 {
            eprintln!(
                "Found {} issue(s): {} error(s), {} warning(s), {} note(s), {} help",
                total, total_by_level[3], total_by_level[2], total_by_level[1], total_by_level[0]
            );
        } else if args.verbose {
            eprintln!("No issues found");
        }
    }
}

fn output_json(results: &[LintResult]) -> Result<()> {
    let output: Vec<serde_json::Value> = results
        .iter()
        .flat_map(|r| {
            r.messages.iter().map(|m| {
                serde_json::json!({
                    "rule": m.rule,
                    "level": m.level.to_string(),
                    "message": m.message,
                    "file": m.file,
                    "line": m.line,
                    "column": m.column,
                    "suggestion": m.suggestion,
                })
            })
        })
        .collect();

    println!("{}", serde_json::to_string_pretty(&output)?);
    Ok(())
}

fn output_compact(results: &[LintResult], config: &LintConfig) {
    for result in results {
        for msg in &result.messages {
            if config.should_report(msg) {
                println!(
                    "{}:{}:{}: {}: [{}] {}",
                    msg.file, msg.line, msg.column, msg.level, msg.rule, msg.message
                );
            }
        }
    }
}
