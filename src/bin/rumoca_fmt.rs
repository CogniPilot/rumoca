//! # Rumoca Modelica Formatter
//!
//! A command-line tool for formatting Modelica files, similar to `rustfmt`.
//!
//! ## Features
//! - Consistent indentation (configurable: 2/4 spaces or tabs)
//! - Proper spacing around operators
//! - Normalized line endings
//! - Multiple consecutive empty lines collapsed to one
//! - Recursive directory formatting
//! - Config file support (`.rumoca_fmt.toml` or `rumoca_fmt.toml`)
//!
//! ## Configuration
//!
//! The formatter looks for config files in the following order:
//! 1. `.rumoca_fmt.toml` in the file's directory or any parent directory
//! 2. `rumoca_fmt.toml` in the file's directory or any parent directory
//!
//! Example config file:
//! ```toml
//! indent_size = 2
//! use_tabs = false
//! max_line_length = 100
//! ```
//!
//! CLI options override config file settings.
//!
//! ## Usage
//! ```sh
//! # Format all .mo files in current directory (recursively)
//! rumoca-fmt
//!
//! # Format specific files
//! rumoca-fmt file1.mo file2.mo
//!
//! # Format all .mo files in a directory (recursively)
//! rumoca-fmt src/models/
//!
//! # Check if files are formatted (exit 1 if not)
//! rumoca-fmt --check
//!
//! # Print files that would be reformatted
//! rumoca-fmt --check -l
//!
//! # Use 4 spaces for indentation (overrides config file)
//! rumoca-fmt --config indent_size=4
//!
//! # Use tabs for indentation
//! rumoca-fmt --config use_tabs=true
//!
//! # Print to stdout instead of modifying files
//! rumoca-fmt --emit stdout file.mo
//!
//! # Read from stdin
//! cat file.mo | rumoca-fmt --stdin
//! ```

use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use rumoca::{CONFIG_FILE_NAMES, FormatOptions, format_modelica};
use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq)]
enum EmitMode {
    /// Write formatted output to the original files
    Files,
    /// Write formatted output to stdout
    Stdout,
}

#[derive(Parser, Debug)]
#[command(
    name = "rumoca-fmt",
    version,
    about = "Format Modelica code",
    long_about = "Format Modelica source files with consistent style.\n\n\
                  Without arguments, formats all .mo files in the current directory recursively.\n\
                  With file/directory arguments, formats those files/directories.\n\n\
                  Similar to rustfmt, by default it modifies files in-place.\n\
                  Use --check to verify formatting without modifying files."
)]
struct Args {
    /// Files or directories to format (default: current directory)
    #[arg(name = "FILES")]
    files: Vec<PathBuf>,

    /// Run in 'check' mode. Exits with 0 if input is formatted correctly.
    /// Exits with 1 and prints a diff if formatting is required.
    #[arg(long)]
    check: bool,

    /// What data to emit and how
    #[arg(long, value_enum, default_value = "files")]
    emit: EmitMode,

    /// Backup any modified files (adds .bak extension)
    #[arg(long)]
    backup: bool,

    /// Prints the names of files that were formatted or would be formatted (with --check)
    #[arg(short = 'l', long = "files-with-diff")]
    files_with_diff: bool,

    /// Set options from command line (key=value, comma-separated)
    /// Available options: indent_size=N, use_tabs=true/false
    #[arg(long, value_name = "key1=val1,key2=val2...")]
    config: Option<String>,

    /// Print verbose output
    #[arg(short = 'v', long)]
    verbose: bool,

    /// Print less output
    #[arg(short = 'q', long)]
    quiet: bool,

    /// Read input from stdin (requires exactly one file argument for filename context)
    #[arg(long)]
    stdin: bool,

    /// Recursively format directories (default: true)
    #[arg(long, default_value = "true")]
    recursive: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Parse CLI config options
    let cli_config = parse_cli_config(&args.config)?;

    // Collect files to format
    let files = collect_files(&args)?;

    // Determine start directory for config file search
    let start_dir = if !files.is_empty() {
        files[0].clone()
    } else {
        std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
    };

    // Load format options (config file + CLI overrides)
    let options = load_format_options(&start_dir, &cli_config, args.verbose);

    if files.is_empty() && !args.stdin {
        if !args.quiet {
            eprintln!("No Modelica files found");
        }
        return Ok(());
    }

    let mut any_unformatted = false;
    let mut formatted_count = 0;
    let mut unchanged_count = 0;
    let mut error_count = 0;

    if args.stdin {
        // Read from stdin
        let result = process_stdin(&options, &args);
        match result {
            Ok(already_formatted) => {
                if !already_formatted {
                    any_unformatted = true;
                    formatted_count += 1;
                } else {
                    unchanged_count += 1;
                }
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                error_count += 1;
            }
        }
    } else {
        for path in &files {
            let result = process_file(path, &options, &args);

            match result {
                Ok(already_formatted) => {
                    if !already_formatted {
                        any_unformatted = true;
                        formatted_count += 1;
                    } else {
                        unchanged_count += 1;
                    }
                }
                Err(e) => {
                    eprintln!("Error processing {}: {}", path.display(), e);
                    error_count += 1;
                }
            }
        }
    }

    // Print summary if verbose
    if args.verbose && !args.stdin {
        if args.check {
            eprintln!(
                "\n{} file(s) would be reformatted, {} file(s) already formatted",
                formatted_count, unchanged_count
            );
        } else {
            eprintln!(
                "\n{} file(s) reformatted, {} file(s) unchanged",
                formatted_count, unchanged_count
            );
        }
        if error_count > 0 {
            eprintln!("{} error(s)", error_count);
        }
    }

    if args.check && any_unformatted {
        std::process::exit(1);
    }

    if error_count > 0 {
        std::process::exit(1);
    }

    Ok(())
}

/// Parsed CLI config options (None means not specified)
struct CliConfigOptions {
    indent_size: Option<usize>,
    use_tabs: Option<bool>,
    max_line_length: Option<usize>,
    blank_lines_between_classes: Option<usize>,
}

/// Parse configuration options from --config flag
fn parse_cli_config(config: &Option<String>) -> Result<CliConfigOptions> {
    let mut options = CliConfigOptions {
        indent_size: None,
        use_tabs: None,
        max_line_length: None,
        blank_lines_between_classes: None,
    };

    if let Some(config_str) = config {
        for part in config_str.split(',') {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }

            let (key, value) = part.split_once('=').ok_or_else(|| {
                anyhow::anyhow!("Invalid config format: '{}'. Expected key=value", part)
            })?;

            match key.trim() {
                "indent_size" => {
                    options.indent_size = Some(
                        value
                            .trim()
                            .parse()
                            .with_context(|| format!("Invalid indent_size value: {}", value))?,
                    );
                }
                "use_tabs" => {
                    options.use_tabs = Some(match value.trim().to_lowercase().as_str() {
                        "true" | "1" | "yes" => true,
                        "false" | "0" | "no" => false,
                        _ => bail!("Invalid use_tabs value: {}. Expected true/false", value),
                    });
                }
                "max_line_length" => {
                    options.max_line_length =
                        Some(value.trim().parse().with_context(|| {
                            format!("Invalid max_line_length value: {}", value)
                        })?);
                }
                "blank_lines_between_classes" => {
                    options.blank_lines_between_classes =
                        Some(value.trim().parse().with_context(|| {
                            format!("Invalid blank_lines_between_classes value: {}", value)
                        })?);
                }
                _ => bail!(
                    "Unknown config option: {}. Available: indent_size, use_tabs, max_line_length, blank_lines_between_classes",
                    key
                ),
            }
        }
    }

    Ok(options)
}

/// Load format options, merging config file and CLI options
fn load_format_options(
    start_dir: &Path,
    cli_config: &CliConfigOptions,
    verbose: bool,
) -> FormatOptions {
    // Start with file-based config or defaults
    let mut options = if let Some(file_options) = FormatOptions::from_config_file(start_dir) {
        if verbose {
            // Find which config file was used
            let mut current = start_dir.to_path_buf();
            if current.is_file()
                && let Some(parent) = current.parent()
            {
                current = parent.to_path_buf();
            }
            'outer: loop {
                for config_name in CONFIG_FILE_NAMES {
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
        file_options
    } else {
        FormatOptions::default()
    };

    // Merge CLI options (they take precedence)
    options.merge_cli_options_ext(
        cli_config.indent_size,
        cli_config.use_tabs,
        cli_config.max_line_length,
        cli_config.blank_lines_between_classes,
    );

    options
}

/// Collect all files to format
fn collect_files(args: &Args) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    let paths = if args.files.is_empty() {
        // Default to current directory
        vec![PathBuf::from(".")]
    } else {
        args.files.clone()
    };

    for path in paths {
        if path.to_string_lossy() == "-" {
            // stdin marker - skip, handled separately
            continue;
        }

        if path.is_dir() {
            // Recursively find .mo files
            collect_mo_files(&path, &mut files, args.recursive)?;
        } else if path.is_file() {
            // Check if it's a .mo file
            if path.extension().is_some_and(|ext| ext == "mo") {
                files.push(path);
            } else if !args.quiet {
                eprintln!("Warning: Skipping non-Modelica file: {}", path.display());
            }
        } else if !args.quiet {
            eprintln!("Warning: Path does not exist: {}", path.display());
        }
    }

    Ok(files)
}

/// Recursively collect .mo files from a directory
fn collect_mo_files(dir: &Path, files: &mut Vec<PathBuf>, recursive: bool) -> Result<()> {
    let entries = fs::read_dir(dir)
        .with_context(|| format!("Failed to read directory: {}", dir.display()))?;

    for entry in entries {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            if recursive {
                // Skip hidden directories and common non-source directories
                let dir_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
                if !dir_name.starts_with('.') && dir_name != "target" && dir_name != "node_modules"
                {
                    collect_mo_files(&path, files, recursive)?;
                }
            }
        } else if path.is_file() && path.extension().is_some_and(|ext| ext == "mo") {
            files.push(path);
        }
    }

    Ok(())
}

/// Process a file
/// Returns Ok(true) if file was already formatted, Ok(false) if it needed formatting
fn process_file(path: &PathBuf, options: &FormatOptions, args: &Args) -> Result<bool> {
    let input = fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {}", path.display()))?;

    let formatted = format_modelica(&input, options);
    let already_formatted = formatted == input;

    if args.check {
        if !already_formatted {
            if args.files_with_diff {
                println!("{}", path.display());
            } else if !args.quiet {
                println!("Would reformat: {}", path.display());
            }
        } else if args.verbose {
            println!("{} (unchanged)", path.display());
        }
        return Ok(already_formatted);
    }

    match args.emit {
        EmitMode::Files => {
            if !already_formatted {
                // Create backup if requested
                if args.backup {
                    let backup_path = path.with_extension("mo.bak");
                    fs::copy(path, &backup_path).with_context(|| {
                        format!("Failed to create backup: {}", backup_path.display())
                    })?;
                }

                fs::write(path, &formatted)
                    .with_context(|| format!("Failed to write file: {}", path.display()))?;

                if args.files_with_diff {
                    println!("{}", path.display());
                } else if !args.quiet {
                    println!("Reformatted: {}", path.display());
                }
            } else if args.verbose {
                println!("{} (unchanged)", path.display());
            }
        }
        EmitMode::Stdout => {
            print!("{}", formatted);
        }
    }

    Ok(already_formatted)
}

/// Process stdin
fn process_stdin(options: &FormatOptions, args: &Args) -> Result<bool> {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .context("Failed to read from stdin")?;

    let formatted = format_modelica(&input, options);
    let already_formatted = formatted == input;

    if args.check {
        if !already_formatted && !args.quiet {
            eprintln!("stdin requires formatting");
        }
        return Ok(already_formatted);
    }

    io::stdout()
        .write_all(formatted.as_bytes())
        .context("Failed to write to stdout")?;

    Ok(already_formatted)
}
