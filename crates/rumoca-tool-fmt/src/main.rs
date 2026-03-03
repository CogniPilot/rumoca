use clap::Parser;
use miette::{Result, miette};
use rumoca_tool_fmt::load_config_from_dir;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

#[derive(Parser)]
#[command(name = "rumoca-fmt", about = "Modelica code formatter")]
struct Cli {
    /// Files or directories to format. If empty, formats the current directory.
    #[arg()]
    paths: Vec<PathBuf>,

    /// Check formatting without writing changes. Exits with code 1 if any files need formatting.
    #[arg(long)]
    check: bool,

    /// Number of spaces per indentation level.
    #[arg(long)]
    indent_size: Option<usize>,

    /// Use tabs instead of spaces.
    #[arg(long)]
    use_tabs: Option<bool>,
}

fn collect_mo_files(path: &Path) -> Vec<PathBuf> {
    if path.is_file() {
        if path.extension().is_some_and(|ext| ext == "mo") {
            return vec![path.to_path_buf()];
        }
        return vec![];
    }
    WalkDir::new(path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "mo"))
        .map(|e| e.path().to_path_buf())
        .collect()
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let paths = if cli.paths.is_empty() {
        vec![PathBuf::from(".")]
    } else {
        cli.paths
    };

    // Load config from the first path's directory (or cwd)
    let config_dir = paths
        .first()
        .map(|p| {
            if p.is_dir() {
                p.clone()
            } else {
                p.parent().unwrap_or(Path::new(".")).to_path_buf()
            }
        })
        .unwrap_or_else(|| PathBuf::from("."));

    let mut options = load_config_from_dir(&config_dir)
        .map_err(|e| miette!("Failed to load config: {e}"))?
        .unwrap_or_default();

    // Apply CLI overrides
    if let Some(indent_size) = cli.indent_size {
        options.indent_size = indent_size;
    }
    if let Some(use_tabs) = cli.use_tabs {
        options.use_tabs = use_tabs;
    }

    let mut files: Vec<PathBuf> = Vec::new();
    for path in &paths {
        files.extend(collect_mo_files(path));
    }

    if files.is_empty() {
        eprintln!("No .mo files found");
        return Ok(());
    }

    let mut needs_formatting = 0usize;
    let mut errors = 0usize;

    for file in &files {
        let source = std::fs::read_to_string(file)
            .map_err(|e| miette!("Failed to read {}: {e}", file.display()))?;

        let formatted = match rumoca_tool_fmt::format(&source, &options) {
            Ok(f) => f,
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
        if cli.check {
            eprintln!("Would reformat: {}", file.display());
        } else {
            std::fs::write(file, &formatted)
                .map_err(|e| miette!("Failed to write {}: {e}", file.display()))?;
            eprintln!("Formatted: {}", file.display());
        }
    }

    let total = files.len();
    let unchanged = total - needs_formatting - errors;

    if cli.check {
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
