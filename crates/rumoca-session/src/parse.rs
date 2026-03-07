//! Parallel file parsing utilities.
//!
//! This module provides efficient parallel parsing of Modelica files using rayon.

use anyhow::{Context, Result};
use rayon::prelude::*;
use rumoca_ir_ast as ast;
use std::path::Path;
use std::sync::Once;

use crate::merge::merge_stored_definitions;

static RAYON_INIT: Once = Once::new();

/// Initialize rayon thread pool with num_cpus - 2 threads and 16MB stack per thread.
/// This leaves two CPUs free for system responsiveness and the main thread.
/// The large stack size is needed for deep MSL class hierarchies.
fn init_rayon_pool() {
    RAYON_INIT.call_once(|| {
        let num_threads = std::thread::available_parallelism()
            .map(|n| n.get().saturating_sub(2).max(1))
            .unwrap_or(1);
        rayon::ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .stack_size(16 * 1024 * 1024) // 16 MB per thread for deep MSL class hierarchies
            .build_global()
            .ok(); // Ignore error if pool already initialized
    });
}

/// Result of successfully parsing a file: (file_path, parsed_definition)
pub type ParseSuccess = (String, ast::StoredDefinition);

/// Result of a failed file operation: (file_path, error_message)
pub type ParseFailure = (String, String);

/// Result type for parsing operations
pub type ParseResult = Result<ParseSuccess>;

/// Combined results from lenient parsing: (successes, failures)
pub type LenientParseResult = (Vec<ParseSuccess>, Vec<ParseFailure>);

/// Structured parse errors surfaced by the parser phase.
pub use rumoca_phase_parse::ParseError;

/// Parse a single Modelica source string into AST.
pub fn parse_source_to_ast(source: &str, file_name: &str) -> Result<ast::StoredDefinition> {
    rumoca_phase_parse::parse_to_ast(source, file_name)
}

/// Parse a single Modelica source with structured parse errors.
pub fn parse_source_to_ast_with_errors(
    source: &str,
    file_name: &str,
) -> std::result::Result<ast::StoredDefinition, Vec<ParseError>> {
    rumoca_phase_parse::parse_to_ast_with_errors(source, file_name)
}

/// Validate Modelica syntax for a source string.
pub fn validate_source_syntax(source: &str, file_name: &str) -> Result<()> {
    parse_source_to_ast(source, file_name).map(|_| ())
}

/// Parse multiple Modelica files in parallel using rayon.
///
/// Each file is parsed independently on its own thread, with results collected
/// into a vector of (file_path, StoredDefinition) tuples ready for merging.
///
/// # Arguments
///
/// * `paths` - Slice of file paths to parse
///
/// # Returns
///
/// A Result containing a vector of (file_path, StoredDefinition) tuples,
/// or an error if any file fails to parse.
pub fn parse_files_parallel<P: AsRef<Path> + Sync>(
    paths: &[P],
) -> Result<Vec<(String, ast::StoredDefinition)>> {
    init_rayon_pool();
    paths
        .par_iter()
        .map(|path| {
            let path = path.as_ref();
            let source = std::fs::read_to_string(path)
                .with_context(|| format!("Failed to read file: {}", path.display()))?;
            let file_name = path.to_string_lossy().to_string();
            let def = rumoca_phase_parse::parse_to_ast(&source, &file_name)
                .with_context(|| format!("Failed to parse file: {}", path.display()))?;
            Ok((file_name, def))
        })
        .collect()
}

/// Parse multiple Modelica files in parallel, collecting successes and failures.
///
/// Unlike `parse_files_parallel`, this function continues parsing all files even
/// if some fail, returning both successful parses and error messages.
///
/// # Arguments
///
/// * `paths` - Slice of file paths to parse
///
/// # Returns
///
/// A tuple of:
/// - Vector of [`ParseSuccess`] for successful parses
/// - Vector of [`ParseFailure`] for failed parses
pub fn parse_files_parallel_lenient<P: AsRef<Path> + Sync>(paths: &[P]) -> LenientParseResult {
    init_rayon_pool();
    let results: Vec<_> = paths
        .par_iter()
        .map(|path| {
            let path = path.as_ref();
            let file_name = path.to_string_lossy().to_string();
            match std::fs::read_to_string(path) {
                Ok(source) => match rumoca_phase_parse::parse_to_ast(&source, &file_name) {
                    Ok(def) => Ok((file_name, def)),
                    Err(e) => Err((file_name, format!("Parse error: {}", e))),
                },
                Err(e) => Err((file_name, format!("Read error: {}", e))),
            }
        })
        .collect();

    let mut successes = Vec::new();
    let mut failures = Vec::new();

    for result in results {
        match result {
            Ok(success) => successes.push(success),
            Err(failure) => failures.push(failure),
        }
    }

    (successes, failures)
}

/// Parse multiple Modelica files and merge them into a single StoredDefinition.
///
/// This is a convenience function that combines `parse_files_parallel` and
/// `merge_stored_definitions` into a single call.
///
/// # Arguments
///
/// * `paths` - Slice of file paths to parse
///
/// # Returns
///
/// A merged StoredDefinition containing all classes from all files.
pub fn parse_and_merge_parallel<P: AsRef<Path> + Sync>(
    paths: &[P],
) -> Result<ast::StoredDefinition> {
    let definitions = parse_files_parallel(paths)?;
    merge_stored_definitions(definitions)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_parse_single_file() {
        let mut file = NamedTempFile::new().unwrap();
        writeln!(file, "model M Real x; end M;").unwrap();

        let results = parse_files_parallel(&[file.path()]).unwrap();
        assert_eq!(results.len(), 1);
        assert!(results[0].1.classes.contains_key("M"));
    }

    #[test]
    fn test_parse_lenient() {
        let mut good_file = NamedTempFile::new().unwrap();
        writeln!(good_file, "model M Real x; end M;").unwrap();

        let mut bad_file = NamedTempFile::new().unwrap();
        writeln!(bad_file, "model M Real x end M;").unwrap(); // Missing semicolon

        let (successes, failures) =
            parse_files_parallel_lenient(&[good_file.path(), bad_file.path()]);

        assert_eq!(successes.len(), 1);
        assert_eq!(failures.len(), 1);
    }
}
