//! Parallel file parsing utilities.
//!
//! This module provides efficient parallel parsing of Modelica files using rayon.

use anyhow::Result;
use rayon::prelude::*;
use rumoca_ir_ast as ast;
use std::path::Path;
use std::sync::Once;

use crate::merge::merge_stored_definitions;
#[cfg(test)]
use crate::parsed_artifact_cache::{
    ParsedArtifactCacheStatus, parse_file_with_artifact_cache_status,
};
use crate::parsed_artifact_cache::{
    parse_file_with_artifact_cache, resolve_parsed_artifact_cache_dir,
};

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
/// Recoverable syntax artifact surfaced by the parser phase.
pub(crate) use rumoca_phase_parse::SyntaxFile;

/// Parse a single Modelica source string into AST.
pub fn parse_source_to_ast(source: &str, file_name: &str) -> Result<ast::StoredDefinition> {
    rumoca_phase_parse::parse_to_ast(source, file_name)
}

/// Parse a single Modelica source string into a recoverable syntax artifact.
pub(crate) fn parse_source_to_syntax(source: &str, file_name: &str) -> SyntaxFile {
    rumoca_phase_parse::parse_to_syntax(source, file_name)
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
    let cache_dir = resolve_parsed_artifact_cache_dir();
    parse_files_parallel_with_cache_dir(paths, cache_dir.as_deref())
}

pub(crate) fn parse_files_parallel_with_cache_dir<P: AsRef<Path> + Sync>(
    paths: &[P],
    cache_dir: Option<&Path>,
) -> Result<Vec<(String, ast::StoredDefinition)>> {
    init_rayon_pool();
    paths
        .par_iter()
        .map(|path| parse_file_with_artifact_cache(path.as_ref(), cache_dir))
        .collect()
}

#[cfg(test)]
pub(crate) fn parse_files_parallel_with_cache_statuses<P: AsRef<Path> + Sync>(
    paths: &[P],
    cache_dir: Option<&Path>,
) -> Result<Vec<(String, ast::StoredDefinition, ParsedArtifactCacheStatus)>> {
    init_rayon_pool();
    paths
        .par_iter()
        .map(|path| parse_file_with_artifact_cache_status(path.as_ref(), cache_dir))
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
    let cache_dir = resolve_parsed_artifact_cache_dir();
    parse_files_parallel_lenient_with_cache_dir(paths, cache_dir.as_deref())
}

pub(crate) fn parse_files_parallel_lenient_with_cache_dir<P: AsRef<Path> + Sync>(
    paths: &[P],
    cache_dir: Option<&Path>,
) -> LenientParseResult {
    init_rayon_pool();
    let results: Vec<_> = paths
        .par_iter()
        .map(|path| {
            let path = path.as_ref();
            let file_name = path.to_string_lossy().to_string();
            match parse_file_with_artifact_cache(path, cache_dir) {
                Ok(success) => Ok(success),
                Err(error) => Err((file_name, error.to_string())),
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

    #[test]
    fn parse_files_parallel_reuses_cached_artifacts_for_unchanged_files() {
        let temp = tempfile::tempdir().expect("tempdir");
        let cache_dir = temp.path().join("cache");
        let file = temp.path().join("Model.mo");
        std::fs::write(&file, "model M\n  Real x;\nend M;\n").expect("write model");

        let first = parse_files_parallel_with_cache_statuses(&[file.as_path()], Some(&cache_dir))
            .expect("first parse");
        assert_eq!(first.len(), 1);
        assert_eq!(first[0].2, ParsedArtifactCacheStatus::Miss);

        let second = parse_files_parallel_with_cache_statuses(&[file.as_path()], Some(&cache_dir))
            .expect("second parse");
        assert_eq!(second.len(), 1);
        assert_eq!(second[0].2, ParsedArtifactCacheStatus::Hit);
    }

    #[test]
    fn parse_files_parallel_only_reparses_changed_workspace_files() {
        let temp = tempfile::tempdir().expect("tempdir");
        let cache_dir = temp.path().join("cache");
        let left = temp.path().join("Left.mo");
        let right = temp.path().join("Right.mo");
        std::fs::write(&left, "model Left\n  Real x;\nend Left;\n").expect("write left");
        std::fs::write(&right, "model Right\n  Real y;\nend Right;\n").expect("write right");

        let first = parse_files_parallel_with_cache_statuses(
            &[left.as_path(), right.as_path()],
            Some(&cache_dir),
        )
        .expect("initial parse");
        assert_eq!(
            first
                .iter()
                .map(|(_, _, status)| *status)
                .collect::<Vec<_>>(),
            vec![
                ParsedArtifactCacheStatus::Miss,
                ParsedArtifactCacheStatus::Miss
            ]
        );
        std::fs::write(&right, "model Right\n  Real y;\n  Real z;\nend Right;\n")
            .expect("update right");
        let second = parse_files_parallel_with_cache_statuses(
            &[left.as_path(), right.as_path()],
            Some(&cache_dir),
        )
        .expect("second parse");
        assert_eq!(second.len(), 2);
        assert_eq!(
            second
                .iter()
                .map(|(_, _, status)| *status)
                .collect::<Vec<_>>(),
            vec![
                ParsedArtifactCacheStatus::Hit,
                ParsedArtifactCacheStatus::Miss
            ],
            "only the changed file should be reparsed on the second pass"
        );
    }
}
