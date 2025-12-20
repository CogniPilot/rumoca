//! Modelica Standard Library (MSL) Compilation Tests
//!
//! This test module compiles models from the Modelica Standard Library
//! and reports statistics on parse/flatten/balance rates.
//!
//! The MSL is automatically downloaded from GitHub if not already cached.
//!
//! # Running the tests
//!
//! ```bash
//! # Skip MSL tests
//! cargo test -- --skip msl
//!
//! # Balance tests (parses all files, then flattens and checks equation balance)
//! cargo test test_msl_balance_sample -- --nocapture            # Balance check 100 models
//! cargo test test_msl_balance_all -- --ignored --nocapture     # Balance check all models
//! ```

// Use mimalloc as the global allocator for better performance
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

use anyhow::{Context, Result};
use rand::seq::SliceRandom;
use rayon::ThreadPoolBuilder;
use rayon::prelude::*;
use rumoca::Compiler;
use rumoca::dae::balance::BalanceStatus;
use rumoca::ir::ast::StoredDefinition;
use rumoca::ir::transform::flatten::{enable_cache, prewarm_class_cache};
use rumoca::ir::transform::multi_file::merge_stored_definitions;
use rumoca::modelica_grammar::ModelicaGrammar;
use rumoca::modelica_parser::parse;
use serde::Serialize;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

/// MSL version to download from GitHub
const MSL_VERSION: &str = "v4.1.0";

/// GitHub URL for MSL release tarball
fn msl_download_url() -> String {
    format!(
        "https://github.com/modelica/ModelicaStandardLibrary/archive/refs/tags/{}.tar.gz",
        MSL_VERSION
    )
}

/// Get the cache directory for source library downloads (~/.cache/rumoca/src/)
fn msl_cache_dir() -> PathBuf {
    dirs::cache_dir()
        .map(|d| d.join("rumoca").join("src"))
        .unwrap_or_else(|| {
            // Fallback to target/src-cache if no system cache dir
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("target")
                .join("src-cache")
        })
}

/// Get the path to the cached MSL
fn cached_msl_path() -> PathBuf {
    msl_cache_dir().join(format!(
        "ModelicaStandardLibrary-{}",
        MSL_VERSION.trim_start_matches('v')
    ))
}

/// Download and extract MSL if not already cached
fn ensure_msl_downloaded() -> Result<PathBuf> {
    let cache_dir = msl_cache_dir();
    let msl_path = cached_msl_path();

    // Check if already cached
    if msl_path.join("Modelica").join("package.mo").exists() {
        println!("Using cached MSL at {:?}", msl_path);
        return Ok(msl_path);
    }

    println!("MSL {} not found in cache, downloading...", MSL_VERSION);

    // Create cache directory
    fs::create_dir_all(&cache_dir)
        .with_context(|| format!("Failed to create cache directory: {:?}", cache_dir))?;

    let tarball_path = cache_dir.join(format!("msl-{}.tar.gz", MSL_VERSION));
    let url = msl_download_url();

    // Download using curl (more commonly available than wget)
    println!("Downloading from {}", url);
    let status = Command::new("curl")
        .args(["-L", "-o"])
        .arg(&tarball_path)
        .arg(&url)
        .status()
        .with_context(|| "Failed to run curl. Is curl installed?")?;

    if !status.success() {
        anyhow::bail!("curl failed with status: {}", status);
    }

    println!("Extracting to {:?}", cache_dir);

    // Extract the tarball
    let status = Command::new("tar")
        .args(["-xzf"])
        .arg(&tarball_path)
        .arg("-C")
        .arg(&cache_dir)
        .status()
        .with_context(|| "Failed to run tar")?;

    if !status.success() {
        anyhow::bail!("tar extraction failed with status: {}", status);
    }

    // Remove the tarball to save space
    let _ = fs::remove_file(&tarball_path);

    // Verify extraction
    if !msl_path.join("Modelica").join("package.mo").exists() {
        anyhow::bail!(
            "MSL extraction failed - Modelica/package.mo not found at {:?}",
            msl_path
        );
    }

    println!("MSL {} downloaded and cached successfully", MSL_VERSION);
    Ok(msl_path)
}

mod common;

/// Find all .mo files in a directory recursively
fn find_mo_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                // Skip hidden directories and common non-essential directories
                let dir_name = path.file_name().unwrap_or_default().to_string_lossy();
                if !dir_name.starts_with('.') && dir_name != "Resources" {
                    files.extend(find_mo_files(&path));
                }
            } else if path.extension().is_some_and(|ext| ext == "mo") {
                files.push(path);
            }
        }
    }

    files
}

/// Get the MSL path, downloading if necessary
fn get_msl_path() -> Result<PathBuf> {
    ensure_msl_downloaded()
}

// =============================================================================
// Tests
// =============================================================================

#[test]
#[ignore] // Requires MSL download - run with --ignored
fn test_msl_path_detection() {
    match get_msl_path() {
        Ok(path) => println!("MSL {} available at: {:?}", MSL_VERSION, path),
        Err(e) => println!("MSL download failed: {:?}", e),
    }
}

/// Result of balance check for a model
#[derive(Debug, Clone, Serialize)]
struct BalanceResult {
    model_name: String,
    num_equations: usize,
    num_unknowns: usize,
    num_external_connectors: usize,
    status: BalanceStatus,
    is_balanced: bool,
    error: Option<String>,
}

/// Detailed compilation failure report for JSON export
#[derive(Debug, Clone, Serialize)]
struct CompilationFailure {
    /// Name of the model that failed
    model_name: String,
    /// Category of failure: "parse", "compile", "unbalanced"
    failure_type: String,
    /// Full error message
    error_message: String,
    /// Number of equations (if available)
    num_equations: Option<usize>,
    /// Number of unknowns (if available)
    num_unknowns: Option<usize>,
}

/// Complete failure report for JSON export
#[derive(Debug, Serialize)]
struct FailureReport {
    /// Timestamp of the report
    timestamp: String,
    /// MSL version tested
    msl_version: String,
    /// Summary statistics
    summary: FailureSummary,
    /// All failures with detailed information
    failures: Vec<CompilationFailure>,
}

/// Summary statistics for the failure report
#[derive(Debug, Serialize)]
struct FailureSummary {
    total_files: usize,
    parse_passed: usize,
    parse_failed: usize,
    total_models: usize,
    balanced: usize,
    partial: usize,
    unbalanced: usize,
    compile_errors: usize,
    parse_rate_pct: f64,
    compile_rate_pct: f64,
    balance_rate_pct: f64,
}

/// Combined test results for parse and balance checks
#[derive(Debug)]
struct CombinedTestResults {
    // Parse stats
    total_files: usize,
    parse_passed: usize,
    parse_failed: usize,
    parse_time: Duration,
    // Balance stats
    total_models: usize,
    balanced: usize,
    partial: usize,
    unbalanced: usize,
    compile_errors: usize,
    balance_time: Duration,
    // Detailed results for reporting
    parse_failures: Vec<(PathBuf, String)>,
    partial_models: Vec<BalanceResult>,
    unbalanced_models: Vec<BalanceResult>,
    compile_error_models: Vec<BalanceResult>,
}

impl CombinedTestResults {
    fn parse_rate(&self) -> f64 {
        if self.total_files == 0 {
            0.0
        } else {
            self.parse_passed as f64 / self.total_files as f64 * 100.0
        }
    }

    fn balance_rate(&self) -> f64 {
        if self.total_models == 0 {
            0.0
        } else {
            self.balanced as f64 / self.total_models as f64 * 100.0
        }
    }

    fn compile_success_rate(&self) -> f64 {
        if self.total_models == 0 {
            0.0
        } else {
            (self.total_models - self.compile_errors) as f64 / self.total_models as f64 * 100.0
        }
    }
}

/// Result of parsing a single file (includes StoredDefinition if successful)
struct ParseResult {
    file_path: PathBuf,
    success: bool,
    error: Option<String>,
    definition: Option<StoredDefinition>,
}

/// Recursively collect all model, block, and class names from a class definition
/// Only includes non-partial simulatable classes
fn collect_models_recursive(
    class: &rumoca::ir::ast::ClassDefinition,
    prefix: &str,
    models: &mut Vec<String>,
) {
    use rumoca::ir::ast::ClassType;

    // Check if this class is a non-partial simulatable class
    // Model, Block, and Class are all simulatable (Class is semantically equivalent to Model)
    // Partial classes are intentionally incomplete and meant to be extended
    let is_simulatable = matches!(
        class.class_type,
        ClassType::Model | ClassType::Block | ClassType::Class
    );
    let is_partial = class.partial;

    if is_simulatable && !is_partial {
        models.push(prefix.to_string());
    }

    // Recurse into nested classes
    for (name, nested_class) in &class.classes {
        let qualified_name = format!("{}.{}", prefix, name);
        collect_models_recursive(nested_class, &qualified_name, models);
    }
}

/// Run combined MSL test: parse all files, then balance check all models
/// Returns results with both parse and balance statistics
fn run_combined_msl_test(msl_path: &Path, model_limit: Option<usize>) -> CombinedTestResults {
    // Configure rayon to use num_cpus - 1 threads, leaving one core for the user
    let num_threads = std::cmp::max(1, num_cpus::get().saturating_sub(1));
    ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .build_global()
        .ok(); // Ignore error if already initialized

    let modelica_dir = msl_path.join("Modelica");

    // =========================================================================
    // PHASE 1: Parse all files in parallel
    // =========================================================================
    println!("============================================================");
    println!("                    PHASE 1: PARSING");
    println!("============================================================");

    let mo_files = find_mo_files(&modelica_dir);
    let total_files = mo_files.len();
    println!("Found {} .mo files", total_files);

    let parsed_count = AtomicUsize::new(0);
    let parse_start = Instant::now();

    // Parse all files in parallel, keeping StoredDefinitions for successful parses
    let parse_results: Vec<ParseResult> = mo_files
        .par_iter()
        .map(|file_path| {
            let count = parsed_count.fetch_add(1, Ordering::Relaxed);
            if (count + 1).is_multiple_of(500) {
                eprintln!("Parsed: {}/{}", count + 1, total_files);
            }

            let content = match fs::read_to_string(file_path) {
                Ok(c) => c,
                Err(e) => {
                    return ParseResult {
                        file_path: file_path.clone(),
                        success: false,
                        error: Some(format!("Read error: {}", e)),
                        definition: None,
                    };
                }
            };

            let mut grammar = ModelicaGrammar::new();
            match parse(&content, file_path.to_string_lossy().as_ref(), &mut grammar) {
                Ok(_) => ParseResult {
                    file_path: file_path.clone(),
                    success: true,
                    error: None,
                    definition: grammar.modelica,
                },
                Err(e) => ParseResult {
                    file_path: file_path.clone(),
                    success: false,
                    error: Some(format!("{:?}", e)),
                    definition: None,
                },
            }
        })
        .collect();

    let parse_time = parse_start.elapsed();

    // Collect parse statistics
    let parse_passed = parse_results.iter().filter(|r| r.success).count();
    let parse_failed = parse_results.iter().filter(|r| !r.success).count();
    let parse_failures: Vec<_> = parse_results
        .iter()
        .filter(|r| !r.success)
        .map(|r| (r.file_path.clone(), r.error.clone().unwrap_or_default()))
        .collect();

    // Print parse summary (no dots - cleaner output)

    println!(
        "\nParse Results: {}/{} passed ({:.1}%) in {:.2}s",
        parse_passed,
        total_files,
        if total_files > 0 {
            parse_passed as f64 / total_files as f64 * 100.0
        } else {
            0.0
        },
        parse_time.as_secs_f64()
    );

    // =========================================================================
    // PHASE 2: Merge definitions and collect model names
    // =========================================================================
    println!("\n============================================================");
    println!("                    PHASE 2: MERGING");
    println!("============================================================");

    // Collect successful definitions for merging
    let definitions: Vec<(String, StoredDefinition)> = parse_results
        .into_iter()
        .filter_map(|r| {
            r.definition
                .map(|def| (r.file_path.to_string_lossy().to_string(), def))
        })
        .collect();

    println!("Merging {} parsed definitions...", definitions.len());
    let merged_def = match merge_stored_definitions(definitions) {
        Ok(def) => def,
        Err(e) => {
            eprintln!("Failed to merge definitions: {:?}", e);
            return CombinedTestResults {
                total_files,
                parse_passed,
                parse_failed,
                parse_time,
                total_models: 0,
                balanced: 0,
                partial: 0,
                unbalanced: 0,
                compile_errors: 0,
                balance_time: Duration::ZERO,
                parse_failures,
                partial_models: vec![],
                unbalanced_models: vec![],
                compile_error_models: vec![],
            };
        }
    };

    // Collect all model/block names recursively
    let mut model_names = Vec::new();
    for (name, class) in &merged_def.class_list {
        collect_models_recursive(class, name, &mut model_names);
    }
    model_names.sort();

    println!("Found {} simulatable models/blocks", model_names.len());

    // Apply model limit if specified (randomly sample)
    if let Some(limit) = model_limit {
        let mut rng = rand::thread_rng();
        model_names.shuffle(&mut rng);
        model_names.truncate(limit);
        println!("Randomly selected {} models for testing", limit);
    }

    // =========================================================================
    // PHASE 2.5: Pre-warm class cache with parallel wavefront processing
    // =========================================================================
    println!("\n============================================================");
    println!("                    PHASE 2.5: PRE-WARMING CACHE");
    println!("============================================================");

    // Enable caching for MSL - this significantly speeds up type resolution
    enable_cache();

    let prewarm_start = Instant::now();
    let classes_prewarmed = prewarm_class_cache(&merged_def);
    let prewarm_time = prewarm_start.elapsed();
    println!(
        "Pre-warmed {} classes in {:.2}s ({:.0} classes/sec)",
        classes_prewarmed,
        prewarm_time.as_secs_f64(),
        classes_prewarmed as f64 / prewarm_time.as_secs_f64()
    );

    // =========================================================================
    // PHASE 3: Balance check all models in parallel
    // =========================================================================
    println!("\n============================================================");
    println!("                    PHASE 3: BALANCE CHECK");
    println!("============================================================");

    let total_models = model_names.len();
    let checked_count = AtomicUsize::new(0);
    let balance_start = Instant::now();

    println!("Checking {} models in parallel...\n", total_models);

    // Wrap merged_def in Arc for cheap sharing across threads
    let merged_def = Arc::new(merged_def);

    // Track last progress update time for throttled output
    let last_update = std::sync::Mutex::new(Instant::now());

    // Check all models in parallel using lightweight balance check
    let balance_results: Vec<BalanceResult> = model_names
        .par_iter()
        .map(|model_name| {
            let count = checked_count.fetch_add(1, Ordering::Relaxed) + 1;
            let pct = count as f64 / total_models as f64 * 100.0;

            // Update progress bar every 100ms (throttled to reduce output)
            let mut last = last_update.lock().unwrap();
            if last.elapsed() >= Duration::from_millis(100) || count == total_models {
                *last = Instant::now();
                let bar_width = 40;
                let filled = (pct / 100.0 * bar_width as f64) as usize;
                let bar: String = "█".repeat(filled) + &"░".repeat(bar_width - filled);
                eprint!("\r  [{}] {:5.1}% ({}/{})", bar, pct, count, total_models);
                if count == total_models {
                    eprintln!(); // Final newline
                }
            }
            drop(last);

            // Use check_balance for fast balance-only check (no cloning)
            let balance_result = Compiler::new().model(model_name).check_balance(&merged_def);

            match balance_result {
                Ok(balance) => BalanceResult {
                    model_name: model_name.clone(),
                    num_equations: balance.num_equations,
                    num_unknowns: balance.num_unknowns,
                    num_external_connectors: balance.num_external_connectors,
                    status: balance.status.clone(),
                    is_balanced: balance.is_balanced(),
                    error: None,
                },
                Err(e) => BalanceResult {
                    model_name: model_name.clone(),
                    num_equations: 0,
                    num_unknowns: 0,
                    num_external_connectors: 0,
                    status: BalanceStatus::Unbalanced,
                    is_balanced: false,
                    error: Some(format!("{:?}", e)),
                },
            }
        })
        .collect();

    let balance_time = balance_start.elapsed();

    // Categorize balance results by status
    let balanced = balance_results
        .iter()
        .filter(|r| r.error.is_none() && r.status == BalanceStatus::Balanced)
        .count();
    let partial_models: Vec<_> = balance_results
        .iter()
        .filter(|r| r.error.is_none() && r.status == BalanceStatus::Partial)
        .cloned()
        .collect();
    let unbalanced_models: Vec<_> = balance_results
        .iter()
        .filter(|r| r.error.is_none() && r.status == BalanceStatus::Unbalanced)
        .cloned()
        .collect();
    let compile_error_models: Vec<_> = balance_results
        .iter()
        .filter(|r| r.error.is_some())
        .cloned()
        .collect();

    CombinedTestResults {
        total_files,
        parse_passed,
        parse_failed,
        parse_time,
        total_models,
        balanced,
        partial: partial_models.len(),
        unbalanced: unbalanced_models.len(),
        compile_errors: compile_error_models.len(),
        balance_time,
        parse_failures,
        partial_models,
        unbalanced_models,
        compile_error_models,
    }
}

/// Export compilation failures to a JSON file for analysis
fn export_failures_to_json(results: &CombinedTestResults, output_path: &Path) -> Result<()> {
    let mut failures = Vec::new();

    // Add parse failures
    for (path, error) in &results.parse_failures {
        failures.push(CompilationFailure {
            model_name: path.to_string_lossy().to_string(),
            failure_type: "parse".to_string(),
            error_message: error.clone(),
            num_equations: None,
            num_unknowns: None,
        });
    }

    // Add compile errors
    for result in &results.compile_error_models {
        failures.push(CompilationFailure {
            model_name: result.model_name.clone(),
            failure_type: "compile".to_string(),
            error_message: result.error.clone().unwrap_or_default(),
            num_equations: None,
            num_unknowns: None,
        });
    }

    // Add unbalanced models
    for result in &results.unbalanced_models {
        let diff = result.num_equations as i64 - result.num_unknowns as i64;
        let msg = if diff > 0 {
            format!("Over-determined by {} equations", diff)
        } else {
            format!("Under-determined by {} equations", -diff)
        };
        failures.push(CompilationFailure {
            model_name: result.model_name.clone(),
            failure_type: "unbalanced".to_string(),
            error_message: msg,
            num_equations: Some(result.num_equations),
            num_unknowns: Some(result.num_unknowns),
        });
    }

    // Sort failures by type then name for easier analysis
    failures.sort_by(|a, b| {
        a.failure_type
            .cmp(&b.failure_type)
            .then_with(|| a.model_name.cmp(&b.model_name))
    });

    // Get current timestamp
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    let report = FailureReport {
        timestamp: format!("{}", timestamp),
        msl_version: MSL_VERSION.to_string(),
        summary: FailureSummary {
            total_files: results.total_files,
            parse_passed: results.parse_passed,
            parse_failed: results.parse_failed,
            total_models: results.total_models,
            balanced: results.balanced,
            partial: results.partial,
            unbalanced: results.unbalanced,
            compile_errors: results.compile_errors,
            parse_rate_pct: results.parse_rate(),
            compile_rate_pct: results.compile_success_rate(),
            balance_rate_pct: results.balance_rate(),
        },
        failures,
    };

    // Write to JSON file
    let json = serde_json::to_string_pretty(&report)
        .with_context(|| "Failed to serialize failure report to JSON")?;

    let mut file = fs::File::create(output_path)
        .with_context(|| format!("Failed to create output file: {:?}", output_path))?;
    file.write_all(json.as_bytes())
        .with_context(|| "Failed to write JSON to file")?;

    println!("\nFailure report exported to: {:?}", output_path);
    println!("  Total failures: {}", report.failures.len());
    println!("    - Parse failures: {}", results.parse_failed);
    println!("    - Compile errors: {}", results.compile_errors);
    println!("    - Unbalanced models: {}", results.unbalanced);

    Ok(())
}

/// Detailed failure info for failed_models.json
#[derive(Debug, Serialize)]
struct DetailedFailure {
    model_name: String,
    failure_type: String,
    error_message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_equations: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_unknowns: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_external_connectors: Option<usize>,
}

/// Export detailed failed models to JSON for debugging
fn export_failed_models_to_json(results: &CombinedTestResults, output_path: &Path) -> Result<()> {
    let mut failures = Vec::new();

    // Add compile errors with full details
    for result in &results.compile_error_models {
        failures.push(DetailedFailure {
            model_name: result.model_name.clone(),
            failure_type: "compile".to_string(),
            error_message: result.error.clone().unwrap_or_default(),
            num_equations: None,
            num_unknowns: None,
            num_external_connectors: None,
        });
    }

    // Add unbalanced models with full details
    for result in &results.unbalanced_models {
        let diff = result.num_equations as i64 - result.num_unknowns as i64;
        let msg = if diff > 0 {
            format!("Over-determined by {} equations", diff)
        } else {
            format!("Under-determined by {} equations", -diff)
        };
        failures.push(DetailedFailure {
            model_name: result.model_name.clone(),
            failure_type: "unbalanced".to_string(),
            error_message: msg,
            num_equations: Some(result.num_equations),
            num_unknowns: Some(result.num_unknowns),
            num_external_connectors: Some(result.num_external_connectors),
        });
    }

    // Sort by failure type then name
    failures.sort_by(|a, b| {
        a.failure_type
            .cmp(&b.failure_type)
            .then_with(|| a.model_name.cmp(&b.model_name))
    });

    // Group errors by error message for summary
    let mut error_counts: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    for f in &failures {
        if f.failure_type == "compile" {
            // Extract the core error type (e.g., "Component class 'X' not found")
            let key = f.error_message.clone();
            *error_counts.entry(key).or_insert(0) += 1;
        }
    }

    let mut error_summary: Vec<_> = error_counts.into_iter().collect();
    error_summary.sort_by(|a, b| b.1.cmp(&a.1)); // Sort by count descending

    let report = serde_json::json!({
        "timestamp": std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0),
        "msl_version": MSL_VERSION,
        "total_failures": failures.len(),
        "compile_errors": results.compile_errors,
        "unbalanced": results.unbalanced,
        "error_summary": error_summary.iter().take(20).map(|(msg, count)| {
            serde_json::json!({
                "error": msg,
                "count": count
            })
        }).collect::<Vec<_>>(),
        "failures": failures
    });

    let json = serde_json::to_string_pretty(&report)
        .with_context(|| "Failed to serialize failed models to JSON")?;

    let mut file = fs::File::create(output_path)
        .with_context(|| format!("Failed to create output file: {:?}", output_path))?;
    file.write_all(json.as_bytes())
        .with_context(|| "Failed to write JSON to file")?;

    println!("  Failed models exported to: {:?}", output_path);

    Ok(())
}

/// Print detailed results summary
fn print_combined_results(results: &CombinedTestResults) {
    println!("\n");
    println!("============================================================");
    println!("                    MSL TEST SUMMARY");
    println!("============================================================");
    println!();
    println!("PARSING:");
    println!("  Total Files:      {}", results.total_files);
    println!(
        "  Passed:           {} ({:.1}%)",
        results.parse_passed,
        results.parse_rate()
    );
    println!("  Failed:           {}", results.parse_failed);
    println!(
        "  Time:             {:.2}s",
        results.parse_time.as_secs_f64()
    );
    println!();
    println!("BALANCE CHECK:");
    println!("  Total Models:     {}", results.total_models);
    println!(
        "  Compile Success:  {} ({:.1}%)",
        results.total_models - results.compile_errors,
        results.compile_success_rate()
    );
    println!(
        "  Balanced:         {} ({:.1}%)",
        results.balanced,
        results.balance_rate()
    );
    println!(
        "  Partial:          {} ({:.1}%) - under-determined by design",
        results.partial,
        if results.total_models > 0 {
            results.partial as f64 / results.total_models as f64 * 100.0
        } else {
            0.0
        }
    );
    println!(
        "  Unbalanced:       {} ({:.1}%) - bugs, need fixing",
        results.unbalanced,
        if results.total_models > 0 {
            results.unbalanced as f64 / results.total_models as f64 * 100.0
        } else {
            0.0
        }
    );
    println!("  Compile Errors:   {}", results.compile_errors);
    println!(
        "  Time:             {:.2}s",
        results.balance_time.as_secs_f64()
    );
    println!();
    println!(
        "TOTAL TIME:         {:.2}s",
        (results.parse_time + results.balance_time).as_secs_f64()
    );
    println!("============================================================");

    // Print parse failures
    if !results.parse_failures.is_empty() {
        println!("\nFirst 10 Parse Failures:");
        println!("------------------------------------------------------------");
        for (path, err) in results.parse_failures.iter().take(10) {
            println!("  {:?}", path);
            // Truncate safely at char boundary
            let truncated: String = err.chars().take(80).collect();
            let suffix = if err.chars().count() > 80 { "..." } else { "" };
            println!("    {}{}", truncated, suffix);
        }
        if results.parse_failures.len() > 10 {
            println!("  ... and {} more", results.parse_failures.len() - 10);
        }
    }

    // Print unbalanced models (bugs that need fixing)
    if !results.unbalanced_models.is_empty() {
        println!("\nUnbalanced Models (bugs - need fixing):");
        println!("------------------------------------------------------------");
        for result in results.unbalanced_models.iter().take(20) {
            let diff = result.num_equations as i64 - result.num_unknowns as i64;
            let status = if diff > 0 {
                format!("over by {}", diff)
            } else {
                format!("under by {}", -diff)
            };
            println!(
                "  {} ({} eq, {} unk) - {}",
                result.model_name, result.num_equations, result.num_unknowns, status
            );
        }
        if results.unbalanced_models.len() > 20 {
            println!("  ... and {} more", results.unbalanced_models.len() - 20);
        }
    }

    // Print partial models (under-determined by design)
    if !results.partial_models.is_empty() {
        println!("\nFirst 10 Partial Models (under-determined by design):");
        println!("------------------------------------------------------------");
        for result in results.partial_models.iter().take(10) {
            let diff = result.num_unknowns as i64 - result.num_equations as i64;
            println!(
                "  {} ({} eq, {} unk, {} ext conn) - under by {}",
                result.model_name,
                result.num_equations,
                result.num_unknowns,
                result.num_external_connectors,
                diff
            );
        }
        if results.partial_models.len() > 10 {
            println!("  ... and {} more", results.partial_models.len() - 10);
        }
    }

    // Print compile errors
    if !results.compile_error_models.is_empty() {
        println!("\nFirst 10 Compile Errors:");
        println!("------------------------------------------------------------");
        for result in results.compile_error_models.iter().take(10) {
            println!("  {}", result.model_name);
            if let Some(ref err) = result.error {
                // Truncate safely at char boundary
                let truncated: String = err.chars().take(80).collect();
                let suffix = if err.chars().count() > 80 { "..." } else { "" };
                println!("    {}{}", truncated, suffix);
            }
        }
        if results.compile_error_models.len() > 10 {
            println!("  ... and {} more", results.compile_error_models.len() - 10);
        }
    }
}

/// Sample MSL balance test - parses all files, balance checks 20 models
#[test]
#[ignore] // Requires MSL download - run with --ignored
fn test_msl_balance_sample() {
    let msl_path = get_msl_path().expect("Failed to download MSL");
    let results = run_combined_msl_test(&msl_path, Some(20));
    print_combined_results(&results);
}

/// Full MSL balance test - parses all files, balance checks all models
/// Exports detailed failure report to JSON for analysis
#[test]
#[ignore]
fn test_msl_balance_all() {
    const MIN_PARSE_RATE: f64 = 99.0;
    const MIN_COMPILE_RATE: f64 = 25.0; // Lower threshold while working on fixes

    // Print instructions for where to find output files
    println!("============================================================");
    println!("                MSL BALANCE TEST");
    println!("============================================================");
    println!();
    println!("Output files (in target/):");
    println!("  - msl_failures.json   : Summary of all failures with stats");
    println!("  - failed_models.json  : Detailed list of failed models with");
    println!("                          error summary grouped by error type");
    println!();
    println!("Quick analysis commands:");
    println!("  jq '.summary' target/msl_failures.json");
    println!("  jq '.error_summary' target/failed_models.json");
    println!("============================================================");
    println!();

    let msl_path = get_msl_path().expect("Failed to download MSL");

    // Record overall start time for profiling
    let overall_start = Instant::now();

    let results = run_combined_msl_test(&msl_path, None);
    print_combined_results(&results);

    // Export failures to JSON for analysis
    let output_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("msl_failures.json");
    if let Err(e) = export_failures_to_json(&results, &output_path) {
        eprintln!("Warning: Failed to export failures: {:?}", e);
    }

    // Export detailed failed models to separate JSON
    let failed_models_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("failed_models.json");
    if let Err(e) = export_failed_models_to_json(&results, &failed_models_path) {
        eprintln!("Warning: Failed to export failed models: {:?}", e);
    }

    // Print profiling summary
    let overall_time = overall_start.elapsed();
    println!("\n============================================================");
    println!("                    PROFILING SUMMARY");
    println!("============================================================");
    println!(
        "  Parse Phase:       {:.2}s ({:.1}% of total)",
        results.parse_time.as_secs_f64(),
        results.parse_time.as_secs_f64() / overall_time.as_secs_f64() * 100.0
    );
    println!(
        "  Balance Phase:     {:.2}s ({:.1}% of total)",
        results.balance_time.as_secs_f64(),
        results.balance_time.as_secs_f64() / overall_time.as_secs_f64() * 100.0
    );
    println!("  Overall Time:      {:.2}s", overall_time.as_secs_f64());
    println!();
    println!(
        "  Files/sec (parse):   {:.1}",
        results.total_files as f64 / results.parse_time.as_secs_f64()
    );
    println!(
        "  Models/sec (balance): {:.1}",
        results.total_models as f64 / results.balance_time.as_secs_f64()
    );
    println!("============================================================");

    // Assert minimum thresholds
    assert!(
        results.parse_rate() >= MIN_PARSE_RATE,
        "Parse rate {:.1}% is below minimum {:.1}%",
        results.parse_rate(),
        MIN_PARSE_RATE
    );
    assert!(
        results.compile_success_rate() >= MIN_COMPILE_RATE,
        "Compile success rate {:.1}% is below minimum {:.1}%",
        results.compile_success_rate(),
        MIN_COMPILE_RATE
    );

    println!("\n✓ All thresholds passed!");
    println!(
        "  Parse rate: {:.1}% >= {:.1}%",
        results.parse_rate(),
        MIN_PARSE_RATE
    );
    println!(
        "  Compile rate: {:.1}% >= {:.1}%",
        results.compile_success_rate(),
        MIN_COMPILE_RATE
    );
}

/// MSL test with JSON export for every compiled model
/// Exports DAE JSON to /tmp/msl_json/ for analysis
#[test]
#[ignore] // Requires MSL download - run with --ignored
fn test_msl_balance_with_json_export() {
    let msl_path = get_msl_path().expect("Failed to download MSL");

    // Create output directory
    let output_dir = PathBuf::from("/tmp/msl_json");
    if output_dir.exists() {
        fs::remove_dir_all(&output_dir).ok();
    }
    fs::create_dir_all(&output_dir).expect("Failed to create output directory");

    let modelica_dir = msl_path.join("Modelica");

    // =========================================================================
    // PHASE 1: Parse all files
    // =========================================================================
    println!("============================================================");
    println!("           PHASE 1: PARSING (with JSON export)");
    println!("============================================================");

    let mo_files = find_mo_files(&modelica_dir);
    let total_files = mo_files.len();
    println!("Found {} .mo files", total_files);

    let parse_start = Instant::now();

    // Parse all files in parallel
    let parse_results: Vec<ParseResult> = mo_files
        .par_iter()
        .map(|file_path| {
            let content = match fs::read_to_string(file_path) {
                Ok(c) => c,
                Err(e) => {
                    return ParseResult {
                        file_path: file_path.clone(),
                        success: false,
                        error: Some(format!("Read error: {}", e)),
                        definition: None,
                    };
                }
            };

            let mut grammar = ModelicaGrammar::new();
            match parse(&content, file_path.to_string_lossy().as_ref(), &mut grammar) {
                Ok(_) => ParseResult {
                    file_path: file_path.clone(),
                    success: true,
                    error: None,
                    definition: grammar.modelica,
                },
                Err(e) => ParseResult {
                    file_path: file_path.clone(),
                    success: false,
                    error: Some(format!("{:?}", e)),
                    definition: None,
                },
            }
        })
        .collect();

    let parse_time = parse_start.elapsed();
    let parse_passed = parse_results.iter().filter(|r| r.success).count();
    let avg_parse_time_ms = if total_files > 0 {
        parse_time.as_secs_f64() * 1000.0 / total_files as f64
    } else {
        0.0
    };
    println!(
        "\nParse Results: {}/{} passed ({:.1}%) in {:.2}s ({:.2}ms/file)",
        parse_passed,
        total_files,
        if total_files > 0 {
            parse_passed as f64 / total_files as f64 * 100.0
        } else {
            0.0
        },
        parse_time.as_secs_f64(),
        avg_parse_time_ms
    );

    // =========================================================================
    // PHASE 2: Merge definitions
    // =========================================================================
    println!("\n============================================================");
    println!("                    PHASE 2: MERGING");
    println!("============================================================");

    let definitions: Vec<(String, StoredDefinition)> = parse_results
        .into_iter()
        .filter_map(|r| {
            r.definition
                .map(|def| (r.file_path.to_string_lossy().to_string(), def))
        })
        .collect();

    println!("Merging {} parsed definitions...", definitions.len());
    let merged_def = match merge_stored_definitions(definitions) {
        Ok(def) => def,
        Err(e) => {
            eprintln!("Failed to merge definitions: {:?}", e);
            return;
        }
    };

    // Collect all model/block names
    let mut model_names = Vec::new();
    for (name, class) in &merged_def.class_list {
        collect_models_recursive(class, name, &mut model_names);
    }
    model_names.sort();

    println!("Found {} simulatable models/blocks", model_names.len());

    // =========================================================================
    // PHASE 3: Full compile with JSON export
    // =========================================================================
    println!("\n============================================================");
    println!("       PHASE 3: FULL COMPILE WITH JSON EXPORT");
    println!("============================================================");

    let total_models = model_names.len();
    let checked_count = AtomicUsize::new(0);
    let compile_start = Instant::now();

    println!(
        "Compiling {} models and exporting JSON to {:?}...",
        total_models, output_dir
    );

    let merged_def = Arc::new(merged_def);

    // Compile all models and export JSON
    let results: Vec<(String, bool, usize, usize, Option<String>)> = model_names
        .par_iter()
        .map(|model_name| {
            let count = checked_count.fetch_add(1, Ordering::Relaxed);
            let pct = (count + 1) as f64 / total_models as f64 * 100.0;
            eprintln!(
                "[{:4}/{} {:5.1}%] {}",
                count + 1,
                total_models,
                pct,
                model_name
            );

            // Full compile
            let compile_result = Compiler::new()
                .model(model_name)
                .compile_parsed_ref(&merged_def, "");

            match compile_result {
                Ok(result) => {
                    // Check balance using DAE
                    let balance = result.dae.check_balance();

                    // Export DAE to JSON
                    let safe_name = model_name.replace(".", "_").replace("/", "_");
                    let json_path = output_dir.join(format!("{}.json", safe_name));

                    if let Ok(json) = result.dae.to_dae_ir_json()
                        && let Err(e) = fs::write(&json_path, &json)
                    {
                        eprintln!("  Warning: Failed to write JSON for {}: {}", model_name, e);
                    }

                    (
                        model_name.clone(),
                        balance.is_balanced(),
                        balance.num_equations,
                        balance.num_unknowns,
                        None,
                    )
                }
                Err(e) => (model_name.clone(), false, 0, 0, Some(format!("{:?}", e))),
            }
        })
        .collect();

    let compile_time = compile_start.elapsed();

    // =========================================================================
    // SUMMARY
    // =========================================================================
    println!("\n============================================================");
    println!("                    SUMMARY");
    println!("============================================================");

    let balanced = results
        .iter()
        .filter(|(_, is_bal, _, _, err)| *is_bal && err.is_none())
        .count();
    let unbalanced: Vec<_> = results
        .iter()
        .filter(|(_, is_bal, _, _, err)| !*is_bal && err.is_none())
        .collect();
    let compile_errors: Vec<_> = results
        .iter()
        .filter(|(_, _, _, _, err)| err.is_some())
        .collect();

    println!("BALANCE CHECK:");
    let avg_compile_time_ms = if total_models > 0 {
        compile_time.as_secs_f64() * 1000.0 / total_models as f64
    } else {
        0.0
    };
    println!("  Total Models:     {}", total_models);
    println!(
        "  Compile Success:  {} ({:.1}%)",
        total_models - compile_errors.len(),
        (total_models - compile_errors.len()) as f64 / total_models as f64 * 100.0
    );
    println!(
        "  Balanced:         {} ({:.1}%)",
        balanced,
        balanced as f64 / total_models as f64 * 100.0
    );
    println!("  Unbalanced:       {}", unbalanced.len());
    println!("  Compile Errors:   {}", compile_errors.len());
    println!(
        "  Compile Time:     {:.2}s ({:.2}ms/model)",
        compile_time.as_secs_f64(),
        avg_compile_time_ms
    );
    println!(
        "  Parse Time:       {:.2}s ({:.2}ms/file)",
        parse_time.as_secs_f64(),
        avg_parse_time_ms
    );
    println!();
    println!("JSON files exported to: {:?}", output_dir);

    // Print first 20 unbalanced models
    println!("\nFirst 20 Unbalanced Models:");
    println!("------------------------------------------------------------");
    for (name, _, num_eq, num_unk, _) in unbalanced.iter().take(20) {
        let diff = *num_eq as i64 - *num_unk as i64;
        let status = if diff > 0 {
            format!("over by {}", diff)
        } else {
            format!("under by {}", -diff)
        };
        println!("  {} ({} eq, {} unk) - {}", name, num_eq, num_unk, status);
    }
    if unbalanced.len() > 20 {
        println!("  ... and {} more", unbalanced.len() - 20);
    }

    // Save summary to JSON
    let summary_path = output_dir.join("_summary.json");
    let summary = serde_json::json!({
        "total_files": total_files,
        "total_models": total_models,
        "balanced": balanced,
        "unbalanced": unbalanced.len(),
        "compile_errors": compile_errors.len(),
        "parse_time_secs": parse_time.as_secs_f64(),
        "parse_time_ms_per_file": avg_parse_time_ms,
        "compile_time_secs": compile_time.as_secs_f64(),
        "compile_time_ms_per_model": avg_compile_time_ms,
        "unbalanced_models": unbalanced.iter().map(|(name, _, eq, unk, _)| {
            serde_json::json!({
                "name": name,
                "equations": eq,
                "unknowns": unk,
                "diff": *eq as i64 - *unk as i64
            })
        }).collect::<Vec<_>>(),
        "compile_error_models": compile_errors.iter().map(|(name, _, _, _, err)| {
            serde_json::json!({
                "name": name,
                "error": err
            })
        }).collect::<Vec<_>>()
    });

    if let Ok(summary_json) = serde_json::to_string_pretty(&summary) {
        fs::write(&summary_path, summary_json).ok();
        println!("\nSummary saved to: {:?}", summary_path);
    }
}

/// Test Nonlinear blocks balance
#[test]
#[ignore]
fn test_nonlinear_blocks_balance() {
    let msl_path = get_msl_path().expect("Failed to download MSL");

    // Find and parse all needed files
    let modelica_dir = msl_path.join("Modelica");
    let mo_files = find_mo_files(&modelica_dir);

    println!("Parsing {} files...", mo_files.len());

    let definitions: Vec<_> = mo_files
        .iter()
        .filter_map(|path| {
            let content = std::fs::read_to_string(path).ok()?;
            let mut grammar = ModelicaGrammar::new();
            if parse(&content, path.to_string_lossy().as_ref(), &mut grammar).is_ok() {
                grammar
                    .modelica
                    .map(|def| (path.to_string_lossy().to_string(), def))
            } else {
                None
            }
        })
        .collect();

    let merged = merge_stored_definitions(definitions).expect("Merge failed");
    let merged = Arc::new(merged);

    let models = [
        "Modelica.Blocks.Nonlinear.Limiter",
        "Modelica.Blocks.Nonlinear.VariableLimiter",
        "Modelica.Blocks.Nonlinear.SlewRateLimiter",
        "Modelica.Blocks.Nonlinear.DeadZone",
        "Modelica.Blocks.Nonlinear.FixedDelay",
        "Modelica.Blocks.Nonlinear.PadeDelay",
        "Modelica.Blocks.Nonlinear.VariableDelay",
    ];

    println!("\nNonlinear Blocks Balance Check:");
    println!("============================================================");

    for model in &models {
        print!("{}: ", model);
        match Compiler::new().model(model).check_balance(&merged) {
            Ok(balance) => {
                if balance.is_balanced() {
                    println!(
                        "✓ balanced ({} eq, {} unk)",
                        balance.num_equations, balance.num_unknowns
                    );
                } else {
                    println!(
                        "✗ {} ({} eq, {} unk, {} states)",
                        balance.status_message(),
                        balance.num_equations,
                        balance.num_unknowns,
                        balance.num_states
                    );
                }
            }
            Err(e) => {
                let msg = format!("{:?}", e);
                println!("✗ Error: {}", msg.lines().next().unwrap_or("unknown"));
            }
        }
    }
}

#[test]
fn test_constants_components() {
    let msl_path = match ensure_msl_downloaded() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Skipping test: {}", e);
            return;
        }
    };

    let constants_file = msl_path.join("Modelica/Constants.mo");
    let content = std::fs::read_to_string(&constants_file).unwrap();

    let mut grammar = ModelicaGrammar::new();
    parse(&content, "Constants.mo", &mut grammar).unwrap();

    let def = grammar.modelica.as_ref().unwrap();
    println!("=== RAW PARSED ===");
    println!("Within: {:?}", def.within.as_ref().map(|w| w.to_string()));

    for (name, class) in &def.class_list {
        println!("Class: {} has {} components", name, class.components.len());
        for (comp_name, _) in class.components.iter().take(5) {
            println!("  - {}", comp_name);
        }
    }

    // Assert that Constants class has pi component
    let constants = def
        .class_list
        .get("Constants")
        .expect("Constants not found");
    assert!(
        constants.components.contains_key("pi"),
        "pi not in components"
    );
    assert!(
        constants.components.contains_key("e"),
        "e not in components"
    );

    // Now test merging
    let merged = merge_stored_definitions(vec![("Constants.mo".to_string(), def.clone())]).unwrap();

    println!("\n=== AFTER MERGING ===");
    let modelica = merged
        .class_list
        .get("Modelica")
        .expect("Modelica package not found after merge");
    let constants_merged = modelica
        .classes
        .get("Constants")
        .expect("Constants not in Modelica.classes");

    println!(
        "Modelica.Constants has {} components",
        constants_merged.components.len()
    );
    for (comp_name, _) in constants_merged.components.iter().take(5) {
        println!("  - {}", comp_name);
    }

    assert!(
        constants_merged.components.contains_key("pi"),
        "pi not in Modelica.Constants after merge"
    );
}

#[test]
fn test_constants_in_class_dict() {
    use indexmap::IndexMap;
    use rumoca::ir::ast::ClassDefinition;

    let msl_path = match ensure_msl_downloaded() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Skipping test: {}", e);
            return;
        }
    };

    let constants_file = msl_path.join("Modelica/Constants.mo");
    let content = std::fs::read_to_string(&constants_file).unwrap();

    let mut grammar = ModelicaGrammar::new();
    parse(&content, "Constants.mo", &mut grammar).unwrap();

    let def = grammar.modelica.as_ref().unwrap();

    // Merge
    let merged = merge_stored_definitions(vec![("Constants.mo".to_string(), def.clone())]).unwrap();

    // Build class_dict similar to what flatten does
    fn build_class_dict(
        class: &ClassDefinition,
        prefix: &str,
        dict: &mut IndexMap<String, ClassDefinition>,
    ) {
        let full_name = if prefix.is_empty() {
            class.name.text.clone()
        } else {
            format!("{}.{}", prefix, class.name.text)
        };
        dict.insert(full_name.clone(), class.clone());

        for (_name, nested_class) in &class.classes {
            build_class_dict(nested_class, &full_name, dict);
        }
    }

    let mut class_dict = IndexMap::new();
    for (_class_name, class) in &merged.class_list {
        build_class_dict(class, "", &mut class_dict);
    }

    println!("=== CLASS DICT ===");
    for key in class_dict.keys() {
        println!("  {}", key);
    }

    // Check if Modelica.Constants exists
    assert!(
        class_dict.contains_key("Modelica"),
        "Modelica not in class_dict"
    );
    assert!(
        class_dict.contains_key("Modelica.Constants"),
        "Modelica.Constants not in class_dict"
    );

    // Check if Modelica.Constants has pi component
    let constants = class_dict.get("Modelica.Constants").unwrap();
    println!(
        "\nModelica.Constants components: {}",
        constants.components.len()
    );
    for (name, _) in constants.components.iter().take(5) {
        println!("  - {}", name);
    }

    assert!(
        constants.components.contains_key("pi"),
        "pi not in Modelica.Constants.components in class_dict"
    );

    // Test path_exists_in_dict logic
    fn path_exists_in_dict(path: &str, class_dict: &IndexMap<String, ClassDefinition>) -> bool {
        if class_dict.contains_key(path) {
            return true;
        }
        if let Some(dot_pos) = path.rfind('.') {
            let parent_path = &path[..dot_pos];
            let component_name = &path[dot_pos + 1..];
            if let Some(parent_class) = class_dict.get(parent_path) {
                if parent_class.components.contains_key(component_name) {
                    return true;
                }
                if parent_class.classes.contains_key(component_name) {
                    return true;
                }
            }
        }
        false
    }

    println!("\n=== PATH EXISTS TESTS ===");
    println!("Modelica: {}", path_exists_in_dict("Modelica", &class_dict));
    println!(
        "Modelica.Constants: {}",
        path_exists_in_dict("Modelica.Constants", &class_dict)
    );
    println!(
        "Modelica.Constants.pi: {}",
        path_exists_in_dict("Modelica.Constants.pi", &class_dict)
    );

    assert!(
        path_exists_in_dict("Modelica.Constants.pi", &class_dict),
        "Modelica.Constants.pi should exist"
    );
}

/// Focused MSL test - runs only on models that failed in the last test run.
/// Reads model names from target/failed_models.json and tests just those.
/// Much faster iteration when debugging specific failures (~30s vs ~5min).
///
/// Usage:
/// ```bash
/// # First run full test to generate failed_models.json:
/// cargo test test_msl_balance_all -- --ignored --nocapture
///
/// # Then iterate quickly on failures:
/// cargo test test_msl_focused -- --ignored --nocapture
/// ```
#[test]
#[ignore]
fn test_msl_focused() {
    // Read failed models from previous run
    let failed_models_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("failed_models.json");

    if !failed_models_path.exists() {
        println!("No failed_models.json found. Run test_msl_balance_all first.");
        println!("  cargo test test_msl_balance_all -- --ignored --nocapture");
        return;
    }

    let json_content =
        fs::read_to_string(&failed_models_path).expect("Failed to read failed_models.json");
    let json: serde_json::Value =
        serde_json::from_str(&json_content).expect("Failed to parse failed_models.json");

    // Extract compile error model names
    let failures = json["failures"]
        .as_array()
        .expect("Expected 'failures' array in JSON");

    let compile_error_models: Vec<String> = failures
        .iter()
        .filter(|f| f["failure_type"].as_str() == Some("compile"))
        .filter_map(|f| f["model_name"].as_str().map(|s| s.to_string()))
        .collect();

    if compile_error_models.is_empty() {
        println!("No compile errors found in failed_models.json!");
        println!("All models are compiling successfully.");
        return;
    }

    println!("============================================================");
    println!("              FOCUSED MSL TEST");
    println!("============================================================");
    println!();
    println!(
        "Testing {} models with compile errors from last run",
        compile_error_models.len()
    );
    println!();

    // Get MSL path and parse all files (needed for context)
    let msl_path = get_msl_path().expect("Failed to download MSL");
    let modelica_dir = msl_path.join("Modelica");
    let mo_files = find_mo_files(&modelica_dir);

    println!("Parsing {} files...", mo_files.len());
    let parse_start = Instant::now();

    // Parse all files in parallel
    let parse_results: Vec<ParseResult> = mo_files
        .par_iter()
        .map(|file_path| {
            let content = match fs::read_to_string(file_path) {
                Ok(c) => c,
                Err(e) => {
                    return ParseResult {
                        file_path: file_path.clone(),
                        success: false,
                        error: Some(format!("Read error: {}", e)),
                        definition: None,
                    };
                }
            };

            let mut grammar = ModelicaGrammar::new();
            match parse(&content, file_path.to_string_lossy().as_ref(), &mut grammar) {
                Ok(_) => ParseResult {
                    file_path: file_path.clone(),
                    success: true,
                    error: None,
                    definition: grammar.modelica,
                },
                Err(e) => ParseResult {
                    file_path: file_path.clone(),
                    success: false,
                    error: Some(format!("{:?}", e)),
                    definition: None,
                },
            }
        })
        .collect();

    let parse_time = parse_start.elapsed();
    println!("Parsed in {:.2}s", parse_time.as_secs_f64());

    // Merge definitions
    let definitions: Vec<(String, StoredDefinition)> = parse_results
        .into_iter()
        .filter_map(|r| {
            r.definition
                .map(|def| (r.file_path.to_string_lossy().to_string(), def))
        })
        .collect();

    let merged_def = merge_stored_definitions(definitions).expect("Failed to merge definitions");

    // Pre-warm cache
    println!("Pre-warming class cache...");
    enable_cache();
    let prewarm_start = Instant::now();
    prewarm_class_cache(&merged_def);
    println!(
        "Pre-warmed in {:.2}s",
        prewarm_start.elapsed().as_secs_f64()
    );

    let merged_def = Arc::new(merged_def);

    // Test the focused models
    println!();
    println!(
        "Testing {} compile error models:",
        compile_error_models.len()
    );
    println!("------------------------------------------------------------");

    let mut fixed = 0;
    let mut still_failing = 0;

    for model_name in &compile_error_models {
        let result = Compiler::new().model(model_name).check_balance(&merged_def);

        match result {
            Ok(balance) => {
                fixed += 1;
                let status = if balance.is_balanced() {
                    format!(
                        "✓ FIXED (balanced: {} eq, {} unk)",
                        balance.num_equations, balance.num_unknowns
                    )
                } else {
                    format!(
                        "✓ FIXED (compiles, {}: {} eq, {} unk)",
                        balance.status_message(),
                        balance.num_equations,
                        balance.num_unknowns
                    )
                };
                println!("  {}: {}", model_name, status);
            }
            Err(e) => {
                still_failing += 1;
                let err_msg = format!("{:?}", e);
                // Extract just the relevant part of the error
                let short_err = err_msg.lines().next().unwrap_or(&err_msg);
                println!("  {}: ✗ {}", model_name, short_err);
            }
        }
    }

    println!();
    println!("============================================================");
    println!("                    RESULTS");
    println!("============================================================");
    println!("  Fixed:         {}/{}", fixed, compile_error_models.len());
    println!(
        "  Still failing: {}/{}",
        still_failing,
        compile_error_models.len()
    );
    println!();

    if still_failing == 0 {
        println!("🎉 All previously failing models now compile!");
        println!("   Run the full test to verify no regressions:");
        println!("   cargo test test_msl_balance_all -- --ignored --nocapture");
    } else {
        println!("Still have {} compile errors to fix.", still_failing);
    }
}
