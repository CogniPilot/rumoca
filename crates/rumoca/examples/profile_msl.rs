//! Profile MSL compilation for performance analysis.
//!
//! Run with: cargo flamegraph --example profile_msl
//! Or: cargo build --release --example profile_msl && ./target/release/examples/profile_msl

use std::path::PathBuf;
use std::time::Instant;

use rumoca_core::msl_cache_dir_from_manifest;
use rumoca_session::compile::{PhaseResult, Session, SessionConfig};
use rumoca_session::parsing::parse_files_parallel_lenient;
use walkdir::WalkDir;

const MSL_VERSION: &str = "v4.1.0";

fn get_msl_cache_dir() -> PathBuf {
    msl_cache_dir_from_manifest(env!("CARGO_MANIFEST_DIR"))
}

fn get_msl_dir() -> PathBuf {
    get_msl_cache_dir().join(format!("ModelicaStandardLibrary-{}", &MSL_VERSION[1..]))
}

fn find_mo_files(msl_dir: &std::path::Path) -> Vec<PathBuf> {
    WalkDir::new(msl_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            let path = e.path();
            path.is_file()
                && path.extension().is_some_and(|ext| ext == "mo")
                && !path.to_string_lossy().contains("Obsolete")
                && !path.to_string_lossy().contains("ModelicaTestConversion")
        })
        .map(|e| e.path().to_path_buf())
        .collect()
}

fn main() {
    let msl_dir = get_msl_dir();
    if !msl_dir.exists() {
        eprintln!("MSL not found at {:?}", msl_dir);
        eprintln!("Run the MSL download test first");
        return;
    }

    // Parse MSL in parallel
    println!("Parsing MSL...");
    let start = Instant::now();

    let mo_files = find_mo_files(&msl_dir);
    let (successes, _failures) = parse_files_parallel_lenient(&mo_files);
    println!("Parsed {} files in {:?}", successes.len(), start.elapsed());

    // Create session and build resolved tree
    println!("Creating session...");
    let start = Instant::now();
    let mut session = Session::new(SessionConfig { parallel: true });
    session.add_parsed_batch(successes);
    let model_names = session.model_names().expect("Failed to get model names");
    println!(
        "Session ready in {:?}, found {} models",
        start.elapsed(),
        model_names.len()
    );

    // Compile ALL models in parallel
    println!(
        "\nCompiling all {} models in parallel...",
        model_names.len()
    );
    let start = Instant::now();

    let results = session
        .compile_all_parallel()
        .expect("Failed to compile models");

    let elapsed = start.elapsed();
    let success_count = results
        .iter()
        .filter(|(_, r)| matches!(r, PhaseResult::Success(_)))
        .count();

    println!(
        "Compiled {} models ({} success) in {:.2}s ({:.0} models/sec)",
        results.len(),
        success_count,
        elapsed.as_secs_f64(),
        results.len() as f64 / elapsed.as_secs_f64()
    );
}
