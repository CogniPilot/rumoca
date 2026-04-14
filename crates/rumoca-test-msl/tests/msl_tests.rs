//! Canonical MSL regression test harness.
//!
//! This integration test exposes a single MSL test entrypoint:
//! `test_msl_all` from `tests/msl_tests/balance_pipeline_core.rs`.
//!
//! Run with:
//! `cargo test --release --package rumoca-test-msl --test msl_tests test_msl_all -- --ignored --nocapture`

use rayon::prelude::*;
use rumoca_phase_flatten::{flatten_phase_timing_stats, reset_flatten_phase_timing_stats};
use rumoca_session::{
    compile::core::msl_cache_dir_from_manifest,
    compile::{
        CompiledSourceRoot, Dae, FailedPhase, PhaseResult, StrictCompileReport,
        compile_phase_timing_stats, reset_compile_phase_timing_stats,
    },
    parsing::parse_files_parallel_lenient,
};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::{self, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::time::{Duration, Instant};
use walkdir::WalkDir;
use zip::read::ZipArchive;

fn check_release_mode() {
    #[cfg(debug_assertions)]
    {
        panic!(
            "\n\n\
            ╔══════════════════════════════════════════════════════════════════╗\n\
            ║  ERROR: MSL tests must be run in RELEASE mode!                   ║\n\
            ║                                                                  ║\n\
            ║  Debug mode is 10x+ slower and will take forever.                ║\n\
            ║                                                                  ║\n\
            ║  Please run with --release flag:                                 ║\n\
            ║  cargo test --release --package rumoca-test-msl --test msl_tests -- --ignored --nocapture\n\
            ╚══════════════════════════════════════════════════════════════════╝\n\n"
        );
    }
}

const MSL_VERSION: &str = "v4.1.0";
const MSL_RELEASE_ZIP_URL: &str = "https://github.com/modelica/ModelicaStandardLibrary/releases/download/v4.1.0/ModelicaStandardLibrary_v4.1.0.zip";
const MSL_MODELICA_DIR_NAME: &str = "Modelica 4.1.0";

fn get_msl_cache_dir() -> PathBuf {
    let cache_dir = msl_cache_dir_from_manifest(env!("CARGO_MANIFEST_DIR"));
    fs::create_dir_all(&cache_dir).expect("Failed to create MSL cache directory");
    cache_dir
}

fn get_msl_dir() -> PathBuf {
    get_msl_cache_dir().join(format!("ModelicaStandardLibrary-{}", &MSL_VERSION[1..]))
}

fn msl_cache_layout_valid(msl_dir: &Path) -> bool {
    msl_dir.join("Complex.mo").is_file()
        && msl_dir
            .join(MSL_MODELICA_DIR_NAME)
            .join("package.mo")
            .is_file()
}

fn msl_exists() -> bool {
    let msl_dir = get_msl_dir();
    msl_cache_layout_valid(&msl_dir)
}

fn reset_msl_cache_dir(msl_dir: &Path) -> io::Result<()> {
    if msl_dir.exists() {
        fs::remove_dir_all(msl_dir)?;
    }
    fs::create_dir_all(msl_dir)?;
    Ok(())
}

fn extract_msl_release_zip(data: &[u8], msl_dir: &Path) -> io::Result<()> {
    let cursor = Cursor::new(data);
    let mut archive = ZipArchive::new(cursor)
        .map_err(|error| io::Error::other(format!("Failed to open MSL zip: {error}")))?;

    for index in 0..archive.len() {
        let mut entry = archive
            .by_index(index)
            .map_err(|error| io::Error::other(format!("Failed to read MSL zip entry: {error}")))?;
        let relative_path = entry
            .enclosed_name()
            .ok_or_else(|| io::Error::other(format!("Invalid zip entry path: {}", entry.name())))?;
        let output_path = msl_dir.join(relative_path);

        if entry.is_dir() {
            fs::create_dir_all(&output_path)?;
            continue;
        }

        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let mut output = File::create(&output_path)?;
        io::copy(&mut entry, &mut output)?;
    }

    Ok(())
}

fn ensure_msl_downloaded() -> io::Result<PathBuf> {
    let msl_dir = get_msl_dir();

    if msl_exists() {
        println!("MSL {} already cached at {:?}", MSL_VERSION, msl_dir);
        return Ok(msl_dir);
    }

    println!("Downloading MSL {} from GitHub...", MSL_VERSION);

    let response = ureq::get(MSL_RELEASE_ZIP_URL)
        .call()
        .map_err(|e| io::Error::other(format!("Download failed: {}", e)))?;

    let len: usize = response
        .header("content-length")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    println!("Downloading {} bytes...", len);

    let mut data = Vec::with_capacity(len);
    response
        .into_reader()
        .read_to_end(&mut data)
        .map_err(|e| io::Error::other(format!("Read failed: {}", e)))?;

    println!(
        "Downloaded {} bytes, extracting official release zip...",
        data.len()
    );

    reset_msl_cache_dir(&msl_dir)?;
    extract_msl_release_zip(&data, &msl_dir)?;

    if !msl_cache_layout_valid(&msl_dir) {
        return Err(io::Error::other(format!(
            "Extracted MSL cache at '{}' is invalid: missing Complex.mo or {}",
            msl_dir.display(),
            msl_dir
                .join(MSL_MODELICA_DIR_NAME)
                .join("package.mo")
                .display()
        )));
    }

    println!("Extracted MSL to {:?}", msl_dir);
    Ok(msl_dir)
}

fn find_mo_files(msl_dir: &Path) -> Vec<PathBuf> {
    let has_modelica_versioned = msl_dir.join(MSL_MODELICA_DIR_NAME).is_dir();
    let has_modelica_services_versioned = msl_dir.join("ModelicaServices 4.1.0").is_dir();
    let has_modelica_reference_versioned = msl_dir.join("ModelicaReference 4.1.0").is_dir();

    WalkDir::new(msl_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            let path = e.path();
            let path_str = path.to_string_lossy();
            let is_unversioned_alias = path
                .strip_prefix(msl_dir)
                .ok()
                .and_then(|relative| relative.components().next())
                .and_then(|component| component.as_os_str().to_str())
                .is_some_and(|top| {
                    (top == "Modelica" && has_modelica_versioned)
                        || (top == "ModelicaServices" && has_modelica_services_versioned)
                        || (top == "ModelicaReference" && has_modelica_reference_versioned)
                });
            path.is_file()
                && path.extension().is_some_and(|ext| ext == "mo")
                && !path_str.contains("Obsolete")
                && !path_str.contains("ModelicaTestConversion")
                && !path_str.contains("ModelicaTest/")
                && !is_unversioned_alias
        })
        .map(|e| e.path().to_path_buf())
        .collect()
}

fn has_component_boundary_prefix(candidate: &str, prefix: &str) -> bool {
    candidate
        .strip_prefix(prefix)
        .and_then(|rest| rest.chars().next())
        .is_some_and(|ch| ch == '.' || ch == '[')
}

fn names_match_via_component_prefix(active_name: &str, discrete_name: &str) -> bool {
    active_name == discrete_name
        || has_component_boundary_prefix(discrete_name, active_name)
        || has_component_boundary_prefix(active_name, discrete_name)
}

fn collect_active_refs_from_dae(dae: &Dae, active: &mut HashSet<String>) {
    for eq in &dae.f_x {
        if let Some(lhs) = &eq.lhs {
            active.insert(lhs.as_str().to_string());
        }
        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
    }
    for eq in &dae.initial_equations {
        if let Some(lhs) = &eq.lhs {
            active.insert(lhs.as_str().to_string());
        }
        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
    }
    for eq in &dae.f_z {
        if let Some(lhs) = &eq.lhs {
            active.insert(lhs.as_str().to_string());
        }
        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
    }
    for eq in &dae.f_m {
        if let Some(lhs) = &eq.lhs {
            active.insert(lhs.as_str().to_string());
        }
        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
    }
    for eq in &dae.f_c {
        if let Some(lhs) = &eq.lhs {
            active.insert(lhs.as_str().to_string());
        }
        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
    }
    for relation in &dae.relation {
        let mut refs = HashSet::new();
        relation.collect_var_refs(&mut refs);
        active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
    }
}

fn collect_active_refs_from_flat_when_equation(
    equation: &rumoca_ir_flat::WhenEquation,
    active: &mut HashSet<String>,
) {
    match equation {
        rumoca_ir_flat::WhenEquation::Assign { target, value, .. } => {
            active.insert(target.as_str().to_string());
            let mut refs = HashSet::new();
            value.collect_var_refs(&mut refs);
            active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
        }
        rumoca_ir_flat::WhenEquation::Reinit { state, value, .. } => {
            active.insert(state.as_str().to_string());
            let mut refs = HashSet::new();
            value.collect_var_refs(&mut refs);
            active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
        }
        rumoca_ir_flat::WhenEquation::Assert { condition, .. } => {
            let mut refs = HashSet::new();
            condition.collect_var_refs(&mut refs);
            active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
        }
        rumoca_ir_flat::WhenEquation::Terminate { .. } => {}
        rumoca_ir_flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                let mut refs = HashSet::new();
                condition.collect_var_refs(&mut refs);
                active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
                for nested in equations {
                    collect_active_refs_from_flat_when_equation(nested, active);
                }
            }
            for nested in else_branch {
                collect_active_refs_from_flat_when_equation(nested, active);
            }
        }
        rumoca_ir_flat::WhenEquation::FunctionCallOutputs {
            outputs, function, ..
        } => {
            for out in outputs {
                active.insert(out.as_str().to_string());
            }
            let mut refs = HashSet::new();
            function.collect_var_refs(&mut refs);
            active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
        }
    }
}

fn collect_active_refs_from_flat(flat: &rumoca_ir_flat::Model, active: &mut HashSet<String>) {
    for when in &flat.when_clauses {
        let mut refs = HashSet::new();
        when.condition.collect_var_refs(&mut refs);
        active.extend(refs.into_iter().map(|name| name.as_str().to_string()));
        for equation in &when.equations {
            collect_active_refs_from_flat_when_equation(equation, active);
        }
    }
}

fn active_discrete_scalar_count(flat: &rumoca_ir_flat::Model, dae: &Dae) -> i64 {
    let mut active: HashSet<String> = HashSet::new();
    collect_active_refs_from_dae(dae, &mut active);
    collect_active_refs_from_flat(flat, &mut active);

    let active_discrete = dae
        .discrete_reals
        .iter()
        .filter(|(name, _)| {
            active
                .iter()
                .any(|active_name| names_match_via_component_prefix(active_name, name.as_str()))
        })
        .map(|(_, v)| v.size())
        .sum::<usize>()
        + dae
            .discrete_valued
            .iter()
            .filter(|(name, _)| {
                active
                    .iter()
                    .any(|active_name| names_match_via_component_prefix(active_name, name.as_str()))
            })
            .map(|(_, v)| v.size())
            .sum::<usize>();

    active_discrete as i64
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct InitializationBalanceCheck {
    deficit_before: i64,
    initial_equation_scalars: i64,
    initial_algorithm_scalars: i64,
    closure_used: i64,
    deficit_after: i64,
}

impl InitializationBalanceCheck {
    fn is_balanced(self) -> bool {
        self.deficit_after == 0
    }
}

fn initialization_balance_check(
    dae: &Dae,
    scalar_unknowns: i64,
    scalar_equations: i64,
) -> InitializationBalanceCheck {
    let deficit_before = (scalar_unknowns - scalar_equations).max(0);
    let initial_equation_scalars = dae
        .initial_equations
        .iter()
        .map(|eq| eq.scalar_count as i64)
        .sum::<i64>();
    let initial_algorithm_scalars = 0;
    let initial_available = initial_equation_scalars + initial_algorithm_scalars;
    let closure_used = initial_available.min(deficit_before);
    InitializationBalanceCheck {
        deficit_before,
        initial_equation_scalars,
        initial_algorithm_scalars,
        closure_used,
        deficit_after: deficit_before - closure_used,
    }
}

fn categorize_flatten_error(error: &str) -> &'static str {
    if error.contains("undefined variable") || error.contains("Undefined variable") {
        "UndefinedVariable"
    } else if error.contains("incompatible connector") {
        "IncompatibleConnectors"
    } else if error.contains("flow variable not found") {
        "MissingFlowVariable"
    } else if error.contains("unsupported equation") {
        "UnsupportedEquation"
    } else if error.contains("internal") {
        "InternalError"
    } else {
        "Other"
    }
}

fn truncate_error(error: &str, max_len: usize) -> String {
    if error.len() > max_len {
        format!("{}...", &error[..max_len])
    } else {
        error.to_string()
    }
}

fn extract_undefined_var(error: &str) -> Option<String> {
    let patterns = ["undefined variable: ", "Undefined variable: "];
    for pat in patterns {
        if let Some(start) = error.find(pat) {
            let rest = &error[start + pat.len()..];
            let end = rest
                .find(|c: char| c.is_whitespace() || c == '\n')
                .unwrap_or(rest.len());
            return Some(rest[..end].to_string());
        }
    }
    None
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct MslModelResult {
    model_name: String,
    phase_reached: String,
    error: Option<String>,
    error_code: Option<String>,
    num_states: Option<usize>,
    num_algebraics: Option<usize>,
    num_f_x: Option<usize>,
    balance: Option<i64>,
    is_balanced: Option<bool>,
    is_partial: Option<bool>,
    #[serde(default)]
    class_type: Option<String>,
    #[serde(default)]
    scalar_equations: Option<usize>,
    #[serde(default)]
    scalar_unknowns: Option<usize>,
    #[serde(default)]
    initial_equation_scalars: Option<usize>,
    #[serde(default)]
    initial_algorithm_scalars: Option<usize>,
    #[serde(default)]
    initial_balance_deficit_before: Option<i64>,
    #[serde(default)]
    initial_closure_used: Option<usize>,
    #[serde(default)]
    initial_balance_deficit_after: Option<i64>,
    #[serde(default)]
    initial_balance_ok: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    compile_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_status: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_build_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_run_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_wall_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_trace_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_trace_error: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct MslPhaseTimings {
    parse_seconds: f64,
    session_build_seconds: f64,
    frontend_compile_seconds: f64,
    compile_seconds: f64,
    #[serde(default)]
    compile_batch_size: usize,
    #[serde(default)]
    compile_chunk_count: usize,
    #[serde(default)]
    worker_threads: usize,
    #[serde(default)]
    compile_instantiate_seconds: f64,
    #[serde(default)]
    compile_typecheck_seconds: f64,
    #[serde(default)]
    compile_flatten_seconds: f64,
    #[serde(default)]
    compile_todae_seconds: f64,
    #[serde(default)]
    compile_instantiate_calls: u64,
    #[serde(default)]
    compile_typecheck_calls: u64,
    #[serde(default)]
    compile_flatten_calls: u64,
    #[serde(default)]
    compile_todae_calls: u64,
    #[serde(default)]
    flatten_connections_seconds: f64,
    #[serde(default)]
    flatten_connections_calls: u64,
    #[serde(default)]
    flatten_eval_fallback_seconds: f64,
    #[serde(default)]
    flatten_eval_fallback_calls: u64,
    render_and_write_seconds: f64,
    summarize_seconds: f64,
    core_pipeline_seconds: f64,
}

#[test]
fn msl_cache_layout_valid_requires_complex_and_modelica_package() {
    let temp = tempfile::tempdir().expect("tempdir");
    let cache_root = temp.path().join("ModelicaStandardLibrary-4.1.0");
    fs::create_dir_all(&cache_root).expect("cache root");

    assert!(
        !msl_cache_layout_valid(&cache_root),
        "empty cache root must be rejected"
    );

    fs::write(
        cache_root.join("Complex.mo"),
        "within; encapsulated package Complex end Complex;",
    )
    .expect("complex package");
    assert!(
        !msl_cache_layout_valid(&cache_root),
        "Complex.mo alone must not mark the cache valid"
    );

    let modelica_dir = cache_root.join(MSL_MODELICA_DIR_NAME);
    fs::create_dir_all(&modelica_dir).expect("Modelica dir");
    fs::write(
        modelica_dir.join("package.mo"),
        "within; package Modelica end Modelica;",
    )
    .expect("Modelica package");

    assert!(
        msl_cache_layout_valid(&cache_root),
        "cache root with Complex.mo and Modelica package must be accepted"
    );
}

mod balance_pipeline;
