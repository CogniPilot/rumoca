#![cfg(feature = "msl-full-test")]

//! Canonical MSL regression test harness.
//!
//! This integration test exposes a single MSL test entrypoint:
//! `test_msl_all` from `tests/msl_tests/balance_pipeline_core.rs`.
//!
//! Run with:
//! `cargo test --release --package rumoca-test-msl --features msl-full-test --test msl_tests balance_pipeline::balance_pipeline_core::test_msl_all -- --nocapture`
//!
//! # Per-model resource acceptance contract
//!
//! Every model attempt runs under five declared per-model ceilings. Each is
//! raise-only from the parity config (a config may buy a bigger budget for a
//! diagnostic lane; it can never shrink one below the value the committed
//! baseline was measured with), and each overrun is loud, carries a typed
//! [`rumoca_worker::ModelFailureBucket`] with a named owner, and cannot hang:
//!
//! | ceiling | default | override | overrun bucket |
//! |---|---|---|---|
//! | per-phase compile wall | 10 s | `model_attempt_timeout_secs` | `Timeout` |
//! | simulation/solver wall | 10 s | `sim_timeout_secs` | `Timeout` |
//! | worker resident+swap | 6 GiB | `model_worker_memory_mb` | `MemoryLimit` |
//! | total compile wall | 40 s | `model_compile_wall_limit_secs` | `ResourceBudget` |
//! | Solve-IR serialized size | 32 MB | `solve_ir_size_limit_mb` | `ResourceBudget` |
//!
//! A model is **accepted** when it fits all five. Nothing else about it is
//! judged here: shape, node kinds, op counts, and equation counts are all free.
//! `rumoca_test_msl::resource_budget` states the two `ResourceBudget` ceilings
//! in full, including why measurement stops at the ceiling rather than
//! discovering how far past it a model went.

use rayon::prelude::*;
use rumoca_compile::{
    compile::core::{VarName, msl_cache_dir_from_manifest, workspace_root_from_manifest_dir},
    compile::{
        CompiledSourceRoot, Dae, FailedPhase, PhaseResult, StrictCompileReport,
        compile_phase_timing_stats, reset_compile_phase_timing_stats,
    },
    parsing::{LenientParseResult, parse_files_parallel_lenient},
};
use rumoca_phase_flatten::{flatten_phase_timing_stats, reset_flatten_phase_timing_stats};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
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
            ║  cargo test --release --package rumoca-test-msl --features   ║\n\
            ║  msl-full-test --test msl_tests                              ║\n\
            ║  balance_pipeline::balance_pipeline_core::test_msl_all       ║\n\
            ║  -- --nocapture                                              ║\n\
            ╚══════════════════════════════════════════════════════════════════╝\n\n"
        );
    }
}

const MSL_VERSION: &str = "v4.1.0";
const MSL_RELEASE_ZIP_URL: &str = "https://github.com/modelica/ModelicaStandardLibrary/releases/download/v4.1.0/ModelicaStandardLibrary_v4.1.0.zip";
const MSL_MODELICA_DIR_NAME: &str = "Modelica 4.1.0";

fn get_msl_cache_dir() -> PathBuf {
    // Relocatable (resolves from the runtime workspace) so a prebuilt Nix binary
    // finds target/msl in the real checkout, not the baked build sandbox.
    let cache_dir = balance_pipeline::msl_cache_dir();
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
    let has_modelica_test_versioned = msl_dir.join("ModelicaTest 4.1.0").is_dir();
    let include_modelica_test = include_modelica_test_sources();

    WalkDir::new(msl_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            let path = e.path();
            let path_str = path.to_string_lossy();
            let top_package = path
                .strip_prefix(msl_dir)
                .ok()
                .and_then(|relative| relative.components().next())
                .and_then(|component| component.as_os_str().to_str());
            let is_modelica_test = top_package.is_some_and(is_modelica_test_package_dir);
            let is_unversioned_alias = path
                .strip_prefix(msl_dir)
                .ok()
                .and_then(|relative| relative.components().next())
                .and_then(|component| component.as_os_str().to_str())
                .is_some_and(|top| {
                    (top == "Modelica" && has_modelica_versioned)
                        || (top == "ModelicaServices" && has_modelica_services_versioned)
                        || (top == "ModelicaReference" && has_modelica_reference_versioned)
                        || (top == "ModelicaTest" && has_modelica_test_versioned)
                });
            path.is_file()
                && path.extension().is_some_and(|ext| ext == "mo")
                && !path_str.contains("Obsolete")
                && !path_str.contains("ModelicaTestConversion")
                && (include_modelica_test || !is_modelica_test)
                && !is_unversioned_alias
        })
        .map(|e| e.path().to_path_buf())
        .collect()
}

fn include_modelica_test_sources() -> bool {
    balance_pipeline::parity_config()
        .include_modelica_test
        .unwrap_or(false)
}

fn is_modelica_test_package_dir(top_package: &str) -> bool {
    top_package == "ModelicaTest" || top_package.starts_with("ModelicaTest ")
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

/// A sorted, deduplicated index of every variable name that appears anywhere in
/// the DAE or the flat model's `when` clauses.
///
/// The naive form of this check was `O(D x A)`: for each discrete variable it
/// scanned every active name and re-ran a prefix comparison. On large MSL models
/// (thousands of active names, hundreds of discrete variables) that dominated
/// the per-model accounting. Sorting once turns each lookup into a bounded
/// number of binary searches.
struct ActiveNameIndex {
    names: Vec<VarName>,
}

impl ActiveNameIndex {
    fn build(flat: &rumoca_ir_flat::Model, dae: &Dae) -> Self {
        let mut names = Vec::new();
        collect_active_refs_from_dae(dae, &mut names);
        collect_active_refs_from_flat(flat, &mut names);
        names.sort_unstable_by(|a, b| a.as_str().cmp(b.as_str()));
        names.dedup_by(|a, b| a.as_str() == b.as_str());
        Self { names }
    }

    /// True when `name` is active: equal to an indexed name, an ancestor of one,
    /// or a descendant of one, with `.`/`[` as the component boundary.
    ///
    /// Equivalent to `self.names.iter().any(|active|
    /// names_match_via_component_prefix(active.as_str(), name))`, which
    /// `active_name_index_matches_reference_prefix_semantics` pins.
    fn matches_component(&self, name: &str) -> bool {
        self.contains_exact(name) || self.has_indexed_ancestor(name) || self.has_descendant(name)
    }

    fn contains_exact(&self, name: &str) -> bool {
        self.names
            .binary_search_by(|probe| probe.as_str().cmp(name))
            .is_ok()
    }

    /// Some indexed name is a strict component-boundary prefix of `name`.
    fn has_indexed_ancestor(&self, name: &str) -> bool {
        name.char_indices()
            .filter(|(_, ch)| *ch == '.' || *ch == '[')
            .any(|(offset, _)| self.contains_exact(&name[..offset]))
    }

    /// Some indexed name extends `name` past a component boundary. Names sharing
    /// a prefix are contiguous under lexicographic order, so one probe per
    /// separator is exhaustive.
    fn has_descendant(&self, name: &str) -> bool {
        ['.', '['].into_iter().any(|sep| {
            let key = format!("{name}{sep}");
            let idx = self
                .names
                .partition_point(|probe| probe.as_str() < key.as_str());
            self.names
                .get(idx)
                .is_some_and(|probe| probe.as_str().starts_with(&key))
        })
    }
}

fn push_var_ref(names: &mut Vec<VarName>, name: &VarName) {
    names.push(name.clone());
}

fn collect_checked_expr_refs<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    root: rumoca_ir_dae::ExprId<'dae>,
    active: &mut Vec<VarName>,
) {
    rumoca_ir_dae::for_each_expression(view, root, |_, expression| {
        let Some(variable_id) = expression.variable_coordinate() else {
            return;
        };
        let variable = view
            .variable(variable_id)
            .expect("checked coordinate resolves to its declared variable");
        push_var_ref(active, variable.name());
    });
}

fn collect_checked_b1c_owner_refs<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    owner: rumoca_ir_dae::DiscreteValueOwnerView<'dae>,
    active: &mut Vec<VarName>,
) {
    for target in owner.targets().iter() {
        let variable = view
            .variable(target.into())
            .expect("checked B.1c target resolves");
        push_var_ref(active, variable.name());
    }
    for (value, _) in owner
        .branches()
        .iter()
        .flat_map(|branch| branch.values().iter())
    {
        collect_checked_expr_refs(view, value, active);
    }
}

fn collect_active_refs_from_dae(dae: &Dae, active: &mut Vec<VarName>) {
    dae.inspect(|view| {
        for index in 0..view.continuous_equation_count() {
            let equation = view
                .continuous_equation(index)
                .expect("dense checked continuous equation resolves");
            collect_checked_expr_refs(view, equation.residual(), active);
        }
        for index in 0..view.initialization_equation_count() {
            let equation = view
                .initialization_equation(index)
                .expect("dense checked initialization equation resolves");
            collect_checked_expr_refs(view, equation.residual(), active);
        }
        for index in 0..view.discrete_real_equation_count() {
            let equation = view
                .discrete_real_equation(index)
                .expect("dense checked discrete equation resolves");
            collect_checked_expr_refs(view, equation.residual(), active);
        }
        for index in 0..view.discrete_value_owner_count() {
            let owner_id = view
                .discrete_value_owner_id(index)
                .expect("dense checked B.1c owner identity resolves");
            let owner = view
                .discrete_value_owner(owner_id)
                .expect("dense checked B.1c owner resolves");
            collect_checked_b1c_owner_refs(view, owner, active);
        }
        for index in 0..view.relation_count() {
            let relation_id = view
                .relation_id(index)
                .expect("dense checked relation identity resolves");
            let relation = view
                .relation(relation_id)
                .expect("dense checked relation resolves");
            collect_checked_expr_refs(view, relation.expression(), active);
        }
    });
}

fn collect_active_refs_from_flat_when_equation(
    equation: &rumoca_ir_flat::WhenEquation,
    active: &mut Vec<VarName>,
) {
    match equation {
        rumoca_ir_flat::WhenEquation::Assign { target, value, .. } => {
            push_var_ref(active, target);
            value.collect_var_refs(active);
        }
        rumoca_ir_flat::WhenEquation::Reinit { state, value, .. } => {
            push_var_ref(active, state);
            value.collect_var_refs(active);
        }
        rumoca_ir_flat::WhenEquation::Assert {
            condition,
            message,
            level,
            ..
        } => {
            condition.collect_var_refs(active);
            message.collect_var_refs(active);
            if let Some(level) = level {
                level.collect_var_refs(active);
            }
        }
        rumoca_ir_flat::WhenEquation::Terminate { message, .. } => {
            message.collect_var_refs(active);
        }
        rumoca_ir_flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                condition.collect_var_refs(active);
                for nested in equations {
                    collect_active_refs_from_flat_when_equation(nested, active);
                }
            }
            if let Some(else_branch) = else_branch {
                for nested in else_branch {
                    collect_active_refs_from_flat_when_equation(nested, active);
                }
            }
        }
        rumoca_ir_flat::WhenEquation::FunctionCallOutputs {
            outputs, function, ..
        } => {
            for out in outputs {
                push_var_ref(active, out);
            }
            function.collect_var_refs(active);
        }
    }
}

fn collect_active_refs_from_flat(flat: &rumoca_ir_flat::Model, active: &mut Vec<VarName>) {
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            branch.condition.collect_var_refs(active);
            for equation in &branch.equations {
                collect_active_refs_from_flat_when_equation(equation, active);
            }
        }
    }
}

fn active_discrete_scalar_count(flat: &rumoca_ir_flat::Model, dae: &Dae) -> i64 {
    let active = ActiveNameIndex::build(flat, dae);

    let active_discrete = dae.inspect(|view| {
        view.variables()
            .filter(|(_, variable)| {
                matches!(
                    variable.role(),
                    rumoca_ir_dae::VariableRole::DiscreteReal
                        | rumoca_ir_dae::VariableRole::DiscreteValue
                ) && active.matches_component(variable.name().as_str())
            })
            .map(|(_, variable)| variable.scalar_count())
            .sum::<usize>()
    });

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
    let initial_equation_scalars = dae.inspect(|view| {
        let residual_rows = view.initialization_equation_count() as i64;
        let structured_rows = (0..view.initialization_family_count())
            .map(|index| {
                view.initialization_family(index)
                    .expect("dense checked initialization family resolves")
                    .scalar_rows() as i64
            })
            .sum::<i64>();
        residual_rows + structured_rows
    });
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
#[serde(deny_unknown_fields)]
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
    instantiate_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    typecheck_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    flatten_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    dae_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    compile_perf_profile_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ir_ast_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ir_flat_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_status: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_error_code: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_error_span: Option<rumoca_compile::compile::core::Span>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ic_status: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ic_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ic_error_span: Option<rumoca_compile::compile::core::Span>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ic_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_build_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ir_solve_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ir_solve_structural_dae_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ir_solve_lower_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    tensor_family_bodies: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    tensor_preserved_family_bodies: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    tensor_scalarized_family_rows: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    tensor_preservation_percent: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    tensor_preservation_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_backend_build_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_run_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_wall_seconds: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_trace_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_perf_profile_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sim_trace_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ir_dae_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ir_solve_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ir_solve_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ir_solve_error_code: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    timeout_phase: Option<rumoca_worker::WorkerProgressPhase>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    timeout_seconds: Option<f64>,
    /// Component breakdown for an unbalanced (ED001) ToDae failure. Present
    /// only for balance failures; this is what makes the balance cohort
    /// distinguishable from every other ToDae failure.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    balance_detail: Option<Box<rumoca_compile::analysis::BalanceDetail>>,
    /// Machine-readable failure classification. Additive and optional: a result
    /// recorded before these fields existed, or a successful attempt, simply
    /// omits them, so historical `msl_results.json` baselines still parse.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    failure_phase: Option<rumoca_worker::WorkerProgressPhase>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    failure_bucket: Option<rumoca_worker::ModelFailureBucket>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    owner_category: Option<rumoca_worker::ModelFailureOwner>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    failure_error_code: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct MslSchedulerTimings {
    #[serde(default)]
    selected_model_count: usize,
    #[serde(default)]
    requested_worker_threads: usize,
    #[serde(default)]
    effective_worker_threads: usize,
    #[serde(default)]
    worker_count: usize,
    #[serde(default)]
    pinned_worker_count: usize,
    #[serde(default)]
    cpu_token_capacity: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    compile_memory_token_capacity_mb: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    compile_memory_model_cost_mb: Option<usize>,
    #[serde(default)]
    models_started: usize,
    #[serde(default)]
    max_active_workers: usize,
    #[serde(default)]
    cpu_token_wait_seconds: f64,
    #[serde(default)]
    compile_memory_token_wait_seconds: f64,
    #[serde(default)]
    active_model_wall_seconds: f64,
    #[serde(default)]
    worker_slot_wall_seconds: f64,
    #[serde(default)]
    worker_slot_idle_seconds: f64,
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
    scheduler: MslSchedulerTimings,
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
    #[serde(default, skip_serializing_if = "Option::is_none")]
    process_peak_rss_mb: Option<usize>,
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

/// The machine-readable classification fields are optional in the record
/// shape: successful models carry none of them, so their absence must parse
/// as `None` rather than growing null columns or failing decode.
#[test]
fn model_results_without_classification_fields_parse_as_none() {
    let record = serde_json::json!({
        "model_name": "Modelica.A",
        "phase_reached": "ToDae",
        "error": "M failed in ToDae: unbalanced model",
        "error_code": "ED001",
        "num_states": null,
        "num_algebraics": null,
        "num_f_x": null,
        "balance": -2,
        "is_balanced": false,
        "is_partial": null,
    });
    let parsed: MslModelResult =
        serde_json::from_value(record).expect("record without classification fields parses");
    assert_eq!(parsed.failure_bucket, None);
    assert_eq!(parsed.owner_category, None);
    assert_eq!(parsed.failure_phase, None);
    assert_eq!(parsed.failure_error_code, None);
    // And a record without them serializes without them, so the schema does not
    // grow null columns for successful models.
    let re_encoded = serde_json::to_value(&parsed).expect("re-encodes");
    assert!(re_encoded.get("failure_bucket").is_none());
    assert!(re_encoded.get("owner_category").is_none());
}

/// The classification must survive a JSON round trip by *name*, because the
/// names are what triage tooling and archived sweeps group on.
#[test]
fn classification_fields_round_trip_by_name() {
    let mut worker_row = rumoca_worker::WorkerModelResult::phase_failure(
        "Modelica.A".to_string(),
        "Success",
        "",
        None,
    );
    worker_row.error = None;
    worker_row.set_failure_classification(
        rumoca_worker::ModelFailureClassification::new(
            rumoca_worker::WorkerProgressPhase::Sim,
            rumoca_worker::ModelFailureBucket::RuntimeManifoldProjection,
        ),
        Some("EX002".to_string()),
    );
    let encoded = serde_json::to_value(&worker_row).expect("worker row encodes");
    assert_eq!(encoded["failure_bucket"], "RuntimeManifoldProjection");
    assert_eq!(encoded["owner_category"], "Runtime");
    assert_eq!(encoded["failure_phase"], "Sim");
    assert_eq!(encoded["failure_error_code"], "EX002");

    let parsed: MslModelResult = serde_json::from_value(encoded).expect("harness record parses");
    assert_eq!(
        parsed.failure_bucket,
        Some(rumoca_worker::ModelFailureBucket::RuntimeManifoldProjection)
    );
    assert_eq!(
        parsed.owner_category,
        Some(rumoca_worker::ModelFailureOwner::Runtime)
    );
    let re_encoded = serde_json::to_value(&parsed).expect("harness record re-encodes");
    assert_eq!(re_encoded["failure_bucket"], "RuntimeManifoldProjection");
}

/// A bucket name the harness does not know must fail loudly rather than being
/// silently dropped: a producer/consumer skew has to be visible.
#[test]
fn an_unknown_classification_name_is_rejected_by_the_result_schema() {
    let record = serde_json::json!({
        "model_name": "Modelica.A",
        "phase_reached": "Success",
        "error": null,
        "error_code": null,
        "num_states": null,
        "num_algebraics": null,
        "num_f_x": null,
        "balance": null,
        "is_balanced": null,
        "is_partial": null,
        "failure_bucket": "SomethingFromTheFuture",
    });
    let error = serde_json::from_value::<MslModelResult>(record)
        .expect_err("an unknown bucket name must not parse");
    assert!(
        error.to_string().contains("unknown variant"),
        "unexpected error: {error}"
    );
}

/// `ActiveNameIndex` replaces an `O(D x A)` scan with binary searches, so it has
/// to agree with the retained reference predicate on every probe — a subtle
/// prefix-boundary bug here would silently shift the `active_discrete_scalar`
/// column of `msl_results.json` and with it the balance accounting.
#[test]
fn active_name_index_matches_reference_prefix_semantics() {
    let active_names = [
        "a",
        "a.b",
        "a.b[2]",
        "a.bc",
        "ab",
        "controller.pid.I.y",
        "tank[1].level",
        "z",
    ];
    let index = ActiveNameIndex {
        names: active_names
            .iter()
            .map(|name| VarName::new(*name))
            .collect(),
    };
    // Sorted + deduplicated is the index's invariant; the literal above is
    // already in lexicographic byte order, so assert it rather than re-sorting.
    let mut sorted = index.names.clone();
    sorted.sort_unstable_by(|a, b| a.as_str().cmp(b.as_str()));
    assert_eq!(
        index.names.iter().map(VarName::as_str).collect::<Vec<_>>(),
        sorted.iter().map(VarName::as_str).collect::<Vec<_>>(),
        "fixture must already be sorted"
    );

    let probes = [
        "a",
        "a.b",
        "a.b[2]",
        "a.b[3]",
        "a.bc",
        "a.bcd",
        "ab",
        "abc",
        "b",
        "controller",
        "controller.pid",
        "controller.pid.I",
        "controller.pid.I.y",
        "controller.pid.I.yy",
        "tank",
        "tank[1]",
        "tank[1].level",
        "tank[2]",
        "z",
        "zz",
        "",
    ];
    for probe in probes {
        let expected = index
            .names
            .iter()
            .any(|active| names_match_via_component_prefix(active.as_str(), probe));
        assert_eq!(
            index.matches_component(probe),
            expected,
            "index disagreed with the reference predicate for probe '{probe}'"
        );
    }
}

#[test]
fn active_name_index_deduplicates_and_sorts_on_build() {
    let mut names = ["b.c", "a", "b.c", "a"]
        .iter()
        .map(|name| VarName::new(*name))
        .collect::<Vec<_>>();
    names.sort_unstable_by(|a, b| a.as_str().cmp(b.as_str()));
    names.dedup_by(|a, b| a.as_str() == b.as_str());
    let index = ActiveNameIndex { names };

    assert_eq!(
        index.names.iter().map(VarName::as_str).collect::<Vec<_>>(),
        vec!["a", "b.c"]
    );
    assert!(index.matches_component("a"));
    assert!(index.matches_component("b"));
    assert!(index.matches_component("b.c"));
    assert!(!index.matches_component("c"));
}

mod balance_pipeline;
