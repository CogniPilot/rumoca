// PyO3 macro expansion currently emits Rust 2024 `unsafe_op_in_unsafe_fn` patterns.
#![expect(
    unsafe_op_in_unsafe_fn,
    reason = "PyO3 macro expansion emits Rust 2024 unsafe_op_in_unsafe_fn patterns"
)]

//! Python bindings for the Rumoca Modelica compiler.
//!
//! The public Python API is a first-class, typed surface: `rumoca.load` /
//! `rumoca.loads` return a [`model::Model`] hub whose metadata, IR, structure,
//! codegen, and simulation all hang off typed PyO3 objects — never JSON. JSON
//! appears only when explicitly requested via `.to_json()` / `.to_dict()`.
//!
//! The live object graph (`Model`, `VarView`, `Result`, ...) is backed directly
//! by the Rust compiler/solver types; this module holds the shared compile
//! plumbing the typed classes reuse.

use ::rumoca::{CompilationResult as HighLevelCompilationResult, TemplateIr};
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList};
use pyo3::{PyErr, exceptions::PyRuntimeError};
use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetBundle, TargetManifest, TargetTemplateIr, render_dae_target_files,
    validate_dae_target_capabilities,
};
use rumoca_compile::compile::{FailedPhase, PhaseResult, Session, SourceRootKind};
use rumoca_compile::parsing::{
    collect_compile_unit_source_files, collect_model_names, validate_source_syntax,
};
use rumoca_compile::scenario::{EffectiveSimulationConfig, ScenarioConfig, ScenarioTask};
use rumoca_compile::source_roots::{
    canonical_path_key, merge_source_root_paths, plan_source_root_loads,
    referenced_unloaded_source_root_paths, source_root_source_set_key,
};
use rumoca_compile::workspace::WorkspaceConfig;
use rumoca_tool_fmt::FormatOptions;
use rumoca_tool_lint::{LintLevel, LintOptions, lint as lint_source};
use serde_json::Value;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

pub mod codegen;
pub mod diagnostics;
pub mod error;
pub mod metadata;
pub mod model;
pub mod result;
pub mod session;
pub mod targets;
#[cfg(test)]
mod tests;

use diagnostics::Diagnostic;

/// Internal error carrier; converts to a Python `RuntimeError`. Public only so
/// it can appear in the signatures of pyo3-exported functions.
#[derive(Debug)]
pub struct PyRuntimeStringError(pub String);

impl From<PyRuntimeStringError> for PyErr {
    fn from(value: PyRuntimeStringError) -> Self {
        PyRuntimeError::new_err(value.0)
    }
}

/// Build a "not found, did you mean / available" message for an unknown name —
/// the §1 "errors that teach" promise, shared by every name-resolution path.
pub(crate) fn unknown_name_message(kind: &str, name: &str, candidates: &[String]) -> String {
    match closest_name(name, candidates) {
        Some(suggestion) => format!("unknown {kind} {name:?}; did you mean {suggestion:?}?"),
        None => {
            let preview: Vec<&str> = candidates.iter().take(12).map(String::as_str).collect();
            let ellipsis = if candidates.len() > 12 { ", ..." } else { "" };
            if preview.is_empty() {
                format!("unknown {kind} {name:?}; this model has none")
            } else {
                format!(
                    "unknown {kind} {name:?}; available: {}{ellipsis}",
                    preview.join(", ")
                )
            }
        }
    }
}

/// The candidate closest to `name` by edit distance, if within a small,
/// length-scaled threshold (so suggestions stay relevant, not noise).
///
/// The threshold scales with the *longer* of the two names (`ceil(len/3)`, floor
/// 1), so a real typo like `ms`→`mass` (distance 2) is offered while a two-edit
/// jump onto a one-character name like `qq`→`x` is not.
pub(crate) fn closest_name(name: &str, candidates: &[String]) -> Option<String> {
    let name_len = name.chars().count();
    candidates
        .iter()
        .map(|candidate| (levenshtein(name, candidate), candidate))
        .filter(|(distance, candidate)| {
            let max_len = name_len.max(candidate.chars().count());
            let threshold = max_len.div_ceil(3).max(1);
            *distance <= threshold
        })
        .min_by_key(|(distance, _)| *distance)
        .map(|(_, candidate)| candidate.clone())
}

/// Standard Levenshtein edit distance (two-row dynamic programming).
fn levenshtein(a: &str, b: &str) -> usize {
    let b_chars: Vec<char> = b.chars().collect();
    let mut prev: Vec<usize> = (0..=b_chars.len()).collect();
    let mut curr = vec![0usize; b_chars.len() + 1];
    for (i, a_char) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, &b_char) in b_chars.iter().enumerate() {
            let cost = usize::from(a_char != b_char);
            curr[j + 1] = (prev[j] + cost).min(prev[j + 1] + 1).min(curr[j] + 1);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[b_chars.len()]
}

/// Get the Rumoca version.
#[pyfunction]
fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

/// Format Modelica source code.
#[pyfunction(name = "format")]
#[pyo3(signature = (source, *, filename=None))]
fn format_source(source: &str, filename: Option<&str>) -> Result<String, PyRuntimeStringError> {
    let filename = filename.unwrap_or("input.mo");
    let options = FormatOptions::default();
    rumoca_tool_fmt::format_with_source_name(source, &options, filename)
        .map_err(|e| PyRuntimeStringError(format!("Format error: {e}")))
}

/// Runtime-discovered codegen targets.
#[pyfunction(name = "targets")]
fn targets_fn() -> Vec<targets::Target> {
    targets::list_targets()
}

/// Solvers available in this build (feature-gated).
#[pyfunction(name = "solvers")]
fn solvers_fn() -> Vec<targets::SolverInfo> {
    targets::list_solvers()
}

// ── shared compile plumbing (reused by `model`/`session`) ───────────────────

pub(crate) fn json_value_to_py(py: Python<'_>, value: &Value) -> PyObject {
    match value {
        Value::Null => py.None(),
        Value::Bool(value) => value.into_py(py),
        Value::Number(value) => json_number_to_py(py, value),
        Value::String(value) => value.into_py(py),
        Value::Array(values) => {
            let items = values
                .iter()
                .map(|value| json_value_to_py(py, value))
                .collect::<Vec<_>>();
            PyList::new_bound(py, items).into_py(py)
        }
        Value::Object(values) => {
            let dict = PyDict::new_bound(py);
            for (key, value) in values {
                dict.set_item(key, json_value_to_py(py, value))
                    .expect("serde_json object keys and values convert to Python");
            }
            dict.into_py(py)
        }
    }
}

fn json_number_to_py(py: Python<'_>, value: &serde_json::Number) -> PyObject {
    if let Some(value) = value.as_i64() {
        return value.into_py(py);
    }
    if let Some(value) = value.as_u64() {
        return value.into_py(py);
    }
    if let Some(value) = value.as_f64() {
        return value.into_py(py);
    }
    py.None()
}

/// Build the structured diagnostics for a source string (syntax error first,
/// then lints). Never raises.
pub(crate) fn diagnostics_for_source(source: &str, filename: &str) -> Vec<Diagnostic> {
    if let Err(e) = validate_source_syntax(source, filename) {
        return vec![Diagnostic::syntax(e.to_string(), filename.to_string())];
    }
    let options = LintOptions::default();
    lint_source(source, filename, &options)
        .into_iter()
        .map(|m| Diagnostic {
            rule: Some(m.rule.to_string()),
            level: lint_level_str(m.level).to_string(),
            message: m.message,
            file: Some(m.file),
            line: Some(m.line),
            column: Some(m.column),
            suggestion: m.suggestion,
        })
        .collect()
}

fn lint_level_str(level: LintLevel) -> &'static str {
    match level {
        LintLevel::Error => "error",
        LintLevel::Warning => "warning",
        LintLevel::Note => "note",
        LintLevel::Help => "help",
    }
}

pub(crate) fn project_configured_source_roots(
    explicit_source_roots: Vec<String>,
    workspace_root: Option<&str>,
    focus_path: Option<&str>,
    model_name: Option<&str>,
    task: &str,
) -> Result<Vec<String>, PyRuntimeStringError> {
    let mut roots = match workspace_root {
        Some(root) => workspace_configured_source_roots(root, focus_path)?,
        None => Vec::new(),
    };
    if let (Some(root), Some(model)) = (workspace_root, model_name) {
        extend_scenario_source_roots(&mut roots, root, model, task)?;
    }
    roots.extend(explicit_source_roots);
    Ok(dedup_source_roots(roots))
}

fn workspace_configured_source_roots(
    workspace_root: &str,
    focus_path: Option<&str>,
) -> Result<Vec<String>, PyRuntimeStringError> {
    let workspace_root = Path::new(workspace_root);
    let focus_path = focus_path
        .map(PathBuf::from)
        .unwrap_or_else(|| workspace_root.to_path_buf());
    let config = WorkspaceConfig::load(workspace_root, &focus_path)
        .map_err(|e| PyRuntimeStringError(format!("Workspace config error: {e}")))?;
    Ok(config.effective_source_roots_for(&focus_path))
}

fn extend_scenario_source_roots(
    roots: &mut Vec<String>,
    workspace_root: &str,
    model_name: &str,
    task: &str,
) -> Result<(), PyRuntimeStringError> {
    let task = parse_scenario_task(task)?;
    let config = ScenarioConfig::load_or_default(Path::new(workspace_root))
        .map_err(|e| PyRuntimeStringError(format!("Scenario config error: {e}")))?;
    match task {
        ScenarioTask::Simulate => {
            let fallback = EffectiveSimulationConfig {
                source_root_paths: roots.clone(),
                ..EffectiveSimulationConfig::default()
            };
            *roots = config
                .simulation_snapshot_for_model(model_name, &fallback)
                .effective
                .source_root_paths;
        }
        ScenarioTask::Codegen => roots.extend(config.resolve_all_source_root_paths()),
    }
    Ok(())
}

fn parse_scenario_task(task: &str) -> Result<ScenarioTask, PyRuntimeStringError> {
    match task {
        "simulate" => Ok(ScenarioTask::Simulate),
        "codegen" => Ok(ScenarioTask::Codegen),
        other => Err(PyRuntimeStringError(format!(
            "unknown scenario task '{other}', expected 'simulate' or 'codegen'"
        ))),
    }
}

fn dedup_source_roots(paths: Vec<String>) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    let mut deduped = Vec::new();
    for path in paths {
        let key = canonical_path_key(&path);
        if seen.insert(key) {
            deduped.push(path);
        }
    }
    deduped
}

fn split_env_modelicapath() -> Vec<String> {
    let Some(raw) = env::var_os("MODELICAPATH") else {
        return Vec::new();
    };
    env::split_paths(&raw)
        .filter(|entry| !entry.as_os_str().is_empty())
        .map(|entry| entry.to_string_lossy().to_string())
        .collect()
}

pub(crate) fn resolve_source_root_paths(configured_paths: &[String]) -> Vec<String> {
    let env_paths = split_env_modelicapath();
    merge_source_root_paths(&env_paths, configured_paths)
}

pub(crate) fn refresh_effective_source_root_paths(
    session: &mut Session,
    configured_paths: &[String],
    effective_paths: &mut Vec<String>,
) {
    let next_paths = resolve_source_root_paths(configured_paths);
    let next_keys = next_paths
        .iter()
        .map(|path| canonical_path_key(path))
        .collect::<std::collections::HashSet<_>>();
    for removed_path in effective_paths.iter() {
        if next_keys.contains(&canonical_path_key(removed_path)) {
            continue;
        }
        let source_set_key = source_root_source_set_key(removed_path);
        let _ = session.replace_parsed_source_set(
            &source_set_key,
            SourceRootKind::External,
            Vec::new(),
            None,
        );
    }
    *effective_paths = next_paths;
}

fn ensure_required_source_roots_loaded(
    session: &mut Session,
    source: &str,
    source_root_paths: &[String],
) -> Result<(), PyRuntimeStringError> {
    let loaded = session.loaded_source_root_path_keys();
    let referenced = referenced_unloaded_source_root_paths(source, source_root_paths, &loaded);
    let plan = plan_source_root_loads(&referenced, &loaded);
    for source_root_path in plan.load_paths {
        let source_root_key = source_root_source_set_key(&source_root_path);
        let report = session.load_source_root_tolerant(
            &source_root_key,
            SourceRootKind::External,
            Path::new(&source_root_path),
            None,
        );
        if !report.diagnostics.is_empty() {
            return Err(PyRuntimeStringError(report.diagnostics.join("\n")));
        }
    }
    Ok(())
}

fn load_local_compile_unit(
    session: &mut Session,
    source: &str,
    file_name: &str,
) -> Result<(), PyRuntimeStringError> {
    let path = Path::new(file_name);
    if !path.is_file() {
        let _ = session.update_document(file_name, source);
        return Ok(());
    }

    let files = collect_compile_unit_source_files(path)
        .map_err(|e| PyRuntimeStringError(format!("Compile-unit error: {e}")))?;
    // The directory scan can yield the requested file under a different spelling
    // (`./test.mo` vs `test.mo`), so compare canonical paths.
    let requested_canonical = path.canonicalize().ok();
    for sibling in files {
        if sibling == path
            || (requested_canonical.is_some() && sibling.canonicalize().ok() == requested_canonical)
        {
            continue;
        }
        let sibling_path = sibling.to_string_lossy().to_string();
        let sibling_source = fs::read_to_string(&sibling)
            .map_err(|e| PyRuntimeStringError(format!("Failed to read {sibling_path}: {e}")))?;
        let _ = session.update_document(&sibling_path, &sibling_source);
    }

    let _ = session.update_document(file_name, source);
    Ok(())
}

fn infer_model_name_from_session(
    session: &mut Session,
    uri: &str,
) -> Result<String, PyRuntimeStringError> {
    let definition = session
        .get_document(uri)
        .map(|doc| doc.best_effort().clone())
        .ok_or_else(|| PyRuntimeStringError(format!("failed to load document '{uri}'")))?;

    let top_level_names = definition
        .classes
        .iter()
        .filter_map(|(name, class)| {
            let class_kind = class.class_type.as_str();
            if class_kind == "model" || class_kind == "block" || class_kind == "class" {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let mut candidates = collect_model_names(&definition);
    candidates.sort();
    candidates.dedup();
    if candidates.is_empty() {
        if let Some((diagnostics, source_map)) =
            session.document_parse_diagnostics_with_source_map(uri)
        {
            let source_summary = diagnostics
                .into_iter()
                .map(|diagnostic| match diagnostic.code {
                    Some(code) => format!("{code}: {}", diagnostic.message),
                    None => diagnostic.message,
                })
                .collect::<Vec<_>>()
                .join("\n");
            return Err(PyRuntimeStringError(format!(
                "failed to infer model from '{uri}': {source_summary}\nsource_map={source_map:?}"
            )));
        }
        return Err(PyRuntimeStringError(format!(
            "No compilable model/block/class candidates found in '{uri}'."
        )));
    }

    if top_level_names.len() == 1
        && let Some(model) = choose_single_candidate_by_suffix(&candidates, &top_level_names[0])
    {
        return Ok(model);
    }

    if candidates.len() == 1 {
        return Ok(candidates[0].clone());
    }

    let file_stem = model_inference_file_stem(uri)?;
    if let Some(model) = choose_single_candidate_by_suffix(&candidates, file_stem) {
        return Ok(model);
    }

    let preview = candidates
        .iter()
        .take(15)
        .cloned()
        .collect::<Vec<_>>()
        .join(", ");
    Err(PyRuntimeStringError(format!(
        "Unable to infer model from '{uri}'. Candidates: {}{}.",
        preview,
        if candidates.len() > 15 { ", ..." } else { "" }
    )))
}

fn model_inference_file_stem(uri: &str) -> Result<&str, PyRuntimeStringError> {
    Path::new(uri)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .ok_or_else(|| {
            PyRuntimeStringError(format!(
                "Unable to infer model from '{uri}': document path has no valid UTF-8 file stem"
            ))
        })
}

fn choose_single_candidate_by_suffix(candidates: &[String], suffix: &str) -> Option<String> {
    let mut matches = candidates
        .iter()
        .filter(|candidate| last_segment(candidate) == suffix || candidate.as_str() == suffix)
        .collect::<Vec<_>>();
    if matches.len() == 1 {
        return Some(matches[0].clone());
    }
    if matches.is_empty() {
        return None;
    }

    matches.sort_by_key(|candidate| candidate.matches('.').count());
    let min_depth = matches[0].matches('.').count();
    let min_matches = matches
        .into_iter()
        .filter(|candidate| candidate.matches('.').count() == min_depth)
        .collect::<Vec<_>>();
    if min_matches.len() == 1 {
        Some(min_matches[0].clone())
    } else {
        None
    }
}

fn last_segment(qualified_name: &str) -> &str {
    rumoca_core::top_level_last_segment(qualified_name)
}

fn compile_requested_model(
    session: &mut Session,
    model_name: &str,
) -> Result<HighLevelCompilationResult, PyRuntimeStringError> {
    let mut report = session.compile_model_strict_reachable_with_recovery(model_name);
    let failure_summary = report.failure_summary(usize::MAX);
    let result = match report.requested_result.take() {
        Some(PhaseResult::Success(result)) => {
            if !report.failures.is_empty() {
                return Err(PyRuntimeStringError(failure_summary));
            }
            *result
        }
        Some(PhaseResult::NeedsInner { .. }) => {
            return Err(PyRuntimeStringError(failure_summary));
        }
        Some(PhaseResult::Failed { phase, .. }) => {
            let phase_name = match phase {
                FailedPhase::Instantiate => "instantiate",
                FailedPhase::Typecheck => "typecheck",
                FailedPhase::Flatten => "flatten",
                FailedPhase::ToDae => "todae",
            };
            return Err(PyRuntimeStringError(format!(
                "{phase_name} failed: {failure_summary}"
            )));
        }
        None => return Err(PyRuntimeStringError(failure_summary)),
    };
    let resolved = session.resolved_cached().ok_or_else(|| {
        PyRuntimeStringError("strict compile produced no cached resolved tree".to_string())
    })?;
    Ok(HighLevelCompilationResult::new(
        result.dae,
        result.balance_detail,
        result.flat,
        resolved,
    ))
}

pub(crate) fn compile_source_in_session(
    session: &mut Session,
    source: &str,
    model_name: Option<&str>,
    file_name: &str,
    source_root_paths: &[String],
) -> Result<(HighLevelCompilationResult, String), PyRuntimeStringError> {
    ensure_required_source_roots_loaded(session, source, source_root_paths)?;
    load_local_compile_unit(session, source, file_name)?;
    let model_name = match model_name {
        Some(name) => name.to_string(),
        None => infer_model_name_from_session(session, file_name)?,
    };
    let result = compile_requested_model(session, &model_name)?;
    Ok((result, model_name))
}

/// Render a codegen target's files from a compiled model, returning the typed
/// (path, content) list. DAE-IR targets render straight from the DAE; solve-IR
/// targets first lower to the scalarized/causalized solve IR — the same path the
/// CLI's `compile --target` uses.
pub(crate) fn render_target_files(
    result: &HighLevelCompilationResult,
    model_name: &str,
    target: &str,
) -> Result<Vec<RenderedTargetFile>, PyRuntimeStringError> {
    let bundle = TargetBundle::load(target)
        .map_err(|e| PyRuntimeStringError(format!("Target error: {e}")))?;
    let manifest = bundle
        .parse_manifest()
        .map_err(|e| PyRuntimeStringError(format!("Target manifest error: {e}")))?;
    if let Some(capabilities) = manifest.capabilities.as_ref() {
        validate_dae_target_capabilities(&result.dae, &manifest, capabilities)
            .map_err(|e| PyRuntimeStringError(format!("Target capability error: {e}")))?;
    }
    if manifest.ir == TargetTemplateIr::Solve {
        render_solve_target_files(&bundle, &manifest, result, model_name)
    } else {
        render_dae_target_files(&bundle, &manifest, &result.dae, model_name)
            .map_err(|e| PyRuntimeStringError(format!("Target rendering error: {e}")))
    }
}

/// Render a solve-IR target's files through the IR-aware renderer — byte-for-byte
/// identical to the CLI's `compile --target`.
fn render_solve_target_files(
    bundle: &TargetBundle,
    manifest: &TargetManifest,
    result: &HighLevelCompilationResult,
    model_name: &str,
) -> Result<Vec<RenderedTargetFile>, PyRuntimeStringError> {
    use rumoca_compile::codegen::targets::TargetTemplateSource;
    let mut files = Vec::with_capacity(manifest.files.len());
    for file in &manifest.files {
        let path = result
            .render_template_str_with_name_and_ir(&file.path, model_name, TemplateIr::Solve)
            .map_err(|e| {
                PyRuntimeStringError(format!("Render target path '{}': {e}", file.path))
            })?;
        let template = bundle.template_source(&file.template).map_err(|e| {
            PyRuntimeStringError(format!("Target template '{}': {e}", file.template))
        })?;
        let content = result
            .render_template_str_with_name_and_ir(template.as_ref(), model_name, TemplateIr::Solve)
            .map_err(|e| {
                PyRuntimeStringError(format!("Render target template '{}': {e}", file.template))
            })?;
        files.push(RenderedTargetFile {
            path: path.trim().to_string(),
            content,
        });
    }
    Ok(files)
}

/// Rumoca compiled extension module (`rumoca._native`).
///
/// The public Python API lives under the `rumoca` package
/// (`crates/rumoca-bind-python/python/rumoca/__init__.py`), which re-exports
/// everything here and adds the `%%modelica` Jupyter cell magic.
#[pymodule]
fn _native(m: &Bound<'_, PyModule>) -> PyResult<()> {
    // module-level functions
    m.add_function(wrap_pyfunction!(version, m)?)?;
    m.add_function(wrap_pyfunction!(format_source, m)?)?;
    m.add_function(wrap_pyfunction!(targets_fn, m)?)?;
    m.add_function(wrap_pyfunction!(solvers_fn, m)?)?;
    m.add_function(wrap_pyfunction!(session::load, m)?)?;
    m.add_function(wrap_pyfunction!(session::loads, m)?)?;
    m.add_function(wrap_pyfunction!(session::validate, m)?)?;
    m.add_function(wrap_pyfunction!(session::validate_source, m)?)?;

    // typed classes
    m.add_class::<session::Session>()?;
    m.add_class::<model::Model>()?;
    m.add_class::<model::SimConfig>()?;
    m.add_class::<model::StructuralInfo>()?;
    m.add_class::<metadata::VarView>()?;
    m.add_class::<metadata::ParamView>()?;
    m.add_class::<metadata::VariableInfo>()?;
    m.add_class::<metadata::ParameterInfo>()?;
    m.add_class::<result::Result>()?;
    m.add_class::<codegen::CodegenResult>()?;
    m.add_class::<codegen::GeneratedFile>()?;
    m.add_class::<targets::Target>()?;
    m.add_class::<targets::SolverInfo>()?;
    m.add_class::<diagnostics::Diagnostic>()?;

    // exception hierarchy
    let py = m.py();
    m.add(
        "RumocaError",
        py.get_type_bound::<diagnostics::RumocaError>(),
    )?;
    m.add("ParseError", py.get_type_bound::<diagnostics::ParseError>())?;
    m.add(
        "CompileError",
        py.get_type_bound::<diagnostics::CompileError>(),
    )?;
    m.add(
        "SimulationError",
        py.get_type_bound::<diagnostics::SimulationError>(),
    )?;
    m.add(
        "StructuralParamError",
        py.get_type_bound::<diagnostics::StructuralParamError>(),
    )?;
    Ok(())
}
