//! Model-file and source-root resolution for the CLI: validating the model-file
//! positional, inferring the model name when `--model` is omitted, merging
//! `--source-root` with `MODELICAPATH`, and collecting Modelica files for the
//! `lint`/`fmt` directory walk. Split out of `cli.rs` so the dispatch module
//! stays focused on command wiring (SPEC_0021 file-size split).

use std::ffi::OsString;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use walkdir::WalkDir;

use crate::cli::ModelOptions;
use crate::{Compiler, CompilerError};
use rumoca_compile::compile::{Session, SessionConfig};

/// Report a missing or non-file model path identically for every command,
/// before either the `--model`-present (compiler E003) or inferred (read in
/// `infer_model_name`) path can produce a less helpful bare `io::Error`.
pub(crate) fn ensure_model_file_readable(model_file: &str) -> Result<()> {
    let path = Path::new(model_file);
    if path.is_dir() {
        bail!("model file `{model_file}` is a directory, not a Modelica file");
    }
    if !path.exists() {
        bail!("model file `{model_file}` not found");
    }
    // A rumoca-scenario.toml scenario fed to the model path otherwise parses as Modelica and
    // dies on `[` with a dead-end syntax error; point at the right command
    // instead.
    if looks_like_scenario_config(path) {
        bail!(
            "`{model_file}` looks like a rumoca-scenario.toml scenario, not a Modelica model; \
             did you mean `rumoca sim check -c {model_file}` (or `rumoca sim -c {model_file}`)?"
        );
    }
    Ok(())
}

/// Whether `path` is a rumoca-scenario.toml scenario rather than a Modelica model: a `.toml`
/// extension, or a first non-blank line of `[rumoca]`/`[model]`.
fn looks_like_scenario_config(path: &Path) -> bool {
    if path.extension().is_some_and(|ext| ext == "toml") {
        return true;
    }
    let Ok(contents) = std::fs::read_to_string(path) else {
        return false;
    };
    contents
        .lines()
        .map(str::trim)
        .find(|line| !line.is_empty())
        .is_some_and(|line| line == "[rumoca]" || line == "[model]")
}

/// Build a [`Compiler`] configured for an in-memory `source`, resolving the model
/// name (explicit `options.model` or inference) and merging `--source-root` with
/// `MODELICAPATH` exactly like the file-based path.
pub(crate) fn compiler_for_source(
    source: &str,
    file_name: &str,
    options: &ModelOptions,
    verbose: bool,
) -> Result<(Compiler, String)> {
    let model = match &options.model {
        Some(model) => model.clone(),
        None => infer_model_name_from_source(source, file_name)?,
    };
    let source_roots = merged_source_root_paths(&options.source_roots);
    let compiler = Compiler::new()
        .model(&model)
        .verbose(verbose)
        .source_roots(&source_roots);
    Ok((compiler, model))
}

pub(crate) fn split_path_list(raw: Option<OsString>) -> Vec<String> {
    let Some(raw) = raw else {
        return Vec::new();
    };
    std::env::split_paths(&raw)
        .filter(|entry| !entry.as_os_str().is_empty())
        .map(|entry| entry.to_string_lossy().to_string())
        .collect()
}

pub(crate) fn merged_source_root_paths(cli_paths: &[String]) -> Vec<String> {
    let env_modelica_paths = split_path_list(std::env::var_os("MODELICAPATH"));
    merge_source_root_path_sources(cli_paths, &env_modelica_paths)
}

pub(crate) fn merge_source_root_path_sources(
    cli_paths: &[String],
    env_modelica_paths: &[String],
) -> Vec<String> {
    let mut merged: Vec<String> = Vec::new();
    for path in cli_paths.iter().chain(env_modelica_paths.iter()) {
        let trimmed = path.trim();
        if trimmed.is_empty() {
            continue;
        }
        let key = if cfg!(windows) {
            trimmed.to_ascii_lowercase()
        } else {
            trimmed.to_string()
        };
        let already_seen = merged.iter().any(|existing| {
            let existing_key = if cfg!(windows) {
                existing.to_ascii_lowercase()
            } else {
                existing.clone()
            };
            existing_key == key
        });
        if !already_seen {
            merged.push(trimmed.to_string());
        }
    }
    merged
}

pub(crate) fn infer_model_name(model_file: &str) -> Result<String> {
    let source = std::fs::read_to_string(model_file)
        .with_context(|| format!("read model file `{model_file}`"))?;
    infer_model_name_from_source(&source, model_file)
}

/// Infer the model name from in-memory `source` keyed under `file_name` (used by
/// the value-returning entrypoints, which never touch disk). Same inference rules
/// and diagnostics as the file-reading [`infer_model_name`].
pub(crate) fn infer_model_name_from_source(source: &str, model_file: &str) -> Result<String> {
    let mut session = Session::new(SessionConfig::default());
    let parse_error = session.update_document(model_file, source);
    let definition = session
        .get_document(model_file)
        .map(|doc| doc.best_effort().clone())
        .ok_or_else(|| anyhow::anyhow!("failed to load document '{}'", model_file))?;

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

    let mut candidates = rumoca_compile::parsing::collect_model_names(&definition);
    candidates.sort();
    candidates.dedup();
    if candidates.is_empty() {
        if parse_error.is_some()
            && let Some((diagnostics, source_map)) =
                session.document_parse_diagnostics_with_source_map(model_file)
        {
            return Err(anyhow::Error::new(CompilerError::SourceDiagnosticsError {
                summary: format!("failed to infer model from '{}'", model_file),
                diagnostics,
                source_map: Box::new(source_map),
            }));
        }
        bail!(
            "No compilable model/block/class candidates found in '{}'; pass --model <NAME>.",
            model_file
        );
    }

    if top_level_names.len() == 1
        && let Some(model) = choose_single_candidate_by_suffix(&candidates, &top_level_names[0])
    {
        return Ok(model);
    }

    if candidates.len() == 1 {
        return Ok(candidates[0].clone());
    }

    if let Some(file_stem) = model_file_stem(model_file)
        && let Some(model) = choose_single_candidate_by_suffix(&candidates, file_stem)
    {
        return Ok(model);
    }

    if parse_error.is_some()
        && let Some((diagnostics, source_map)) =
            session.document_parse_diagnostics_with_source_map(model_file)
    {
        return Err(anyhow::Error::new(CompilerError::SourceDiagnosticsError {
            summary: format!("failed to infer model from '{}'", model_file),
            diagnostics,
            source_map: Box::new(source_map),
        }));
    }

    let preview = candidates
        .iter()
        .take(15)
        .cloned()
        .collect::<Vec<_>>()
        .join(", ");
    bail!(
        "Unable to infer model from '{}'. Candidates: {}{} . Pass --model <NAME>.",
        model_file,
        preview,
        if candidates.len() > 15 { ", ..." } else { "" }
    );
}

fn model_file_stem(model_file: &str) -> Option<&str> {
    let stem = Path::new(model_file).file_stem()?;
    let stem = stem.to_str()?;
    (!stem.is_empty()).then_some(stem)
}

pub(crate) fn normalize_target_paths(paths: &[PathBuf]) -> Vec<PathBuf> {
    if paths.is_empty() {
        vec![PathBuf::from(".")]
    } else {
        paths.to_vec()
    }
}

/// Validate paths the user passed explicitly to `lint`/`fmt`. An explicit path
/// that doesn't exist, or an explicit file that isn't `.mo`, is an error — not a
/// silent "No .mo files found" / exit 0, which reads as a clean pass and lets a
/// `lint`/`fmt` CI gate no-op on a typo. The default `.`-walk
/// (empty `paths`) is exempt: finding nothing there is a legitimate clean run.
pub(crate) fn validate_explicit_target_paths(paths: &[PathBuf]) -> Result<()> {
    for path in paths {
        if !path.exists() {
            bail!("path `{}` not found", path.display());
        }
        if path.is_file() && path.extension().is_none_or(|ext| ext != "mo") {
            bail!(
                "`{}` is not a Modelica (.mo) file; pass a .mo file or a directory",
                path.display()
            );
        }
    }
    Ok(())
}

pub(crate) fn first_path_config_dir(paths: &[PathBuf]) -> PathBuf {
    match paths.first() {
        Some(p) => {
            if p.is_dir() {
                p.clone()
            } else {
                parent_dir_or_current(p).to_path_buf()
            }
        }
        None => PathBuf::from("."),
    }
}

pub(crate) fn parent_dir_or_current(path: &Path) -> &Path {
    match path.parent() {
        Some(parent) => parent,
        None => Path::new("."),
    }
}

/// Directories never worth walking for source `.mo` files (build output, VCS,
/// caches, deps). Skipping them keeps `lint`/`fmt` on the current directory fast
/// — otherwise a repo's `target/` (which can hold the whole MSL stdlib) makes it
/// appear to hang.
fn is_ignored_walk_dir(name: &str) -> bool {
    matches!(
        name,
        "target" | ".git" | ".rumoca" | "node_modules" | ".venv" | "dist" | "build"
    )
}

pub(crate) fn collect_modelica_files(paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut out = Vec::<PathBuf>::new();
    for path in paths {
        if path.is_file() {
            if path.extension().is_some_and(|ext| ext == "mo") {
                out.push(path.to_path_buf());
            }
            continue;
        }
        let walk = WalkDir::new(path).into_iter().filter_entry(|entry| {
            // Always descend the explicitly-passed root; prune ignored subdirs.
            !entry.file_type().is_dir()
                || entry.depth() == 0
                || entry
                    .file_name()
                    .to_str()
                    .is_none_or(|name| !is_ignored_walk_dir(name))
        });
        for entry in walk.filter_map(Result::ok) {
            let candidate = entry.path();
            if candidate.is_file() && candidate.extension().is_some_and(|ext| ext == "mo") {
                out.push(candidate.to_path_buf());
            }
        }
    }
    out.sort();
    out.dedup();
    out
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
