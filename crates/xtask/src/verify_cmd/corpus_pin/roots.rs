//! Qualified numerical corpus inputs. Flight sources are checked against the
//! immutable verification archive before workers start. Explicit argv/config
//! roots remain authoritative and fail closed instead of selecting another input.

use anyhow::{Context, Result};
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

use super::manifest::{Corpus, CorpusManifest};
use crate::modelica_dependency_cache::{QualifiedSource, qualify_flight_source};

/// Fixed headline for a run that could not measure a corpus. Operators and CI
/// summaries grep for this exact text, so it is spelled once.
pub(crate) const CORPUS_UNMEASURED_HEADLINE: &str =
    "corpus unmeasured: the pinned corpus is not on this machine";

/// Fixed-path config channel for the corpus roots, relative to the workspace
/// root. SPEC_0018 permits one inspectable fixed-path file where argv cannot
/// carry the value; here it is what lets `cargo xtask verify full` run the gate
/// without every caller retyping the path.
pub(crate) const CONFIG_PATH: &str = "target/verification/corpus-config.json";

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CorpusConfig {
    /// Checkout of the out-of-tree flight-model library.
    pub(crate) models_root: Option<PathBuf>,
    /// Modelica Standard Library release root, when it is not the cached one.
    pub(crate) msl_root: Option<PathBuf>,
}

/// The resolved roots, each already proven to carry every entry point the
/// manifest asks of it.
#[derive(Debug)]
pub(crate) struct CorpusRoots {
    flight_models: Option<QualifiedSource>,
    pub(crate) msl: PathBuf,
}

impl CorpusRoots {
    pub(crate) fn root_for(&self, corpus: Corpus) -> Result<&Path> {
        match corpus {
            Corpus::FlightModels => Ok(self
                .flight_models
                .as_ref()
                .context("flight corpus was not selected")?
                .root()),
            Corpus::Msl => Ok(&self.msl),
        }
    }
}

pub(crate) fn config_path(root: &Path) -> PathBuf {
    root.join(CONFIG_PATH)
}

pub(crate) fn read_config(root: &Path) -> Result<Option<CorpusConfig>> {
    let path = config_path(root);
    let Ok(raw) = fs::read_to_string(&path) else {
        return Ok(None);
    };
    let config = serde_json::from_str(&raw)
        .with_context(|| format!("failed to parse corpus config {}", path.display()))?;
    Ok(Some(config))
}

/// Explicit roots are authoritative, including a root named in the config.
pub(super) fn named_flight_root<'a>(
    from_argv: Option<&'a Path>,
    config: Option<&'a CorpusConfig>,
) -> Option<&'a Path> {
    from_argv.or_else(|| config.and_then(|config| config.models_root.as_deref()))
}

/// Resolve and qualify the selected corpora exactly once before launching workers.
pub(crate) fn resolve(
    root: &Path,
    manifest: &CorpusManifest,
    from_argv: Option<&Path>,
    msl: PathBuf,
    config: Option<&CorpusConfig>,
) -> Result<CorpusRoots> {
    let flight_models = if manifest
        .entries
        .iter()
        .any(|entry| entry.corpus == Corpus::FlightModels)
    {
        let source = qualify_flight_source(root, named_flight_root(from_argv, config))
            .context(CORPUS_UNMEASURED_HEADLINE)?;
        let candidates = [(
            "qualified flight source".to_string(),
            source.root().to_path_buf(),
        )];
        require_entry_points(root, manifest, Corpus::FlightModels, &candidates)?;
        println!(
            "  flight input: {} (archive SHA-256 {})",
            source.revision(),
            source.archive_sha256()
        );
        Some(source)
    } else {
        None
    };
    require_entry_points(
        root,
        manifest,
        Corpus::Msl,
        &[("MSL release".to_string(), msl.clone())],
    )?;
    Ok(CorpusRoots { flight_models, msl })
}

fn require_entry_points(
    root: &Path,
    manifest: &CorpusManifest,
    corpus: Corpus,
    candidates: &[(String, PathBuf)],
) -> Result<()> {
    if candidates
        .iter()
        .all(|(_, path)| missing_entry_points(manifest, corpus, path).is_empty())
    {
        Ok(())
    } else {
        Err(unmeasured(root, manifest, corpus, candidates))
    }
}

fn missing_entry_points(manifest: &CorpusManifest, corpus: Corpus, root: &Path) -> Vec<String> {
    manifest
        .entries
        .iter()
        .filter(|entry| entry.corpus == corpus)
        .map(|entry| entry.entry_point.clone())
        .filter(|entry_point| !root.join(entry_point).is_file())
        .collect()
}

fn unmeasured(
    root: &Path,
    manifest: &CorpusManifest,
    corpus: Corpus,
    candidates: &[(String, PathBuf)],
) -> anyhow::Error {
    let mut report = format!(
        "{CORPUS_UNMEASURED_HEADLINE}\n  corpus: {}\n  looked in:\n",
        corpus.label()
    );
    for (source, path) in candidates {
        let missing = missing_entry_points(manifest, corpus, path);
        report.push_str(&format!(
            "    {} ({source}): {}\n",
            path.display(),
            describe_miss(&missing)
        ));
    }
    report.push_str(&remedy(root, corpus));
    anyhow::Error::msg(report)
}

fn describe_miss(missing: &[String]) -> String {
    match missing.split_first() {
        None => "usable".to_string(),
        Some((first, [])) => format!("missing {first}"),
        Some((first, rest)) => format!("missing {first} and {} more", rest.len()),
    }
}

fn remedy(root: &Path, corpus: Corpus) -> String {
    match corpus {
        Corpus::FlightModels => format!(
            "  Without --models-root or a models_root in {}, the gate provisions the immutable \
             numerical flight input from infra/verification/corpus-dependencies.toml. An explicit \
             root must match every source file in that archive; it is never replaced by a fallback.",
            config_path(root).display(),
        ),
        Corpus::Msl => format!(
            "  The MSL release is normally provisioned into {}. Run any MSL gate once to \
             populate it, or pass --msl-root.",
            root.join("target/msl").display()
        ),
    }
}
