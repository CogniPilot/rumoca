//! Finding the two corpora, and refusing to run when either one is not there.
//!
//! # Why no path in the repository
//!
//! The flight corpus is an out-of-tree checkout. A machine path committed here
//! would be wrong for everyone except the machine it was written on, and a
//! `RUMOCA_*` environment variable is banned outright by SPEC_0018. So the root
//! arrives at RUN time only, through channels SPEC_0018 names: argv first, then
//! a fixed-path config file under `target/`, then the location CI already
//! checks the pinned corpus out to, then the Modelica-standard `MODELICAPATH`
//! (a pass-through variable, explicitly out of scope for the `RUMOCA_*` rule,
//! and already how the sibling RDD2 gates find this same corpus).
//!
//! # Why absence is red
//!
//! [`CORPUS_UNMEASURED_HEADLINE`] mirrors `parity unmeasured` exactly. A gate
//! that skips when its corpus is missing reports "ok" for a run in which
//! nothing was compared, which is the one failure mode an oracle net cannot
//! tolerate: the green tick would mean "not measured" and read as "correct".

use anyhow::{Context, Result, bail};
use serde::Deserialize;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use super::manifest::{Corpus, CorpusManifest};

/// Fixed headline for a run that could not measure a corpus. Operators and CI
/// summaries grep for this exact text, so it is spelled once.
pub(crate) const CORPUS_UNMEASURED_HEADLINE: &str =
    "corpus unmeasured: the pinned corpus is not on this machine";

/// Fixed-path config channel for the corpus roots, relative to the workspace
/// root. SPEC_0018 permits one inspectable fixed-path file where argv cannot
/// carry the value; here it is what lets `cargo xtask verify full` run the gate
/// without every caller retyping the path.
pub(crate) const CONFIG_PATH: &str = "target/verification/corpus-config.json";

/// Where CI checks the pinned `modelica_models` revision out to. Trying it
/// keeps the CI job and a local run on the same resolution order.
const CI_FLIGHT_MODELS_PATH: &str = "target/modelica-models";

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
    pub(crate) flight_models: PathBuf,
    pub(crate) msl: PathBuf,
}

impl CorpusRoots {
    pub(crate) fn root_for(&self, corpus: Corpus) -> &Path {
        match corpus {
            Corpus::FlightModels => &self.flight_models,
            Corpus::Msl => &self.msl,
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

/// Candidate flight-model roots in resolution order.
pub(super) fn flight_model_candidates(
    root: &Path,
    from_argv: Option<&Path>,
    config: Option<&CorpusConfig>,
) -> Vec<(String, PathBuf)> {
    // An explicitly named root is authoritative, never a preference: if it is
    // unusable the gate reports "corpus unmeasured" rather than silently
    // measuring whatever fallback happens to be usable. A developer who
    // mistypes a path, or whose checkout is one file short, must see the
    // refusal, not a green run against a different corpus.
    if let Some(path) = from_argv {
        return vec![("--models-root".to_string(), path.to_path_buf())];
    }
    let mut candidates = Vec::new();
    if let Some(path) = config.and_then(|config| config.models_root.as_deref()) {
        candidates.push((format!("{CONFIG_PATH} models_root"), path.to_path_buf()));
    }
    candidates.push((
        CI_FLIGHT_MODELS_PATH.to_string(),
        root.join(CI_FLIGHT_MODELS_PATH),
    ));
    if let Some(raw) = env::var_os("MODELICAPATH") {
        candidates.extend(env::split_paths(&raw).map(|path| ("MODELICAPATH".to_string(), path)));
    }
    candidates
}

/// Resolve both roots or fail closed.
pub(crate) fn resolve(
    root: &Path,
    manifest: &CorpusManifest,
    from_argv: Option<&Path>,
    msl: PathBuf,
    config: Option<&CorpusConfig>,
) -> Result<CorpusRoots> {
    let candidates = flight_model_candidates(root, from_argv, config);
    let flight_models = first_usable(manifest, Corpus::FlightModels, &candidates)
        .ok_or_else(|| unmeasured(root, manifest, Corpus::FlightModels, &candidates))?;
    let msl_candidates = vec![("the cached MSL release".to_string(), msl)];
    let msl = first_usable(manifest, Corpus::Msl, &msl_candidates)
        .ok_or_else(|| unmeasured(root, manifest, Corpus::Msl, &msl_candidates))?;
    Ok(CorpusRoots { flight_models, msl })
}

/// The first candidate that carries every entry point the manifest names for
/// this corpus.
///
/// "Carries every entry point" rather than "exists": a directory that happens
/// to be there but holds none of the models would otherwise be accepted and
/// then fail every row with a compiler error, reporting a compiler regression
/// where the real fact is that nothing was measured.
fn first_usable(
    manifest: &CorpusManifest,
    corpus: Corpus,
    candidates: &[(String, PathBuf)],
) -> Option<PathBuf> {
    candidates
        .iter()
        .map(|(_, path)| path)
        .find(|path| missing_entry_points(manifest, corpus, path).is_empty())
        .cloned()
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
            "  Point the gate at your checkout, either way:\n    \
             cargo xtask verify corpus-pin --models-root /path/to/modelica_models\n  or write \
             the path once:\n    mkdir -p {parent} && printf '{{\"models_root\": \
             \"/path/to/modelica_models\"}}\\n' > {config}\n  This is a hard failure rather \
             than a skip: a green run that compared nothing would report the corpus as correct.",
            parent = root.join("target/verification").display(),
            config = config_path(root).display(),
        ),
        Corpus::Msl => format!(
            "  The MSL release is normally provisioned into {}. Run any MSL gate once to \
             populate it, or pass --msl-root.",
            root.join("target/msl").display()
        ),
    }
}

/// Refuse a root that exists but is not the corpus, before anything is run.
pub(crate) fn ensure_usable(manifest: &CorpusManifest, roots: &CorpusRoots) -> Result<()> {
    for corpus in [Corpus::FlightModels, Corpus::Msl] {
        let root = roots.root_for(corpus);
        let missing = missing_entry_points(manifest, corpus, root);
        if !missing.is_empty() {
            bail!(
                "{CORPUS_UNMEASURED_HEADLINE}\n  corpus: {} at {}\n  missing entry points: {}",
                corpus.label(),
                root.display(),
                missing.join(", ")
            );
        }
    }
    Ok(())
}
