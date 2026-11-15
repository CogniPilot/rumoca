//! One explicit complete source set and its compile: the primary source as
//! `input.mo` beside companion documents named by canonical relative paths.
//! The browser's prebuilt and precompiled exports and the linked FMU packager
//! compile through this one construction, so every source id (FNV of the
//! document name) and the compile mode are the same on both sides.
use std::{collections::BTreeMap, fmt};

use serde::de::{MapAccess, Visitor};
use serde::{Deserialize, Deserializer};

use crate::compile::{CompilationMode, CompilationResult, FailedPhase, PhaseResult, Session};

/// The document name the primary source is compiled under.
pub const PRIMARY_SOURCE_NAME: &str = "input.mo";

/// An explicit complete workspace; never reads ambient session state or paths.
pub struct SourceSet(BTreeMap<String, String>);

struct UniqueSources;

impl<'de> Visitor<'de> for UniqueSources {
    type Value = BTreeMap<String, String>;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("an object of unique canonical paths to Modelica source strings")
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
        let mut sources = BTreeMap::new();
        while let Some((path, source)) = map.next_entry::<String, String>()? {
            if path.contains(['\\', ':'])
                || path.split('/').any(|part| matches!(part, "" | "." | ".."))
            {
                return Err(serde::de::Error::custom(
                    "Source paths must be canonical relative paths",
                ));
            }
            if sources.insert(path, source).is_some() {
                return Err(serde::de::Error::custom("Duplicate source path"));
            }
        }
        Ok(sources)
    }
}

impl<'de> Deserialize<'de> for SourceSet {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_map(UniqueSources).map(Self)
    }
}

impl SourceSet {
    /// The companion documents of `sources_json` with `source` as `input.mo`.
    pub fn parse(source: &str, sources_json: &str) -> Result<Self, String> {
        let mut sources: Self = serde_json::from_str(sources_json)
            .map_err(|error| format!("Invalid prebuilt source set: {error}"))?;
        if sources
            .0
            .insert(PRIMARY_SOURCE_NAME.into(), source.into())
            .is_some()
        {
            return Err("Prebuilt source set reserves input.mo for the primary source".into());
        }
        Ok(sources)
    }

    /// Every document's name with `digest` of its source, in name order.
    pub fn hashes(&self, digest: impl Fn(&str) -> String) -> BTreeMap<&str, String> {
        self.0
            .iter()
            .map(|(path, source)| (path.as_str(), digest(source)))
            .collect()
    }

    /// Add every document to `session` under its own name.
    pub fn load(&self, session: &mut Session) -> Result<(), String> {
        for (path, source) in &self.0 {
            session
                .add_document(path, source)
                .map_err(|error| format!("Invalid source {path}: {error}"))?;
        }
        Ok(())
    }
}

/// Compile `model_name` with `mode`; any failure is refused with its summary.
pub fn compile_requested(
    session: &mut Session,
    model_name: &str,
    mode: CompilationMode,
) -> Result<CompilationResult, String> {
    let mut report = session.compile_model_with_mode(model_name, mode);
    if !report.failures.is_empty() {
        return Err(format!("Compilation error: {}", report.failure_summary(8)));
    }
    match report.requested_result.take() {
        Some(PhaseResult::Success(result)) => Ok(*result),
        Some(PhaseResult::NeedsInner { missing_inners, .. }) => Err(format!(
            "Compilation error: missing inner declarations: {}",
            missing_inners.join(", ")
        )),
        Some(PhaseResult::Failed { phase, error, .. }) => {
            let phase_name = match phase {
                FailedPhase::Instantiate => "instantiate",
                FailedPhase::Typecheck => "typecheck",
                FailedPhase::Flatten => "flatten",
                FailedPhase::ToDae => "todae",
            };
            Err(format!("Compilation error: {phase_name} failed: {error}"))
        }
        None => Err(format!("Compilation error: {}", report.failure_summary(8))),
    }
}

/// Compile `model_name` (qualified against `input.mo`) from the explicit
/// complete source set in a fresh isolated session, strict over reachable
/// classes, uncached.
pub fn compile_source_set(
    source: &str,
    model_name: &str,
    sources_json: &str,
) -> Result<CompilationResult, String> {
    let sources = SourceSet::parse(source, sources_json)?;
    let mut session = Session::default();
    sources.load(&mut session)?;
    let requested = session.qualify_model_name(PRIMARY_SOURCE_NAME, model_name);
    compile_requested(
        &mut session,
        &requested,
        CompilationMode::StrictReachableUncachedWithRecovery,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_primary_name_is_reserved_and_companion_paths_are_canonical() {
        assert!(SourceSet::parse("", r#"{"input.mo": ""}"#).is_err());
        for refused in [
            r#"{"../a.mo": ""}"#,
            r#"{"/a.mo": ""}"#,
            r#"{"a\\b.mo": ""}"#,
            r#"{"c:/a.mo": ""}"#,
            r#"{"a/./b.mo": ""}"#,
        ] {
            assert!(SourceSet::parse("", refused).is_err(), "{refused}");
        }
        assert!(SourceSet::parse("", r#"{"Lib/package.mo": ""}"#).is_ok());
    }

    #[test]
    fn a_source_set_compiles_the_primary_source_with_its_companions() {
        let library = "package Lib\n  model Decay\n    parameter Real k = 2;\n    \
                       Real x(start = 1);\n  equation\n    der(x) = -k * x;\n  end Decay;\nend Lib;\n";
        let sources = serde_json::json!({ "Lib.mo": library }).to_string();
        assert!(compile_source_set("model M\n  Lib.Decay d;\nend M;\n", "M", &sources).is_ok());
        let missing = compile_source_set("model M\n  Lib.Missing m;\nend M;\n", "M", &sources)
            .err()
            .unwrap_or_default();
        assert!(missing.starts_with("Compilation error"), "{missing}");
    }
}
