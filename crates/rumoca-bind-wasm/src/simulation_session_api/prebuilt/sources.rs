use std::{collections::BTreeMap, fmt};

use rumoca_compile::Session;
use serde::de::{MapAccess, Visitor};
use serde::{Deserialize, Deserializer};

use crate::WasmError;

/// An explicit complete workspace; never reads ambient session state or paths.
pub(super) struct SourceSet(BTreeMap<String, String>);

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
    pub(super) fn parse(source: &str, sources_json: &str) -> Result<Self, WasmError> {
        let mut sources: Self = serde_json::from_str(sources_json)
            .map_err(|error| WasmError::new(format!("Invalid prebuilt source set: {error}")))?;
        if sources.0.insert("input.mo".into(), source.into()).is_some() {
            return Err(WasmError::new(
                "Prebuilt source set reserves input.mo for the primary source",
            ));
        }
        Ok(sources)
    }

    pub(super) fn hashes(&self) -> BTreeMap<&str, String> {
        self.0
            .iter()
            .map(|(path, source)| (path.as_str(), super::digest(source)))
            .collect()
    }

    pub(super) fn load(&self, session: &mut Session) -> Result<(), WasmError> {
        for (path, source) in &self.0 {
            session
                .add_document(path, source)
                .map_err(|error| WasmError::new(format!("Invalid source {path}: {error}")))?;
        }
        Ok(())
    }
}
