//! Stable source identity, content ownership, and checked source-map replay.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

use crate::{SourceId, Span};

/// Prefix of the canonical `SourceMap` name for a source whose path is unknown.
const PLACEHOLDER_SOURCE_NAME_PREFIX: &str = "<source-id:";

/// Canonical `SourceMap` name for a source identity with no known path.
///
/// SPEC_0029 §3a and SPEC_0008 "Source Identity" require every `SourceMap`
/// entry to be keyed by a name that *derives* its [`SourceId`]. Parser spans fix
/// the id before the session knows a path for the AST, so those entries are
/// filed under this name: [`source_id_for_name`] decodes it back to the same id,
/// which keeps the invariant true for path-less registrations as well.
pub fn placeholder_source_name(id: SourceId) -> String {
    format!("{PLACEHOLDER_SOURCE_NAME_PREFIX}{:016x}>", id.0)
}

/// The `SourceId` that a `SourceMap` name derives.
///
/// Identical to [`SourceId::from_source_name`] for real file names, and also
/// decodes the placeholder names produced by [`placeholder_source_name`].
pub fn source_id_for_name(name: &str) -> SourceId {
    decode_placeholder_source_name(name).unwrap_or_else(|| SourceId::from_source_name(name))
}

fn decode_placeholder_source_name(name: &str) -> Option<SourceId> {
    let hex = name
        .strip_prefix(PLACEHOLDER_SOURCE_NAME_PREFIX)?
        .strip_suffix('>')?;
    if hex.len() != 16 {
        return None;
    }
    u64::from_str_radix(hex, 16).ok().map(SourceId)
}

/// Maps file names to SourceIds and stores source content for diagnostics.
///
/// This enables diagnostics to point to the correct source file when
/// compiling models that span multiple files.
#[derive(Debug, Clone, Default, Serialize)]
pub struct SourceMap {
    /// (stable source id, name, content) in deterministic insertion order.
    files: Vec<(SourceId, String, Arc<str>)>,
    /// Reverse lookup from file name to SourceId.
    #[serde(skip)]
    name_to_id: HashMap<String, SourceId>,
    /// Reverse lookup from SourceId to `files` index.
    #[serde(skip)]
    id_to_index: HashMap<SourceId, usize>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SourceMapWire {
    files: Vec<(SourceId, String, Arc<str>)>,
}

struct SourceIndexes {
    name_to_id: HashMap<String, SourceId>,
    id_to_index: HashMap<SourceId, usize>,
}

impl<'de> Deserialize<'de> for SourceMap {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let SourceMapWire { files } = SourceMapWire::deserialize(deserializer)?;
        let indexes = checked_source_indexes(&files).map_err(serde::de::Error::custom)?;
        let SourceIndexes {
            name_to_id,
            id_to_index,
        } = indexes;
        Ok(Self {
            files,
            name_to_id,
            id_to_index,
        })
    }
}

fn checked_source_indexes(files: &[(SourceId, String, Arc<str>)]) -> Result<SourceIndexes, String> {
    let mut name_to_id = HashMap::with_capacity(files.len());
    let mut id_to_index = HashMap::with_capacity(files.len());
    for (index, (id, name, _)) in files.iter().enumerate() {
        let derived = source_id_for_name(name);
        if derived != *id {
            return Err(format!(
                "source map name `{name}` derives {derived:?}, not stored identity {id:?}"
            ));
        }
        let std::collections::hash_map::Entry::Vacant(name_entry) = name_to_id.entry(name.clone())
        else {
            return Err(format!("duplicate source map name `{name}`"));
        };
        let std::collections::hash_map::Entry::Vacant(id_entry) = id_to_index.entry(*id) else {
            return Err(format!("duplicate source map identity {id:?}"));
        };
        name_entry.insert(*id);
        id_entry.insert(index);
    }
    Ok(SourceIndexes {
        name_to_id,
        id_to_index,
    })
}

impl SourceMap {
    /// Create a new empty source map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a source file and return its SourceId.
    ///
    /// If the file was already added, returns the existing SourceId.
    pub fn add(&mut self, name: &str, content: &str) -> SourceId {
        self.add_shared(name, Arc::<str>::from(content))
    }

    /// Add a source file using shared source content and return its SourceId.
    ///
    /// This lets LSP/session caches share source text with diagnostics instead
    /// of copying whole files into every `SourceMap`.
    pub fn add_shared(&mut self, name: &str, content: Arc<str>) -> SourceId {
        let id = source_id_for_name(name);
        self.name_to_id.insert(name.to_string(), id);
        if self.get_source(id).is_some() {
            return id;
        }
        let index = self.files.len();
        self.files.push((id, name.to_string(), content));
        self.id_to_index.insert(id, index);
        id
    }

    /// Look up a SourceId by file name.
    ///
    /// The answer is derived from `name` itself, so it cannot depend on
    /// insertion order.
    pub fn get_id(&self, name: &str) -> Option<SourceId> {
        self.name_to_id.get(name).copied().or_else(|| {
            let id = source_id_for_name(name);
            self.id_to_index.contains_key(&id).then_some(id)
        })
    }

    /// Get (name, content) for a SourceId.
    pub fn get_source(&self, id: SourceId) -> Option<(&str, &str)> {
        self.id_to_index
            .get(&id)
            .and_then(|&index| self.files.get(index))
            .map(|(_, name, content)| (name.as_str(), content.as_ref()))
    }

    /// Get the first source id in deterministic map order.
    pub fn first_source_id(&self) -> Option<SourceId> {
        self.files.first().map(|(id, _, _)| *id)
    }

    /// Register `content` under an explicit `SourceId`.
    ///
    /// This entry point exists for ASTs handed to the compiler without their
    /// original path: parser spans have already fixed the `SourceId`, and
    /// `display_name` is only a caller-supplied stand-in.
    ///
    /// The entry is still filed under a name that derives `id`, as required by
    /// SPEC_0029 §3a: `display_name` is used only when it already derives `id`,
    /// otherwise [`placeholder_source_name`] is. That keeps a stand-in label
    /// from shadowing a real file of the same name in the name index and keeps
    /// [`SourceMap::get_id`] stable across serialization.
    ///
    /// Returns `false` when the id (or its derived name) is already registered;
    /// the existing entry always wins.
    pub fn register_id(&mut self, id: SourceId, display_name: &str, content: Arc<str>) -> bool {
        let name = if source_id_for_name(display_name) == id {
            display_name.to_string()
        } else {
            placeholder_source_name(id)
        };
        if self.get_source(id).is_some() || self.name_to_id.contains_key(&name) {
            return false;
        }
        let index = self.files.len();
        self.files.push((id, name.clone(), content));
        self.name_to_id.insert(name, id);
        self.id_to_index.insert(id, index);
        true
    }

    /// Get the registered file name for a `SourceId`.
    pub fn name(&self, id: SourceId) -> Option<&str> {
        self.get_source(id).map(|(name, _)| name)
    }

    /// Try to create a `Span` from a `SourceId` and byte offsets.
    ///
    /// Returns `None` when the source is not registered in this map, matching
    /// the registered-source semantics of [`SourceMap::try_location_to_span`].
    pub fn try_span(&self, source: SourceId, start: usize, end: usize) -> Option<Span> {
        self.get_source(source)?;
        Some(Span::from_offsets(source, start, end))
    }

    /// Try to create a Span from a file name and byte offsets.
    pub fn try_location_to_span(&self, file_name: &str, start: usize, end: usize) -> Option<Span> {
        let source_id = self.get_id(file_name)?;
        Some(Span::from_offsets(source_id, start, end))
    }

    /// Snapshot file-name to source-id mappings.
    pub fn source_ids(&self) -> HashMap<String, SourceId> {
        self.name_to_id.clone()
    }

    /// Return a copy that preserves source-id/name mappings but omits source text.
    pub fn without_source_contents(&self) -> Self {
        let files = self
            .files
            .iter()
            .map(|(id, name, _)| (*id, name.clone(), Arc::<str>::from("")))
            .collect();
        Self {
            files,
            name_to_id: self.name_to_id.clone(),
            id_to_index: self.id_to_index.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Diagnostic, Label, PrimaryLabel};

    #[test]
    fn try_location_to_span_does_not_misattribute_unknown_files_to_source_zero() {
        let mut source_map = SourceMap::new();
        let source = source_map.add("known.mo", "model Known end Known;");

        assert_eq!(
            source_map.try_location_to_span("known.mo", 1, 5),
            Some(Span::from_offsets(source, 1, 5))
        );
        assert_eq!(source_map.try_location_to_span("missing.mo", 1, 5), None);
    }

    #[test]
    fn source_ids_are_stable_across_source_map_insertion_order() {
        let mut first_order = SourceMap::new();
        let first_known = first_order.add("known.mo", "model Known end Known;");
        first_order.add("other.mo", "model Other end Other;");

        let mut second_order = SourceMap::new();
        second_order.add("other.mo", "model Other end Other;");
        let second_known = second_order.add("known.mo", "model Known end Known;");

        assert_eq!(first_known, second_known);
        assert_eq!(first_known, SourceId::from_source_name("known.mo"));
    }

    #[test]
    fn miette_report_uses_primary_label_source_before_first_label() {
        let mut source_map = SourceMap::new();
        let first = source_map.add("first.mo", "model First end First;");
        let second = source_map.add("second.mo", "model Second end Second;");
        let diagnostic = Diagnostic::error(
            "E000",
            "primary source selection",
            PrimaryLabel::new(Span::from_offsets(second, 6, 12)).with_message("primary source"),
        )
        .with_label(Label::secondary(Span::from_offsets(first, 0, 5)).with_message("secondary"));

        let report = diagnostic.to_miette_with_source_map(&source_map);
        assert_eq!(report.labels.len(), 1);
        assert_eq!(report.labels[0].label(), Some("primary source"));
        assert_eq!(report.labels[0].offset(), 6);
    }

    #[test]
    fn try_span_rejects_unregistered_source() {
        let map = SourceMap::new();
        assert!(
            map.try_span(SourceId::from_source_name("missing.mo"), 0, 1)
                .is_none()
        );
    }

    #[test]
    fn try_span_matches_try_location_to_span() {
        let mut map = SourceMap::new();
        map.add("pkg/A.mo", "model A end A;");
        let source = SourceId::from_source_name("pkg/A.mo");
        assert_eq!(
            map.try_span(source, 6, 7),
            map.try_location_to_span("pkg/A.mo", 6, 7)
        );
        assert_eq!(
            map.try_span(source, 6, 7),
            Some(Span::from_offsets(source, 6, 7))
        );
    }

    #[test]
    fn name_resolves_registered_source_id() {
        let mut map = SourceMap::new();
        let id = map.add("pkg/A.mo", "model A end A;");
        assert_eq!(map.name(id), Some("pkg/A.mo"));
        assert_eq!(map.name(SourceId::from_source_name("other.mo")), None);
    }

    #[test]
    fn every_registered_name_derives_its_source_id() {
        let mut map = SourceMap::new();
        map.add("pkg/A.mo", "model A end A;");
        let parser_id = SourceId::from_source_name("original/path/A.mo");
        assert!(map.register_id(parser_id, "<parsed-source-root>", Arc::from("body")));
        for (id, name, _) in &map.files {
            assert_eq!(
                source_id_for_name(name),
                *id,
                "entry `{name}` is filed under a name that does not derive its id"
            );
            assert_eq!(map.get_id(name), Some(*id));
        }
        assert_eq!(
            map.try_span(parser_id, 0, 3),
            Some(Span::from_offsets(parser_id, 0, 3))
        );
        assert!(!map.register_id(parser_id, "other", Arc::from("")));
        assert_eq!(
            map.get_source(parser_id).map(|(_, text)| text),
            Some("body")
        );
    }

    #[test]
    fn placeholder_source_names_round_trip_to_their_id() {
        let interior = SourceId::from_source_name("pkg/RoundTrip.mo");
        for id in [SourceId::DUMMY, interior, SourceId(u64::MAX)] {
            let name = placeholder_source_name(id);
            assert_eq!(source_id_for_name(&name), id, "{name}");
        }
        assert_eq!(
            source_id_for_name("pkg/A.mo"),
            SourceId::from_source_name("pkg/A.mo")
        );
    }

    #[test]
    fn register_id_display_name_never_shadows_a_real_file() {
        let mut map = SourceMap::new();
        let document_id = map.add("<parsed-source-root>", "model A end A;");
        let parser_id = SourceId::from_source_name("original/path/A.mo");
        assert_ne!(document_id, parser_id);
        assert!(map.register_id(parser_id, "<parsed-source-root>", Arc::from("")));

        assert_eq!(map.get_id("<parsed-source-root>"), Some(document_id));
        let encoded = serde_json::to_vec(&map).expect("source map serializes");
        let map: SourceMap =
            serde_json::from_slice(&encoded).expect("canonical source map deserializes");
        assert_eq!(
            map.get_id("<parsed-source-root>"),
            Some(document_id),
            "serialization must not change what a file name resolves to"
        );
        assert_eq!(
            map.get_source(document_id).map(|(_, text)| text),
            Some("model A end A;")
        );
        assert!(map.get_source(parser_id).is_some());
    }

    #[test]
    fn duplicate_name_registration_keeps_the_first_entry() {
        let mut map = SourceMap::new();
        let first = map.add("pkg/A.mo", "model A end A;");
        assert_eq!(map.add("pkg/A.mo", "replacement"), first);
        assert!(!map.register_id(first, "pkg/A.mo", Arc::from("replacement")));
        assert_eq!(
            map.get_source(first).map(|(_, text)| text),
            Some("model A end A;")
        );
        assert_eq!(map.files.len(), 1);
    }

    #[test]
    fn source_map_json_and_bincode_round_trip_rebuild_canonical_indexes() {
        let mut map = SourceMap::new();
        let first = map.add("pkg/A.mo", "model A end A;");
        let second = SourceId::from_source_name("original/path/B.mo");
        assert!(map.register_id(second, "<parsed-source-root>", Arc::from("model B end B;")));

        let json = serde_json::to_vec(&map).expect("source map serializes as JSON");
        let from_json: SourceMap =
            serde_json::from_slice(&json).expect("canonical JSON source map deserializes");
        let binary = bincode::serialize(&map).expect("source map serializes as bincode");
        let from_binary: SourceMap =
            bincode::deserialize(&binary).expect("canonical bincode source map deserializes");
        let second_name = placeholder_source_name(second);

        for decoded in [&from_json, &from_binary] {
            assert_eq!(decoded.get_id("pkg/A.mo"), Some(first));
            assert_eq!(decoded.name(first), Some("pkg/A.mo"));
            assert_eq!(
                decoded.get_source(second),
                Some((second_name.as_str(), "model B end B;"))
            );
        }
    }

    #[test]
    fn source_map_resolves_normalized_separator_alias_without_an_index_scan() {
        let mut map = SourceMap::new();
        let id = map.add("pkg/A.mo", "model A end A;");

        assert_eq!(map.get_id(r"pkg\A.mo"), Some(id));

        let encoded = serde_json::to_vec(&map).expect("source map serializes");
        let decoded: SourceMap =
            serde_json::from_slice(&encoded).expect("canonical source map deserializes");
        assert_eq!(decoded.get_id(r"pkg\A.mo"), Some(id));
    }

    #[test]
    fn source_map_deserialize_rejects_mismatched_name_identity() {
        let stored = SourceId::from_source_name("pkg/A.mo");
        let malformed = serde_json::json!({
            "files": [[stored, "pkg/B.mo", "model B end B;"]]
        });

        let error = serde_json::from_value::<SourceMap>(malformed)
            .expect_err("a source name cannot claim another source identity");

        assert!(error.to_string().contains("not stored identity"));
    }

    #[test]
    fn source_map_deserialize_rejects_duplicate_name_and_identity() {
        let id = SourceId::from_source_name("pkg/A.mo");
        let duplicate_name = serde_json::json!({
            "files": [
                [id, "pkg/A.mo", "first"],
                [id, "pkg/A.mo", "second"],
            ]
        });
        let duplicate_identity = serde_json::json!({
            "files": [
                [id, "pkg/A.mo", "first"],
                [id, placeholder_source_name(id), "second"],
            ]
        });

        let name_error = serde_json::from_value::<SourceMap>(duplicate_name)
            .expect_err("duplicate source names are not canonical");
        let identity_error = serde_json::from_value::<SourceMap>(duplicate_identity)
            .expect_err("duplicate source identities are not canonical");

        assert!(name_error.to_string().contains("duplicate source map name"));
        assert!(
            identity_error
                .to_string()
                .contains("duplicate source map identity")
        );
    }
}
