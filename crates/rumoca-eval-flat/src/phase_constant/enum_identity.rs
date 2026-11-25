//! Structured enumeration identity for flat constant evaluation.
//!
//! MLS 3.7 §4.8.5 identifies an enumeration value by its enumeration *type*
//! together with its *literal*. Both halves are compiler-owned data, so they
//! are kept as separate structured fields here instead of being recovered from
//! rendered display text: a rendered component path is not a plain dotted
//! chain (`bus[data.medium].Types.Init` contains dots that are not segment
//! boundaries), so chopping at a dot can invent an enumeration type that the
//! model never declared.
//!
//! Path segmentation happens exactly once, through [`ComponentPath`], which
//! reuses the name interner's top-level segment boundaries. Everything after
//! that carries [`EnumLiteralIdentity`].

use rumoca_core::ComponentPath;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::constant::Value;

/// Enumeration value identity: enumeration type path plus literal name.
#[derive(Clone, Debug)]
pub(crate) struct EnumLiteralIdentity {
    type_path: ComponentPath,
    literal: String,
}

impl EnumLiteralIdentity {
    /// Build an identity from already-segmented path segments.
    ///
    /// The final segment is the enumeration literal (MLS §4.8.5); the
    /// preceding segments are the enumeration type path. A single-segment path
    /// carries no enumeration type prefix, so its type path is the root path.
    fn from_segments(segments: &[String]) -> Option<Self> {
        let (literal, type_segments) = segments.split_last()?;
        Some(Self {
            type_path: ComponentPath::from_parts(type_segments.iter().cloned()),
            literal: literal.clone(),
        })
    }

    /// Enumeration type path; empty when the literal carried no type prefix.
    fn type_name(&self) -> &str {
        self.type_path.as_str()
    }

    /// Structured evaluation value for this enumeration identity.
    pub(crate) fn to_value(&self) -> Value {
        Value::Enum(self.type_name().to_string(), self.literal.clone())
    }

    /// Flat rendering `<type path>.<literal>` for name-keyed evaluation maps.
    pub(crate) fn to_flat_string(&self) -> String {
        self.type_path
            .join_part_slice(std::slice::from_ref(&self.literal))
            .to_flat_string()
    }
}

/// Canonicalize a potentially partially-qualified enum literal using known enum values.
///
/// Example:
/// - known value: `Modelica.Fluid.Types.ModelStructure.a_vb`
/// - literal: `pipe.Types.ModelStructure.a_vb`
/// - canonicalized: `Modelica.Fluid.Types.ModelStructure.a_vb`
///
/// This preserves MLS §4.9.5 enum identity across equivalent qualification paths.
pub fn canonicalize_enum_literal(literal: &str, known_enums: &FxHashMap<String, String>) -> String {
    EnumCanonicalizer::new(known_enums)
        .canonicalize(literal)
        .map_or_else(|| literal.to_string(), |identity| identity.to_flat_string())
}

#[derive(Clone)]
struct EnumSuffixCandidate {
    segments: Vec<String>,
    ambiguous: bool,
}

/// Segment-aware suffix index for enum identity recovery.
///
/// Building this once avoids the former O(E²) rescanning and reparsing of the
/// complete enum inventory while constructing each parameter evaluation
/// context. Keys are segment slices, never rendered text, so `NotTypes.X`
/// cannot match `Types.X`.
pub(crate) struct EnumCanonicalizer {
    suffixes: FxHashMap<Vec<String>, EnumSuffixCandidate>,
    /// The literals the class tree declares, by each suffix of their
    /// canonical paths; a suffix two declared literals share is ambiguous.
    declared: FxHashMap<Vec<String>, EnumSuffixCandidate>,
}

impl EnumCanonicalizer {
    pub(crate) fn new(known_enums: &FxHashMap<String, String>) -> Self {
        let values: FxHashSet<&str> = known_enums.values().map(String::as_str).collect();
        let mut suffixes: FxHashMap<Vec<String>, EnumSuffixCandidate> = FxHashMap::default();
        for value in values {
            let segments = ComponentPath::from_flat_path(value).into_parts();
            for start in 0..segments.len().saturating_sub(1) {
                merge_enum_suffix_candidate(&mut suffixes, segments[start..].to_vec(), &segments);
            }
        }
        Self {
            suffixes,
            declared: FxHashMap::default(),
        }
    }

    /// Prefer the literal the class tree declares (MLS 3.7 §4.9.5): a
    /// spelling relative to an import or an enclosing scope (`MS.c` read in
    /// `pipe`, `Mode.Slow` inside `Types`) names the declared literal whose
    /// canonical path ends with it, when exactly one does.
    pub(crate) fn with_declared<'a>(mut self, declared: impl IntoIterator<Item = &'a str>) -> Self {
        for path in declared {
            let segments = ComponentPath::from_flat_path(path).into_parts();
            for start in 0..segments.len().saturating_sub(1) {
                merge_declared_suffix(&mut self.declared, segments[start..].to_vec(), &segments);
            }
        }
        self
    }

    /// Resolve a possibly partially-qualified enum literal to structured identity.
    ///
    /// This is the single adapter for enum values that reach this crate as
    /// rendered text (`FlattenContext::enum_parameter_values`): the path is
    /// segmented once here and every later consumer works on the resulting
    /// type-path/literal pair. Returns `None` for an empty path, which carries
    /// no enumeration identity at all.
    pub(crate) fn canonicalize(&self, literal: &str) -> Option<EnumLiteralIdentity> {
        let segments = ComponentPath::from_flat_path(literal).into_parts();

        // The declared literal first, then the held values, each by
        // progressively shorter structured suffixes.
        for start in 0..segments.len().saturating_sub(1) {
            if let Some(candidate) = self.declared.get(&segments[start..])
                && !candidate.ambiguous
            {
                return EnumLiteralIdentity::from_segments(&candidate.segments);
            }
        }
        for start in 0..segments.len().saturating_sub(1) {
            if let Some(candidate) = self.suffixes.get(&segments[start..])
                && !candidate.ambiguous
            {
                return EnumLiteralIdentity::from_segments(&candidate.segments);
            }
        }

        EnumLiteralIdentity::from_segments(&segments)
    }

    /// The identity of `path` when it is itself the canonical path of a literal
    /// some known enumeration value holds: exactly the paths `canonicalize` maps
    /// a held value to, since that canonical form is the longest held value
    /// sharing its suffix and so maps to itself.
    pub(crate) fn held_canonical(&self, path: &str) -> Option<EnumLiteralIdentity> {
        let segments = ComponentPath::from_flat_path(path).into_parts();
        self.suffixes
            .get(&segments[..])
            .filter(|candidate| !candidate.ambiguous && candidate.segments == segments)
            .and_then(|candidate| EnumLiteralIdentity::from_segments(&candidate.segments))
    }
}

/// Index one suffix of a declared literal's canonical path; a suffix two
/// different declared literals share names neither.
fn merge_declared_suffix(
    declared: &mut FxHashMap<Vec<String>, EnumSuffixCandidate>,
    suffix: Vec<String>,
    segments: &[String],
) {
    match declared.get_mut(&suffix) {
        Some(candidate) if candidate.segments != segments => candidate.ambiguous = true,
        Some(_) => {}
        None => {
            declared.insert(
                suffix,
                EnumSuffixCandidate {
                    segments: segments.to_vec(),
                    ambiguous: false,
                },
            );
        }
    }
}

fn merge_enum_suffix_candidate(
    suffixes: &mut FxHashMap<Vec<String>, EnumSuffixCandidate>,
    suffix: Vec<String>,
    segments: &[String],
) {
    let replacement = EnumSuffixCandidate {
        segments: segments.to_vec(),
        ambiguous: false,
    };
    let Some(candidate) = suffixes.get_mut(&suffix) else {
        suffixes.insert(suffix, replacement);
        return;
    };
    if segments.len() > candidate.segments.len() {
        *candidate = replacement;
    } else if segments.len() == candidate.segments.len() && candidate.segments != segments {
        candidate.ambiguous = true;
    }
}

#[cfg(test)]
mod tests {
    use super::{EnumCanonicalizer, EnumLiteralIdentity};
    use crate::constant::Value;
    use rustc_hash::FxHashMap;

    fn known_enums(entries: &[(&str, &str)]) -> FxHashMap<String, String> {
        entries
            .iter()
            .map(|(name, value)| ((*name).to_string(), (*value).to_string()))
            .collect()
    }

    fn identity(inventory: &FxHashMap<String, String>, literal: &str) -> EnumLiteralIdentity {
        EnumCanonicalizer::new(inventory)
            .canonicalize(literal)
            .expect("enum literal has identity")
    }

    #[test]
    fn a_relative_spelling_names_the_one_declared_literal_ending_with_it() {
        let held = known_enums(&[("q", "Mode.Slow")]);
        let canonicalizer = EnumCanonicalizer::new(&held).with_declared([
            "Types.Mode.Fast",
            "Types.Mode.Slow",
            "P.Types.MS.c",
        ]);
        let value = |spelling: &str| {
            canonicalizer
                .canonicalize(spelling)
                .expect("enum literal has identity")
                .to_value()
        };
        assert_eq!(
            value("Mode.Slow"),
            Value::Enum("Types.Mode".to_string(), "Slow".to_string())
        );
        assert_eq!(
            value("pipe.MS.c"),
            Value::Enum("P.Types.MS".to_string(), "c".to_string())
        );
    }

    #[test]
    fn a_spelling_two_declared_literals_end_with_is_not_decided_by_declaration() {
        let canonicalizer =
            EnumCanonicalizer::new(&known_enums(&[])).with_declared(["A.Mode.Slow", "B.Mode.Slow"]);
        assert_eq!(
            canonicalizer
                .canonicalize("Mode.Slow")
                .expect("enum literal has identity")
                .to_value(),
            Value::Enum("Mode".to_string(), "Slow".to_string()),
            "ambiguous by declaration, so the spelling stands as before",
        );
        assert_eq!(
            canonicalizer
                .canonicalize("A.Mode.Slow")
                .expect("enum literal has identity")
                .to_value(),
            Value::Enum("A.Mode".to_string(), "Slow".to_string())
        );
    }

    #[test]
    fn nested_enum_type_path_is_kept_whole() {
        let inventory = known_enums(&[("init", "Modelica.Blocks.Types.Init.NoInit")]);

        let resolved = identity(&inventory, "controller.Types.Init.NoInit");

        assert_eq!(
            resolved.to_value(),
            Value::Enum(
                "Modelica.Blocks.Types.Init".to_string(),
                "NoInit".to_string()
            ),
            "MLS §4.8.5 identity is the complete enumeration type path plus literal",
        );
        assert_eq!(
            resolved.to_flat_string(),
            "Modelica.Blocks.Types.Init.NoInit"
        );
    }

    #[test]
    fn subscripted_segment_is_not_treated_as_enum_type_boundary() {
        let inventory = known_enums(&[]);

        // A rendered path is not a plain dotted chain: the dot inside the
        // subscript is not a segment boundary, so it must not become the
        // enumeration type/literal split point.
        let resolved = identity(&inventory, "bus[data.medium]");

        assert_eq!(
            resolved.to_value(),
            Value::Enum(String::new(), "bus[data.medium]".to_string()),
            "a subscript dot must not invent the enumeration type `bus[data`",
        );
    }

    #[test]
    fn scope_qualified_literal_through_subscripted_instance_resolves() {
        let inventory = known_enums(&[("init", "Modelica.Blocks.Types.Init.NoInit")]);

        let resolved = identity(&inventory, "controllers[bus.index].Types.Init.NoInit");

        assert_eq!(
            resolved.to_flat_string(),
            "Modelica.Blocks.Types.Init.NoInit"
        );
    }

    #[test]
    fn ambiguous_suffix_keeps_the_original_qualification() {
        let inventory = known_enums(&[
            ("a", "PkgA.Types.Mode.On"),
            ("b", "PkgB.Types.Mode.On"),
            ("c", "PkgA.Types.Mode.Off"),
        ]);

        let resolved = identity(&inventory, "Types.Mode.On");

        assert_eq!(
            resolved.to_value(),
            Value::Enum("Types.Mode".to_string(), "On".to_string()),
            "an ambiguous suffix must not silently pick one package",
        );
    }
}
