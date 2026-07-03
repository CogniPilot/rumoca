//! Typed model of `__content.xml` (`efmiContainerManifest.xsd`, 0.11.0).
//!
//! `__content.xml` is the registry of all model representations in an eFMU
//! container. Field-by-field from the XSD; the fixed attributes
//! (`xsdVersion="0.11.0"`, `efmiVersion="1.0.0"`) are not modeled as fields —
//! the serializer emits them as constants, so wrong versions are
//! unrepresentable (GAL-022).

use std::collections::BTreeSet;

use crate::checksum::Sha1Hex;
use crate::diagnostic::EfmiError;
use crate::ids::{ManifestId, NameWithoutSlashes, NormalizedText, UtcTimestamp};

/// Shared top-level manifest attribute group (`efmiManifestAttributesBase`).
///
/// Used by `__content.xml` and by every model representation manifest.
/// The fixed `efmiVersion="1.0.0"` attribute is emitted by the serializer
/// and is deliberately not a field.
#[derive(Debug, Clone, PartialEq)]
pub struct ManifestAttributes {
    /// Brace-wrapped UUID of this manifest file (`id`, required).
    pub id: ManifestId,
    /// Name of the block/eFMU as in the source environment (`name`, required).
    pub name: NormalizedText,
    /// Brief description (`description`, optional; unrestricted `xs:string`).
    pub description: Option<String>,
    /// Block version (`version`, optional).
    pub version: Option<NormalizedText>,
    /// Last modification of the manifest or any contents of its container
    /// (`generationDateAndTime`, required, strict UTC).
    pub generation_date_and_time: UtcTimestamp,
    /// Generating tool; `"manual"` if hand-made (`generationTool`, optional).
    pub generation_tool: Option<NormalizedText>,
    /// Copyright info (`copyright`, optional).
    pub copyright: Option<NormalizedText>,
    /// License info (`license`, optional).
    pub license: Option<NormalizedText>,
}

/// `ModelRepresentation` entry: one registered container.
#[derive(Debug, Clone, PartialEq)]
pub struct ModelRepresentation {
    /// Unique container name; also the container's root directory name
    /// relative to `__content.xml` (`name`, required).
    pub name: NameWithoutSlashes,
    /// Representation kind (`kind`, required).
    pub kind: ModelRepresentationKind,
    /// Manifest file name in the container's root directory (`manifest`, required).
    pub manifest: NameWithoutSlashes,
    /// SHA-1 of the raw bytes of that manifest file (`checksum`, required).
    /// Must be the real digest — a wrong checksum makes the eFMU invalid
    /// (GAL-021), so never store a placeholder here.
    pub checksum: Sha1Hex,
    /// The manifest's own UUID, duplicated here to speed up reference
    /// resolution (`manifestRefId`, required).
    pub manifest_ref_id: ManifestId,
}

/// `ModelRepresentationKind` enumeration (`efmiModelRepresentationKind.xsd`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModelRepresentationKind {
    /// Algorithm Code (GALEC + manifest); exactly one per eFMU.
    AlgorithmCode,
    /// Behavioral Model (tests/reference results).
    BehavioralModel,
    /// Production Code (generated C).
    ProductionCode,
    /// Binary Code (compiled objects).
    BinaryCode,
}

impl ModelRepresentationKind {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::AlgorithmCode => "AlgorithmCode",
            Self::BehavioralModel => "BehavioralModel",
            Self::ProductionCode => "ProductionCode",
            Self::BinaryCode => "BinaryCode",
        }
    }
}

/// Unvalidated field set for [`Content`]. Assemble freely, then validate via
/// [`Content::new`].
#[derive(Debug, Clone, PartialEq)]
pub struct ContentParts {
    /// Shared top-level attributes of `__content.xml` itself.
    pub attributes: ManifestAttributes,
    /// Name of the representation whose FMU is currently unpacked at the
    /// container root (`activeFmu`, optional). Must not be set otherwise.
    pub active_fmu: Option<NameWithoutSlashes>,
    /// Registered model representations, in emission order.
    pub model_representations: Vec<ModelRepresentation>,
}

/// Validated `__content.xml` model. Construction enforces (fail-early):
///
/// - representation names unique (they double as directory names);
/// - `manifestRefId`s unique and distinct from the content's own id;
/// - exactly one `AlgorithmCode` representation (eFMI ch. 2 cardinality rule);
/// - `activeFmu`, when set, names a registered representation.
#[derive(Debug, Clone, PartialEq)]
pub struct Content(ContentParts);

impl Content {
    /// Validate the parts and construct the model.
    pub fn new(parts: ContentParts) -> Result<Self, EfmiError> {
        validate_representations(&parts)?;
        Ok(Self(parts))
    }

    /// The validated field set.
    pub fn parts(&self) -> &ContentParts {
        &self.0
    }
}

fn validate_representations(parts: &ContentParts) -> Result<(), EfmiError> {
    let mut names: BTreeSet<&str> = BTreeSet::new();
    let mut ref_ids: BTreeSet<String> = BTreeSet::new();
    ref_ids.insert(parts.attributes.id.to_string());
    let mut algorithm_code_count = 0usize;
    for representation in &parts.model_representations {
        if !names.insert(representation.name.as_str()) {
            return Err(EfmiError::DuplicateRepresentationName {
                name: representation.name.as_str().to_owned(),
            });
        }
        let ref_id = representation.manifest_ref_id.to_string();
        if !ref_ids.insert(ref_id.clone()) {
            return Err(EfmiError::DuplicateId { id: ref_id });
        }
        if representation.kind == ModelRepresentationKind::AlgorithmCode {
            algorithm_code_count += 1;
        }
    }
    if algorithm_code_count != 1 {
        return Err(EfmiError::AlgorithmCodeCardinality {
            count: algorithm_code_count,
        });
    }
    if let Some(active) = &parts.active_fmu
        && !names.contains(active.as_str())
    {
        return Err(EfmiError::UnknownActiveFmu {
            name: active.as_str().to_owned(),
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn attributes() -> ManifestAttributes {
        ManifestAttributes {
            id: ManifestId::parse("{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}").unwrap(),
            name: NormalizedText::new("TestBlock").unwrap(),
            description: None,
            version: None,
            generation_date_and_time: UtcTimestamp::parse("2026-07-02T12:00:00Z").unwrap(),
            generation_tool: None,
            copyright: None,
            license: None,
        }
    }

    fn representation(
        name: &str,
        kind: ModelRepresentationKind,
        uuid: &str,
    ) -> ModelRepresentation {
        ModelRepresentation {
            name: NameWithoutSlashes::new(name).unwrap(),
            kind,
            manifest: NameWithoutSlashes::new("manifest.xml").unwrap(),
            checksum: Sha1Hex::of_bytes(b"abc"),
            manifest_ref_id: ManifestId::parse(uuid).unwrap(),
        }
    }

    const UUID_A: &str = "{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}";
    const UUID_B: &str = "{aa6b1a52-4f0e-4d59-9d3b-215f8e5b6a21}";

    #[test]
    fn accepts_single_algorithm_code() {
        let content = Content::new(ContentParts {
            attributes: attributes(),
            active_fmu: None,
            model_representations: vec![representation(
                "AlgorithmCode",
                ModelRepresentationKind::AlgorithmCode,
                UUID_A,
            )],
        });
        assert!(content.is_ok());
    }

    #[test]
    fn rejects_missing_or_extra_algorithm_code() {
        let err = Content::new(ContentParts {
            attributes: attributes(),
            active_fmu: None,
            model_representations: vec![],
        })
        .unwrap_err();
        assert_eq!(err, EfmiError::AlgorithmCodeCardinality { count: 0 });

        let err = Content::new(ContentParts {
            attributes: attributes(),
            active_fmu: None,
            model_representations: vec![
                representation("A1", ModelRepresentationKind::AlgorithmCode, UUID_A),
                representation("A2", ModelRepresentationKind::AlgorithmCode, UUID_B),
            ],
        })
        .unwrap_err();
        assert_eq!(err, EfmiError::AlgorithmCodeCardinality { count: 2 });
    }

    #[test]
    fn rejects_duplicate_names_and_ref_ids() {
        let err = Content::new(ContentParts {
            attributes: attributes(),
            active_fmu: None,
            model_representations: vec![
                representation("Same", ModelRepresentationKind::AlgorithmCode, UUID_A),
                representation("Same", ModelRepresentationKind::ProductionCode, UUID_B),
            ],
        })
        .unwrap_err();
        assert_eq!(err.code(), "EFM011");

        let err = Content::new(ContentParts {
            attributes: attributes(),
            active_fmu: None,
            model_representations: vec![
                representation("A1", ModelRepresentationKind::AlgorithmCode, UUID_A),
                representation("P1", ModelRepresentationKind::ProductionCode, UUID_A),
            ],
        })
        .unwrap_err();
        assert_eq!(err.code(), "EFM008");
    }

    #[test]
    fn rejects_unknown_active_fmu() {
        let err = Content::new(ContentParts {
            attributes: attributes(),
            active_fmu: Some(NameWithoutSlashes::new("Nope").unwrap()),
            model_representations: vec![representation(
                "AlgorithmCode",
                ModelRepresentationKind::AlgorithmCode,
                UUID_A,
            )],
        })
        .unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnknownActiveFmu {
                name: "Nope".into()
            }
        );
    }
}
