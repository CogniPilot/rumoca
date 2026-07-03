//! eFMI packaging module (eFMI Standard 1.0.0 Beta 1, ch. 2 and §3.1).
//!
//! Typed models of `__content.xml` ([`content`]), the schema parts shared by
//! all manifest kinds ([`manifest_common`]), and the Algorithm Code manifest
//! ([`algorithm_code_manifest`]); deterministic XML serialization ([`xml`]),
//! SHA-1 checksums over exact raw bytes ([`checksum`]), and
//! UUID/timestamp/id discipline ([`ids`]). Errors are one typed enum with
//! stable codes ([`diagnostic`]).
//!
//! This crate is representation-agnostic packaging: it has no Rumoca IR or
//! GALEC dependencies (GAL-010). The vendored Beta-1 XSDs under
//! `assets/efmi-schemas/` are ground truth for every field (GAL-023); the
//! [`container`] module writes the eFMU layout (directory and `.efmu` zip
//! forms, `schemas/` populated from the vendored assets), and
//! [`xml::validate_against_xsd`] validates emitted XML via `xmllint`.
//!
//! Construction validates eagerly (SPEC_0008: fail early, no silent
//! defaults); serialization is a pure function of validated models, so
//! emitted bytes — and therefore the SHA-1 checksums that make an eFMU valid
//! (GAL-021) — are reproducible. See `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`.

pub mod algorithm_code_manifest;
pub mod checksum;
pub mod container;
pub mod content;
pub mod diagnostic;
pub mod ids;
pub mod manifest_common;
pub mod xml;

/// Fixed `efmiVersion` attribute value of eFMI Standard 1.0.0 (GAL-022).
pub const EFMI_VERSION: &str = "1.0.0";

/// eFMI profile string for the pinned Beta-1 schema set (GAL-022).
pub const EFMI_PROFILE: &str = "efmi-1.0.0-beta-1";

/// Fixed `xsdVersion` of `efmiContainerManifest.xsd` in Beta-1 (GAL-022).
pub const CONTAINER_MANIFEST_XSD_VERSION: &str = "0.11.0";

/// Fixed `xsdVersion` of `AlgorithmCode/efmiAlgorithmCodeManifest.xsd` in
/// Beta-1 (GAL-022).
pub const ALGORITHM_CODE_MANIFEST_XSD_VERSION: &str = "0.14.0";

/// Fixed `xsdVersion` of `ProductionCode/efmiProductionCodeManifest.xsd` in
/// Beta-1 (GAL-022); reserved for the Production Code rung of the
/// conformance ladder.
pub const PRODUCTION_CODE_MANIFEST_XSD_VERSION: &str = "0.17.0";

pub use checksum::Sha1Hex;
pub use container::{
    EfmuLayout, EfmuMeta, ModelRepresentationFiles, RepresentationFile, WrittenRepresentation,
    write_efmu_container, write_efmu_zip,
};
pub use diagnostic::EfmiError;
pub use ids::{
    FilePath, IdRegistry, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, UtcTimestamp,
};
pub use xml::{algorithm_code_manifest_to_xml, content_to_xml, validate_against_xsd};
