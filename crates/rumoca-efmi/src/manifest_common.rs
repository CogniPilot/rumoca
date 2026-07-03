//! Typed models of the schema parts shared by *all* manifest kinds:
//! the `efmiManifestAttributesBase` attribute group
//! ([`ManifestAttributes`]), `efmiFiles.xsd` ([`File`], [`FileChecksum`],
//! [`FileRole`]), `efmiUnits.xsd` ([`Unit`], [`BaseUnit`]), and
//! `efmiAnnotation.xsd` ([`Annotation`]).
//!
//! These XSDs sit at the top level of the vendored schema tree and are
//! included by every manifest kind (Algorithm Code today; Production Code and
//! the others when their rungs land), so their models live here rather than
//! in any kind-specific module.
//!
//! `description` attributes are `xs:string` in the XSDs but are modeled as
//! [`NormalizedText`] here: XML attribute-value normalization silently
//! replaces tabs/newlines with spaces on re-parse, so accepting them would
//! serialize bytes that do not round-trip (and XML-illegal control
//! characters would make the emitted bytes non-well-formed). An absent
//! description is `None`, never an empty string.

use crate::checksum::Sha1Hex;
use crate::ids::{
    FilePath, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, UtcTimestamp,
};

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
    /// Brief description (`description`, optional; see the module docs for
    /// why this is normalized despite the XSD's `xs:string`).
    pub description: Option<NormalizedText>,
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

/// One `File` entry of the `Files` list (`efmiFiles.xsd`).
#[derive(Debug, Clone, PartialEq)]
pub struct File {
    /// File id (`id`, required), referenced by `fileRefId` attributes.
    pub id: Identifier,
    /// File name including suffix (`name`, required). The XSD type is plain
    /// `xs:normalizedString`, but a file name containing a path separator
    /// can never resolve inside a container, so the slash-free newtype is
    /// the faithful model.
    pub name: NameWithoutSlashes,
    /// Directory part relative to the container root (`path`, required).
    pub path: FilePath,
    /// Checksum policy; encodes the XSD rule that `checksum` must be given
    /// if, and only if, `needsChecksum="true"`.
    pub checksum: FileChecksum,
    /// Role of the file (`role`, required).
    pub role: FileRole,
    /// Free-text description (`description`, optional), especially for
    /// [`FileRole::Other`].
    pub description: Option<NormalizedText>,
}

/// Checksum policy of a [`File`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FileChecksum {
    /// `needsChecksum="true"` with the real SHA-1 of the file's raw bytes.
    Sha1(Sha1Hex),
    /// `needsChecksum="false"`: the file does not participate in checksum
    /// calculation.
    NotNeeded,
}

/// `FileRole` enumeration (`efmiFiles.xsd`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FileRole {
    /// Code file of the representation (the `.alg` file here).
    Code,
    /// The manifest file itself.
    Manifest,
    /// The one-and-only zipped FMU of this representation.
    Fmu,
    /// Unzipped FMU content.
    FmuFolder,
    /// Reference data (e.g. CSV of variable reference values).
    ReferenceData,
    /// Anything else; describe via `description`.
    Other,
}

impl FileRole {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Code => "Code",
            Self::Manifest => "Manifest",
            Self::Fmu => "FMU",
            Self::FmuFolder => "FMUFolder",
            Self::ReferenceData => "ReferenceData",
            Self::Other => "other",
        }
    }
}

/// `BaseUnit` element: SI exponents plus `factor`/`offset`
/// (`BaseUnit_value = factor*Unit_value + offset`).
#[derive(Debug, Clone, PartialEq)]
pub struct BaseUnit {
    /// Exponent of SI base unit "kg".
    pub kg: i32,
    /// Exponent of SI base unit "m".
    pub m: i32,
    /// Exponent of SI base unit "s".
    pub s: i32,
    /// Exponent of SI base unit "A".
    pub ampere: i32,
    /// Exponent of SI base unit "K".
    pub kelvin: i32,
    /// Exponent of SI base unit "mol".
    pub mol: i32,
    /// Exponent of SI base unit "cd".
    pub cd: i32,
    /// Exponent of SI derived unit "rad".
    pub rad: i32,
    /// Conversion factor (XSD default 1). Must be finite.
    pub factor: f64,
    /// Conversion offset (XSD default 0). Must be finite.
    pub offset: f64,
}

impl BaseUnit {
    /// Whether this decomposition is exactly the SI second (`s^1`, all other
    /// exponents zero, identity conversion) — the only unit the manifest
    /// `Clock` sample period may carry (§3.1.2, SPEC_0034 D6).
    pub fn is_seconds(&self) -> bool {
        let Self {
            kg,
            m,
            s,
            ampere,
            kelvin,
            mol,
            cd,
            rad,
            factor,
            offset,
        } = self;
        [kg, m, ampere, kelvin, mol, cd, rad]
            .iter()
            .all(|e| **e == 0)
            && *s == 1
            && *factor == 1.0
            && *offset == 0.0
    }
}

impl Default for BaseUnit {
    fn default() -> Self {
        Self {
            kg: 0,
            m: 0,
            s: 0,
            ampere: 0,
            kelvin: 0,
            mol: 0,
            cd: 0,
            rad: 0,
            factor: 1.0,
            offset: 0.0,
        }
    }
}

/// One `Unit` definition (`efmiUnits.xsd`; FMI 3.0 units minus display units,
/// plus a required `id`).
#[derive(Debug, Clone, PartialEq)]
pub struct Unit {
    /// Unit id (`id`, required), referenced by `unitRefId`.
    pub id: Identifier,
    /// Unit name, unique within the `Units` list (`name`, required).
    pub name: NormalizedText,
    /// Optional SI decomposition.
    pub base_unit: Option<BaseUnit>,
}

/// One vendor `Annotation` (`efmiAnnotation.xsd`). Nested vendor XML content
/// is not yet modeled; the element is emitted empty.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    /// Reverse-domain tool name (`type`, required), unique within its list.
    /// `org.modelica` and `org.efmi-standard` domains are reserved.
    pub annotation_type: NormalizedText,
}
