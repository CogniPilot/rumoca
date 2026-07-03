//! SPEC_0008-shaped typed errors for eFMI packaging.
//!
//! One phase-local enum with stable, CI-aggregatable codes. Constructors of
//! the model types in this crate validate eagerly and return these errors;
//! there are no silent defaults anywhere in the crate (GAL-007, GAL-021).
//!
//! Stable code table (append-only; never renumber):
//!
//! | Code   | Variant                        |
//! |--------|--------------------------------|
//! | EFM001 | [`EfmiError::InvalidManifestId`] |
//! | EFM002 | [`EfmiError::InvalidTimestamp`] |
//! | EFM003 | [`EfmiError::InvalidIdentifier`] |
//! | EFM004 | [`EfmiError::InvalidText`] |
//! | EFM005 | [`EfmiError::InvalidContainerName`] |
//! | EFM006 | [`EfmiError::InvalidFilePath`] |
//! | EFM007 | [`EfmiError::InvalidChecksum`] |
//! | EFM008 | [`EfmiError::DuplicateId`] |
//! | EFM009 | [`EfmiError::DuplicateVariableName`] |
//! | EFM010 | [`EfmiError::DuplicateUnitName`] |
//! | EFM011 | [`EfmiError::DuplicateRepresentationName`] |
//! | EFM012 | [`EfmiError::DuplicateAnnotationType`] |
//! | EFM013 | [`EfmiError::DuplicateSignal`] |
//! | EFM014 | [`EfmiError::EmptyVariables`] |
//! | EFM015 | [`EfmiError::InvalidDimension`] |
//! | EFM016 | [`EfmiError::StartLengthMismatch`] |
//! | EFM017 | [`EfmiError::ArrayStartOnScalar`] |
//! | EFM018 | [`EfmiError::DimensionProductOverflow`] |
//! | EFM019 | [`EfmiError::UnresolvedReference`] |
//! | EFM020 | [`EfmiError::InvalidClockVariable`] |
//! | EFM021 | [`EfmiError::InvalidVariableRange`] |
//! | EFM022 | [`EfmiError::NonFiniteReal`] |
//! | EFM023 | [`EfmiError::AlgorithmCodeCardinality`] |
//! | EFM024 | [`EfmiError::UnknownActiveFmu`] |
//! | EFM025 | [`EfmiError::XmlWrite`] |
//! | EFM026 | [`EfmiError::InvalidContainerLayout`] |
//! | EFM027 | [`EfmiError::DuplicateContainerFile`] |
//! | EFM028 | [`EfmiError::OutputDirNotEmpty`] |
//! | EFM029 | [`EfmiError::Io`] |
//! | EFM030 | [`EfmiError::Zip`] |
//! | EFM031 | [`EfmiError::XmllintUnavailable`] |
//! | EFM032 | [`EfmiError::XsdValidationFailed`] |
//! | EFM033 | [`EfmiError::NonFiniteUnitAttribute`] |
//! | EFM034 | [`EfmiError::MultipleFmuFiles`] |
//! | EFM035 | [`EfmiError::ManifestFileMissing`] |
//! | EFM036 | [`EfmiError::StaleFileChecksum`] |
//! | EFM037 | [`EfmiError::FileRefRoleNotCode`] |
//! | EFM038 | [`EfmiError::ManifestRefIdMismatch`] |
//! | EFM039 | [`EfmiError::UnresolvedForeignReference`] |
//! | EFM040 | [`EfmiError::UnmappedAlgorithmCodeEntity`] |
//! | EFM041 | [`EfmiError::DuplicateForeignMapping`] |
//! | EFM042 | [`EfmiError::ManifestReferenceUuidMismatch`] |
//! | EFM043 | [`EfmiError::EmptyCodeFiles`] |

use thiserror::Error;

/// Errors produced by eFMI packaging model construction and serialization.
///
/// Every variant has a stable code (see [`EfmiError::code`]).
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum EfmiError {
    /// A manifest id is not a brace-wrapped UUID (`{8-4-4-4-12}` hex).
    #[error("invalid manifest id `{value}`: {reason}")]
    InvalidManifestId { value: String, reason: String },

    /// A timestamp does not match the strict UTC pattern `YYYY-MM-DDTHH:MM:SSZ`.
    #[error("invalid UTC timestamp `{value}`: {reason}")]
    InvalidTimestamp { value: String, reason: String },

    /// An id does not match the `efmiIdentifierType` character set.
    #[error("invalid eFMI identifier `{value}`: {reason}")]
    InvalidIdentifier { value: String, reason: String },

    /// A normalized-string field contains tab/newline/carriage-return or is empty.
    #[error("invalid normalized text `{value}`: {reason}")]
    InvalidText { value: String, reason: String },

    /// A container/manifest file name is empty or contains a slash.
    #[error("invalid container name `{value}`: {reason}")]
    InvalidContainerName { value: String, reason: String },

    /// A file path does not match `./(segment/)*` (`efmiFilePathType`).
    #[error("invalid eFMI file path `{value}`: {reason}")]
    InvalidFilePath { value: String, reason: String },

    /// A checksum string is not 40 lowercase hex characters.
    #[error("invalid SHA-1 checksum `{value}`: {reason}")]
    InvalidChecksum { value: String, reason: String },

    /// The same id value occurs twice within one manifest.
    #[error("duplicate id `{id}`: all id values in a manifest must be unique")]
    DuplicateId { id: String },

    /// Two variables in the `Variables` list share a name.
    #[error("duplicate variable name `{name}` in Variables list")]
    DuplicateVariableName { name: String },

    /// Two units in the `Units` list share a name.
    #[error("duplicate unit name `{name}` in Units list")]
    DuplicateUnitName { name: String },

    /// Two model representations in `__content.xml` share a name.
    #[error("duplicate model representation name `{name}` in __content.xml")]
    DuplicateRepresentationName { name: String },

    /// Two annotations in one `Annotations` list share a `type`.
    #[error("duplicate annotation type `{annotation_type}` in Annotations list of {owner}")]
    DuplicateAnnotationType {
        annotation_type: String,
        owner: String,
    },

    /// The same error signal is listed twice for one block method.
    #[error("duplicate signal `{signal}` for block method {method}")]
    DuplicateSignal { method: String, signal: String },

    /// The Algorithm Code manifest declares no variables.
    #[error(
        "Algorithm Code manifest has no variables; at least the Clock sample-period constant is required"
    )]
    EmptyVariables,

    /// A dimension size is below the XSD minimum of 1.
    #[error(
        "variable `{variable}` dimension {dimension_number} has size {size}; sizes must be >= 1"
    )]
    InvalidDimension {
        variable: String,
        dimension_number: usize,
        size: u64,
    },

    /// An array start value does not cover the variable's element count.
    #[error("variable `{variable}` start value has {actual} elements, expected {expected}")]
    StartLengthMismatch {
        variable: String,
        expected: u64,
        actual: usize,
    },

    /// An array start value was supplied for a scalar variable.
    #[error("variable `{variable}` is scalar but has an array start value; use a scalar start")]
    ArrayStartOnScalar { variable: String },

    /// The product of dimension sizes overflows `u64`.
    #[error("variable `{variable}` dimension sizes overflow the element count")]
    DimensionProductOverflow { variable: String },

    /// A `*RefId` attribute references an id that does not exist in this manifest.
    #[error("unresolved reference: {attribute} refers to unknown id `{id}`")]
    UnresolvedReference { attribute: String, id: String },

    /// The Clock references a variable that is not a Real constant (§3.1.2).
    #[error("invalid clock variable `{id}`: {reason}")]
    InvalidClockVariable { id: String, reason: String },

    /// A variable violates min/max/nominal/start range constraints.
    #[error("variable `{variable}` violates range constraints: {reason}")]
    InvalidVariableRange { variable: String, reason: String },

    /// A Real attribute is NaN or infinite and cannot be encoded in the manifest.
    #[error("variable `{variable}` attribute `{attribute}` is not a finite Real value")]
    NonFiniteReal { variable: String, attribute: String },

    /// `__content.xml` must register exactly one Algorithm Code representation.
    #[error("__content.xml registers {count} AlgorithmCode representations; exactly 1 is required")]
    AlgorithmCodeCardinality { count: usize },

    /// `activeFmu` names a representation that is not registered.
    #[error("activeFmu `{name}` does not match any registered model representation name")]
    UnknownActiveFmu { name: String },

    /// The XML event writer failed (I/O layer).
    #[error("XML serialization failed: {message}")]
    XmlWrite { message: String },

    /// A container layout rule is violated (reserved/unsafe names, wrong
    /// `.efmu` extension, missing `__content.xml`, ...).
    #[error("invalid eFMU container layout: {item}: {reason}")]
    InvalidContainerLayout { item: String, reason: String },

    /// Two files of one model representation resolve to the same
    /// container-relative path.
    #[error("representation `{representation}` writes `{path}` more than once")]
    DuplicateContainerFile {
        representation: String,
        path: String,
    },

    /// The eFMU output directory already exists and is not empty.
    #[error(
        "eFMU output directory `{path}` already exists and is not empty; refusing to overwrite"
    )]
    OutputDirNotEmpty { path: String },

    /// A file system operation failed while writing or reading a container.
    #[error("I/O error at `{path}`: {message}")]
    Io { path: String, message: String },

    /// Writing the `.efmu` zip archive failed.
    #[error("failed to write .efmu zip `{path}`: {message}")]
    Zip { path: String, message: String },

    /// `xmllint` is not installed; XSD validation cannot run and is never
    /// silently skipped.
    #[error(
        "xmllint not found on PATH: XSD validation requires libxml2's xmllint \
         (Debian/Ubuntu: `apt-get install libxml2-utils`)"
    )]
    XmllintUnavailable,

    /// `xmllint` rejected an XML document against its XSD.
    #[error("`{xml}` failed XSD validation against `{xsd}`:\n{output}")]
    XsdValidationFailed {
        xml: String,
        xsd: String,
        output: String,
    },

    /// A `BaseUnit` attribute is NaN or infinite and cannot be encoded as
    /// `xs:double`.
    #[error("unit `{unit}` BaseUnit attribute `{attribute}` is not a finite Real value")]
    NonFiniteUnitAttribute { unit: String, attribute: String },

    /// A `Files` list has more than one `role="FMU"` entry; a representation
    /// has at most one FMU (eFMI ch. 2.3.6).
    #[error("Files list has {count} role=\"FMU\" entries; at most 1 is allowed")]
    MultipleFmuFiles { count: usize },

    /// A manifest lists a `File` entry that is not among the files being
    /// written into its representation container (a dangling reference,
    /// regardless of `needsChecksum`).
    #[error(
        "representation `{representation}` manifest lists file `{path}` \
         but no such file is being written"
    )]
    ManifestFileMissing {
        representation: String,
        path: String,
    },

    /// A manifest `File/@checksum` does not match the SHA-1 of the bytes
    /// being written for that file (a wrong checksum makes the eFMU invalid).
    #[error(
        "representation `{representation}` manifest records checksum {listed} for `{path}` \
         but the bytes being written hash to {computed}; re-serialize the manifest after \
         changing file contents"
    )]
    StaleFileChecksum {
        representation: String,
        path: String,
        listed: String,
        computed: String,
    },

    /// A code-file reference resolves to a `File` whose role is not `Code`:
    /// the Algorithm Code `Manifest/@fileRefId` must reference the GALEC
    /// program implementing the block (§3.1.1), and each Production Code
    /// `CodeFile`'s `FileReference/@fileRefId` must reference a generated
    /// code file — both are by definition `role="Code"` files.
    #[error(
        "fileRefId `{id}` references a role=\"{role}\" file; it must \
         reference one of the representation's role=\"Code\" code files"
    )]
    FileRefRoleNotCode { id: String, role: String },

    /// `__content.xml` would register a `manifestRefId` that differs from the
    /// `id` inside the manifest bytes being written — a dangling reference in
    /// an otherwise schema-valid eFMU (e.g. the manifest was regenerated with
    /// a fresh id but the registration was not updated).
    #[error(
        "representation `{representation}` registers manifestRefId {registered} but its \
         manifest bytes carry id {found}; update manifest_ref_id after regenerating the manifest"
    )]
    ManifestRefIdMismatch {
        representation: String,
        registered: String,
        found: String,
    },

    /// A Production Code `LogicalData` foreign reference names an id that
    /// does not exist in the referenced Algorithm Code manifest (variable,
    /// `ErrorSignalStatus`, or block-method id, per reference kind).
    #[error(
        "unresolved foreign reference: {attribute} refers to id `{id}`, which is \
         not present in the referenced Algorithm Code manifest"
    )]
    UnresolvedForeignReference { attribute: String, id: String },

    /// An Algorithm Code entity that must be mapped by the Production Code
    /// `LogicalData` (every variable, all three block methods) has no
    /// mapping. This exactly-once obligation is rumoca's own conformance
    /// invariant (the XSD allows zero references).
    #[error(
        "Algorithm Code {entity} `{id}` has no LogicalData mapping in the \
         Production Code manifest"
    )]
    UnmappedAlgorithmCodeEntity { entity: String, id: String },

    /// One `LogicalData` maps the same foreign id more than once (across
    /// data and function references combined); each Algorithm Code entity is
    /// mapped at most once.
    #[error(
        "LogicalData maps foreign id `{foreign_ref_id}` more than once; each \
         Algorithm Code entity is mapped at most once"
    )]
    DuplicateForeignMapping { foreign_ref_id: String },

    /// The Production Code `ManifestReference/@manifestRefId` UUID differs
    /// from the `id` of the Algorithm Code manifest it claims to reference —
    /// the container would be stale/dangling by construction.
    #[error(
        "Production Code ManifestReference references Algorithm Code manifest \
         {referenced} but the Algorithm Code manifest id is {actual}"
    )]
    ManifestReferenceUuidMismatch { referenced: String, actual: String },

    /// A `CodeContainer` declares no code files; the XSD requires at least
    /// one `CodeFile` (`ProductionCode/efmiCodeFiles.xsd`, 1..unbounded).
    #[error("CodeContainer has no CodeFiles; at least one CodeFile is required")]
    EmptyCodeFiles,
}

impl EfmiError {
    /// Stable diagnostic code for CI aggregation (SPEC_0008).
    pub fn code(&self) -> &'static str {
        match self {
            Self::InvalidManifestId { .. } => "EFM001",
            Self::InvalidTimestamp { .. } => "EFM002",
            Self::InvalidIdentifier { .. } => "EFM003",
            Self::InvalidText { .. } => "EFM004",
            Self::InvalidContainerName { .. } => "EFM005",
            Self::InvalidFilePath { .. } => "EFM006",
            Self::InvalidChecksum { .. } => "EFM007",
            Self::DuplicateId { .. } => "EFM008",
            Self::DuplicateVariableName { .. } => "EFM009",
            Self::DuplicateUnitName { .. } => "EFM010",
            Self::DuplicateRepresentationName { .. } => "EFM011",
            Self::DuplicateAnnotationType { .. } => "EFM012",
            Self::DuplicateSignal { .. } => "EFM013",
            Self::EmptyVariables => "EFM014",
            Self::InvalidDimension { .. } => "EFM015",
            Self::StartLengthMismatch { .. } => "EFM016",
            Self::ArrayStartOnScalar { .. } => "EFM017",
            Self::DimensionProductOverflow { .. } => "EFM018",
            Self::UnresolvedReference { .. } => "EFM019",
            Self::InvalidClockVariable { .. } => "EFM020",
            Self::InvalidVariableRange { .. } => "EFM021",
            Self::NonFiniteReal { .. } => "EFM022",
            Self::AlgorithmCodeCardinality { .. } => "EFM023",
            Self::UnknownActiveFmu { .. } => "EFM024",
            Self::XmlWrite { .. } => "EFM025",
            Self::InvalidContainerLayout { .. } => "EFM026",
            Self::DuplicateContainerFile { .. } => "EFM027",
            Self::OutputDirNotEmpty { .. } => "EFM028",
            Self::Io { .. } => "EFM029",
            Self::Zip { .. } => "EFM030",
            Self::XmllintUnavailable => "EFM031",
            Self::XsdValidationFailed { .. } => "EFM032",
            Self::NonFiniteUnitAttribute { .. } => "EFM033",
            Self::MultipleFmuFiles { .. } => "EFM034",
            Self::ManifestFileMissing { .. } => "EFM035",
            Self::StaleFileChecksum { .. } => "EFM036",
            Self::FileRefRoleNotCode { .. } => "EFM037",
            Self::ManifestRefIdMismatch { .. } => "EFM038",
            Self::UnresolvedForeignReference { .. } => "EFM039",
            Self::UnmappedAlgorithmCodeEntity { .. } => "EFM040",
            Self::DuplicateForeignMapping { .. } => "EFM041",
            Self::ManifestReferenceUuidMismatch { .. } => "EFM042",
            Self::EmptyCodeFiles => "EFM043",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::EfmiError;

    #[test]
    fn codes_are_stable() {
        assert_eq!(
            EfmiError::InvalidManifestId {
                value: "x".into(),
                reason: "r".into()
            }
            .code(),
            "EFM001"
        );
        assert_eq!(EfmiError::EmptyVariables.code(), "EFM014");
        assert_eq!(
            EfmiError::XmlWrite {
                message: "m".into()
            }
            .code(),
            "EFM025"
        );
    }
}
