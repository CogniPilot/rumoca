//! Typed model of the Production Code manifest
//! (`ProductionCode/efmiProductionCodeManifest.xsd`, 0.17.0).
//!
//! Mirrors the [`crate::manifest_context::algorithm_code_manifest`] idiom: a public
//! [`ProductionCodeManifestParts`] struct in XSD child order plus a newtype
//! wrapper whose [`ProductionCodeManifest::new`] validates eagerly
//! (SPEC_0008: fail early, no silent defaults). The XSDs under
//! `assets/efmi-schemas/ProductionCode/` are ground truth for every field
//! (GAL-023).
//!
//! Illegal states are unrepresentable where cheap:
//!
//! - the fixed attributes (`xsdVersion="0.17.0"`, `kind="ProductionCode"`,
//!   `efmiVersion="1.0.0"`) are serializer constants, not fields (GAL-022);
//! - with exactly one [`ManifestReference`] its `origin` attribute must be
//!   `true`, so it is not a field either — the serializer emits
//!   `origin="true"` unconditionally;
//! - `Dimension`/`FormalParameter` `number` attributes are derived from
//!   position (0-based here — the Production Code convention; the Algorithm
//!   Code schema documents 1-based, so the two serializers differ
//!   deliberately);
//! - the `Alias`/`Components` shape of a `Typedef` is one enum,
//!   [`TypedefBody`].
//!
//! # Minimal-conformant subset
//!
//! This models exactly the surface a rumoca producer constructs. XSD
//! machinery with no producer is deliberately absent (`minOccurs="0"` /
//! `xs:choice` make producer-side subsetting schema-valid); each is an
//! extension point to be added together with its first producer:
//!
//! - `CompilerOptions`, `LinkerOptions`, `TechnicalInformationLookUps`;
//! - `Macros` (and with them `Dimension/@valueMacroRefId` — `Vec<u64>`
//!   dimensions make the macro arm unrepresentable until macros exist);
//! - the `Variables` wrapper of a `CodeFile` and the `GlobalVariable` arm of
//!   `DataReference`: generated C has no globals (all state is struct fields
//!   reached via the `self` formal parameter);
//! - the `Pointer` and `EnumerationItems` [`TypedefBody`] shapes;
//! - parameter `min`/`max` (note for later: they are `xs:decimal`, while
//!   `Variable` min/max are `xs:normalizedString` — two field types, never
//!   unify).
//!
//! Cross-manifest consistency against the referenced Algorithm Code manifest
//! is a separate, explicit step: [`validate_against_algorithm_code`].

use std::collections::{BTreeMap, BTreeSet};

use crate::manifest_context::algorithm_code_manifest::AlgorithmCodeManifest;
use crate::manifest_context::checksum::Sha1Hex;
use crate::manifest_context::diagnostic::EfmiError;
use crate::manifest_context::ids::{IdRegistry, Identifier, ManifestId, NormalizedText};
use crate::manifest_context::manifest_common::{
    Annotation, File, FileRole, ManifestAttributes, validate_annotations, validate_files,
};

/// The one `ManifestReference` (`efmiManifestReferences.xsd`): the Production
/// Code manifest's link to the Algorithm Code manifest it was generated from.
///
/// The XSD wrapper admits exactly one reference, and with exactly one
/// reference `origin` must be `true`, so there is no `origin` field: the
/// serializer emits `origin="true"` unconditionally.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestReference {
    /// Local anchor id (`id`, required), referenced by the
    /// `manifestReferenceRefId` of every [`ForeignReference`].
    pub id: Identifier,
    /// Braced UUID of the referenced Algorithm Code manifest
    /// (`manifestRefId`, required) — typed, taken from the typed AC model,
    /// never re-parsed from rendered bytes.
    pub manifest_ref_id: ManifestId,
    /// SHA-1 of the exact rendered Algorithm Code manifest bytes
    /// (`checksum`, required); used by consumers to detect stale containers.
    pub checksum: Sha1Hex,
}

/// `efmiSupportedLanguages` (`ProductionCode/efmiSupportedLanguages.xsd`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SupportedLanguage {
    /// `C`
    C,
    /// `C++`
    CPlusPlus,
}

impl SupportedLanguage {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::C => "C",
            Self::CPlusPlus => "C++",
        }
    }
}

/// `efmiSupportedPlatforms` (`ProductionCode/efmiSupportedPlatforms.xsd`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SupportedPlatform {
    /// `Legacy` — no standardized platform support.
    Legacy,
    /// `Classic AUTOSAR` (the literal contains a space).
    ClassicAutosar,
    /// `Adaptive AUTOSAR` (the literal contains a space).
    AdaptiveAutosar,
}

impl SupportedPlatform {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Legacy => "Legacy",
            Self::ClassicAutosar => "Classic AUTOSAR",
            Self::AdaptiveAutosar => "Adaptive AUTOSAR",
        }
    }
}

/// `efmiFloatingPointPrecision` (`efmiFloatingPointPrecision.xsd`,
/// IEEE 754-2019 widths).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FloatPrecision {
    /// `16-bit`
    Bits16,
    /// `32-bit`
    Bits32,
    /// `64-bit`
    Bits64,
    /// `128-bit`
    Bits128,
    /// `256-bit`
    Bits256,
}

impl FloatPrecision {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Bits16 => "16-bit",
            Self::Bits32 => "32-bit",
            Self::Bits64 => "64-bit",
            Self::Bits128 => "128-bit",
            Self::Bits256 => "256-bit",
        }
    }
}

/// `efmiTargetDataTypeKind` (`ProductionCode/efmiTargetTypes.xsd`): the 12
/// eFMI data-type kinds a platform type can be bound to. Note the exact
/// literals: `efmiBool` (not `efmiBoolean`) and no `efmiFloat128`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TargetTypeKind {
    /// `efmiBool`
    EfmiBool,
    /// `efmiFloat32`
    EfmiFloat32,
    /// `efmiFloat64`
    EfmiFloat64,
    /// `efmiInteger8`
    EfmiInteger8,
    /// `efmiInteger16`
    EfmiInteger16,
    /// `efmiInteger32`
    EfmiInteger32,
    /// `efmiInteger64`
    EfmiInteger64,
    /// `efmiUnsignedInteger8`
    EfmiUnsignedInteger8,
    /// `efmiUnsignedInteger16`
    EfmiUnsignedInteger16,
    /// `efmiUnsignedInteger32`
    EfmiUnsignedInteger32,
    /// `efmiUnsignedInteger64`
    EfmiUnsignedInteger64,
    /// `efmiVoid`
    EfmiVoid,
}

impl TargetTypeKind {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::EfmiBool => "efmiBool",
            Self::EfmiFloat32 => "efmiFloat32",
            Self::EfmiFloat64 => "efmiFloat64",
            Self::EfmiInteger8 => "efmiInteger8",
            Self::EfmiInteger16 => "efmiInteger16",
            Self::EfmiInteger32 => "efmiInteger32",
            Self::EfmiInteger64 => "efmiInteger64",
            Self::EfmiUnsignedInteger8 => "efmiUnsignedInteger8",
            Self::EfmiUnsignedInteger16 => "efmiUnsignedInteger16",
            Self::EfmiUnsignedInteger32 => "efmiUnsignedInteger32",
            Self::EfmiUnsignedInteger64 => "efmiUnsignedInteger64",
            Self::EfmiVoid => "efmiVoid",
        }
    }
}

/// `FileType` of a `CodeFile` (`ProductionCode/efmiCodeFiles.xsd`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CodeFileType {
    /// `ProductionCode`
    ProductionCode,
    /// `SimulationCode`
    SimulationCode,
    /// `ToolSpecificCode`
    ToolSpecificCode,
}

impl CodeFileType {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::ProductionCode => "ProductionCode",
            Self::SimulationCode => "SimulationCode",
            Self::ToolSpecificCode => "ToolSpecificCode",
        }
    }
}

/// `CodeType` of a `CodeFile` (`ProductionCode/efmiCodeFiles.xsd`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CodeType {
    /// `SourceFile`
    SourceFile,
    /// `HeaderFile`
    HeaderFile,
}

impl CodeType {
    /// The exact XSD enumeration literal.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::SourceFile => "SourceFile",
            Self::HeaderFile => "HeaderFile",
        }
    }
}

/// One `TargetType` (`ProductionCode/efmiTargetTypes.xsd`): binds an eFMI
/// data-type kind to the platform type spelling used by the generated code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetType {
    /// Target-type id (`id`, required), referenced by `Alias/@targetTypeRefId`.
    pub id: Identifier,
    /// The eFMI data-type kind (`kind`, required).
    pub kind: TargetTypeKind,
    /// The platform type as spelled in the code, e.g. `double` (`codedType`,
    /// required).
    pub coded_type: NormalizedText,
}

/// One `CodeFile` (`ProductionCode/efmiCodeFiles.xsd`): a generated source
/// or header file plus the code entities it declares.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeFile {
    /// Code-file id (`id`, required), referenced by `Include/@codeFileRefId`.
    pub id: Identifier,
    /// `fileType` (required).
    pub file_type: CodeFileType,
    /// `codeType` (required).
    pub code_type: CodeType,
    /// `FileReference/@fileRefId` (required child, 1..1): resolves to a
    /// `File` of the `Files` list, whose role must be `Code`.
    pub file_ref_id: Identifier,
    /// `FileReference/@kind` (optional free string).
    pub file_ref_kind: Option<NormalizedText>,
    /// `Includes/Include/@codeFileRefId` values: references to **CodeFile**
    /// ids (not `File` ids). System headers (`<math.h>`, ...) are not
    /// representable and simply not listed. Wrapper absent when empty.
    pub includes: Vec<Identifier>,
    /// `Typedefs` (wrapper absent when empty).
    pub typedefs: Vec<Typedef>,
    /// `Functions`: globally accessible functions only (file-internal
    /// helpers are not published). Wrapper absent when empty.
    pub functions: Vec<Function>,
}

/// One `Typedef` (`ProductionCode/efmiTypeDefs.xsd`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Typedef {
    /// Typedef id (`id`, required), referenced by every `typeDefRefId`.
    pub id: Identifier,
    /// The C type token this typedef introduces (`name`, required).
    pub name: NormalizedText,
    /// The `xs:choice` body shape.
    pub body: TypedefBody,
}

/// The `xs:choice` inside a `Typedef`. The `Pointer` and `EnumerationItems`
/// arms are deliberately absent from slice 1 (no producer — see module docs).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedefBody {
    /// `Alias`: renames a target type (`typedef double Real;`), optionally
    /// cascading through another typedef.
    Alias {
        /// `targetTypeRefId` (required): resolves to a [`TargetType`] id.
        target_type_ref_id: Identifier,
        /// `typeDefRefId` (optional cascade): resolves to a [`Typedef`] id.
        type_def_ref_id: Option<Identifier>,
    },
    /// `Components`: a struct type (here: the block state struct).
    Components(Vec<Component>),
}

/// One `Component` of a struct [`Typedef`]
/// (`ProductionCode/efmiTypeDefs.xsd`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Component {
    /// Component id (`id`, required).
    pub id: Identifier,
    /// The C field token (`name`, required).
    pub name: NormalizedText,
    /// Element type (`typeDefRefId`, required): resolves to a [`Typedef`] id
    /// — never to a [`TargetType`] directly.
    pub type_def_ref_id: Identifier,
    /// Dimension sizes in order; empty means scalar. Each size must be >= 1;
    /// the `Dimension/@number` attribute is derived from position (0-based).
    pub dimensions: Vec<u64>,
    /// `pointer` (XSD default `false`; emitted only when `true`).
    pub pointer: bool,
}

/// One `Function` (`ProductionCode/efmiFunctions.xsd`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    /// Function id (`id`, required), referenced by
    /// `GlobalFunction/@functionRefId`.
    pub id: Identifier,
    /// The C function token (`name`, required).
    pub name: NormalizedText,
    /// `ReturnParameter` (required, `minOccurs="1"`); a `void` return is
    /// expressed via a typedef aliasing the `efmiVoid` target type.
    pub return_parameter: ParameterCore,
    /// `FormalParameters` in call order; the `number` attribute is derived
    /// from position (0-based). Wrapper absent when empty.
    pub formal_parameters: Vec<FormalParameter>,
}

/// The `FunctionParameterAttributes` group shared by `ReturnParameter` and
/// `FormalParameter` (`ProductionCode/efmiFunctions.xsd`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterCore {
    /// Parameter id (`id`, required). Formal-parameter ids are referenced by
    /// `FormalParameter/@formalParameterRefId` in `LogicalData`.
    pub id: Identifier,
    /// Parameter type (`typeDefRefId`, required): resolves to a [`Typedef`]
    /// id — never to a [`TargetType`] directly.
    pub type_def_ref_id: Identifier,
    /// `const` (XSD default `false`).
    pub constant: bool,
    /// `pointer` (XSD default `false`).
    pub pointer: bool,
    /// `constPointer` (XSD default `false`).
    pub const_pointer: bool,
    /// Dimension sizes in order; empty means scalar. Each size must be >= 1;
    /// the `Dimension/@number` attribute is derived from position (0-based).
    pub dimensions: Vec<u64>,
}

/// One `FormalParameter` (`ProductionCode/efmiFunctions.xsd`); its `number`
/// attribute is derived from its position (0-based).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormalParameter {
    /// The C parameter token (`name`, required).
    pub name: NormalizedText,
    /// The shared parameter attribute group.
    pub core: ParameterCore,
}

/// A `ForeignReference` (`efmiManifestReferences.xsd`): an item reference
/// into another manifest via a local [`ManifestReference`] anchor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignReference {
    /// `manifestReferenceRefId` (required): resolves to the
    /// [`ManifestReference`] id of this manifest.
    pub manifest_reference_ref_id: Identifier,
    /// `foreignRefId` (required): an id inside the referenced manifest;
    /// resolvable only against that manifest
    /// ([`validate_against_algorithm_code`]).
    pub foreign_ref_id: Identifier,
}

/// One `DataReference` (`ProductionCode/efmiLogicalData.xsd`): maps an
/// Algorithm Code variable onto production-code data. Only the
/// `FormalParameter` choice arm is modeled (generated C has no globals —
/// see module docs).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataReference {
    /// `ForeignVariableReference` (required): the Algorithm Code variable.
    pub foreign: ForeignReference,
    /// `FormalParameter/@formalParameterRefId` (required): resolves to a
    /// formal-parameter id of a [`Function`] in this manifest.
    pub formal_parameter_ref_id: Identifier,
    /// `FormalParameter/@componentIdentifier` (optional): path within the
    /// parameter's aggregate type, e.g. `a.b[3].c`.
    pub component_identifier: Option<NormalizedText>,
}

/// One `FunctionReference` (`ProductionCode/efmiLogicalData.xsd`): maps an
/// Algorithm Code block method onto a production-code function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionReference {
    /// `ForeignFunctionReference` (required): the Algorithm Code block
    /// method.
    pub foreign: ForeignReference,
    /// `GlobalFunction/@functionRefId` (required): resolves to a
    /// [`Function`] id in this manifest.
    pub function_ref_id: Identifier,
}

/// `LogicalData` (`ProductionCode/efmiLogicalData.xsd`): the cross-container
/// mapping to the Algorithm Code manifest. Both wrapper elements are 1..1
/// with optional children, so the serializer always emits them — the inverse
/// of the absent-when-empty list rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicalData {
    /// `DataReferences` children (0..unbounded).
    pub data_references: Vec<DataReference>,
    /// `FunctionReferences` children (0..unbounded).
    pub function_references: Vec<FunctionReference>,
}

/// `CodeContainer` (`ProductionCode/efmiProductionCodeManifest.xsd`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeContainer {
    /// `language` (required).
    pub language: SupportedLanguage,
    /// `standard` (required free string), e.g. `C99`.
    pub standard: NormalizedText,
    /// `platform` (required).
    pub platform: SupportedPlatform,
    /// `floatPrecision` (required).
    pub float_precision: FloatPrecision,
    /// `description` (optional; `xs:string` in the XSD, held to
    /// normalized-attribute discipline — see [`crate::manifest_context::manifest_common`]
    /// module docs).
    pub description: Option<NormalizedText>,
    /// `Target` (required element): `Generic` unless the code carries
    /// target-specific parts. Always emitted explicitly — the XSD default
    /// only fills an empty element.
    pub target: NormalizedText,
    /// `TargetTypes` (0..1 wrapper; absent when empty). Always populated by
    /// rumoca producers: the `typeDefRefId → Typedef → Alias → TargetType`
    /// chain is the semantic core and cannot resolve without it.
    pub target_types: Vec<TargetType>,
    /// `CodeFiles` (1..unbounded — [`ProductionCodeManifest::new`] rejects
    /// empty).
    pub code_files: Vec<CodeFile>,
    /// `LogicalData` (1..1).
    pub logical_data: LogicalData,
}

/// Unvalidated field set for [`ProductionCodeManifest`]. Assemble freely,
/// then validate via [`ProductionCodeManifest::new`]. Fields are in XSD
/// child order.
#[derive(Debug, Clone, PartialEq)]
pub struct ProductionCodeManifestParts {
    /// Shared top-level attributes (`efmiManifestAttributesBase`).
    pub attributes: ManifestAttributes,
    /// The single `ManifestReferences/ManifestReference` (the Algorithm Code
    /// manifest this Production Code was generated from).
    pub manifest_reference: ManifestReference,
    /// `Files` list (the generated code files; the manifest itself is not
    /// listed, matching the Algorithm Code precedent).
    pub files: Vec<File>,
    /// `CodeContainer` (exactly one).
    pub code_container: CodeContainer,
    /// Manifest-level vendor annotations (`Annotations`, 0..1; absent when
    /// empty).
    pub annotations: Vec<Annotation>,
}

/// Validated Production Code manifest model. Construction enforces
/// (fail-early, GAL-018/020/021):
///
/// - all id values unique across the whole manifest (principle 6);
/// - the shared `Files` rules (at most one `role="FMU"` file);
/// - every `CodeFile`'s `FileReference/@fileRefId` resolves to a
///   `role="Code"` file;
/// - every `Include/@codeFileRefId` resolves to a `CodeFile` id (not a
///   `File` id);
/// - every `typeDefRefId` (components, parameters, alias cascades) resolves
///   to a `Typedef` id — never to a `TargetType` — and every
///   `Alias/@targetTypeRefId` resolves to a `TargetType` id;
/// - `LogicalData` references resolve locally: `manifestReferenceRefId` to
///   the one [`ManifestReference`], `formalParameterRefId` to a formal
///   parameter, `functionRefId` to a function;
/// - each foreign id is mapped at most once across the whole `LogicalData`;
/// - dimension sizes >= 1 and non-empty `CodeFiles`.
///
/// Foreign `foreignRefId` values can only be resolved against the referenced
/// Algorithm Code manifest — that is [`validate_against_algorithm_code`].
#[derive(Debug, Clone, PartialEq)]
pub struct ProductionCodeManifest(ProductionCodeManifestParts);

impl ProductionCodeManifest {
    /// Validate the parts and construct the model.
    pub fn new(parts: ProductionCodeManifestParts) -> Result<Self, EfmiError> {
        validate_unique_ids(&parts)?;
        validate_files(&parts.files)?;
        validate_annotations("Manifest", &parts.annotations)?;
        validate_code_container(&parts)?;
        validate_logical_data(&parts)?;
        Ok(Self(parts))
    }

    /// The validated field set.
    pub fn parts(&self) -> &ProductionCodeManifestParts {
        &self.0
    }
}

/// Cross-manifest consistency of a Production Code manifest against the
/// Algorithm Code manifest it references (both typed models — no bytes):
///
/// - the `ManifestReference` UUID equals the Algorithm Code manifest id
///   (typed equality, [`EfmiError::ManifestReferenceUuidMismatch`]);
/// - every data `foreignRefId` is an Algorithm Code variable id or the
///   `ErrorSignalStatus` id, and every function `foreignRefId` is a block
///   method id ([`EfmiError::UnresolvedForeignReference`]);
/// - all three block methods and every Algorithm Code variable are mapped
///   exactly once ([`EfmiError::UnmappedAlgorithmCodeEntity`] for the
///   missing side; the more-than-once side is already rejected at
///   construction by [`ProductionCodeManifest::new`],
///   [`EfmiError::DuplicateForeignMapping`]).
///
/// The exactly-once obligation is rumoca's own conformance invariant (a
/// prose obligation; the XSD allows zero references) — enforced here, in the
/// owning phase. Mapping the `ErrorSignalStatus` is permitted but never
/// required (rumoca's methods return void and expose no status variable).
/// Byte-level agreement of `ManifestReference/@checksum` with the rendered
/// Algorithm Code manifest bytes is NOT checked here — the typed models
/// cannot see bytes; that is builder-input discipline plus the e2e
/// staleness-chain test.
pub fn validate_against_algorithm_code(
    pc: &ProductionCodeManifest,
    ac: &AlgorithmCodeManifest,
) -> Result<(), EfmiError> {
    let pc_parts = pc.parts();
    let ac_parts = ac.parts();

    if pc_parts.manifest_reference.manifest_ref_id != ac_parts.attributes.id {
        return Err(EfmiError::ManifestReferenceUuidMismatch {
            referenced: pc_parts.manifest_reference.manifest_ref_id.to_string(),
            actual: ac_parts.attributes.id.to_string(),
        });
    }

    let variable_ids: BTreeSet<&str> = ac_parts
        .variables
        .iter()
        .map(|v| v.common().id.as_str())
        .collect();
    let error_signal_status_id = ac_parts.error_signal_status.id.as_str();
    let methods = &ac_parts.block_methods;
    let method_ids = [
        ("Startup", methods.startup.id.as_str()),
        ("Recalibrate", methods.recalibrate.id.as_str()),
        ("DoStep", methods.do_step.id.as_str()),
    ];

    let logical_data = &pc_parts.code_container.logical_data;
    let mut data_mapped: BTreeSet<&str> = BTreeSet::new();
    for reference in &logical_data.data_references {
        let id = reference.foreign.foreign_ref_id.as_str();
        if !variable_ids.contains(id) && id != error_signal_status_id {
            return Err(EfmiError::UnresolvedForeignReference {
                attribute: "ForeignVariableReference/@foreignRefId".to_owned(),
                id: id.to_owned(),
            });
        }
        data_mapped.insert(id);
    }
    let mut functions_mapped: BTreeSet<&str> = BTreeSet::new();
    for reference in &logical_data.function_references {
        let id = reference.foreign.foreign_ref_id.as_str();
        if !method_ids.iter().any(|(_, method_id)| *method_id == id) {
            return Err(EfmiError::UnresolvedForeignReference {
                attribute: "ForeignFunctionReference/@foreignRefId".to_owned(),
                id: id.to_owned(),
            });
        }
        functions_mapped.insert(id);
    }

    for variable in &ac_parts.variables {
        let id = variable.common().id.as_str();
        if !data_mapped.contains(id) {
            return Err(EfmiError::UnmappedAlgorithmCodeEntity {
                entity: "variable".to_owned(),
                id: id.to_owned(),
            });
        }
    }
    for (name, id) in method_ids {
        if !functions_mapped.contains(id) {
            return Err(EfmiError::UnmappedAlgorithmCodeEntity {
                entity: format!("block method {name}"),
                id: id.to_owned(),
            });
        }
    }
    Ok(())
}

/// V1: global id uniqueness over every id in the file (principle 6).
fn validate_unique_ids(parts: &ProductionCodeManifestParts) -> Result<(), EfmiError> {
    let mut registry = IdRegistry::new();
    registry.register(&parts.attributes.id.to_string())?;
    registry.register(parts.manifest_reference.id.as_str())?;
    for file in &parts.files {
        registry.register(file.id.as_str())?;
    }
    for target_type in &parts.code_container.target_types {
        registry.register(target_type.id.as_str())?;
    }
    for code_file in &parts.code_container.code_files {
        register_code_file_ids(&mut registry, code_file)?;
    }
    Ok(())
}

/// V1, per code file: `CodeFile`, `Typedef`, `Component`, `Function`, and
/// return/formal parameter ids.
fn register_code_file_ids(
    registry: &mut IdRegistry,
    code_file: &CodeFile,
) -> Result<(), EfmiError> {
    registry.register(code_file.id.as_str())?;
    for typedef in &code_file.typedefs {
        registry.register(typedef.id.as_str())?;
        let TypedefBody::Components(components) = &typedef.body else {
            continue;
        };
        for component in components {
            registry.register(component.id.as_str())?;
        }
    }
    for function in &code_file.functions {
        registry.register(function.id.as_str())?;
        registry.register(function.return_parameter.id.as_str())?;
        for parameter in &function.formal_parameters {
            registry.register(parameter.core.id.as_str())?;
        }
    }
    Ok(())
}

/// Local id index for reference resolution inside one `CodeContainer`.
struct LocalIds<'a> {
    files: BTreeMap<&'a str, &'a File>,
    code_file_ids: BTreeSet<&'a str>,
    /// Typedef ids only: V1 makes them disjoint from TargetType ids, so
    /// resolving `typeDefRefId` against this set is exactly the "resolves to
    /// a Typedef, never a TargetType" rule (V5).
    typedef_ids: BTreeSet<&'a str>,
    target_type_ids: BTreeSet<&'a str>,
}

impl<'a> LocalIds<'a> {
    fn build(parts: &'a ProductionCodeManifestParts) -> Self {
        let container = &parts.code_container;
        Self {
            files: parts.files.iter().map(|f| (f.id.as_str(), f)).collect(),
            code_file_ids: container.code_files.iter().map(|c| c.id.as_str()).collect(),
            typedef_ids: container
                .code_files
                .iter()
                .flat_map(|c| c.typedefs.iter())
                .map(|t| t.id.as_str())
                .collect(),
            target_type_ids: container
                .target_types
                .iter()
                .map(|t| t.id.as_str())
                .collect(),
        }
    }
}

fn unresolved(attribute: &str, id: &Identifier) -> EfmiError {
    EfmiError::UnresolvedReference {
        attribute: attribute.to_owned(),
        id: id.as_str().to_owned(),
    }
}

/// V3-V6, V9, V10: `CodeContainer` well-formedness and local reference
/// resolution.
fn validate_code_container(parts: &ProductionCodeManifestParts) -> Result<(), EfmiError> {
    let container = &parts.code_container;
    if container.code_files.is_empty() {
        return Err(EfmiError::EmptyCodeFiles); // V10
    }
    let ids = LocalIds::build(parts);
    for code_file in &container.code_files {
        validate_code_file(code_file, &ids)?;
    }
    Ok(())
}

/// V3-V6, V9 for one `CodeFile`.
fn validate_code_file(code_file: &CodeFile, ids: &LocalIds<'_>) -> Result<(), EfmiError> {
    // V3: FileReference resolves to a role="Code" file.
    let Some(referenced) = ids.files.get(code_file.file_ref_id.as_str()) else {
        return Err(unresolved(
            "FileReference/@fileRefId",
            &code_file.file_ref_id,
        ));
    };
    if referenced.role != FileRole::Code {
        return Err(EfmiError::FileRefRoleNotCode {
            id: code_file.file_ref_id.as_str().to_owned(),
            role: referenced.role.as_str().to_owned(),
        });
    }
    // V4: Include references resolve to CodeFile ids, not File ids.
    for include in &code_file.includes {
        if !ids.code_file_ids.contains(include.as_str()) {
            return Err(unresolved("Include/@codeFileRefId", include));
        }
    }
    for typedef in &code_file.typedefs {
        validate_typedef_body(&typedef.body, ids)?;
    }
    for function in &code_file.functions {
        let return_owner = format!("{}/ReturnParameter", function.name.as_str());
        validate_parameter_core(&return_owner, &function.return_parameter, &ids.typedef_ids)?;
        for parameter in &function.formal_parameters {
            let owner = format!("{}/{}", function.name.as_str(), parameter.name.as_str());
            validate_parameter_core(&owner, &parameter.core, &ids.typedef_ids)?;
        }
    }
    Ok(())
}

/// V5, V6, V9 for one `Typedef` body.
fn validate_typedef_body(body: &TypedefBody, ids: &LocalIds<'_>) -> Result<(), EfmiError> {
    match body {
        TypedefBody::Alias {
            target_type_ref_id,
            type_def_ref_id,
        } => {
            // V6
            if !ids.target_type_ids.contains(target_type_ref_id.as_str()) {
                return Err(unresolved("Alias/@targetTypeRefId", target_type_ref_id));
            }
            // V5 (cascade arm)
            if let Some(cascade) = type_def_ref_id
                && !ids.typedef_ids.contains(cascade.as_str())
            {
                return Err(unresolved("Alias/@typeDefRefId", cascade));
            }
        }
        TypedefBody::Components(components) => {
            for component in components {
                // V5
                if !ids.typedef_ids.contains(component.type_def_ref_id.as_str()) {
                    return Err(unresolved(
                        "Component/@typeDefRefId",
                        &component.type_def_ref_id,
                    ));
                }
                // V9
                validate_dimensions(component.name.as_str(), &component.dimensions)?;
            }
        }
    }
    Ok(())
}

/// V5 + V9 for one `ReturnParameter`/`FormalParameter` attribute group.
fn validate_parameter_core(
    owner: &str,
    core: &ParameterCore,
    typedef_ids: &BTreeSet<&str>,
) -> Result<(), EfmiError> {
    if !typedef_ids.contains(core.type_def_ref_id.as_str()) {
        return Err(EfmiError::UnresolvedReference {
            attribute: format!("{owner}/@typeDefRefId"),
            id: core.type_def_ref_id.as_str().to_owned(),
        });
    }
    validate_dimensions(owner, &core.dimensions)
}

/// V9: every dimension extent >= 1. `dimension_number` reports the 0-based
/// position, matching the serialized Production Code `Dimension/@number`
/// convention (unlike the 1-based Algorithm Code schema).
fn validate_dimensions(owner: &str, dimensions: &[u64]) -> Result<(), EfmiError> {
    for (number, size) in dimensions.iter().enumerate() {
        if *size < 1 {
            return Err(EfmiError::InvalidDimension {
                variable: owner.to_owned(),
                dimension_number: number,
                size: *size,
            });
        }
    }
    Ok(())
}

/// V7 + V8: `LogicalData` local resolution and at-most-once foreign mapping.
fn validate_logical_data(parts: &ProductionCodeManifestParts) -> Result<(), EfmiError> {
    let container = &parts.code_container;
    let manifest_reference_id = parts.manifest_reference.id.as_str();
    let function_ids: BTreeSet<&str> = container
        .code_files
        .iter()
        .flat_map(|c| c.functions.iter())
        .map(|f| f.id.as_str())
        .collect();
    // Formal-parameter ids only: return parameters are not addressable by
    // `formalParameterRefId`.
    let formal_parameter_ids: BTreeSet<&str> = container
        .code_files
        .iter()
        .flat_map(|c| c.functions.iter())
        .flat_map(|f| f.formal_parameters.iter())
        .map(|p| p.core.id.as_str())
        .collect();

    // V8 is one set across data and function references: a foreign entity is
    // mapped at most once, period.
    let mut foreign_mapped: BTreeSet<&str> = BTreeSet::new();

    for reference in &container.logical_data.data_references {
        if reference.foreign.manifest_reference_ref_id.as_str() != manifest_reference_id {
            return Err(unresolved(
                "ForeignVariableReference/@manifestReferenceRefId",
                &reference.foreign.manifest_reference_ref_id,
            ));
        }
        if !formal_parameter_ids.contains(reference.formal_parameter_ref_id.as_str()) {
            return Err(unresolved(
                "FormalParameter/@formalParameterRefId",
                &reference.formal_parameter_ref_id,
            ));
        }
        map_foreign_once(&mut foreign_mapped, &reference.foreign)?;
    }
    for reference in &container.logical_data.function_references {
        if reference.foreign.manifest_reference_ref_id.as_str() != manifest_reference_id {
            return Err(unresolved(
                "ForeignFunctionReference/@manifestReferenceRefId",
                &reference.foreign.manifest_reference_ref_id,
            ));
        }
        if !function_ids.contains(reference.function_ref_id.as_str()) {
            return Err(unresolved(
                "GlobalFunction/@functionRefId",
                &reference.function_ref_id,
            ));
        }
        map_foreign_once(&mut foreign_mapped, &reference.foreign)?;
    }
    Ok(())
}

/// V8: record one foreign mapping, failing on the second occurrence.
fn map_foreign_once<'a>(
    foreign_mapped: &mut BTreeSet<&'a str>,
    foreign: &'a ForeignReference,
) -> Result<(), EfmiError> {
    if !foreign_mapped.insert(foreign.foreign_ref_id.as_str()) {
        return Err(EfmiError::DuplicateForeignMapping {
            foreign_ref_id: foreign.foreign_ref_id.as_str().to_owned(),
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::manifest_context::algorithm_code_manifest::{
        AlgorithmCodeManifestParts, BlockCausality, BlockMethod, BlockMethods, Clock,
        ErrorSignalStatus, RealVariable, StartValue, Variable, VariableCommon,
    };
    use crate::manifest_context::ids::{FilePath, NameWithoutSlashes, UtcTimestamp};
    use crate::manifest_context::manifest_common::FileChecksum;

    /// UUID of the Algorithm Code manifest both fixtures agree on.
    const AC_UUID: &str = "{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}";
    /// UUID of the Production Code manifest itself.
    const PC_UUID: &str = "{3c9aa74e-1d2f-4b6a-9e07-51c4f0d88b31}";

    fn ident(value: &str) -> Identifier {
        Identifier::new(value).unwrap()
    }

    fn text(value: &str) -> NormalizedText {
        NormalizedText::new(value).unwrap()
    }

    fn attributes() -> ManifestAttributes {
        ManifestAttributes {
            id: ManifestId::parse(PC_UUID).unwrap(),
            name: text("TestBlock"),
            description: None,
            version: None,
            generation_date_and_time: UtcTimestamp::parse("2026-07-03T12:00:00Z").unwrap(),
            generation_tool: None,
            copyright: None,
            license: None,
        }
    }

    fn code_file_entry(id: &str, name: &str) -> File {
        File {
            id: ident(id),
            name: NameWithoutSlashes::new(name).unwrap(),
            path: FilePath::root(),
            checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(name.as_bytes())),
            role: FileRole::Code,
            description: None,
        }
    }

    fn alias_typedef(id: &str, name: &str, target_type_id: &str) -> Typedef {
        Typedef {
            id: ident(id),
            name: text(name),
            body: TypedefBody::Alias {
                target_type_ref_id: ident(target_type_id),
                type_def_ref_id: None,
            },
        }
    }

    fn scalar_component(id: &str, name: &str) -> Component {
        Component {
            id: ident(id),
            name: text(name),
            type_def_ref_id: ident("TD_F64"),
            dimensions: vec![],
            pointer: false,
        }
    }

    fn void_self_function(id: &str, name: &str, rp_id: &str, fp_id: &str) -> Function {
        Function {
            id: ident(id),
            name: text(name),
            return_parameter: ParameterCore {
                id: ident(rp_id),
                type_def_ref_id: ident("TD_VOID"),
                constant: false,
                pointer: false,
                const_pointer: false,
                dimensions: vec![],
            },
            formal_parameters: vec![FormalParameter {
                name: text("self"),
                core: ParameterCore {
                    id: ident(fp_id),
                    type_def_ref_id: ident("TD_STATE"),
                    constant: false,
                    pointer: true,
                    const_pointer: false,
                    dimensions: vec![],
                },
            }],
        }
    }

    fn data_reference(foreign_id: &str, component: &str) -> DataReference {
        DataReference {
            foreign: ForeignReference {
                manifest_reference_ref_id: ident("MR_AC"),
                foreign_ref_id: ident(foreign_id),
            },
            formal_parameter_ref_id: ident("FP_DOSTEP_SELF"),
            component_identifier: Some(text(component)),
        }
    }

    fn function_reference(foreign_id: &str, function_id: &str) -> FunctionReference {
        FunctionReference {
            foreign: ForeignReference {
                manifest_reference_ref_id: ident("MR_AC"),
                foreign_ref_id: ident(foreign_id),
            },
            function_ref_id: ident(function_id),
        }
    }

    /// Minimal Production Code manifest shaped like the GALEC builder's
    /// output: `.h`/`.c` code files, alias typedefs to the target types, a
    /// state struct with one field, three void block-method functions taking
    /// `self`, and a LogicalData mapping the one AC variable `V_T` plus all
    /// three block methods.
    fn base_parts() -> ProductionCodeManifestParts {
        ProductionCodeManifestParts {
            attributes: attributes(),
            manifest_reference: ManifestReference {
                id: ident("MR_AC"),
                manifest_ref_id: ManifestId::parse(AC_UUID).unwrap(),
                checksum: Sha1Hex::of_bytes(b"algorithm code manifest bytes"),
            },
            files: vec![
                code_file_entry("F_H", "TestBlock.h"),
                code_file_entry("F_C", "TestBlock.c"),
            ],
            code_container: CodeContainer {
                language: SupportedLanguage::C,
                standard: text("C99"),
                platform: SupportedPlatform::Legacy,
                float_precision: FloatPrecision::Bits64,
                description: None,
                target: text("Generic"),
                target_types: vec![
                    TargetType {
                        id: ident("TT_F64"),
                        kind: TargetTypeKind::EfmiFloat64,
                        coded_type: text("double"),
                    },
                    TargetType {
                        id: ident("TT_VOID"),
                        kind: TargetTypeKind::EfmiVoid,
                        coded_type: text("void"),
                    },
                ],
                code_files: vec![
                    CodeFile {
                        id: ident("CF_H"),
                        file_type: CodeFileType::ProductionCode,
                        code_type: CodeType::HeaderFile,
                        file_ref_id: ident("F_H"),
                        file_ref_kind: None,
                        includes: vec![],
                        typedefs: vec![
                            alias_typedef("TD_F64", "double", "TT_F64"),
                            alias_typedef("TD_VOID", "void", "TT_VOID"),
                            Typedef {
                                id: ident("TD_STATE"),
                                name: text("TestBlock_state"),
                                body: TypedefBody::Components(vec![scalar_component("CO_0", "x")]),
                            },
                        ],
                        functions: vec![],
                    },
                    CodeFile {
                        id: ident("CF_C"),
                        file_type: CodeFileType::ProductionCode,
                        code_type: CodeType::SourceFile,
                        file_ref_id: ident("F_C"),
                        file_ref_kind: None,
                        includes: vec![ident("CF_H")],
                        typedefs: vec![],
                        functions: vec![
                            void_self_function(
                                "FN_STARTUP",
                                "TestBlock_startup",
                                "RP_STARTUP",
                                "FP_STARTUP_SELF",
                            ),
                            void_self_function(
                                "FN_RECALIBRATE",
                                "TestBlock_recalibrate",
                                "RP_RECALIBRATE",
                                "FP_RECALIBRATE_SELF",
                            ),
                            void_self_function(
                                "FN_DOSTEP",
                                "TestBlock_dostep",
                                "RP_DOSTEP",
                                "FP_DOSTEP_SELF",
                            ),
                        ],
                    },
                ],
                logical_data: LogicalData {
                    data_references: vec![data_reference("V_T", "x")],
                    function_references: vec![
                        function_reference("BM_STARTUP", "FN_STARTUP"),
                        function_reference("BM_RECALIBRATE", "FN_RECALIBRATE"),
                        function_reference("BM_DOSTEP", "FN_DOSTEP"),
                    ],
                },
            },
            annotations: vec![],
        }
    }

    fn ac_real_variable(id: &str, name: &str, dimensions: Vec<u64>) -> Variable {
        Variable::Real(RealVariable {
            common: VariableCommon {
                id: ident(id),
                name: text(name),
                description: None,
                block_causality: if dimensions.is_empty() {
                    BlockCausality::Constant
                } else {
                    BlockCausality::State
                },
                dimensions,
                annotations: vec![],
            },
            start: StartValue::Scalar(0.0),
            unit_ref_id: None,
            relative_quantity: false,
            min: None,
            max: None,
            nominal: None,
        })
    }

    /// Algorithm Code counterpart of [`base_parts`]: id [`AC_UUID`], block
    /// methods `BM_*`, `ErrorSignalStatus` `ESS`, one variable `V_T`.
    fn ac_manifest(extra_variables: Vec<Variable>) -> AlgorithmCodeManifest {
        let mut variables = vec![ac_real_variable("V_T", "T", vec![])];
        variables.extend(extra_variables);
        AlgorithmCodeManifest::new(AlgorithmCodeManifestParts {
            attributes: ManifestAttributes {
                id: ManifestId::parse(AC_UUID).unwrap(),
                name: text("TestBlock"),
                description: None,
                version: None,
                generation_date_and_time: UtcTimestamp::parse("2026-07-03T12:00:00Z").unwrap(),
                generation_tool: None,
                copyright: None,
                license: None,
            },
            file_ref_id: ident("F_ALG"),
            files: vec![File {
                id: ident("F_ALG"),
                name: NameWithoutSlashes::new("TestBlock.alg").unwrap(),
                path: FilePath::root(),
                checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(b"alg")),
                role: FileRole::Code,
                description: None,
            }],
            clock: Clock {
                id: ident("CLK"),
                variable_ref_id: ident("V_T"),
            },
            block_methods: BlockMethods {
                startup: BlockMethod {
                    id: ident("BM_STARTUP"),
                    signals: vec![],
                },
                recalibrate: BlockMethod {
                    id: ident("BM_RECALIBRATE"),
                    signals: vec![],
                },
                do_step: BlockMethod {
                    id: ident("BM_DOSTEP"),
                    signals: vec![],
                },
            },
            error_signal_status: ErrorSignalStatus { id: ident("ESS") },
            units: vec![],
            variables,
            annotations: vec![],
        })
        .unwrap()
    }

    /// Mutable access to the state-struct components of a parts fixture.
    fn state_components(parts: &mut ProductionCodeManifestParts) -> &mut Vec<Component> {
        let TypedefBody::Components(ref mut components) =
            parts.code_container.code_files[0].typedefs[2].body
        else {
            unreachable!()
        };
        components
    }

    // ---- model positives (§7) ----------------------------------------

    #[test]
    fn minimal_manifest_validates() {
        assert!(ProductionCodeManifest::new(base_parts()).is_ok());
    }

    #[test]
    fn array_variable_validates() {
        let mut parts = base_parts();
        state_components(&mut parts).push(Component {
            id: ident("CO_1"),
            name: text("arr"),
            type_def_ref_id: ident("TD_F64"),
            dimensions: vec![2, 3],
            pointer: false,
        });
        parts
            .code_container
            .logical_data
            .data_references
            .push(data_reference("V_ARR", "arr"));
        let pc = ProductionCodeManifest::new(parts).unwrap();
        // Arrays map whole-field: cross-validation against an AC manifest
        // carrying the array variable passes as-is.
        let ac = ac_manifest(vec![ac_real_variable("V_ARR", "arr", vec![2, 3])]);
        assert!(validate_against_algorithm_code(&pc, &ac).is_ok());
    }

    #[test]
    fn cross_validation_accepts_matching_manifests() {
        let pc = ProductionCodeManifest::new(base_parts()).unwrap();
        assert!(validate_against_algorithm_code(&pc, &ac_manifest(vec![])).is_ok());
    }

    /// Mapping the `ErrorSignalStatus` is permitted (never required).
    #[test]
    fn error_signal_status_mapping_permitted() {
        let mut parts = base_parts();
        parts
            .code_container
            .logical_data
            .data_references
            .push(data_reference("ESS", "status"));
        let pc = ProductionCodeManifest::new(parts).unwrap();
        assert!(validate_against_algorithm_code(&pc, &ac_manifest(vec![])).is_ok());
    }

    /// Guard for the serializer: the XSD literals with surprising spellings
    /// (space in `Classic AUTOSAR`, `efmiBool` not `efmiBoolean`, `-bit`
    /// suffixes) must never drift.
    #[test]
    fn xsd_literals_are_exact() {
        assert_eq!(SupportedLanguage::CPlusPlus.as_str(), "C++");
        assert_eq!(
            SupportedPlatform::ClassicAutosar.as_str(),
            "Classic AUTOSAR"
        );
        assert_eq!(
            SupportedPlatform::AdaptiveAutosar.as_str(),
            "Adaptive AUTOSAR"
        );
        assert_eq!(FloatPrecision::Bits16.as_str(), "16-bit");
        assert_eq!(FloatPrecision::Bits256.as_str(), "256-bit");
        assert_eq!(TargetTypeKind::EfmiBool.as_str(), "efmiBool");
        assert_eq!(
            TargetTypeKind::EfmiUnsignedInteger64.as_str(),
            "efmiUnsignedInteger64"
        );
        assert_eq!(TargetTypeKind::EfmiVoid.as_str(), "efmiVoid");
        assert_eq!(CodeFileType::ToolSpecificCode.as_str(), "ToolSpecificCode");
        assert_eq!(CodeType::HeaderFile.as_str(), "HeaderFile");
    }

    // ---- model negatives (§7 row "model negative") --------------------

    #[test]
    fn duplicate_id_across_sections_rejected() {
        let mut parts = base_parts();
        // TargetType id colliding with a File id: uniqueness is per
        // manifest, not per element type.
        parts.code_container.target_types[0].id = ident("F_H");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(err, EfmiError::DuplicateId { id: "F_H".into() });
    }

    #[test]
    fn unresolved_type_def_ref_rejected() {
        // A typeDefRefId naming a TargetType id must not resolve: the chain
        // is always Component -> Typedef -> TargetType.
        let mut parts = base_parts();
        state_components(&mut parts)[0].type_def_ref_id = ident("TT_F64");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "Component/@typeDefRefId".into(),
                id: "TT_F64".into()
            }
        );

        // Return-parameter arm.
        let mut parts = base_parts();
        parts.code_container.code_files[1].functions[2]
            .return_parameter
            .type_def_ref_id = ident("MISSING");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "TestBlock_dostep/ReturnParameter/@typeDefRefId".into(),
                id: "MISSING".into()
            }
        );

        // Formal-parameter arm.
        let mut parts = base_parts();
        parts.code_container.code_files[1].functions[0].formal_parameters[0]
            .core
            .type_def_ref_id = ident("MISSING");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "TestBlock_startup/self/@typeDefRefId".into(),
                id: "MISSING".into()
            }
        );

        // Alias-cascade arm.
        let mut parts = base_parts();
        parts.code_container.code_files[0].typedefs[0].body = TypedefBody::Alias {
            target_type_ref_id: ident("TT_F64"),
            type_def_ref_id: Some(ident("MISSING")),
        };
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "Alias/@typeDefRefId".into(),
                id: "MISSING".into()
            }
        );
    }

    #[test]
    fn unresolved_target_type_ref_rejected() {
        let mut parts = base_parts();
        parts.code_container.code_files[0].typedefs[0] =
            alias_typedef("TD_F64", "double", "TT_MISSING");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "Alias/@targetTypeRefId".into(),
                id: "TT_MISSING".into()
            }
        );
    }

    #[test]
    fn unresolved_code_file_include_rejected() {
        // Include references CodeFile ids, NOT File ids: pointing at the
        // header's File entry must fail.
        let mut parts = base_parts();
        parts.code_container.code_files[1].includes = vec![ident("F_H")];
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "Include/@codeFileRefId".into(),
                id: "F_H".into()
            }
        );
    }

    #[test]
    fn unresolved_formal_parameter_ref_rejected() {
        // Return parameters are not formal parameters: a formalParameterRefId
        // naming a ReturnParameter id must not resolve.
        let mut parts = base_parts();
        parts.code_container.logical_data.data_references[0].formal_parameter_ref_id =
            ident("RP_DOSTEP");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "FormalParameter/@formalParameterRefId".into(),
                id: "RP_DOSTEP".into()
            }
        );
    }

    #[test]
    fn unresolved_function_ref_rejected() {
        let mut parts = base_parts();
        parts.code_container.logical_data.function_references[2].function_ref_id =
            ident("FN_MISSING");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "GlobalFunction/@functionRefId".into(),
                id: "FN_MISSING".into()
            }
        );
    }

    #[test]
    fn unresolved_manifest_reference_ref_rejected() {
        let mut parts = base_parts();
        parts.code_container.logical_data.data_references[0]
            .foreign
            .manifest_reference_ref_id = ident("MR_OTHER");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "ForeignVariableReference/@manifestReferenceRefId".into(),
                id: "MR_OTHER".into()
            }
        );

        let mut parts = base_parts();
        parts.code_container.logical_data.function_references[0]
            .foreign
            .manifest_reference_ref_id = ident("MR_OTHER");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedReference {
                attribute: "ForeignFunctionReference/@manifestReferenceRefId".into(),
                id: "MR_OTHER".into()
            }
        );
    }

    #[test]
    fn file_ref_role_not_code_rejected() {
        let mut parts = base_parts();
        parts.files.push(File {
            id: ident("F_DOC"),
            name: NameWithoutSlashes::new("readme.txt").unwrap(),
            path: FilePath::root(),
            checksum: FileChecksum::NotNeeded,
            role: FileRole::Other,
            description: None,
        });
        parts.code_container.code_files[0].file_ref_id = ident("F_DOC");
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::FileRefRoleNotCode {
                id: "F_DOC".into(),
                role: "other".into()
            }
        );
    }

    #[test]
    fn empty_code_files_rejected() {
        let mut parts = base_parts();
        parts.code_container.code_files.clear();
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(err, EfmiError::EmptyCodeFiles);
        assert_eq!(err.code(), "EFM043");
    }

    #[test]
    fn zero_dimension_rejected() {
        // Component arm; dimension_number is the 0-based Dimension/@number.
        let mut parts = base_parts();
        state_components(&mut parts)[0].dimensions = vec![2, 0];
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::InvalidDimension {
                variable: "x".into(),
                dimension_number: 1,
                size: 0
            }
        );

        // Formal-parameter arm.
        let mut parts = base_parts();
        parts.code_container.code_files[1].functions[0].formal_parameters[0]
            .core
            .dimensions = vec![0];
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::InvalidDimension {
                variable: "TestBlock_startup/self".into(),
                dimension_number: 0,
                size: 0
            }
        );
    }

    #[test]
    fn duplicate_foreign_mapping_rejected() {
        // The same AC variable mapped by two DataReferences.
        let mut parts = base_parts();
        parts
            .code_container
            .logical_data
            .data_references
            .push(data_reference("V_T", "x"));
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::DuplicateForeignMapping {
                foreign_ref_id: "V_T".into()
            }
        );
        assert_eq!(err.code(), "EFM041");
    }

    /// §2.4 "all three BlockMethods mapped exactly once", more-than-once
    /// side: rejected at construction (V8), so
    /// [`validate_against_algorithm_code`] never sees it.
    #[test]
    fn block_method_mapped_twice_rejected_at_construction() {
        let mut parts = base_parts();
        parts
            .code_container
            .logical_data
            .function_references
            .push(function_reference("BM_DOSTEP", "FN_STARTUP"));
        let err = ProductionCodeManifest::new(parts).unwrap_err();
        assert_eq!(
            err,
            EfmiError::DuplicateForeignMapping {
                foreign_ref_id: "BM_DOSTEP".into()
            }
        );
    }

    // ---- cross-ref negatives (§7 row "cross-ref", §2.4) ----------------

    #[test]
    fn cross_unknown_data_foreign_ref_rejected() {
        let mut parts = base_parts();
        parts
            .code_container
            .logical_data
            .data_references
            .push(data_reference("V_MISSING", "y"));
        let pc = ProductionCodeManifest::new(parts).unwrap();
        let err = validate_against_algorithm_code(&pc, &ac_manifest(vec![])).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedForeignReference {
                attribute: "ForeignVariableReference/@foreignRefId".into(),
                id: "V_MISSING".into()
            }
        );
        assert_eq!(err.code(), "EFM039");
    }

    #[test]
    fn cross_unknown_function_foreign_ref_rejected() {
        let mut parts = base_parts();
        parts.code_container.logical_data.function_references[2]
            .foreign
            .foreign_ref_id = ident("BM_BOGUS");
        let pc = ProductionCodeManifest::new(parts).unwrap();
        let err = validate_against_algorithm_code(&pc, &ac_manifest(vec![])).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnresolvedForeignReference {
                attribute: "ForeignFunctionReference/@foreignRefId".into(),
                id: "BM_BOGUS".into()
            }
        );
    }

    #[test]
    fn cross_unmapped_variable_rejected() {
        let pc = ProductionCodeManifest::new(base_parts()).unwrap();
        let ac = ac_manifest(vec![ac_real_variable("V_X", "x_unmapped", vec![])]);
        let err = validate_against_algorithm_code(&pc, &ac).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnmappedAlgorithmCodeEntity {
                entity: "variable".into(),
                id: "V_X".into()
            }
        );
        assert_eq!(err.code(), "EFM040");
    }

    #[test]
    fn cross_unmapped_block_method_rejected() {
        let mut parts = base_parts();
        parts.code_container.logical_data.function_references.pop(); // drop BM_DOSTEP
        let pc = ProductionCodeManifest::new(parts).unwrap();
        let err = validate_against_algorithm_code(&pc, &ac_manifest(vec![])).unwrap_err();
        assert_eq!(
            err,
            EfmiError::UnmappedAlgorithmCodeEntity {
                entity: "block method DoStep".into(),
                id: "BM_DOSTEP".into()
            }
        );
    }

    #[test]
    fn cross_uuid_mismatch_rejected() {
        let mut parts = base_parts();
        parts.manifest_reference.manifest_ref_id =
            ManifestId::parse("{11111111-2222-3333-4444-555555555555}").unwrap();
        let pc = ProductionCodeManifest::new(parts).unwrap();
        let err = validate_against_algorithm_code(&pc, &ac_manifest(vec![])).unwrap_err();
        assert_eq!(
            err,
            EfmiError::ManifestReferenceUuidMismatch {
                referenced: "{11111111-2222-3333-4444-555555555555}".into(),
                actual: AC_UUID.into()
            }
        );
        assert_eq!(err.code(), "EFM042");
    }
}
