//! Product-agnostic, `#[derive(Serialize)]` context views the eFMI packaging
//! templates consume (contract §2, WI-1).
//!
//! This is the neutral-data vs product-framing line the SPEC_0034 D3
//! amendment asks for. Two layers:
//!
//! - [`ModelExport`] (Layer A) — pure semantics, no eFMI id strings, no
//!   `xsdVersion`/`kind`/UUID/checksum. A future non-eFMI product (e.g. a
//!   Rust-source package) reuses it verbatim. Mapping is expressed by
//!   **neutral keys** (1-based position / storage-field name), never eFMI
//!   anchors.
//! - [`EfmiManifestContext`] (Layer B) — the eFMI product adapter. Wraps a
//!   [`ModelExport`] and adds the eFMI identity + cross-reference graph (the
//!   local anchor id strings and their resolved references), the
//!   per-invocation packaging facts, the unit table, and pre-formatted
//!   `xs:double` lexical strings. It emits **no XML text**: element/attribute
//!   names, fixed `efmiVersion`/`xsdVersion`/`kind` literals, escaping, and
//!   wrapper cardinality all live in the templates.
//!
//! The eFMI-framed sub-views ([`AcManifestCtx`], [`PcManifestCtx`],
//! [`ContentCtx`], [`UnitCtx`], …) are built directly from the validated
//! typed models ([`crate::manifest_context::algorithm_code_manifest::AlgorithmCodeManifest`],
//! [`crate::manifest_context::production_code_manifest::ProductionCodeManifest`],
//! [`crate::manifest_context::content::Content`]) — construction already ran every
//! data-integrity validator (SPEC_0008 fail-early), so the views reflect
//! exactly what those validated models carry, with numeric lexicals
//! pre-formatted through the same `format_real` guarantee the
//! typed serializer uses (explicit decimal point, never exponent, finite
//! only). Booleans stay plain bools and ids stay plain strings; the template
//! owns the "emit only when true" / attribute-spelling rules (contract §3a).

use serde::Serialize;

use crate::manifest_context::algorithm_code_manifest::{
    AlgorithmCodeManifest, BlockCausality, BlockMethod, StartValue, Variable, VariableCommon,
};
use crate::manifest_context::content::{Content, ModelRepresentation};
use crate::manifest_context::manifest_common::{
    Annotation, BaseUnit, File, FileChecksum, ManifestAttributes, Unit,
};
use crate::manifest_context::production_code_manifest::{
    CodeContainer, CodeFile, Component, DataReference, FormalParameter, Function,
    FunctionReference, LogicalData, ManifestReference, ParameterCore, ProductionCodeManifest,
    TargetType, Typedef, TypedefBody,
};

/// Render a finite Real with an explicit decimal point (`2` → `2.0`), using
/// Rust's shortest round-trip formatting. Deterministic; non-finite values
/// have no `xs:double` lexical form under this scheme and are rejected at
/// model construction (EFM022/EFM033), so they can never reach this point.
/// (Formerly `xml::format_real`; the typed serializer is gone — SPEC_0034 D3
/// amended — but the same lexical guarantee is pre-formatted here.)
#[must_use]
fn format_real(value: f64) -> String {
    debug_assert!(
        value.is_finite(),
        "non-finite Reals must be rejected at model construction"
    );
    let rendered = format!("{value}");
    if rendered.contains('.') {
        rendered
    } else {
        format!("{rendered}.0")
    }
}

// ---------------------------------------------------------------------------
// Filters (contract §3b): registered on the manifest render env by the caller
// (`target_manifest.rs` / `galec_api.rs`). Kept as plain, unit-testable Rust
// so the crate carries no minijinja dependency; the render sites wrap them in
// a one-line closure. Autoescape is OFF everywhere, so **every** interpolated
// text value passes through [`xml_escape`]; every raw `f64` passes through
// [`xs_double`].
// ---------------------------------------------------------------------------

/// XML-escape a text value for either attribute or element content: the five
/// predefined entities `& < > " '`. Idempotent on the restricted-charset
/// fields the context carries, so applying it uniformly is safe and
/// auditable. Control-char rejection is NOT this filter's job — that is the
/// `NormalizedText`/`invalid_attribute_char_reason` validator on the context
/// (contract §3b/§5), the only guard against non-well-formed bytes.
#[must_use]
pub fn xml_escape(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&apos;"),
            _ => out.push(ch),
        }
    }
    out
}

/// Render a finite `f64` as a valid `xs:double` lexical form, reusing
/// `format_real`'s guarantee: an explicit decimal point
/// (`2` → `2.0`), never exponent notation, finite only. A naive `{{ value }}`
/// would emit `2` (bare int) or `1e300` (exponent), both xmllint-invalid.
#[must_use]
pub fn xs_double(value: f64) -> String {
    format_real(value)
}

// ---------------------------------------------------------------------------
// Layer A — ModelExport (neutral; reusable by a non-eFMI product)
// ---------------------------------------------------------------------------

/// Neutral semantic export of a compiled block (contract §2a). Carries no
/// eFMI id strings, no `xsdVersion`, no `kind`, no UUID, no checksum. The C
/// realization fields (`struct_name`/`function_prefix`/`include_guard`/
/// `methods`) are the typed-C-printer projection facts; the eFMI adapter does
/// not read them, but a Rust-source product would.
#[derive(Debug, Clone, Serialize)]
pub struct ModelExport {
    /// CLI id, dots→underscores; used in `[[files]]` paths.
    pub model_name: String,
    /// Block spelling (RAW, unescaped — the template escapes it).
    pub block_name: String,
    /// Block description, if any.
    pub description: Option<String>,
    /// State-struct type token (projection-owned).
    pub struct_name: String,
    /// `<prefix>_startup` / `_recalibrate` / `_dostep`.
    pub function_prefix: String,
    /// Header include guard token.
    pub include_guard: String,
    /// Ordered variables; 1-based position is the neutral key.
    pub variables: Vec<VarEntry>,
    /// Which [`VarEntry::key`] is the sample-period constant.
    pub clock_variable_key: u32,
}

/// One neutral variable (contract §2a). RAW typed values; each product
/// formats its own way.
#[derive(Debug, Clone, Serialize)]
pub struct VarEntry {
    /// 1-based ordered position — the neutral cross-reference key.
    pub key: u32,
    /// Full unique name.
    pub name: String,
    /// Description, if any.
    pub description: Option<String>,
    /// Scalar kind (from classification, never inferred — S8).
    pub scalar_type: ScalarKind,
    /// Block causality.
    pub causality: Causality,
    /// Dimension sizes; empty = scalar.
    pub dimensions: Vec<u64>,
    /// Initial value(s), RAW.
    pub start: StartView,
    /// Lower bound, RAW.
    pub min: Option<NumberView>,
    /// Upper bound, RAW.
    pub max: Option<NumberView>,
    /// Nominal value, RAW.
    pub nominal: Option<NumberView>,
    /// Unit, if any.
    pub unit: Option<UnitDecomp>,
    /// Ignore base-unit offset in conversions.
    pub relative_quantity: bool,
    /// Realized struct field name.
    pub storage_name: String,
    /// Realized storage type token (`double`/`int32_t`/`bool`).
    pub storage_type: StorageType,
}

/// Neutral scalar kind (contract §2a).
#[derive(Debug, Clone, Copy, Serialize)]
#[serde(rename_all = "PascalCase")]
pub enum ScalarKind {
    /// A Real (double) scalar.
    Real,
    /// An Integer (int32) scalar.
    Integer,
    /// A Boolean scalar.
    Boolean,
}

/// Neutral causality (contract §2a; the eFMI `blockCausality` spellings).
#[derive(Debug, Clone, Copy, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Causality {
    /// Provided from the environment at step start.
    Input,
    /// Provided to the environment at step end.
    Output,
    /// Independent parameter, calibratable between steps.
    TunableParameter,
    /// Computed from other parameters at init/recalibration.
    DependentParameter,
    /// Value fixed by `start`, never changes.
    Constant,
    /// Protected state passed between method calls.
    State,
}

/// Neutral storage-type token of a realized struct field (contract §2a).
#[derive(Debug, Clone, Copy, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum StorageType {
    /// `double`.
    Double,
    /// `int32_t`.
    Int32T,
    /// `bool`.
    Bool,
}

/// RAW start value: a scalar broadcast or a row-major element list
/// (contract §2a).
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum StartView {
    /// One value; for arrays it applies to every element.
    Scalar {
        /// The single value.
        value: NumberView,
    },
    /// Row-major full element list.
    Array {
        /// The element values.
        values: Vec<NumberView>,
    },
}

/// RAW numeric literal (contract §2a): a Real, Integer, or Boolean value.
#[derive(Debug, Clone, Copy, Serialize)]
#[serde(untagged)]
pub enum NumberView {
    /// A Real value.
    Real(f64),
    /// An Integer value.
    Integer(i32),
    /// A Boolean value.
    Boolean(bool),
}

/// Neutral unit decomposition (contract §2a).
#[derive(Debug, Clone, Serialize)]
pub struct UnitDecomp {
    /// Unit name.
    pub name: String,
    /// Optional SI base-unit decomposition.
    pub base_unit: Option<BaseUnitExp>,
}

/// SI base-unit exponents plus conversion factor/offset (contract §2a).
#[derive(Debug, Clone, Copy, Serialize)]
pub struct BaseUnitExp {
    /// kg exponent.
    pub kg: i32,
    /// m exponent.
    pub m: i32,
    /// s exponent.
    pub s: i32,
    /// A exponent.
    pub ampere: i32,
    /// K exponent.
    pub kelvin: i32,
    /// mol exponent.
    pub mol: i32,
    /// cd exponent.
    pub cd: i32,
    /// rad exponent.
    pub rad: i32,
    /// Conversion factor (RAW; template applies `xs_double`).
    pub factor: f64,
    /// Conversion offset (RAW; template applies `xs_double`).
    pub offset: f64,
}

impl From<&BaseUnit> for BaseUnitExp {
    fn from(base: &BaseUnit) -> Self {
        Self {
            kg: base.kg,
            m: base.m,
            s: base.s,
            ampere: base.ampere,
            kelvin: base.kelvin,
            mol: base.mol,
            cd: base.cd,
            rad: base.rad,
            factor: base.factor,
            offset: base.offset,
        }
    }
}

// ---------------------------------------------------------------------------
// Layer B — EfmiManifestContext (product adapter; validated; no XML text)
// ---------------------------------------------------------------------------

/// The full eFMI render context (contract §2b): the neutral [`ModelExport`]
/// plus the eFMI identity/cross-reference graph, per-invocation packaging,
/// the unit table, and the pre-formatted eFMI-framed manifest views. Emits no
/// XML — templates own every element/attribute name and fixed literal.
///
/// Templates consume it as the single root `ctx`: `ctx.ac`, `ctx.pc`,
/// `ctx.units`, `ctx.content`, `ctx.packaging`, `ctx.model`.
#[derive(Debug, Clone, Serialize)]
pub struct EfmiManifestContext {
    /// Neutral semantics (Layer A).
    pub model: ModelExport,
    /// Per-invocation packaging facts (minted once).
    pub packaging: Packaging,
    /// Algorithm Code manifest view.
    pub ac: AcManifestCtx,
    /// Production Code manifest view; absent for the AC-only `galec` target.
    pub pc: Option<PcManifestCtx>,
    /// SI unit table (shared; the AC manifest's `Units` list).
    pub units: Vec<UnitCtx>,
    /// `__content.xml` registry view.
    pub content: ContentCtx,
}

/// Per-invocation packaging facts, minted once (contract §2b). The distinct
/// manifest UUIDs also live on each manifest view's `attributes.id`; these are
/// the shared identity handles the `__content.xml` `manifestRefId`s and the PC
/// `ManifestReference/@manifestRefId` agree with (contract §4d — shared
/// context, never a template mint).
#[derive(Debug, Clone, Serialize)]
pub struct Packaging {
    /// AC manifest UUID (brace-wrapped).
    pub ac_manifest_id: String,
    /// PC manifest UUID (brace-wrapped); absent for the AC-only target.
    pub pc_manifest_id: Option<String>,
    /// `__content.xml` UUID (brace-wrapped).
    pub content_id: String,
    /// Strict UTC `YYYY-MM-DDTHH:MM:SSZ`.
    pub generated_at: String,
    /// Generation tool string (`"rumoca X.Y.Z"`).
    pub generation_tool: Option<String>,
    /// `AlgorithmCode/<model>.alg` file name.
    pub alg_file_name: String,
    /// `ProductionCode/<model>.h` file name (absent for AC-only).
    pub header_file_name: Option<String>,
    /// `ProductionCode/<model>.c` file name (absent for AC-only).
    pub source_file_name: Option<String>,
}

/// Shared `efmiManifestAttributesBase` group (contract §3a: `efmiVersion` is a
/// template literal, so it is deliberately absent here).
#[derive(Debug, Clone, Serialize)]
pub struct AttributesCtx {
    /// Brace-wrapped manifest UUID.
    pub id: String,
    /// Block/eFMU name (RAW).
    pub name: String,
    /// Description (RAW), if any.
    pub description: Option<String>,
    /// Block version (RAW), if any.
    pub version: Option<String>,
    /// Strict UTC last-modification timestamp.
    pub generation_date_and_time: String,
    /// Generating tool (RAW), if any.
    pub generation_tool: Option<String>,
    /// Copyright (RAW), if any.
    pub copyright: Option<String>,
    /// License (RAW), if any.
    pub license: Option<String>,
}

impl From<&ManifestAttributes> for AttributesCtx {
    fn from(attributes: &ManifestAttributes) -> Self {
        Self {
            id: attributes.id.to_string(),
            name: attributes.name.as_str().to_owned(),
            description: opt_text(attributes.description.as_ref()),
            version: opt_text(attributes.version.as_ref()),
            generation_date_and_time: attributes.generation_date_and_time.to_string(),
            generation_tool: opt_text(attributes.generation_tool.as_ref()),
            copyright: opt_text(attributes.copyright.as_ref()),
            license: opt_text(attributes.license.as_ref()),
        }
    }
}

/// One `File` entry view (contract §3a). `needs_checksum`/`checksum` mirror the
/// typed `FileChecksum` coupling.
#[derive(Debug, Clone, Serialize)]
pub struct FileCtx {
    /// File anchor id.
    pub id: String,
    /// File name including suffix (RAW).
    pub name: String,
    /// Directory part relative to the container root (RAW).
    pub path: String,
    /// `needsChecksum` flag.
    pub needs_checksum: bool,
    /// SHA-1 hex when `needs_checksum`; `None` otherwise.
    pub checksum: Option<String>,
    /// `role` enum literal.
    pub role: String,
    /// Free-text description (RAW), if any.
    pub description: Option<String>,
}

impl From<&File> for FileCtx {
    fn from(file: &File) -> Self {
        let (needs_checksum, checksum) = match &file.checksum {
            FileChecksum::Sha1(sha1) => (true, Some(sha1.as_str().to_owned())),
            FileChecksum::NotNeeded => (false, None),
        };
        Self {
            id: file.id.as_str().to_owned(),
            name: file.name.as_str().to_owned(),
            path: file.path.as_str().to_owned(),
            needs_checksum,
            checksum,
            role: file.role.as_str().to_owned(),
            description: opt_text(file.description.as_ref()),
        }
    }
}

/// One vendor `Annotation` view (contract §3a).
#[derive(Debug, Clone, Serialize)]
pub struct AnnotationCtx {
    /// Reverse-domain tool name (RAW).
    pub annotation_type: String,
}

impl From<&Annotation> for AnnotationCtx {
    fn from(annotation: &Annotation) -> Self {
        Self {
            annotation_type: annotation.annotation_type.as_str().to_owned(),
        }
    }
}

/// One `Unit` view (contract §3a). `factor`/`offset` stay RAW `f64` so the
/// template exercises `xs_double`; exponents stay `i32` for the only-when-
/// nonzero rule.
#[derive(Debug, Clone, Serialize)]
pub struct UnitCtx {
    /// Unit anchor id.
    pub id: String,
    /// Unit name (RAW).
    pub name: String,
    /// SI decomposition, if any.
    pub base_unit: Option<BaseUnitExp>,
}

impl From<&Unit> for UnitCtx {
    fn from(unit: &Unit) -> Self {
        Self {
            id: unit.id.as_str().to_owned(),
            name: unit.name.as_str().to_owned(),
            base_unit: unit.base_unit.as_ref().map(BaseUnitExp::from),
        }
    }
}

/// One eFMI-framed `Variable` view (contract §3a). `start`/`min`/`max`/
/// `nominal` are pre-formatted lexical strings (row-major space-joined start),
/// so the template interpolates them directly; the self-closing decision is
/// `dimensions` empty and `annotations` empty.
#[derive(Debug, Clone, Serialize)]
pub struct VariableCtx {
    /// `"Real"` | `"Integer"` | `"Boolean"` — which element to emit.
    pub kind: &'static str,
    /// Variable anchor id.
    pub id: String,
    /// Full unique name (RAW).
    pub name: String,
    /// Description (RAW), if any.
    pub description: Option<String>,
    /// `blockCausality` enum literal.
    pub block_causality: &'static str,
    /// Dimension sizes; empty = scalar (1-based `Dimension/@number` in AC).
    pub dimensions: Vec<u64>,
    /// Pre-formatted, row-major space-joined `start` lexical.
    pub start: String,
    /// Vendor annotations.
    pub annotations: Vec<AnnotationCtx>,
    /// `unitRefId` (Real only), if any.
    pub unit_ref_id: Option<String>,
    /// `relativeQuantity` (Real only; emit only when true).
    pub relative_quantity: bool,
    /// Pre-formatted `min` lexical (Real/Integer), if any.
    pub min: Option<String>,
    /// Pre-formatted `max` lexical (Real/Integer), if any.
    pub max: Option<String>,
    /// Pre-formatted `nominal` lexical (Real only), if any.
    pub nominal: Option<String>,
}

/// Algorithm Code manifest view (contract §3a AC child order:
/// `Files` → `Clock` → `BlockMethods` → `ErrorSignalStatus` → `Units` →
/// `Variables` → `Annotations`).
#[derive(Debug, Clone, Serialize)]
pub struct AcManifestCtx {
    /// Shared top-level attributes.
    pub attributes: AttributesCtx,
    /// Root `fileRefId` (the `role="Code"` `.alg` file).
    pub file_ref_id: String,
    /// `Files` list (wrapper always emitted, even when empty).
    pub files: Vec<FileCtx>,
    /// `Clock` element.
    pub clock: ClockCtx,
    /// `BlockMethods` (Startup, Recalibrate, DoStep in that order).
    pub block_methods: BlockMethodsCtx,
    /// `ErrorSignalStatus/@id`.
    pub error_signal_status_id: String,
    /// Ordered `Variables` (always non-empty).
    pub variables: Vec<VariableCtx>,
    /// Manifest-level annotations (wrapper absent when empty).
    pub annotations: Vec<AnnotationCtx>,
}

/// `Clock` view.
#[derive(Debug, Clone, Serialize)]
pub struct ClockCtx {
    /// Sample-period anchor id.
    pub id: String,
    /// Reference to the sample-period constant.
    pub variable_ref_id: String,
}

/// `BlockMethods` view — exactly three named methods.
#[derive(Debug, Clone, Serialize)]
pub struct BlockMethodsCtx {
    /// The `Startup` method.
    pub startup: BlockMethodCtx,
    /// The `Recalibrate` method.
    pub recalibrate: BlockMethodCtx,
    /// The `DoStep` method.
    pub do_step: BlockMethodCtx,
}

/// One `BlockMethod` view (self-closing iff `signals` empty).
#[derive(Debug, Clone, Serialize)]
pub struct BlockMethodCtx {
    /// Method anchor id.
    pub id: String,
    /// `kind` literal (`Startup`/`Recalibrate`/`DoStep`).
    pub kind: &'static str,
    /// Exposed error-signal enum literals.
    pub signals: Vec<String>,
}

/// Production Code manifest view (contract §3a PC child order:
/// `ManifestReferences` → `Files` → `CodeContainer` → `Annotations`).
#[derive(Debug, Clone, Serialize)]
pub struct PcManifestCtx {
    /// Shared top-level attributes.
    pub attributes: AttributesCtx,
    /// The single `ManifestReference` (`origin="true"` is a template literal).
    pub manifest_reference: ManifestReferenceCtx,
    /// `Files` list.
    pub files: Vec<FileCtx>,
    /// `CodeContainer`.
    pub code_container: CodeContainerCtx,
    /// Manifest-level annotations (wrapper absent when empty).
    pub annotations: Vec<AnnotationCtx>,
}

/// `ManifestReference` view.
#[derive(Debug, Clone, Serialize)]
pub struct ManifestReferenceCtx {
    /// Local anchor id (`MR_AC`).
    pub id: String,
    /// Referenced AC manifest UUID.
    pub manifest_ref_id: String,
    /// SHA-1 of the exact rendered AC manifest bytes.
    pub checksum: String,
}

/// `CodeContainer` view.
#[derive(Debug, Clone, Serialize)]
pub struct CodeContainerCtx {
    /// `language` enum literal.
    pub language: String,
    /// `standard` free string (RAW).
    pub standard: String,
    /// `platform` enum literal.
    pub platform: String,
    /// `floatPrecision` enum literal.
    pub float_precision: String,
    /// `description` (RAW), if any.
    pub description: Option<String>,
    /// `<Target>` text element (RAW).
    pub target: String,
    /// `TargetTypes` (wrapper absent when empty).
    pub target_types: Vec<TargetTypeCtx>,
    /// `CodeFiles` (wrapper always emitted, non-empty by validation).
    pub code_files: Vec<CodeFileCtx>,
    /// `LogicalData` (both inner wrappers always emitted).
    pub logical_data: LogicalDataCtx,
}

/// `TargetType` view.
#[derive(Debug, Clone, Serialize)]
pub struct TargetTypeCtx {
    /// Target-type anchor id.
    pub id: String,
    /// eFMI data-type `kind` enum literal.
    pub kind: &'static str,
    /// Platform type spelling (RAW).
    pub coded_type: String,
}

/// `CodeFile` view.
#[derive(Debug, Clone, Serialize)]
pub struct CodeFileCtx {
    /// Code-file anchor id.
    pub id: String,
    /// `fileType` enum literal.
    pub file_type: &'static str,
    /// `codeType` enum literal.
    pub code_type: &'static str,
    /// `FileReference/@fileRefId`.
    pub file_ref_id: String,
    /// `FileReference/@kind` (RAW), if any.
    pub file_ref_kind: Option<String>,
    /// `Includes/Include/@codeFileRefId` (wrapper absent when empty).
    pub includes: Vec<String>,
    /// `Typedefs` (wrapper absent when empty).
    pub typedefs: Vec<TypedefCtx>,
    /// `Functions` (wrapper absent when empty).
    pub functions: Vec<FunctionCtx>,
}

/// `Typedef` view — exactly one of `alias`/`components` is `Some`.
#[derive(Debug, Clone, Serialize)]
pub struct TypedefCtx {
    /// Typedef anchor id.
    pub id: String,
    /// C type token (RAW).
    pub name: String,
    /// `Alias` body, if this is an alias typedef.
    pub alias: Option<AliasCtx>,
    /// `Components` body, if this is a struct typedef.
    pub components: Option<Vec<ComponentCtx>>,
}

/// `Alias` typedef body view.
#[derive(Debug, Clone, Serialize)]
pub struct AliasCtx {
    /// `targetTypeRefId` (a `TargetType` id).
    pub target_type_ref_id: String,
    /// Optional `typeDefRefId` cascade (a `Typedef` id).
    pub type_def_ref_id: Option<String>,
}

/// `Component` view (self-closing iff `dimensions` empty; 0-based
/// `Dimension/@number` in PC).
#[derive(Debug, Clone, Serialize)]
pub struct ComponentCtx {
    /// Component anchor id.
    pub id: String,
    /// C field token (RAW).
    pub name: String,
    /// Element type (`typeDefRefId` → a `Typedef` id).
    pub type_def_ref_id: String,
    /// Dimension sizes; empty = scalar.
    pub dimensions: Vec<u64>,
    /// `pointer` (emit only when true).
    pub pointer: bool,
}

/// `Function` view.
#[derive(Debug, Clone, Serialize)]
pub struct FunctionCtx {
    /// Function anchor id.
    pub id: String,
    /// C function token (RAW).
    pub name: String,
    /// `ReturnParameter` attribute group (no name/number).
    pub return_parameter: ParameterCtx,
    /// `FormalParameters` (wrapper absent when empty; 0-based `@number`).
    pub formal_parameters: Vec<FormalParameterCtx>,
}

/// The `FunctionParameterAttributes` group view (self-closing iff
/// `dimensions` empty; XSD-defaulted booleans emit only when true).
#[derive(Debug, Clone, Serialize)]
pub struct ParameterCtx {
    /// Parameter anchor id.
    pub id: String,
    /// Parameter type (`typeDefRefId` → a `Typedef` id).
    pub type_def_ref_id: String,
    /// `const` (emit only when true).
    pub constant: bool,
    /// `pointer` (emit only when true).
    pub pointer: bool,
    /// `constPointer` (emit only when true).
    pub const_pointer: bool,
    /// Dimension sizes; empty = scalar (0-based `Dimension/@number`).
    pub dimensions: Vec<u64>,
}

impl From<&ParameterCore> for ParameterCtx {
    fn from(core: &ParameterCore) -> Self {
        Self {
            id: core.id.as_str().to_owned(),
            type_def_ref_id: core.type_def_ref_id.as_str().to_owned(),
            constant: core.constant,
            pointer: core.pointer,
            const_pointer: core.const_pointer,
            dimensions: core.dimensions.clone(),
        }
    }
}

/// One `FormalParameter` view (its `@number` is `loop.index0` in the
/// template).
#[derive(Debug, Clone, Serialize)]
pub struct FormalParameterCtx {
    /// C parameter token (RAW).
    pub name: String,
    /// Shared attribute group.
    pub core: ParameterCtx,
}

impl From<&FormalParameter> for FormalParameterCtx {
    fn from(parameter: &FormalParameter) -> Self {
        Self {
            name: parameter.name.as_str().to_owned(),
            core: ParameterCtx::from(&parameter.core),
        }
    }
}

/// `LogicalData` view — both inner wrappers always emitted, even when empty.
#[derive(Debug, Clone, Serialize)]
pub struct LogicalDataCtx {
    /// `DataReferences` children.
    pub data_references: Vec<DataReferenceCtx>,
    /// `FunctionReferences` children.
    pub function_references: Vec<FunctionReferenceCtx>,
}

/// One `DataReference` view.
#[derive(Debug, Clone, Serialize)]
pub struct DataReferenceCtx {
    /// `ForeignVariableReference/@manifestReferenceRefId`.
    pub manifest_reference_ref_id: String,
    /// `ForeignVariableReference/@foreignRefId` (an AC variable or ESS id).
    pub foreign_ref_id: String,
    /// `FormalParameter/@formalParameterRefId`.
    pub formal_parameter_ref_id: String,
    /// `FormalParameter/@componentIdentifier` (RAW), if any.
    pub component_identifier: Option<String>,
}

impl From<&DataReference> for DataReferenceCtx {
    fn from(reference: &DataReference) -> Self {
        Self {
            manifest_reference_ref_id: reference
                .foreign
                .manifest_reference_ref_id
                .as_str()
                .to_owned(),
            foreign_ref_id: reference.foreign.foreign_ref_id.as_str().to_owned(),
            formal_parameter_ref_id: reference.formal_parameter_ref_id.as_str().to_owned(),
            component_identifier: opt_text(reference.component_identifier.as_ref()),
        }
    }
}

/// One `FunctionReference` view.
#[derive(Debug, Clone, Serialize)]
pub struct FunctionReferenceCtx {
    /// `ForeignFunctionReference/@manifestReferenceRefId`.
    pub manifest_reference_ref_id: String,
    /// `ForeignFunctionReference/@foreignRefId` (a block-method id).
    pub foreign_ref_id: String,
    /// `GlobalFunction/@functionRefId`.
    pub function_ref_id: String,
}

impl From<&FunctionReference> for FunctionReferenceCtx {
    fn from(reference: &FunctionReference) -> Self {
        Self {
            manifest_reference_ref_id: reference
                .foreign
                .manifest_reference_ref_id
                .as_str()
                .to_owned(),
            foreign_ref_id: reference.foreign.foreign_ref_id.as_str().to_owned(),
            function_ref_id: reference.function_ref_id.as_str().to_owned(),
        }
    }
}

/// `__content.xml` registry view (contract §3a: shared attrs then one empty
/// `<ModelRepresentation>` per registered representation; `activeFmu` is
/// omitted when absent, and rumoca never sets it).
#[derive(Debug, Clone, Serialize)]
pub struct ContentCtx {
    /// Shared top-level attributes.
    pub attributes: AttributesCtx,
    /// `activeFmu` (RAW), if set.
    pub active_fmu: Option<String>,
    /// Registered representations in emission order.
    pub representations: Vec<RepresentationCtx>,
}

/// One `ModelRepresentation` view.
#[derive(Debug, Clone, Serialize)]
pub struct RepresentationCtx {
    /// Container name / root directory (RAW).
    pub name: String,
    /// Representation `kind` enum literal.
    pub kind: &'static str,
    /// Manifest file name (RAW).
    pub manifest: String,
    /// SHA-1 of the raw manifest bytes (web-injected).
    pub checksum: String,
    /// Manifest UUID (shared context, not template-minted).
    pub manifest_ref_id: String,
}

// ---------------------------------------------------------------------------
// Constructors from the validated typed models
// ---------------------------------------------------------------------------

impl AcManifestCtx {
    /// Build the eFMI-framed AC view from a validated manifest.
    #[must_use]
    pub fn from_manifest(manifest: &AlgorithmCodeManifest) -> Self {
        let parts = manifest.parts();
        Self {
            attributes: AttributesCtx::from(&parts.attributes),
            file_ref_id: parts.file_ref_id.as_str().to_owned(),
            files: parts.files.iter().map(FileCtx::from).collect(),
            clock: ClockCtx {
                id: parts.clock.id.as_str().to_owned(),
                variable_ref_id: parts.clock.variable_ref_id.as_str().to_owned(),
            },
            block_methods: BlockMethodsCtx {
                startup: block_method_ctx("Startup", &parts.block_methods.startup),
                recalibrate: block_method_ctx("Recalibrate", &parts.block_methods.recalibrate),
                do_step: block_method_ctx("DoStep", &parts.block_methods.do_step),
            },
            error_signal_status_id: parts.error_signal_status.id.as_str().to_owned(),
            variables: parts.variables.iter().map(variable_ctx).collect(),
            annotations: parts.annotations.iter().map(AnnotationCtx::from).collect(),
        }
    }

    /// The AC manifest's `Units` list as the shared unit table.
    #[must_use]
    pub fn units(manifest: &AlgorithmCodeManifest) -> Vec<UnitCtx> {
        manifest.parts().units.iter().map(UnitCtx::from).collect()
    }
}

impl PcManifestCtx {
    /// Build the eFMI-framed PC view from a validated manifest.
    #[must_use]
    pub fn from_manifest(manifest: &ProductionCodeManifest) -> Self {
        let parts = manifest.parts();
        Self {
            attributes: AttributesCtx::from(&parts.attributes),
            manifest_reference: ManifestReferenceCtx::from(&parts.manifest_reference),
            files: parts.files.iter().map(FileCtx::from).collect(),
            code_container: CodeContainerCtx::from(&parts.code_container),
            annotations: parts.annotations.iter().map(AnnotationCtx::from).collect(),
        }
    }
}

impl From<&ManifestReference> for ManifestReferenceCtx {
    fn from(reference: &ManifestReference) -> Self {
        Self {
            id: reference.id.as_str().to_owned(),
            manifest_ref_id: reference.manifest_ref_id.to_string(),
            checksum: reference.checksum.as_str().to_owned(),
        }
    }
}

impl From<&CodeContainer> for CodeContainerCtx {
    fn from(container: &CodeContainer) -> Self {
        Self {
            language: container.language.as_str().to_owned(),
            standard: container.standard.as_str().to_owned(),
            platform: container.platform.as_str().to_owned(),
            float_precision: container.float_precision.as_str().to_owned(),
            description: opt_text(container.description.as_ref()),
            target: container.target.as_str().to_owned(),
            target_types: container
                .target_types
                .iter()
                .map(TargetTypeCtx::from)
                .collect(),
            code_files: container.code_files.iter().map(CodeFileCtx::from).collect(),
            logical_data: LogicalDataCtx::from(&container.logical_data),
        }
    }
}

impl From<&TargetType> for TargetTypeCtx {
    fn from(target_type: &TargetType) -> Self {
        Self {
            id: target_type.id.as_str().to_owned(),
            kind: target_type.kind.as_str(),
            coded_type: target_type.coded_type.as_str().to_owned(),
        }
    }
}

impl From<&CodeFile> for CodeFileCtx {
    fn from(code_file: &CodeFile) -> Self {
        Self {
            id: code_file.id.as_str().to_owned(),
            file_type: code_file.file_type.as_str(),
            code_type: code_file.code_type.as_str(),
            file_ref_id: code_file.file_ref_id.as_str().to_owned(),
            file_ref_kind: opt_text(code_file.file_ref_kind.as_ref()),
            includes: code_file
                .includes
                .iter()
                .map(|id| id.as_str().to_owned())
                .collect(),
            typedefs: code_file.typedefs.iter().map(TypedefCtx::from).collect(),
            functions: code_file.functions.iter().map(FunctionCtx::from).collect(),
        }
    }
}

impl From<&Typedef> for TypedefCtx {
    fn from(typedef: &Typedef) -> Self {
        let (alias, components) = match &typedef.body {
            TypedefBody::Alias {
                target_type_ref_id,
                type_def_ref_id,
            } => (
                Some(AliasCtx {
                    target_type_ref_id: target_type_ref_id.as_str().to_owned(),
                    type_def_ref_id: type_def_ref_id.as_ref().map(|id| id.as_str().to_owned()),
                }),
                None,
            ),
            TypedefBody::Components(components) => (
                None,
                Some(components.iter().map(ComponentCtx::from).collect()),
            ),
        };
        Self {
            id: typedef.id.as_str().to_owned(),
            name: typedef.name.as_str().to_owned(),
            alias,
            components,
        }
    }
}

impl From<&Component> for ComponentCtx {
    fn from(component: &Component) -> Self {
        Self {
            id: component.id.as_str().to_owned(),
            name: component.name.as_str().to_owned(),
            type_def_ref_id: component.type_def_ref_id.as_str().to_owned(),
            dimensions: component.dimensions.clone(),
            pointer: component.pointer,
        }
    }
}

impl From<&Function> for FunctionCtx {
    fn from(function: &Function) -> Self {
        Self {
            id: function.id.as_str().to_owned(),
            name: function.name.as_str().to_owned(),
            return_parameter: ParameterCtx::from(&function.return_parameter),
            formal_parameters: function
                .formal_parameters
                .iter()
                .map(FormalParameterCtx::from)
                .collect(),
        }
    }
}

impl From<&LogicalData> for LogicalDataCtx {
    fn from(logical_data: &LogicalData) -> Self {
        Self {
            data_references: logical_data
                .data_references
                .iter()
                .map(DataReferenceCtx::from)
                .collect(),
            function_references: logical_data
                .function_references
                .iter()
                .map(FunctionReferenceCtx::from)
                .collect(),
        }
    }
}

impl ContentCtx {
    /// Build the `__content.xml` view from a validated [`Content`] model.
    #[must_use]
    pub fn from_content(content: &Content) -> Self {
        let parts = content.parts();
        Self {
            attributes: AttributesCtx::from(&parts.attributes),
            active_fmu: parts
                .active_fmu
                .as_ref()
                .map(|name| name.as_str().to_owned()),
            representations: parts
                .model_representations
                .iter()
                .map(RepresentationCtx::from)
                .collect(),
        }
    }
}

impl From<&ModelRepresentation> for RepresentationCtx {
    fn from(representation: &ModelRepresentation) -> Self {
        Self {
            name: representation.name.as_str().to_owned(),
            kind: representation.kind.as_str(),
            manifest: representation.manifest.as_str().to_owned(),
            checksum: representation.checksum.as_str().to_owned(),
            manifest_ref_id: representation.manifest_ref_id.to_string(),
        }
    }
}

fn block_method_ctx(kind: &'static str, method: &BlockMethod) -> BlockMethodCtx {
    BlockMethodCtx {
        id: method.id.as_str().to_owned(),
        kind,
        signals: method
            .signals
            .iter()
            .map(|s| s.as_str().to_owned())
            .collect(),
    }
}

fn variable_ctx(variable: &Variable) -> VariableCtx {
    let common = variable.common();
    let (kind, start, unit_ref_id, relative_quantity, min, max, nominal) = match variable {
        Variable::Real(real) => (
            "Real",
            join_real_start(&real.start),
            real.unit_ref_id.as_ref().map(|id| id.as_str().to_owned()),
            real.relative_quantity,
            real.min.map(format_real),
            real.max.map(format_real),
            real.nominal.map(format_real),
        ),
        Variable::Integer(integer) => (
            "Integer",
            join_integer_start(&integer.start),
            None,
            false,
            integer.min.map(|v| v.to_string()),
            integer.max.map(|v| v.to_string()),
            None,
        ),
        Variable::Boolean(boolean) => (
            "Boolean",
            join_boolean_start(&boolean.start),
            None,
            false,
            None,
            None,
            None,
        ),
    };
    VariableCtx {
        kind,
        id: common.id.as_str().to_owned(),
        name: common.name.as_str().to_owned(),
        description: opt_text(common.description.as_ref()),
        block_causality: block_causality_str(common),
        dimensions: common.dimensions.clone(),
        start,
        annotations: common.annotations.iter().map(AnnotationCtx::from).collect(),
        unit_ref_id,
        relative_quantity,
        min,
        max,
        nominal,
    }
}

fn block_causality_str(common: &VariableCommon) -> &'static str {
    match common.block_causality {
        BlockCausality::Input => "input",
        BlockCausality::Output => "output",
        BlockCausality::TunableParameter => "tunableParameter",
        BlockCausality::DependentParameter => "dependentParameter",
        BlockCausality::Constant => "constant",
        BlockCausality::State => "state",
    }
}

/// Row-major, whitespace-separated `start` encoding (mirrors the typed
/// serializer's `join_start`); Reals via [`format_real`], Integers via
/// decimal, Booleans as `true`/`false`.
fn join_real_start(start: &StartValue<f64>) -> String {
    start
        .iter()
        .map(|v| format_real(*v))
        .collect::<Vec<_>>()
        .join(" ")
}

fn join_integer_start(start: &StartValue<i32>) -> String {
    start
        .iter()
        .map(|v| v.to_string())
        .collect::<Vec<_>>()
        .join(" ")
}

fn join_boolean_start(start: &StartValue<bool>) -> String {
    start
        .iter()
        .map(|v| {
            if *v {
                "true".to_owned()
            } else {
                "false".to_owned()
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// A [`crate::manifest_context::ids::NormalizedText`] option → owned `String` option.
fn opt_text(value: Option<&crate::manifest_context::ids::NormalizedText>) -> Option<String> {
    value.map(|text| text.as_str().to_owned())
}
