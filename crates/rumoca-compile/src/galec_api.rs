//! GALEC / eFMI Algorithm Code export facade (SPEC_0034 GAL-010).
//!
//! Frontends reach the `rumoca-galec-codegen` projection only through this
//! module: it supplies the auxiliary provenance the canonical DAE does not
//! carry — the [`ScalarTypeMap`] built from Flat-side declared types — and
//! drives the projection into the CLI's declarative eFMU packaging step.
//!
//! The container products are produced through [`GalecPackagingPlan`]: the
//! `rumoca` crate's generic checksum/container build step renders each
//! `target.toml` `[[files]]` in topological order, and for the manifest
//! templates it asks the plan for a product-agnostic context
//! ([`GalecPackagingPlan::template_ctx`]) — the typed models are assembled
//! and validated here (SPEC_0008 fail-early) but never serialized to XML;
//! the minijinja templates own all XML text (SPEC_0034 D3 amended). Every
//! SHA-1 in the checksum web is computed from the exact rendered producer
//! bytes (GAL-021: no placeholder checksums, ever).
//!
//! [`render_galec_c_export`] serves the non-eFMI `embedded-c-galec` target
//! (GAL-024): the serialized typed C-layout context, no manifest mapping.
//!
//! # Scalar-type provenance rules
//!
//! `rumoca_ir_dae::Variable` deliberately carries no scalar type, and the
//! projection refuses to guess (`ET011`, S8). The facade contributes the
//! one piece of provenance the projection cannot see: every Flat variable
//! whose `type_id` resolves to the builtin `Real` / `Integer` / `Boolean`
//! maps to the matching GALEC scalar type (flatten's
//! `resolve_primitive_type_id` already collapsed type aliases down to
//! these builtin ids).
//!
//! Generated variables the Flat model never declared — the when-condition
//! vector (`f_c` targets, Boolean per MLS B.1d) and `__pre__.<base>` slots
//! (inheriting their base variable's type) — are deliberately NOT entered
//! here: `rumoca-galec-codegen`'s `classify` module owns those structural
//! fallbacks, and map entries take precedence over them, so duplicating
//! the rules in the facade would let a stale copy silently win on drift.
//!
//! Anything else — `String`/`Clock`/enumeration/record types, unresolved
//! `TypeId::UNKNOWN` — stays absent from the map. Absence is loud, never a
//! default: the projection rejects untypeable variables with `ET011`.

use std::cell::RefCell;
use std::collections::BTreeMap;

use crate::codegen_target::{TargetBundle, TargetTemplateSource};
use rumoca_core::TypeId;
use rumoca_galec_codegen::manifest_context::algorithm_code_manifest::AlgorithmCodeManifest;
use rumoca_galec_codegen::manifest_context::content::{
    Content, ContentParts, ModelRepresentation, ModelRepresentationKind,
};
use rumoca_galec_codegen::manifest_context::manifest_common::ManifestAttributes;
use rumoca_galec_codegen::{
    AcManifestCtx, AlgorithmCodePackage, ContentCtx, EmittedCodeFile, GalecInput, GalecOptions,
    GalecTargetError, ManifestId, ManifestIdentity, NameWithoutSlashes, NormalizedText,
    PcManifestCtx, ScalarTypeMap, Sha1Hex, UtcTimestamp, assemble_manifest_with_identity,
    assemble_production_manifest_with_identity, c_template_context, lower_to_algorithm_code,
    render_algorithm_code,
};
use rumoca_ir_ast::TypeTable;
use rumoca_ir_dae::Dae;
use rumoca_ir_flat::Model as FlatModel;
use rumoca_ir_galec::ast::ScalarType;

/// SPEC_0008-shaped failure of the export facade.
#[derive(Debug, thiserror::Error)]
pub enum GalecExportError {
    /// The projection rejected the model (admissibility, classification,
    /// or lowering diagnostics — every collected `ET0xx`).
    #[error("GALEC projection rejected the model:\n{}", render_diagnostics(.0))]
    Projection(Vec<GalecTargetError>),
    /// Rendering a validated package failed (validator/printer/serializer).
    #[error("GALEC rendering failed: {0}")]
    Render(#[from] GalecTargetError),
    /// Rendering the shared `embedded-c-galec` C-layout templates failed:
    /// the builtin bundle/template is missing (a build-system invariant
    /// break) or strict-undefined interpolation rejected the context.
    #[error("GALEC C-layout template rendering failed: {detail}")]
    CTemplate { detail: String },
    /// An internal invariant break in the packaging plan (a build-step bug):
    /// a missing or malformed injected checksum, a checksum-web topological
    /// order violation, or a context serialization failure. Distinct from
    /// [`Self::CTemplate`] so a misdeclared checksum web is not reported as a
    /// C-layout *template* failure (SPEC_0008 structured, correctly-attributed
    /// errors).
    #[error("GALEC packaging invariant broken: {detail}")]
    Internal { detail: String },
    /// [`render_galec_sources`] was asked for a target that is not one of the
    /// GALEC codegen targets.
    #[error(
        "'{0}' is not a GALEC codegen target (expected {GALEC_TARGET}, {GALEC_PRODUCTION_TARGET}, or {EMBEDDED_C_GALEC_TARGET})"
    )]
    UnknownTarget(String),
}

fn render_diagnostics(diagnostics: &[GalecTargetError]) -> String {
    diagnostics
        .iter()
        .map(|diagnostic| format!("  - {diagnostic}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// GALEC-derived embedded C export (the `embedded-c-galec` target,
/// SPEC_0034 GAL-024): the serialized typed template context the target's
/// minijinja templates consume. Explicitly NOT an eFMI Production Code
/// container — no manifest mapping is emitted; the honest self-description
/// lives in the target manifest.
#[derive(Debug, Clone)]
pub struct GalecCExport {
    /// Serialized typed `CContext` (`rumoca-galec-codegen`'s `emit` module):
    /// `model_name`, `block_name`, `struct_name`, `function_prefix`,
    /// `include_guard`, `variables`, `methods` — the complete template
    /// context, C text intelligence stays in the typed printer (D2/GAL-008).
    pub context: serde_json::Value,
}

/// Render the embedded C export context for a compiled model: the same
/// projection every export shares, serialized through `c_template_context`
/// instead of the `.alg`/manifest printers.
///
/// # Errors
///
/// [`GalecExportError::Projection`] with all collected projection
/// diagnostics, or [`GalecExportError::Render`] for validator/mangler/
/// C-printer failures on the validated package (`ET018`/`ET022`/`ET023`).
pub fn render_galec_c_export(
    dae: &Dae,
    flat: &FlatModel,
    model_name: &str,
) -> Result<GalecCExport, GalecExportError> {
    let package = lower_package(dae, flat, model_name)?;
    Ok(GalecCExport {
        context: c_template_context(&package, model_name)?,
    })
}

// ===========================================================================
// Switch-dispatch packaging plan (contract §9 WI-5)
// ===========================================================================

/// A per-invocation eFMU packaging plan for the switch-dispatch build step
/// (contract §9 WI-5): ONE projection, ONE minted packaging identity
/// (shared UUIDs/timestamp/tool so the cross-manifest `manifestRefId` links
/// agree), and the pre-rendered `.alg`/C texts the passthrough templates
/// interpolate.
///
/// The manifest **contexts** are built on demand by the declarative build
/// step (`rumoca::render_and_package`) in topological order: each producer's
/// real SHA-1 (of the exact rendered bytes) is threaded in under the
/// `target.toml`-declared `[[files.checksums]]` `as` key, and the plan slots
/// it into the typed model it assembles for that file — so no placeholder
/// checksum is ever representable (GAL-021), and the typed data-integrity
/// validators still run (on the assembled model, before the template
/// renders). The typed models exist only to (a) run those validators and
/// (b) feed the product-agnostic [`AcManifestCtx`]/[`PcManifestCtx`]/
/// [`ContentCtx`] views the templates consume; they are never serialized to
/// XML (the templates own all XML text — SPEC_0034 D3 amended).
pub struct GalecPackagingPlan {
    package: AlgorithmCodePackage,
    /// GALEC block source (`.alg`), pre-rendered; the `.alg` passthrough
    /// template interpolates it and the build step hashes its bytes.
    alg_text: String,
    /// Generated C99 header (empty for the AC-only plan).
    c_header: String,
    /// Generated C99 source (empty for the AC-only plan).
    c_source: String,
    /// Model identifier (dots→underscores); names the `.h`/`.c` files the PC
    /// manifest `Files` entries reference.
    model_identifier: String,
    /// `Content/@name`: the source model name as given on the command line
    /// (dotted for hierarchical models), per eFMI ch. 2.3.1.
    content_name: String,
    /// Shared strict-UTC timestamp (minted once).
    generated_at: UtcTimestamp,
    /// Shared generation-tool string (minted once).
    generation_tool: Option<NormalizedText>,
    /// AC manifest UUID (shared with `__content.xml`'s AC `manifestRefId`).
    ac_manifest_id: ManifestId,
    /// PC manifest UUID (present iff [`PackageKind::AlgorithmCodeAndProduction`]).
    pc_manifest_id: Option<ManifestId>,
    /// `__content.xml` UUID.
    content_id: ManifestId,
    /// The typed AC manifest, assembled while rendering the AC manifest and
    /// consumed while assembling the PC manifest (cross-validation +
    /// `ManifestReference/@manifestRefId`). Topological order guarantees it
    /// is populated before the PC context is built.
    ac_manifest: RefCell<Option<AlgorithmCodeManifest>>,
}

impl GalecPackagingPlan {
    /// The pre-rendered `.alg` block source (passthrough template input).
    #[must_use]
    pub fn alg_text(&self) -> &str {
        &self.alg_text
    }

    /// The pre-rendered C99 header (empty for the AC-only plan).
    #[must_use]
    pub fn c_header(&self) -> &str {
        &self.c_header
    }

    /// The pre-rendered C99 source (empty for the AC-only plan).
    #[must_use]
    pub fn c_source(&self) -> &str {
        &self.c_source
    }

    fn ac_identity(&self) -> ManifestIdentity {
        ManifestIdentity {
            id: self.ac_manifest_id,
            generated_at: self.generated_at,
            generation_tool: self.generation_tool.clone(),
        }
    }

    fn pc_identity(&self) -> Result<ManifestIdentity, GalecExportError> {
        let id = self
            .pc_manifest_id
            .ok_or_else(|| GalecExportError::Internal {
                detail:
                    "internal: Production Code manifest requested from an Algorithm-Code-only plan"
                        .to_owned(),
            })?;
        Ok(ManifestIdentity {
            id,
            generated_at: self.generated_at,
            generation_tool: self.generation_tool.clone(),
        })
    }

    /// Build the render context (the `ctx` root the manifest templates read)
    /// for `template`, slotting the build-step-injected checksums (keyed by
    /// their `target.toml` `as` name) into the typed model it assembles.
    /// Non-manifest templates (`.alg`/`.h`/`.c` passthroughs and `[[files]]`
    /// path templates) get a null `ctx` — they interpolate only the top-level
    /// `model_name`/`galec_*` keys the render site supplies.
    ///
    /// # Errors
    ///
    /// [`GalecExportError::Render`] when a required checksum is missing or
    /// malformed, or the assembled typed model fails a data-integrity
    /// validator (the fail-early guarantee, before any XML is emitted).
    pub fn template_ctx(
        &self,
        template: &str,
        checksums: &BTreeMap<String, String>,
    ) -> Result<serde_json::Value, GalecExportError> {
        match template {
            "__content.xml.jinja" => self.content_context(
                self.checksum(checksums, "ac_manifest_sha1")?,
                checksums.get("pc_manifest_sha1").map(String::as_str),
            ),
            "pc_manifest.xml.jinja" => self.pc_context(
                self.checksum(checksums, "ac_manifest_sha1")?,
                self.checksum(checksums, "c_header_sha1")?,
                self.checksum(checksums, "c_source_sha1")?,
            ),
            "ac_manifest.xml.jinja" | "manifest.xml.jinja" => {
                self.ac_context(self.checksum(checksums, "alg_sha1")?)
            }
            _ => Ok(serde_json::Value::Null),
        }
    }

    fn checksum<'a>(
        &self,
        checksums: &'a BTreeMap<String, String>,
        key: &str,
    ) -> Result<&'a str, GalecExportError> {
        checksums
            .get(key)
            .map(String::as_str)
            .ok_or_else(|| GalecExportError::Internal {
                detail: format!(
                    "internal: build step did not inject checksum '{key}' \
                     (missing [[files.checksums]] edge?)"
                ),
            })
    }

    /// Assemble the typed AC manifest with the rendered `.alg` SHA-1, cache
    /// it for the PC step, and serialize the `{ ac, units }` template context.
    fn ac_context(&self, alg_sha1: &str) -> Result<serde_json::Value, GalecExportError> {
        let checksum = parse_sha1(alg_sha1)?;
        let manifest =
            assemble_manifest_with_identity(&self.package, checksum, &self.ac_identity())?;
        let value = serde_json::json!({
            "ac": to_ctx_value(&AcManifestCtx::from_manifest(&manifest))?,
            "units": to_ctx_value(&AcManifestCtx::units(&manifest))?,
        });
        *self.ac_manifest.borrow_mut() = Some(manifest);
        Ok(value)
    }

    /// Assemble the typed PC manifest from the cached AC manifest and the
    /// rendered AC-manifest / C-file SHA-1s, and serialize the `{ pc }`
    /// template context.
    fn pc_context(
        &self,
        ac_manifest_sha1: &str,
        c_header_sha1: &str,
        c_source_sha1: &str,
    ) -> Result<serde_json::Value, GalecExportError> {
        let cached = self.ac_manifest.borrow();
        let ac_manifest = cached.as_ref().ok_or_else(|| GalecExportError::Internal {
            detail: "internal: Algorithm Code manifest must be rendered before the Production \
                     Code manifest (checksum-web topological order)"
                .to_owned(),
        })?;
        let header =
            emitted_code_file_from_sha1(format!("{}.h", self.model_identifier), c_header_sha1)?;
        let source =
            emitted_code_file_from_sha1(format!("{}.c", self.model_identifier), c_source_sha1)?;
        let manifest = assemble_production_manifest_with_identity(
            &self.package,
            ac_manifest,
            parse_sha1(ac_manifest_sha1)?,
            &header,
            &source,
            &self.pc_identity()?,
        )?;
        Ok(serde_json::json!({
            "pc": to_ctx_value(&PcManifestCtx::from_manifest(&manifest))?,
        }))
    }

    /// Build the validated `__content.xml` registry and serialize the
    /// `{ content }` template context (one representation per rendered
    /// manifest, each carrying the web-injected SHA-1 of its exact bytes).
    fn content_context(
        &self,
        ac_manifest_sha1: &str,
        pc_manifest_sha1: Option<&str>,
    ) -> Result<serde_json::Value, GalecExportError> {
        let mut representations = vec![ModelRepresentation {
            name: representation_name("AlgorithmCode")?,
            kind: ModelRepresentationKind::AlgorithmCode,
            manifest: representation_name("manifest.xml")?,
            checksum: parse_sha1(ac_manifest_sha1)?,
            manifest_ref_id: self.ac_manifest_id,
        }];
        if let Some(pc_sha1) = pc_manifest_sha1 {
            let pc_id = self
                .pc_manifest_id
                .ok_or_else(|| GalecExportError::Internal {
                    detail:
                        "internal: a Production Code representation checksum was injected into an \
                         Algorithm-Code-only plan"
                            .to_owned(),
                })?;
            representations.push(ModelRepresentation {
                name: representation_name("ProductionCode")?,
                kind: ModelRepresentationKind::ProductionCode,
                manifest: representation_name("manifest.xml")?,
                checksum: parse_sha1(pc_sha1)?,
                manifest_ref_id: pc_id,
            });
        }
        let content = Content::new(ContentParts {
            attributes: ManifestAttributes {
                id: self.content_id,
                name: NormalizedText::new(self.content_name.clone()).map_err(as_render_error)?,
                description: None,
                version: None,
                generation_date_and_time: self.generated_at,
                generation_tool: self.generation_tool.clone(),
                copyright: None,
                license: None,
            },
            active_fmu: None,
            model_representations: representations,
        })
        .map_err(as_render_error)?;
        Ok(serde_json::json!({
            "content": to_ctx_value(&ContentCtx::from_content(&content))?,
        }))
    }
}

/// Build the switch-dispatch packaging plan for the `galec` target (AC-only
/// eFMU): one projection, minted packaging identity, the pre-rendered `.alg`.
///
/// `model_identifier` is the file-system-safe model name (dots→underscores)
/// used for the projection and the `.alg` file name; `content_name` is the
/// source model name as given on the command line (dotted allowed), becoming
/// `Content/@name`.
///
/// # Errors
///
/// [`GalecExportError::Projection`] with all collected projection
/// diagnostics, or [`GalecExportError::Render`] for printer/identity
/// failures.
pub fn plan_galec_export(
    dae: &Dae,
    flat: &FlatModel,
    model_identifier: &str,
    content_name: &str,
) -> Result<GalecPackagingPlan, GalecExportError> {
    let package = lower_package(dae, flat, model_identifier)?;
    let alg_text = render_algorithm_code(&package)?;
    let identity = ManifestIdentity::generated()?;
    Ok(GalecPackagingPlan {
        package,
        alg_text,
        c_header: String::new(),
        c_source: String::new(),
        model_identifier: model_identifier.to_owned(),
        content_name: content_name.to_owned(),
        generated_at: identity.generated_at,
        generation_tool: identity.generation_tool,
        ac_manifest_id: ManifestId::generate(),
        pc_manifest_id: None,
        content_id: ManifestId::generate(),
        ac_manifest: RefCell::new(None),
    })
}

/// Build the switch-dispatch packaging plan for the `galec-production`
/// target (AC + PC eFMU): one projection, minted packaging identity, the
/// pre-rendered `.alg` and the generated C99 header/source (with the
/// Production Code conformance header, D10).
///
/// # Errors
///
/// [`GalecExportError::Projection`] with all collected projection
/// diagnostics, [`GalecExportError::Render`] for printer/identity failures,
/// or [`GalecExportError::CTemplate`] when the shared C-layout templates
/// cannot be resolved or rendered.
pub fn plan_galec_production_export(
    dae: &Dae,
    flat: &FlatModel,
    model_identifier: &str,
    content_name: &str,
) -> Result<GalecPackagingPlan, GalecExportError> {
    let package = lower_package(dae, flat, model_identifier)?;
    let alg_text = render_algorithm_code(&package)?;
    let c_context = c_template_context(&package, model_identifier)?;
    let bundle = embedded_c_layout_bundle()?;
    let c_header = render_c_layout_template(
        &bundle,
        HEADER_TEMPLATE,
        &c_context,
        PRODUCTION_CONFORMANCE_LINES,
        PRODUCTION_CONFORMANCE_SUMMARY,
    )?;
    let c_source = render_c_layout_template(
        &bundle,
        SOURCE_TEMPLATE,
        &c_context,
        PRODUCTION_CONFORMANCE_LINES,
        PRODUCTION_CONFORMANCE_SUMMARY,
    )?;
    let identity = ManifestIdentity::generated()?;
    Ok(GalecPackagingPlan {
        package,
        alg_text,
        c_header,
        c_source,
        model_identifier: model_identifier.to_owned(),
        content_name: content_name.to_owned(),
        generated_at: identity.generated_at,
        generation_tool: identity.generation_tool,
        ac_manifest_id: ManifestId::generate(),
        pc_manifest_id: Some(ManifestId::generate()),
        content_id: ManifestId::generate(),
        ac_manifest: RefCell::new(None),
    })
}

/// Parse a build-step-injected SHA-1 hex digest, mapping a malformed value to
/// a render error (it can only be malformed on an internal build-step bug —
/// `Sha1Hex::of_bytes` always emits a valid digest).
fn parse_sha1(hex: &str) -> Result<Sha1Hex, GalecExportError> {
    Sha1Hex::parse(hex).map_err(|error| GalecExportError::Internal {
        detail: format!("internal: build step injected a malformed SHA-1 '{hex}': {error}"),
    })
}

/// Wrap one rendered C file for the manifest builder from its name and the
/// build-step-injected SHA-1 of its exact bytes (GAL-021 — the digest is of
/// the real rendered bytes, threaded here by the checksum web).
fn emitted_code_file_from_sha1(
    name: String,
    sha1: &str,
) -> Result<EmittedCodeFile, GalecExportError> {
    Ok(EmittedCodeFile {
        name: NameWithoutSlashes::new(name).map_err(as_render_error)?,
        sha1: parse_sha1(sha1)?,
    })
}

fn representation_name(name: &str) -> Result<NameWithoutSlashes, GalecExportError> {
    NameWithoutSlashes::new(name).map_err(as_render_error)
}

/// Serialize a product-agnostic context view to a `serde_json::Value` for the
/// manifest templates; a serialization failure is an internal bug.
fn to_ctx_value<T: serde::Serialize>(value: &T) -> Result<serde_json::Value, GalecExportError> {
    serde_json::to_value(value).map_err(|error| GalecExportError::Internal {
        detail: format!("internal: manifest context serialization failed: {error}"),
    })
}

fn as_render_error(error: impl Into<GalecTargetError>) -> GalecExportError {
    GalecExportError::Render(error.into())
}

/// Project a compiled model to the validated Algorithm Code package — the
/// shared first step of every export facade: Flat-side scalar-type
/// provenance plus the untouched DAE through `lower_to_algorithm_code`.
fn lower_package(
    dae: &Dae,
    flat: &FlatModel,
    model_name: &str,
) -> Result<AlgorithmCodePackage, GalecExportError> {
    let scalar_types = build_scalar_type_map(flat);
    let input = GalecInput::new(dae, model_name).with_scalar_types(&scalar_types);
    lower_to_algorithm_code(&input, &GalecOptions::default()).map_err(GalecExportError::Projection)
}

/// The three GALEC codegen target names (all `ir = "dae"`). Shared so the
/// compile facade, the CLI, the LSP, and the WASM addon agree on one spelling.
pub const GALEC_TARGET: &str = "galec";
pub const GALEC_PRODUCTION_TARGET: &str = "galec-production";
/// Also the builtin bundle whose C-layout templates both C tracks share
/// (D10: the prelude is a fixed contract with the typed C printer; duplicating
/// it would be drift-prone).
pub const EMBEDDED_C_GALEC_TARGET: &str = "embedded-c-galec";
/// Shared C-layout template names (`[[files]]` of the builtin target).
const HEADER_TEMPLATE: &str = "model.h.jinja";
const SOURCE_TEMPLATE: &str = "model.c.jinja";

/// The `galec-production` conformance claim interpolated into the shared
/// C-layout templates' strict-undefined `conformance_header` key (contract
/// §5 / D10): inside the conformant eFMU the C files ARE the eFMI Production
/// Code representation, and the conformance surface is the manifest's
/// `LogicalData` mapping — not the C identifiers. Entries must stay
/// C-comment-safe (no newlines, no `*/` — pinned by unit test). Public so the
/// one authoritative spelling is shared with the CLI and the WASM addon.
pub const PRODUCTION_CONFORMANCE_LINES: &[&str] = &[
    "eFMI Production Code representation; the conformance surface is",
    "ProductionCode/manifest.xml (LogicalData), not these C names",
    "(SPEC_0034 GAL-024).",
];
/// One-line short claim spliced into the C source file's header comment.
pub const PRODUCTION_CONFORMANCE_SUMMARY: &str =
    "eFMI Production Code representation (SPEC_0034 GAL-024).";

/// The `embedded-c-galec` conformance claim: the non-eFMI track of GAL-024
/// must self-describe as NOT an eFMI Production Code container. The single
/// authoritative spelling (the CLI's `target_manifest.rs` and the WASM addon
/// reference these instead of keeping their own copies). Comment-safe (pinned
/// by the CLI honesty test).
pub const EMBEDDED_C_GALEC_CONFORMANCE_LINES: &[&str] = &[
    "NOT an eFMI Production Code container: no eFMU manifest mapping exists",
    "for this artifact. Generated by rumoca --target embedded-c-galec.",
];
pub const EMBEDDED_C_GALEC_CONFORMANCE_SUMMARY: &str = "NOT an eFMI Production Code container.";

/// Resolve the shared builtin C-layout template bundle. Absence is a
/// build-system invariant break (the templates are embedded at compile
/// time), reported loudly per SPEC_0008.
fn embedded_c_layout_bundle() -> Result<TargetBundle, GalecExportError> {
    TargetBundle::builtin(EMBEDDED_C_GALEC_TARGET).ok_or_else(|| GalecExportError::CTemplate {
        detail: format!("builtin target '{EMBEDDED_C_GALEC_TARGET}' is not embedded"),
    })
}

/// Render one shared C-layout template under a strict-undefined minijinja
/// environment: the serialized typed `CContext` plus the one key that is
/// target identity rather than projection data — this target's conformance
/// header (`galec-production` claims the PC representation; `embedded-c-galec`
/// self-describes as NOT a container).
fn render_c_layout_template(
    bundle: &TargetBundle,
    template: &str,
    c_context: &serde_json::Value,
    conformance_lines: &[&str],
    conformance_summary: &str,
) -> Result<String, GalecExportError> {
    let source = bundle
        .template_source(template)
        .map_err(|error| GalecExportError::CTemplate {
            detail: format!("{error:#}"),
        })?;
    let mut env = minijinja::Environment::new();
    env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
    register_manifest_filters(&mut env);
    env.render_str(
        &source,
        minijinja::context! {
            conformance_header => minijinja::context! {
                lines => conformance_lines,
                summary => conformance_summary,
            },
            ..minijinja::Value::from_serialize(c_context)
        },
    )
    .map_err(|error| GalecExportError::CTemplate {
        detail: format!("render '{template}': {error}"),
    })
}

/// Whether `target` is one of the GALEC codegen targets, so a codegen caller
/// can route it to [`render_galec_sources`] instead of the generic DAE
/// template render (which lacks the GALEC projection context).
#[must_use]
pub fn is_galec_target(target: &str) -> bool {
    matches!(
        target,
        GALEC_TARGET | GALEC_PRODUCTION_TARGET | EMBEDDED_C_GALEC_TARGET
    )
}

/// The rendered, inspectable GALEC sources for one target: the `.alg`
/// Algorithm Code text and — for the C tracks — the generated C header and
/// source. The `c_*` fields are empty for the Algorithm-Code-only `galec`
/// target. This is the identity-free artifact text (no eFMU container, no
/// UUID/timestamp) that the GUI codegen paths (LSP + WASM addon) inspect.
#[derive(Debug, Clone)]
pub struct GalecSources {
    pub alg: String,
    pub c_header: String,
    pub c_source: String,
}

/// The C-file conformance header for a GALEC target, or `None` for a target
/// that emits no C (`galec`).
fn galec_c_conformance(target: &str) -> Option<(&'static [&'static str], &'static str)> {
    match target {
        GALEC_PRODUCTION_TARGET => {
            Some((PRODUCTION_CONFORMANCE_LINES, PRODUCTION_CONFORMANCE_SUMMARY))
        }
        EMBEDDED_C_GALEC_TARGET => Some((
            EMBEDDED_C_GALEC_CONFORMANCE_LINES,
            EMBEDDED_C_GALEC_CONFORMANCE_SUMMARY,
        )),
        _ => None,
    }
}

/// Render the inspectable GALEC sources (`.alg` plus, for the C tracks, the
/// embedded C header/source) for a compiled model, identity-free: no eFMU
/// container, no UUID or timestamp, so it is safe on `wasm32` and cheap for
/// interactive codegen. The single shared renderer behind the WASM addon and
/// the LSP's GALEC codegen; the eFMU *container* stays with the CLI.
///
/// # Errors
///
/// [`GalecExportError::UnknownTarget`] for a non-GALEC target,
/// [`GalecExportError::Projection`] with all projection diagnostics for an
/// inadmissible model, or [`GalecExportError::Render`]/[`GalecExportError::CTemplate`]
/// on printer/template failure.
pub fn render_galec_sources(
    dae: &Dae,
    flat: &FlatModel,
    model_name: &str,
    target: &str,
) -> Result<GalecSources, GalecExportError> {
    if !is_galec_target(target) {
        return Err(GalecExportError::UnknownTarget(target.to_string()));
    }
    let package = lower_package(dae, flat, model_name)?;
    let alg = render_algorithm_code(&package)?;
    let (c_header, c_source) = match galec_c_conformance(target) {
        None => (String::new(), String::new()),
        Some((lines, summary)) => {
            let c_context = c_template_context(&package, model_name)?;
            let bundle = embedded_c_layout_bundle()?;
            let header =
                render_c_layout_template(&bundle, HEADER_TEMPLATE, &c_context, lines, summary)?;
            let source =
                render_c_layout_template(&bundle, SOURCE_TEMPLATE, &c_context, lines, summary)?;
            (header, source)
        }
    };
    Ok(GalecSources {
        alg,
        c_header,
        c_source,
    })
}

/// Register the eFMI manifest render filters (contract §3b) on a bare
/// minijinja environment: `xml_escape` (autoescape is OFF, so every text
/// value is escaped explicitly) and `xs_double` (raw `f64` → valid `xs:double`
/// lexical). Reuses the filter functions re-exported by `rumoca-galec-codegen` so the
/// guarantee is defined once and shared by every manifest render env.
pub(crate) fn register_manifest_filters(env: &mut minijinja::Environment<'_>) {
    env.add_filter("xml_escape", |text: String| {
        rumoca_galec_codegen::xml_escape(&text)
    });
    env.add_filter("xs_double", rumoca_galec_codegen::xs_double);
}

/// Build the [`ScalarTypeMap`] for a compiled model from Flat-side declared
/// types (module docs). Generated condition/`__pre__` variables stay absent
/// on purpose — the projection's `classify` fallbacks own them — and any
/// other missing mapping is reported loudly by the projection's `ET011`.
#[must_use]
pub fn build_scalar_type_map(flat: &FlatModel) -> ScalarTypeMap {
    let builtins = BuiltinScalarTypeIds::resolve();
    let mut map = ScalarTypeMap::new();
    for variable in flat.variables.values() {
        if let Some(scalar_type) = builtins.scalar_type(variable.type_id) {
            map.insert(variable.name.clone(), scalar_type);
        }
    }
    map
}

/// The builtin scalar `TypeId`s the Flat IR references.
///
/// `TypeTable::new()` registers the MLS predefined types first, in a fixed
/// order, so the builtin ids are identical across every table the pipeline
/// builds; instantiate's `resolve_primitive_type_id` collapses primitive
/// component types (including type aliases like `SI.Voltage`) down to
/// exactly these ids. The unit tests below pin this invariant against the
/// real pipeline.
struct BuiltinScalarTypeIds {
    real: TypeId,
    integer: TypeId,
    boolean: TypeId,
}

impl BuiltinScalarTypeIds {
    fn resolve() -> Self {
        let table = TypeTable::new();
        Self {
            real: table.real(),
            integer: table.integer(),
            boolean: table.boolean(),
        }
    }

    /// GALEC scalar type for a Flat `type_id`, when it is one of the three
    /// GALEC-representable builtins. `String`/`Clock`/enumerations/records/
    /// `UNKNOWN` return `None` — the caller leaves them absent.
    fn scalar_type(&self, type_id: TypeId) -> Option<ScalarType> {
        if type_id == self.real {
            Some(ScalarType::Real)
        } else if type_id == self.integer {
            Some(ScalarType::Integer)
        } else if type_id == self.boolean {
            Some(ScalarType::Boolean)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compile::{CompilationResult, Session, SessionConfig};
    use rumoca_core::{VarName, pre_slot_name};
    use rumoca_ir_dae::component_base_name;

    /// Fixed-sample discrete fixture exercising every mapping rule:
    /// Real/Integer/Boolean parameters and constants, a `pre()` slot on an
    /// Integer discrete state, and a generated when-condition vector.
    const DISCRETE_SOURCE: &str = r#"
model GalecFacadeDemo
  constant Real samplePeriod = 0.001;
  parameter Real gain = 2.0;
  parameter Integer countMax = 10;
  parameter Boolean enabled = true;
  discrete output Real y(start = 0.0);
  discrete Integer count(start = 0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    y = gain * count;
  end when;
end GalecFacadeDemo;
"#;

    fn compile(source: &str, model: &str) -> CompilationResult {
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document("test.mo", source)
            .expect("fixture should parse");
        session
            .compile_model(model)
            .expect("fixture should compile")
    }

    fn get(map: &ScalarTypeMap, name: &str) -> Option<ScalarType> {
        map.get(&VarName::new(name)).copied()
    }

    #[test]
    fn scalar_type_map_types_parameters_and_constants_from_flat() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let map = build_scalar_type_map(&result.flat);

        assert_eq!(get(&map, "samplePeriod"), Some(ScalarType::Real));
        assert_eq!(get(&map, "gain"), Some(ScalarType::Real));
        assert_eq!(get(&map, "countMax"), Some(ScalarType::Integer));
        assert_eq!(get(&map, "enabled"), Some(ScalarType::Boolean));
        assert_eq!(get(&map, "y"), Some(ScalarType::Real));
        assert_eq!(get(&map, "count"), Some(ScalarType::Integer));
    }

    /// Generated condition/`__pre__` variables are typed by the
    /// projection's `classify` fallbacks (condition targets Boolean per
    /// MLS B.1d, pre-slots inheriting their base type), NOT by facade map
    /// entries — a facade copy of those rules would shadow the projection's
    /// own logic on drift (map hits take precedence). This pins both
    /// halves: the map stays silent about generated names, and the
    /// projection still types them (the export succeeds end-to-end).
    #[test]
    fn generated_condition_and_pre_slot_types_are_owned_by_the_projection() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let map = build_scalar_type_map(&result.flat);

        let pre_count = pre_slot_name("count");
        assert_eq!(
            get(&map, pre_count.as_str()),
            None,
            "facade must not duplicate the projection's pre-slot rule"
        );
        let condition_bases: Vec<String> = result
            .dae
            .conditions
            .equations
            .iter()
            .filter_map(|equation| equation.lhs.as_ref())
            .filter_map(|lhs| component_base_name(lhs.as_str()))
            .collect();
        assert!(
            !condition_bases.is_empty(),
            "when-equation fixture must generate condition targets"
        );
        for base in &condition_bases {
            assert_eq!(
                get(&map, base),
                None,
                "facade must not duplicate the projection's condition rule for '{base}'"
            );
        }

        plan_galec_export(
            &result.dae,
            &result.flat,
            "GalecFacadeDemo",
            "GalecFacadeDemo",
        )
        .expect(
            "projection's structural fallbacks must type generated condition/pre-slot variables",
        );
    }

    #[test]
    fn scalar_type_map_leaves_unmappable_names_absent() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let map = build_scalar_type_map(&result.flat);
        assert_eq!(get(&map, "no.such.variable"), None);
    }

    #[test]
    fn plan_galec_export_pre_renders_the_alg_block() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let plan = plan_galec_export(
            &result.dae,
            &result.flat,
            "GalecFacadeDemo",
            "GalecFacadeDemo",
        )
        .expect("discrete fixture should project to GALEC");

        assert!(
            plan.alg_text().contains("GalecFacadeDemo"),
            "alg text should name the block:\n{}",
            plan.alg_text()
        );
        assert!(
            plan.alg_text().contains("DoStep"),
            "alg text should contain the DoStep method:\n{}",
            plan.alg_text()
        );
        // The AC manifest context assembles (validators pass) once the build
        // step injects the `.alg` SHA-1 under its declared `as` key; the XML
        // text itself is the template's job (SPEC_0034 D3 amended), exercised
        // black-box by the CLI tests.
        let mut checksums = BTreeMap::new();
        checksums.insert(
            "alg_sha1".to_string(),
            Sha1Hex::of_bytes(b"alg").as_str().to_string(),
        );
        plan.template_ctx("ac_manifest.xml.jinja", &checksums)
            .expect("AC manifest context assembles from the injected checksum");
    }

    #[test]
    fn render_galec_c_export_produces_the_template_context() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let export = render_galec_c_export(&result.dae, &result.flat, "GalecFacadeDemo")
            .expect("discrete fixture should project to the C context");
        let context = &export.context;
        assert_eq!(context["model_name"], "GalecFacadeDemo");
        assert_eq!(context["struct_name"], "GalecFacadeDemoState");
        assert!(
            context["methods"]["do_step"]
                .as_array()
                .is_some_and(|statements| !statements.is_empty()),
            "DoStep must carry C statements: {context}"
        );
    }

    #[test]
    fn plan_galec_production_export_pre_renders_alg_and_c() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let plan = plan_galec_production_export(
            &result.dae,
            &result.flat,
            "GalecFacadeDemo",
            "GalecFacadeDemo",
        )
        .expect("discrete fixture should plan the production export");

        for (name, text) in [
            ("alg_text", plan.alg_text()),
            ("c_header", plan.c_header()),
            ("c_source", plan.c_source()),
        ] {
            assert!(!text.is_empty(), "{name} must be non-empty");
        }
    }

    /// D10 honesty swap: inside the conformant container the pre-rendered C
    /// files claim the Production Code representation and point at the
    /// manifest as the conformance surface — the shared templates'
    /// `embedded-c-galec` NOT-a-PC text must not leak in.
    #[test]
    fn production_c_files_carry_the_production_code_conformance_header() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let plan = plan_galec_production_export(
            &result.dae,
            &result.flat,
            "GalecFacadeDemo",
            "GalecFacadeDemo",
        )
        .expect("discrete fixture should plan the production export");

        for line in PRODUCTION_CONFORMANCE_LINES {
            assert!(
                plan.c_header().contains(line),
                "C header must carry the PC conformance line '{line}':\n{}",
                plan.c_header()
            );
        }
        assert!(
            plan.c_source().contains(PRODUCTION_CONFORMANCE_SUMMARY),
            "C source must carry the PC conformance summary:\n{}",
            plan.c_source()
        );
        for text in [plan.c_header(), plan.c_source()] {
            assert!(
                !text.contains("NOT an eFMI Production Code container"),
                "embedded-c-galec's NOT-a-PC claim must not leak into the \
                 production export:\n{text}"
            );
        }
    }

    /// The conformance-header entries become C block-comment lines; a
    /// newline or `*/` would break the generated files (mirrors the
    /// `embedded-c-galec` guard in `rumoca`'s `target_manifest.rs`).
    #[test]
    fn production_conformance_header_stays_c_comment_safe() {
        for text in PRODUCTION_CONFORMANCE_LINES
            .iter()
            .copied()
            .chain([PRODUCTION_CONFORMANCE_SUMMARY])
        {
            assert!(
                !text.contains('\n') && !text.contains("*/"),
                "conformance header entry is not C-comment-safe: {text:?}"
            );
        }
    }

    #[test]
    fn plan_galec_export_rejects_continuous_models_loudly() {
        let result = compile(
            r#"
model ContinuousDemo
  Real x(start = 1.0);
  parameter Real k = 2.0;
equation
  der(x) = -k * x;
end ContinuousDemo;
"#,
            "ContinuousDemo",
        );
        // `GalecPackagingPlan` is intentionally not `Debug` (it holds the
        // interior-mutable typed manifest cache), so match rather than
        // `expect_err`.
        let error = match plan_galec_export(
            &result.dae,
            &result.flat,
            "ContinuousDemo",
            "ContinuousDemo",
        ) {
            Ok(_) => panic!("continuous dynamics must be rejected"),
            Err(error) => error,
        };
        let GalecExportError::Projection(diagnostics) = &error else {
            panic!("expected projection rejection, got: {error}");
        };
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| diagnostic.to_string().contains("[ET001]")),
            "expected ET001 continuous-dynamics rejection: {diagnostics:#?}"
        );
    }
}
