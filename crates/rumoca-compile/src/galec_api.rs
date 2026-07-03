//! GALEC / eFMI Algorithm Code export facade (SPEC_0034 GAL-010).
//!
//! Frontends reach the `rumoca-target-galec` projection only through this
//! module: it supplies the auxiliary provenance the canonical DAE does not
//! carry — the [`ScalarTypeMap`] built from Flat-side declared types — and
//! renders the projection's two text artifacts (`<Model>.alg` plus the
//! Algorithm Code `manifest.xml`).
//!
//! This is the "GALEC-derived text export" rung of the SPEC_0034
//! conformance ladder (GAL-021): rendered files with honest
//! self-description, not yet a packaged eFMU container.
//!
//! [`render_galec_production_export`] climbs to the "eFMI Production Code
//! export" rung (GAL-024 conformant track): the SAME single projection
//! rendered as the full two-representation container payload — the
//! Algorithm Code texts above plus generated C99 and the Production Code
//! manifest whose `LogicalData` maps every block variable and method to
//! the C entities, with every SHA-1 in the checksum web computed from the
//! exact rendered bytes.
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
//! here: `rumoca-target-galec`'s `classify` module owns those structural
//! fallbacks, and map entries take precedence over them, so duplicating
//! the rules in the facade would let a stale copy silently win on drift.
//!
//! Anything else — `String`/`Clock`/enumeration/record types, unresolved
//! `TypeId::UNKNOWN` — stays absent from the map. Absence is loud, never a
//! default: the projection rejects untypeable variables with `ET011`.

use crate::codegen_target::{TargetBundle, TargetTemplateSource};
use rumoca_core::TypeId;
use rumoca_efmi::{NameWithoutSlashes, Sha1Hex, production_code_manifest_to_xml};
use rumoca_galec::ast::ScalarType;
use rumoca_ir_ast::TypeTable;
use rumoca_ir_dae::Dae;
use rumoca_ir_flat::Model as FlatModel;
use rumoca_target_galec::{
    AlgorithmCodePackage, EmittedCodeFile, GalecInput, GalecOptions, GalecTargetError,
    ScalarTypeMap, assemble_production_manifest, c_template_context, lower_to_algorithm_code,
    render_algorithm_code, render_manifest_document, render_manifest_xml,
};

/// Rendered GALEC-derived text export: the `.alg` block printed by the
/// typed GALEC printer and the Algorithm Code manifest emitted by the
/// typed XML serializer (GAL-009 / D3 — never template-authored text).
#[derive(Debug, Clone)]
pub struct GalecExport {
    /// GALEC block source (`<Model>.alg`).
    pub alg_text: String,
    /// eFMI Algorithm Code `manifest.xml` document.
    pub manifest_xml: String,
}

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
}

fn render_diagnostics(diagnostics: &[GalecTargetError]) -> String {
    diagnostics
        .iter()
        .map(|diagnostic| format!("  - {diagnostic}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Render the GALEC text export for a compiled model: build Flat-side
/// scalar-type provenance, project the untouched DAE, and print/serialize
/// both artifacts.
///
/// # Errors
///
/// [`GalecExportError::Projection`] with all collected projection
/// diagnostics, or [`GalecExportError::Render`] for validator/printer/
/// serializer failures on the validated package.
pub fn render_galec_export(
    dae: &Dae,
    flat: &FlatModel,
    model_name: &str,
) -> Result<GalecExport, GalecExportError> {
    let package = lower_package(dae, flat, model_name)?;
    Ok(GalecExport {
        alg_text: render_algorithm_code(&package)?,
        manifest_xml: render_manifest_xml(&package)?,
    })
}

/// GALEC-derived embedded C export (the `embedded-c-galec` target,
/// SPEC_0034 GAL-024): the serialized typed template context the target's
/// minijinja templates consume. Explicitly NOT an eFMI Production Code
/// container — no manifest mapping is emitted; the honest self-description
/// lives in the target manifest.
#[derive(Debug, Clone)]
pub struct GalecCExport {
    /// Serialized typed `CContext` (`rumoca-target-galec`'s `emit` module):
    /// `model_name`, `block_name`, `struct_name`, `function_prefix`,
    /// `include_guard`, `variables`, `methods` — the complete template
    /// context, C text intelligence stays in the typed printer (D2/GAL-008).
    pub context: serde_json::Value,
}

/// Render the embedded C export context for a compiled model: the same
/// projection as [`render_galec_export`], serialized through
/// `c_template_context` instead of the `.alg`/manifest printers.
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

/// Rendered eFMI Production Code export (the `galec-production` target,
/// SPEC_0034 GAL-024 conformant track): the complete text payload of a
/// two-representation eFMU container. Co-emission of the Algorithm Code
/// representation is mandatory per eFMI §2.2 — a PC-only container is
/// non-conformant — so the AC texts are the same artifacts
/// [`render_galec_export`] produces, from the same single projection.
#[derive(Debug, Clone)]
pub struct GalecProductionExport {
    /// `AlgorithmCode/<Model>.alg` — GALEC block source.
    pub alg_text: String,
    /// `AlgorithmCode/manifest.xml` — Algorithm Code manifest document.
    pub ac_manifest_xml: String,
    /// `ProductionCode/<Model>.h` — generated C99 block-state header.
    pub c_header: String,
    /// `ProductionCode/<Model>.c` — generated C99 method definitions.
    pub c_source: String,
    /// `ProductionCode/manifest.xml` — Production Code manifest whose
    /// `ManifestReference` checksums the exact `ac_manifest_xml` bytes and
    /// whose `LogicalData` maps every block variable and method.
    pub pc_manifest_xml: String,
}

/// Render the eFMI Production Code export for a compiled model: ONE
/// projection (single-renderer invariant), every checksum computed over
/// final bytes, the PC manifest assembled and serialized exactly once.
///
/// Assembly order (contract §3.3): project once; print the `.alg` text;
/// render the AC manifest as a typed-document/XML pair; render the C
/// header/source through the shared `embedded-c-galec` layout templates
/// with this target's Production Code conformance header (D10); SHA-1 the
/// AC manifest and C bytes; assemble the PC manifest; serialize it.
///
/// The C files are rendered here — not later by the CLI template pass —
/// because their SHA-1s go into the PC manifest before it is serialized;
/// the container writer cross-checks the identical passthrough-rendered
/// bytes at write time (EFM036).
///
/// # Errors
///
/// [`GalecExportError::Projection`] with all collected projection
/// diagnostics, [`GalecExportError::Render`] for validator/printer/
/// serializer/manifest failures on the validated package, or
/// [`GalecExportError::CTemplate`] when the shared C-layout templates
/// cannot be resolved or rendered.
pub fn render_galec_production_export(
    dae: &Dae,
    flat: &FlatModel,
    model_name: &str,
) -> Result<GalecProductionExport, GalecExportError> {
    // (1) ONE projection: every artifact below derives from this package.
    let package = lower_package(dae, flat, model_name)?;

    // (2) GALEC block source.
    let alg_text = render_algorithm_code(&package)?;

    // (3) Typed AC manifest + XML from ONE assemble+serialize pass: the PC
    // `ManifestReference` checksums these exact bytes and reads the AC
    // UUID from the typed half (never re-parsed, never re-serialized).
    let (ac_manifest, ac_manifest_xml) = render_manifest_document(&package)?;

    // (4) C header/source through the shared C-layout templates with the
    // Production Code conformance header.
    let c_context = c_template_context(&package, model_name)?;
    let bundle = embedded_c_layout_bundle()?;
    let c_header = render_c_layout_template(&bundle, HEADER_TEMPLATE, &c_context)?;
    let c_source = render_c_layout_template(&bundle, SOURCE_TEMPLATE, &c_context)?;

    // (5) SHA-1 over the final bytes, assemble the PC manifest (which
    // cross-validates against the typed AC manifest), serialize ONCE.
    let header_file = emitted_code_file(format!("{model_name}.h"), &c_header)?;
    let source_file = emitted_code_file(format!("{model_name}.c"), &c_source)?;
    let pc_manifest = assemble_production_manifest(
        &package,
        &ac_manifest,
        Sha1Hex::of_bytes(ac_manifest_xml.as_bytes()),
        &header_file,
        &source_file,
    )?;
    let pc_bytes = production_code_manifest_to_xml(&pc_manifest).map_err(GalecTargetError::from)?;
    let pc_manifest_xml =
        String::from_utf8(pc_bytes).map_err(|error| GalecTargetError::LoweringInternal {
            detail: format!("Production Code manifest XML is not valid UTF-8: {error}"),
        })?;

    Ok(GalecProductionExport {
        alg_text,
        ac_manifest_xml,
        c_header,
        c_source,
        pc_manifest_xml,
    })
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

/// The builtin target whose C-layout templates the production export
/// shares (D10: the prelude is a fixed contract with the typed C printer;
/// duplicating it would be drift-prone).
const EMBEDDED_C_GALEC_TARGET: &str = "embedded-c-galec";
/// Shared C-layout template names (`[[files]]` of the builtin target).
const HEADER_TEMPLATE: &str = "model.h.jinja";
const SOURCE_TEMPLATE: &str = "model.c.jinja";

/// The `galec-production` conformance claim interpolated into the shared
/// C-layout templates' strict-undefined `conformance_header` key (contract
/// §5 / D10): inside the conformant eFMU the C files ARE the eFMI
/// Production Code representation, and the conformance surface is the
/// manifest's `LogicalData` mapping — not the C identifiers. Entries must
/// stay C-comment-safe (no newlines, no `*/` — pinned by unit test); the
/// `embedded-c-galec` render path supplies its own NOT-a-PC text the same
/// way (`rumoca`'s `target_manifest.rs`).
const PRODUCTION_CONFORMANCE_LINES: &[&str] = &[
    "eFMI Production Code representation; the conformance surface is",
    "ProductionCode/manifest.xml (LogicalData), not these C names",
    "(SPEC_0034 GAL-024).",
];
/// One-line short claim spliced into the C source file's header comment.
const PRODUCTION_CONFORMANCE_SUMMARY: &str =
    "eFMI Production Code representation (SPEC_0034 GAL-024).";

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
/// target identity rather than projection data — this target's Production
/// Code conformance header.
fn render_c_layout_template(
    bundle: &TargetBundle,
    template: &str,
    c_context: &serde_json::Value,
) -> Result<String, GalecExportError> {
    let source = bundle
        .template_source(template)
        .map_err(|error| GalecExportError::CTemplate {
            detail: format!("{error:#}"),
        })?;
    let mut env = minijinja::Environment::new();
    env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
    env.render_str(
        &source,
        minijinja::context! {
            conformance_header => minijinja::context! {
                lines => PRODUCTION_CONFORMANCE_LINES,
                summary => PRODUCTION_CONFORMANCE_SUMMARY,
            },
            ..minijinja::Value::from_serialize(c_context)
        },
    )
    .map_err(|error| GalecExportError::CTemplate {
        detail: format!("render '{template}': {error}"),
    })
}

/// Wrap one rendered C file for the manifest builder: name plus SHA-1 of
/// the exact final bytes (GAL-021 — hashed here, after rendering, never
/// re-rendered downstream).
fn emitted_code_file(name: String, rendered: &str) -> Result<EmittedCodeFile, GalecExportError> {
    let name = NameWithoutSlashes::new(name).map_err(GalecTargetError::from)?;
    Ok(EmittedCodeFile {
        name,
        sha1: Sha1Hex::of_bytes(rendered.as_bytes()),
    })
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

        render_galec_export(&result.dae, &result.flat, "GalecFacadeDemo").expect(
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
    fn render_galec_export_produces_alg_and_manifest() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let export = render_galec_export(&result.dae, &result.flat, "GalecFacadeDemo")
            .expect("discrete fixture should project to GALEC");

        assert!(
            export.alg_text.contains("GalecFacadeDemo"),
            "alg text should name the block:\n{}",
            export.alg_text
        );
        assert!(
            export.alg_text.contains("DoStep"),
            "alg text should contain the DoStep method:\n{}",
            export.alg_text
        );
        assert!(
            export.manifest_xml.starts_with("<?xml"),
            "manifest should be an XML document:\n{}",
            export.manifest_xml
        );
        assert!(
            export.manifest_xml.contains("Manifest"),
            "manifest root element expected:\n{}",
            export.manifest_xml
        );
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
    fn render_galec_production_export_emits_the_five_container_texts() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let export = render_galec_production_export(&result.dae, &result.flat, "GalecFacadeDemo")
            .expect("discrete fixture should render the production export");

        for (name, text) in [
            ("alg_text", &export.alg_text),
            ("ac_manifest_xml", &export.ac_manifest_xml),
            ("c_header", &export.c_header),
            ("c_source", &export.c_source),
            ("pc_manifest_xml", &export.pc_manifest_xml),
        ] {
            assert!(!text.is_empty(), "{name} must be non-empty");
        }
        assert!(
            export.pc_manifest_xml.starts_with("<?xml"),
            "PC manifest should be an XML document:\n{}",
            export.pc_manifest_xml
        );
    }

    /// The checksum web (contract §4): the PC `ManifestReference` pins the
    /// EXACT rendered AC manifest bytes — the facade must hash the same
    /// string it returns, with no re-serialization in between.
    #[test]
    fn production_manifest_reference_checksums_the_exact_ac_manifest_bytes() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let export = render_galec_production_export(&result.dae, &result.flat, "GalecFacadeDemo")
            .expect("discrete fixture should render the production export");

        let expected = rumoca_efmi::Sha1Hex::of_bytes(export.ac_manifest_xml.as_bytes());
        let reference = export
            .pc_manifest_xml
            .lines()
            .find(|line| line.contains("<ManifestReference "))
            .expect("PC manifest must carry the AC ManifestReference");
        assert!(
            reference.contains(&format!(r#"checksum="{}""#, expected.as_str())),
            "PC ManifestReference must checksum the exact AC manifest bytes \
             (expected {expected}):\n{reference}"
        );
    }

    /// D10 honesty swap: inside the conformant container the C files claim
    /// the Production Code representation and point at the manifest as the
    /// conformance surface — the shared templates' `embedded-c-galec`
    /// NOT-a-PC text must not leak in.
    #[test]
    fn production_c_files_carry_the_production_code_conformance_header() {
        let result = compile(DISCRETE_SOURCE, "GalecFacadeDemo");
        let export = render_galec_production_export(&result.dae, &result.flat, "GalecFacadeDemo")
            .expect("discrete fixture should render the production export");

        for line in PRODUCTION_CONFORMANCE_LINES {
            assert!(
                export.c_header.contains(line),
                "C header must carry the PC conformance line '{line}':\n{}",
                export.c_header
            );
        }
        assert!(
            export.c_source.contains(PRODUCTION_CONFORMANCE_SUMMARY),
            "C source must carry the PC conformance summary:\n{}",
            export.c_source
        );
        for text in [&export.c_header, &export.c_source] {
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
    fn render_galec_export_rejects_continuous_models_loudly() {
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
        let error = render_galec_export(&result.dae, &result.flat, "ContinuousDemo")
            .expect_err("continuous dynamics must be rejected");
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
