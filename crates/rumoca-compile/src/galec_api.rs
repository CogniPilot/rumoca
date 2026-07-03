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

use rumoca_core::TypeId;
use rumoca_galec::ast::ScalarType;
use rumoca_ir_ast::TypeTable;
use rumoca_ir_dae::Dae;
use rumoca_ir_flat::Model as FlatModel;
use rumoca_target_galec::{
    GalecInput, GalecOptions, GalecTargetError, ScalarTypeMap, lower_to_algorithm_code,
    render_algorithm_code, render_manifest_xml,
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
    let scalar_types = build_scalar_type_map(flat);
    let input = GalecInput::new(dae, model_name).with_scalar_types(&scalar_types);
    let package = lower_to_algorithm_code(&input, &GalecOptions::default())
        .map_err(GalecExportError::Projection)?;
    Ok(GalecExport {
        alg_text: render_algorithm_code(&package)?,
        manifest_xml: render_manifest_xml(&package)?,
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
    use rumoca_core::VarName;
    use rumoca_ir_dae::component_base_name;
    use rumoca_target_galec::classify::PRE_SLOT_PREFIX;

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

        let pre_count = format!("{PRE_SLOT_PREFIX}count");
        assert_eq!(
            get(&map, &pre_count),
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
