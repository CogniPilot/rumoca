//! Projection from the canonical Rumoca DAE IR to an eFMI Algorithm Code
//! package: admissibility checks over the untouched DAE, lowering to the
//! GALEC AST plus a typed Algorithm Code manifest model, and package
//! validation. See `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`.
//!
//! This crate is the projection + eFMI-packaging layer of the SPEC_0034
//! module split (GAL-010, D3 amended): `rumoca-ir-galec` owns the language
//! (AST/printer/validator), this crate maps canonical compiler artifacts
//! onto the GALEC AST **and** owns the eFMI packaging data model +
//! data-integrity validators + product-agnostic context views
//! ([`crate::manifest_context`], the dissolved standalone eFMI packaging crate). The
//! generic checksum/container build step lives beside the CLI in the
//! `rumoca` crate; all XML text lives in minijinja templates. It is reached
//! through `rumoca-compile`; GALEC is a target language, never a canonical
//! IR stage (GAL-001).
//!
//! Module map:
//!
//! - [`input`] — [`GalecInput`] (borrowed untouched DAE + model name +
//!   optional Flat-side scalar-type provenance) and [`GalecOptions`]
//!   (profile pin, block-name override);
//! - [`admissibility`] — GAL-004 pre-projection checks over the untouched
//!   canonical DAE: no continuous dynamics, no external functions, no
//!   runtime events (clocked relations stay admissible), exactly one
//!   fixed-period clock, no partial/unsupported class types, literal
//!   positive array dimensions;
//! - [`classify`] — the GAL-020 variable classification (causality is
//!   authoritative; `__pre__.` slots become `'previous(x)'` protected
//!   state; generated condition machinery is projection-internal) and the
//!   structural scalar-type resolution (S8: never from start values);
//! - [`mangle`] — GAL-015 name mapping: injective, reserved-disjoint,
//!   quoted identifiers carrying original scalarized Modelica names;
//! - [`manifest_vars`] — manifest `Variables` population (ids, unique
//!   names, `blockCausality`, literal dimensions, evaluated start/min/max/
//!   nominal via the [`manifest_vars::const_eval`] evaluator);
//! - [`lower`] — the DAE → [`AlgorithmCodePackage`] lowering
//!   ([`lower_to_algorithm_code`]): sample-tick when-edge guard unwrapping,
//!   typed expression lowering with T5 explicit casts and the T8 builtin
//!   mapping table, dependency ordering of the sequential DoStep
//!   assignments (MLS B.1b rows are simultaneous; discrete algebraic loops
//!   are rejected), `'previous(x)'` state materialization + end-of-DoStep
//!   commits (T2), Startup/Recalibrate construction (GAL-017), manifest
//!   Clock wiring (GAL-016/D6), and GAL-004 post-validation;
//! - [`emit`] — structured GALEC/C template contexts,
//!   [`assemble_manifest_with_identity`] (typed Algorithm Code manifest, no
//!   XML — the templates own that), and [`c_template_context`]
//!   (typed-then-serialized minijinja context of the `galec-c`
//!   target, GAL-024);
//! - [`crate::manifest_context`] — the eFMI packaging data model (manifest /
//!   `__content.xml` models, checksums, id discipline), its data-integrity
//!   validators, and the `#[derive(Serialize)]` context views the packaging
//!   templates consume (the dissolved eFMI packaging crate; D3 amended);
//! - [`c_mangle`] / [`c_lower`] — the embedded-C side of the projection:
//!   collision-checked GALEC-name → C-identifier mangling and GALEC AST →
//!   structured C codegen IR feeding [`c_template_context`];
//! - [`production_manifest`] — [`assemble_production_manifest`]: the typed
//!   eFMI Production Code manifest describing the generated C files
//!   (`TargetTypes`/`CodeFiles`/`LogicalData` mapping every Algorithm Code
//!   variable and block method), assembled from typed inputs only and
//!   cross-validated against the Algorithm Code manifest before returning;
//! - [`package`] — the [`AlgorithmCodePackage`] output contract
//!   (constructed only by lowering, post-validated per GAL-004);
//! - [`diagnostic`] — SPEC_0008-shaped [`GalecTargetError`] with stable
//!   `ET0xx` codes and DAE source spans where available; projection-scope
//!   rejections use the GAL-025 wording.

pub mod admissibility;
pub mod c_lower;
pub mod c_mangle;
pub mod classify;
pub mod diagnostic;
pub mod emit;
pub mod input;
pub mod lower;
pub mod mangle;
pub mod manifest_context;
pub mod manifest_vars;
pub mod package;
pub mod production_manifest;

pub use admissibility::{AdmittedClock, check_admissibility};
pub use c_lower::CContextLowerer;
pub use c_mangle::{CNameTable, c_identifier};
pub use classify::{Classification, ClassifiedVariable, VariableClass, classify_variables};
pub use diagnostic::GalecTargetError;
pub use emit::{
    ManifestIdentity, algorithm_template_context, assemble_manifest_with_identity,
    c_template_context, c_template_context_for_block, render_algorithm_code,
};
pub use input::{GalecInput, GalecOptions, GalecProfile, ScalarTypeMap};
pub use lower::lower_to_algorithm_code;
pub use manifest_context::{
    AcManifestCtx, ContentCtx, EfmiError, EfmiManifestContext, FilePath, IdRegistry, Identifier,
    ManifestId, ModelExport, NameWithoutSlashes, NormalizedText, PcManifestCtx, Sha1Hex, UnitCtx,
    UtcTimestamp, xml_escape, xs_double,
};
pub use manifest_vars::{ManifestVariables, build_manifest_variables};
pub use package::{AlgorithmCodePackage, ManifestFragment};
pub use production_manifest::{
    EmittedCodeFile, assemble_production_manifest, assemble_production_manifest_with_identity,
};
