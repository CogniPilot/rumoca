//! Projection from the canonical Rumoca DAE IR to an eFMI Algorithm Code
//! package: admissibility checks over the untouched DAE, lowering to the
//! GALEC AST plus a typed Algorithm Code manifest model, and package
//! validation. See `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`.
//!
//! This crate is the projection layer of the SPEC_0034 three-module split
//! (GAL-010): `rumoca-galec` owns the language (AST/printer/validator),
//! `rumoca-efmi` owns packaging (manifest models/XML/checksums/container),
//! and this crate maps canonical compiler artifacts onto both. It is
//! reached through `rumoca-compile`; GALEC is a target language, never a
//! canonical IR stage (GAL-001).
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
//! - [`emit`] — rendering facades: [`render_algorithm_code`] (`.alg` text),
//!   [`render_manifest_xml`] (typed manifest → XML), and
//!   [`c_template_context`] (typed-then-serialized minijinja context,
//!   unstable until slice 5c);
//! - [`package`] — the [`AlgorithmCodePackage`] output contract
//!   (constructed only by lowering, post-validated per GAL-004);
//! - [`diagnostic`] — SPEC_0008-shaped [`GalecTargetError`] with stable
//!   `ET0xx` codes and DAE source spans where available; projection-scope
//!   rejections use the GAL-025 wording.

pub mod admissibility;
pub mod classify;
pub mod diagnostic;
pub mod emit;
pub mod input;
pub mod lower;
pub mod mangle;
pub mod manifest_vars;
pub mod package;

pub use admissibility::{AdmittedClock, check_admissibility};
pub use classify::{Classification, ClassifiedVariable, VariableClass, classify_variables};
pub use diagnostic::GalecTargetError;
pub use emit::{c_template_context, render_algorithm_code, render_manifest_xml};
pub use input::{GalecInput, GalecOptions, GalecProfile, ScalarTypeMap};
pub use lower::lower_to_algorithm_code;
pub use manifest_vars::{ManifestVariables, build_manifest_variables};
pub use package::{AlgorithmCodePackage, ManifestFragment};
// Re-exported so `ScalarTypeMap` builders (rumoca-compile, GAL-010) can name
// the map's value type without depending on `rumoca-galec` directly.
pub use rumoca_galec::ast::ScalarType;
