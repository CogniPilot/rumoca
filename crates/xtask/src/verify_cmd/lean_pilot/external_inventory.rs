//! Fail-closed inventory of the translator's external definitions.
//!
//! The replay may only supply hand-written models for definitions this file
//! names. A new external definition therefore refuses the replay instead of
//! silently acquiring a model, which is the failure this inventory exists to
//! prevent: a model is trusted base, and trusted base must never grow without
//! a verdict.
//!
//! The inventory reads the pinned translator's own `-emit-json` manifest. It
//! never parses generated Lean text, so a renamed or reformatted template
//! cannot change what is admitted.

use anyhow::{Result, bail, ensure};
use serde::Deserialize;
use std::collections::BTreeSet;
use std::path::Path;

/// The manifest category a definition is listed in.
///
/// Category is part of a definition's identity: the same name in a different
/// category is a different declaration with different admitted operations, so
/// the permitted list names both and a moved entry refuses.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Category {
    Function,
    Type,
    Global,
    TraitDecl,
    TraitImpl,
}

impl Category {
    fn label(self) -> &'static str {
        match self {
            Self::Function => "function",
            Self::Type => "type",
            Self::Global => "global",
            Self::TraitDecl => "trait declaration",
            Self::TraitImpl => "trait implementation",
        }
    }
}

/// External definitions this replay is permitted to model, by category, Rust
/// path, and the template file the translator must list them in.
///
/// `f64`'s storage primitive is **not** here: it is supplied as a missing
/// Aeneas standard-library type rather than listed as an external definition,
/// so this list does not inventory it. See the cut packet's trusted-model
/// decision.
const PERMITTED_EXTERNAL: &[(Category, &str, &str)] = &[
    (
        Category::Function,
        "core::f64::{f64}::to_bits",
        "FunsExternal_Template.lean",
    ),
    (
        Category::Type,
        "alloc::sync::Arc",
        "TypesExternal_Template.lean",
    ),
    (
        Category::Type,
        "indexmap::map::IndexMap",
        "TypesExternal_Template.lean",
    ),
    (
        Category::Type,
        "std::hash::random::RandomState",
        "TypesExternal_Template.lean",
    ),
    (
        Category::Type,
        "core::num::nonzero::NonZero",
        "TypesExternal_Template.lean",
    ),
    (
        Category::Type,
        "core::num::niche_types::NonZeroU32Inner",
        "TypesExternal_Template.lean",
    ),
    (
        Category::Type,
        "core::num::niche_types::NonZeroU64Inner",
        "TypesExternal_Template.lean",
    ),
];

/// The template suffix the translator gives files holding external definitions.
const TEMPLATE_SUFFIX: &str = "External_Template.lean";

/// Function entries carry an opacity flag; other categories do not. Missing
/// fields are refused rather than defaulted, so a schema change cannot be read
/// as a permissive value.
#[derive(Debug, Deserialize)]
pub(super) struct Manifest {
    functions: Vec<FunctionEntry>,
    types: Vec<Entry>,
    globals: Vec<Entry>,
    trait_decls: Vec<Entry>,
    trait_impls: Vec<Entry>,
}

#[derive(Debug, Deserialize)]
struct FunctionEntry {
    rust_name: String,
    lean_file: String,
    /// A first-party function the translator could not translate would need a
    /// hand-written model exactly like an external one.
    is_opaque: bool,
    source: Option<Source>,
}

#[derive(Debug, Deserialize)]
struct Entry {
    rust_name: String,
    lean_file: String,
    source: Option<Source>,
}

#[derive(Debug, Deserialize)]
struct Source {
    file: String,
}

/// One definition, flattened across categories for the shared checks.
struct Listed<'manifest> {
    category: Category,
    rust_name: &'manifest str,
    lean_file: &'manifest str,
    source: Option<&'manifest str>,
}

impl Manifest {
    fn listed(&self) -> Vec<Listed<'_>> {
        let functions = self.functions.iter().map(|entry| Listed {
            category: Category::Function,
            rust_name: &entry.rust_name,
            lean_file: &entry.lean_file,
            source: entry.source.as_ref().map(|source| source.file.as_str()),
        });
        let others = [
            (Category::Type, &self.types),
            (Category::Global, &self.globals),
            (Category::TraitDecl, &self.trait_decls),
            (Category::TraitImpl, &self.trait_impls),
        ]
        .into_iter()
        .flat_map(|(category, entries)| {
            entries.iter().map(move |entry| Listed {
                category,
                rust_name: &entry.rust_name,
                lean_file: &entry.lean_file,
                source: entry.source.as_ref().map(|source| source.file.as_str()),
            })
        });
        functions.chain(others).collect()
    }
}

/// Read the translator's manifest and refuse anything outside the declared
/// trusted boundary.
pub(super) fn check(path: &Path, bound_sources: &[&str]) -> Result<()> {
    let bytes =
        std::fs::read(path).map_err(|error| anyhow::anyhow!("read {}: {error}", path.display()))?;
    let manifest: Manifest = serde_json::from_slice(&bytes)
        .map_err(|error| anyhow::anyhow!("parse {}: {error}", path.display()))?;
    check_manifest(&manifest, bound_sources)
}

/// The whole policy, separated from file reading so it is directly testable.
///
/// `bound_sources` is the replay's own captured source list, so a first-party
/// definition can only come from bytes the replay already bound at entry.
pub(super) fn check_manifest(manifest: &Manifest, bound_sources: &[&str]) -> Result<()> {
    let mut seen: BTreeSet<(Category, &str)> = BTreeSet::new();
    for entry in manifest.listed() {
        if entry.lean_file.ends_with(TEMPLATE_SUFFIX) {
            let permitted = PERMITTED_EXTERNAL
                .iter()
                .find(|(_, name, _)| *name == entry.rust_name);
            let Some((category, _, file)) = permitted else {
                bail!(
                    "external {} `{}` in {} is not permitted; a model for it would \
                     enlarge the trusted base without review",
                    entry.category.label(),
                    entry.rust_name,
                    entry.lean_file
                );
            };
            ensure!(
                *category == entry.category,
                "external `{}` is listed as a {}, expected a {}",
                entry.rust_name,
                entry.category.label(),
                category.label()
            );
            ensure!(
                *file == entry.lean_file,
                "external `{}` moved to {}, expected {file}",
                entry.rust_name,
                entry.lean_file
            );
            ensure!(
                seen.insert((entry.category, entry.rust_name)),
                "external `{}` is listed more than once",
                entry.rust_name
            );
            continue;
        }

        // Everything not modelled is translated from bytes this replay bound at
        // entry. `is_local` is not consulted: a cross-crate first-party type
        // such as `LinearOp` reports `is_local: false`, so keying on it would
        // let a relocated first-party definition through.
        let Some(file) = entry.source else {
            bail!(
                "{} `{}` in {} has no source binding; its bytes cannot be checked",
                entry.category.label(),
                entry.rust_name,
                entry.lean_file
            );
        };
        ensure!(
            bound_sources.contains(&file),
            "extraction reached undeclared source {file} for {} `{}`; declare it \
             before trusting its bytes",
            entry.category.label(),
            entry.rust_name
        );
    }

    // Opacity is a function-only property in this schema, and an opaque
    // first-party function would need a hand-written model exactly like an
    // external one.
    for function in &manifest.functions {
        ensure!(
            !function.is_opaque || function.lean_file.ends_with(TEMPLATE_SUFFIX),
            "first-party function `{}` is opaque in {}; it would need a \
             hand-written model",
            function.rust_name,
            function.lean_file
        );
    }

    for (category, name, file) in PERMITTED_EXTERNAL {
        ensure!(
            seen.contains(&(*category, *name)),
            "permitted external {} `{name}` is missing from {file}; the model \
             would apply to nothing",
            category.label()
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The parent's captured source list, which the real replay passes in.
    const BOUND: &[&str] = super::super::SOURCES;

    /// Shaped after the actual full-caller manifest, which translates the
    /// first-party global `SOLVE_OWNERS` and reports the cross-crate
    /// first-party `LinearOp` with `is_local: false`. Every negative below is
    /// this value with one thing changed, so a refusal is attributable.
    fn accepted() -> serde_json::Value {
        serde_json::json!({
            "functions": [
                {
                    "rust_name": "core::f64::{f64}::to_bits",
                    "lean_file": "FunsExternal_Template.lean",
                    "is_opaque": true,
                    "source": { "file": "/rustc/library/core/src/num/f64.rs" }
                },
                {
                    "rust_name": "rumoca_phase_solve::scalar_constant_derivative_refinement::project_operations",
                    "lean_file": "Funs.lean",
                    "is_opaque": false,
                    "source": { "file": "crates/rumoca-phase-solve/src/scalar_constant_derivative_refinement.rs" }
                }
            ],
            "types": [
                {
                    "rust_name": "alloc::sync::Arc",
                    "lean_file": "TypesExternal_Template.lean",
                    "source": { "file": "/rustc/library/alloc/src/sync.rs" }
                },
                {
                    "rust_name": "rumoca_ir_solve::linear_op::LinearOp",
                    "lean_file": "Types.lean",
                    "source": { "file": "crates/rumoca-ir-solve/src/linear_op.rs" }
                },
                {
                    "rust_name": "indexmap::map::IndexMap",
                    "lean_file": "TypesExternal_Template.lean",
                    "source": { "file": "/cargo/registry/src/indexmap/src/map.rs" }
                },
                {
                    "rust_name": "std::hash::random::RandomState",
                    "lean_file": "TypesExternal_Template.lean",
                    "source": { "file": "/rustc/library/std/src/hash/random.rs" }
                },
                {
                    "rust_name": "core::num::nonzero::NonZero",
                    "lean_file": "TypesExternal_Template.lean",
                    "source": { "file": "/rustc/library/core/src/num/nonzero.rs" }
                },
                {
                    "rust_name": "core::num::niche_types::NonZeroU32Inner",
                    "lean_file": "TypesExternal_Template.lean",
                    "source": { "file": "/rustc/library/core/src/num/niche_types.rs" }
                },
                {
                    "rust_name": "core::num::niche_types::NonZeroU64Inner",
                    "lean_file": "TypesExternal_Template.lean",
                    "source": { "file": "/rustc/library/core/src/num/niche_types.rs" }
                }
            ],
            "globals": [
                {
                    "rust_name": "rumoca_phase_solve::scalar_constant_derivative_refinement::SOLVE_OWNERS",
                    "lean_file": "Funs.lean",
                    "source": { "file": "crates/rumoca-phase-solve/src/scalar_constant_derivative_refinement.rs" }
                }
            ],
            "trait_decls": [],
            "trait_impls": []
        })
    }

    fn verdict(manifest: serde_json::Value) -> Result<()> {
        check_manifest(
            &serde_json::from_value(manifest).expect("manifest shape"),
            BOUND,
        )
    }

    #[test]
    fn the_actual_full_caller_translation_is_admitted() {
        verdict(accepted()).expect("the real translation boundary must pass");
    }

    #[test]
    fn translated_marker_requires_its_exact_bound_source() {
        let mut fixture = accepted();
        fixture["types"]
            .as_array_mut()
            .expect("types")
            .push(serde_json::json!({
                "rust_name": "core::marker::PhantomData",
                "lean_file": "Types.lean",
                "source": { "file": "/rustc/library/core/src/marker.rs" }
            }));
        let manifest: Manifest = serde_json::from_value(fixture.clone()).expect("manifest");
        assert!(check_manifest(&manifest, BOUND).is_err());
        let mut bound = BOUND.to_vec();
        bound.push(super::super::TOOLCHAIN_SOURCES[0].0);
        check_manifest(&manifest, &bound).expect("the exact translated source was captured");
        let types = fixture["types"].as_array_mut().expect("types");
        types.last_mut().expect("marker")["source"]["file"] =
            serde_json::json!("/rustc/library/core/src/other.rs");
        let foreign: Manifest = serde_json::from_value(fixture).expect("foreign manifest");
        assert!(check_manifest(&foreign, &bound).is_err());
    }

    #[test]
    fn a_translated_first_party_global_is_not_rejected() {
        let mut manifest = accepted();
        manifest["functions"] = serde_json::json!([manifest["functions"][0].clone()]);
        manifest["types"].as_array_mut().expect("types").remove(1);
        verdict(manifest).expect("a source-bound first-party global is not external");
    }

    #[test]
    fn an_external_global_refuses() {
        let mut manifest = accepted();
        manifest["globals"]
            .as_array_mut()
            .expect("globals")
            .push(serde_json::json!({
                "rust_name": "core::f64::consts::PI",
                "lean_file": "GlobalsExternal_Template.lean",
                "source": { "file": "/rustc/library/core/src/num/f64.rs" }
            }));
        let error = verdict(manifest).expect_err("an external global must refuse");
        assert!(error.to_string().contains("PI"), "{error}");
    }

    #[test]
    fn an_unknown_external_definition_refuses() {
        let mut manifest = accepted();
        manifest["functions"]
            .as_array_mut()
            .expect("functions")
            .push(serde_json::json!({
                "rust_name": "core::f64::{f64}::from_bits",
                "lean_file": "FunsExternal_Template.lean",
                "is_opaque": true,
                "source": { "file": "/rustc/library/core/src/num/f64.rs" }
            }));
        let error = verdict(manifest).expect_err("a new external definition must refuse");
        assert!(error.to_string().contains("from_bits"), "{error}");
    }

    #[test]
    fn admitted_storage_types_do_not_admit_their_operations() {
        for name in [
            "indexmap::map::{indexmap::map::IndexMap<K, V, S>}::len",
            "std::hash::random::{std::hash::random::RandomState}::new",
            "core::num::nonzero::{core::num::nonzero::NonZero<T>}::get",
        ] {
            let mut manifest = accepted();
            manifest["functions"]
                .as_array_mut()
                .expect("functions")
                .push(serde_json::json!({
                    "rust_name": name,
                    "lean_file": "FunsExternal_Template.lean",
                    "is_opaque": true,
                    "source": { "file": "/external/operation.rs" }
                }));
            let error = verdict(manifest).expect_err("storage is not an operation contract");
            assert!(error.to_string().contains(name), "{error}");
        }
    }

    #[test]
    fn every_root_storage_type_is_required() {
        for name in [
            "indexmap::map::IndexMap",
            "std::hash::random::RandomState",
            "core::num::nonzero::NonZero",
            "core::num::niche_types::NonZeroU32Inner",
            "core::num::niche_types::NonZeroU64Inner",
        ] {
            let mut manifest = accepted();
            let types = manifest["types"].as_array_mut().expect("types");
            let original_length = types.len();
            types.retain(|entry| entry["rust_name"] != name);
            assert_eq!(
                types.len(),
                original_length - 1,
                "fixture must contain {name}"
            );
            let error = verdict(manifest).expect_err("a disappearing model must refuse");
            assert!(error.to_string().contains(name), "{error}");
        }
    }

    #[test]
    fn a_permitted_definition_in_the_wrong_category_refuses() {
        let mut manifest = accepted();
        manifest["types"] = serde_json::json!([manifest["types"][1].clone()]);
        manifest["functions"]
            .as_array_mut()
            .expect("functions")
            .push(serde_json::json!({
                "rust_name": "alloc::sync::Arc",
                "lean_file": "TypesExternal_Template.lean",
                "is_opaque": false,
                "source": { "file": "/rustc/library/alloc/src/sync.rs" }
            }));
        let error = verdict(manifest).expect_err("a category move must refuse");
        assert!(
            error.to_string().contains("listed as a function"),
            "{error}"
        );
    }

    #[test]
    fn a_missing_permitted_definition_refuses() {
        let mut manifest = accepted();
        manifest["types"] = serde_json::json!([manifest["types"][1].clone()]);
        let error = verdict(manifest).expect_err("a vanished external definition must refuse");
        assert!(error.to_string().contains("alloc::sync::Arc"), "{error}");
    }

    #[test]
    fn a_duplicated_external_definition_refuses() {
        let mut manifest = accepted();
        let duplicate = manifest["functions"][0].clone();
        manifest["functions"]
            .as_array_mut()
            .expect("functions")
            .push(duplicate);
        let error = verdict(manifest).expect_err("a duplicate must refuse");
        assert!(error.to_string().contains("more than once"), "{error}");
    }

    #[test]
    fn a_relocated_external_definition_refuses() {
        let mut manifest = accepted();
        manifest["functions"][0]["lean_file"] = serde_json::json!("TypesExternal_Template.lean");
        let error = verdict(manifest).expect_err("a relocated definition must refuse");
        assert!(
            error
                .to_string()
                .contains("expected FunsExternal_Template.lean"),
            "{error}"
        );
    }

    #[test]
    fn an_opaque_first_party_function_refuses() {
        let mut manifest = accepted();
        manifest["functions"][1]["is_opaque"] = serde_json::json!(true);
        let error = verdict(manifest).expect_err("an untranslated body must refuse");
        assert!(error.to_string().contains("project_operations"), "{error}");
    }

    /// The escape that `is_local` keying allowed: `LinearOp` reports
    /// `is_local: false`, so a source check gated on locality skipped it.
    ///
    /// The relocation target must remain outside the captured list; this
    /// assertion diagnoses a stale fixture before exercising admission.
    #[test]
    fn a_relocated_cross_crate_first_party_type_refuses() {
        const UNCAPTURED: &str = "crates/rumoca-ir-solve/src/certificate.rs";
        assert!(
            !BOUND.contains(&UNCAPTURED),
            "this test needs a source the replay does not capture; {UNCAPTURED} is now in SOURCES"
        );
        let mut manifest = accepted();
        manifest["types"][1]["source"]["file"] = serde_json::json!(UNCAPTURED);
        let error = verdict(manifest).expect_err("a newly reached source must refuse");
        assert!(error.to_string().contains("certificate.rs"), "{error}");
    }

    #[test]
    fn a_definition_without_a_source_binding_refuses() {
        let mut manifest = accepted();
        manifest["types"][1]
            .as_object_mut()
            .expect("entry")
            .remove("source");
        let error = verdict(manifest).expect_err("an unbindable definition must refuse");
        assert!(error.to_string().contains("no source binding"), "{error}");
    }

    #[test]
    fn a_missing_category_array_refuses_instead_of_defaulting_empty() {
        let mut manifest = accepted();
        manifest
            .as_object_mut()
            .expect("manifest")
            .remove("trait_impls");
        let parsed: std::result::Result<Manifest, _> = serde_json::from_value(manifest);
        assert!(parsed.is_err(), "a missing category must not read as empty");
    }

    #[test]
    fn a_missing_opacity_flag_refuses_instead_of_defaulting_false() {
        let mut manifest = accepted();
        manifest["functions"][1]
            .as_object_mut()
            .expect("entry")
            .remove("is_opaque");
        let parsed: std::result::Result<Manifest, _> = serde_json::from_value(manifest);
        assert!(
            parsed.is_err(),
            "a missing opacity flag must not read as false"
        );
    }
}
