//! Temporary constructor-exclusivity/anti-bypass migration ratchet.
//!
//! This module does not validate Modelica semantics and does not replay a
//! phase checker.  It freezes reviewed architectural bypasses while the
//! compiler migrates to one fallible construction authority per trust
//! boundary, opaque success-bearing roots, and infallible invariant-preserving
//! internal operations (SPEC_0036).  A matching digest proves only that the
//! reviewed source item has not changed; it is not evidence that the item is
//! correct. Normalized owner tokens include documentation attributes, so a
//! documentation-only edit deliberately requires the same explicit re-review.

mod budget_controls;
mod mutation_tests;
mod owner_ledger;
mod owner_scan;
mod root_surface;

use std::collections::BTreeSet;
use std::fs;

use crate::architecture_hardening_support::{collect_rs_files, workspace_root};
use owner_ledger::{
    FAIL_OPEN_CUTOFF_DEBT, PUBLIC_PROOF_ESCAPE_OWNER_DEBT, RECOVERY_AUTHORITY_DEBT,
    RETIRED_OWNER_TOMBSTONES,
};
use owner_scan::{
    MacroTombstoneHit, OwnerDigest, collect_workspace_macro_tombstone_hits,
    collect_workspace_owners, compare_ledger,
};

const MIGRATION_NOTICE: &str = "\
This is a temporary SPEC_0036 constructor-exclusivity/anti-bypass migration ratchet, not semantic \
validation. A matching digest only says that a reviewed owner is unchanged; \
it does not prove that owner correct. Remove the bypass and its reviewed row \
when the sole constructor carries the fact forward instead.";

#[test]
fn test_reviewed_second_validators_and_recovery_authorities_do_not_change() {
    assert_debt_ledger(
        "second validator / recovery authority",
        RECOVERY_AUTHORITY_DEBT,
    );
}

#[test]
fn test_reviewed_fail_open_semantic_cutoffs_do_not_change() {
    assert_debt_ledger("fail-open semantic cutoff", FAIL_OPEN_CUTOFF_DEBT);
}

#[test]
fn test_reviewed_public_success_root_owner_escapes_do_not_change() {
    assert!(
        PUBLIC_PROOF_ESCAPE_OWNER_DEBT.is_empty(),
        "closed front-end proof ownership has no reviewed escape debt"
    );
    let differences = root_surface::closed_frontend_proof_owner_differences();
    assert!(
        differences.is_empty(),
        "actual production front-end proof owners escaped their closed catalogs:\n{}\n\n{MIGRATION_NOTICE}",
        differences.join("\n")
    );
}

#[test]
fn test_flatten_orchestration_has_no_legacy_raw_tree_parameter() {
    let path = workspace_root().join("crates/rumoca-compile/src/session/compile_support.rs");
    let source = fs::read_to_string(&path).expect("read compile support source");
    let syntax = syn::parse_file(&source).expect("parse compile support source");
    let function = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == "flat_model_outcome_from_typed" => {
                Some(function)
            }
            _ => None,
        })
        .expect("the sole typed-to-flat orchestration function exists");
    assert_eq!(
        function.sig.inputs.len(),
        1,
        "typed-to-flat orchestration accepts only the affine proof outcome; a raw tree parameter recreates proof-A/tree-B shape"
    );
    let Some(syn::FnArg::Typed(argument)) = function.sig.inputs.first() else {
        panic!("typed-to-flat orchestration input is a typed argument");
    };
    let syn::Type::Path(path) = argument.ty.as_ref() else {
        panic!("typed-to-flat orchestration input is the closed outcome type");
    };
    assert!(
        path.path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "TypedModelOutcome")
    );
}

#[test]
fn test_retired_recovery_owners_stay_deleted() {
    let present = present_tombstones(RETIRED_OWNER_TOMBSTONES, production_owners());
    let macro_hits = production_macro_tombstone_hits();
    assert!(
        present.is_empty() && macro_hits.is_empty(),
        "retired semantic recovery authorities were recreated:\n  exact owners: {present:#?}\n  macro token hits: {macro_hits:#?}\n\n{MIGRATION_NOTICE}"
    );
}

#[test]
fn test_resolve_post_pass_validation_stays_deleted() {
    let source_root = workspace_root().join("crates/rumoca-phase-resolve/src");
    assert!(
        !source_root.join("validation.rs").exists(),
        "Resolve lookup failures belong to their first producer; the deleted post-pass validator must not return"
    );

    let forbidden = [
        "validate_resolution",
        "ValidationResult",
        "UnresolvedSymbol",
        "find_inherited_type",
        "resolve_function_first_part",
        "resolve_type_name_with_inheritance",
        "is_ambiguous_inherited_type",
        "check_ambiguous_unqualified_imports",
    ];
    let mut sources = Vec::new();
    collect_rs_files(&source_root, &mut sources);
    let mut offenders = Vec::new();
    for path in sources {
        let source = fs::read_to_string(&path).expect("read Resolve source");
        for token in forbidden {
            if source.contains(token) {
                offenders.push(format!("{} contains {token}", path.display()));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "Resolve post-pass validation or a manual lookup authority was recreated; keep ScopeTree as the sole lookup authority and emit ER002/ER003 at the producer: {offenders:?}"
    );
}

#[test]
fn test_scope_lookup_keeps_ambiguity_typed_and_selective_imports_named() {
    let root = workspace_root();
    let scope = fs::read_to_string(root.join("crates/rumoca-ir-ast/src/scope.rs"))
        .expect("read ScopeTree source");
    for required in [
        "pub enum LookupOutcome",
        "AmbiguousInherited",
        "AmbiguousUnqualifiedImport",
        "SingleDefinition",
        "WildcardMember",
    ] {
        assert!(
            scope.contains(required),
            "ScopeTree lost required closed lookup/import state `{required}`"
        );
    }
    for forbidden in ["fn into_option", "fn as_option", "fn def_id_or_none"] {
        assert!(
            !scope.contains(forbidden),
            "ScopeTree recreated lossy ambiguity conversion `{forbidden}`"
        );
    }

    let imports = fs::read_to_string(root.join("crates/rumoca-phase-resolve/src/extends.rs"))
        .expect("read Resolve import construction");
    assert!(
        !imports.contains("ast::scope::Import::Unqualified")
            && !imports.contains("ast::scope::Import::Selective"),
        "Resolve must lower qualified, renamed, and selective imports to the one SingleDefinition tier"
    );
}

fn present_tombstones<'a>(tombstones: &'a [&'a str], owners: &[OwnerDigest]) -> Vec<&'a str> {
    tombstones
        .iter()
        .filter(|identity| owners.iter().any(|owner| owner.identity == **identity))
        .copied()
        .collect()
}

fn assert_debt_ledger(label: &str, ledger: &[OwnerDigest]) {
    let differences = compare_ledger(label, ledger, production_owners());
    assert!(
        differences.is_empty(),
        "reviewed {label} ledger changed:\n{}\n\n{MIGRATION_NOTICE}",
        differences.join("\n")
    );
}

fn production_owners() -> &'static [OwnerDigest] {
    static OWNERS: std::sync::OnceLock<Vec<OwnerDigest>> = std::sync::OnceLock::new();
    OWNERS.get_or_init(|| {
        let root = workspace_root();
        let crate_names = all_ledger_crates();
        let crates = crate_names
            .iter()
            .map(|name| (name.as_str(), root.join("crates").join(name)))
            .collect::<Vec<_>>();
        collect_workspace_owners(&crates, &root)
    })
}

fn production_macro_tombstone_hits() -> &'static [MacroTombstoneHit] {
    static HITS: std::sync::OnceLock<Vec<MacroTombstoneHit>> = std::sync::OnceLock::new();
    HITS.get_or_init(|| {
        let root = workspace_root();
        let crate_names = all_ledger_crates();
        let crates = crate_names
            .iter()
            .map(|name| (name.as_str(), root.join("crates").join(name)))
            .collect::<Vec<_>>();
        collect_workspace_macro_tombstone_hits(&crates, &root, RETIRED_OWNER_TOMBSTONES)
    })
}

fn all_ledger_crates() -> BTreeSet<String> {
    ledger_crates(
        RECOVERY_AUTHORITY_DEBT,
        FAIL_OPEN_CUTOFF_DEBT,
        PUBLIC_PROOF_ESCAPE_OWNER_DEBT,
        RETIRED_OWNER_TOMBSTONES,
    )
}

fn ledger_crates(
    recovery: &[OwnerDigest],
    fail_open: &[OwnerDigest],
    proof_escapes: &[OwnerDigest],
    tombstones: &[&str],
) -> BTreeSet<String> {
    recovery
        .iter()
        .map(|entry| entry.identity)
        .chain(fail_open.iter().map(|entry| entry.identity))
        .chain(proof_escapes.iter().map(|entry| entry.identity))
        .chain(tombstones.iter().copied())
        .filter_map(|identity| identity.split("::").next())
        .map(str::to_string)
        .collect()
}
