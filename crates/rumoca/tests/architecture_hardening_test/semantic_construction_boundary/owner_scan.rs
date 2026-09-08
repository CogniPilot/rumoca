//! Exact Rust-item ownership and normalized digest support for the migration ratchet.

use std::collections::{BTreeMap, btree_map::Entry};
use std::path::{Path, PathBuf};

use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;
use syn::{ImplItem, Item, TraitItem, Type};

use crate::architecture_hardening_support::{
    attributes_require_test, production_rust_source_contexts,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) struct OwnerDigest {
    pub(super) identity: &'static str,
    pub(super) normalized_len: usize,
    pub(super) blake3: &'static str,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct MeasuredOwner {
    identity: String,
    normalized_len: usize,
    blake3: String,
    normalized: String,
    path: PathBuf,
    source_ordinal: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct MacroTombstoneHit {
    pub(super) tombstone: String,
    pub(super) path: PathBuf,
}

pub(super) struct OwnerTokenInventory {
    pub(super) tokens: BTreeMap<String, String>,
    pub(super) differences: Vec<String>,
}

pub(super) fn collect_workspace_owners(
    crates: &[(&str, PathBuf)],
    workspace: &Path,
) -> Vec<OwnerDigest> {
    let mut owners = collect_measured_owners(crates, workspace);
    owners.dedup();
    owners
        .into_iter()
        .map(|owner| OwnerDigest {
            identity: Box::leak(owner.identity.into_boxed_str()),
            normalized_len: owner.normalized_len,
            blake3: Box::leak(owner.blake3.into_boxed_str()),
        })
        .collect()
}

fn collect_measured_owners(crates: &[(&str, PathBuf)], workspace: &Path) -> Vec<MeasuredOwner> {
    let mut owners = Vec::new();
    for (crate_name, crate_root) in crates {
        for context in production_rust_source_contexts(crate_root, workspace) {
            let parsed = syn::parse_file(&context.source).unwrap_or_else(|error| {
                panic!(
                    "parse production Rust source {} for architecture ownership: {error}",
                    context.path.display()
                )
            });
            let mut source_ordinal = 0;
            collect_file_owners(
                crate_name,
                &context.module_path,
                &context.path,
                &parsed.items,
                &mut source_ordinal,
                &mut owners,
            );
        }
    }
    sort_measured_owners(&mut owners);
    owners
}

fn sort_measured_owners(owners: &mut [MeasuredOwner]) {
    owners.sort_by(|left, right| {
        (
            &left.identity,
            &left.path,
            left.source_ordinal,
            &left.blake3,
        )
            .cmp(&(
                &right.identity,
                &right.path,
                right.source_ordinal,
                &right.blake3,
            ))
    });
}

fn collect_file_owners(
    crate_name: &str,
    module_path: &[String],
    path: &Path,
    items: &[Item],
    source_ordinal: &mut usize,
    owners: &mut Vec<MeasuredOwner>,
) {
    for item in items {
        match item {
            Item::Fn(function) => collect_function_owner(
                crate_name,
                module_path,
                path,
                function,
                source_ordinal,
                owners,
            ),
            Item::Impl(implementation) => collect_impl_owners(
                crate_name,
                module_path,
                path,
                implementation,
                source_ordinal,
                owners,
            ),
            Item::Trait(trait_item) => collect_trait_owners(
                crate_name,
                module_path,
                path,
                trait_item,
                source_ordinal,
                owners,
            ),
            Item::Mod(module) => collect_module_owners(
                crate_name,
                module_path,
                path,
                module,
                source_ordinal,
                owners,
            ),
            _ => {}
        }
    }
}

fn collect_function_owner(
    crate_name: &str,
    module_path: &[String],
    path: &Path,
    function: &syn::ItemFn,
    source_ordinal: &mut usize,
    owners: &mut Vec<MeasuredOwner>,
) {
    if attributes_require_test(&function.attrs) {
        return;
    }
    owners.push(measured_owner(
        item_identity(
            crate_name,
            module_path,
            None,
            &function.sig.ident.to_string(),
        ),
        function,
        path,
        source_ordinal,
    ));
}

fn collect_impl_owners(
    crate_name: &str,
    module_path: &[String],
    path: &Path,
    implementation: &syn::ItemImpl,
    source_ordinal: &mut usize,
    owners: &mut Vec<MeasuredOwner>,
) {
    if attributes_require_test(&implementation.attrs) {
        return;
    }
    let Some(type_name) = simple_type_name(&implementation.self_ty) else {
        return;
    };
    let owner_name = implementation
        .trait_
        .as_ref()
        .map_or(type_name.clone(), |(_, path, _)| {
            trait_impl_owner_name(&type_name, path)
        });
    for member in &implementation.items {
        let ImplItem::Fn(method) = member else {
            continue;
        };
        if attributes_require_test(&method.attrs) {
            continue;
        }
        owners.push(measured_owner(
            item_identity(
                crate_name,
                module_path,
                Some(&owner_name),
                &method.sig.ident.to_string(),
            ),
            method,
            path,
            source_ordinal,
        ));
    }
}

fn collect_trait_owners(
    crate_name: &str,
    module_path: &[String],
    path: &Path,
    trait_item: &syn::ItemTrait,
    source_ordinal: &mut usize,
    owners: &mut Vec<MeasuredOwner>,
) {
    if attributes_require_test(&trait_item.attrs) {
        return;
    }
    for member in &trait_item.items {
        let TraitItem::Fn(method) = member else {
            continue;
        };
        if method.default.is_none() || attributes_require_test(&method.attrs) {
            continue;
        }
        owners.push(measured_owner(
            item_identity(
                crate_name,
                module_path,
                Some(&trait_item.ident.to_string()),
                &method.sig.ident.to_string(),
            ),
            method,
            path,
            source_ordinal,
        ));
    }
}

fn collect_module_owners(
    crate_name: &str,
    module_path: &[String],
    path: &Path,
    module: &syn::ItemMod,
    source_ordinal: &mut usize,
    owners: &mut Vec<MeasuredOwner>,
) {
    if attributes_require_test(&module.attrs) {
        return;
    }
    let Some((_, nested)) = &module.content else {
        return;
    };
    let mut nested_path = module_path.to_vec();
    nested_path.push(module.ident.to_string());
    collect_file_owners(
        crate_name,
        &nested_path,
        path,
        nested,
        source_ordinal,
        owners,
    );
}

fn trait_impl_owner_name(type_name: &str, trait_path: &syn::Path) -> String {
    let normalized_trait = trait_path.to_token_stream().to_string().replace(' ', "");
    format!("<{type_name} as {normalized_trait}>")
}

fn simple_type_name(ty: &Type) -> Option<String> {
    let Type::Path(path) = ty else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

fn item_identity(
    crate_name: &str,
    module_path: &[String],
    type_name: Option<&str>,
    item_name: &str,
) -> String {
    let mut segments = vec![crate_name.to_string()];
    segments.extend(module_path.iter().cloned());
    segments.extend(type_name.map(str::to_string));
    segments.push(item_name.to_string());
    segments.join("::")
}

fn measured_owner(
    identity: String,
    item: &impl ToTokens,
    path: &Path,
    source_ordinal: &mut usize,
) -> MeasuredOwner {
    let normalized = item.to_token_stream().to_string();
    let measured = MeasuredOwner {
        identity,
        normalized_len: normalized.len(),
        blake3: blake3::hash(normalized.as_bytes()).to_hex().to_string(),
        normalized,
        path: path.to_path_buf(),
        source_ordinal: *source_ordinal,
    };
    *source_ordinal += 1;
    measured
}

pub(super) fn collect_workspace_owner_tokens(
    crates: &[(&str, PathBuf)],
    workspace: &Path,
) -> OwnerTokenInventory {
    let mut owners = collect_measured_owners(crates, workspace);
    owners.dedup();
    owner_token_inventory(owners)
}

fn owner_token_inventory(owners: Vec<MeasuredOwner>) -> OwnerTokenInventory {
    let mut by_identity = BTreeMap::new();
    let mut differences = Vec::new();
    for owner in owners {
        match by_identity.entry(owner.identity.clone()) {
            Entry::Vacant(entry) => {
                entry.insert(owner.normalized);
            }
            Entry::Occupied(_) => differences.push(format!(
                "  multiple production owners claim `{}` (including {}); exact construction authority is not exclusive",
                owner.identity,
                owner.path.display()
            )),
        }
    }
    OwnerTokenInventory {
        tokens: by_identity,
        differences,
    }
}

pub(super) fn collect_workspace_macro_tombstone_hits(
    crates: &[(&str, PathBuf)],
    workspace: &Path,
    tombstones: &[&str],
) -> Vec<MacroTombstoneHit> {
    let mut hits = Vec::new();
    for (crate_name, crate_root) in crates {
        for context in production_rust_source_contexts(crate_root, workspace) {
            let parsed = syn::parse_file(&context.source).unwrap_or_else(|error| {
                panic!(
                    "parse production Rust source {} for macro tombstones: {error}",
                    context.path.display()
                )
            });
            collect_file_macro_tombstone_hits(
                crate_name,
                &context.path,
                &parsed.items,
                tombstones,
                &mut hits,
            );
        }
    }
    hits.sort_by(|left, right| (&left.tombstone, &left.path).cmp(&(&right.tombstone, &right.path)));
    hits.dedup();
    hits
}

fn collect_file_macro_tombstone_hits(
    crate_name: &str,
    path: &Path,
    items: &[Item],
    tombstones: &[&str],
    hits: &mut Vec<MacroTombstoneHit>,
) {
    for item in items {
        match item {
            Item::Macro(item_macro) => {
                collect_macro_tombstone_hits(crate_name, path, item_macro, tombstones, hits)
            }
            Item::Mod(module) => {
                collect_module_macro_tombstone_hits(crate_name, path, module, tombstones, hits)
            }
            _ => {}
        }
    }
}

fn collect_macro_tombstone_hits(
    crate_name: &str,
    path: &Path,
    item_macro: &syn::ItemMacro,
    tombstones: &[&str],
    hits: &mut Vec<MacroTombstoneHit>,
) {
    if attributes_require_test(&item_macro.attrs) {
        return;
    }
    for tombstone in tombstones {
        if tombstone_crate(tombstone) != Some(crate_name) {
            continue;
        }
        let Some(item_name) = tombstone.rsplit("::").next() else {
            continue;
        };
        if !token_stream_contains_identifier(&item_macro.mac.tokens, item_name) {
            continue;
        }
        hits.push(MacroTombstoneHit {
            tombstone: (*tombstone).to_string(),
            path: path.to_path_buf(),
        });
    }
}

fn collect_module_macro_tombstone_hits(
    crate_name: &str,
    path: &Path,
    module: &syn::ItemMod,
    tombstones: &[&str],
    hits: &mut Vec<MacroTombstoneHit>,
) {
    if attributes_require_test(&module.attrs) {
        return;
    }
    let Some((_, nested)) = &module.content else {
        return;
    };
    collect_file_macro_tombstone_hits(crate_name, path, nested, tombstones, hits);
}

fn tombstone_crate(tombstone: &str) -> Option<&str> {
    tombstone.split("::").next()
}

fn token_stream_contains_identifier(tokens: &TokenStream, expected: &str) -> bool {
    tokens.clone().into_iter().any(|token| match token {
        TokenTree::Ident(identifier) => identifier == expected,
        TokenTree::Group(group) => token_stream_contains_identifier(&group.stream(), expected),
        TokenTree::Punct(_) | TokenTree::Literal(_) => false,
    })
}

pub(super) fn compare_ledger(
    label: &str,
    expected: &[OwnerDigest],
    measured: &[OwnerDigest],
) -> Vec<String> {
    let expected_by_owner = unique_entries(label, expected);
    let mut measured_by_owner = BTreeMap::<_, Vec<_>>::new();
    for entry in measured {
        measured_by_owner
            .entry(entry.identity)
            .or_default()
            .push(*entry);
    }
    let mut differences = Vec::new();
    for (identity, expected) in expected_by_owner {
        let Some(actual) = measured_by_owner.get(identity) else {
            differences.push(format!(
                "  stale/deleted/renamed owner `{identity}` (reviewed row must be removed only after architectural review)"
            ));
            continue;
        };
        if actual.len() != 1 {
            differences.push(format!(
                "  owner identity `{identity}` has {} production definitions; construction authority is not exclusive",
                actual.len()
            ));
            continue;
        }
        let actual = actual[0];
        if expected.normalized_len != actual.normalized_len || expected.blake3 != actual.blake3 {
            differences.push(format!(
                "  changed owner `{identity}`: expected len {} blake3 {}, measured len {} blake3 {}",
                expected.normalized_len,
                expected.blake3,
                actual.normalized_len,
                actual.blake3
            ));
        }
    }
    differences
}

fn unique_entries<'a>(label: &str, entries: &'a [OwnerDigest]) -> BTreeMap<&'a str, OwnerDigest> {
    let mut unique = BTreeMap::new();
    for entry in entries {
        assert!(
            unique.insert(entry.identity, *entry).is_none(),
            "duplicate owner `{}` in {label} ledger",
            entry.identity
        );
        assert_eq!(
            entry.blake3.len(),
            64,
            "owner `{}` has an unreviewed/non-blake3 digest",
            entry.identity
        );
        assert!(
            entry.normalized_len > 0,
            "owner `{}` has no reviewed normalized token length",
            entry.identity
        );
    }
    unique
}

#[cfg(test)]
pub(super) fn collect_fixture_owners(crate_name: &str, source: &str) -> Vec<OwnerDigest> {
    let parsed = syn::parse_file(source).expect("parse ownership mutation fixture");
    let mut measured = Vec::new();
    let mut source_ordinal = 0;
    collect_file_owners(
        crate_name,
        &[],
        Path::new("fixture.rs"),
        &parsed.items,
        &mut source_ordinal,
        &mut measured,
    );
    measured
        .into_iter()
        .map(|owner| OwnerDigest {
            identity: Box::leak(owner.identity.into_boxed_str()),
            normalized_len: owner.normalized_len,
            blake3: Box::leak(owner.blake3.into_boxed_str()),
        })
        .collect()
}

#[cfg(test)]
pub(super) fn collect_fixture_owner_tokens(crate_name: &str, source: &str) -> OwnerTokenInventory {
    let parsed = syn::parse_file(source).expect("parse ownership-token mutation fixture");
    let mut measured = Vec::new();
    let mut source_ordinal = 0;
    collect_file_owners(
        crate_name,
        &[],
        Path::new("fixture.rs"),
        &parsed.items,
        &mut source_ordinal,
        &mut measured,
    );
    sort_measured_owners(&mut measured);
    measured.dedup();
    owner_token_inventory(measured)
}

#[cfg(test)]
pub(super) fn collect_fixture_macro_tombstone_hits(
    crate_name: &str,
    source: &str,
    tombstones: &[&str],
) -> Vec<MacroTombstoneHit> {
    let parsed = syn::parse_file(source).expect("parse macro-tombstone mutation fixture");
    let mut hits = Vec::new();
    collect_file_macro_tombstone_hits(
        crate_name,
        Path::new("fixture.rs"),
        &parsed.items,
        tombstones,
        &mut hits,
    );
    hits
}
