//! Adversarial fixtures for the migration ratchet itself.

use super::owner_scan::{
    OwnerDigest, collect_fixture_macro_tombstone_hits, collect_fixture_owner_tokens,
    collect_fixture_owners, compare_ledger,
};
use super::root_surface::{
    RootSurface, RouteKind, fixture_closed_proof_route_inventory, fixture_root_surface,
    fixture_route_inventory,
};
use super::{ledger_crates, present_tombstones};

fn sole_owner(source: &str) -> OwnerDigest {
    let owners = collect_fixture_owners("fixture", source);
    assert_eq!(owners.len(), 1, "fixture must contain one owner");
    owners[0]
}

#[test]
fn equal_length_body_change_invalidates_full_owner_digest() {
    let expected = sole_owner("fn recover(required: i32) -> i32 { required + 1 }");
    let changed = sole_owner("fn recover(required: i32) -> i32 { required - 1 }");
    assert_eq!(expected.normalized_len, changed.normalized_len);
    assert_ne!(expected.blake3, changed.blake3);
    assert!(!compare_ledger("fixture recovery", &[expected], &[changed]).is_empty());
}

#[test]
fn owner_rename_and_deletion_leave_stale_review_rows() {
    let expected = sole_owner("fn recover() -> Option<u8> { None }");
    let renamed = sole_owner("fn rescue() -> Option<u8> { None }");
    let rename_diff = compare_ledger("fixture recovery", &[expected], &[renamed]);
    let deletion_diff = compare_ledger("fixture recovery", &[expected], &[]);
    assert!(
        rename_diff
            .iter()
            .any(|difference| difference.contains("stale/deleted/renamed"))
    );
    assert!(
        deletion_diff
            .iter()
            .any(|difference| difference.contains("stale/deleted/renamed"))
    );
}

#[test]
fn recreated_exact_tombstone_is_detected_as_a_new_owner() {
    let recreated =
        collect_fixture_owners("fixture", "fn retired_recovery() -> Option<u8> { None }");
    assert_eq!(
        present_tombstones(&["fixture::retired_recovery"], &recreated),
        vec!["fixture::retired_recovery"]
    );
}

#[test]
fn a_tombstone_only_crate_remains_in_the_production_scan_set() {
    let crates = ledger_crates(&[], &[], &[], &["retired-only::Owner::recover"]);
    assert_eq!(crates, ["retired-only".to_string()].into_iter().collect());
}

#[test]
fn duplicate_exact_owner_identity_breaks_constructor_exclusivity() {
    let expected = sole_owner("fn recover() -> Option<u8> { None }");
    let duplicate = collect_fixture_owners(
        "fixture",
        "fn recover() -> Option<u8> { None } fn recover() -> Option<u8> { Some(1) }",
    );
    let differences = compare_ledger("fixture recovery", &[expected], &duplicate);
    assert!(
        differences
            .iter()
            .any(|difference| difference.contains("construction authority is not exclusive"))
    );
}

#[test]
fn replacing_a_typed_error_with_absence_invalidates_owner() {
    let expected = sole_owner("fn close() -> Result<u8, Error> { Err(Error) }");
    let fail_open = sole_owner("fn close() -> Option<u8> { None }");
    let differences = compare_ledger("fixture cutoff", &[expected], &[fail_open]);
    assert!(
        differences
            .iter()
            .any(|difference| difference.contains("changed owner"))
    );
}

#[test]
fn every_named_public_root_escape_changes_the_structural_surface() {
    let baseline = fixture_root_surface("pub struct Root { proof: u8 }", "Root");
    let mutations = [
        "pub struct Root { proof: u8, pub raw: u8 }",
        "#[derive(Default)] pub struct Root { proof: u8 }",
        "pub struct Root { proof: u8 } impl Root { pub fn new() -> Self { Self { proof: 0 } } }",
        "pub struct Root { proof: u8 } impl Root { pub fn validate(&self) {} }",
        "pub struct Root { proof: u8 } impl std::ops::Deref for Root { type Target = u8; fn deref(&self) -> &u8 { &self.proof } } impl std::ops::DerefMut for Root { fn deref_mut(&mut self) -> &mut u8 { &mut self.proof } }",
    ];
    for mutation in mutations {
        assert_ne!(baseline, fixture_root_surface(mutation, "Root"));
    }
}

#[test]
fn typecheck_proof_mint_visibility_and_projection_adoption_mutants_are_visible() {
    let restricted_mint = fixture_root_surface(
        "pub struct TypedInstancedTree; impl TypedInstancedTree { pub(crate) fn mint() -> Self { Self } }",
        "TypedInstancedTree",
    );
    let public_mint = fixture_root_surface(
        "pub struct TypedInstancedTree; impl TypedInstancedTree { pub fn mint() -> Self { Self } }",
        "TypedInstancedTree",
    );
    assert_ne!(
        restricted_mint, public_mint,
        "widening the sole mint to public must change the compiler-backed root surface"
    );

    let baseline = fixture_closed_proof_route_inventory(
        "pub struct TypedInstancedTree; pub struct TypedOverlayProjection;",
        "TypedInstancedTree",
    );
    let adopted = fixture_closed_proof_route_inventory(
        "pub struct TypedInstancedTree; pub struct TypedOverlayProjection; fn adopt(_: TypedOverlayProjection) -> TypedInstancedTree { TypedInstancedTree }",
        "TypedInstancedTree",
    );
    assert_ne!(
        baseline, adopted,
        "a same-module projection-to-proof adoption route must enter the exact route inventory"
    );
}

#[test]
fn existing_mint_signature_cannot_be_changed_to_adopt_the_projection() {
    let raw_overlay_mint = fixture_closed_proof_route_inventory(
        "pub struct TypedInstancedTree; struct InstanceOverlay; pub struct TypedOverlayProjection; impl TypedInstancedTree { pub(crate) fn mint(_: InstanceOverlay) -> Self { Self } }",
        "TypedInstancedTree",
    );
    let projection_adoption_mint = fixture_closed_proof_route_inventory(
        "pub struct TypedInstancedTree; struct InstanceOverlay; pub struct TypedOverlayProjection; impl TypedInstancedTree { pub(crate) fn mint(_: TypedOverlayProjection) -> Self { Self } }",
        "TypedInstancedTree",
    );
    assert_ne!(
        raw_overlay_mint, projection_adoption_mint,
        "changing the existing mint's input must change the exact signature inventory"
    );
}

#[test]
fn carrier_associated_macros_are_visible_even_when_they_spell_only_self() {
    let baseline = fixture_closed_proof_route_inventory(
        "pub struct TypedInstancedTree; pub struct TypedOverlayProjection;",
        "TypedInstancedTree",
    );
    let associated_macro = fixture_closed_proof_route_inventory(
        "macro_rules! adopt { () => { fn adopt(_: TypedOverlayProjection) -> Self { Self } } } pub struct TypedInstancedTree; pub struct TypedOverlayProjection; impl TypedInstancedTree { adopt!(); }",
        "TypedInstancedTree",
    );
    assert_ne!(baseline, associated_macro);
    assert!(
        associated_macro
            .iter()
            .any(|route| { route.kinds.contains(&RouteKind::UnexpandedMacroBoundary) })
    );
}

#[test]
fn downstream_private_carriers_and_macro_consumers_enter_the_full_route_set() {
    let baseline = fixture_closed_proof_route_inventory(
        "pub struct TypedInstancedTree;",
        "TypedInstancedTree",
    );
    let private_carrier = fixture_closed_proof_route_inventory(
        "pub struct TypedInstancedTree; struct Extra(TypedInstancedTree);",
        "TypedInstancedTree",
    );
    assert_ne!(baseline, private_carrier);
    assert!(private_carrier.iter().any(|route| {
        route.identity == "fixture::Extra" && route.kinds.contains(&RouteKind::CarrierDefinition)
    }));

    let macro_consumer = fixture_closed_proof_route_inventory(
        "pub struct TypedInstancedTree; macro_rules! consume { ($ty:ty) => { fn consume(_: $ty) {} } } consume!(TypedInstancedTree);",
        "TypedInstancedTree",
    );
    assert_ne!(baseline, macro_consumer);
    assert!(
        macro_consumer
            .iter()
            .any(|route| { route.kinds.contains(&RouteKind::UnexpandedMacroBoundary) })
    );
}

#[test]
fn every_deserialization_side_door_changes_the_structural_surface() {
    let derive_deserialize = fixture_root_surface(
        "#[derive(Deserialize)] pub struct Root { proof: u8 }",
        "Root",
    );
    let serde_attribute = fixture_root_surface(
        "#[serde(deny_unknown_fields)] pub struct Root { #[serde(default)] proof: u8 }",
        "Root",
    );
    let manual_deserialize = fixture_root_surface(
        "pub struct Root; impl<'de> Deserialize<'de> for Root { fn deserialize<D>(_: D) -> Result<Self, D::Error> { todo!() } }",
        "Root",
    );

    assert_eq!(derive_deserialize.deserialization_authorities, 1);
    assert_eq!(serde_attribute.serde_attributes, 2);
    assert_eq!(manual_deserialize.deserialization_authorities, 1);
}

#[test]
fn every_conversion_and_deref_trait_changes_the_structural_surface() {
    let mutations = [
        (
            "pub struct Root; struct Raw; impl From<Raw> for Root { fn from(_: Raw) -> Self { Root } }",
            "From",
        ),
        (
            "pub struct Root; struct Raw; impl TryFrom<Raw> for Root { type Error = (); fn try_from(_: Raw) -> Result<Self, Self::Error> { Ok(Root) } }",
            "TryFrom",
        ),
        (
            "pub struct Root; impl FromIterator<u8> for Root { fn from_iter<T: IntoIterator<Item = u8>>(_: T) -> Self { Root } }",
            "FromIterator",
        ),
    ];
    for (source, trait_name) in mutations {
        let surface = fixture_root_surface(source, "Root");
        assert_eq!(
            surface.conversion_constructors, 1,
            "{trait_name} must be counted as construction authority"
        );
    }

    let deref = fixture_root_surface(
        "pub struct Root(u8); impl Deref for Root { type Target = u8; fn deref(&self) -> &u8 { &self.0 } }",
        "Root",
    );
    assert_eq!(deref.deref, 1);
}

#[test]
fn every_root_producer_shape_is_enumerated_for_catalog_classification() {
    let surface = fixture_root_surface(
        r#"
            pub struct Root(u8);
            impl Root {
                pub fn direct() -> Self { Root(0) }
                pub fn named() -> Root { Root(0) }
                pub fn boxed() -> Box<Self> { Box::new(Root(0)) }
                pub fn nested() -> Option<std::sync::Arc<Self>> { None }
                pub fn opaque() -> impl Iterator<Item = Self> { std::iter::empty() }
                pub fn borrowed(&self) -> &Self { self }
            }
        "#,
        "Root",
    );
    assert_eq!(
        surface.public_root_producers,
        ["boxed", "direct", "named", "nested", "opaque"]
            .into_iter()
            .map(str::to_owned)
            .collect()
    );
}

#[test]
fn trait_constructor_routes_and_derive_routes_are_enumerated() {
    let surface = fixture_root_surface(
        r#"
            #[derive(Clone, serde::Deserialize)]
            pub struct Root(u8);
            impl From<u8> for Root { fn from(value: u8) -> Self { Root(value) } }
            impl std::str::FromStr for Root {
                type Err = ();
                fn from_str(_: &str) -> Result<Self, Self::Err> { Ok(Root(0)) }
            }
        "#,
        "Root",
    );
    assert_eq!(
        surface.derived_traits,
        ["Clone", "serde :: Deserialize"]
            .into_iter()
            .map(str::to_owned)
            .collect()
    );
    assert_eq!(surface.trait_impls.len(), 2);
    assert_eq!(surface.public_root_producers.len(), 2);
    assert!(
        surface
            .public_root_producers
            .iter()
            .any(|route| route.contains("From < u8 >") && route.ends_with("::from"))
    );
    assert!(
        surface
            .public_root_producers
            .iter()
            .any(|route| route.contains("FromStr") && route.ends_with("::from_str"))
    );
}

#[test]
fn mutable_consuming_and_semantic_check_routes_are_enumerated() {
    let surface = fixture_root_surface(
        r#"
            pub struct Root(u8);
            impl Root {
                pub fn exposed(&mut self) -> Option<&mut u8> { Some(&mut self.0) }
                pub fn into_raw(self) -> u8 { self.0 }
                pub fn validate_shape(&self) -> Result<(), ()> { Ok(()) }
            }
        "#,
        "Root",
    );
    assert_eq!(
        surface.public_mutable_projections,
        ["exposed"].into_iter().map(str::to_owned).collect()
    );
    assert_eq!(
        surface.public_consuming_extractions,
        ["into_raw"].into_iter().map(str::to_owned).collect()
    );
    assert_eq!(
        surface.public_semantic_checks,
        ["validate_shape"].into_iter().map(str::to_owned).collect()
    );
}

#[test]
fn public_mutable_receiver_methods_change_the_structural_surface() {
    let ordinary = fixture_root_surface(
        "pub struct Root; impl Root { pub fn insert_equation(&mut self, _: u8) {} }",
        "Root",
    );
    let typed = fixture_root_surface(
        "pub struct Root; impl Root { pub fn replace(self: &mut Self) {} }",
        "Root",
    );
    let trait_method = fixture_root_surface(
        "pub struct Root; pub trait Sink { fn insert(&mut self); } \
         impl Sink for Root { fn insert(&mut self) {} }",
        "Root",
    );
    assert_eq!(ordinary.public_mutators, 1);
    assert_eq!(typed.public_mutators, 1);
    assert_eq!(trait_method.public_mutators, 1);
}

#[test]
fn trait_default_method_is_an_exact_tombstone_owner() {
    let recreated = collect_fixture_owners(
        "fixture",
        "trait Recovery { fn retired_recovery() -> Option<u8> { None } }",
    );
    assert_eq!(
        present_tombstones(&["fixture::Recovery::retired_recovery"], &recreated),
        vec!["fixture::Recovery::retired_recovery"]
    );
}

#[test]
fn macro_tokens_are_checked_for_exact_tombstone_item_identifiers() {
    let tombstones = ["fixture::retired_recovery"];
    let hits = collect_fixture_macro_tombstone_hits(
        "fixture",
        "make_owner!(retired_recovery);",
        &tombstones,
    );
    let near_miss = collect_fixture_macro_tombstone_hits(
        "fixture",
        "make_owner!(retired_recovery_suffix);",
        &tombstones,
    );
    assert_eq!(hits.len(), 1);
    assert_eq!(hits[0].tombstone, tombstones[0]);
    assert!(near_miss.is_empty());
}

#[test]
fn distinct_trait_instantiations_have_distinct_owner_identities() {
    let inventory = collect_fixture_owner_tokens(
        "fixture",
        "struct Root; struct RawA; struct RawB; \
         impl From<RawA> for Root { fn from(_: RawA) -> Self { Root } } \
         impl From<RawB> for Root { fn from(_: RawB) -> Self { Root } }",
    );
    assert!(inventory.differences.is_empty());
    assert_eq!(inventory.tokens.len(), 2);
    assert!(
        inventory
            .tokens
            .contains_key("fixture::<Root as From<RawA>>::from")
    );
    assert!(
        inventory
            .tokens
            .contains_key("fixture::<Root as From<RawB>>::from")
    );
}

#[test]
fn duplicate_token_owner_identity_is_reported_without_panicking() {
    let inventory = collect_fixture_owner_tokens(
        "fixture",
        "fn recover() -> Option<u8> { None } fn recover() -> Option<u8> { None }",
    );
    assert_eq!(inventory.tokens.len(), 1);
    assert!(
        inventory
            .differences
            .iter()
            .any(|difference| difference.contains("construction authority is not exclusive"))
    );
}

#[test]
fn an_additional_constructor_authority_is_counted_not_collapsed() {
    let one = fixture_root_surface(
        "pub struct Root; impl Root { pub fn new() -> Self { Self } }",
        "Root",
    );
    let two = fixture_root_surface(
        "pub struct Root; impl Root { pub fn new() -> Self { Self } pub fn from_byte(_: u8) -> Self { Self } }",
        "Root",
    );
    assert_eq!(one.public_new, 1);
    assert_eq!(two.public_new, 1);
    assert_eq!(two.public_root_producers.len(), 2);
}

#[test]
fn whole_crate_inventory_catches_free_factories_aliases_wrappers_and_reexports() {
    let routes = fixture_route_inventory(
        r#"
            pub struct Root { value: u8 }
            pub type Alias = Root;
            pub struct Envelope { root: Alias }
            pub struct Factory;
            impl Factory {
                pub fn make() -> Option<Envelope> { None }
                pub fn mutate(callback: impl FnOnce(&mut Alias)) { let _ = callback; }
                pub fn constrained<F>(callback: F)
                where
                    F: FnOnce(&mut Root),
                {
                    let _ = callback;
                }
                pub fn raw() -> *mut Root { std::ptr::null_mut() }
            }
            pub trait RootRoutes {
                type Product: Into<Root>;
                const ROOT: Root;
            }
            impl RootRoutes for Factory {
                type Product = Root;
                const ROOT: Root = Root { value: 0 };
            }
            pub fn free() -> Result<Alias, ()> { Err(()) }
            pub fn tuple() -> (Root, u8) { (Root { value: 0 }, 0) }
            pub use Root as ExportedRoot;
        "#,
        "Root",
    );
    for fragment in [
        "<Factory>::<inherent>::make",
        "free",
        "tuple",
        "use:Root as ExportedRoot",
    ] {
        assert!(
            routes.iter().any(|route| route.identity.contains(fragment)),
            "whole-crate route `{fragment}` escaped: {routes:#?}"
        );
    }
    assert!(routes.iter().any(|route| {
        route.identity.contains("mutate") && route.kinds.contains(&RouteKind::MutatesRoot)
    }));
    assert!(routes.iter().any(|route| {
        route.identity.contains("constrained")
            && route.kinds.contains(&RouteKind::GenericConstraint)
            && route.kinds.contains(&RouteKind::MutatesRoot)
    }));
    assert!(routes.iter().any(|route| {
        route.identity.contains("raw") && route.kinds.contains(&RouteKind::MutableProjection)
    }));
    for associated in ["<type:Product>", "<const:ROOT>"] {
        assert!(
            routes
                .iter()
                .any(|route| route.identity.contains(associated)),
            "associated route `{associated}` escaped: {routes:#?}"
        );
    }
}

#[test]
fn mutable_trait_capabilities_are_exact_routes() {
    let routes = fixture_route_inventory(
        r#"
            pub struct Root(u8);
            impl AsMut<u8> for Root { fn as_mut(&mut self) -> &mut u8 { &mut self.0 } }
            impl std::borrow::BorrowMut<u8> for Root {
                fn borrow_mut(&mut self) -> &mut u8 { &mut self.0 }
            }
            impl std::ops::Index<usize> for Root {
                type Output = u8;
                fn index(&self, _: usize) -> &u8 { &self.0 }
            }
            impl std::ops::IndexMut<usize> for Root {
                fn index_mut(&mut self, _: usize) -> &mut u8 { &mut self.0 }
            }
        "#,
        "Root",
    );
    for capability in ["AsMut", "BorrowMut", "IndexMut"] {
        assert!(routes.iter().any(|route| {
            route.identity.contains(capability) && route.kinds.contains(&RouteKind::MutableTrait)
        }));
    }
}

#[test]
fn same_name_signature_and_same_count_mutator_swaps_change_the_exact_inventory() {
    let profile = fixture_route_inventory(
        "pub struct Root(u8); impl Root { pub fn empty(_: u8) -> Self { Root(0) } pub fn insert(&mut self) {} }",
        "Root",
    );
    let implicit = fixture_route_inventory(
        "pub struct Root(u8); impl Root { pub fn empty() -> Self { Root(0) } pub fn raw_mut(&mut self) -> &mut u8 { &mut self.0 } }",
        "Root",
    );
    assert_ne!(profile, implicit);
    assert_eq!(
        profile
            .iter()
            .filter(|route| route.kinds.contains(&RouteKind::MutatesRoot))
            .count(),
        implicit
            .iter()
            .filter(|route| route.kinds.contains(&RouteKind::MutatesRoot))
            .count(),
        "the exact route/signature ledger, not a count change, must catch the swap"
    );
}

#[test]
fn private_root_storage_and_same_count_wire_attribute_swaps_change_the_inventory() {
    let skipped = fixture_route_inventory(
        "#[derive(serde::Serialize)] pub struct Root { #[serde(skip)] hidden: u8 }",
        "Root",
    );
    let defaulted = fixture_route_inventory(
        "#[derive(serde::Serialize)] pub struct Root { #[serde(default)] hidden: u16 }",
        "Root",
    );
    assert_ne!(skipped, defaulted);
    assert_eq!(
        skipped
            .iter()
            .filter(|route| route.kinds.contains(&RouteKind::SerdeAttribute))
            .count(),
        defaulted
            .iter()
            .filter(|route| route.kinds.contains(&RouteKind::SerdeAttribute))
            .count(),
        "exact private storage/attribute signatures, not route counts, must catch the swap"
    );
}

#[test]
fn test_only_routes_and_self_associated_types_do_not_fabricate_production_routes() {
    let routes = fixture_route_inventory(
        r#"
            pub struct Root;
            impl Root {
                #[cfg(test)] pub fn forge() -> Self { Root }
            }
            pub trait Diagnostic { type Error; fn diagnostic() -> Self::Error; }
            impl Diagnostic for Root {
                type Error = ();
                fn diagnostic() -> Self::Error {}
            }
        "#,
        "Root",
    );
    assert!(!routes.iter().any(|route| route.identity.ends_with("forge")));
    assert!(
        routes
            .iter()
            .all(|route| !route.identity.ends_with("diagnostic")),
        "`Self::Error` is an associated type, not an owned root"
    );
}

#[test]
fn dae_shape_is_the_positive_opaque_constructor_control() {
    let surface = fixture_root_surface(
        "pub struct Dae { storage: u8 } impl Dae { pub fn construct() -> Result<Self, Error> { Ok(Self { storage: 0 }) } }",
        "Dae",
    );
    assert_eq!(
        surface,
        RootSurface {
            public_result_construct: 1,
            public_root_producers: ["construct".to_owned()].into_iter().collect(),
            ..RootSurface::default()
        }
    );
}
