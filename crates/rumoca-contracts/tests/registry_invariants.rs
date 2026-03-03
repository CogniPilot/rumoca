use rumoca_contracts::{IMPLEMENTED_CONTRACT_IDS, create_registry};
use std::collections::BTreeSet;

fn split_contract_id(id: &str) -> (&str, &str) {
    id.split_once('-')
        .unwrap_or_else(|| panic!("invalid contract id format: {id}"))
}

#[test]
fn registry_has_unique_well_formed_ids() {
    let registry = create_registry();
    let mut seen = BTreeSet::new();

    for contract in registry.all() {
        let id = contract.id.to_string();
        assert!(seen.insert(id.clone()), "duplicate contract id: {id}");

        let (prefix, digits) = split_contract_id(&id);
        assert!(
            !prefix.is_empty() && prefix.chars().all(|c| c.is_ascii_uppercase()),
            "invalid contract prefix: {id}"
        );
        assert!(
            digits.len() == 3 && digits.chars().all(|c| c.is_ascii_digit()),
            "invalid contract numeric suffix: {id}"
        );
    }

    assert_eq!(seen.len(), registry.len());
}

#[test]
fn id_prefix_matches_category_prefix() {
    let registry = create_registry();

    for contract in registry.all() {
        let id = contract.id.to_string();
        let (prefix, _) = split_contract_id(&id);
        assert_eq!(
            prefix,
            contract.category.prefix(),
            "id/category prefix mismatch for {}",
            id
        );
    }
}

#[test]
fn metadata_is_non_empty_and_tier_in_range() {
    let registry = create_registry();

    for contract in registry.all() {
        assert!(
            !contract.name.trim().is_empty(),
            "empty name for {}",
            contract.id
        );
        assert!(
            !contract.mls_ref.trim().is_empty(),
            "empty mls_ref for {}",
            contract.id
        );
        assert!(
            !contract.requirement.trim().is_empty(),
            "empty requirement for {}",
            contract.id
        );
        assert!(
            (1..=3).contains(&contract.tier),
            "tier out of range for {}: {}",
            contract.id,
            contract.tier
        );
    }
}

#[test]
fn implemented_id_list_is_unique_and_exists_in_registry() {
    let registry = create_registry();
    let mut seen = BTreeSet::new();

    for id in IMPLEMENTED_CONTRACT_IDS {
        assert!(
            seen.insert((*id).to_string()),
            "duplicate IMPLEMENTED id: {id}"
        );
        assert!(
            registry.get(id).is_some(),
            "IMPLEMENTED id missing in registry: {id}"
        );
    }
}
