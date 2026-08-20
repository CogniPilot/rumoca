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

/// The SPEC_0022 catalog is the source of truth for which contracts exist;
/// `data/contracts.toml` is its machine-readable mirror. Adding a catalog row
/// without a registry row (or the reverse) is drift, so pin set equality here
/// rather than only the per-category counts.
#[test]
fn registry_ids_match_spec_0022_catalog() {
    let catalog = std::fs::read_to_string(spec_0022_path())
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", spec_0022_path().display()));
    let catalog_ids = catalog_contract_ids(&catalog);
    assert!(
        !catalog_ids.is_empty(),
        "SPEC_0022 catalog parsed to zero contract rows; the table format changed"
    );

    let registry_ids: BTreeSet<String> = create_registry()
        .all()
        .map(|contract| contract.id.to_string())
        .collect();

    let missing_in_registry: Vec<&String> = catalog_ids.difference(&registry_ids).collect();
    let missing_in_catalog: Vec<&String> = registry_ids.difference(&catalog_ids).collect();
    assert!(
        missing_in_registry.is_empty() && missing_in_catalog.is_empty(),
        "SPEC_0022 catalog and contract registry disagree; \
         in catalog only: {missing_in_registry:?}, in registry only: {missing_in_catalog:?}"
    );
}

fn spec_0022_path() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md")
}

/// Collect the leading cell of every catalog row that is shaped like a
/// contract ID (`| XXX-NNN | ... |`), ignoring the spec's other tables.
fn catalog_contract_ids(catalog: &str) -> BTreeSet<String> {
    catalog
        .lines()
        .filter_map(|line| {
            let (cell, _) = line.strip_prefix('|')?.split_once('|')?;
            contract_id_shaped(cell.trim()).map(str::to_string)
        })
        .collect()
}

fn contract_id_shaped(cell: &str) -> Option<&str> {
    let (prefix, digits) = cell.split_once('-')?;
    let shaped = !prefix.is_empty()
        && prefix.chars().all(|c| c.is_ascii_uppercase())
        && digits.len() == 3
        && digits.chars().all(|c| c.is_ascii_digit());
    shaped.then_some(cell)
}
