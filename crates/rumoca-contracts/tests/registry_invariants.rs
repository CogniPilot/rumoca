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

/// SPEC_0022's summary tables restate the catalog: the §5 per-category
/// counts and total, the document summary total, and the section index
/// (per-category contract counts and each section's line range). They are
/// derived facts, so any drift from the catalog is caught here.
#[test]
fn spec_0022_summaries_match_the_catalog() {
    let catalog = std::fs::read_to_string(spec_0022_path())
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", spec_0022_path().display()));
    let mut by_prefix = std::collections::BTreeMap::<String, usize>::new();
    for id in catalog_contract_ids(&catalog) {
        *by_prefix
            .entry(split_contract_id(&id).0.to_string())
            .or_default() += 1;
    }
    let total: usize = by_prefix.values().sum();
    let mut drift = Vec::new();

    let summary = table_rows_after(&catalog, "## 5. Contract Summary by Category");
    for cells in &summary {
        let [_, prefix, count] = cells.as_slice() else {
            continue;
        };
        let count = count.trim_matches('*');
        if prefix.is_empty() {
            if count != total.to_string() {
                drift.push(format!("§5 total is {count}, catalog has {total}"));
            }
        } else if by_prefix.get(*prefix).map(usize::to_string).as_deref() != Some(count) {
            drift.push(format!(
                "§5 {prefix} count is {count}, catalog has {:?}",
                by_prefix.get(*prefix)
            ));
        }
    }
    let summary_prefixes = summary
        .iter()
        .filter_map(|cells| cells.get(1).filter(|prefix| !prefix.is_empty()))
        .map(|prefix| prefix.to_string())
        .collect::<BTreeSet<_>>();
    let catalog_prefixes = by_prefix.keys().cloned().collect::<BTreeSet<_>>();
    if summary_prefixes != catalog_prefixes {
        drift.push(format!(
            "§5 lists {summary_prefixes:?}, catalog has {catalog_prefixes:?}"
        ));
    }

    for cells in table_rows_after(&catalog, "## Document Summary") {
        if let ["Total Contracts", count] = cells.as_slice()
            && *count != total.to_string()
        {
            drift.push(format!(
                "document summary total is {count}, catalog has {total}"
            ));
        }
    }

    drift.extend(section_index_drift(&catalog, &by_prefix));
    assert!(
        drift.is_empty(),
        "SPEC_0022 summaries drift:\n{}",
        drift.join("\n")
    );
}

/// Rows (trimmed cells) of the first Markdown table after `heading`, header
/// and separator rows excluded.
fn table_rows_after<'a>(text: &'a str, heading: &str) -> Vec<Vec<&'a str>> {
    text.lines()
        .skip_while(|line| !line.starts_with(heading))
        .skip(1)
        .skip_while(|line| !line.starts_with('|'))
        .take_while(|line| line.starts_with('|'))
        .skip(2)
        .map(|line| {
            line.trim()
                .trim_matches('|')
                .split('|')
                .map(str::trim)
                .collect()
        })
        .collect()
}

/// The section number a `##`/`###` heading opens (`## 4. Contract Catalog`
/// is `4`, `### 4.3 Instantiation ...` is `4.3`).
fn heading_number(line: &str) -> Option<&str> {
    let rest = line
        .strip_prefix("## ")
        .or_else(|| line.strip_prefix("### "))?;
    let number = rest.split_whitespace().next()?.trim_end_matches('.');
    // Dot-separated decimal components, none empty.
    let well_formed = !number.is_empty()
        && number.chars().all(|c| c.is_ascii_digit() || c == '.')
        && !number.starts_with('.')
        && !number.ends_with('.')
        && !number.contains("..");
    well_formed.then_some(number)
}

fn section_index_drift(
    catalog: &str,
    by_prefix: &std::collections::BTreeMap<String, usize>,
) -> Vec<String> {
    let lines = catalog.lines().collect::<Vec<_>>();
    let mut headings = std::collections::BTreeMap::<&str, usize>::new();
    for (index, line) in lines.iter().enumerate() {
        if let Some(number) = heading_number(line) {
            headings.entry(number).or_insert(index + 1);
        }
    }
    let rows = table_rows_after(catalog, "### Section Index");
    let numbers = rows
        .iter()
        .map(|cells| {
            cells[0]
                .trim_start_matches('§')
                .split_whitespace()
                .next()
                .unwrap_or_default()
                .trim_end_matches('.')
        })
        .collect::<Vec<_>>();
    // A section starts at its heading; the first numbered child of an
    // unindexed parent (`§4.1` under `## 4. Contract Catalog`) also owns the
    // parent heading.
    let starts = numbers
        .iter()
        .map(|number| {
            let parent = number
                .strip_suffix(".1")
                .filter(|parent| !numbers.contains(parent));
            parent
                .and_then(|parent| headings.get(parent))
                .or_else(|| headings.get(number))
                .copied()
        })
        .collect::<Vec<_>>();
    let mut drift = Vec::new();
    for (row, cells) in rows.iter().enumerate() {
        let Some(start) = starts[row] else {
            drift.push(format!("section index row {} names no heading", cells[0]));
            continue;
        };
        let end = match starts.get(row + 1) {
            Some(Some(next)) => next - 1,
            Some(None) => continue,
            None => lines
                .iter()
                .enumerate()
                .skip(start)
                .find(|(_, line)| line.starts_with("## "))
                .map_or(lines.len(), |(index, _)| index),
        };
        let expected = format!("{start}–{end}");
        if cells.get(1) != Some(&expected.as_str()) {
            drift.push(format!(
                "section index {} lines are {:?}, expected {expected}",
                cells[0],
                cells.get(1)
            ));
        }
        let prefix = cells[0].split_whitespace().nth(1).unwrap_or_default();
        if let Some(count) = by_prefix.get(prefix) {
            let stated = cells
                .get(2)
                .and_then(|content| content.split_once('('))
                .and_then(|(_, rest)| rest.split_once(" contracts)"))
                .map(|(count, _)| count);
            if stated != Some(count.to_string().as_str()) {
                drift.push(format!(
                    "section index {} states {stated:?} contracts, catalog has {count}",
                    cells[0]
                ));
            }
        }
    }
    drift
}
