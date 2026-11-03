//! Exact warning inventory comparison for the native fixture census.
//!
//! Callers use the collector's existing warning predicate. This comparison
//! never changes its floor or turns a warning into a scaling proof.

#[derive(Debug)]
pub(crate) struct Inventory<'a> {
    pub complete: bool,
    pub emitted_entries: usize,
    pub unattributed_copies: u64,
    pub warnings: &'a [String],
}

pub(crate) fn check(fixture: &str, actual: Inventory<'_>) -> Result<(), String> {
    let expected = expected(include_str!("known-warnings.tsv"), fixture)?;
    compare(&expected, actual)
}

fn expected<'a>(manifest: &'a str, fixture: &str) -> Result<Vec<&'a str>, String> {
    let mut names = std::collections::BTreeSet::new();
    let mut selected = None;
    for row in manifest.lines().filter(|row| !row.starts_with('#')) {
        let Some((name, keys)) = row.split_once('\t') else {
            return Err(format!("malformed warning manifest row: {row:?}"));
        };
        if name.is_empty() || keys.contains('\t') || !names.insert(name) {
            return Err(format!("invalid or duplicate fixture: {name:?}"));
        }
        let keys: Vec<_> = if keys == "-" {
            Vec::new()
        } else {
            keys.split(',').collect()
        };
        let unique: std::collections::BTreeSet<_> = keys.iter().copied().collect();
        if unique.contains("") || unique.len() != keys.len() {
            return Err(format!("empty or duplicate warning key: {name:?}"));
        }
        if name == fixture {
            selected = Some(keys);
        }
    }
    selected.ok_or_else(|| format!("unmeasured fixture {fixture:?}; review its raw census first"))
}

pub(crate) fn compare(expected: &[&str], actual: Inventory<'_>) -> Result<(), String> {
    if !actual.complete || actual.emitted_entries == 0 || actual.unattributed_copies != 0 {
        return Err(format!("missing or incomplete census: {actual:?}"));
    }
    let mut expected = expected.to_vec();
    expected.sort_unstable();
    let mut observed: Vec<_> = actual.warnings.iter().map(String::as_str).collect();
    observed.sort_unstable();
    if observed != expected {
        return Err(format!(
            "unforced warning inventory changed; review raw source-bound entries\n\
             expected: {expected:#?}\nobserved: {observed:#?}"
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{Inventory, check, compare, expected};

    fn inventory(warnings: &[String]) -> Inventory<'_> {
        Inventory {
            complete: true,
            emitted_entries: 1,
            unattributed_copies: 0,
            warnings,
        }
    }

    #[test]
    fn an_observed_census_with_no_warnings_is_distinct_from_missing_collection() {
        assert!(compare(&[], inventory(&[])).is_ok());
        let mut missing = inventory(&[]);
        missing.emitted_entries = 0;
        assert!(compare(&[], missing).is_err());
    }

    #[test]
    fn incomplete_or_unattributed_reports_cannot_be_clean_passes() {
        let mut partial = inventory(&[]);
        partial.complete = false;
        assert!(compare(&[], partial).is_err());
        let mut unattributed = inventory(&[]);
        unattributed.unattributed_copies = 1;
        assert!(compare(&[], unattributed).is_err());
    }

    #[test]
    fn a_new_site_and_a_duplicate_existing_site_are_both_rejected() {
        let existing = "definition/typed/ConstructAggregate";
        let new = "definition/row/StoreOutputRange";
        assert!(compare(&[existing], inventory(&[existing.into(), new.into()])).is_err());
        assert!(compare(&[existing], inventory(&[existing.into(), existing.into()])).is_err());
    }

    #[test]
    fn a_removed_warning_requires_removing_its_manifest_entry() {
        assert!(compare(&["definition/typed/Map"], inventory(&[])).is_err());
    }

    #[test]
    fn ordering_does_not_hide_inventory_changes() {
        let observed = ["b".into(), "a".into()];
        assert!(compare(&["a", "b"], inventory(&observed)).is_ok());
        assert!(compare(&["a", "c"], inventory(&observed)).is_err());
    }

    #[test]
    fn unknown_fixtures_and_malformed_manifests_fail_closed() {
        assert!(expected("known\t-\n", "new").is_err());
        for manifest in [
            "known",
            "known\t\n",
            "known\t-\nknown\t-\n",
            "known\ta,a\n",
            "known\ta,\n",
        ] {
            assert!(expected(manifest, "known").is_err());
        }
    }

    #[test]
    fn measured_empty_and_nonempty_rows_use_the_same_comparison() {
        assert!(check("literal-16", inventory(&[])).is_ok());
        assert!(check("literal-17", inventory(&[])).is_err());
        let keys = expected(include_str!("known-warnings.tsv"), "literal-17").unwrap();
        let observed: Vec<_> = keys.iter().map(|key| (*key).to_owned()).collect();
        assert!(check("literal-17", inventory(&observed)).is_ok());
        assert!(check("unknown-fixture", inventory(&observed)).is_err());
    }
}
