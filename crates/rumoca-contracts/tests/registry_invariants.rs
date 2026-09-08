//! Registry invariants, and the one parser for the SPEC_0022 section 4 catalog.

use rumoca_contracts::create_registry;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// The `(prefix, ordinal)` of a well-formed contract ID.
///
/// This is the only ID rule in this file: the registry checks and the catalog
/// parser both come here, so "PREFIX-NNN" cannot mean two things at once.
fn contract_id_parts(cell: &str) -> Option<(&str, u16)> {
    let (prefix, digits) = cell.split_once('-')?;
    if prefix.is_empty() || !prefix.bytes().all(|byte| byte.is_ascii_uppercase()) {
        return None;
    }
    if digits.len() != 3 || !digits.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    let ordinal = digits.parse::<u16>().ok()?;
    Some((prefix, ordinal))
}

#[test]
fn registry_has_unique_well_formed_ids() {
    let registry = create_registry();
    let mut seen = BTreeSet::new();

    for contract in registry.all() {
        let id = contract.id.to_string();
        assert!(seen.insert(id.clone()), "duplicate contract id: {id}");
        assert!(
            contract_id_parts(&id).is_some(),
            "malformed contract id: {id}"
        );
    }

    assert_eq!(seen.len(), registry.len());
}

#[test]
fn id_prefix_matches_category_prefix() {
    let registry = create_registry();

    for contract in registry.all() {
        let id = contract.id.to_string();
        let Some((prefix, _)) = contract_id_parts(&id) else {
            panic!("malformed contract id: {id}");
        };
        assert_eq!(
            prefix,
            contract.category.prefix(),
            "id/category prefix mismatch for {id}"
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

/// Exact-symbol resurrection guard for one retired constant, and nothing wider.
///
/// `Implemented` membership is a field on `Contract`, so the single screaming-
/// snake-case constant that used to hold it in parallel must not come back.
/// This scans this crate's `.md`, `.rs`, and `.toml` files for that one literal
/// spelling, which `retired_symbol` below assembles from fragments; the prose
/// here must never write it contiguously either, because this file is inside
/// the scanned tree and would then report itself.
///
/// What this deliberately does not prove: it says nothing about a parallel
/// implemented-ID authority under a different name, one materialised by a build
/// script or code generator into bytes this walk never reads, or one living
/// outside this crate. Spelling is not identity, so no length of needle list
/// could prove that absence, and none is kept here; a longer blacklist would
/// restate the same non-proof with more needles.
#[test]
fn retired_implemented_contract_ids_symbol_is_not_resurrected() {
    let retired_symbol = ["IMPLEMENTED_CONTRACT_", "IDS"].concat();
    let mut offenders = Vec::new();
    find_text_in_source_tree(
        Path::new(env!("CARGO_MANIFEST_DIR")),
        &retired_symbol,
        &mut offenders,
    );
    assert!(
        offenders.is_empty(),
        "Implemented membership belongs to Contract.status; do not restore {retired_symbol}: {offenders:?}"
    );
}

fn find_text_in_source_tree(root: &Path, needle: &str, offenders: &mut Vec<String>) {
    let entries = std::fs::read_dir(root)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", root.display()));
    for entry in entries {
        let path = entry.expect("source-tree entry must be readable").path();
        if path.is_dir() {
            find_text_in_source_tree(&path, needle, offenders);
        } else if matches!(
            path.extension().and_then(|extension| extension.to_str()),
            Some("md" | "rs" | "toml")
        ) {
            let source = std::fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("failed to read {}: {error}", path.display()));
            if source.contains(needle) {
                offenders.push(path.display().to_string());
            }
        }
    }
}

/// Set equality between the SPEC_0022 section 4 catalog and the registry.
///
/// The authority direction is written down once, in the header of
/// `crates/rumoca-contracts/data/contracts.toml`; this test enforces the ID
/// half of it. Adding a catalog row without a registry row, or the reverse, is
/// drift, so pin set equality here rather than only the per-category counts.
///
/// What this does not do: it compares IDs and nothing else. The category, name,
/// MLS reference, and requirement are parsed and checked non-empty, but their
/// equality with the registry is not gated anywhere yet.
#[test]
fn registry_ids_match_spec_0022_catalog() {
    let catalog_ids = CatalogRows::parse(&catalog_document()).ids();
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

/// The section index and summary are navigation/progress views of the contract
/// rows, never independent count authorities. Pin both views to counts derived
/// from the catalog IDs so adding or removing a row requires no hand-maintained
/// test constants and stale prose fails immediately.
#[test]
fn spec_0022_count_views_match_catalog_rows() {
    let catalog = catalog_document();
    let catalog_counts = CatalogRows::parse(&catalog).counts();
    assert!(!catalog_counts.is_empty(), "SPEC_0022 catalog has no rows");
    let section_index_counts = section_index_contract_counts(&catalog);
    let (summary_counts, summary_total) = summary_contract_counts(&catalog);

    assert_eq!(
        section_index_counts, catalog_counts,
        "SPEC_0022 section-index counts must be derived from its catalog rows"
    );
    assert_eq!(
        summary_counts, catalog_counts,
        "SPEC_0022 summary counts must be derived from its catalog rows"
    );
    assert_eq!(
        summary_total,
        catalog_counts.values().sum::<usize>(),
        "SPEC_0022 summary total must equal its catalog-row count"
    );
}

/// Heading that opens the contract catalog. Anchored with its surrounding
/// newlines so it matches the heading line and not a mention of it in prose.
const CATALOG_SECTION_OPEN: &str = "\n## 4. Contract Catalog\n";

/// Heading that closes the contract catalog. The summary section immediately
/// follows it, and the summary's own table is a view, never a catalog row.
const CATALOG_SECTION_CLOSE: &str = "\n## 5. Contract Summary by Category\n";

/// The exact header row that opens every section 4 category table.
const CATALOG_HEADER_ROW: &str = "| ID | Contract | MLS | Requirement |";

/// The exact separator row that follows every such header.
const CATALOG_SEPARATOR_ROW: &str = "|----|----------|-----|-------------|";

/// The bytes between the catalog heading and the summary heading.
///
/// Scoping is what makes "a contract row" mean "a row section 4 declares". A
/// document-wide scan cannot tell a declaration from a contract-shaped row that
/// drifted into another table, so under it a moved row is invisible: the ID is
/// still found, just not where the catalog defines it. Slicing first makes that
/// move a deletion, which the registry-equality and count views both observe.
fn catalog_section(catalog: &str) -> &str {
    let (_, tail) = catalog
        .split_once(CATALOG_SECTION_OPEN)
        .expect("SPEC_0022 catalog section heading changed");
    let (section, _) = tail
        .split_once(CATALOG_SECTION_CLOSE)
        .expect("SPEC_0022 catalog section end heading changed");
    section
}

/// Classification of a heading that may change the active catalog category.
enum CatalogSubsectionHeading<'line> {
    Category(&'line str),
    NestedNarrative,
    MalformedCategory,
    Other,
}

/// Classify a section-4 heading without conflating malformed and nested forms.
///
/// `### 4.16.1 ...` is nested narrative, so it retains the enclosing category.
/// A bare `### 4.N ...` heading instead declares a category and must end in a
/// non-empty uppercase-ASCII prefix. A malformed bare category is reported
/// separately so the parser cannot let its rows inherit the previous prefix.
fn classify_catalog_subsection_heading(line: &str) -> CatalogSubsectionHeading<'_> {
    let Some(rest) = line.strip_prefix("### 4.") else {
        return CatalogSubsectionHeading::Other;
    };
    let (ordinal, title) = rest
        .split_once(' ')
        .map_or((rest, None), |(ordinal, title)| (ordinal, Some(title)));
    let numeric_parts = ordinal
        .split('.')
        .all(|part| !part.is_empty() && part.bytes().all(|byte| byte.is_ascii_digit()));
    if ordinal.contains('.') && numeric_parts {
        return CatalogSubsectionHeading::NestedNarrative;
    }
    if ordinal.is_empty() || !ordinal.bytes().all(|byte| byte.is_ascii_digit()) {
        return CatalogSubsectionHeading::Other;
    }
    let Some((_, prefix)) = title
        .and_then(|title| title.strip_suffix(')'))
        .and_then(|title| title.rsplit_once('('))
    else {
        return CatalogSubsectionHeading::MalformedCategory;
    };
    if prefix.is_empty() || !prefix.bytes().all(|byte| byte.is_ascii_uppercase()) {
        return CatalogSubsectionHeading::MalformedCategory;
    }
    CatalogSubsectionHeading::Category(prefix)
}

/// The four cells of one strict catalog row: ID, name, MLS reference, and
/// requirement, in that column order.
///
/// The row must open with `|`, *close* with `|`, hold exactly four cells
/// between them, and leave none of them empty after trimming. Requiring the
/// terminating delimiter is what makes a truncated row a refusal: a rule that
/// needs only a second `|` accepts `| LEX-013 | name | ref | text` unchanged,
/// and a rule that reads only the first cell accepts `| LEX-001 | | | |`.
///
/// Nothing here rewrites a cell. A row that does not match is refused by the
/// caller, never trimmed, padded, or repaired into one that does.
fn strict_row_cells(line: &str) -> Option<[&str; 4]> {
    let mut cells = line.strip_prefix('|')?.strip_suffix('|')?.split('|');
    let id = cells.next()?.trim();
    let name = cells.next()?.trim();
    let mls_ref = cells.next()?.trim();
    let requirement = cells.next()?.trim();
    if cells.next().is_some() {
        return None;
    }
    if id.is_empty() || name.is_empty() || mls_ref.is_empty() || requirement.is_empty() {
        return None;
    }
    Some([id, name, mls_ref, requirement])
}

/// One section 4 catalog row, with the five semantic values the catalog owns.
///
/// `category_prefix` comes from the enclosing `### 4.N ... (PREFIX)` heading
/// rather than from the row, which is what makes "this row is in the wrong
/// subsection" expressible at all.
struct CatalogRow {
    id: String,
    category_prefix: String,
    name: String,
    mls_ref: String,
    requirement: String,
}

/// The parsed section 4 catalog, as one value.
///
/// Every catalog reader in the workspace goes through [`CatalogRows::parse`].
/// That is a consequence of deleting
/// `crates/rumoca/tests/suite_gates/mls_formalization_manifest_test.rs`, which
/// carried a second, document-wide row rule that agreed with this one on the
/// live document only by coincidence; it is not a property this type could
/// establish on its own. A future reader that re-derived its own rule would
/// make the claim false again and nothing here would notice.
struct CatalogRows {
    /// Every row in document order. Duplicate IDs are refused during parsing,
    /// so this vector and the derived ID set always have the same length; the
    /// vector is the shape that keeps document order and all five cells.
    rows: Vec<CatalogRow>,
}

impl CatalogRows {
    /// Parse a SPEC_0022-shaped document into its section 4 catalog rows.
    ///
    /// Every line inside section 4 that begins with `|` is either the exact
    /// header row, the exact separator row, or a candidate contract row. A
    /// candidate that is not a strict full row, whose ID is malformed, whose
    /// prefix disagrees with its enclosing subsection, whose ID repeats, or
    /// whose ordinal does not increase inside its prefix, is refused by name.
    /// A malformed bare category heading is refused before any following row
    /// can inherit the preceding category; nested narrative headings retain it.
    /// Rows outside section 4 are not catalog rows at all, so a row that drifts
    /// into another table is a deletion here, not a silent survivor.
    fn parse(catalog: &str) -> Self {
        let mut rows: Vec<CatalogRow> = Vec::new();
        let mut ids: BTreeSet<String> = BTreeSet::new();
        let mut last_ordinal: BTreeMap<String, u16> = BTreeMap::new();
        let mut category: Option<&str> = None;

        for line in catalog_section(catalog).lines() {
            match classify_catalog_subsection_heading(line) {
                CatalogSubsectionHeading::Category(prefix) => {
                    category = Some(prefix);
                    continue;
                }
                CatalogSubsectionHeading::MalformedCategory => {
                    panic!("malformed SPEC_0022 category subsection heading: {line}");
                }
                CatalogSubsectionHeading::NestedNarrative | CatalogSubsectionHeading::Other => {}
            }
            if !line.starts_with('|') || line == CATALOG_HEADER_ROW {
                continue;
            }
            if line == CATALOG_SEPARATOR_ROW {
                continue;
            }
            let Some(enclosing) = category else {
                panic!("SPEC_0022 catalog row outside any category subsection: {line}");
            };
            let Some([id, name, mls_ref, requirement]) = strict_row_cells(line) else {
                panic!("malformed SPEC_0022 catalog row: {line}");
            };
            let Some((prefix, ordinal)) = contract_id_parts(id) else {
                panic!("malformed SPEC_0022 contract id: {line}");
            };
            assert!(
                prefix == enclosing,
                "SPEC_0022 row {id} is declared under the ({enclosing}) subsection"
            );
            assert!(
                ids.insert(id.to_string()),
                "duplicate SPEC_0022 catalog row for {id}"
            );
            if let Some(previous) = last_ordinal.insert(prefix.to_string(), ordinal) {
                assert!(
                    ordinal > previous,
                    "SPEC_0022 ordinals must increase within {prefix}: {id} follows {previous}"
                );
            }
            rows.push(CatalogRow {
                id: id.to_string(),
                category_prefix: prefix.to_string(),
                name: name.to_string(),
                mls_ref: mls_ref.to_string(),
                requirement: requirement.to_string(),
            });
        }

        Self { rows }
    }

    /// The catalog IDs as a set, proven duplicate-free by [`Self::parse`].
    fn ids(&self) -> BTreeSet<String> {
        self.rows.iter().map(|row| row.id.clone()).collect()
    }

    /// Per-category row counts, keyed by the enclosing subsection prefix.
    fn counts(&self) -> BTreeMap<String, usize> {
        let mut counts = BTreeMap::new();
        for row in &self.rows {
            *counts.entry(row.category_prefix.clone()).or_insert(0) += 1;
        }
        counts
    }
}

/// Build a minimal SPEC_0022-shaped document in memory.
///
/// Each `(prefix, rows)` entry emits a `### 4.N ... (PREFIX)` heading followed
/// by the *exact* header and separator rows the real document uses, then its
/// rows verbatim. Reusing the real header and separator is what keeps a fixture
/// from proving something about a table shape production never meets.
/// `trailing_rows` land after `## 5. Contract Summary by Category`, which is
/// the only way to put a contract-shaped row on the far side of the section 4
/// boundary. Fixtures are strings rather than edits to the real spec, so these
/// witnesses are hermetic and cannot leave the checked-in document dirty.
fn catalog_fixture(subsections: &[(&str, &[&str])], trailing_rows: &[&str]) -> String {
    let mut doc = String::from("# Fixture\n\n## 4. Contract Catalog\n\n");
    for (index, (prefix, rows)) in subsections.iter().enumerate() {
        let ordinal = index + 1;
        doc.push_str(&format!("### 4.{ordinal} Fixture Contracts ({prefix})\n\n"));
        doc.push_str(CATALOG_HEADER_ROW);
        doc.push('\n');
        doc.push_str(CATALOG_SEPARATOR_ROW);
        doc.push('\n');
        for row in *rows {
            doc.push_str(row);
            doc.push('\n');
        }
        doc.push('\n');
    }
    doc.push_str("## 5. Contract Summary by Category\n\n");
    for row in trailing_rows {
        doc.push_str(row);
        doc.push('\n');
    }
    doc
}

/// Control for the mutant witnesses below.
///
/// It pins that the fixture shape parses the way the real document does, so a
/// failure in any mutant is a failure on the mutation and not on a fixture the
/// parser never understood. Without this, a fixture typo that silenced every
/// row would make the moved-row witness pass for the wrong reason.
#[test]
fn catalog_fixture_parses_its_section_four_rows() {
    let lex: &[&str] = &[
        "| LEX-001 | first | S2 | text one |",
        "| LEX-002 | second | S2 | text two |",
    ];
    let arr: &[&str] = &["| ARR-001 | third | S10 | text three |"];
    let trailing = ["| **Parsing** | source | tree |"];
    let subsections = [("LEX", lex), ("ARR", arr)];
    let parsed = CatalogRows::parse(&catalog_fixture(&subsections, &trailing));

    let ids: Vec<&str> = parsed.rows.iter().map(|row| row.id.as_str()).collect();
    assert_eq!(ids, ["LEX-001", "LEX-002", "ARR-001"]);
    assert_eq!(parsed.counts().get("LEX"), Some(&2));
    assert_eq!(parsed.counts().get("ARR"), Some(&1));
    assert_eq!(parsed.ids().len(), 3);
}

/// Mutant witness: each cell binds to its own field, in column order.
///
/// The mutation this fails against is any permutation of the cell-to-field
/// binding, including the requirement inversion that swaps the MLS reference
/// with the requirement: the four sentinels are pairwise distinct, so no
/// permutation leaves all four assertions true. It also fails against the
/// first-cell-only rule this parser replaced, under which `name`, `mls_ref`,
/// and `requirement` are never populated at all.
#[test]
fn catalog_row_cells_bind_to_their_own_fields() {
    let rows: &[&str] = &["| LEX-001 | cell-name | cell-mls | cell-requirement |"];
    let parsed = CatalogRows::parse(&catalog_fixture(&[("LEX", rows)], &[]));

    let row = &parsed.rows[0];
    assert_eq!(row.id, "LEX-001");
    assert_eq!(row.category_prefix, "LEX");
    assert_eq!(row.name, "cell-name");
    assert_eq!(row.mls_ref, "cell-mls");
    assert_eq!(row.requirement, "cell-requirement");
}

/// Mutant witness: all four semantic cells must be present and non-empty.
///
/// This calls the row predicate production calls. The accepted control comes
/// first so an empty-cell refusal cannot pass because the shape was rejected
/// for an unrelated reason. Each mutant empties exactly one column, so a
/// predicate that checked only some of them fails on the columns it skipped;
/// the pre-repair rule, which observed the ID cell alone, fails on three of the
/// four.
#[test]
fn every_semantic_cell_must_be_non_empty() {
    let accepted = "| LEX-001 | name | S2 | text |";
    assert!(strict_row_cells(accepted).is_some());
    for mutant in [
        "|  | name | S2 | text |",
        "| LEX-001 |  | S2 | text |",
        "| LEX-001 | name |  | text |",
        "| LEX-001 | name | S2 |  |",
    ] {
        assert!(
            strict_row_cells(mutant).is_none(),
            "an empty cell must refuse the row: {mutant}"
        );
    }
}

/// Mutant witness: a row must close with its delimiter and hold four cells.
///
/// The first mutant is the live defect this cut repaired: twelve SPEC_0022 rows
/// carried no terminating `|`. The rule they passed needed only a *second*
/// pipe, so they parsed rather than being discarded, which is why repairing the
/// document and tightening the predicate had to land together. The second
/// mutant fails against a predicate that stops after four cells and ignores the
/// rest of the line.
#[test]
fn a_row_must_close_with_its_delimiter_and_hold_four_cells() {
    let closed = "| LEX-013 | escapes | S2.4.4 | text |";
    let truncated = "| LEX-013 | escapes | S2.4.4 | text";
    let five_cells = "| LEX-013 | escapes | S2.4.4 | text | extra |";
    assert!(strict_row_cells(closed).is_some());
    assert!(
        strict_row_cells(truncated).is_none(),
        "a row without its terminating delimiter must be refused"
    );
    assert!(
        strict_row_cells(five_cells).is_none(),
        "a row with a fifth cell must be refused"
    );
}

/// Mutant witness: a malformed contract ID is refused, not skipped.
///
/// A parser that filtered non-ID rows out instead of refusing them would drop
/// this row and report a catalog one row shorter, which the registry-equality
/// and count views would then have to catch indirectly. Refusing here names the
/// offending line.
#[test]
#[should_panic(expected = "malformed SPEC_0022 contract id")]
fn malformed_contract_id_is_refused() {
    let rows: &[&str] = &["| LEX-1 | name | S2 | text |"];
    let _ = CatalogRows::parse(&catalog_fixture(&[("LEX", rows)], &[]));
}

/// Mutant witness: a row's prefix must match its enclosing subsection.
///
/// The mutation is a row filed under the wrong category heading. Nothing in the
/// row itself is malformed, so only the binding to the enclosing
/// `### 4.N ... (PREFIX)` heading can reject it; a parser that read the prefix
/// out of the ID alone would accept it and mis-attribute its count.
#[test]
#[should_panic(expected = "SPEC_0022 row ARR-001 is declared under the (LEX) subsection")]
fn row_under_the_wrong_subsection_is_refused() {
    let rows: &[&str] = &[
        "| LEX-001 | first | S2 | text |",
        "| ARR-001 | second | S10 | text |",
    ];
    let _ = CatalogRows::parse(&catalog_fixture(&[("LEX", rows)], &[]));
}

/// Mutant witness: a malformed bare category heading cannot inherit the
/// preceding category, even when the following row repeats that prefix.
///
/// Lowercasing only `(LEX)` to `(lex)` leaves both IDs and all derived counts
/// unchanged. The heading itself must therefore fail closed; otherwise the
/// second row silently remains in the first valid LEX subsection.
#[test]
#[should_panic(
    expected = "malformed SPEC_0022 category subsection heading: ### 4.2 Fixture Contracts (lex)"
)]
fn malformed_bare_category_heading_cannot_inherit_previous_prefix() {
    let catalog = format!(
        "# Fixture\n\n## 4. Contract Catalog\n\n\
         ### 4.1 Fixture Contracts (LEX)\n\n{CATALOG_HEADER_ROW}\n{CATALOG_SEPARATOR_ROW}\n\
         | LEX-001 | first | S2 | text one |\n\n\
         ### 4.2 Fixture Contracts (lex)\n\n{CATALOG_HEADER_ROW}\n{CATALOG_SEPARATOR_ROW}\n\
         | LEX-002 | second | S2 | text two |\n\n\
         ## 5. Contract Summary by Category\n"
    );
    let _ = CatalogRows::parse(&catalog);
}

/// A nested section-4 narrative heading does not end its category's table.
#[test]
fn nested_narrative_heading_retains_enclosing_category() {
    let catalog = format!(
        "# Fixture\n\n## 4. Contract Catalog\n\n\
         ### 4.1 Fixture Contracts (LEX)\n\n{CATALOG_HEADER_ROW}\n{CATALOG_SEPARATOR_ROW}\n\
         | LEX-001 | first | S2 | text one |\n\n\
         ### 4.1.1 Narrative Scope\n\n\
         | LEX-002 | second | S2 | text two |\n\n\
         ## 5. Contract Summary by Category\n"
    );
    let parsed = CatalogRows::parse(&catalog);
    let ids: Vec<&str> = parsed.rows.iter().map(|row| row.id.as_str()).collect();
    assert_eq!(ids, ["LEX-001", "LEX-002"]);
    assert_eq!(parsed.counts().get("LEX"), Some(&2));
}

/// Mutant witness: a duplicated catalog row must be rejected by ID.
///
/// The live SPEC_0022 contains no duplicate row, so nothing in the checked-in
/// document exercises this branch; without this test the duplicate rejection is
/// only incidentally green and could be reverted to a set-collecting parse
/// unnoticed. The expectation names `LEX-002` rather than merely demanding a
/// panic, because a rejection that cannot say which row is duplicated does not
/// discharge the requirement.
#[test]
#[should_panic(expected = "duplicate SPEC_0022 catalog row for LEX-002")]
fn duplicate_catalog_row_is_rejected_and_named() {
    let rows: &[&str] = &[
        "| LEX-001 | first | S2 | text |",
        "| LEX-002 | second | S2 | text |",
        "| LEX-002 | second again | S2 | text |",
    ];
    let _ = CatalogRows::parse(&catalog_fixture(&[("LEX", rows)], &[]));
}

/// Mutant witness: ordinals must increase inside a prefix.
///
/// The mutation is a reordered or reinserted row: `LEX-001` after `LEX-002` is
/// neither a duplicate nor malformed, so duplicate rejection and ID shape both
/// accept it. Only the monotone check sees it, and it is what keeps the
/// document's reading order and its numbering from drifting apart.
#[test]
#[should_panic(expected = "SPEC_0022 ordinals must increase within LEX")]
fn non_monotone_ordinal_is_refused() {
    let rows: &[&str] = &[
        "| LEX-002 | second | S2 | text |",
        "| LEX-001 | first | S2 | text |",
    ];
    let _ = CatalogRows::parse(&catalog_fixture(&[("LEX", rows)], &[]));
}

/// Mutant witness: a contract-shaped row outside section 4 is not a catalog row.
///
/// The live SPEC_0022 has no row outside section 4 either, so this branch is
/// likewise unexercised by the checked-in document. The two parses differ only
/// in where the `LEX-002` row sits, and the in-section parse is asserted first
/// so the out-of-section assertions cannot pass because the fixture was empty
/// or malformed. Both effects are pinned, presence and derived count, because
/// either alone can hold for the wrong reason: a count could fall while the ID
/// lingers, and an ID could vanish while a count is read from elsewhere.
#[test]
fn catalog_row_moved_outside_section_four_leaves_the_catalog() {
    let both: &[&str] = &[
        "| LEX-001 | first | S2 | text |",
        "| LEX-002 | second | S2 | text |",
    ];
    let in_section = CatalogRows::parse(&catalog_fixture(&[("LEX", both)], &[]));
    assert!(in_section.ids().contains("LEX-002"));
    assert_eq!(in_section.counts().get("LEX"), Some(&2));
    assert_eq!(in_section.rows.len(), 2);

    let kept: &[&str] = &["| LEX-001 | first | S2 | text |"];
    let moved_rows = ["| LEX-002 | second | S2 | text |"];
    let moved = CatalogRows::parse(&catalog_fixture(&[("LEX", kept)], &moved_rows));
    assert!(
        !moved.ids().contains("LEX-002"),
        "a contract-shaped row outside section 4 must not be a catalog row: {:?}",
        moved.ids()
    );
    assert_eq!(
        moved.counts().get("LEX"),
        Some(&1),
        "moving a row out of section 4 must lower its category count: {:?}",
        moved.counts()
    );
    assert_eq!(moved.rows.len(), 1);
}

fn section_index_contract_counts(catalog: &str) -> BTreeMap<String, usize> {
    let section = catalog
        .split_once("### Section Index (for selective loading)")
        .and_then(|(_, tail)| tail.split_once("\n---"))
        .map(|(section, _)| section)
        .expect("SPEC_0022 section-index boundaries changed");
    let mut counts = BTreeMap::new();

    for line in section.lines().filter(|line| line.contains(" contracts |")) {
        let mut cells = line.split('|').map(str::trim);
        let _empty = cells.next();
        let heading = cells.next().expect("section-index row has a heading");
        let description = cells.nth(1).expect("section-index row has a description");
        let prefix = heading
            .split_whitespace()
            .nth(1)
            .expect("section-index contract heading has a prefix");
        let count = description
            .split_once('(')
            .and_then(|(_, tail)| tail.split_once(" contracts)"))
            .and_then(|(count, _)| count.parse::<usize>().ok())
            .unwrap_or_else(|| panic!("section-index row has no contract count: {line}"));
        assert!(
            counts.insert(prefix.to_string(), count).is_none(),
            "duplicate section-index count for {prefix}"
        );
    }

    assert!(
        !counts.is_empty(),
        "SPEC_0022 section index has no contract counts"
    );
    counts
}

fn summary_contract_counts(catalog: &str) -> (BTreeMap<String, usize>, usize) {
    let section = catalog
        .split_once("## 5. Contract Summary by Category")
        .and_then(|(_, tail)| tail.split_once("\n---"))
        .map(|(section, _)| section)
        .expect("SPEC_0022 contract-summary boundaries changed");
    let mut counts = BTreeMap::new();
    let mut total = None;

    for line in section.lines().filter(|line| line.starts_with('|')) {
        let cells: Vec<&str> = line.split('|').map(str::trim).collect();
        if cells.len() < 5 {
            continue;
        }
        if cells[1] == "**Total**" {
            total = cells[3].trim_matches('*').parse::<usize>().ok();
            continue;
        }
        let prefix = cells[2];
        let Some(count) = cells[3].parse::<usize>().ok() else {
            continue;
        };
        assert!(
            counts.insert(prefix.to_string(), count).is_none(),
            "duplicate summary count for {prefix}"
        );
    }

    assert!(
        !counts.is_empty(),
        "SPEC_0022 summary has no category counts"
    );
    (counts, total.expect("SPEC_0022 summary has no total"))
}

const SPEC_0022_REL: &str = "spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md";
const MLS_FORMALIZATION_MANIFEST_REL: &str = "infra/verification/mls-formalization-coverage.json";
const ASSOCIATION_GAPS_REL: &str = "infra/verification/modelica-association-gaps.json";
const FORMAL_STATEMENTS_REL: &str = "crates/rumoca-contracts/data/formal_statements.toml";
const ALLOWED_FORMALIZATION_STATUSES: &[&str] =
    &["unformalized", "formalized_unproved", "machine_proved"];

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn read_workspace_file(relative: &str) -> String {
    let path = workspace_root().join(relative);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", path.display()))
}

/// The checked-in SPEC_0022 document.
fn catalog_document() -> String {
    read_workspace_file(SPEC_0022_REL)
}

/// The catalog IDs of whichever document a tracker names as its catalog.
///
/// The tracker points at the catalog by path rather than assuming one, and the
/// bytes it points at are parsed by [`CatalogRows::parse`]. A tracker that
/// retargets a different document therefore gets that document's section 4
/// rows, under the same rule, instead of a second rule of its own.
fn tracked_catalog_ids(tracker: &Value) -> BTreeSet<String> {
    let catalog_rel = tracker["contract_catalog"]
        .as_str()
        .expect("contract_catalog string");
    CatalogRows::parse(&read_workspace_file(catalog_rel)).ids()
}

/// The formal MLS semantics must account for every contract in SPEC_0022.
/// Absent overrides resolve to the manifest's honest `unformalized` default;
/// this keeps gaps visible while the formal definition is built incrementally.
#[test]
fn mls_formalization_manifest_tracks_every_catalog_contract() {
    let manifest_text = read_workspace_file(MLS_FORMALIZATION_MANIFEST_REL);
    let manifest: Value = serde_json::from_str(&manifest_text).expect("parse manifest JSON");
    assert_eq!(manifest["schema_version"], 1);
    assert_eq!(manifest["mls_version"], "3.7");
    assert_eq!(
        manifest["ordinary_compilation_policy"], "informational_only",
        "proof coverage must not reject otherwise accepted Modelica"
    );

    let ids = tracked_catalog_ids(&manifest);
    assert_eq!(
        manifest["catalog_contract_count"].as_u64(),
        Some(ids.len() as u64),
        "catalog changes must update the reviewed manifest count"
    );

    let default_status = manifest["default_status"]
        .as_str()
        .expect("default_status string");
    assert!(ALLOWED_FORMALIZATION_STATUSES.contains(&default_status));
    let entries = manifest["contracts"].as_object().expect("contracts object");
    for (id, entry) in entries {
        assert!(ids.contains(id), "manifest names unknown contract `{id}`");
        let status = entry["status"].as_str().expect("contract status string");
        assert!(
            ALLOWED_FORMALIZATION_STATUSES.contains(&status),
            "contract `{id}` has unknown status `{status}`"
        );
        if status != "unformalized" {
            assert!(
                entry["evidence"]
                    .as_array()
                    .is_some_and(|items| !items.is_empty()),
                "contract `{id}` needs machine-readable evidence"
            );
        }
    }

    let complete = ids.iter().all(|id| {
        let status = match entries.get(id) {
            Some(entry) => entry["status"].as_str().expect("contract status string"),
            None => default_status,
        };
        status == "machine_proved"
    });
    assert_eq!(
        manifest["formalization_complete"].as_bool(),
        Some(complete),
        "formalization_complete must be derived from every catalog contract"
    );
}

#[test]
fn modelica_association_gap_records_are_traceable_to_the_catalog() {
    let gaps_text = read_workspace_file(ASSOCIATION_GAPS_REL);
    let payload: Value = serde_json::from_str(&gaps_text).expect("parse gap tracker JSON");
    assert_eq!(payload["schema_version"], 1);
    assert_eq!(payload["mls_version"], "3.7");
    let ids = tracked_catalog_ids(&payload);
    let allowed_states = ["discovered", "proposal_drafted", "submitted", "resolved"];
    let mut issue_ids = BTreeSet::new();
    let mut mapped_statements = BTreeSet::new();
    for issue in payload["issues"].as_array().expect("issues array") {
        let issue_id = issue["id"].as_str().expect("gap id");
        assert!(issue_ids.insert(issue_id), "duplicate gap `{issue_id}`");
        assert!(
            allowed_states.contains(&issue["status"].as_str().expect("gap status")),
            "gap `{issue_id}` has unknown submission status"
        );
        assert!(
            issue["contract_ids"]
                .as_array()
                .is_some_and(|items| !items.is_empty()),
            "gap `{issue_id}` must name affected contracts"
        );
        for contract in issue["contract_ids"].as_array().expect("contract IDs") {
            let contract = contract.as_str().expect("contract ID string");
            assert!(
                ids.contains(contract),
                "gap `{issue_id}` names `{contract}`"
            );
        }
        for statement in issue["formal_statement_ids"]
            .as_array()
            .expect("formal_statement_ids array")
        {
            let statement = statement.as_str().expect("formal statement ID string");
            assert!(
                mapped_statements.insert(statement.to_string()),
                "formal statement `{statement}` maps to multiple gap records"
            );
        }
        for field in ["mls_clauses", "counterexample", "proposed_clarification"] {
            assert!(
                issue[field]
                    .as_str()
                    .is_some_and(|text| !text.trim().is_empty()),
                "gap `{issue_id}` must record `{field}`"
            );
        }
    }
    let statements_text = read_workspace_file(FORMAL_STATEMENTS_REL);
    let statements: toml::Value = toml::from_str(&statements_text).expect("formal statements");
    let spec_silent = statements["statements"]
        .as_array()
        .expect("formal statements array")
        .iter()
        .filter(|statement| statement["tier"].as_str() == Some("SpecSilent"))
        .map(|statement| {
            statement["id"]
                .as_str()
                .expect("formal statement ID")
                .to_string()
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(
        mapped_statements, spec_silent,
        "every SpecSilent formal statement must map exactly once to an upstream gap record"
    );
}
