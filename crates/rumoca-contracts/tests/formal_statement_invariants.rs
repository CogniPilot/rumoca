//! Well-formedness guards for the formal-statement registry.
//!
//! The registry's value is that a reader can trust a row without opening the
//! files it names. These tests are what make that trust earned: every pin points
//! at a file that exists and text that occurs in it, every specification quote
//! already appears in the tree rather than being introduced here, and a row
//! claiming enforcement points at something executable.
//!
//! What these guards cannot check is *attribution*: whether a section number is
//! the right one, and whether a quotation the tree already carries was correctly
//! transcribed from the specification in the first place. Both are invisible
//! here — a row citing §8.3.5 for a §8.3.5.4 rule passes every test below. That
//! is review work, and the module header of `registry::formal` says so.

use rumoca_contracts::{
    EnforcementStatus, FormalStatement, PinPolarity, StatementPin, StatementTier, create_registry,
    formal_statements, parse_formal_statements,
};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root is two levels above crates/rumoca-contracts")
        .to_path_buf()
}

/// Read a repository-relative file, failing with the row that named it.
fn read_repo_file(relative: &str, owner: &str) -> String {
    let path = workspace_root().join(relative);
    assert!(
        path.exists(),
        "{owner} names a file that does not exist: {}",
        path.display()
    );
    fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("{owner} names an unreadable file {relative}: {error}"))
}

/// Line-wrapping artifacts stripped and whitespace collapsed.
///
/// A quotation reaches the tree wrapped across lines: as a doc comment each line
/// carries a marker, and as a diagnostic string each line carries a Rust
/// continuation backslash. Comparing the normalized forms is what lets a row
/// quote a whole sentence instead of whichever fragment fitted on one line.
fn normalized_prose(text: &str) -> String {
    let mut words = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        let line = line
            .strip_prefix("//!")
            .or_else(|| line.strip_prefix("///"))
            .or_else(|| line.strip_prefix("//"))
            .unwrap_or(line);
        let line = line.trim_end().strip_suffix('\\').unwrap_or(line);
        words.extend(line.split_whitespace());
    }
    words.join(" ")
}

fn statements() -> &'static [FormalStatement] {
    formal_statements()
}

#[test]
fn registry_is_not_empty_and_carries_every_tier() {
    let all = statements();
    assert!(
        !all.is_empty(),
        "the formal-statement registry must not be empty"
    );

    for label in ["SpecSourced", "OracleImplied", "SpecSilent"] {
        let count = all
            .iter()
            .filter(|entry| entry.tier.label() == label)
            .count();
        assert!(
            count > 0,
            "tier {label} is unpopulated; a tier no row uses is a tier nobody reaches for, and the \
             rows that needed it end up filed under one that misdescribes them"
        );
    }
}

#[test]
fn ids_are_unique_and_name_their_own_category() {
    let mut seen = BTreeSet::new();
    for entry in statements() {
        assert!(
            seen.insert(entry.id.clone()),
            "duplicate formal-statement id: {}",
            entry.id
        );
        let expected = format!("FS-{}-", entry.category_prefix());
        assert!(
            entry.id.starts_with(&expected),
            "{} does not start with its own category prefix {expected}",
            entry.id
        );
    }
}

#[test]
fn statements_read_as_one_sentence() {
    for entry in statements() {
        let statement = entry.statement.trim();
        assert!(
            statement.ends_with('.'),
            "{}: the formal statement must be a sentence ending in a period",
            entry.id
        );
        assert!(
            statement.len() >= 40,
            "{}: the formal statement is too short to be precise: {statement:?}",
            entry.id
        );
    }
}

/// Fail once with every offending row, so adding five rows reports five
/// problems instead of the first one five times.
fn report(kind: &str, offenders: Vec<String>) {
    assert!(
        offenders.is_empty(),
        "{} row(s) fail the {kind} check:\n  {}",
        offenders.len(),
        offenders.join("\n  ")
    );
}

#[test]
fn every_pin_file_exists_and_contains_its_anchor() {
    let mut cache: BTreeMap<String, String> = BTreeMap::new();
    let mut offenders = Vec::new();
    for entry in statements() {
        let file = entry.pin.file();
        let content = cache
            .entry(file.to_string())
            .or_insert_with(|| read_repo_file(file, &entry.id));
        let needle = entry.pin.needle();
        let found = content.contains(&needle)
            || normalized_prose(content).contains(&normalized_prose(&needle));
        if !found {
            offenders.push(format!("{}: {file} does not contain {needle:?}", entry.id));
        }
    }
    report("pin-anchor", offenders);
}

/// How many lines above a `fn` an attribute may sit and still govern it.
///
/// Doc comments and `#[allow]` lines routinely separate the two; four lines
/// covers every test in this tree and is short enough that a plain helper
/// function cannot borrow an unrelated attribute from further up.
const ATTRIBUTE_LOOKBACK: usize = 4;

/// Whether `file` declares `function` as a test rather than as a plain helper.
fn declares_a_test_function(content: &str, function: &str) -> bool {
    let signature = format!("fn {function}(");
    let lines: Vec<&str> = content.lines().collect();
    lines.iter().enumerate().any(|(index, line)| {
        if !line.contains(&signature) {
            return false;
        }
        let start = index.saturating_sub(ATTRIBUTE_LOOKBACK);
        lines[start..index]
            .iter()
            .any(|above| above.trim_start().starts_with("#[test]") || above.contains("#[rstest"))
    })
}

#[test]
fn test_pins_name_a_real_test_function() {
    let mut cache: BTreeMap<String, String> = BTreeMap::new();
    let mut offenders = Vec::new();
    for entry in statements() {
        let StatementPin::Test { file, function } = &entry.pin else {
            continue;
        };
        let content = cache
            .entry(file.clone())
            .or_insert_with(|| read_repo_file(file, &entry.id));
        if !declares_a_test_function(content, function) {
            offenders.push(format!(
                "{}: {file} declares `fn {function}` but not under #[test]; a helper function is \
                 not evidence",
                entry.id
            ));
        }
    }
    report("test-pin", offenders);
}

/// Shortest `Site` anchor that can still name one place rather than many.
const MIN_SITE_ANCHOR: usize = 12;

#[test]
fn site_anchors_are_specific_enough_to_name_one_place() {
    let mut offenders = Vec::new();
    for entry in statements() {
        let StatementPin::Site { file, symbol } = &entry.pin else {
            continue;
        };
        if symbol.trim().len() < MIN_SITE_ANCHOR {
            offenders.push(format!(
                "{}: Site anchor {symbol:?} in {file} is shorter than {MIN_SITE_ANCHOR} \
                 characters, so it cannot be trusted to name one place",
                entry.id
            ));
        }
    }
    report("site-anchor-specificity", offenders);
}

#[test]
fn pin_polarity_agrees_with_status() {
    let mut offenders = Vec::new();
    for entry in statements() {
        let required = match entry.status {
            EnforcementStatus::Enforced => Some(PinPolarity::Asserts),
            EnforcementStatus::RecordedDivergence => Some(PinPolarity::PinsDivergence),
            EnforcementStatus::Unimplemented => None,
        };
        if let Some(required) = required
            && entry.pin_polarity != required
        {
            offenders.push(format!(
                "{}: status {:?} requires pin_polarity {required:?}, found {:?}",
                entry.id, entry.status, entry.pin_polarity
            ));
        }
    }
    report("pin-polarity", offenders);
}

#[test]
fn spec_silent_rows_establish_the_silence() {
    let mut offenders = Vec::new();
    for entry in statements() {
        let StatementTier::SpecSilent {
            rationale,
            alternatives_considered,
            ..
        } = &entry.tier
        else {
            continue;
        };
        // A silence is established by naming what the specification *does*
        // scope, not by observing that it failed to mention the case.
        let cites_a_section = rationale.contains('§');
        if !cites_a_section {
            offenders.push(format!(
                "{}: the rationale must name the section whose stated scope leaves this open, so a \
                 reader can tell a searched-for silence from an unread one",
                entry.id
            ));
        }
        if alternatives_considered.len() < 60 {
            offenders.push(format!(
                "{}: alternatives_considered must say what else was available",
                entry.id
            ));
        }
    }
    report("spec-silent", offenders);
}

#[test]
fn spec_sourced_quotes_already_appear_in_their_source() {
    let mut cache: BTreeMap<String, String> = BTreeMap::new();
    let mut offenders = Vec::new();
    for entry in statements() {
        let StatementTier::SpecSourced {
            quote,
            quote_source,
            ..
        } = &entry.tier
        else {
            continue;
        };
        let content = cache
            .entry(quote_source.clone())
            .or_insert_with(|| read_repo_file(quote_source, &entry.id));
        if !normalized_prose(content).contains(&normalized_prose(quote)) {
            offenders.push(format!(
                "{}: {quote_source} does not contain {quote:?}",
                entry.id
            ));
        }
    }
    report(
        "quote-grounding (a registry row cites a quotation, it never introduces one)",
        offenders,
    );
}

#[test]
fn spec_sourced_sections_are_well_formed() {
    for entry in statements() {
        let StatementTier::SpecSourced { section, .. } = &entry.tier else {
            continue;
        };
        for reference in std::iter::once(section).chain(entry.related_sections.iter()) {
            let Some(digits) = reference.strip_prefix('§') else {
                panic!("{}: section {reference:?} must start with §", entry.id);
            };
            assert!(
                !digits.is_empty()
                    && digits
                        .chars()
                        .all(|c| c.is_ascii_digit() || c == '.' || c.is_ascii_uppercase())
                    && !digits.ends_with('.'),
                "{}: section {reference:?} is not an MLS section number",
                entry.id
            );
        }
    }
}

#[test]
fn oracle_implied_rows_name_an_oracle_and_its_evidence() {
    for entry in statements() {
        let StatementTier::OracleImplied {
            oracle,
            evidence,
            latitude_note,
        } = &entry.tier
        else {
            continue;
        };
        assert!(
            evidence.contains('/') || evidence.contains(".rs"),
            "{}: the evidence pointer must name a file, not a claim: {evidence:?}",
            entry.id
        );
        assert!(
            latitude_note.len() >= 60,
            "{}: the latitude note must say what the specification leaves open",
            entry.id
        );
        assert!(!oracle.trim().is_empty(), "{}: oracle is blank", entry.id);
    }
}

#[test]
fn enforced_statements_pin_executable_evidence() {
    for entry in statements() {
        if entry.status != EnforcementStatus::Enforced {
            continue;
        }
        assert!(
            entry.pin.is_executable(),
            "{}: an Enforced statement must pin a Test or a Site; a written record is not \
             enforcement",
            entry.id
        );
    }
}

#[test]
fn recorded_divergences_say_what_diverges() {
    for entry in statements() {
        if entry.status != EnforcementStatus::RecordedDivergence {
            continue;
        }
        let has_prose = entry.note.is_some()
            || matches!(entry.tier, StatementTier::OracleImplied { .. })
            || matches!(entry.pin, StatementPin::Record { .. });
        assert!(
            has_prose,
            "{}: a recorded divergence must carry a note, an oracle tier, or a Record pin",
            entry.id
        );
    }
}

#[test]
fn referenced_contracts_exist_in_the_spec_0022_registry() {
    let registry = create_registry();
    for entry in statements() {
        for contract in &entry.contracts {
            assert!(
                registry.get(contract.as_str()).is_some(),
                "{}: references contract {contract}, which is not in the SPEC_0022 registry",
                entry.id
            );
        }
    }
}

#[test]
fn no_two_statements_state_the_same_thing() {
    let mut seen: BTreeMap<String, &str> = BTreeMap::new();
    for entry in statements() {
        let key = normalized_prose(&entry.statement).to_lowercase();
        if let Some(previous) = seen.insert(key, entry.id.as_str()) {
            panic!(
                "{} and {} state the same thing; one of them is redundant",
                previous, entry.id
            );
        }
    }
}

// ---------------------------------------------------------------------------
// The loader's checks are the registry's guarantee, so they are exercised.
// ---------------------------------------------------------------------------

const WELL_FORMED_ROW: &str = r#"
[[statements]]
id = 'FS-EQN-900'
statement = 'A when-clause activates on a rising edge, which is a sentence long enough to pass.'
tier = 'SpecSourced'
edition = '3.6'
section = '§8.3.5'
quote_kind = 'Verbatim'
quote = 'formal statement'
quote_source = 'crates/rumoca-contracts/src/registry/formal.rs'
pin_kind = 'Test'
pin_file = 'crates/rumoca-contracts/tests/formal_statement_invariants.rs'
pin_anchor = 'ids_are_unique_and_name_their_own_category'
pin_polarity = 'Asserts'
status = 'Enforced'
"#;

fn row_without(field: &str) -> String {
    WELL_FORMED_ROW
        .lines()
        .filter(|line| !line.starts_with(&format!("{field} =")))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn the_well_formed_fixture_row_parses() {
    let parsed = parse_formal_statements(WELL_FORMED_ROW).expect("fixture row must parse");
    assert_eq!(parsed.len(), 1);
}

#[test]
fn a_spec_sourced_row_without_a_section_is_refused() {
    let error = parse_formal_statements(&row_without("section"))
        .expect_err("a SpecSourced row without a section is not a statement");
    assert!(
        error.to_string().contains("section"),
        "the refusal must name the missing field: {error}"
    );
}

#[test]
fn a_spec_sourced_row_missing_any_quotation_field_is_refused() {
    for field in ["quote", "quote_source", "quote_kind", "edition"] {
        let error = parse_formal_statements(&row_without(field))
            .unwrap_err_or_else(|| format!("a SpecSourced row without {field} must be refused"));
        assert!(
            error.to_string().contains(field),
            "the refusal must name {field}: {error}"
        );
    }
}

/// `Result::expect_err` with a lazily built message.
trait UnwrapErrOrElse<T, E> {
    fn unwrap_err_or_else(self, message: impl FnOnce() -> String) -> E;
}

impl<T: std::fmt::Debug, E> UnwrapErrOrElse<T, E> for Result<T, E> {
    fn unwrap_err_or_else(self, message: impl FnOnce() -> String) -> E {
        match self {
            Ok(value) => panic!("{}, got {value:?}", message()),
            Err(error) => error,
        }
    }
}

#[test]
fn an_oracle_implied_row_without_evidence_is_refused() {
    let row = "
[[statements]]
id = 'FS-EQN-901'
statement = 'An oracle-implied row states something long enough to be a real sentence here.'
tier = 'OracleImplied'
oracle = 'OpenModelica (omc)'
latitude_note = 'the specification leaves this open'
pin_kind = 'Record'
pin_file = 'crates/rumoca-contracts/data/formal_statements.toml'
pin_anchor = 'FS-EQN-001'
pin_polarity = 'PinsDivergence'
status = 'RecordedDivergence'
";
    let error = parse_formal_statements(row)
        .expect_err("an OracleImplied row without evidence is not a statement");
    assert!(
        error.to_string().contains("evidence"),
        "the refusal must name the missing evidence pointer: {error}"
    );
}

#[test]
fn a_row_mixing_the_two_tiers_is_refused() {
    let row = WELL_FORMED_ROW.replace(
        "status = 'Enforced'",
        "oracle = 'OpenModelica (omc)'\nstatus = 'Enforced'",
    );
    let error = parse_formal_statements(&row)
        .expect_err("a SpecSourced row carrying an oracle is not a statement");
    assert!(
        error.to_string().contains("oracle"),
        "the refusal must name the foreign field: {error}"
    );
}

#[test]
fn an_unknown_category_prefix_is_refused() {
    let row = WELL_FORMED_ROW.replace("FS-EQN-900", "FS-NOPE-900");
    let error =
        parse_formal_statements(&row).expect_err("an unknown category prefix is not a statement");
    assert!(
        error.to_string().contains("NOPE"),
        "the refusal must name the unknown prefix: {error}"
    );
}

#[test]
fn an_unknown_field_is_refused() {
    let row = WELL_FORMED_ROW.replace("status = 'Enforced'", "status = 'Enforced'\ntypo = 'x'");
    parse_formal_statements(&row).expect_err("an unknown field is a typo, not a statement");
}

#[test]
fn an_enforced_row_whose_pin_records_a_divergence_is_refused() {
    let row = WELL_FORMED_ROW.replace(
        "pin_polarity = 'Asserts'",
        "pin_polarity = 'PinsDivergence'",
    );
    let error = parse_formal_statements(&row)
        .expect_err("an Enforced row cannot pin what the compiler does instead");
    assert!(
        error.to_string().contains("Asserts"),
        "the refusal must name the required polarity: {error}"
    );
}

#[test]
fn a_recorded_divergence_that_claims_to_assert_is_refused() {
    // The fixture already declares `pin_polarity = 'Asserts'`, so changing only
    // the status is exactly the mistake this guard exists to catch.
    let row = WELL_FORMED_ROW.replace("status = 'Enforced'", "status = 'RecordedDivergence'");
    let error = parse_formal_statements(&row)
        .expect_err("a divergence record cannot claim its pin asserts the statement");
    assert!(
        error.to_string().contains("PinsDivergence"),
        "the refusal must name the required polarity: {error}"
    );
}

#[test]
fn a_spec_silent_row_without_alternatives_is_refused() {
    let row = "
[[statements]]
id = 'FS-EQN-902'
statement = 'The specification does not decide which end of the chain the surviving slope comes from.'
tier = 'SpecSilent'
rationale = 'MLS §3.7.4.5 states the rule over a chain without naming an endpoint.'
pin_kind = 'Site'
pin_file = 'crates/rumoca-contracts/src/registry/formal.rs'
pin_anchor = 'load_all_formal_statements'
pin_polarity = 'Asserts'
status = 'Enforced'
";
    let error = parse_formal_statements(row)
        .expect_err("a SpecSilent row without alternatives_considered is not a statement");
    assert!(
        error.to_string().contains("alternatives_considered"),
        "the refusal must name the missing field: {error}"
    );
}

#[test]
fn a_spec_silent_row_carrying_an_oracle_field_is_refused() {
    let row = "
[[statements]]
id = 'FS-EQN-903'
statement = 'The specification does not decide which end of the chain the surviving slope comes from.'
tier = 'SpecSilent'
rationale = 'MLS §3.7.4.5 states the rule over a chain without naming an endpoint.'
alternatives_considered = 'Either endpoint satisfies the rule; the compiler takes declaration order.'
oracle = 'OpenModelica (omc)'
pin_kind = 'Site'
pin_file = 'crates/rumoca-contracts/src/registry/formal.rs'
pin_anchor = 'load_all_formal_statements'
pin_polarity = 'Asserts'
status = 'Enforced'
";
    let error = parse_formal_statements(row)
        .expect_err("`oracle` belongs to OracleImplied; SpecSilent carries `oracle_consulted`");
    assert!(
        error.to_string().contains("oracle"),
        "the refusal must name the foreign field: {error}"
    );
}
