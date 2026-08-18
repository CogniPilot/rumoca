//! SPEC_0043 §1 `rumoca-ir-dae` LOC review triggers, measured and checked
//! against the §1b acknowledgment ledger.
//!
//! # Why this exists
//!
//! SPEC_0043 §1 states three review triggers (core, wire, total). Nothing
//! measured them, so all three sat above their threshold with no review and no
//! written acknowledgment. A trigger nobody computes is a comment.
//!
//! # Acceptance contract (SPEC_0008)
//!
//! This gate **rejects**:
//!
//! * a trigger above its threshold with no ledger row in SPEC_0043 §1b;
//! * a trigger above the ceiling its ledger row acknowledges;
//! * a ceiling that is not the measured value rounded up to the next
//!   [`CEILING_STEP_LINES`] lines — too high hides headroom, too low is stale;
//! * a ledger row for a trigger that is at or below its threshold;
//! * a ledger row whose threshold cell disagrees with [`TRIGGERS`], or a
//!   SPEC_0043 §1 trigger sentence that no longer names the same threshold.
//!
//! It **accepts**:
//!
//! * any measured value, however large, once its ledger row records it — size
//!   alone never blocks a landing, an unwritten acknowledgment does;
//! * a trigger at or below its threshold carrying no ledger row;
//! * every edit that stays inside the current 250-line step, so ordinary work
//!   does not churn the ledger.
//!
//! Owner: this file measures and checks the ledger. The thresholds and the
//! ledger rows are owned by `spec/SPEC_0043_CONSTRUCTION_CATALOG.md` §1/§1b.
//! The module review an exceeded trigger demands stays a human obligation of
//! SPEC_0036; acknowledging debt here does not discharge it.
//!
//! Evidence: [`test_dae_loc_triggers_are_acknowledged`] over the live tree,
//! plus the parser and ceiling unit tests below.

use std::fs;
use std::path::{Path, PathBuf};

/// Granularity of an acknowledged ceiling. Small enough that a growth wave is
/// visible in review, large enough that a one-line edit does not rewrite the
/// ledger.
const CEILING_STEP_LINES: usize = 250;

/// SPEC_0043 §1: trigger id, threshold, and the sentence in §1 that states it.
/// The sentence is checked verbatim so a threshold cannot move in the spec
/// without moving here.
const TRIGGERS: &[(&str, usize, &str)] = &[
    ("dae-core-loc", 11_000, "Core above 11,000 LOC"),
    ("dae-wire-loc", 3_250, "Wire above 3,250 LOC"),
    ("dae-total-loc", 14_250, "Total above 14,250 LOC"),
];

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

/// SPEC_0043 §1a: test and generated sources are excluded from every count.
fn is_excluded_source(relative: &Path) -> bool {
    let text = relative.to_string_lossy().replace('\\', "/");
    text.split('/')
        .any(|segment| segment == "tests" || segment == "generated")
        || text.ends_with("/tests.rs")
        || text == "tests.rs"
        || text.ends_with("_tests.rs")
}

/// SPEC_0043 §1a: the wire partition is `model/wire.rs` plus `model/wire/`.
fn is_wire_source(relative: &Path) -> bool {
    let text = relative.to_string_lossy().replace('\\', "/");
    text == "model/wire.rs" || text.starts_with("model/wire/")
}

fn collect_rust_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("read rumoca-ir-dae source directory") {
        let entry = entry.expect("source directory entry");
        let path = entry.path();
        if path.is_dir() {
            collect_rust_sources(&path, out);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
            out.push(path);
        }
    }
}

/// Measured core, wire, and total production line counts, in `TRIGGERS` order.
fn measure_dae_lines(root: &Path) -> [usize; 3] {
    let src = root.join("crates/rumoca-ir-dae/src");
    let mut files = Vec::new();
    collect_rust_sources(&src, &mut files);

    let (mut core, mut wire) = (0usize, 0usize);
    for path in files {
        let relative = path.strip_prefix(&src).expect("source under src");
        if is_excluded_source(relative) {
            continue;
        }
        let lines = fs::read_to_string(&path)
            .expect("read rumoca-ir-dae source")
            .lines()
            .count();
        if is_wire_source(relative) {
            wire += lines;
        } else {
            core += lines;
        }
    }
    [core, wire, core + wire]
}

/// One parsed SPEC_0043 §1b ledger row.
#[derive(Debug, PartialEq, Eq)]
struct LedgerRow {
    threshold: usize,
    ceiling: usize,
}

fn parse_ledger_number(cell: &str) -> Option<usize> {
    let digits = cell.trim().trim_matches('`').replace(',', "");
    (!digits.is_empty() && digits.bytes().all(|byte| byte.is_ascii_digit()))
        .then(|| digits.parse().expect("digits parse"))
}

/// Read the ledger row for `trigger_id` out of the SPEC_0043 markdown.
///
/// A row is `| \`<id>\` | <threshold> | <ceiling> | <reduction owner> |`. A row
/// whose numbers do not parse, or that names no reduction owner, is reported as
/// malformed rather than silently ignored.
fn parse_ledger_row(spec: &str, trigger_id: &str) -> Result<Option<LedgerRow>, String> {
    let key = format!("`{trigger_id}`");
    let mut found = None;
    for line in spec.lines() {
        let trimmed = line.trim();
        if !trimmed.starts_with('|') {
            continue;
        }
        let cells: Vec<&str> = trimmed
            .trim_matches('|')
            .split('|')
            .map(str::trim)
            .collect();
        if cells.first() != Some(&key.as_str()) {
            continue;
        }
        if found.is_some() {
            return Err(format!("{trigger_id}: duplicate ledger rows"));
        }
        if cells.len() != 4 {
            return Err(format!(
                "{trigger_id}: ledger row needs 4 cells (trigger, threshold, ceiling, owner), got {}",
                cells.len()
            ));
        }
        let threshold = parse_ledger_number(cells[1]).ok_or_else(|| {
            format!(
                "{trigger_id}: threshold cell `{}` is not a number",
                cells[1]
            )
        })?;
        let ceiling = parse_ledger_number(cells[2])
            .ok_or_else(|| format!("{trigger_id}: ceiling cell `{}` is not a number", cells[2]))?;
        if cells[3].is_empty() {
            return Err(format!("{trigger_id}: ledger row names no reduction owner"));
        }
        found = Some(LedgerRow { threshold, ceiling });
    }
    Ok(found)
}

/// SPEC_0043 §1b: the ceiling a measured value must record.
fn required_ceiling(measured: usize) -> usize {
    measured.div_ceil(CEILING_STEP_LINES) * CEILING_STEP_LINES
}

fn check_trigger(
    trigger_id: &str,
    threshold: usize,
    measured: usize,
    row: Option<LedgerRow>,
) -> Vec<String> {
    let mut failures = Vec::new();
    match row {
        None if measured > threshold => failures.push(format!(
            "{trigger_id}: {measured} lines exceeds the SPEC_0043 §1 review trigger of \
{threshold} and is not acknowledged. Add a §1b ledger row \
`| `{trigger_id}` | {threshold} | {} | <reduction owner> |`, or reduce below the trigger.",
            required_ceiling(measured)
        )),
        None => {}
        Some(row) if measured <= threshold => failures.push(format!(
            "{trigger_id}: {measured} lines is back at or below the trigger of {threshold}; \
delete the stale SPEC_0043 §1b ledger row (ceiling {})",
            row.ceiling
        )),
        Some(row) => {
            if row.threshold != threshold {
                failures.push(format!(
                    "{trigger_id}: ledger row records threshold {} but SPEC_0043 §1 states \
{threshold}; the two MUST be the same number",
                    row.threshold
                ));
            }
            let required = required_ceiling(measured);
            if row.ceiling != required {
                failures.push(format!(
                    "{trigger_id}: {measured} lines requires an acknowledged ceiling of \
{required} (measured rounded up to the next {CEILING_STEP_LINES}), ledger row records {}. \
Update the SPEC_0043 §1b row in this change.",
                    row.ceiling
                ));
            }
        }
    }
    failures
}

#[test]
fn test_dae_loc_triggers_are_acknowledged() {
    let root = workspace_root();
    let spec = fs::read_to_string(root.join("spec/SPEC_0043_CONSTRUCTION_CATALOG.md"))
        .expect("read SPEC_0043");
    let measured = measure_dae_lines(&root);

    let mut failures = Vec::new();
    for (index, (trigger_id, threshold, sentence)) in TRIGGERS.iter().enumerate() {
        if !spec.contains(sentence) {
            failures.push(format!(
                "{trigger_id}: SPEC_0043 §1 no longer states `{sentence}`; the spec threshold and \
TRIGGERS in this gate MUST move together"
            ));
            continue;
        }
        match parse_ledger_row(&spec, trigger_id) {
            Ok(row) => failures.extend(check_trigger(trigger_id, *threshold, measured[index], row)),
            Err(error) => failures.push(error),
        }
    }

    assert!(
        failures.is_empty(),
        "SPEC_0043 §1 LOC review triggers are unacknowledged or their ledger is stale \
(measured core={}, wire={}, total={}):\n  {}",
        measured[0],
        measured[1],
        measured[2],
        failures.join("\n  "),
    );
}

#[test]
fn test_ceiling_is_the_next_step_above_the_measurement() {
    assert_eq!(required_ceiling(12_503), 12_750);
    assert_eq!(required_ceiling(12_750), 12_750);
    assert_eq!(required_ceiling(12_751), 13_000);
    assert_eq!(required_ceiling(0), 0);
}

#[test]
fn test_trigger_partition_follows_the_measurement_convention() {
    assert!(is_wire_source(Path::new("model/wire.rs")));
    assert!(is_wire_source(Path::new("model/wire/function_replay.rs")));
    assert!(!is_wire_source(Path::new("model/view.rs")));
    assert!(is_excluded_source(Path::new("tests.rs")));
    assert!(is_excluded_source(Path::new("tests/derived_wire.rs")));
    assert!(is_excluded_source(Path::new("model/wire/wire_tests.rs")));
    assert!(is_excluded_source(Path::new("generated/wire_schema.rs")));
    assert!(!is_excluded_source(Path::new("model/storage.rs")));
}

#[test]
fn test_ledger_parser_reads_rows_and_reports_malformed_ones() {
    let spec = "| `dae-core-loc` | 11,000 | 12,750 | owner |\n\
                | `dae-wire-loc` | 3,250 | ~ | owner |\n\
                | `dae-total-loc` | 14,250 | 16,250 |\n";
    assert_eq!(
        parse_ledger_row(spec, "dae-core-loc"),
        Ok(Some(LedgerRow {
            threshold: 11_000,
            ceiling: 12_750,
        }))
    );
    assert_eq!(parse_ledger_row(spec, "dae-missing-loc"), Ok(None));
    assert!(
        parse_ledger_row(spec, "dae-wire-loc")
            .expect_err("non-numeric ceiling is malformed")
            .contains("ceiling cell")
    );
    assert!(
        parse_ledger_row(spec, "dae-total-loc")
            .expect_err("a row missing the owner cell is malformed")
            .contains("4 cells")
    );
}

#[test]
fn test_unacknowledged_exceedance_fails_and_acknowledged_exceedance_passes() {
    let acknowledged = LedgerRow {
        threshold: 11_000,
        ceiling: 12_750,
    };
    assert!(
        check_trigger("dae-core-loc", 11_000, 12_503, None)
            .first()
            .is_some_and(|failure| failure.contains("is not acknowledged"))
    );
    assert!(check_trigger("dae-core-loc", 11_000, 12_503, Some(acknowledged)).is_empty());
    assert!(check_trigger("dae-core-loc", 11_000, 10_000, None).is_empty());
}

#[test]
fn test_ledger_must_track_growth_and_retirement() {
    let stale_low = LedgerRow {
        threshold: 11_000,
        ceiling: 12_750,
    };
    assert!(
        check_trigger("dae-core-loc", 11_000, 12_800, Some(stale_low))
            .first()
            .is_some_and(|failure| failure.contains("requires an acknowledged ceiling of 13000"))
    );

    let headroom = LedgerRow {
        threshold: 11_000,
        ceiling: 20_000,
    };
    assert!(
        check_trigger("dae-core-loc", 11_000, 12_503, Some(headroom))
            .first()
            .is_some_and(|failure| failure.contains("requires an acknowledged ceiling of 12750"))
    );

    let retired = LedgerRow {
        threshold: 11_000,
        ceiling: 12_750,
    };
    assert!(
        check_trigger("dae-core-loc", 11_000, 10_999, Some(retired))
            .first()
            .is_some_and(|failure| failure.contains("delete the stale"))
    );

    let drifted = LedgerRow {
        threshold: 9_000,
        ceiling: 12_750,
    };
    assert!(
        check_trigger("dae-core-loc", 11_000, 12_503, Some(drifted))
            .first()
            .is_some_and(|failure| failure.contains("records threshold 9000"))
    );
}
