//! Comparator-evidence check for `cargo xtask verify msl-parity`.
//!
//! # Why this exists at the xtask boundary
//!
//! The harness now fails its own quality gate when the OMC comparator produced
//! no bands. That check runs *inside* the libtest, so it can only fire on runs
//! that reach the gate — and a sharded run deliberately skips the aggregate
//! gate, so a shard that silently produced no comparator artifacts would still
//! exit 0 and be merged. This module is the second, independent boundary: after
//! the harness process exits, xtask looks at what actually landed on disk and
//! refuses to call a run finished when no comparison happened.
//!
//! # Acceptance contract (SPEC 0008)
//!
//! [`check_comparator_evidence`] runs for every `verify msl-parity` invocation
//! that is expected to simulate against the cohort target set.
//!
//! It **accepts**:
//!
//! * a results directory carrying a non-empty `omc_simulation_reference.json`
//!   whose `trace_comparison.models_compared` is greater than zero, alongside a
//!   `sim_trace_comparison.json` with the same model count and zero unmeasured
//!   initialization models;
//! * any run passing `--allow-unmeasured-parity`, which prints the same
//!   "parity unmeasured" headline as a warning instead of failing. The flag is
//!   the ONLY way to get a green cohort-shaped run with no comparison, and it
//!   has to be typed out.
//!
//! It **rejects** (exit nonzero, headline
//! [`PARITY_UNMEASURED_HEADLINE`]): a missing or empty reference, a missing or
//! invalid trace comparison, a reference whose comparator compared zero models,
//! or any compared model without exact-start initialization evidence.
//!
//! Owner: this module. The artifact names it guards are written by
//! `rumoca-msl-tools omc-simulation-reference`
//! (`crates/rumoca-test-msl/src/msl_tools/omc_simulation_reference/output.rs`),
//! and the in-harness counterpart is `MslParityMeasurement` in
//! `crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_quality_gate/parity_measurement.rs`.

use anyhow::{Result, bail};
use std::fs;
use std::path::Path;

/// Fixed headline shared with the harness. Operators and CI summaries grep for
/// this exact text, so both boundaries must spell it identically.
pub(crate) const PARITY_UNMEASURED_HEADLINE: &str = "parity unmeasured: comparator did not run";

const OMC_REFERENCE_FILE: &str = "omc_simulation_reference.json";
const TRACE_COMPARISON_FILE: &str = "sim_trace_comparison.json";

/// What the comparator left behind, or why there is nothing to read.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ComparatorEvidence {
    /// `models_compared` models were compared against OMC.
    Measured { models_compared: usize },
    /// Named reason there is no reading.
    Unmeasured { reason: String },
}

/// Read the comparator artifacts a run left in `results_dir`.
pub(crate) fn read_comparator_evidence(results_dir: &Path) -> ComparatorEvidence {
    let reference = results_dir.join(OMC_REFERENCE_FILE);
    let comparison = results_dir.join(TRACE_COMPARISON_FILE);
    for path in [&reference, &comparison] {
        match fs::metadata(path) {
            Ok(meta) if meta.len() > 0 => {}
            Ok(_) => {
                return ComparatorEvidence::Unmeasured {
                    reason: format!("{} is empty", path.display()),
                };
            }
            Err(error) => {
                return ComparatorEvidence::Unmeasured {
                    reason: format!("{} is missing ({error})", path.display()),
                };
            }
        }
    }
    let raw = match fs::read_to_string(&reference) {
        Ok(raw) => raw,
        Err(error) => {
            return ComparatorEvidence::Unmeasured {
                reason: format!("{} is unreadable ({error})", reference.display()),
            };
        }
    };
    let payload: serde_json::Value = match serde_json::from_str(&raw) {
        Ok(payload) => payload,
        Err(error) => {
            return ComparatorEvidence::Unmeasured {
                reason: format!("{} is not valid JSON ({error})", reference.display()),
            };
        }
    };
    let comparison_raw = match fs::read_to_string(&comparison) {
        Ok(raw) => raw,
        Err(error) => {
            return ComparatorEvidence::Unmeasured {
                reason: format!("{} is unreadable ({error})", comparison.display()),
            };
        }
    };
    let comparison_payload: serde_json::Value = match serde_json::from_str(&comparison_raw) {
        Ok(payload) => payload,
        Err(error) => {
            return ComparatorEvidence::Unmeasured {
                reason: format!("{} is not valid JSON ({error})", comparison.display()),
            };
        }
    };
    let models_compared = payload
        .pointer("/trace_comparison/models_compared")
        .and_then(serde_json::Value::as_u64);
    match models_compared {
        Some(count) if count > 0 => {
            let trace_count = comparison_payload
                .pointer("/models_compared")
                .and_then(serde_json::Value::as_u64);
            if trace_count != Some(count) {
                return ComparatorEvidence::Unmeasured {
                    reason: format!(
                        "{} models_compared {:?} does not match reference count {count}",
                        comparison.display(),
                        trace_count
                    ),
                };
            }
            let unmeasured_initial = comparison_payload
                .pointer("/summary/initial_condition/models_with_unmeasured_initial_conditions")
                .and_then(serde_json::Value::as_u64);
            match unmeasured_initial {
                Some(0) => ComparatorEvidence::Measured {
                    models_compared: count as usize,
                },
                Some(unmeasured) => ComparatorEvidence::Unmeasured {
                    reason: format!(
                        "{} reports {unmeasured} model(s) without exact-start initialization evidence",
                        comparison.display()
                    ),
                },
                None => ComparatorEvidence::Unmeasured {
                    reason: format!(
                        "{} carries no summary.initial_condition.models_with_unmeasured_initial_conditions",
                        comparison.display()
                    ),
                },
            }
        }
        Some(_) => ComparatorEvidence::Unmeasured {
            reason: format!(
                "{} reports trace_comparison.models_compared = 0",
                reference.display()
            ),
        },
        None => ComparatorEvidence::Unmeasured {
            reason: format!(
                "{} carries no trace_comparison.models_compared",
                reference.display()
            ),
        },
    }
}

/// Enforce the contract in the module docs. `allow_unmeasured` downgrades the
/// rejection to a warning.
pub(crate) fn check_comparator_evidence(results_dir: &Path, allow_unmeasured: bool) -> Result<()> {
    match read_comparator_evidence(results_dir) {
        ComparatorEvidence::Measured { models_compared } => {
            println!(
                "MSL parity comparator check: {models_compared} model(s) compared against OMC in {}.",
                results_dir.display()
            );
            Ok(())
        }
        ComparatorEvidence::Unmeasured { reason } => {
            if allow_unmeasured {
                eprintln!(
                    "warning: MSL {PARITY_UNMEASURED_HEADLINE} ({reason}); \
                     --allow-unmeasured-parity was passed, so this run reports no parity number"
                );
                return Ok(());
            }
            bail!(
                "MSL {PARITY_UNMEASURED_HEADLINE} ({reason}). \
                 sim_ok is completion, never parity: a run with no OMC comparison has no parity \
                 number to report. Install `omc`, or pass --allow-unmeasured-parity to accept a \
                 run that measures nothing."
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    fn write(dir: &Path, name: &str, body: &str) {
        fs::write(dir.join(name), body).expect("write fixture");
    }

    fn complete_reference(models_compared: usize) -> String {
        format!(r#"{{"trace_comparison":{{"models_compared":{models_compared}}}}}"#)
    }

    fn complete_comparison(models_compared: usize, unmeasured_initial: usize) -> String {
        format!(
            r#"{{"models_compared":{models_compared},"summary":{{"initial_condition":{{"models_with_unmeasured_initial_conditions":{unmeasured_initial}}}}}}}"#
        )
    }

    #[test]
    fn a_complete_pair_of_artifacts_is_measured() {
        let dir = tempdir().expect("tempdir");
        write(dir.path(), OMC_REFERENCE_FILE, &complete_reference(48));
        write(
            dir.path(),
            TRACE_COMPARISON_FILE,
            &complete_comparison(48, 0),
        );
        assert_eq!(
            read_comparator_evidence(dir.path()),
            ComparatorEvidence::Measured {
                models_compared: 48
            }
        );
        check_comparator_evidence(dir.path(), false).expect("a measured run passes");
    }

    #[test]
    fn a_missing_reference_is_unmeasured_and_fails() {
        let dir = tempdir().expect("tempdir");
        assert!(matches!(
            read_comparator_evidence(dir.path()),
            ComparatorEvidence::Unmeasured { .. }
        ));
        let error = check_comparator_evidence(dir.path(), false)
            .expect_err("a run with no OMC reference must fail");
        let message = format!("{error:#}");
        assert!(
            message.contains(PARITY_UNMEASURED_HEADLINE),
            "got: {message}"
        );
        assert!(message.contains(OMC_REFERENCE_FILE), "got: {message}");
    }

    #[test]
    fn a_reference_without_a_trace_comparison_file_is_unmeasured() {
        let dir = tempdir().expect("tempdir");
        write(dir.path(), OMC_REFERENCE_FILE, &complete_reference(48));
        let error = check_comparator_evidence(dir.path(), false)
            .expect_err("a reference with no trace comparison is not a comparison");
        assert!(format!("{error:#}").contains(TRACE_COMPARISON_FILE));
    }

    #[test]
    fn a_reference_that_compared_nothing_is_unmeasured() {
        let dir = tempdir().expect("tempdir");
        write(dir.path(), OMC_REFERENCE_FILE, &complete_reference(0));
        write(
            dir.path(),
            TRACE_COMPARISON_FILE,
            &complete_comparison(0, 0),
        );
        let error = check_comparator_evidence(dir.path(), false)
            .expect_err("comparing zero models is not a parity reading");
        assert!(format!("{error:#}").contains("models_compared = 0"));
    }

    #[test]
    fn an_empty_reference_file_is_unmeasured() {
        let dir = tempdir().expect("tempdir");
        write(dir.path(), OMC_REFERENCE_FILE, "");
        write(dir.path(), TRACE_COMPARISON_FILE, "{}");
        let error = check_comparator_evidence(dir.path(), false)
            .expect_err("an empty reference is not a reading");
        assert!(format!("{error:#}").contains("is empty"));
    }

    #[test]
    fn a_corrupt_reference_is_unmeasured_rather_than_ignored() {
        let dir = tempdir().expect("tempdir");
        write(dir.path(), OMC_REFERENCE_FILE, "{not json");
        write(dir.path(), TRACE_COMPARISON_FILE, "{}");
        let error = check_comparator_evidence(dir.path(), false)
            .expect_err("unparseable reference must not read as a pass");
        assert!(format!("{error:#}").contains("not valid JSON"));
    }

    #[test]
    fn a_model_without_exact_start_initialization_evidence_is_unmeasured() {
        let dir = tempdir().expect("tempdir");
        write(dir.path(), OMC_REFERENCE_FILE, &complete_reference(48));
        write(
            dir.path(),
            TRACE_COMPARISON_FILE,
            &complete_comparison(48, 1),
        );
        let error = check_comparator_evidence(dir.path(), false)
            .expect_err("missing initialization evidence must fail closed");
        assert!(format!("{error:#}").contains("without exact-start initialization evidence"));
    }

    #[test]
    fn a_focused_run_has_no_unmeasured_bypass() {
        let dir = tempdir().expect("tempdir");
        check_comparator_evidence(dir.path(), false)
            .expect_err("selecting named targets must not suppress comparator evidence");
    }

    #[test]
    fn the_escape_hatch_downgrades_the_failure_but_keeps_the_headline() {
        let dir = tempdir().expect("tempdir");
        check_comparator_evidence(dir.path(), true)
            .expect("--allow-unmeasured-parity accepts a run that measures nothing");
    }
}
