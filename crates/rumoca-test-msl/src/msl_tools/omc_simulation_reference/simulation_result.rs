//! Same-request OMC simulation evidence, before reference trace admission.

use std::io;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use super::super::common::has_fatal_omc_error;
use super::{OmcSimOutcome, SimModelResult, empty_omc_result, omc_assertion_failure_lines};
use super::{round3, summarize_omc_error_with_assertions};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub(super) struct FileRevision {
    bytes: u64,
    modified: SystemTime,
}

/// Issued only for the expected owned result changed by this simulation request.
#[derive(Debug, Clone)]
pub(super) struct FreshResultFile {
    path: PathBuf,
    declared: String,
    reply: String,
    messages: String,
    error: String,
}

impl FreshResultFile {
    pub(super) fn snapshot(path: &Path) -> io::Result<Option<FileRevision>> {
        match std::fs::symlink_metadata(path) {
            Ok(metadata) if metadata.is_file() => Ok(Some(FileRevision {
                bytes: metadata.len(),
                modified: metadata.modified()?,
            })),
            Ok(_) => Err(io::Error::other("OMC result is not a regular file")),
            Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(None),
            Err(error) => Err(error),
        }
    }

    pub(super) fn from_request(
        expected: &Path,
        prior: io::Result<Option<FileRevision>>,
        reply: &str,
        error: String,
    ) -> OmcSimOutcome {
        let mut outcome = super::omc_session::parse_sim_record(reply, error);
        outcome.fresh_result = Self::issue(expected, prior, &outcome);
        outcome
    }

    fn issue(
        expected: &Path,
        prior: io::Result<Option<FileRevision>>,
        outcome: &OmcSimOutcome,
    ) -> Option<Self> {
        if !outcome
            .raw_reply
            .trim()
            .strip_prefix("record SimulationResult")
            .is_some_and(|rest| rest.starts_with(char::is_whitespace))
            || !outcome.raw_reply.trim().ends_with("end SimulationResult;")
        {
            return None;
        }
        let declared = Path::new(outcome.result_file.as_deref()?);
        let current = Self::snapshot(expected).ok()??;
        let expected = std::fs::canonicalize(expected).ok()?;
        if std::fs::canonicalize(declared).ok()? != expected
            || !simulation_finished(&outcome.messages)
        {
            return None;
        }
        (current.bytes > 0 && prior.ok()? != Some(current)).then_some(Self {
            path: expected,
            declared: outcome.result_file.clone()?,
            reply: outcome.raw_reply.clone(),
            messages: outcome.messages.clone(),
            error: outcome.error.clone(),
        })
    }

    fn matches(&self, outcome: &OmcSimOutcome) -> bool {
        outcome
            .result_file
            .as_deref()
            .and_then(|path| std::fs::canonicalize(path).ok())
            .as_ref()
            == Some(&self.path)
            && outcome.result_file.as_deref() == Some(self.declared.as_str())
            && outcome.raw_reply == self.reply
            && outcome.messages == self.messages
            && outcome.error == self.error
    }
}

fn simulation_finished(messages: &str) -> bool {
    [
        "The initialization finished successfully",
        "The simulation finished successfully",
    ]
    .iter()
    .all(|message| {
        messages
            .lines()
            .any(|line| line.starts_with("LOG_SUCCESS") && line.contains(message))
    })
}

pub(super) fn build_session_model_result(outcome: &OmcSimOutcome, elapsed: f64) -> SimModelResult {
    let mut diagnostics = outcome.error.clone();
    if !diagnostics.is_empty() && !outcome.messages.is_empty() {
        diagnostics.push('\n');
    }
    diagnostics.push_str(&outcome.messages);
    let missing_result = outcome
        .result_file
        .as_deref()
        .is_none_or(|path| path.trim().is_empty());
    let fresh = outcome
        .fresh_result
        .as_ref()
        .is_some_and(|proof| proof.matches(outcome));
    let messages_failed = outcome.messages.contains("Simulation Failed")
        || outcome.messages.contains("Simulation execution failed")
        || outcome.messages.contains("does not exist")
        || outcome
            .messages
            .to_ascii_lowercase()
            .contains("simulation terminated")
        || has_fatal_omc_error(&outcome.messages)
        || outcome.messages.lines().any(|line| {
            line.starts_with("LOG_")
                && line
                    .split('|')
                    .nth(1)
                    .is_some_and(|level| level.trim() == "error")
        });
    let assertion_failures = simulation_assertion_failures(&outcome.error, &outcome.messages);
    let fatal = missing_result
        || !fresh
        || messages_failed
        || !assertion_failures.is_empty()
        || (has_fatal_omc_error(&outcome.error)
            && !recovered_sparse_pattern_errors(&outcome.error));
    if missing_result {
        diagnostics.push_str("\nOMC simulation returned no result file");
    } else if !fresh {
        diagnostics.push_str("\nOMC simulation returned no fresh same-request result");
    }
    SimModelResult {
        status: if fatal { "error" } else { "success" }.into(),
        error: fatal
            .then(|| summarize_omc_error_with_assertions(&diagnostics, &assertion_failures)),
        omc_diagnostics: (!diagnostics.is_empty()).then_some(diagnostics),
        sim_system_seconds: outcome.timing.simulation,
        total_system_seconds: outcome.timing.total,
        omc_phases: Some(outcome.timing.clone()),
        omc_settings: Some(outcome.settings.clone()),
        omc_wall_seconds: Some(round3(elapsed)),
        result_file: outcome.result_file.clone(),
        ..empty_omc_result()
    }
}

fn simulation_assertion_failures(error: &str, messages: &str) -> Vec<String> {
    let mut failures = omc_assertion_failure_lines(error);
    let mut warning_log = false;
    for line in messages.lines() {
        let line = line.trim_start();
        if line.starts_with("LOG_") {
            warning_log = line
                .split('|')
                .nth(1)
                .is_some_and(|level| level.trim() == "warning");
        } else if !line.starts_with('|') {
            warning_log = false;
        }
        if !warning_log {
            failures.extend(omc_assertion_failure_lines(line));
        }
    }
    failures
}

/// OMC explicitly recovered this optional optimization. Every error must belong
/// to its known backend stack, and the recovery must follow the entire stack.
fn recovered_sparse_pattern_errors(diagnostics: &str) -> bool {
    const RECOVERY: &str = "Warning: The optimization module detectJacobianSparsePattern failed. This module will be skipped and the transformation process continued.";
    const STACK: &[&str] = &[
        "Error: Internal error BackendDAETransform.analyseStrongComponentBlock failed",
        "Error: Internal error function analyseStrongComponentBlock failed",
        "Error: Internal error function analyseStrongComponentScalar failed",
        "Error: Internal error function strongComponentsScalar failed (sorting strong components)",
        "Error: Internal error Transformation module sort components failed",
    ];
    let mut saw_error = false;
    let mut recovered = false;
    for line in diagnostics.lines() {
        if has_fatal_omc_error(line) {
            let error = line
                .split_once("Error:")
                .map_or("", |(_, text)| text.trim());
            let known = STACK.iter().any(|message| {
                error == message.trim_start_matches("Error: ")
                    || error == format!(
                        "{} (Purely discrete algebraic loops cannot be solved by iterative processes. Try to break them open using the delay() operator.)",
                        message.trim_start_matches("Error: ")
                    )
            });
            if recovered || !known {
                return false;
            }
            saw_error = true;
        } else if line.trim() == RECOVERY {
            recovered = saw_error;
        }
    }
    recovered
}
