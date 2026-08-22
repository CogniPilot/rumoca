//! Judging one measured run against its pin.
//!
//! Every deviation is red, in both directions. A row pinned to compile that
//! stops compiling is the obvious failure; a row pinned to be refused that
//! starts compiling is equally red, because the manifest is the reviewed record
//! of what the compiler accepts and an unreviewed widening of that set is the
//! silent drift this gate exists to catch. Making the improvement green is one
//! line of manifest diff, and that diff is the point.

use super::execution::{ModelRun, first_diagnostic_code};
use super::manifest::{Check, CorpusEntry, Expectation, PinnedObservation};

/// The outcome of judging one row.
pub(crate) struct Verdict {
    pub(crate) id: String,
    /// Empty when the row matched its pin.
    pub(crate) findings: Vec<String>,
    pub(crate) elapsed_seconds: f64,
    pub(crate) command_line: String,
}

impl Verdict {
    pub(crate) fn passed(&self) -> bool {
        self.findings.is_empty()
    }
}

pub(crate) fn judge(entry: &CorpusEntry, run: &ModelRun) -> Verdict {
    let findings = match &entry.expect {
        Expectation::Succeeds { observations } => judge_success(entry, run, observations),
        Expectation::Refused { diagnostic } => judge_refusal(run, diagnostic),
    };
    Verdict {
        id: entry.id.clone(),
        findings,
        elapsed_seconds: run.elapsed.as_secs_f64(),
        command_line: run.command_line.clone(),
    }
}

fn judge_success(
    entry: &CorpusEntry,
    run: &ModelRun,
    observations: &[PinnedObservation],
) -> Vec<String> {
    if !run.succeeded {
        return vec![format!(
            "pinned to succeed but the run failed{}\n{}",
            first_diagnostic_code(&run.output)
                .map(|code| format!(" with {code}"))
                .unwrap_or_default(),
            indent(&tail(&run.output))
        )];
    }
    if observations.is_empty() {
        return match &entry.check {
            Check::Compile { target: _ } => Vec::new(),
            Check::Simulate {
                t_end: _,
                dt: _,
                solver: _,
            } => vec![
                "pinned to succeed but pins no reading, so this row proves only that the \
                 process exited zero. Record its readings with `--record` and copy the \
                 adjudicated numbers into the manifest."
                    .to_string(),
            ],
        };
    }
    let trace = match run.trace() {
        Ok(trace) => trace,
        Err(error) => {
            return vec![format!(
                "pinned readings could not be checked: {error:#}. The run exited zero, so \
                 the trace is missing or unreadable rather than absent by design."
            )];
        }
    };
    observations
        .iter()
        .filter_map(|observation| compare(entry, &trace, observation))
        .collect()
}

fn compare(
    entry: &CorpusEntry,
    trace: &super::trace::Trace,
    observation: &PinnedObservation,
) -> Option<String> {
    let PinnedObservation {
        variable,
        time,
        value,
        tolerance,
    } = observation;
    let measured = match trace.value_at(variable, *time) {
        Ok(measured) => measured,
        Err(error) => return Some(format!("`{variable}` at t={time}: {error:#}")),
    };
    let deviation = (measured - value).abs();
    if deviation <= *tolerance {
        return None;
    }
    Some(format!(
        "`{variable}` at t={time} is {measured} but {model} is pinned to {value} \
         (deviation {deviation:.3e}, tolerance {tolerance:.3e}). One of the two is wrong about \
         what this model means: adjudicate before editing either number.",
        model = entry.model
    ))
}

fn judge_refusal(run: &ModelRun, diagnostic: &str) -> Vec<String> {
    if run.succeeded {
        return vec![format!(
            "pinned to be refused with {diagnostic}, but the run succeeded. If the compiler \
             genuinely got better, record that in the manifest so the improvement is reviewed."
        )];
    }
    match first_diagnostic_code(&run.output) {
        Some(found) if found == diagnostic => Vec::new(),
        Some(found) => vec![format!(
            "refused with {found} but pinned to {diagnostic}. A changed refusal reason is a \
             behavior change:\n{}",
            indent(&tail(&run.output))
        )],
        None => vec![format!(
            "refused without any diagnostic code, but pinned to {diagnostic}:\n{}",
            indent(&tail(&run.output))
        )],
    }
}

/// The last few lines of child output: enough to identify the failure without
/// burying the report under a full MSL diagnostic dump.
fn tail(text: &str) -> String {
    const LINES: usize = 12;
    let lines: Vec<&str> = text
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();
    let start = lines.len().saturating_sub(LINES);
    lines[start..].join("\n")
}

fn indent(text: &str) -> String {
    text.lines()
        .map(|line| format!("      {line}"))
        .collect::<Vec<_>>()
        .join("\n")
}
