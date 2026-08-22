//! Judging one measured run against its pin.
//!
//! Every deviation is red, in both directions. A row pinned to compile that
//! stops compiling is the obvious failure; a row pinned to be refused that
//! starts compiling is equally red, because the manifest is the reviewed record
//! of what the compiler accepts and an unreviewed widening of that set is the
//! silent drift this gate exists to catch. Making the improvement green is one
//! line of manifest diff, and that diff is the point.
//!
//! Three outcomes are red without being a deviation from the pinned behavior,
//! because in each of them the row measured nothing: a run that outran its
//! deadline, a simulate row whose pins cannot see the run move
//! ([`super::observability`]), and a compile row that declares no output
//! artifact ([`super::artifacts`]). A green tick that means "nothing was
//! compared" is the one outcome an oracle net cannot survive, so each of them
//! fails instead.

use super::artifacts;
use super::execution::{ModelRun, first_diagnostic_code};
use super::manifest::{Check, CorpusEntry, Expectation, ExpectedArtifact, PinnedObservation};
use super::observability::{self, Unobserved};

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
    let findings = if run.timed_out {
        // A killed run produced no evidence for or against its pin, whichever
        // direction the pin points, so the timeout is the whole finding.
        vec![timed_out(run)]
    } else {
        match &entry.expect {
            Expectation::Succeeds {
                observations,
                artifacts,
            } => judge_success(entry, run, observations, artifacts),
            Expectation::Refused { diagnostic } => judge_refusal(run, diagnostic),
        }
    };
    Verdict {
        id: entry.id.clone(),
        findings,
        elapsed_seconds: run.elapsed.as_secs_f64(),
        command_line: run.command_line.clone(),
    }
}

fn timed_out(run: &ModelRun) -> String {
    format!(
        "timed out after {:.0}s and was killed: `{}` never finished. The deadline is this \
         row's share of the manifest's runtime_budget_seconds, sized as a hang catcher rather \
         than a performance assertion, so a row that outruns it is not slow, it stopped \
         terminating.",
        run.deadline.as_secs_f64(),
        run.command_line
    )
}

fn judge_success(
    entry: &CorpusEntry,
    run: &ModelRun,
    observations: &[PinnedObservation],
    artifacts: &[ExpectedArtifact],
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
    match &entry.check {
        Check::Compile { target: _ } => judge_artifacts(run, artifacts),
        Check::Simulate {
            t_end,
            dt: _,
            solver: _,
        } => judge_readings(entry, run, observations, *t_end),
    }
}

fn judge_artifacts(run: &ModelRun, artifacts: &[ExpectedArtifact]) -> Vec<String> {
    let Some(output_dir) = run.output_dir.as_deref() else {
        return vec![
            "pinned to compile but the run recorded no output path, so nothing it wrote can \
             be read back"
                .to_string(),
        ];
    };
    artifacts::judge(output_dir, artifacts)
}

fn judge_readings(
    entry: &CorpusEntry,
    run: &ModelRun,
    observations: &[PinnedObservation],
    t_end: f64,
) -> Vec<String> {
    if observations.is_empty() {
        // Settled before the trace is opened: a row that pins nothing is red
        // whether or not it wrote a readable trace, and reporting a trace
        // problem here would name the wrong defect.
        return vec![Unobserved::NoReadings.finding()];
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
    if let Some(unobserved) = observability::assess(observations, &trace, t_end) {
        // A pinned variable the trace does not carry is the sharper finding:
        // the unobserved verdict would tell the operator to pin something that
        // moves when the real defect is that the pinned name vanished. Both
        // paths stay red; only the message changes.
        let vanished: Vec<String> = observations
            .iter()
            .filter(|observation| trace.spread_until(&observation.variable, t_end).is_none())
            .filter_map(|observation| compare(entry, &trace, observation))
            .collect();
        if !vanished.is_empty() {
            return vanished;
        }
        return vec![unobserved.finding()];
    }
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
