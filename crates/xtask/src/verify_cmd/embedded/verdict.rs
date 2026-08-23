//! Judging one measured artifact against its budget.
//!
//! Judgement is kept apart from measurement for the same reason the sibling
//! corpus gate does it: everything here is a pure function of numbers and
//! symbol names, so the whole decision layer is unit-testable on a machine with
//! no ARM toolchain on it. The gate's negative controls (a ceiling below the
//! measured size, a ceiling below the measured floating-point count, a planted
//! double-precision call, an allocator) are unit tests against these functions,
//! which is what lets them run everywhere rather than only where a cross
//! compiler is installed.
//!
//! A row that could not be measured at all is judged red here too, with the
//! reason and the command that failed. There is no third outcome: a skip would
//! report "within budget" for a run in which nothing was weighed.

use super::manifest::{Budget, BudgetEntry};
use super::symbols::{Forbidden, ObjectSymbols, scan};

/// What one row measured, once every tool has run.
pub(crate) struct Metrics {
    /// `.text` per emitted translation unit, keyed by object file name. The
    /// ceiling is on the sum: an artifact that moved code from one translation
    /// unit into another did not get smaller.
    pub(crate) text_units: Vec<(String, u64)>,
    pub(crate) state_bytes: u64,
    /// Single-precision arithmetic instructions per emitted translation unit,
    /// counted from the disassembly. Summed for the same reason `.text` is:
    /// arithmetic moved from one translation unit into another is arithmetic
    /// still executed.
    pub(crate) fp_units: Vec<(String, u64)>,
}

impl Metrics {
    pub(crate) fn text_bytes(&self) -> u64 {
        self.text_units.iter().map(|(_, bytes)| bytes).sum()
    }

    pub(crate) fn fp_ops(&self) -> u64 {
        self.fp_units.iter().map(|(_, count)| count).sum()
    }
}

/// The outcome of judging one row.
pub(crate) struct Verdict {
    pub(crate) id: String,
    /// Empty when the artifact is inside every budget and clean of forbidden
    /// symbols.
    pub(crate) findings: Vec<String>,
    /// `None` when the row could not be measured.
    pub(crate) metrics: Option<Metrics>,
    pub(crate) elapsed_seconds: f64,
    pub(crate) command_line: String,
}

impl Verdict {
    pub(crate) fn passed(&self) -> bool {
        self.findings.is_empty()
    }
}

/// A row that produced no measurement. Naming the row and the command that
/// failed is the whole contract: an operator has to be able to re-run exactly
/// what the gate ran.
pub(crate) fn unmeasured(
    entry: &BudgetEntry,
    error: &anyhow::Error,
    elapsed_seconds: f64,
) -> Verdict {
    Verdict {
        id: entry.id.clone(),
        findings: vec![format!(
            "`{}` for --target {} could not be measured, so this row certifies nothing:\n      \
             {error:#}",
            entry.model, entry.target
        )],
        metrics: None,
        elapsed_seconds,
        command_line: format!(
            "cargo xtask verify embedded --models-root <path> --arm-toolchain <path> --only {}",
            entry.id
        ),
    }
}

/// One ceiling, the measurement judged against it, and what both mean.
struct Ceiling<'a> {
    /// Name of the quantity, as the finding opens with it.
    what: &'a str,
    /// The word the numbers are counted in, singular.
    unit: &'a str,
    measured: u64,
    ceiling: u64,
    /// What the manifest recorded when the ceiling was set.
    recorded: u64,
    /// What the quantity is, in one clause.
    means: &'a str,
}

/// Judge a measured row: the three ceilings, then the symbol policy.
pub(crate) fn judge(
    entry: &BudgetEntry,
    metrics: Metrics,
    undefined: &[ObjectSymbols],
    elapsed_seconds: f64,
    command_line: String,
) -> Verdict {
    let mut findings = Vec::new();
    findings.extend(over_ceiling(&Ceiling {
        what: "text",
        unit: "byte",
        measured: metrics.text_bytes(),
        ceiling: entry.budget.text_bytes,
        recorded: entry.budget.measured.text_bytes,
        means: ".text summed over every emitted translation unit",
    }));
    findings.extend(over_ceiling(&Ceiling {
        what: "state",
        unit: "byte",
        measured: metrics.state_bytes,
        ceiling: entry.budget.state_bytes,
        recorded: entry.budget.measured.state_bytes,
        means: "sizeof(<Model>State) for the target ABI",
    }));
    findings.extend(over_ceiling(&Ceiling {
        what: "fp",
        unit: "instruction",
        measured: metrics.fp_ops(),
        ceiling: entry.budget.fp_ops,
        recorded: entry.budget.measured.fp_ops,
        means: "single-precision arithmetic instructions summed over every emitted translation \
                unit, which is the work that decides whether the step makes its rate",
    }));
    findings.extend(forbidden_findings(&scan(undefined)));
    Verdict {
        id: entry.id.clone(),
        findings,
        metrics: Some(metrics),
        elapsed_seconds,
        command_line,
    }
}

/// One ceiling check. Naming both numbers, the excess, and the recorded
/// measurement the ceiling was set from is what makes the failure actionable:
/// it says whether the artifact grew or the ceiling was always too tight.
fn over_ceiling(check: &Ceiling<'_>) -> Option<String> {
    let Ceiling {
        what,
        unit,
        measured,
        ceiling,
        recorded,
        means,
    } = *check;
    if measured <= ceiling {
        return None;
    }
    Some(format!(
        "{what} is {measured} {unit}s, over the {ceiling} {unit} ceiling by {} ({means}). The \
         manifest records {recorded} {unit}s when the ceiling was set, so this artifact has \
         grown {} since. Ceilings come down as the work that earns them lands; raising one \
         needs a reviewed justification in the manifest.",
        measured - ceiling,
        drift(measured, recorded, unit),
    ))
}

/// Signed drift against the recorded measurement, in units and percent.
fn drift(measured: u64, recorded: u64, unit: &str) -> String {
    let delta = i128::from(measured) - i128::from(recorded);
    if recorded == 0 {
        return format!("{delta} {unit}s");
    }
    let percent = (delta as f64) * 100.0 / (recorded as f64);
    format!("{delta} {unit}s ({percent:+.1}%)")
}

fn forbidden_findings(forbidden: &[Forbidden]) -> Vec<String> {
    forbidden
        .iter()
        .map(|hit| {
            format!(
                "{} (undefined in {})",
                hit.reason.describe(&hit.symbol),
                hit.objects.join(", ")
            )
        })
        .collect()
}

/// The headroom line printed for every measured row, so a green run still says
/// how close each artifact is to each of its limits.
pub(crate) fn headroom(budget: &Budget, metrics: &Metrics) -> String {
    format!(
        "text {}/{} B, state {}/{} B, fp {}/{}",
        metrics.text_bytes(),
        budget.text_bytes,
        metrics.state_bytes,
        budget.state_bytes,
        metrics.fp_ops(),
        budget.fp_ops,
    )
}
