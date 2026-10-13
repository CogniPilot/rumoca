use super::*;
use rumoca_ir_solve::fmi::IndicatorReading;

/// Reserve every indicator working buffer, each sized to the plan so a read
/// reserves nothing proportional to the model.
pub(super) fn reserve_indicator_scratch(
    indicator_plan: &FmiIndicatorPlan,
) -> Result<IndicatorScratch, MeError> {
    Ok(IndicatorScratch {
        indicator_value_scratch: reserved_indicator_values(
            indicator_plan.len(),
            "event-indicator values",
        )?,
        indicator_root_scratch: reserved_indicator_values(
            indicator_plan.root_value_len(),
            "event-indicator roots",
        )?,
        indicator_deadline_scratch: reserved_indicator_values(
            indicator_plan.deadline_len(),
            "event-indicator dynamic-time deadlines",
        )?,
        indicator_domain_scratch: reserved_indicator_domains(indicator_plan.len())?,
        frozen_indicator_positive: reserved_indicator_domains(indicator_plan.len())?,
    })
}

/// The scalar value one resolved indicator position reports.
pub(super) fn indicator_reading_value(
    reading: IndicatorReading,
    time: f64,
    root_values: &[f64],
    deadlines: &[f64],
) -> Result<f64, MeError> {
    match reading {
        IndicatorReading::RootValue { index } => root_values
            .get(index)
            .copied()
            .ok_or_else(|| contract("FMI root indicator source is out of range")),
        IndicatorReading::DeadlineDistance { index } => deadlines
            .get(index)
            .copied()
            .map(|deadline| deadline - time)
            .ok_or_else(|| contract("FMI dynamic-time indicator source is out of range")),
    }
}

/// The resolved indicator table together with the addresses of the buffers
/// every indicator read uses.
#[cfg(test)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct IndicatorStorageIdentity {
    pub(super) entries: Vec<(IndicatorReading, IndicatorZeroSide, Option<usize>)>,
    pub(super) root_value_len: usize,
    pub(super) deadline_len: usize,
    pub(super) root_buffer: usize,
    pub(super) deadline_buffer: usize,
    pub(super) value_buffer: usize,
    /// The frozen-domain buffer and its working buffer, which the completed-step
    /// callback swaps, so the pair is compared as a set.
    pub(super) domain_buffers: [usize; 2],
}

/// Reserve one indicator working buffer at instantiation.
///
/// Every FMI event-indicator buffer this component reads is sized here, so a
/// reservation failure is an instantiation refusal rather than a per-step
/// abort, and the step path never grows a buffer.
fn reserved_indicator_values(entries: usize, context: &'static str) -> Result<Vec<f64>, MeError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(entries)
        .map_err(|_| MeError::Allocation { context, entries })?;
    values.resize(entries, 0.0);
    Ok(values)
}

/// Reserve one indicator-domain buffer at instantiation.
///
/// The completed-step callback swaps the frozen domains with their working
/// buffer, so both start with the whole inventory reserved and neither grows
/// afterward.
fn reserved_indicator_domains(entries: usize) -> Result<Vec<bool>, MeError> {
    let mut domains = Vec::new();
    domains
        .try_reserve_exact(entries)
        .map_err(|_| MeError::Allocation {
            context: "event-indicator domains",
            entries,
        })?;
    Ok(domains)
}
