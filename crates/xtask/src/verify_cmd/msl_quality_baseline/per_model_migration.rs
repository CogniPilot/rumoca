//! Migration integrity judged per model: a baseline-wide count may rise only
//! where no model lost ground, when both sides of the crossing record each
//! model's stage and trace.

use super::*;

/// More initial-condition solver failures are a regression only when a model
/// that initialized before no longer does. A model that newly reaches
/// initialization and fails it (it compiled, or timed out, before) got further,
/// and a model flipping between a timeout and an initialization failure was
/// never initialized on either side. So with each side's model stages the rise
/// is judged per model; without them, the count may not rise.
pub(super) fn ensure_initialization_not_lost(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
) -> Result<()> {
    let label = "initial-condition solver failures";
    if checked_in.ic_solver_fail <= promoted.ic_solver_fail {
        return Ok(());
    }
    let (Some(before), Some(after)) = (&promoted.model_states, &checked_in.model_states) else {
        return ensure_not_raised(label, promoted.ic_solver_fail, checked_in.ic_solver_fail);
    };
    let initialized = |stage: &rumoca_msl_quality::ModelStage| {
        *stage >= rumoca_msl_quality::ModelStage::Initialized
    };
    // Walk the old side: a model that initialized before is lost whether the
    // candidate records it below initialization or does not record it at all.
    let lost: Vec<&str> = before
        .iter()
        .filter(|(model, old)| {
            initialized(&old.stage)
                && after
                    .get(*model)
                    .is_none_or(|state| !initialized(&state.stage))
        })
        .map(|(model, _)| model.as_str())
        .collect();
    ensure!(
        lost.is_empty(),
        "MSL migration raises unrelated {label} (promoted={}, checked-in={}): initialized before and no longer: {}",
        promoted.ic_solver_fail,
        checked_in.ic_solver_fail,
        lost.join(", ")
    );
    for (model, state) in after {
        let old = before.get(model);
        let newly_failed = state.outcome.as_deref() == Some("ic_solver_fail")
            && old.is_none_or(|old| old.outcome.as_deref() != Some("ic_solver_fail"));
        if newly_failed {
            println!(
                "MSL quality baseline: {label} {} -> {} judged per model: {model} now reaches initialization and fails it (before: {}); it never initialized, so nothing was lost",
                promoted.ic_solver_fail,
                checked_in.ic_solver_fail,
                old.map_or("not measured".to_string(), |old| format!(
                    "{:?}/{}",
                    old.stage,
                    old.outcome.as_deref().unwrap_or("-")
                ))
            );
        }
    }
    Ok(())
}

/// The promoted header with the model stages of its side of the crossing:
/// its own when it records them, otherwise those of the evidence the checked-in
/// baseline's crossed reference boundaries pin (sha-verified). With no such
/// evidence the header is returned as it is, and counts are judged instead.
pub(super) fn with_old_side_states(
    root: &Path,
    promoted_path: &Path,
    mut promoted: MslQualityBaselineHeader,
    checked_in_path: &Path,
) -> Result<MslQualityBaselineHeader> {
    if promoted.model_states.is_some() {
        return Ok(promoted);
    }
    let read = |path: &Path| -> Result<serde_json::Value> {
        let bytes = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
        serde_json::from_slice(&bytes)
            .with_context(|| format!("invalid MSL quality baseline JSON in {}", path.display()))
    };
    let (promoted_value, checked_in_value) = (read(promoted_path)?, read(checked_in_path)?);
    let Ok(crossed) =
        rumoca_msl_quality::snapshot::crossed_links(&checked_in_value, &promoted_value)
    else {
        return Ok(promoted);
    };
    if !crossed
        .iter()
        .any(|link| link.get("evidence_model_states").is_some())
    {
        return Ok(promoted);
    }
    promoted.model_states = Some(
        rumoca_msl_quality::snapshot::previous_states(&promoted_value, &crossed, root)
            .map_err(anyhow::Error::msg)?,
    );
    Ok(promoted)
}

/// A model's trace band (none when unscored) and its violation mass.
fn trace_of(
    state: &rumoca_msl_quality::ModelState,
) -> (Option<rumoca_msl_quality::BandLabel>, f64) {
    let band = match state.trace {
        rumoca_msl_quality::TraceState::Scored(band) => Some(band),
        rumoca_msl_quality::TraceState::Unscored(_) => None,
    };
    (band, state.trace_violation_mass.unwrap_or(0.0))
}

/// A reviewed roster addition's channel counts, from its reviewed entry:
/// `deviation_channels` counts every channel in the deviation band, severe ones
/// included (the comparator's `channel_deviation_count`, summed into
/// `bad_channels_total`); `severe_channels` is the subset at or above the
/// severe threshold (summed into `severe_channels_total`).
pub(super) fn reviewed_channels(model: &str, entry: &serde_json::Value) -> Result<(usize, usize)> {
    let count = |key: &str| {
        entry
            .get(key)
            .and_then(serde_json::Value::as_u64)
            .and_then(|count| usize::try_from(count).ok())
            .with_context(|| {
                format!("the reviewed roster addition {model} records no {key} to deduct")
            })
    };
    Ok((count("deviation_channels")?, count("severe_channels")?))
}

/// The trace-error totals (deviation models, models with bad channels, bad and
/// severe channels, violation mass) may rise across a crossing only by the
/// models its reviewed boundaries add to the roster. With each side's model
/// stages, a model outside those additions whose band or violation mass
/// worsened is refused by name. Each reviewed addition that was unscored before
/// is recorded and its own contribution deducted exactly: its band and mass
/// from the candidate, its deviation and severe channel counts from its
/// reviewed entry (severe within deviation within its compared channels, and
/// at least one deviation channel, since the model is not high). The remainder must
/// still hold on every total. The additions come from the compiled-in reviewed
/// chain, never from the candidate. Without stages on both sides the totals may
/// not rise.
pub(super) fn ensure_trace_error_not_raised(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
) -> Result<()> {
    let (before_trace, after_trace) = (
        &promoted.trace_accuracy_stats,
        &checked_in.trace_accuracy_stats,
    );
    let (Some(before), Some(after)) = (&promoted.model_states, &checked_in.model_states) else {
        return validate_omc_error_metric_integrity(before_trace, after_trace);
    };
    if validate_omc_error_metric_integrity(before_trace, after_trace).is_ok() {
        return Ok(());
    }
    let chain = super::reference_boundary::reviewed_chain_snapshot()?;
    let reviewed = super::ratchet::crossed_roster_addition_entries(
        &chain,
        &serde_json::json!({ "quality_gate_version": promoted.quality_gate_version }),
    )?;
    judge_trace_errors((before_trace, before), (after_trace, after), &reviewed)
}

/// [`ensure_trace_error_not_raised`] once both sides' model stages and the
/// crossed reviewed roster additions are known.
pub(super) fn judge_trace_errors(
    (before_trace, before): (&TraceAccuracyStats, &rumoca_msl_quality::ModelStates),
    (after_trace, after): (&TraceAccuracyStats, &rumoca_msl_quality::ModelStates),
    reviewed: &std::collections::BTreeMap<String, &serde_json::Value>,
) -> Result<()> {
    let mut remainder = after_trace.clone();
    let mut worsened = Vec::new();
    for (model, state) in after {
        let (band, mass) = trace_of(state);
        if band.is_none_or(|band| band == rumoca_msl_quality::BandLabel::High) {
            continue;
        }
        let (old_band, old_mass) = before.get(model).map_or((None, 0.0), trace_of);
        let got_worse = match old_band {
            None | Some(rumoca_msl_quality::BandLabel::High) => true,
            Some(old) => band > Some(old) || mass > old_mass + 1.0e-9,
        };
        if !got_worse {
            continue;
        }
        let Some(entry) = reviewed.get(model).filter(|_| old_band.is_none()) else {
            worsened.push(model.as_str());
            continue;
        };
        let (deviation, severe) = reviewed_channels(model, entry)?;
        ensure!(
            0 < deviation && severe <= deviation && deviation <= state.compared_channels,
            "the reviewed roster addition {model} records {deviation} deviation and {severe} severe channels (severe being a subset), which do not fit its {} compared channels and {band:?} band",
            state.compared_channels
        );
        println!(
            "MSL quality baseline: trace errors judged per model: {model} is newly scored as a reviewed roster addition ({band:?}, {deviation} deviation and {severe} severe channels, violation mass {mass:.4e}); deducted"
        );
        if band == Some(rumoca_msl_quality::BandLabel::Deviation) {
            remainder.agreement_deviation = remainder.agreement_deviation.saturating_sub(1);
        }
        if deviation > 0 {
            remainder.models_with_any_channel_deviation = remainder
                .models_with_any_channel_deviation
                .saturating_sub(1);
        }
        remainder.bad_channels_total = remainder.bad_channels_total.saturating_sub(deviation);
        remainder.severe_channels_total = remainder.severe_channels_total.saturating_sub(severe);
        remainder.violation_mass_total -= mass;
    }
    ensure!(
        worsened.is_empty(),
        "MSL migration worsens the trace of models no reviewed boundary adds to the roster: {}",
        worsened.join(", ")
    );
    validate_omc_error_metric_integrity(before_trace, &remainder)
}
