//! Project an issued Boolean periodic activation onto its GALEC task.

use super::*;

pub(super) fn runs_in_domain<'dae>(
    view: dae::DaeView<'dae>,
    owners: &HashMap<u32, u32>,
    target: dae::VariableId<'dae>,
    activation: dae::DiscreteRealActivation<'dae>,
    clock: dae::ClockId<'dae>,
    span: Span,
) -> Result<bool, GalecTargetError> {
    if let Some(owner) = owners.get(&target.index()) {
        return Ok(*owner == clock.index());
    }
    let dae::DiscreteRealActivation::When { trigger, .. } = activation else {
        return Err(unsupported(
            "clock-domain",
            "discrete Real definition has no explicit clock or periodic activation".to_owned(),
            span,
        ));
    };
    let clocks = condition_clocks(view, trigger);
    if clocks.len() != 1 {
        return Err(unsupported(
            "clock-domain",
            "a discrete Real activation must select one periodic task".to_owned(),
            span,
        ));
    }
    if !clocks.contains(&clock.index()) {
        return Ok(false);
    }
    require_periodic_trigger(view, trigger, clock, span)?;
    Ok(true)
}
