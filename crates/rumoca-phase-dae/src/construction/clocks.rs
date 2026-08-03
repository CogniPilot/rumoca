use std::collections::HashMap;

use rumoca_core::{InstanceId, PeriodicClockSchedule, Span, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use super::Coordinate;
use super::analysis::{ClockPlan, ClockedValuePlan};

pub(super) struct LoweredClocks<'dae> {
    pub(super) by_plan: HashMap<ClockPlan, dae::PeriodicClockId<'dae>>,
    /// MLS §3.7.5 event clocks are identified entirely by their exact
    /// periodic schedule. Every occurrence and exact Boolean alias reuses this
    /// one owner rather than allocating parallel activation lanes.
    by_sample_schedule: HashMap<PeriodicClockSchedule, dae::PeriodicClockId<'dae>>,
    /// Clock coordinates keyed by their Flat catalog name, the identity every
    /// Flat expression occurrence names. `Reference::instance_id` carries the
    /// enclosing class occurrence, so it cannot select a referenced coordinate.
    pub(super) by_coordinate: HashMap<VarName, dae::PeriodicClockId<'dae>>,
}

impl<'dae> LoweredClocks<'dae> {
    pub(super) fn id(
        &self,
        plan: &ClockPlan,
        span: rumoca_core::Span,
    ) -> Result<dae::PeriodicClockId<'dae>, dae::DaeConstructionError> {
        self.by_plan
            .get(plan)
            .copied()
            .ok_or(dae::DaeConstructionError::MissingClockDomainOwner { span })
    }

    pub(super) fn sample_id(
        &self,
        schedule: PeriodicClockSchedule,
        span: Span,
    ) -> Result<dae::PeriodicClockId<'dae>, dae::DaeConstructionError> {
        self.by_sample_schedule
            .get(&schedule)
            .copied()
            .ok_or(dae::DaeConstructionError::MissingClockDomainOwner { span })
    }
}

pub(super) fn lower_clocks<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    clocked_values: &HashMap<InstanceId, ClockedValuePlan>,
    sample_schedules: impl Iterator<Item = (PeriodicClockSchedule, Span)>,
) -> Result<LoweredClocks<'dae>, dae::DaeConstructionError> {
    let mut plan_ids = HashMap::new();
    let mut coordinate_ids = HashMap::new();
    for (name, variable) in &flat.variables {
        let Some(plan) = plans.get(&variable.instance_id).copied() else {
            continue;
        };
        let clock = if let Some(clock) = plan_ids.get(&plan).copied() {
            clock
        } else {
            let provenance = dae::DaeProvenance::source(plan.constructor_span)?;
            let clock = construction.clocks(|clocks| clocks.periodic(plan.lattice, provenance))?;
            plan_ids.insert(plan, clock);
            clock
        };
        coordinate_ids.insert(name.clone(), clock);
    }
    for value in clocked_values.values() {
        if plan_ids.contains_key(&value.clock) {
            continue;
        }
        let provenance = dae::DaeProvenance::source(value.clock.constructor_span)?;
        let clock =
            construction.clocks(|clocks| clocks.periodic(value.clock.lattice, provenance))?;
        plan_ids.insert(value.clock, clock);
    }
    let mut sample_ids = HashMap::new();
    for (schedule, span) in sample_schedules {
        if sample_ids.contains_key(&schedule) {
            continue;
        }
        let provenance = dae::DaeProvenance::source(span)?;
        let clock = construction.clocks(|clocks| clocks.scheduled(schedule, provenance))?;
        sample_ids.insert(schedule, clock);
    }
    Ok(LoweredClocks {
        by_plan: plan_ids,
        by_sample_schedule: sample_ids,
        by_coordinate: coordinate_ids,
    })
}

pub(super) fn lower_clocked_value_owners<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    clocked_values: &HashMap<InstanceId, ClockedValuePlan>,
    clocks: &LoweredClocks<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    for (name, variable) in &flat.variables {
        let Some(plan) = clocked_values.get(&variable.instance_id).copied() else {
            continue;
        };
        let clock = clocks.id(&plan.clock, plan.ownership_span)?;
        let ownership = dae::DaeProvenance::source(plan.ownership_span)?;
        let coordinate = coordinates.get(name).copied().ok_or_else(|| {
            dae::DaeConstructionError::InvalidVariableRole {
                name: name.clone(),
                span: plan.ownership_span,
            }
        })?;
        construction.clocks(|clocks| match coordinate {
            Coordinate::DiscreteReal(variable) if plan.sampled => {
                clocks.own_sampled_discrete_real(clock.into(), variable, ownership)?;
                Ok(())
            }
            Coordinate::DiscreteReal(variable) => {
                clocks.own_discrete_real(clock.into(), variable, ownership)?;
                Ok(())
            }
            Coordinate::DiscreteValue(variable) if plan.sampled => {
                clocks.own_sampled_discrete_value(clock.into(), variable, ownership)?;
                Ok(())
            }
            Coordinate::DiscreteValue(variable) => {
                clocks.own_discrete_value(clock.into(), variable, ownership)?;
                Ok(())
            }
            _ => Err(dae::DaeConstructionError::InvalidVariableRole {
                name: name.clone(),
                span: plan.ownership_span,
            }),
        })?;
    }
    Ok(())
}
