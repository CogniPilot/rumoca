use std::collections::HashMap;

use rumoca_core::{InstanceId, PeriodicClockSchedule, Span, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use super::Coordinate;
use super::analysis::{ClockOwnerId, ClockPlan, ClockTransferPlans, ClockedValuePlan};

pub(super) struct LoweredClocks<'dae> {
    pub(super) by_owner: HashMap<ClockOwnerId, dae::PeriodicClockId<'dae>>,
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
        self.by_owner
            .get(&plan.owner)
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
    transfers: &ClockTransferPlans,
    sample_schedules: impl Iterator<Item = (PeriodicClockSchedule, Span)>,
) -> Result<LoweredClocks<'dae>, dae::DaeConstructionError> {
    let mut plan_ids = HashMap::new();
    let mut issued_plans = HashMap::new();
    let mut coordinate_ids = HashMap::new();
    let mut planned_coordinates = flat
        .variables
        .iter()
        .filter_map(|(name, variable)| {
            plans
                .get(&variable.instance_id)
                .copied()
                .map(|plan| (name, plan))
        })
        .collect::<Vec<_>>();
    planned_coordinates.sort_by_key(|(_, plan)| plan.order_key());
    for (name, plan) in planned_coordinates {
        let clock = lower_clock_plan(construction, &mut plan_ids, &mut issued_plans, plan)?;
        coordinate_ids.insert(name.clone(), clock);
    }
    for (_, value) in clocked_values_in_instance_order(clocked_values) {
        lower_clock_plan(construction, &mut plan_ids, &mut issued_plans, value.clock)?;
    }
    let mut transfer_plans = transfers
        .values()
        .flat_map(|transfer| [transfer.source, transfer.target])
        .collect::<Vec<_>>();
    transfer_plans.sort_by_key(|plan| plan.order_key());
    for plan in transfer_plans {
        lower_clock_plan(construction, &mut plan_ids, &mut issued_plans, plan)?;
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
        by_owner: plan_ids,
        by_sample_schedule: sample_ids,
        by_coordinate: coordinate_ids,
    })
}

fn lower_clock_plan<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    ids: &mut HashMap<ClockOwnerId, dae::PeriodicClockId<'dae>>,
    issued: &mut HashMap<ClockOwnerId, ClockPlan>,
    plan: ClockPlan,
) -> Result<dae::PeriodicClockId<'dae>, dae::DaeConstructionError> {
    if let Some(established) = issued.get(&plan.owner).copied() {
        if !established.matches_exactly(plan) {
            return Err(
                dae::DaeConstructionError::ConflictingExpressionClockDomains {
                    established: established.constructor_span,
                    attempted: plan.constructor_span,
                },
            );
        }
        return ids.get(&plan.owner).copied().ok_or(
            dae::DaeConstructionError::MissingClockDomainOwner {
                span: plan.constructor_span,
            },
        );
    }
    let provenance = dae::DaeProvenance::source(plan.constructor_span)?;
    let clock = construction.clocks(|clocks| clocks.periodic(plan.lattice, provenance))?;
    issued.insert(plan.owner, plan);
    ids.insert(plan.owner, clock);
    Ok(clock)
}

fn clocked_values_in_instance_order(
    values: &HashMap<InstanceId, ClockedValuePlan>,
) -> Vec<(InstanceId, &ClockedValuePlan)> {
    let mut ordered: Vec<_> = values.iter().collect();
    ordered.sort_unstable_by_key(|(instance, _)| instance.index());
    ordered
        .into_iter()
        .map(|(instance, plan)| (*instance, plan))
        .collect()
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

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ClockLattice, ClockRational};

    use crate::construction::analysis::ClockOwnerId;

    fn plan() -> ClockedValuePlan {
        ClockedValuePlan {
            clock: ClockPlan::periodic(
                ClockOwnerId::Coordinate(InstanceId::new(1)),
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
                Span::DUMMY,
            ),
            ownership_span: Span::DUMMY,
            sampled: false,
        }
    }

    #[test]
    fn clocked_value_allocation_order_uses_instance_identity() {
        let mut values = HashMap::new();
        values.insert(InstanceId::new(9), plan());
        values.insert(InstanceId::new(2), plan());
        values.insert(InstanceId::new(5), plan());

        let ids: Vec<_> = clocked_values_in_instance_order(&values)
            .into_iter()
            .map(|(instance, _)| instance.index())
            .collect();

        assert_eq!(ids, [2, 5, 9]);
    }

    #[test]
    fn same_owner_with_different_lattice_rejects_before_clock_reuse() {
        let mut sources = rumoca_core::SourceMap::new();
        let source = sources.add("clock_plan_conflict.mo", "Clock(1) Clock(2)");
        let first_span = Span::from_offsets(source, 0, 8);
        let second_span = Span::from_offsets(source, 9, 17);
        let owner = ClockOwnerId::Coordinate(InstanceId::new(7));
        let first = ClockPlan::periodic(
            owner,
            ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
            first_span,
        );
        let second = ClockPlan::periodic(
            owner,
            ClockLattice::new(ClockRational::new(2, 1).unwrap(), ClockRational::ZERO).unwrap(),
            second_span,
        );
        let error = dae::Dae::construct(sources, |construction| {
            let mut ids = HashMap::new();
            let mut issued = HashMap::new();
            lower_clock_plan(construction, &mut ids, &mut issued, first)?;
            lower_clock_plan(construction, &mut ids, &mut issued, second)?;
            Ok(())
        })
        .expect_err("one semantic owner cannot carry two plans");
        assert!(matches!(
            error,
            dae::DaeConstructionError::ConflictingExpressionClockDomains {
                established,
                attempted,
            } if established == first_span && attempted == second_span
        ));
    }
}
