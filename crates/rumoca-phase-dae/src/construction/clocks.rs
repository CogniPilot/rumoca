use std::collections::HashMap;

use rumoca_core::VarName;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use super::Coordinate;
use super::analysis::{ClockPlan, SampledValuePlan};

pub(super) struct LoweredClocks<'dae> {
    pub(super) by_plan: HashMap<ClockPlan, dae::ClockId<'dae>>,
    pub(super) by_variable: HashMap<VarName, dae::ClockId<'dae>>,
}

pub(super) fn lower_clocks<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    plans: &HashMap<VarName, ClockPlan>,
) -> Result<LoweredClocks<'dae>, dae::DaeConstructionError> {
    let mut plan_ids = HashMap::new();
    let mut variable_ids = HashMap::new();
    for name in flat.variables.keys() {
        let Some(plan) = plans.get(name).copied() else {
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
        variable_ids.insert(name.clone(), clock);
    }
    Ok(LoweredClocks {
        by_plan: plan_ids,
        by_variable: variable_ids,
    })
}

pub(super) fn lower_sampled_value_clocks<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    sampled_values: &HashMap<VarName, SampledValuePlan>,
    clocks: &LoweredClocks<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    for name in flat.variables.keys() {
        let Some(plan) = sampled_values.get(name).copied() else {
            continue;
        };
        let clock = clocks.by_plan[&plan.clock];
        let ownership = dae::DaeProvenance::source(plan.sample_span)?;
        construction.clocks(|clocks| match coordinates[name] {
            Coordinate::DiscreteReal(variable) => {
                clocks.own_discrete_real(clock, variable, ownership)?;
                Ok(())
            }
            Coordinate::DiscreteValue(variable) => {
                clocks.own_discrete_value(clock, variable, ownership)?;
                Ok(())
            }
            _ => unreachable!("sample ownership analysis classifies a discrete coordinate"),
        })?;
    }
    Ok(())
}
