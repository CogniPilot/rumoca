//! Checked final storage and helper linkage for exact schedules.
use super::*;
use crate::TypedCallFault;

pub(super) struct Ready {
    pub plan: Plan,
    pub arena: arena::ArenaPlan,
    pub calls: CallProgramPlan,
    pub imports: Vec<MathImport>,
    pub bodies: Vec<Function>,
    pub faults: Vec<TypedCallFault>,
}

impl Ready {
    pub(super) fn new(
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
        layout: &VarLayout,
        table: &solve::SolvePureCallTable,
    ) -> Result<Self, String> {
        checked_slot_count(layout.y_scalars())?;
        checked_slot_count(layout.p_scalars())?;
        let plan = Plan::new(source, owners, schedule, layout)?;
        Self::from_plan(plan, layout, table, layout.y_scalars())
    }

    pub(super) fn private(
        block: &solve::ScalarProgramBlock,
        layout: &VarLayout,
        table: &solve::SolvePureCallTable,
    ) -> Result<Self, String> {
        checked_slot_count(layout.y_scalars())?;
        checked_slot_count(layout.p_scalars())?;
        let plan = Plan::private(block)?;
        let work = layout.y_scalars().max(plan.maximum_tuple);
        Self::from_plan(plan, layout, table, work)
    }

    fn from_plan(
        plan: Plan,
        layout: &VarLayout,
        table: &solve::SolvePureCallTable,
        work: usize,
    ) -> Result<Self, String> {
        let programs = checked_programs(&plan, layout)?;
        let sites = conditional::call_sites(&programs)?;
        let mut helpers = ProgramHelpers::new(table, &sites).map_err(|error| error.to_string())?;
        checked_call_interfaces(&sites)?;
        let mut merged = collect_imports(&programs)?
            .into_iter()
            .collect::<std::collections::BTreeSet<_>>();
        merged.extend(helpers.math_imports().iter().copied());
        let imports = merged.into_iter().collect::<Vec<_>>();
        helpers
            .relocate(&imports)
            .map_err(|error| error.to_string())?;
        let (bodies, faults) = helpers.bodies(table).map_err(|error| error.to_string())?;
        // A checked forced arena allocates source registers even for scalar-only tuples;
        // this is storage planning, never a synthetic tensor source operation.
        let arena = arena::ArenaPlan::required(&programs, 0)?;
        let calls = CallProgramPlan::new(&programs, &helpers, work, arena.inner_counter)?;
        if plan
            .maximum_tuple
            .checked_mul(8)
            .is_none_or(|bytes| bytes > calls.work_bytes as usize)
        {
            return Err("WASM exact tuple exceeds its checked output scratch".into());
        }
        Ok(Self {
            plan,
            arena,
            calls,
            imports,
            bodies,
            faults,
        })
    }
}

fn checked_programs(plan: &Plan, layout: &VarLayout) -> Result<Vec<Vec<LinearOp>>, String> {
    for program in &plan.programs {
        crate::exact_assignments::profile::disjoint_destinations(&program.block)?;
    }
    let programs = plan
        .programs
        .iter()
        .map(|program| program.block.programs()[0].clone())
        .collect::<Vec<_>>();
    for program in &programs {
        conditional::visit_operations(
            std::slice::from_ref(program),
            crate::exact_assignments::profile::primal_only,
        )?;
        affine::validate_loads(program, &[], layout)?;
    }
    Ok(programs)
}

fn checked_call_interfaces(sites: &[solve::SolvePureCallSite]) -> Result<(), String> {
    for site in sites {
        let real = |value: &solve::SolveValueType| {
            matches!(
                value.element_type(),
                solve::SolveScalarType::Real {
                    format: solve::SolveRealFormat::Binary64,
                    ..
                }
            )
        };
        if !site.inputs().iter().all(real)
            || !site
                .outputs()
                .iter()
                .all(|output| real(output.value_type()))
        {
            return Err("WASM exact calls require Binary64 Real interfaces".into());
        }
    }
    Ok(())
}
