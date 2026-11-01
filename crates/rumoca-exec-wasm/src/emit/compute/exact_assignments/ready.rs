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
    pub scratch_report: crate::ScratchReport,
    pub observations: Vec<crate::typed_call::retained_event::InvocationStorage>,
    pub seed_offset: Option<u32>,
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
        Self::from_plan(plan, layout, table, layout.y_scalars(), None)
    }

    #[cfg(test)]
    pub(super) fn private(
        block: &solve::ScalarProgramBlock,
        layout: &VarLayout,
        table: &solve::SolvePureCallTable,
    ) -> Result<Self, String> {
        Self::private_entries(&[(block, None)], layout, table)
    }

    #[cfg(test)]
    pub(super) fn directional(
        block: &solve::ScalarProgramBlock,
        layout: &VarLayout,
        table: &solve::SolvePureCallTable,
        seed_len: usize,
    ) -> Result<Self, String> {
        Self::private_entries(&[(block, Some(seed_len))], layout, table)
    }

    /// Shared linkage and one storage plan sized for the widest of all entries.
    pub(super) fn private_entries(
        entries: &[(&solve::ScalarProgramBlock, Option<usize>)],
        layout: &VarLayout,
        table: &solve::SolvePureCallTable,
    ) -> Result<Self, String> {
        checked_slot_count(layout.y_scalars())?;
        checked_slot_count(layout.p_scalars())?;
        let plan = Plan::private_entries(entries)?;
        let work = layout.y_scalars().max(plan.maximum_tuple);
        Self::from_plan(plan, layout, table, work, None)
    }

    pub(super) fn event(
        source: &solve::RetainedEventProgram,
        layout: &VarLayout,
    ) -> Result<Self, String> {
        let block = solve::ScalarProgramBlock::with_program_spans(
            vec![source.program().to_vec()],
            vec![source.span()],
        )
        .map_err(|error| error.to_string())?;
        let plan = Plan::private(&block)?;
        let work = layout.y_scalars().max(plan.maximum_tuple);
        Self::from_plan(plan, layout, source.table(), work, Some(source))
    }

    /// Every refusal `private_entries` and entry emission can raise for these
    /// entries alone, raised now, while their shared module is emitted later.
    /// Call-owner linkage is computed once per distinct call-site set.
    pub(super) fn admit(
        entries: &[(&solve::ScalarProgramBlock, Option<usize>)],
        layout: &VarLayout,
        table: &solve::SolvePureCallTable,
        cache: &mut AdmissionCache,
    ) -> Result<(), String> {
        checked_slot_count(layout.y_scalars())?;
        checked_slot_count(layout.p_scalars())?;
        let plan = Plan::private_entries(entries)?;
        let programs = checked_programs(&plan, layout)?;
        let sites = conditional::call_sites(&programs)?;
        let directional = conditional::directional_call_sites(&programs)?;
        checked_call_interfaces(&sites)?;
        for site in &directional {
            checked_interface(site.inputs(), site.outputs())?;
        }
        let linked = cache.link(table, sites, directional)?;
        let arena = storage_plan(&plan, &programs)?;
        let work = layout.y_scalars().max(plan.maximum_tuple);
        let calls =
            CallProgramPlan::new(&programs, &linked.helpers, work, arena.last_integer_local())?;
        if plan
            .maximum_tuple
            .checked_mul(8)
            .is_none_or(|bytes| bytes > calls.view().work_bytes as usize)
        {
            return Err("WASM exact tuple exceeds its checked output scratch".into());
        }
        let mut bytes = calls.view().bytes;
        let seed_len = plan
            .programs
            .iter()
            .filter_map(|program| program.seed)
            .max();
        let seed_offset = seed_len.map(|_| bytes);
        if let Some(count) = seed_len {
            let seed_bytes = count
                .checked_mul(8)
                .and_then(|bytes| u32::try_from(bytes).ok())
                .ok_or("private seed storage exceeds memory32")?;
            bytes = bytes
                .checked_add(seed_bytes)
                .ok_or("private seed storage exceeds memory32")?;
        }
        let calls = calls.with_private_extension_bytes(bytes)?;
        let scratch_report = calls.scratch_report(Vec::new());
        let ready = Self {
            plan,
            arena,
            calls,
            imports: MathImport::ALL.to_vec(),
            bodies: Vec::new(),
            faults: linked.faults.clone(),
            scratch_report,
            observations: Vec::new(),
            seed_offset,
        };
        let mut module = Module::new();
        let types = call_program::add_types(&mut module);
        let catalog = add_import_section(&mut module, &ready.imports, &types);
        for (entry, program) in ready.plan.programs.iter().enumerate() {
            let seed = program.seed.and(ready.seed_offset);
            super::emit_programs(&catalog, &ready, layout, true, entry..entry + 1, seed)?;
        }
        Ok(())
    }

    fn from_plan(
        plan: Plan,
        layout: &VarLayout,
        table: &solve::SolvePureCallTable,
        work: usize,
        event: Option<&solve::RetainedEventProgram>,
    ) -> Result<Self, String> {
        // Every directional entry reads its seed from one shared extension.
        let seed_len = plan
            .programs
            .iter()
            .filter_map(|program| program.seed)
            .max();
        let programs = checked_programs(&plan, layout)?;
        let sites = conditional::call_sites(&programs)?;
        let directional = conditional::directional_call_sites(&programs)?;
        let mut helpers = match event {
            Some(source) => ProgramHelpers::event(source, &sites),
            None => ProgramHelpers::with_directional(table, &sites, &directional),
        }
        .map_err(|error| error.to_string())?;
        checked_call_interfaces(&sites)?;
        for site in &directional {
            checked_interface(site.inputs(), site.outputs())?;
        }
        let mut merged = collect_imports(&programs)?
            .into_iter()
            .collect::<std::collections::BTreeSet<_>>();
        merged.extend(helpers.math_imports().iter().copied());
        let imports = merged.into_iter().collect::<Vec<_>>();
        helpers
            .relocate(&imports)
            .map_err(|error| error.to_string())?;
        let (bodies, faults) = helpers
            .bodies(table, crate::outline::FUNCTION_BYTES)
            .map_err(|error| error.to_string())?;
        // A checked forced arena allocates source registers even for scalar-only tuples;
        // this is storage planning, never a synthetic tensor source operation.
        let arena = storage_plan(&plan, &programs)?;
        let calls = CallProgramPlan::new(&programs, &helpers, work, arena.last_integer_local())?;
        if plan
            .maximum_tuple
            .checked_mul(8)
            .is_none_or(|bytes| bytes > calls.view().work_bytes as usize)
        {
            return Err("WASM exact tuple exceeds its checked output scratch".into());
        }
        let mut observations = Vec::new();
        let mut bytes = calls.view().bytes;
        if let Some(source) = event {
            for invocation in source.invocations() {
                let site = invocation.owner().call_site();
                let layout = helpers.layout(&site).map_err(|error| error.to_string())?;
                let carrier = helpers
                    .observations(&site)
                    .map_err(|error| error.to_string())?;
                observations.push(crate::typed_call::retained_event::InvocationStorage::new(
                    invocation.region_path().to_vec(),
                    invocation.operation(),
                    &layout,
                    carrier,
                    &mut bytes,
                )?);
            }
        }
        let seed_offset = seed_len.map(|_| bytes);
        if let Some(count) = seed_len {
            let seed_bytes = count
                .checked_mul(8)
                .and_then(|bytes| u32::try_from(bytes).ok())
                .ok_or("private seed storage exceeds memory32")?;
            bytes = bytes
                .checked_add(seed_bytes)
                .ok_or("private seed storage exceeds memory32")?;
        }
        let calls = calls.with_private_extension_bytes(bytes)?;
        let scratch_report = calls.scratch_report(helpers.owner_scratch(table));
        Ok(Self {
            plan,
            arena,
            calls,
            scratch_report,
            imports,
            bodies,
            faults,
            observations,
            seed_offset,
        })
    }
}

fn storage_plan(plan: &Plan, programs: &[Vec<LinearOp>]) -> Result<arena::ArenaPlan, String> {
    if !plan.programs.iter().any(|program| program.seed.is_some()) {
        return arena::ArenaPlan::required(programs, 0);
    }
    for (program, ops) in plan.programs.iter().zip(programs) {
        if program.seed.is_none() {
            arena::ArenaPlan::validate_primal(std::slice::from_ref(ops))?;
        }
    }
    arena::ArenaPlan::required_directional(programs, 0)
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
    for (issued, program) in plan.programs.iter().zip(&programs) {
        let seed_len = issued.seed;
        conditional::visit_operations(std::slice::from_ref(program), |operation| match seed_len {
            None => crate::exact_assignments::profile::primal_only(operation),
            Some(count) => directional_operation(operation, count),
        })?;
        let context =
            affine::directional_load_context(layout, seed_len.unwrap_or(layout.y_scalars()));
        affine::validate_loads(program, &[], context)?;
    }
    Ok(programs)
}

fn directional_operation(operation: &LinearOp, seed_len: usize) -> Result<(), String> {
    let end = match operation {
        LinearOp::LoadSeed { index, .. } => index.checked_add(1),
        LinearOp::LoadIndexedSeed { base, count, .. } => base.checked_add(*count),
        LinearOp::TensorLoad {
            seed_start: Some(start),
            count,
            ..
        } => start.checked_add(*count),
        _ => return Ok(()),
    };
    if end.is_none_or(|end| end > seed_len) {
        return Err("WASM private source seed exceeds its declared coordinate storage".into());
    }
    Ok(())
}

fn checked_call_interfaces(sites: &[solve::SolvePureCallSite]) -> Result<(), String> {
    for site in sites {
        checked_interface(site.inputs(), site.outputs())?;
    }
    Ok(())
}

fn checked_interface(
    inputs: &[solve::SolveValueType],
    outputs: &[solve::SolvePureCallOutput],
) -> Result<(), String> {
    let admitted = |value: &solve::SolveValueType| {
        matches!(
            value.element_type(),
            solve::SolveScalarType::Real {
                format: solve::SolveRealFormat::Binary64,
                ..
            } | solve::SolveScalarType::Integer(_)
                | solve::SolveScalarType::Boolean
        )
    };
    if !inputs.iter().all(admitted) || !outputs.iter().all(|output| admitted(output.value_type())) {
        return Err("WASM exact calls require Binary64 typed interfaces".into());
    }
    Ok(())
}

/// One call-site set's linked, relocated and compiled call owners.
pub(crate) struct LinkedSites {
    helpers: ProgramHelpers,
    faults: Vec<TypedCallFault>,
}

/// Linkage results of one batch's requests, keyed by their exact call-site
/// set. Every request of a batch shares one call table.
#[derive(Default)]
pub(crate) struct AdmissionCache {
    linked: std::collections::HashMap<Vec<solve::SolvePureCallOwnerId>, Vec<LinkedEntry>>,
}

type LinkedEntry = (
    Vec<solve::SolvePureCallSite>,
    Vec<solve::SolvePureCallDirectionalSite>,
    Result<std::rc::Rc<LinkedSites>, String>,
);

impl AdmissionCache {
    fn link(
        &mut self,
        table: &solve::SolvePureCallTable,
        sites: Vec<solve::SolvePureCallSite>,
        directional: Vec<solve::SolvePureCallDirectionalSite>,
    ) -> Result<std::rc::Rc<LinkedSites>, String> {
        let key = sites
            .iter()
            .map(solve::SolvePureCallSite::owner)
            .chain(
                directional
                    .iter()
                    .map(solve::SolvePureCallDirectionalSite::owner),
            )
            .collect::<Vec<_>>();
        let bucket = self.linked.entry(key).or_default();
        if let Some((_, _, linked)) = bucket.iter().find(|(known, known_directional, _)| {
            *known == sites && *known_directional == directional
        }) {
            return linked.clone();
        }
        let linked = link_sites(table, &sites, &directional).map(std::rc::Rc::new);
        bucket.push((sites, directional, linked.clone()));
        linked
    }
}

fn link_sites(
    table: &solve::SolvePureCallTable,
    sites: &[solve::SolvePureCallSite],
    directional: &[solve::SolvePureCallDirectionalSite],
) -> Result<LinkedSites, String> {
    let mut helpers = ProgramHelpers::with_directional(table, sites, directional)
        .map_err(|error| error.to_string())?;
    // Refusals do not depend on import indices; the complete ABI stands in for
    // the entry's own import set.
    helpers
        .relocate(&MathImport::ALL)
        .map_err(|error| error.to_string())?;
    let (_, faults) = helpers
        .bodies(table, crate::outline::FUNCTION_BYTES)
        .map_err(|error| error.to_string())?;
    Ok(LinkedSites { helpers, faults })
}
