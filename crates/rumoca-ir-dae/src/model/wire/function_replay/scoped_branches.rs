use super::*;

pub(super) enum BranchReplayFrame<'dae> {
    Body(FunctionBranchFinish<FunctionBody<'dae>>),
    Fold(FunctionBranchFinish<FunctionLoop<'dae>>),
}

pub(super) struct ConditionalReplayFrame<'wire, 'dae> {
    conditions: &'wire [u32],
    branch_count: usize,
    branches: Vec<FunctionBranchOwner<'dae>>,
    provenance: DaeProvenance,
}

pub(super) fn begin_conditional<'wire, 'dae>(
    state: &mut FunctionReplay<'wire, '_, 'dae>,
    conditions: &'wire [u32],
    branch_count: usize,
    provenance: DaeProvenance,
) {
    state.conditionals.push(ConditionalReplayFrame {
        conditions,
        branch_count,
        branches: Vec::new(),
        provenance,
    });
}

pub(super) fn begin_branch<'dae>(
    dae: &mut DaeConstruction<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let (child, frame) = match state.capability.take() {
        Some(ReplayCapability::Body(parent)) => {
            let (child, finish) =
                dae.functions(|functions| functions.begin_branch(parent, provenance))?;
            (child, BranchReplayFrame::Body(finish))
        }
        Some(ReplayCapability::Fold(parent)) => {
            let (child, finish) =
                dae.functions(|functions| functions.begin_branch_loop(parent, provenance))?;
            (child, BranchReplayFrame::Fold(finish))
        }
        Some(ReplayCapability::External(_)) | None => {
            return Err(malformed("functions.statements.branch"));
        }
    };
    state.capability = Some(ReplayCapability::Body(child));
    state.branches.push(frame);
    Ok(())
}

pub(super) fn end_branch<'dae>(
    dae: &mut DaeConstruction<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
) -> Result<(), DaeConstructionError> {
    let Some(ReplayCapability::Body(child)) = state.capability.take() else {
        return Err(malformed("functions.statements.branch"));
    };
    let frame = state
        .branches
        .pop()
        .ok_or_else(|| malformed("functions.statements.branch"))?;
    let (parent, owner) = match frame {
        BranchReplayFrame::Body(finish) => {
            let (parent, owner) =
                dae.functions(|functions| functions.finish_branch(finish, child))?;
            (ReplayCapability::Body(parent), owner)
        }
        BranchReplayFrame::Fold(finish) => {
            let (parent, owner) =
                dae.functions(|functions| functions.finish_branch_loop(finish, child))?;
            (ReplayCapability::Fold(parent), owner)
        }
    };
    state.capability = Some(parent);
    let conditional = state
        .conditionals
        .last_mut()
        .ok_or_else(|| malformed("functions.statements.conditional"))?;
    if conditional.branches.len() >= conditional.branch_count {
        return Err(malformed("functions.statements.conditional"));
    }
    conditional.branches.push(owner);
    Ok(())
}

pub(super) fn projection_count(
    dae: &DaeConstruction<'_>,
    state: &FunctionReplay<'_, '_, '_>,
) -> Result<usize, DaeConstructionError> {
    let conditional = state
        .conditionals
        .last()
        .ok_or_else(|| malformed("functions.statements.conditional"))?;
    if conditional.branches.len() != conditional.branch_count || conditional.branch_count == 0 {
        return Err(malformed("functions.statements.conditional"));
    }
    let body = active_body(state)?;
    let ordinals = conditional
        .branches
        .iter()
        .map(|owner| owner.ordinal)
        .collect::<Vec<_>>();
    let (fallback, branches) = ordinals
        .split_last()
        .ok_or_else(|| malformed("functions.statements.conditional"))?;
    Ok(crate::model::scoped_conditionals::projection_targets(
        &dae.storage.functions[state.function_index],
        &function_build_state(dae.storage, body).current_values,
        branches,
        *fallback,
    )
    .count())
}

pub(super) fn end_conditional<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
) -> Result<(), DaeConstructionError> {
    let count = projection_count(dae, state)?;
    let conditional = state
        .conditionals
        .pop()
        .ok_or_else(|| malformed("functions.statements.conditional"))?;
    let mut owners = conditional.branches;
    let fallback = owners
        .pop()
        .ok_or_else(|| malformed("functions.statements.conditional"))?;
    let conditions = conditional
        .conditions
        .iter()
        .map(|raw| mapped_expression(ids, *raw, conditional.provenance))
        .collect::<Result<Vec<_>, _>>()?;
    let branches = conditions.into_iter().zip(owners).collect();
    let expression_start = ids.expressions.len();
    let definition_start = dae.storage.functions[state.function_index]
        .definitions
        .len();
    match state.capability.as_mut() {
        Some(ReplayCapability::Body(parent)) => {
            dae.functions(|functions| {
                functions.finish_conditional(parent, branches, fallback, conditional.provenance)
            })?;
        }
        Some(ReplayCapability::Fold(parent)) => {
            dae.functions(|functions| {
                functions.finish_conditional_loop(
                    parent,
                    branches,
                    fallback,
                    conditional.provenance,
                )
            })?;
        }
        Some(ReplayCapability::External(_)) | None => {
            return Err(malformed("functions.statements.conditional"));
        }
    }
    record_generated_group(
        wire,
        dae,
        ids,
        GeneratedGroupReplay {
            function: state.function_index as u32,
            target_count: count,
            group: GeneratedGroup::Conditional,
            expression_start,
            definition_start,
        },
    )
}

fn active_body<'state, 'dae>(
    state: &'state FunctionReplay<'_, '_, 'dae>,
) -> Result<&'state FunctionBody<'dae>, DaeConstructionError> {
    match state.capability.as_ref() {
        Some(ReplayCapability::Body(body)) => Ok(body),
        Some(ReplayCapability::Fold(body)) => Ok(body.body()),
        Some(ReplayCapability::External(_)) | None => {
            Err(malformed("functions.statements.conditional"))
        }
    }
}

pub(super) fn require_assigned<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
    target: u32,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    expect_function_value(wire, state.function_index, target, provenance)?;
    let value = FunctionValueId::from_raw(state.function_index as u32, target);
    let expression_start = ids.expressions.len();
    let definition_start = dae.storage.functions[state.function_index]
        .definitions
        .len();
    match state.capability.as_mut() {
        Some(ReplayCapability::Body(body)) => {
            dae.functions(|functions| functions.require_assigned(body, value, provenance))?;
        }
        Some(ReplayCapability::Fold(body)) => {
            dae.functions(|functions| functions.require_assigned_loop(body, value, provenance))?;
        }
        Some(ReplayCapability::External(_)) | None => {
            return Err(malformed("functions.statements.require_assigned"));
        }
    }
    record_generated_group(
        wire,
        dae,
        ids,
        GeneratedGroupReplay {
            function: state.function_index as u32,
            target_count: 1,
            group: GeneratedGroup::Conditional,
            expression_start,
            definition_start,
        },
    )
}
