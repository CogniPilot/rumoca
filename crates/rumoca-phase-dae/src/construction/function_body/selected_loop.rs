//! Construct a child fold inside the selected region that owns its entry reads.

use super::*;

pub(super) fn needs_selected_entry<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    parent: &dae::FunctionBody<'dae>,
    reads: (&[VarName], &[(VarName, Span)]),
    owner: dae::DaeProvenance,
) -> Result<bool, dae::DaeConstructionError> {
    for name in reads.0.iter().chain(reads.1.iter().map(|(name, _)| name)) {
        let target = function_value_coordinate(symbols.coordinates, name);
        match construction
            .functions(|functions| functions.current_definition_id(parent, target, owner))
        {
            Ok(_) => {}
            Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: "function value",
                index,
                ..
            }) if index == target.ordinal() => return Ok(true),
            Err(error) => return Err(error),
        }
    }
    Ok(false)
}

pub(super) fn lower_selected_parent<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    parent: dae::FunctionBody<'dae>,
    condition: &Expression,
    mut input: FunctionFold<'_, '_, 'dae>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let owner = input.owner;
    let selected = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        &parent,
        symbols.binders,
        condition,
    )?;
    let (body, finish) =
        construction.functions(|functions| functions.begin_branch(parent, owner))?;
    input.selected_condition = None;
    let body = lower_function_fold(construction, symbols, body, input)?;
    let (parent, taken) =
        construction.functions(|functions| functions.finish_branch(finish, body))?;
    let (body, finish) =
        construction.functions(|functions| functions.begin_branch(parent, owner))?;
    let (mut parent, skipped) =
        construction.functions(|functions| functions.finish_branch(finish, body))?;
    construction.functions(|functions| {
        functions.finish_conditional(&mut parent, vec![(selected, taken)], skipped, owner)
    })?;
    Ok(parent)
}

pub(super) fn lower_selected_child<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    parent_symbols: FunctionSymbols<'_, 'dae>,
    parent: dae::FunctionLoop<'dae>,
    condition: &Expression,
    child_symbols: FunctionSymbols<'_, 'dae>,
    input: FunctionFold<'_, '_, 'dae>,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let owner = input.owner;
    let selected = lower_function_expression_scoped(
        construction,
        parent_symbols.coordinates,
        parent_symbols.functions,
        parent_symbols.shapes,
        parent.body(),
        parent_symbols.binders,
        condition,
    )?;
    let (mut body, finish) =
        construction.functions(|functions| functions.begin_branch_loop(parent, owner))?;
    require_proven_function_reads(construction, parent_symbols, &mut body, input.entry_reads)?;
    let targets = input
        .targets
        .iter()
        .map(|target| function_value_coordinate(child_symbols.coordinates, target));
    let locals = input
        .iteration_locals
        .iter()
        .map(|target| function_value_coordinate(child_symbols.coordinates, target));
    let child = construction.functions(|functions| {
        functions.begin_loop_with_iteration_locals(body, input.domain, targets, locals, owner)
    })?;
    let child = lower_child_transition(construction, child_symbols, child, input)?;
    let body = construction.functions(|functions| functions.finish_loop(child, owner))?;
    let (parent, taken) =
        construction.functions(|functions| functions.finish_branch_loop(finish, body))?;
    let (body, finish) =
        construction.functions(|functions| functions.begin_branch_loop(parent, owner))?;
    let (mut parent, skipped) =
        construction.functions(|functions| functions.finish_branch_loop(finish, body))?;
    construction.functions(|functions| {
        functions.finish_conditional_loop(&mut parent, vec![(selected, taken)], skipped, owner)
    })?;
    Ok(parent)
}

pub(super) fn lower_child_transition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut child: dae::FunctionLoop<'dae>,
    input: FunctionFold<'_, '_, 'dae>,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    continue_while_selected(
        construction,
        symbols,
        &mut child,
        (input.binders, input.domain, input.structured, input.owner),
        (input.statements, input.plans),
    )?;
    lower_function_iteration_local_seeds(
        construction,
        symbols,
        &mut child,
        input.plans,
        input.iteration_locals,
        input.owner.span(),
    )?;
    lower_function_loop_statements(
        construction,
        symbols,
        child,
        input.binders,
        input.statements,
        input.plans,
    )
}
