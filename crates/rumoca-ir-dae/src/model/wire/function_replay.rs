use super::*;

pub(super) fn reconstruct<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    reservations: Vec<FunctionReservation<'dae>>,
) -> Result<(), DaeConstructionError> {
    let mut functions = begin_functions(wire, dae, reservations)?;
    while ids.expressions.len() < wire.expressions.nodes.len() {
        let index = ids.expressions.len();
        let expression = wire_expression(wire, index)?;
        match expression.node {
            ExprNodeWire::FunctionValue {
                function,
                value,
                definition_ordinal,
            } => replay_read(
                wire,
                dae,
                ids,
                &mut functions,
                (*function, *value, *definition_ordinal),
            )?,
            ExprNodeWire::FunctionFoldParameter { function, fold, .. } => {
                replay_loop_entry(wire, dae, ids, &mut functions, *function, *fold)?;
            }
            ExprNodeWire::FunctionFoldOutput { function, fold, .. } => {
                replay_loop_exit(wire, dae, ids, &mut functions, *function, *fold)?;
            }
            _ => {
                if !super::reconstruct_next_expression(wire, dae, ids)? {
                    return Err(malformed("expressions.nodes"));
                }
            }
        }
    }
    expect_expression_arena_consumed(wire, dae, ids)?;
    finish_functions(wire, dae, ids, functions)
}

struct FunctionReplay<'wire, 'dae> {
    function_index: usize,
    body: Option<FunctionBody<'dae>>,
    statements: &'wire [FunctionStatementInput],
    next_statement: usize,
    active_loop: Option<LoopReplay<'wire, 'dae>>,
    seen_definitions: Vec<bool>,
}

struct LoopReplay<'wire, 'dae> {
    fold_raw: usize,
    fold_ordinal: u32,
    statements: &'wire [FunctionStatementInput],
    next_statement: usize,
    provenance: DaeProvenance,
    body: FunctionLoop<'dae>,
}

fn begin_functions<'wire, 'dae>(
    wire: &'wire StorageWire,
    dae: &mut DaeConstruction<'dae>,
    reservations: Vec<FunctionReservation<'dae>>,
) -> Result<Vec<FunctionReplay<'wire, 'dae>>, DaeConstructionError> {
    if reservations.len() != wire.functions.len() {
        return Err(malformed("functions"));
    }
    wire.functions
        .iter()
        .zip(reservations)
        .enumerate()
        .map(|(function_index, (function, reservation))| {
            let definition = function
                .definition
                .as_ref()
                .ok_or_else(|| incomplete("function", function_index, function.declaration))?;
            let body =
                dae.functions(|functions| functions.begin(reservation, function.declaration))?;
            Ok(FunctionReplay {
                function_index,
                body: Some(body),
                statements: &definition.statements,
                next_statement: 0,
                active_loop: None,
                seen_definitions: vec![false; function.definitions.len()],
            })
        })
        .collect()
}

fn replay_read<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    functions: &mut [FunctionReplay<'_, 'dae>],
    identity: (u32, u32, u32),
) -> Result<(), DaeConstructionError> {
    let (function_raw, value_raw, definition_ordinal) = identity;
    let index = ids.expressions.len();
    let expression = wire_expression(wire, index)?;
    let provenance = expression.provenance;
    let state = function_state_mut(functions, function_raw, provenance)?;
    let function = function_wire(wire, state.function_index, provenance)?;
    function
        .values
        .get(value_raw as usize)
        .ok_or_else(|| unknown("function value", value_raw, provenance))?;
    prepare_definition_for_read(
        wire,
        dae,
        ids,
        state,
        value_raw,
        definition_ordinal,
        provenance,
    )?;
    let body = body_for_definition(state, value_raw, definition_ordinal, provenance)?;
    let value = FunctionValueId::from_raw(function_raw, value_raw);
    let current =
        dae.functions(|functions| functions.current_definition_id(body, value, provenance))?;
    if current.ordinal() != definition_ordinal {
        return Err(DaeConstructionError::InvalidFunctionValueRead {
            value: value_raw,
            expected_definition: Some(current.ordinal()),
            found_definition: definition_ordinal,
            span: provenance.span(),
        });
    }
    let rebuilt = dae.functions(|functions| functions.read(body, value, provenance))?;
    record_expression(wire, dae, ids, rebuilt, provenance)
}

fn prepare_definition_for_read<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
    value: u32,
    definition: u32,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if body_for_definition(state, value, definition, provenance).is_ok() {
        return Ok(());
    }
    if state.active_loop.is_some() {
        advance_loop_to_definition(wire, dae, ids, state, value, definition, provenance)
    } else {
        advance_root_to_definition(wire, dae, ids, state, value, definition, provenance)
    }
}

fn advance_root_to_definition<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
    value: u32,
    definition: u32,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    while body_for_definition(state, value, definition, provenance).is_err() {
        let ordinal = match state.statements.get(state.next_statement) {
            Some(FunctionStatementInput::Assignment { definition }) => *definition,
            _ => break,
        };
        let input = definition_input(wire, state.function_index, ordinal)?;
        if input.rhs as usize >= ids.expressions.len() {
            break;
        }
        apply_root_assignment(wire, dae, ids, state, ordinal)?;
        state.next_statement += 1;
    }
    body_for_definition(state, value, definition, provenance).map(|_| ())
}

fn advance_loop_to_definition<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
    value: u32,
    definition: u32,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    while body_for_definition(state, value, definition, provenance).is_err() {
        let ordinal = match state
            .active_loop
            .as_ref()
            .and_then(|active| active.statements.get(active.next_statement))
        {
            Some(FunctionStatementInput::Assignment { definition }) => *definition,
            Some(FunctionStatementInput::For { .. }) => {
                return Err(malformed("function_folds.nesting"));
            }
            None => break,
        };
        let input = definition_input(wire, state.function_index, ordinal)?;
        if input.rhs as usize >= ids.expressions.len() {
            break;
        }
        apply_loop_assignment(wire, dae, ids, state, ordinal)?;
        let active = state
            .active_loop
            .as_mut()
            .ok_or_else(|| malformed("function_folds"))?;
        active.next_statement += 1;
    }
    body_for_definition(state, value, definition, provenance).map(|_| ())
}

fn body_for_definition<'state, 'wire, 'dae>(
    state: &'state FunctionReplay<'wire, 'dae>,
    value: u32,
    definition: u32,
    provenance: DaeProvenance,
) -> Result<&'state FunctionBody<'dae>, DaeConstructionError> {
    if let Some(active) = &state.active_loop {
        let current = active
            .body
            .body
            .current_values
            .get(value as usize)
            .copied()
            .flatten();
        if current == Some(definition) {
            return Ok(&active.body.body);
        }
    }
    let root = state
        .body
        .as_ref()
        .ok_or_else(|| incomplete("function body", state.function_index, provenance))?;
    let current = root.current_values.get(value as usize).copied().flatten();
    if current == Some(definition) {
        Ok(root)
    } else {
        Err(DaeConstructionError::InvalidFunctionValueRead {
            value,
            expected_definition: current,
            found_definition: definition,
            span: provenance.span(),
        })
    }
}

fn replay_loop_entry<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    functions: &mut [FunctionReplay<'_, 'dae>],
    function_raw: u32,
    fold_ordinal: u32,
) -> Result<(), DaeConstructionError> {
    let provenance = wire_expression(wire, ids.expressions.len())?.provenance;
    let state = function_state_mut(functions, function_raw, provenance)?;
    if state.active_loop.is_some() {
        return Err(malformed("function_folds.nesting"));
    }
    apply_ready_root_assignments(wire, dae, ids, state)?;
    let (statement_fold, statements, statement_provenance) = next_root_fold(state)
        .ok_or_else(|| incomplete("function fold", fold_ordinal as usize, provenance))?;
    if statement_fold != fold_ordinal {
        return Err(malformed("functions.definition.statements.fold"));
    }
    let fold_raw = validate_fold_header(
        wire,
        ids,
        state,
        function_raw,
        fold_ordinal,
        statement_provenance,
    )?;
    let fold = &wire.function_folds[fold_raw];
    let targets = checked_fold_targets(wire, function_raw, fold, statement_provenance)?;
    let parent = state
        .body
        .take()
        .ok_or_else(|| incomplete("function body", state.function_index, fold.provenance))?;
    expect_current_definitions(
        &parent,
        &targets,
        &fold.initial_definitions,
        fold.provenance,
    )?;
    let definition_start = dae.storage.functions[function_raw as usize]
        .definitions
        .len();
    let expression_start = ids.expressions.len();
    let loop_body = dae.functions(|functions| {
        functions.begin_loop(
            parent,
            mapped(&ids.domains, fold.domain, "domain", fold.provenance)?,
            targets.iter().copied(),
            fold.provenance,
        )
    })?;
    if loop_body.fold().ordinal() != fold_ordinal {
        return Err(malformed("function_folds.ordinal"));
    }
    expect_fold_raw(
        dae,
        function_raw,
        fold_ordinal,
        fold_raw,
        statement_provenance,
    )?;
    record_generated_group(
        wire,
        dae,
        ids,
        state,
        GeneratedGroupReplay {
            body: &loop_body.body,
            fold,
            group: GeneratedGroup::Parameter,
            expression_start,
            definition_start,
        },
    )?;
    state.next_statement += 1;
    ids.function_folds[fold_raw] = Some(loop_body.fold());
    state.active_loop = Some(LoopReplay {
        fold_raw,
        fold_ordinal,
        statements,
        next_statement: 0,
        provenance: statement_provenance,
        body: loop_body,
    });
    Ok(())
}

fn replay_loop_exit<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    functions: &mut [FunctionReplay<'_, 'dae>],
    function_raw: u32,
    fold_ordinal: u32,
) -> Result<(), DaeConstructionError> {
    let provenance = wire_expression(wire, ids.expressions.len())?.provenance;
    let state = function_state_mut(functions, function_raw, provenance)?;
    apply_ready_loop_assignments(wire, dae, ids, state)?;
    let active = state
        .active_loop
        .as_ref()
        .ok_or_else(|| incomplete("function fold", fold_ordinal as usize, provenance))?;
    if active.fold_ordinal != fold_ordinal || active.next_statement != active.statements.len() {
        return Err(malformed("functions.definition.statements"));
    }
    let fold = &wire.function_folds[active.fold_raw];
    let targets = checked_fold_targets(wire, function_raw, fold, active.provenance)?;
    expect_current_definitions(
        &active.body.body,
        &targets,
        &fold.update_definitions,
        active.provenance,
    )?;
    let expression_start = ids.expressions.len();
    let definition_start = dae.storage.functions[function_raw as usize]
        .definitions
        .len();
    let active = state
        .active_loop
        .take()
        .ok_or_else(|| incomplete("function fold", fold_ordinal as usize, provenance))?;
    let fold_raw = active.fold_raw;
    let finish_provenance = active.provenance;
    let body = dae.functions(|functions| functions.finish_loop(active.body, finish_provenance))?;
    record_generated_group(
        wire,
        dae,
        ids,
        state,
        GeneratedGroupReplay {
            body: &body,
            fold,
            group: GeneratedGroup::Output,
            expression_start,
            definition_start,
        },
    )?;
    state.body = Some(body);
    expect_complete_fold(dae, fold, fold_raw, finish_provenance)
}

fn apply_ready_root_assignments<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
) -> Result<(), DaeConstructionError> {
    loop {
        let Some(FunctionStatementInput::Assignment { definition }) =
            state.statements.get(state.next_statement)
        else {
            return Ok(());
        };
        let input = definition_input(wire, state.function_index, *definition)?;
        if input.rhs as usize >= ids.expressions.len() {
            return Ok(());
        }
        apply_root_assignment(wire, dae, ids, state, *definition)?;
        state.next_statement += 1;
    }
}

fn apply_ready_loop_assignments<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
) -> Result<(), DaeConstructionError> {
    loop {
        let definition = match state
            .active_loop
            .as_ref()
            .and_then(|active| active.statements.get(active.next_statement))
        {
            Some(FunctionStatementInput::Assignment { definition }) => *definition,
            Some(FunctionStatementInput::For { .. }) => {
                return Err(malformed("function_folds.nesting"));
            }
            None => return Ok(()),
        };
        let input = definition_input(wire, state.function_index, definition)?;
        if input.rhs as usize >= ids.expressions.len() {
            return Ok(());
        }
        apply_loop_assignment(wire, dae, ids, state, definition)?;
        let active = state
            .active_loop
            .as_mut()
            .ok_or_else(|| malformed("function_folds"))?;
        active.next_statement += 1;
    }
}

fn apply_root_assignment<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
    definition: u32,
) -> Result<(), DaeConstructionError> {
    let input = definition_input(wire, state.function_index, definition)?;
    let target = checked_definition_target(wire, state.function_index, input)?;
    expect_next_definition(dae, state.function_index, definition, input.provenance)?;
    let rhs = mapped_expression(ids, input.rhs, input.provenance)?;
    let body = state
        .body
        .as_mut()
        .ok_or_else(|| incomplete("function body", state.function_index, input.provenance))?;
    dae.functions(|functions| functions.assign(body, target, rhs, input.provenance))?;
    consume_definition(wire, dae, state, definition)
}

fn apply_loop_assignment<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
    definition: u32,
) -> Result<(), DaeConstructionError> {
    let input = definition_input(wire, state.function_index, definition)?;
    let target = checked_definition_target(wire, state.function_index, input)?;
    expect_next_definition(dae, state.function_index, definition, input.provenance)?;
    let rhs = mapped_expression(ids, input.rhs, input.provenance)?;
    let active = state
        .active_loop
        .as_mut()
        .ok_or_else(|| incomplete("function fold", definition as usize, input.provenance))?;
    dae.functions(|functions| {
        functions.assign_loop(&mut active.body, target, rhs, input.provenance)
    })?;
    consume_definition(wire, dae, state, definition)
}

fn finish_functions<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    mut functions: Vec<FunctionReplay<'_, 'dae>>,
) -> Result<(), DaeConstructionError> {
    for state in &mut functions {
        apply_ready_root_assignments(wire, dae, ids, state)?;
        if state.active_loop.is_some() || state.next_statement != state.statements.len() {
            return Err(incomplete(
                "function statement",
                state.next_statement,
                function_declaration(wire, state.function_index),
            ));
        }
        if state.seen_definitions.iter().any(|seen| !seen)
            || dae.storage.functions[state.function_index]
                .definitions
                .len()
                != state.seen_definitions.len()
        {
            return Err(malformed("functions.definitions"));
        }
        let function = &wire.functions[state.function_index];
        let definition = function
            .definition
            .as_ref()
            .ok_or_else(|| incomplete("function", state.function_index, function.declaration))?;
        let body = state
            .body
            .as_ref()
            .ok_or_else(|| incomplete("function", state.function_index, function.declaration))?;
        let actual_results = function_results(function, body)?;
        if actual_results != definition.results {
            return Err(malformed("functions.definition.results"));
        }
        let body = state.body.take().ok_or_else(|| {
            incomplete("function body", state.function_index, function.declaration)
        })?;
        dae.functions(|functions| functions.define(body, function.declaration))?;
    }
    if ids.function_folds.iter().any(Option::is_none) {
        return Err(malformed("function_folds"));
    }
    Ok(())
}

fn function_results(
    function: &FunctionEntryWire,
    body: &FunctionBody<'_>,
) -> Result<Vec<u32>, DaeConstructionError> {
    function
        .output_values
        .iter()
        .map(|output| {
            body.current_values
                .get(*output as usize)
                .copied()
                .flatten()
                .ok_or(DaeConstructionError::IncompleteDefinition {
                    kind: "function output",
                    index: *output,
                    span: function.declaration.span(),
                })
        })
        .collect()
}

#[derive(Clone, Copy)]
enum GeneratedGroup {
    Parameter,
    Output,
}

struct GeneratedGroupReplay<'wire, 'dae> {
    body: &'wire FunctionBody<'dae>,
    fold: &'wire FunctionFoldEntryWire,
    group: GeneratedGroup,
    expression_start: usize,
    definition_start: usize,
}

fn record_generated_group<'dae>(
    wire: &StorageWire,
    dae: &DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
    replay: GeneratedGroupReplay<'_, 'dae>,
) -> Result<(), DaeConstructionError> {
    let definitions = match replay.group {
        GeneratedGroup::Parameter => &replay.fold.parameter_definitions,
        GeneratedGroup::Output => &replay.fold.output_definitions,
    };
    if definitions.len() != replay.fold.targets.len()
        || dae.storage.expressions.nodes.len() != replay.expression_start + definitions.len()
        || dae.storage.functions[state.function_index]
            .definitions
            .len()
            != replay.definition_start + definitions.len()
    {
        return Err(malformed("function_folds.definitions"));
    }
    for (carried, ((target, definition), raw)) in replay
        .fold
        .targets
        .iter()
        .zip(definitions)
        .zip(replay.expression_start..)
        .enumerate()
    {
        if replay
            .body
            .current_values
            .get(*target as usize)
            .copied()
            .flatten()
            != Some(*definition)
        {
            return Err(malformed("function_folds.definitions"));
        }
        let expression = wire_expression(wire, raw)?;
        expect_generated_node(
            expression.node,
            replay.fold,
            carried,
            *definition,
            replay.group,
        )?;
        consume_definition(wire, dae, state, *definition)?;
        let raw = u32::try_from(raw).map_err(|_| malformed("expressions.nodes"))?;
        let id = ExprId::from_raw(raw);
        record_expression(wire, dae, ids, id, expression.provenance)?;
    }
    Ok(())
}

fn expect_generated_node(
    node: &ExprNodeWire,
    fold: &FunctionFoldEntryWire,
    carried: usize,
    definition: u32,
    group: GeneratedGroup,
) -> Result<(), DaeConstructionError> {
    let expected_carried =
        u32::try_from(carried).map_err(|_| malformed("function_folds.targets"))?;
    let matches = match (node, group) {
        (
            ExprNodeWire::FunctionFoldParameter {
                function,
                fold: ordinal,
                carried,
                definition_ordinal,
            },
            GeneratedGroup::Parameter,
        )
        | (
            ExprNodeWire::FunctionFoldOutput {
                function,
                fold: ordinal,
                carried,
                definition_ordinal,
            },
            GeneratedGroup::Output,
        ) => {
            *function == fold.function
                && *ordinal == fold.ordinal
                && *carried == expected_carried
                && *definition_ordinal == definition
        }
        _ => false,
    };
    if matches {
        Ok(())
    } else {
        Err(malformed("expressions.nodes"))
    }
}

fn record_expression<'dae>(
    wire: &StorageWire,
    dae: &DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    rebuilt: ExprId<'dae>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let index = ids.expressions.len();
    if rebuilt.index() as usize != index {
        return Err(malformed("expressions.nodes"));
    }
    expect_no_expression_type_anchor(wire, ids, provenance)?;
    if dae.storage.expressions.provenance[index] != provenance {
        return Err(DaeConstructionError::ShapeMismatch {
            span: provenance.span(),
        });
    }
    ids.expressions.push(rebuilt);
    Ok(())
}

fn validate_fold_header(
    wire: &StorageWire,
    ids: &WireIds<'_>,
    state: &FunctionReplay<'_, '_>,
    function_raw: u32,
    fold_ordinal: u32,
    provenance: DaeProvenance,
) -> Result<usize, DaeConstructionError> {
    let function = function_wire(wire, state.function_index, provenance)?;
    let fold_raw = *function
        .folds
        .get(fold_ordinal as usize)
        .ok_or_else(|| unknown("function fold", fold_ordinal, provenance))?
        as usize;
    let fold = wire
        .function_folds
        .get(fold_raw)
        .ok_or_else(|| unknown("function fold", fold_raw as u32, provenance))?;
    if fold.function != function_raw
        || fold.ordinal != fold_ordinal
        || ids
            .function_folds
            .get(fold_raw)
            .copied()
            .flatten()
            .is_some()
    {
        return Err(malformed("function_folds"));
    }
    Ok(fold_raw)
}

fn checked_fold_targets<'dae>(
    wire: &StorageWire,
    function_raw: u32,
    fold: &FunctionFoldEntryWire,
    provenance: DaeProvenance,
) -> Result<Vec<FunctionValueId<'dae>>, DaeConstructionError> {
    let function = function_wire(wire, function_raw as usize, provenance)?;
    fold.targets
        .iter()
        .map(|target| {
            function
                .values
                .get(*target as usize)
                .ok_or_else(|| unknown("function value", *target, provenance))?;
            Ok(FunctionValueId::from_raw(function_raw, *target))
        })
        .collect()
}

fn expect_current_definitions(
    body: &FunctionBody<'_>,
    targets: &[FunctionValueId<'_>],
    expected: &[u32],
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if targets.len() != expected.len() {
        return Err(malformed("function_folds.definitions"));
    }
    for (target, expected) in targets.iter().zip(expected) {
        let found = body
            .current_values
            .get(target.ordinal() as usize)
            .copied()
            .flatten();
        if found != Some(*expected) {
            return Err(DaeConstructionError::InvalidFunctionValueRead {
                value: target.ordinal(),
                expected_definition: found,
                found_definition: *expected,
                span: provenance.span(),
            });
        }
    }
    Ok(())
}

fn expect_fold_raw(
    dae: &DaeConstruction<'_>,
    function: u32,
    ordinal: u32,
    expected_raw: usize,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let found = dae.storage.functions[function as usize]
        .folds
        .get(ordinal as usize)
        .copied();
    if found == u32::try_from(expected_raw).ok() {
        Ok(())
    } else {
        Err(DaeConstructionError::UnknownId {
            kind: "function fold",
            index: ordinal,
            span: provenance.span(),
        })
    }
}

fn expect_complete_fold(
    dae: &DaeConstruction<'_>,
    wire: &FunctionFoldEntryWire,
    raw: usize,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let actual = dae
        .storage
        .function_folds
        .get(raw)
        .ok_or_else(|| unknown("function fold", raw as u32, provenance))?;
    if actual.function == wire.function
        && actual.ordinal == wire.ordinal
        && actual.domain == wire.domain
        && actual.targets == wire.targets
        && actual.parameter_definitions == wire.parameter_definitions
        && actual.initial_definitions == wire.initial_definitions
        && actual.update_definitions == wire.update_definitions
        && actual.output_definitions == wire.output_definitions
        && actual.provenance == wire.provenance
    {
        Ok(())
    } else {
        Err(malformed("function_folds"))
    }
}

fn next_root_fold<'wire>(
    state: &FunctionReplay<'wire, '_>,
) -> Option<(u32, &'wire [FunctionStatementInput], DaeProvenance)> {
    match state.statements.get(state.next_statement)? {
        FunctionStatementInput::For {
            fold,
            statements,
            provenance,
        } => Some((*fold, statements, *provenance)),
        FunctionStatementInput::Assignment { .. } => None,
    }
}

fn consume_definition(
    wire: &StorageWire,
    dae: &DaeConstruction<'_>,
    state: &mut FunctionReplay<'_, '_>,
    ordinal: u32,
) -> Result<(), DaeConstructionError> {
    let input = definition_input(wire, state.function_index, ordinal)?;
    let seen = state
        .seen_definitions
        .get_mut(ordinal as usize)
        .ok_or_else(|| unknown("function definition", ordinal, input.provenance))?;
    if std::mem::replace(seen, true) {
        return Err(malformed("functions.definitions"));
    }
    let actual = dae.storage.functions[state.function_index]
        .definitions
        .get(ordinal as usize)
        .ok_or_else(|| unknown("function definition", ordinal, input.provenance))?;
    if actual.target == input.target
        && actual.rhs == input.rhs
        && actual.provenance == input.provenance
    {
        Ok(())
    } else {
        Err(malformed("functions.definitions"))
    }
}

fn expect_next_definition(
    dae: &DaeConstruction<'_>,
    function_index: usize,
    expected: u32,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let next = dae.storage.functions[function_index].definitions.len();
    if u32::try_from(next).ok() == Some(expected) {
        Ok(())
    } else {
        Err(DaeConstructionError::DuplicateDefinition {
            kind: "function definition",
            index: expected,
            span: provenance.span(),
        })
    }
}

fn checked_definition_target<'dae>(
    wire: &StorageWire,
    function_index: usize,
    input: &FunctionSsaDefinitionInput,
) -> Result<FunctionValueId<'dae>, DaeConstructionError> {
    function_wire(wire, function_index, input.provenance)?
        .values
        .get(input.target as usize)
        .ok_or_else(|| unknown("function value", input.target, input.provenance))?;
    let function = u32::try_from(function_index).map_err(|_| malformed("functions"))?;
    Ok(FunctionValueId::from_raw(function, input.target))
}

fn definition_input(
    wire: &StorageWire,
    function_index: usize,
    ordinal: u32,
) -> Result<&FunctionSsaDefinitionInput, DaeConstructionError> {
    let function = wire
        .functions
        .get(function_index)
        .ok_or_else(|| malformed("functions"))?;
    function
        .definitions
        .get(ordinal as usize)
        .ok_or_else(|| unknown("function definition", ordinal, function.declaration))
}

fn function_state_mut<'state, 'wire, 'dae>(
    functions: &'state mut [FunctionReplay<'wire, 'dae>],
    raw: u32,
    provenance: DaeProvenance,
) -> Result<&'state mut FunctionReplay<'wire, 'dae>, DaeConstructionError> {
    functions
        .get_mut(raw as usize)
        .ok_or_else(|| unknown("function", raw, provenance))
}

fn function_wire(
    wire: &StorageWire,
    index: usize,
    provenance: DaeProvenance,
) -> Result<&FunctionEntryWire, DaeConstructionError> {
    wire.functions
        .get(index)
        .ok_or_else(|| unknown("function", index as u32, provenance))
}

fn function_declaration(wire: &StorageWire, index: usize) -> DaeProvenance {
    wire.functions[index].declaration
}
