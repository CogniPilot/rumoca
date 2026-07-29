use super::*;
use serde::Serialize;
use serde::ser::SerializeSeq;

pub(super) struct FunctionArenaOutput<'storage> {
    functions: &'storage [FunctionEntry],
    folds: &'storage [FunctionFoldEntry],
}

impl<'storage> FunctionArenaOutput<'storage> {
    pub(super) const fn new(
        functions: &'storage [FunctionEntry],
        folds: &'storage [FunctionFoldEntry],
    ) -> Self {
        Self { functions, folds }
    }
}

impl Serialize for FunctionArenaOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut sequence = serializer.serialize_seq(Some(self.functions.len()))?;
        for (index, function) in self.functions.iter().enumerate() {
            let output =
                function_output(index, function, self.folds).map_err(serde::ser::Error::custom)?;
            sequence.serialize_element(&output)?;
        }
        sequence.end()
    }
}

#[derive(Serialize)]
struct FunctionOutput<'storage> {
    name: &'storage rumoca_core::VarName,
    parameters: Vec<NamedValueOutput<'storage>>,
    outputs: Vec<NamedValueOutput<'storage>>,
    locals: Vec<NamedValueOutput<'storage>>,
    statements: Vec<FunctionStatementOutput>,
    declaration: DaeProvenance,
}

#[derive(Serialize)]
struct NamedValueOutput<'storage> {
    name: &'storage rumoca_core::VarName,
    value_type: u32,
    declaration: DaeProvenance,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
enum FunctionStatementOutput {
    Assignment {
        target: u32,
        rhs: u32,
        provenance: DaeProvenance,
    },
    For {
        domain: u32,
        targets: Vec<u32>,
        statements: Vec<FunctionStatementOutput>,
        begin_provenance: DaeProvenance,
        finish_provenance: DaeProvenance,
    },
}

fn function_output<'storage>(
    function_index: usize,
    function: &'storage FunctionEntry,
    folds: &[FunctionFoldEntry],
) -> Result<FunctionOutput<'storage>, &'static str> {
    let output_count = function.output_values.len();
    let outputs = function
        .values
        .get(..output_count)
        .ok_or("function output values are not a prefix")?;
    let locals = function
        .values
        .get(output_count..)
        .ok_or("function local values are not a suffix")?;
    let definition = function
        .definition
        .as_ref()
        .ok_or("function body is incomplete")?;
    Ok(FunctionOutput {
        name: &function.name,
        parameters: function
            .parameter_values
            .iter()
            .map(|value| NamedValueOutput {
                name: &value.name,
                value_type: value.value_type,
                declaration: value.declaration,
            })
            .collect(),
        outputs: named_values(outputs),
        locals: named_values(locals),
        statements: project_statements(function_index, function, folds, &definition.statements)?,
        declaration: function.declaration,
    })
}

fn named_values(values: &[FunctionValueEntry]) -> Vec<NamedValueOutput<'_>> {
    values
        .iter()
        .map(|value| NamedValueOutput {
            name: &value.name,
            value_type: value.value_type,
            declaration: value.declaration,
        })
        .collect()
}

fn project_statements(
    function_index: usize,
    function: &FunctionEntry,
    folds: &[FunctionFoldEntry],
    statements: &[FunctionStatementWire],
) -> Result<Vec<FunctionStatementOutput>, &'static str> {
    statements
        .iter()
        .map(|statement| match statement {
            FunctionStatementWire::Assignment { definition } => {
                let definition = function
                    .definitions
                    .get(*definition as usize)
                    .ok_or("function assignment definition is missing")?;
                Ok(FunctionStatementOutput::Assignment {
                    target: definition.target,
                    rhs: definition.rhs,
                    provenance: definition.provenance,
                })
            }
            FunctionStatementWire::For {
                fold: fold_ordinal,
                statements,
                provenance,
            } => {
                let raw_function =
                    u32::try_from(function_index).map_err(|_| "function index exceeds u32")?;
                let raw = function
                    .folds
                    .get(*fold_ordinal as usize)
                    .ok_or("function fold identity is missing")?;
                let fold = folds
                    .get(*raw as usize)
                    .ok_or("function fold entry is missing")?;
                if fold.function != raw_function || fold.ordinal != *fold_ordinal {
                    return Err("function fold owner is inconsistent");
                }
                Ok(FunctionStatementOutput::For {
                    domain: fold.domain,
                    targets: fold.targets.clone(),
                    statements: project_statements(function_index, function, folds, statements)?,
                    begin_provenance: fold.provenance,
                    finish_provenance: *provenance,
                })
            }
        })
        .collect()
}

pub(super) fn reconstruct<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    reservations: Vec<FunctionReservation<'dae>>,
) -> Result<(), DaeConstructionError> {
    let mut functions = begin_functions(wire, dae, reservations)?;
    while ids.expressions.len() < wire.expressions.nodes.len() {
        let expression = wire_expression(wire, ids.expressions.len())?;
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
}

struct LoopReplay<'wire, 'dae> {
    fold_ordinal: u32,
    statements: &'wire [FunctionStatementInput],
    next_statement: usize,
    finish_provenance: DaeProvenance,
    target_count: usize,
    body: FunctionLoop<'dae>,
}

#[derive(Clone, Copy)]
struct AssignmentInput {
    target: u32,
    rhs: u32,
    provenance: DaeProvenance,
}

struct LoopInput<'wire> {
    domain: u32,
    targets: &'wire [u32],
    statements: &'wire [FunctionStatementInput],
    begin_provenance: DaeProvenance,
    finish_provenance: DaeProvenance,
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
            let body =
                dae.functions(|functions| functions.begin(reservation, function.declaration))?;
            Ok(FunctionReplay {
                function_index,
                body: Some(body),
                statements: &function.statements,
                next_statement: 0,
                active_loop: None,
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
    let expression = wire_expression(wire, ids.expressions.len())?;
    let provenance = expression.provenance;
    let state = function_state_mut(functions, function_raw, provenance)?;
    expect_function_value(wire, state.function_index, value_raw, provenance)?;
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
        let Some(assignment) = state
            .statements
            .get(state.next_statement)
            .and_then(assignment_input)
        else {
            break;
        };
        if assignment.rhs as usize >= ids.expressions.len() {
            break;
        }
        apply_root_assignment(wire, dae, ids, state, assignment)?;
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
        let statement = state
            .active_loop
            .as_ref()
            .and_then(|active| active.statements.get(active.next_statement));
        let Some(assignment) = statement.and_then(assignment_input) else {
            if matches!(statement, Some(FunctionStatementInput::For { .. })) {
                return Err(malformed("functions.statements.nesting"));
            }
            break;
        };
        if assignment.rhs as usize >= ids.expressions.len() {
            break;
        }
        apply_loop_assignment(wire, dae, ids, state, assignment)?;
        state
            .active_loop
            .as_mut()
            .ok_or_else(|| malformed("functions.statements"))?
            .next_statement += 1;
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
        return Err(malformed("functions.statements.nesting"));
    }
    apply_ready_root_assignments(wire, dae, ids, state)?;
    let input = next_root_loop(state.statements, state.next_statement)
        .ok_or_else(|| incomplete("function fold", fold_ordinal as usize, provenance))?;
    let expected_fold = checked_u32(
        dae.storage.functions[state.function_index].folds.len(),
        "function fold",
        input.begin_provenance,
    )?;
    if fold_ordinal != expected_fold {
        return Err(malformed("functions.statements.fold"));
    }
    let targets = checked_loop_targets(
        wire,
        state.function_index,
        input.targets,
        input.begin_provenance,
    )?;
    let parent = state.body.take().ok_or_else(|| {
        incomplete(
            "function body",
            state.function_index,
            input.begin_provenance,
        )
    })?;
    let expression_start = ids.expressions.len();
    let definition_start = dae.storage.functions[state.function_index]
        .definitions
        .len();
    let loop_body = dae.functions(|functions| {
        functions.begin_loop(
            parent,
            mapped(&ids.domains, input.domain, "domain", input.begin_provenance)?,
            targets,
            input.begin_provenance,
        )
    })?;
    if loop_body.fold().ordinal() != fold_ordinal {
        return Err(malformed("functions.statements.fold"));
    }
    record_generated_group(
        wire,
        dae,
        ids,
        GeneratedGroupReplay {
            function: function_raw,
            fold: fold_ordinal,
            target_count: input.targets.len(),
            group: GeneratedGroup::Parameter,
            expression_start,
            definition_start,
        },
    )?;
    state.next_statement += 1;
    state.active_loop = Some(LoopReplay {
        fold_ordinal,
        statements: input.statements,
        next_statement: 0,
        finish_provenance: input.finish_provenance,
        target_count: input.targets.len(),
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
        return Err(malformed("functions.statements"));
    }
    let expression_start = ids.expressions.len();
    let definition_start = dae.storage.functions[state.function_index]
        .definitions
        .len();
    let active = state
        .active_loop
        .take()
        .ok_or_else(|| incomplete("function fold", fold_ordinal as usize, provenance))?;
    let target_count = active.target_count;
    let body =
        dae.functions(|functions| functions.finish_loop(active.body, active.finish_provenance))?;
    record_generated_group(
        wire,
        dae,
        ids,
        GeneratedGroupReplay {
            function: function_raw,
            fold: fold_ordinal,
            target_count,
            group: GeneratedGroup::Output,
            expression_start,
            definition_start,
        },
    )?;
    state.body = Some(body);
    Ok(())
}

fn apply_ready_root_assignments<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
) -> Result<(), DaeConstructionError> {
    loop {
        let Some(assignment) = state
            .statements
            .get(state.next_statement)
            .and_then(assignment_input)
        else {
            return Ok(());
        };
        if assignment.rhs as usize >= ids.expressions.len() {
            return Ok(());
        }
        apply_root_assignment(wire, dae, ids, state, assignment)?;
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
        let statement = state
            .active_loop
            .as_ref()
            .and_then(|active| active.statements.get(active.next_statement));
        let Some(assignment) = statement.and_then(assignment_input) else {
            if matches!(statement, Some(FunctionStatementInput::For { .. })) {
                return Err(malformed("functions.statements.nesting"));
            }
            return Ok(());
        };
        if assignment.rhs as usize >= ids.expressions.len() {
            return Ok(());
        }
        apply_loop_assignment(wire, dae, ids, state, assignment)?;
        state
            .active_loop
            .as_mut()
            .ok_or_else(|| malformed("functions.statements"))?
            .next_statement += 1;
    }
}

fn apply_root_assignment<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
    input: AssignmentInput,
) -> Result<(), DaeConstructionError> {
    let target = checked_assignment_target(wire, state.function_index, input)?;
    let rhs = mapped_expression(ids, input.rhs, input.provenance)?;
    let body = state
        .body
        .as_mut()
        .ok_or_else(|| incomplete("function body", state.function_index, input.provenance))?;
    dae.functions(|functions| functions.assign(body, target, rhs, input.provenance))
}

fn apply_loop_assignment<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, 'dae>,
    input: AssignmentInput,
) -> Result<(), DaeConstructionError> {
    let target = checked_assignment_target(wire, state.function_index, input)?;
    let rhs = mapped_expression(ids, input.rhs, input.provenance)?;
    let active = state
        .active_loop
        .as_mut()
        .ok_or_else(|| incomplete("function fold", input.target as usize, input.provenance))?;
    dae.functions(|functions| {
        functions.assign_loop(&mut active.body, target, rhs, input.provenance)
    })
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
        let body = state.body.take().ok_or_else(|| {
            incomplete(
                "function body",
                state.function_index,
                function_declaration(wire, state.function_index),
            )
        })?;
        dae.functions(|functions| {
            functions.define(body, function_declaration(wire, state.function_index))
        })?;
    }
    Ok(())
}

#[derive(Clone, Copy)]
enum GeneratedGroup {
    Parameter,
    Output,
}

struct GeneratedGroupReplay {
    function: u32,
    fold: u32,
    target_count: usize,
    group: GeneratedGroup,
    expression_start: usize,
    definition_start: usize,
}

fn record_generated_group<'dae>(
    wire: &StorageWire,
    dae: &DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    replay: GeneratedGroupReplay,
) -> Result<(), DaeConstructionError> {
    let expected_expression_end = replay
        .expression_start
        .checked_add(replay.target_count)
        .ok_or_else(|| malformed("expressions.nodes"))?;
    let expected_definition_end = replay
        .definition_start
        .checked_add(replay.target_count)
        .ok_or_else(|| malformed("functions.statements"))?;
    if dae.storage.expressions.nodes.len() != expected_expression_end
        || dae.storage.functions[replay.function as usize]
            .definitions
            .len()
            != expected_definition_end
    {
        return Err(malformed("functions.statements"));
    }
    for (carried, raw) in (replay.expression_start..expected_expression_end).enumerate() {
        let expression = wire_expression(wire, raw)?;
        let definition_index = replay
            .definition_start
            .checked_add(carried)
            .ok_or_else(|| malformed("functions.statements"))?;
        let definition = checked_u32(
            definition_index,
            "function definition",
            expression.provenance,
        )?;
        expect_generated_node(
            expression.node,
            replay.function,
            replay.fold,
            carried,
            definition,
            replay.group,
        )?;
        let raw = u32::try_from(raw).map_err(|_| malformed("expressions.nodes"))?;
        record_expression(wire, dae, ids, ExprId::from_raw(raw), expression.provenance)?;
    }
    Ok(())
}

fn expect_generated_node(
    node: &ExprNodeWire,
    function: u32,
    fold: u32,
    carried: usize,
    definition: u32,
    group: GeneratedGroup,
) -> Result<(), DaeConstructionError> {
    let carried = u32::try_from(carried).map_err(|_| malformed("functions.statements.targets"))?;
    let matches = match (node, group) {
        (
            ExprNodeWire::FunctionFoldParameter {
                function: found_function,
                fold: found_fold,
                carried: found_carried,
                definition_ordinal,
            },
            GeneratedGroup::Parameter,
        )
        | (
            ExprNodeWire::FunctionFoldOutput {
                function: found_function,
                fold: found_fold,
                carried: found_carried,
                definition_ordinal,
            },
            GeneratedGroup::Output,
        ) => {
            *found_function == function
                && *found_fold == fold
                && *found_carried == carried
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

fn assignment_input(statement: &FunctionStatementInput) -> Option<AssignmentInput> {
    match statement {
        FunctionStatementInput::Assignment {
            target,
            rhs,
            provenance,
        } => Some(AssignmentInput {
            target: *target,
            rhs: *rhs,
            provenance: *provenance,
        }),
        FunctionStatementInput::For { .. } => None,
    }
}

fn next_root_loop(
    statements: &[FunctionStatementInput],
    next_statement: usize,
) -> Option<LoopInput<'_>> {
    match statements.get(next_statement)? {
        FunctionStatementInput::For {
            domain,
            targets,
            statements,
            begin_provenance,
            finish_provenance,
        } => Some(LoopInput {
            domain: *domain,
            targets,
            statements,
            begin_provenance: *begin_provenance,
            finish_provenance: *finish_provenance,
        }),
        FunctionStatementInput::Assignment { .. } => None,
    }
}

fn checked_loop_targets<'dae>(
    wire: &StorageWire,
    function_index: usize,
    targets: &[u32],
    provenance: DaeProvenance,
) -> Result<Vec<FunctionValueId<'dae>>, DaeConstructionError> {
    targets
        .iter()
        .map(|target| {
            expect_function_value(wire, function_index, *target, provenance)?;
            Ok(FunctionValueId::from_raw(
                checked_u32(function_index, "function", provenance)?,
                *target,
            ))
        })
        .collect()
}

fn checked_assignment_target<'dae>(
    wire: &StorageWire,
    function_index: usize,
    input: AssignmentInput,
) -> Result<FunctionValueId<'dae>, DaeConstructionError> {
    expect_function_value(wire, function_index, input.target, input.provenance)?;
    Ok(FunctionValueId::from_raw(
        checked_u32(function_index, "function", input.provenance)?,
        input.target,
    ))
}

fn expect_function_value(
    wire: &StorageWire,
    function_index: usize,
    value: u32,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let function = function_wire(wire, function_index, provenance)?;
    let count = function
        .outputs
        .len()
        .checked_add(function.locals.len())
        .ok_or_else(|| malformed("functions.values"))?;
    if (value as usize) < count {
        Ok(())
    } else {
        Err(unknown("function value", value, provenance))
    }
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
