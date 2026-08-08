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
        for function in self.functions {
            let output = function_output(function, self.folds);
            sequence.serialize_element(&output)?;
        }
        sequence.end()
    }
}

fn function_output<'storage>(
    function: &'storage FunctionEntry,
    folds: &[FunctionFoldEntry],
) -> FunctionEntryWire<&'storage rumoca_core::VarName> {
    let output_count = function.output_values.len();
    let (outputs, locals) = function.values.split_at(output_count);
    let definition = function
        .definition
        .as_ref()
        .expect("finalized function has a body");
    let (statements, external) = match definition {
        FunctionBodyEntry::Modelica(body) => {
            (project_statements(function, folds, &body.statements), None)
        }
        FunctionBodyEntry::External(body) => (
            Vec::new(),
            Some(ExternalBodyInput {
                purity: body.purity,
                language: body.language,
                symbol: &body.symbol,
                arguments: body.arguments.clone(),
                result: body.result,
                linkage: body.linkage.clone(),
            }),
        ),
    };
    FunctionEntryWire {
        name: &function.name,
        parameters: function
            .parameter_values
            .iter()
            .map(|value| FunctionNamedValueWire {
                name: &value.name,
                value_type: value.value_type,
                declaration: value.declaration,
            })
            .collect(),
        outputs: named_values(outputs),
        locals: named_values(locals),
        statements,
        external,
        declaration: function.declaration,
    }
}

fn named_values(
    values: &[FunctionValueEntry],
) -> Vec<FunctionNamedValueWire<&rumoca_core::VarName>> {
    values
        .iter()
        .map(|value| FunctionNamedValueWire {
            name: &value.name,
            value_type: value.value_type,
            declaration: value.declaration,
        })
        .collect()
}

fn project_statements(
    function: &FunctionEntry,
    folds: &[FunctionFoldEntry],
    statements: &[FunctionStatementWire],
) -> Vec<FunctionStatementInput> {
    statements
        .iter()
        .map(|statement| match statement {
            FunctionStatementWire::Assignment { definition } => {
                let definition = &function.definitions[*definition as usize];
                FunctionStatementInput::Assignment {
                    target: definition.target,
                    rhs: definition.rhs,
                    provenance: definition.provenance,
                }
            }
            FunctionStatementWire::AssignmentGroup {
                definitions,
                conditional,
            } => FunctionStatementInput::AssignmentGroup {
                assignments: definitions
                    .iter()
                    .map(|definition| {
                        let definition = &function.definitions[*definition as usize];
                        FunctionAssignmentInput {
                            target: definition.target,
                            rhs: definition.rhs,
                            provenance: definition.provenance,
                        }
                    })
                    .collect(),
                conditional: conditional
                    .as_ref()
                    .map(|conditional| FunctionConditionalInput {
                        conditions: conditional.conditions.clone(),
                        branches: conditional.branches.clone(),
                        fallback: conditional.fallback.clone(),
                    }),
            },
            FunctionStatementWire::Assertion {
                condition,
                message,
                provenance,
            } => FunctionStatementInput::Assertion {
                condition: *condition,
                message: *message,
                provenance: *provenance,
            },
            FunctionStatementWire::For {
                fold: fold_ordinal,
                statements,
                provenance,
            } => {
                let fold = &folds[function.folds[*fold_ordinal as usize] as usize];
                FunctionStatementInput::For {
                    domain: fold.domain,
                    targets: fold.targets.clone(),
                    statements: project_statements(function, folds, statements),
                    begin_provenance: fold.provenance,
                    finish_provenance: *provenance,
                }
            }
        })
        .collect()
}

pub(super) fn reconstruct<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for component in function_graph::function_components(wire)? {
        if component.recursive {
            reconstruct_recursive_component(
                wire,
                dae,
                ids,
                &component.functions,
                component.expression_end,
            )?;
        } else {
            reconstruct_acyclic_function(
                wire,
                dae,
                ids,
                component.functions[0],
                component.expression_end,
            )?;
        }
    }
    while ids.expressions.len() < wire.expressions.nodes.len() {
        let expression = wire_expression(wire, ids.expressions.len())?;
        match expression.node {
            ExprNodeWire::FunctionValue { .. }
            | ExprNodeWire::FunctionFoldParameter { .. }
            | ExprNodeWire::FunctionFoldOutput { .. } => {
                return Err(malformed("expressions.nodes.function_scope"));
            }
            _ => {
                if !super::reconstruct_next_expression(wire, dae, ids)? {
                    return Err(malformed("expressions.nodes"));
                }
            }
        }
    }
    expect_expression_arena_consumed(wire, dae, ids)
}

fn reconstruct_acyclic_function<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    function_index: usize,
    expression_end: usize,
) -> Result<(), DaeConstructionError> {
    let signature = function_signature(wire, ids, function_index)?;
    let (function, ()) = dae.function(signature, |dae, reservation| {
        expect_ordinal(
            "function",
            function_index,
            reservation.function().index(),
            wire.functions[function_index].declaration,
        )?;
        ids.functions.push(reservation.function());
        reconstruct_function_values(
            &wire.functions[function_index],
            dae,
            &ids.types,
            &reservation,
        )?;
        replay_component(wire, dae, ids, vec![reservation], expression_end)
    })?;
    expect_ordinal(
        "function",
        function_index,
        function.index(),
        wire.functions[function_index].declaration,
    )
}

fn reconstruct_recursive_component<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    function_indices: &[usize],
    expression_end: usize,
) -> Result<(), DaeConstructionError> {
    let mut signatures = function_indices
        .iter()
        .map(|&index| function_signature(wire, ids, index))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter();
    let first = signatures
        .next()
        .expect("wire function components are nonempty");
    dae.recursive_functions(first, signatures, |dae, reservations| {
        for (&index, reservation) in function_indices.iter().zip(&reservations) {
            expect_ordinal(
                "function",
                index,
                reservation.function().index(),
                wire.functions[index].declaration,
            )?;
            ids.functions.push(reservation.function());
        }
        for (&index, reservation) in function_indices.iter().zip(&reservations) {
            reconstruct_function_values(&wire.functions[index], dae, &ids.types, reservation)?;
        }
        replay_component(wire, dae, ids, reservations, expression_end)
    })?;
    Ok(())
}

fn function_signature<'dae>(
    wire: &StorageWire,
    ids: &WireIds<'dae>,
    index: usize,
) -> Result<FunctionSignature<'dae>, DaeConstructionError> {
    let function = &wire.functions[index];
    Ok(FunctionSignature::new(
        function.name.clone(),
        map_function_value_types(&ids.types, &function.parameters)?,
        map_function_value_types(&ids.types, &function.outputs)?,
        function.declaration,
    ))
}

fn replay_component<'group, 'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    reservations: Vec<FunctionReservation<'group, 'dae>>,
    expression_end: usize,
) -> Result<(), DaeConstructionError> {
    let mut functions = begin_functions(wire, dae, reservations)?;
    while ids.expressions.len() < expression_end {
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
            ExprNodeWire::FunctionFoldParameter { function } => {
                replay_loop_entry(wire, dae, ids, &mut functions, *function)?;
            }
            ExprNodeWire::FunctionFoldOutput { function } => {
                replay_loop_exit(wire, dae, ids, &mut functions, *function)?;
            }
            _ => {
                if !super::reconstruct_next_expression(wire, dae, ids)? {
                    return Err(malformed("expressions.nodes"));
                }
            }
        }
    }
    finish_functions(wire, dae, ids, functions)
}

struct FunctionReplay<'wire, 'group, 'dae> {
    function_index: usize,
    operations: Vec<ReplayOp<'wire>>,
    next_operation: usize,
    capability: Option<ReplayCapability<'group, 'dae>>,
}

enum ReplayCapability<'group, 'dae> {
    Body(FunctionBody<'dae>),
    Fold(FunctionLoop<'dae>),
    /// An MLS §12.9 external body keeps its reservation until its checked
    /// interface replays; it never opens a Modelica body build state.
    External(FunctionReservation<'group, 'dae>),
}

#[derive(Clone, Copy)]
enum ReplayOp<'wire> {
    Assign(AssignmentInput),
    AssignGroup(AssignmentGroupInput<'wire>),
    Assert(AssertionInput),
    BeginFold(FoldInput<'wire>),
    EndFold(FoldEnd),
}

#[derive(Clone, Copy)]
struct AssignmentGroupInput<'wire> {
    assignments: &'wire [FunctionAssignmentInput],
    conditional: Option<&'wire FunctionConditionalInput>,
}

#[derive(Clone, Copy)]
struct AssignmentInput {
    target: u32,
    rhs: u32,
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
struct AssertionInput {
    condition: u32,
    message: u32,
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
struct FoldInput<'wire> {
    domain: u32,
    targets: &'wire [u32],
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
struct FoldEnd {
    target_count: usize,
    provenance: DaeProvenance,
}

fn begin_functions<'wire, 'group, 'dae>(
    wire: &'wire StorageWire,
    dae: &mut DaeConstruction<'dae>,
    reservations: Vec<FunctionReservation<'group, 'dae>>,
) -> Result<Vec<FunctionReplay<'wire, 'group, 'dae>>, DaeConstructionError> {
    reservations
        .into_iter()
        .map(|reservation| {
            let function_index = reservation.function().index() as usize;
            let function = function_wire(
                wire,
                function_index,
                function_declaration(wire, function_index),
            )?;
            if function.external.is_some() {
                if !function.statements.is_empty() {
                    return Err(malformed("functions.external"));
                }
                return Ok(FunctionReplay {
                    function_index,
                    operations: Vec::new(),
                    next_operation: 0,
                    capability: Some(ReplayCapability::External(reservation)),
                });
            }
            let body =
                dae.functions(|functions| functions.begin(reservation, function.declaration))?;
            Ok(FunctionReplay {
                function_index,
                operations: flatten_operations(&function.statements)?,
                next_operation: 0,
                capability: Some(ReplayCapability::Body(body)),
            })
        })
        .collect()
}

fn flatten_operations(
    statements: &[FunctionStatementInput],
) -> Result<Vec<ReplayOp<'_>>, DaeConstructionError> {
    let mut operations = Vec::with_capacity(statements.len());
    for statement in statements {
        match statement {
            FunctionStatementInput::Assignment { .. } => {
                operations.push(ReplayOp::Assign(assignment(statement)?));
            }
            FunctionStatementInput::AssignmentGroup {
                assignments,
                conditional,
            } => {
                operations.push(ReplayOp::AssignGroup(AssignmentGroupInput {
                    assignments,
                    conditional: conditional.as_ref(),
                }));
            }
            FunctionStatementInput::Assertion { .. } => {
                operations.push(ReplayOp::Assert(assertion(statement)?));
            }
            FunctionStatementInput::For {
                domain,
                targets,
                statements,
                begin_provenance,
                finish_provenance,
            } => push_fold_operations(
                &mut operations,
                FoldInput {
                    domain: *domain,
                    targets,
                    provenance: *begin_provenance,
                },
                statements,
                *finish_provenance,
            )?,
        }
    }
    Ok(operations)
}

fn push_fold_operations<'wire>(
    operations: &mut Vec<ReplayOp<'wire>>,
    begin: FoldInput<'wire>,
    statements: &'wire [FunctionStatementInput],
    finish: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    operations.push(ReplayOp::BeginFold(begin));
    for statement in statements {
        match statement {
            FunctionStatementInput::Assignment { .. } => {
                operations.push(ReplayOp::Assign(assignment(statement)?));
            }
            FunctionStatementInput::AssignmentGroup {
                assignments,
                conditional,
            } => {
                operations.push(ReplayOp::AssignGroup(AssignmentGroupInput {
                    assignments,
                    conditional: conditional.as_ref(),
                }));
            }
            FunctionStatementInput::Assertion { .. } => {
                operations.push(ReplayOp::Assert(assertion(statement)?));
            }
            FunctionStatementInput::For { .. } => {
                return Err(malformed("functions.statements.nesting"));
            }
        }
    }
    operations.push(ReplayOp::EndFold(FoldEnd {
        target_count: begin.targets.len(),
        provenance: finish,
    }));
    Ok(())
}

fn assignment(statement: &FunctionStatementInput) -> Result<AssignmentInput, DaeConstructionError> {
    match statement {
        FunctionStatementInput::Assignment {
            target,
            rhs,
            provenance,
        } => Ok(AssignmentInput {
            target: *target,
            rhs: *rhs,
            provenance: *provenance,
        }),
        FunctionStatementInput::AssignmentGroup { .. }
        | FunctionStatementInput::Assertion { .. } => {
            Err(malformed("functions.statements.assignment"))
        }
        FunctionStatementInput::For { .. } => Err(malformed("functions.statements.nesting")),
    }
}

fn assertion(statement: &FunctionStatementInput) -> Result<AssertionInput, DaeConstructionError> {
    match statement {
        FunctionStatementInput::Assertion {
            condition,
            message,
            provenance,
        } => Ok(AssertionInput {
            condition: *condition,
            message: *message,
            provenance: *provenance,
        }),
        FunctionStatementInput::Assignment { .. }
        | FunctionStatementInput::AssignmentGroup { .. }
        | FunctionStatementInput::For { .. } => Err(malformed("functions.statements.assertion")),
    }
}

fn replay_read<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    functions: &mut [FunctionReplay<'_, '_, 'dae>],
    identity: (u32, u32, u32),
) -> Result<(), DaeConstructionError> {
    let (function_raw, value_raw, definition_ordinal) = identity;
    let expression = wire_expression(wire, ids.expressions.len())?;
    let provenance = expression.provenance;
    let state = function_state_mut(functions, function_raw, provenance)?;
    expect_function_value(wire, state.function_index, value_raw, provenance)?;
    advance_to_definition(
        wire,
        dae,
        ids,
        state,
        value_raw,
        definition_ordinal,
        provenance,
    )?;
    let body = body_for_definition(dae, state, value_raw, definition_ordinal, provenance)?;
    let value = FunctionValueId::from_raw(function_raw, value_raw);
    let rebuilt = dae.functions(|functions| functions.read(body, value, provenance))?;
    record_expression(dae, ids, rebuilt, provenance)
}

fn advance_to_definition<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
    value: u32,
    definition: u32,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    loop {
        let current = current_definition_ordinal(dae, state, value, provenance)?;
        if current == Some(definition) {
            return Ok(());
        }
        if let Some(current) = current
            && current > definition
        {
            return Err(DaeConstructionError::InvalidFunctionValueRead {
                value,
                expected_definition: Some(current),
                found_definition: definition,
                span: provenance.span(),
            });
        }
        if !apply_next_ready_operation(wire, dae, ids, state)? {
            break;
        }
    }
    body_for_definition(dae, state, value, definition, provenance).map(|_| ())
}

fn current_definition_ordinal<'dae>(
    dae: &mut DaeConstruction<'dae>,
    state: &FunctionReplay<'_, '_, 'dae>,
    value: u32,
    provenance: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    let body = match state
        .capability
        .as_ref()
        .ok_or_else(|| incomplete("function capability", state.function_index, provenance))?
    {
        ReplayCapability::Body(body) => body,
        ReplayCapability::Fold(body) => body.body(),
        ReplayCapability::External(_) => return Err(malformed("functions.external")),
    };
    let value = FunctionValueId::from_raw(state.function_index as u32, value);
    match dae.functions(|functions| functions.current_definition_id(body, value, provenance)) {
        Ok(definition) => Ok(Some(FunctionDefinitionId::ordinal(definition))),
        Err(DaeConstructionError::IncompleteDefinition {
            kind: "function value",
            index,
            ..
        }) if index == value.ordinal() => Ok(None),
        Err(error) => Err(error),
    }
}

fn apply_next_ready_operation<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
) -> Result<bool, DaeConstructionError> {
    let Some(operation) = state.operations.get(state.next_operation).copied() else {
        return Ok(false);
    };
    match operation {
        ReplayOp::Assign(assignment) => {
            if assignment.rhs as usize >= ids.expressions.len() {
                return Ok(false);
            }
            apply_assignment(wire, dae, ids, state, assignment)?;
        }
        ReplayOp::AssignGroup(group) => {
            if group
                .assignments
                .iter()
                .any(|assignment| assignment.rhs as usize >= ids.expressions.len())
                || group.conditional.is_some_and(|conditional| {
                    conditional
                        .conditions
                        .iter()
                        .chain(conditional.branches.iter().flatten())
                        .chain(&conditional.fallback)
                        .any(|expression| *expression as usize >= ids.expressions.len())
                })
            {
                return Ok(false);
            }
            apply_assignment_group(wire, dae, ids, state, group)?;
        }
        ReplayOp::Assert(assertion) => {
            if [assertion.condition, assertion.message]
                .into_iter()
                .any(|expression| expression as usize >= ids.expressions.len())
            {
                return Ok(false);
            }
            apply_assertion(dae, ids, state, assertion)?;
        }
        ReplayOp::BeginFold(_) | ReplayOp::EndFold(_) => return Ok(false),
    }
    state.next_operation += 1;
    Ok(true)
}

fn apply_ready_operations<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
) -> Result<(), DaeConstructionError> {
    while apply_next_ready_operation(wire, dae, ids, state)? {}
    Ok(())
}

fn apply_assertion<'dae>(
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
    input: AssertionInput,
) -> Result<(), DaeConstructionError> {
    let condition = mapped_expression(ids, input.condition, input.provenance)?;
    let message = mapped_expression(ids, input.message, input.provenance)?;
    let capability = state.capability.as_mut().ok_or_else(|| {
        incomplete(
            "function capability",
            state.function_index,
            input.provenance,
        )
    })?;
    dae.functions(|functions| match capability {
        ReplayCapability::Body(body) => {
            functions.assertion(body, condition, message, input.provenance)
        }
        ReplayCapability::Fold(loop_body) => {
            functions.assertion_loop(loop_body, condition, message, input.provenance)
        }
        ReplayCapability::External(_) => Err(malformed("functions.external")),
    })
}

fn apply_assignment<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
    input: AssignmentInput,
) -> Result<(), DaeConstructionError> {
    let target = checked_assignment_target(wire, state.function_index, input)?;
    let rhs = mapped_expression(ids, input.rhs, input.provenance)?;
    let capability = state.capability.as_mut().ok_or_else(|| {
        incomplete(
            "function capability",
            state.function_index,
            input.provenance,
        )
    })?;
    dae.functions(|functions| match capability {
        ReplayCapability::Body(body) => functions.assign(body, target, rhs, input.provenance),
        ReplayCapability::Fold(body) => functions.assign_loop(body, target, rhs, input.provenance),
        ReplayCapability::External(_) => Err(malformed("functions.external")),
    })
}

fn apply_assignment_group<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    state: &mut FunctionReplay<'_, '_, 'dae>,
    group: AssignmentGroupInput<'_>,
) -> Result<(), DaeConstructionError> {
    let inputs = group.assignments;
    let provenance = inputs
        .first()
        .map(|input| input.provenance)
        .ok_or_else(|| malformed("functions.statements.assignment_group"))?;
    if inputs.iter().any(|input| input.provenance != provenance) {
        return Err(malformed(
            "functions.statements.assignment_group.provenance",
        ));
    }
    let assignments = inputs
        .iter()
        .map(|input| {
            Ok((
                checked_assignment_target(
                    wire,
                    state.function_index,
                    AssignmentInput {
                        target: input.target,
                        rhs: input.rhs,
                        provenance: input.provenance,
                    },
                )?,
                mapped_expression(ids, input.rhs, input.provenance)?,
            ))
        })
        .collect::<Result<Vec<_>, DaeConstructionError>>()?;
    let capability = state
        .capability
        .as_mut()
        .ok_or_else(|| incomplete("function capability", state.function_index, provenance))?;
    dae.functions(|functions| match (capability, group.conditional) {
        (ReplayCapability::Body(body), Some(conditional)) => functions.replay_conditional_all(
            body,
            &assignments,
            &conditional
                .conditions
                .iter()
                .map(|value| mapped_expression(ids, *value, provenance))
                .collect::<Result<Vec<_>, _>>()?,
            &conditional
                .branches
                .iter()
                .map(|branch| {
                    branch
                        .iter()
                        .map(|value| mapped_expression(ids, *value, provenance))
                        .collect::<Result<Vec<_>, _>>()
                })
                .collect::<Result<Vec<_>, _>>()?,
            &conditional
                .fallback
                .iter()
                .map(|value| mapped_expression(ids, *value, provenance))
                .collect::<Result<Vec<_>, _>>()?,
            provenance,
        ),
        (ReplayCapability::Fold(body), Some(conditional)) => functions.replay_conditional_all_loop(
            body,
            &assignments,
            &conditional
                .conditions
                .iter()
                .map(|value| mapped_expression(ids, *value, provenance))
                .collect::<Result<Vec<_>, _>>()?,
            &conditional
                .branches
                .iter()
                .map(|branch| {
                    branch
                        .iter()
                        .map(|value| mapped_expression(ids, *value, provenance))
                        .collect::<Result<Vec<_>, _>>()
                })
                .collect::<Result<Vec<_>, _>>()?,
            &conditional
                .fallback
                .iter()
                .map(|value| mapped_expression(ids, *value, provenance))
                .collect::<Result<Vec<_>, _>>()?,
            provenance,
        ),
        (ReplayCapability::Body(body), None) => {
            functions.assign_all(body, &assignments, provenance)
        }
        (ReplayCapability::Fold(body), None) => {
            functions.assign_all_loop(body, &assignments, provenance)
        }
        (ReplayCapability::External(_), _) => Err(malformed("functions.external")),
    })
}

fn body_for_definition<'state, 'wire, 'group, 'dae>(
    dae: &mut DaeConstruction<'dae>,
    state: &'state FunctionReplay<'wire, 'group, 'dae>,
    value: u32,
    definition: u32,
    provenance: DaeProvenance,
) -> Result<&'state FunctionBody<'dae>, DaeConstructionError> {
    let body = match state
        .capability
        .as_ref()
        .ok_or_else(|| incomplete("function capability", state.function_index, provenance))?
    {
        ReplayCapability::Body(body) => body,
        ReplayCapability::Fold(body) => body.body(),
        ReplayCapability::External(_) => return Err(malformed("functions.external")),
    };
    let value_id = FunctionValueId::from_raw(state.function_index as u32, value);
    let current =
        dae.functions(|functions| functions.current_definition_id(body, value_id, provenance))?;
    if current.ordinal() == definition {
        Ok(body)
    } else {
        Err(DaeConstructionError::InvalidFunctionValueRead {
            value,
            expected_definition: Some(current.ordinal()),
            found_definition: definition,
            span: provenance.span(),
        })
    }
}

fn replay_loop_entry<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    functions: &mut [FunctionReplay<'_, '_, 'dae>],
    function_raw: u32,
) -> Result<(), DaeConstructionError> {
    let provenance = wire_expression(wire, ids.expressions.len())?.provenance;
    let state = function_state_mut(functions, function_raw, provenance)?;
    apply_ready_operations(wire, dae, ids, state)?;
    let pending_fold = dae.storage.functions[state.function_index].folds.len();
    let Some(ReplayOp::BeginFold(input)) = state.operations.get(state.next_operation).copied()
    else {
        return Err(incomplete("function fold begin", pending_fold, provenance));
    };
    let targets =
        checked_loop_targets(wire, state.function_index, input.targets, input.provenance)?;
    let capability = state.capability.take().ok_or_else(|| {
        incomplete(
            "function capability",
            state.function_index,
            input.provenance,
        )
    })?;
    let ReplayCapability::Body(parent) = capability else {
        return Err(malformed("functions.statements.nesting"));
    };
    let expression_start = ids.expressions.len();
    let definition_start = dae.storage.functions[state.function_index]
        .definitions
        .len();
    let loop_body = dae.functions(|functions| {
        functions.begin_loop(
            parent,
            mapped(&ids.domains, input.domain, "domain", input.provenance)?,
            targets,
            input.provenance,
        )
    })?;
    record_generated_group(
        wire,
        dae,
        ids,
        GeneratedGroupReplay {
            function: function_raw,
            target_count: input.targets.len(),
            group: GeneratedGroup::Parameter,
            expression_start,
            definition_start,
        },
    )?;
    state.next_operation += 1;
    state.capability = Some(ReplayCapability::Fold(loop_body));
    Ok(())
}

fn replay_loop_exit<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    functions: &mut [FunctionReplay<'_, '_, 'dae>],
    function_raw: u32,
) -> Result<(), DaeConstructionError> {
    let provenance = wire_expression(wire, ids.expressions.len())?.provenance;
    let state = function_state_mut(functions, function_raw, provenance)?;
    apply_ready_operations(wire, dae, ids, state)?;
    let open_fold = dae.storage.functions[state.function_index]
        .folds
        .len()
        .saturating_sub(1);
    let Some(ReplayOp::EndFold(input)) = state.operations.get(state.next_operation).copied() else {
        return Err(incomplete("function fold end", open_fold, provenance));
    };
    let expression_start = ids.expressions.len();
    let definition_start = dae.storage.functions[state.function_index]
        .definitions
        .len();
    let capability = state.capability.take().ok_or_else(|| {
        incomplete(
            "function capability",
            state.function_index,
            input.provenance,
        )
    })?;
    let ReplayCapability::Fold(body) = capability else {
        return Err(malformed("functions.statements.fold"));
    };
    let body = dae.functions(|functions| functions.finish_loop(body, input.provenance))?;
    record_generated_group(
        wire,
        dae,
        ids,
        GeneratedGroupReplay {
            function: function_raw,
            target_count: input.target_count,
            group: GeneratedGroup::Output,
            expression_start,
            definition_start,
        },
    )?;
    state.next_operation += 1;
    state.capability = Some(ReplayCapability::Body(body));
    Ok(())
}

fn finish_functions<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    mut functions: Vec<FunctionReplay<'_, '_, 'dae>>,
) -> Result<(), DaeConstructionError> {
    for state in &mut functions {
        apply_ready_operations(wire, dae, ids, state)?;
        if state.next_operation != state.operations.len() {
            return Err(incomplete(
                "function operation",
                state.next_operation,
                function_declaration(wire, state.function_index),
            ));
        }
        let capability = state.capability.take().ok_or_else(|| {
            incomplete(
                "function capability",
                state.function_index,
                function_declaration(wire, state.function_index),
            )
        })?;
        match capability {
            ReplayCapability::Body(body) => {
                dae.functions(|functions| {
                    functions.define(body, function_declaration(wire, state.function_index))
                })?;
            }
            ReplayCapability::External(reservation) => {
                replay_external_body(wire, dae, ids, state.function_index, reservation)?;
            }
            ReplayCapability::Fold(_) => {
                return Err(incomplete(
                    "function fold",
                    state.function_index,
                    function_declaration(wire, state.function_index),
                ));
            }
        }
    }
    Ok(())
}

/// Replay one MLS §12.9 external interface through its checked construction op.
///
/// Wire columns supply only raw ordinals; every argument, produced output, and
/// link fact re-enters the IR through `define_external`, so a forged interface
/// fails exactly where a forged production interface would.
fn replay_external_body<'group, 'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    function_index: usize,
    reservation: FunctionReservation<'group, 'dae>,
) -> Result<(), DaeConstructionError> {
    let declaration = function_declaration(wire, function_index);
    let function = function_wire(wire, function_index, declaration)?;
    let external = function
        .external
        .as_ref()
        .ok_or_else(|| malformed("functions.external"))?;
    let function_raw = checked_u32(function_index, "function", declaration)?;
    let mut arguments = Vec::with_capacity(external.arguments.len());
    for argument in &external.arguments {
        arguments.push(match argument {
            ExternalArgumentEntry::Input(expression) => {
                ExternalArgument::Input(mapped_expression(ids, *expression, declaration)?)
            }
            ExternalArgumentEntry::Output(value) => {
                expect_function_value(wire, function_index, *value, declaration)?;
                ExternalArgument::Output(FunctionValueId::from_raw(function_raw, *value))
            }
        });
    }
    let result = match external.result {
        Some(value) => {
            expect_function_value(wire, function_index, value, declaration)?;
            Some(FunctionValueId::from_raw(function_raw, value))
        }
        None => None,
    };
    let body = ExternalFunctionBody::new(
        external.purity,
        external.language,
        external.symbol.clone(),
        arguments,
        result,
        external.linkage.clone(),
    );
    dae.functions(|functions| functions.define_external(reservation, body, declaration))
}

#[derive(Clone, Copy)]
enum GeneratedGroup {
    Parameter,
    Output,
}

struct GeneratedGroupReplay {
    function: u32,
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
    for raw in replay.expression_start..expected_expression_end {
        let expression = wire_expression(wire, raw)?;
        expect_generated_node(expression.node, replay.function, replay.group)?;
        let raw = u32::try_from(raw).map_err(|_| malformed("expressions.nodes"))?;
        record_expression(dae, ids, ExprId::from_raw(raw), expression.provenance)?;
    }
    Ok(())
}

/// A generated node names only the function whose fold transition issued it.
///
/// The fold ordinal, carried position, and defining assignment that node holds
/// in the arena are results of `begin_loop`/`finish_loop`, so replay re-issues
/// them here instead of reading them back from wire columns.
fn expect_generated_node(
    node: &ExprNodeWire,
    function: u32,
    group: GeneratedGroup,
) -> Result<(), DaeConstructionError> {
    let matches = match (node, group) {
        (
            ExprNodeWire::FunctionFoldParameter {
                function: found_function,
            },
            GeneratedGroup::Parameter,
        )
        | (
            ExprNodeWire::FunctionFoldOutput {
                function: found_function,
            },
            GeneratedGroup::Output,
        ) => *found_function == function,
        _ => false,
    };
    if matches {
        Ok(())
    } else {
        Err(malformed("expressions.nodes"))
    }
}

fn record_expression<'dae>(
    dae: &DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    rebuilt: ExprId<'dae>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let index = ids.expressions.len();
    if rebuilt.index() as usize != index {
        return Err(malformed("expressions.nodes"));
    }
    if dae.storage.expressions.provenance[index] != provenance {
        return Err(DaeConstructionError::ShapeMismatch {
            span: provenance.span(),
        });
    }
    ids.expressions.push(rebuilt);
    Ok(())
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

fn function_state_mut<'state, 'wire, 'group, 'dae>(
    functions: &'state mut [FunctionReplay<'wire, 'group, 'dae>],
    raw: u32,
    provenance: DaeProvenance,
) -> Result<&'state mut FunctionReplay<'wire, 'group, 'dae>, DaeConstructionError> {
    functions
        .iter_mut()
        .find(|state| state.function_index == raw as usize)
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
