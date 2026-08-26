//! Array-native projection of reachable Modelica functions into GALEC.
// SPEC_0021 file-size exception: this module still owns both function-body
// control-flow projection and value materialization. split plan: move checked
// correlation materialization into its own lower::user_functions submodule.

use super::*;

mod indexed_update;
mod tensor_loops;
mod update_aliasing;

use indexed_update::{lower_indexed_function_update, preserves_function_target};
/// Only the retired contraction-fission recognizer still asks this, and it
/// compiles only where its assertion does.
#[cfg(debug_assertions)]
pub(super) use tensor_loops::is_reorderable;
pub(super) use tensor_loops::nest_tensor_loops;
pub(in crate::lower) use update_aliasing::same_index;
use update_aliasing::{GroupDefinition, UpdatedAggregate, order_by_value_dependency};

pub(super) fn lower_reachable<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    roots: HashSet<u32>,
) -> Result<Vec<gast::UserFunction>, GalecTargetError> {
    let mut pending = roots.into_iter().collect::<Vec<_>>();
    pending.sort_unstable();
    let mut lowered = HashMap::new();
    while let Some(raw) = pending.pop() {
        if lowered.contains_key(&raw) {
            continue;
        }
        let id =
            view.function_id(raw as usize)
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: format!("reachable function identity {raw} does not resolve"),
                })?;
        let (function, calls) = lower_function(view, definitions, id)?;
        lowered.insert(raw, function);
        for call in calls {
            if !lowered.contains_key(&call) {
                pending.push(call);
            }
        }
    }
    let mut functions = lowered.into_iter().collect::<Vec<_>>();
    functions.sort_by_key(|(id, _)| *id);
    Ok(functions
        .into_iter()
        .map(|(_, function)| function)
        .collect())
}

pub(super) fn is_directly_lowerable<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionId<'dae>,
) -> bool {
    let Some(function) = view.function(function) else {
        return false;
    };
    if function.is_external() || function.result_types().is_empty() {
        return false;
    }
    function
        .parameters()
        .all(|parameter| is_direct_value_type(view, parameter.value_type()))
        && function
            .result_types()
            .iter()
            .all(|result| is_direct_value_type(view, result))
        && function
            .values()
            .all(|value| is_direct_value_type(view, value.value_type()))
}

fn is_direct_value_type<'dae>(
    view: dae::DaeView<'dae>,
    value_type: dae::ValueTypeId<'dae>,
) -> bool {
    let Some(ty) = view.value_type(value_type) else {
        return false;
    };
    !ty.is_record()
        || (ty.dimensions().is_empty()
            && (0..ty.record_field_count()).all(|ordinal| {
                view.record_field(value_type, ordinal)
                    .and_then(|(_, field)| view.value_type(field))
                    .is_some_and(|field| !field.is_record())
            }))
}

pub(super) fn function_name(
    view: dae::DaeView<'_>,
    function: dae::FunctionView<'_>,
) -> Result<gast::Name, GalecTargetError> {
    let duplicate_count = (0..view.function_count())
        .filter_map(|index| view.function_id(index))
        .filter_map(|id| view.function(id))
        .filter(|candidate| candidate.name() == function.name())
        .count();
    let name = if duplicate_count > 1 {
        format!(
            "{}_specialization_{}",
            function.name(),
            function.id().index()
        )
    } else {
        function.name().to_string()
    };
    crate::mangle::galec_variable_name(&name)
}

pub(super) fn parameter_name(
    parameter: dae::FunctionParameterView<'_>,
) -> Result<gast::Name, GalecTargetError> {
    crate::mangle::galec_variable_name(parameter.name().as_str())
}

pub(super) fn value_name(
    value: dae::FunctionValueView<'_>,
) -> Result<gast::Name, GalecTargetError> {
    crate::mangle::galec_variable_name(value.name().as_str())
}

pub(super) fn record_value_field_name(
    value: dae::FunctionValueView<'_>,
    field: &rumoca_core::VarName,
) -> Result<gast::Name, GalecTargetError> {
    crate::mangle::galec_variable_name(&format!("{}.{}", value.name(), field))
}

pub(super) fn record_parameter_field_name(
    parameter: dae::FunctionParameterView<'_>,
    field: &rumoca_core::VarName,
) -> Result<gast::Name, GalecTargetError> {
    crate::mangle::galec_variable_name(&format!("{}.{}", parameter.name(), field))
}

pub(super) fn flattened_result_index<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    selected_output: u32,
    selected_field: Option<u32>,
) -> Option<usize> {
    let mut flattened = 0;
    for (output, result) in function.result_types().iter().enumerate() {
        let ty = view.value_type(result)?;
        if !ty.is_record() {
            if output == selected_output as usize && selected_field.is_none() {
                return Some(flattened);
            }
            flattened += 1;
            continue;
        }
        for field in 0..ty.record_field_count() {
            if output == selected_output as usize && selected_field == Some(field as u32) {
                return Some(flattened);
            }
            flattened += 1;
        }
    }
    None
}

pub(super) fn dimensions(extents: &[u32]) -> Vec<gast::Dimension> {
    extents
        .iter()
        .map(|extent| gast::Dimension::Expr(gast::Expression::Integer(i64::from(*extent))))
        .collect()
}

fn lower_function<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    id: dae::FunctionId<'dae>,
) -> Result<(gast::UserFunction, HashSet<u32>), GalecTargetError> {
    if !is_directly_lowerable(view, id) {
        return Err(unsupported(
            "galec-user-function",
            "reachable Modelica function is external, has no result, or uses a nested record value type that the GALEC profile cannot represent"
                .to_owned(),
            view.function(id)
                .expect("checked function identity resolves")
                .declaration()
                .span(),
        ));
    }
    let function = view
        .function(id)
        .expect("checked function identity resolves");
    let variables = HashMap::new();
    let previous = HashMap::new();
    let mut lowerer =
        ExpressionLowerer::with_do_step_effects(view, definitions, &variables, &previous);
    lowerer.function_scope = Some(id);
    let parameters = function_parameters(view, function)?;
    let locals = function_locals(view, function)?;
    let mut statements = Vec::new();
    for statement in function.statements() {
        lower_function_statement(view, statement, &mut lowerer, &mut statements)?;
    }
    let statements = coalesce_correlated_guards(statements);
    let mut locals = locals;
    locals.extend(lowerer.take_temporary_locals());
    let calls = lowerer.take_called_user_functions();
    Ok((
        gast::UserFunction {
            kind: gast::FunctionKind::Stateless,
            name: function_name(view, function)?,
            signals: Vec::new(),
            parameters,
            locals,
            statements,
            span: function.declaration().span(),
        },
        calls,
    ))
}

fn function_parameters<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
) -> Result<Vec<gast::Parameter>, GalecTargetError> {
    let mut parameters = Vec::new();
    for parameter in function.parameters() {
        append_parameter_declarations(view, parameter, &mut parameters)?;
    }
    for value in function
        .values()
        .filter(|value| value.role() == dae::FunctionValueRole::Output)
    {
        append_output_declarations(view, value, &mut parameters)?;
    }
    Ok(parameters)
}

fn append_parameter_declarations<'dae>(
    view: dae::DaeView<'dae>,
    parameter: dae::FunctionParameterView<'dae>,
    declarations: &mut Vec<gast::Parameter>,
) -> Result<(), GalecTargetError> {
    let ty = view
        .value_type(parameter.value_type())
        .expect("checked function parameter type resolves");
    if !ty.is_record() {
        declarations.push(gast::Parameter {
            direction: gast::Direction::Input,
            decl: parameter_declaration(view, parameter)?,
        });
        return Ok(());
    }
    for ordinal in 0..ty.record_field_count() {
        let (field_name, field_type) = view
            .record_field(parameter.value_type(), ordinal)
            .expect("checked record parameter field resolves");
        let field_type = view
            .value_type(field_type)
            .expect("checked record parameter field type resolves");
        declarations.push(gast::Parameter {
            direction: gast::Direction::Input,
            decl: primitive_declaration(
                field_type,
                field_name,
                record_parameter_field_name(parameter, field_name)?,
                parameter.declaration().span(),
            )?,
        });
    }
    Ok(())
}

fn append_output_declarations<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::FunctionValueView<'dae>,
    declarations: &mut Vec<gast::Parameter>,
) -> Result<(), GalecTargetError> {
    let ty = view
        .value_type(value.value_type())
        .expect("checked function output type resolves");
    if !ty.is_record() {
        declarations.push(gast::Parameter {
            direction: gast::Direction::Output,
            decl: value_declaration(view, value)?,
        });
        return Ok(());
    }
    for ordinal in 0..ty.record_field_count() {
        let (field_name, field_type) = view
            .record_field(value.value_type(), ordinal)
            .expect("checked record output field resolves");
        let field_type = view
            .value_type(field_type)
            .expect("checked record output field type resolves");
        declarations.push(gast::Parameter {
            direction: gast::Direction::Output,
            decl: primitive_declaration(
                field_type,
                field_name,
                record_value_field_name(value, field_name)?,
                value.declaration().span(),
            )?,
        });
    }
    Ok(())
}

fn function_locals<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
) -> Result<Vec<gast::VariableDeclaration>, GalecTargetError> {
    let output_names = flattened_output_names(view, function)?;
    let mut locals = Vec::new();
    for value in function
        .values()
        .filter(|value| value.role() == dae::FunctionValueRole::Local)
    {
        let ty = view
            .value_type(value.value_type())
            .expect("checked function value type resolves");
        if !ty.is_record() {
            let declaration = value_declaration(view, value)?;
            if !output_names.contains(&declaration.name) {
                locals.push(declaration);
            }
            continue;
        }
        for ordinal in 0..ty.record_field_count() {
            let (field_name, field_type) = view
                .record_field(value.value_type(), ordinal)
                .expect("checked record field type resolves");
            let field_type = view
                .value_type(field_type)
                .expect("checked record field value type resolves");
            let declaration = gast::VariableDeclaration {
                ty: gast::TypeRef::Primitive(scalar_type(
                    field_type.scalar_type(),
                    field_name.as_str(),
                    value.declaration().span(),
                )?),
                name: record_value_field_name(value, field_name)?,
                dimensions: dimensions(field_type.dimensions()),
                range: gast::RangeAttributes::default(),
                span: value.declaration().span(),
            };
            if !output_names.contains(&declaration.name) {
                locals.push(declaration);
            }
        }
    }
    Ok(locals)
}

fn flattened_output_names<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
) -> Result<Vec<gast::Name>, GalecTargetError> {
    let mut declarations = Vec::new();
    for output in function
        .values()
        .filter(|value| value.role() == dae::FunctionValueRole::Output)
    {
        append_output_declarations(view, output, &mut declarations)?;
    }
    Ok(declarations
        .into_iter()
        .map(|parameter| parameter.decl.name)
        .collect())
}

fn parameter_declaration<'dae>(
    view: dae::DaeView<'dae>,
    parameter: dae::FunctionParameterView<'dae>,
) -> Result<gast::VariableDeclaration, GalecTargetError> {
    let ty = view
        .value_type(parameter.value_type())
        .expect("checked function parameter type resolves");
    primitive_declaration(
        ty,
        parameter.name(),
        parameter_name(parameter)?,
        parameter.declaration().span(),
    )
}

fn value_declaration<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::FunctionValueView<'dae>,
) -> Result<gast::VariableDeclaration, GalecTargetError> {
    let ty = view
        .value_type(value.value_type())
        .expect("checked function value type resolves");
    primitive_declaration(
        ty,
        value.name(),
        value_name(value)?,
        value.declaration().span(),
    )
}

fn primitive_declaration(
    ty: &dae::ValueType,
    source_name: &rumoca_core::VarName,
    name: gast::Name,
    span: Span,
) -> Result<gast::VariableDeclaration, GalecTargetError> {
    Ok(gast::VariableDeclaration {
        ty: gast::TypeRef::Primitive(scalar_type(ty.scalar_type(), source_name.as_str(), span)?),
        name,
        dimensions: dimensions(ty.dimensions()),
        range: gast::RangeAttributes::default(),
        span,
    })
}

/// Names of the function locals a statement list may assign, including inside
/// nested loops and conditional groups.
///
/// Structural only: it reads targets, never expressions, so it is cheap enough
/// to run at each block boundary and cannot depend on lowering having happened.
fn assigned_local_names<'dae>(
    view: dae::DaeView<'dae>,
    statements: dae::FunctionStatements<'dae>,
) -> HashSet<String> {
    let mut assigned = HashSet::new();
    collect_assigned_local_names(view, statements, &mut assigned);
    assigned
}

fn collect_assigned_local_names<'dae>(
    view: dae::DaeView<'dae>,
    statements: dae::FunctionStatements<'dae>,
    assigned: &mut HashSet<String>,
) {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Assignment { definition } => {
                insert_definition_target(view, definition, assigned);
            }
            dae::FunctionStatementView::AssignmentGroup { definitions, .. } => {
                for definition in definitions.iter() {
                    insert_definition_target(view, definition, assigned);
                }
            }
            dae::FunctionStatementView::For {
                statements: body, ..
            } => collect_assigned_local_names(view, body, assigned),
            dae::FunctionStatementView::Assertion { .. } => {}
        }
    }
}

fn insert_definition_target<'dae>(
    view: dae::DaeView<'dae>,
    definition: dae::FunctionDefinitionView<'dae>,
    assigned: &mut HashSet<String>,
) {
    if let Some(target) = view
        .function(definition.id().function())
        .expect("checked function identity resolves")
        .values()
        .find(|value| value.id() == definition.target())
    {
        assigned.insert(target.name().as_str().to_owned());
    }
}

fn lower_function_statement<'a, 'dae>(
    view: dae::DaeView<'dae>,
    statement: dae::FunctionStatementView<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    match statement {
        dae::FunctionStatementView::Assignment { definition } => {
            lower_function_assignment(view, definition, lowerer, statements)?;
            lowerer.finish_statement_group();
        }
        dae::FunctionStatementView::AssignmentGroup {
            definitions,
            conditional,
        } => {
            if let Some(conditional) = conditional {
                lower_function_conditional_group(
                    view,
                    definitions,
                    conditional,
                    lowerer,
                    statements,
                )?;
            } else {
                let group = definitions.iter().collect::<Vec<_>>();
                let group =
                    order_by_value_dependency(view, &group, |definition| GroupDefinition {
                        target: definition.target(),
                        value: definition.rhs(),
                        span: definition.provenance().span(),
                    })?;
                let outer = lowerer.carry_assigned_primitives(group.carried);
                let lowered = group.ordered.into_iter().try_for_each(|definition| {
                    lower_function_assignment(view, definition, lowerer, statements)
                });
                lowerer.carry_assigned_primitives(outer);
                lowered?;
            }
            lowerer.finish_statement_group();
        }
        dae::FunctionStatementView::Assertion {
            condition,
            provenance,
            ..
        } => {
            lowerer.lower_function_assertion(condition, provenance.span())?;
            statements.extend(lowerer.take_prefix_statements());
        }
        dae::FunctionStatementView::For {
            fold,
            statements: body,
            provenance,
        } => {
            // The ranges a reader finds inside the body are the union of what
            // held before the loop with what one iteration can leave behind, so
            // the entry ranges are solved as a fixpoint over speculative passes.
            let span = provenance.span();
            let assigned = assigned_local_names(view, body.clone());
            let speculative_body = body.clone();
            let carried = LoopIntegerBounds::enter(
                lowerer,
                assigned,
                function_loop_runs_body(view, fold),
                |speculative| {
                    let mut discarded = Vec::new();
                    lower_function_for(
                        view,
                        fold,
                        speculative_body.clone(),
                        span,
                        speculative,
                        &mut discarded,
                    )
                },
            );
            lower_function_for(view, fold, body, span, lowerer, statements)?;
            carried.commit(lowerer, span)?;
            lowerer.finish_statement_group();
        }
    }
    Ok(())
}

struct LoweredFunctionConditionalBranch {
    prefix: Vec<gast::Spanned<gast::Statement>>,
    condition: gast::Expression,
    body: Vec<gast::Spanned<gast::Statement>>,
    span: Span,
}

/// Lower the fallback branch of a function conditional.
///
/// The fallback is its own statement sequence, so it is ordered against its own
/// values by [`order_by_value_dependency`], exactly as each guarded branch is.
fn lower_function_conditional_fallback<'a, 'dae>(
    view: dae::DaeView<'dae>,
    definitions: &[(usize, dae::FunctionDefinitionView<'dae>)],
    conditional: dae::FunctionConditionalView<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let mut fallback = Vec::new();
    let fallback_values = conditional.fallback().collect::<Vec<_>>();
    let arm = order_by_value_dependency(view, definitions, |(definition_ordinal, definition)| {
        GroupDefinition {
            target: definition.target(),
            value: fallback_values[definition_ordinal],
            span: definition.provenance().span(),
        }
    })?;
    let outer = lowerer.carry_assigned_primitives(arm.carried);
    let lowered =
        lower_arm_definitions(view, &arm.ordered, &fallback_values, lowerer, &mut fallback);
    lowerer.carry_assigned_primitives(outer);
    lowered?;
    Ok(fallback)
}

/// Lower one arm of a function conditional in the order that arm was given.
fn lower_arm_definitions<'a, 'dae>(
    view: dae::DaeView<'dae>,
    arm: &[(usize, dae::FunctionDefinitionView<'dae>)],
    values: &[dae::ExprId<'dae>],
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    for (definition_ordinal, definition) in arm.iter().copied() {
        let target = view
            .function(definition.id().function())
            .expect("checked function identity resolves")
            .values()
            .find(|candidate| candidate.id() == definition.target())
            .expect("checked function conditional target resolves");
        lower_function_value_assignment(
            view,
            target,
            values[definition_ordinal],
            definition.provenance().span(),
            lowerer,
            statements,
        )?;
    }
    Ok(())
}

fn lower_function_conditional_group<'a, 'dae>(
    view: dae::DaeView<'dae>,
    definitions: dae::FunctionDefinitionValues<'dae>,
    conditional: dae::FunctionConditionalView<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    // This is the group's preference order, not its final order: each arm is
    // ordered against its own values by `order_by_value_dependency`, which moves
    // a producer ahead of the siblings that read it and leaves every independent
    // pair where this key puts it. Aggregate destinations come first among
    // independent definitions: they preserve the widest value identity and
    // materialize any shared branch-local call before scalar consumers. The
    // original ordinal is the deterministic tie-breaker.
    let mut definitions = definitions.iter().enumerate().collect::<Vec<_>>();
    definitions.sort_by_key(|(ordinal, definition)| {
        let target = view
            .function(definition.id().function())
            .expect("checked function identity resolves")
            .values()
            .find(|candidate| candidate.id() == definition.target())
            .expect("checked conditional target resolves");
        let ty = view
            .value_type(target.value_type())
            .expect("checked conditional target type resolves");
        (!ty.is_record(), ty.dimensions().is_empty(), *ordinal)
    });
    let conditions = conditional.conditions().collect::<Vec<_>>();
    let activation_operands = conditions
        .iter()
        .map(|condition| condition.index())
        .collect::<Vec<_>>();
    let entry_materialization = lowerer.conditional_materialization_snapshot();
    let mut reaching_bounds = ConditionalIntegerBounds::enter(lowerer);
    let mut branches = Vec::with_capacity(conditions.len());
    for (ordinal, condition_id) in conditions.into_iter().enumerate() {
        reaching_bounds.start_arm(lowerer);
        let prefix_start = lowerer.pending_prefix_statements.len();
        let condition = lowerer.lower(condition_id)?;
        let span = view
            .expression(condition_id)
            .expect("checked function conditional condition resolves")
            .provenance()
            .span();
        require_boolean(&condition, span)?;
        let prefix = lowerer.pending_prefix_statements.split_off(prefix_start);
        let condition_materialization = lowerer.conditional_materialization_snapshot();
        lowerer
            .conditional_activation_path
            .push(ConditionalActivationKey {
                kind: ConditionalActivationKind::FunctionConditional,
                operands: activation_operands.clone(),
                branch: u32::try_from(ordinal).map_err(|_| GalecTargetError::LoweringInternal {
                    detail: "function conditional branch ordinal exceeds capacity".to_owned(),
                })?,
            });
        let mut body = Vec::new();
        let values = conditional
            .branch(ordinal)
            .expect("checked function conditional branch resolves")
            .collect::<Vec<_>>();
        let arm =
            order_by_value_dependency(view, &definitions, |(definition_ordinal, definition)| {
                GroupDefinition {
                    target: definition.target(),
                    value: values[definition_ordinal],
                    span: definition.provenance().span(),
                }
            })?;
        let outer = lowerer.carry_assigned_primitives(arm.carried);
        let lowered = lower_arm_definitions(view, &arm.ordered, &values, lowerer, &mut body);
        lowerer.carry_assigned_primitives(outer);
        lowered?;
        lowerer.conditional_activation_path.pop();
        lowerer.restore_conditional_materialization(&condition_materialization);
        reaching_bounds.finish_arm(lowerer);
        branches.push(LoweredFunctionConditionalBranch {
            prefix,
            condition: condition.expression,
            body,
            span,
        });
    }

    lowerer
        .conditional_activation_path
        .push(ConditionalActivationKey {
            kind: ConditionalActivationKind::FunctionConditional,
            operands: activation_operands,
            branch: u32::try_from(conditional.branch_count()).map_err(|_| {
                GalecTargetError::LoweringInternal {
                    detail: "function conditional fallback ordinal exceeds capacity".to_owned(),
                }
            })?,
        });
    reaching_bounds.start_arm(lowerer);
    let fallback = lower_function_conditional_fallback(view, &definitions, conditional, lowerer)?;
    lowerer.conditional_activation_path.pop();
    lowerer.restore_conditional_materialization(&entry_materialization);
    reaching_bounds.finish_arm(lowerer);
    reaching_bounds.commit(lowerer);
    statements.extend(nest_function_conditional_branches(branches, fallback));
    Ok(())
}

fn nest_function_conditional_branches(
    branches: Vec<LoweredFunctionConditionalBranch>,
    mut fallback: Vec<gast::Spanned<gast::Statement>>,
) -> Vec<gast::Spanned<gast::Statement>> {
    for branch in branches.into_iter().rev() {
        let mut statements = branch.prefix;
        statements.push(gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![gast::IfBranch {
                    condition: gast::Condition::Expression(branch.condition),
                    body: branch.body,
                    span: branch.span,
                }],
                else_body: Some(fallback),
            }),
            branch.span,
        ));
        fallback = statements;
    }
    fallback
}

fn lower_function_assignment<'a, 'dae>(
    view: dae::DaeView<'dae>,
    definition: dae::FunctionDefinitionView<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let target = view
        .function(definition.id().function())
        .expect("checked function identity resolves")
        .values()
        .find(|value| value.id() == definition.target())
        .expect("checked function definition target resolves");
    lower_function_value_assignment(
        view,
        target,
        definition.rhs(),
        definition.provenance().span(),
        lowerer,
        statements,
    )
}

fn lower_function_value_assignment<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    expression: dae::ExprId<'dae>,
    span: Span,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let target_type = view
        .value_type(target.value_type())
        .expect("checked function target type resolves");
    if lower_shared_record_assignment(
        view,
        target,
        target_type,
        expression,
        span,
        lowerer,
        statements,
    )? {
        return Ok(());
    }
    if let dae::ExpressionOperation::Conditional(operands) = view
        .expression(expression)
        .expect("checked function assignment expression resolves")
        .operation()
    {
        lower_conditional_function_value_assignment(
            view, target, operands, span, lowerer, statements,
        )?;
        if !target_type.is_record() {
            lowerer.remember_joined_assignment(expression.index(), value_name(target)?);
        }
        return Ok(());
    }
    if preserves_function_target(view, target, expression) {
        if !target_type.is_record() {
            lowerer.remember_primitive_assignment(expression.index(), value_name(target)?);
        }
        return Ok(());
    }
    if let Some(updates) =
        lower_indexed_function_update(view, target, target_type, expression, lowerer, span)?
    {
        statements.extend(lowerer.drain_prefix_statements());
        statements.extend(updates);
        lowerer.remember_primitive_assignment(expression.index(), value_name(target)?);
        return Ok(());
    }
    if target_type.is_record() {
        lower_record_function_assignment(
            view,
            target,
            target_type,
            expression,
            span,
            lowerer,
            statements,
        )
    } else {
        lower_primitive_function_assignment(
            target,
            target_type,
            expression,
            span,
            lowerer,
            statements,
        )?;
        lowerer.remember_primitive_assignment(expression.index(), value_name(target)?);
        Ok(())
    }
}

fn lower_shared_record_assignment<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    expression: dae::ExprId<'dae>,
    span: Span,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<bool, GalecTargetError> {
    if !target_type.is_record()
        || !target_type.dimensions().is_empty()
        || !lowerer
            .materialized_shared_record_fields
            .contains_key(&(expression.index(), 0))
    {
        return Ok(false);
    }
    for ordinal in 0..target_type.record_field_count() {
        let (field_name, _) = view
            .record_field(target.value_type(), ordinal)
            .expect("checked shared record target field resolves");
        let value = lowerer
            .materialized_shared_record_fields
            .get(&(expression.index(), ordinal))
            .expect("checked shared record materializes every field")
            .clone();
        let destination = gast::Reference::local(record_value_field_name(target, field_name)?);
        if matches!(&value, gast::Expression::Ref(source) if same_local_reference(&destination, source))
        {
            continue;
        }
        statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: destination,
                value,
            },
            span,
        ));
    }
    Ok(true)
}

fn lower_conditional_function_value_assignment<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    operands: dae::ExpressionOperands<'dae>,
    span: Span,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    materialize_common_record_conditionals(view, operands, lowerer, statements)?;
    let activation_operands = conditional_activation_operands(operands);
    let entry_materialization = lowerer.conditional_materialization_snapshot();
    let mut reaching_bounds = ConditionalIntegerBounds::enter(lowerer);
    let mut branches = Vec::with_capacity(operands.len() / 2);
    for ordinal in (0..operands.len() - 1).step_by(2) {
        reaching_bounds.start_arm(lowerer);
        let condition_id = operands
            .get(ordinal)
            .expect("checked conditional assignment condition");
        let prefix_start = lowerer.pending_prefix_statements.len();
        let condition = lowerer.lower(condition_id)?;
        require_boolean(&condition, span)?;
        let prefix = lowerer.pending_prefix_statements.split_off(prefix_start);
        let condition_materialization = lowerer.conditional_materialization_snapshot();
        lowerer
            .conditional_activation_path
            .push(ConditionalActivationKey {
                kind: ConditionalActivationKind::FunctionConditional,
                operands: activation_operands.clone(),
                branch: u32::try_from(ordinal / 2).map_err(|_| {
                    GalecTargetError::LoweringInternal {
                        detail: "nested function conditional branch exceeds capacity".to_owned(),
                    }
                })?,
            });
        let mut body = Vec::new();
        lower_function_value_assignment(
            view,
            target,
            operands
                .get(ordinal + 1)
                .expect("checked conditional assignment branch value"),
            span,
            lowerer,
            &mut body,
        )?;
        lowerer.conditional_activation_path.pop();
        lowerer.restore_conditional_materialization(&condition_materialization);
        reaching_bounds.finish_arm(lowerer);
        branches.push(LoweredFunctionConditionalBranch {
            prefix,
            condition: condition.expression,
            body,
            span,
        });
    }
    lowerer
        .conditional_activation_path
        .push(ConditionalActivationKey {
            kind: ConditionalActivationKind::FunctionConditional,
            operands: activation_operands,
            branch: u32::try_from(operands.len() / 2).map_err(|_| {
                GalecTargetError::LoweringInternal {
                    detail: "nested function conditional fallback exceeds capacity".to_owned(),
                }
            })?,
        });
    reaching_bounds.start_arm(lowerer);
    let mut fallback = Vec::new();
    lower_function_value_assignment(
        view,
        target,
        operands
            .get(operands.len() - 1)
            .expect("checked conditional assignment fallback"),
        span,
        lowerer,
        &mut fallback,
    )?;
    lowerer.conditional_activation_path.pop();
    lowerer.restore_conditional_materialization(&entry_materialization);
    reaching_bounds.finish_arm(lowerer);
    reaching_bounds.commit(lowerer);
    statements.extend(nest_function_conditional_branches(branches, fallback));
    Ok(())
}

fn materialize_common_record_conditionals<'a, 'dae>(
    view: dae::DaeView<'dae>,
    operands: dae::ExpressionOperands<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let value_roots = (1..operands.len() - 1)
        .step_by(2)
        .chain(std::iter::once(operands.len() - 1))
        .map(|ordinal| operands.get(ordinal).expect("checked conditional value"))
        .collect::<Vec<_>>();
    let mut common: Option<HashSet<u32>> = None;
    for root in value_roots {
        let mut descendants = HashSet::new();
        dae::for_each_expression(view, root, |id, _| {
            descendants.insert(id.index());
        });
        common = Some(match common {
            Some(current) => current.intersection(&descendants).copied().collect(),
            None => descendants,
        });
    }
    let mut candidates = common.unwrap_or_default().into_iter().collect::<Vec<_>>();
    candidates.sort_unstable();
    for raw in candidates {
        let Some(expression) = view.expression_id(raw as usize) else {
            continue;
        };
        let node = view
            .expression(expression)
            .expect("checked common record expression resolves");
        if !node.value_type().is_record()
            || !node.value_type().dimensions().is_empty()
            || !matches!(node.operation(), dae::ExpressionOperation::Conditional(_))
            || expression_calls_asserting_function(view, expression)
            || lowerer
                .materialized_shared_record_fields
                .contains_key(&(raw, 0))
        {
            continue;
        }
        statements.extend(materialize_shared_record(view, expression, lowerer)?);
    }
    Ok(())
}

pub(super) fn expression_calls_asserting_function<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let mut calls_asserting_function = false;
    dae::for_each_expression(view, expression, |_, node| {
        let dae::ExpressionOperation::Call { function, .. } = node.operation() else {
            return;
        };
        calls_asserting_function |= view
            .function(function)
            .is_some_and(|function| first_function_assertion(function.statements()).is_some());
    });
    calls_asserting_function
}

fn materialize_shared_record<'a, 'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let node = view
        .expression(expression)
        .expect("checked shared record expression resolves");
    let record_type = node.value_type();
    let span = node.provenance().span();
    let mut statements = Vec::new();
    let mut fields = Vec::with_capacity(record_type.record_field_count());
    for ordinal in 0..record_type.record_field_count() {
        let (field_name, field_type_id) = view
            .record_field(node.value_type_id(), ordinal)
            .expect("checked shared record field resolves");
        let field_type = view
            .value_type(field_type_id)
            .expect("checked shared record field type resolves");
        let name = crate::mangle::galec_variable_name(&format!(
            "rumoca.tmp.shared.expr{}.{}",
            expression.index(),
            field_name
        ))?;
        lowerer.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar_type(
                field_type.scalar_type(),
                field_name.as_str(),
                span,
            )?),
            name: name.clone(),
            dimensions: dimensions(field_type.dimensions()),
            range: gast::RangeAttributes::default(),
            span,
        });
        if let Some(lowered) = lower_tensor_function_assignment(
            TensorAssignment {
                target: name.clone(),
                target_type: field_type,
                expression,
                record_field: Some(ordinal),
                aggregate: None,
                span,
            },
            lowerer,
        )? {
            statements.extend(lowered.before);
            if let Some(nested) = lowered.nested {
                statements.push(nested);
            }
        } else {
            let value = lowerer.lower_aggregate_record_field(expression, ordinal, field_type_id)?;
            statements.extend(lowerer.drain_prefix_statements());
            statements.push(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: gast::Reference::local(name.clone()),
                    value,
                },
                span,
            ));
        }
        fields.push((
            (expression.index(), ordinal),
            gast::Expression::Ref(gast::Reference::local(name)),
        ));
    }
    lowerer.materialized_shared_record_fields.extend(fields);
    Ok(statements)
}

fn lower_record_function_assignment<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    expression: dae::ExprId<'dae>,
    span: Span,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    if let Some(call) =
        lowerer.lower_direct_record_call_assignment(expression, target, target_type, span)?
    {
        statements.extend(lowerer.drain_prefix_statements());
        statements.push(call);
        return Ok(());
    }
    let mut record_statements = Vec::new();
    for ordinal in 0..target_type.record_field_count() {
        lower_record_function_field(
            view,
            target,
            expression,
            span,
            ordinal,
            lowerer,
            &mut record_statements,
        )?;
    }
    statements.extend(merge_guarded_tensor_loops(record_statements));
    Ok(())
}

fn lower_record_function_field<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    expression: dae::ExprId<'dae>,
    span: Span,
    ordinal: usize,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let (field_name, field_type) = view
        .record_field(target.value_type(), ordinal)
        .expect("checked record target field resolves");
    let field_view = view
        .value_type(field_type)
        .expect("checked record target field type resolves");
    if let Some(lowered) = lower_tensor_function_assignment(
        TensorAssignment {
            target: record_value_field_name(target, field_name)?,
            target_type: field_view,
            expression,
            record_field: Some(ordinal),
            aggregate: None,
            span,
        },
        lowerer,
    )? {
        statements.extend(lowered.before);
        if let Some(nested) = lowered.nested
            && !is_identity_assignment_tree(&nested)
        {
            statements.push(nested);
        }
        return Ok(());
    }
    let value = lowerer.lower_aggregate_record_field(expression, ordinal, field_type)?;
    statements.extend(lowerer.drain_prefix_statements());
    let target = gast::Reference::local(record_value_field_name(target, field_name)?);
    if matches!(&value, gast::Expression::Ref(source) if same_local_reference(&target, source)) {
        return Ok(());
    }
    statements.push(gast::Spanned::new(
        gast::Statement::Assignment { target, value },
        span,
    ));
    Ok(())
}

fn is_identity_assignment_tree(statement: &gast::Spanned<gast::Statement>) -> bool {
    match &statement.node {
        gast::Statement::Assignment { target, value } => {
            matches!(value, gast::Expression::Ref(source) if same_local_reference(target, source))
        }
        gast::Statement::For(loop_statement) if loop_statement.body.len() == 1 => {
            is_identity_assignment_tree(&loop_statement.body[0])
        }
        _ => false,
    }
}

fn same_local_reference(lhs: &gast::Reference, rhs: &gast::Reference) -> bool {
    let (gast::Reference::Local(lhs), gast::Reference::Local(rhs)) = (lhs, rhs) else {
        return false;
    };
    lhs.name.lexeme() == rhs.name.lexeme() && lhs.subscripts == rhs.subscripts
}

fn lower_primitive_function_assignment<'a, 'dae>(
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    expression: dae::ExprId<'dae>,
    span: Span,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let target_name = value_name(target)?;
    if let Some(call) =
        lowerer.lower_direct_aggregate_call_assignment(expression, target_name.clone(), span)?
    {
        statements.extend(lowerer.drain_prefix_statements());
        statements.push(call);
        return Ok(());
    }
    if let Some(lowered) = lower_tensor_function_assignment(
        TensorAssignment {
            target: target_name.clone(),
            target_type,
            expression,
            record_field: None,
            aggregate: Some(UpdatedAggregate {
                value: target,
                rank: target_type.dimensions().len(),
            }),
            span,
        },
        lowerer,
    )? {
        statements.extend(lowered.before);
        if let Some(nested) = lowered.nested {
            statements.push(nested);
        }
        return Ok(());
    }
    let target_scalar = scalar_type(target_type.scalar_type(), target.name().as_str(), span)?;
    let value = lowerer.lower_aggregate_expression_as(expression, target_scalar)?;
    statements.extend(lowerer.drain_prefix_statements());
    // Carry the assigned range forward so a later subscript by this local can
    // be proven in bounds. `pivotRow := n - reverseIndex + 1` is the shape that
    // matters: an index derived from a loop binder, then used to subscript in
    // the same body. The record is narrowed at each control-flow join (a
    // conditional unions its arms, a loop drops what its body writes), so what
    // is remembered always bounds every value a reader can observe.
    if target_scalar == gast::ScalarType::Integer && target_type.dimensions().is_empty() {
        lowerer.remember_local_integer_bounds(target_name.clone(), &value);
    }
    statements.push(gast::Spanned::new(
        gast::Statement::Assignment {
            target: gast::Reference::local(target_name),
            value,
        },
        span,
    ));
    Ok(())
}

fn lower_function_for<'a, 'dae>(
    view: dae::DaeView<'dae>,
    fold: dae::FunctionFoldId<'dae>,
    body: dae::FunctionStatements<'dae>,
    span: Span,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let fold_view = view
        .function_fold(fold)
        .expect("checked function fold resolves");
    let domain = view
        .domain(fold_view.domain())
        .expect("checked function fold domain resolves");
    let names = function_loop_names(fold, domain);
    let bounds_depth = lowerer.loop_index_bounds.len();
    for (binder, name) in domain.structured().binders.iter().zip(&names) {
        lowerer.loop_index_bounds.push(LoopIndexBound {
            name: name.clone(),
            minimum: binder.lower.min(binder.upper),
            maximum: binder.lower.max(binder.upper),
        });
    }
    lowerer.comprehension_frames.push(ComprehensionFrame {
        domain: fold_view.domain().index(),
        binders: names
            .iter()
            .cloned()
            .map(|name| gast::Expression::Ref(gast::Reference::local(name)))
            .collect(),
    });
    let mut lowered_body = Vec::new();
    for nested in body {
        lower_function_statement(view, nested, lowerer, &mut lowered_body)?;
    }
    lowerer.comprehension_frames.pop();
    lowerer.loop_index_bounds.truncate(bounds_depth);
    statements.extend(wrap_function_loops(domain, names, lowered_body, span));
    Ok(())
}

/// Whether the loop nest emitted for one fold always runs its body.
///
/// Every binder is emitted with a literal start, stop and step, so the nest runs
/// its body exactly when each binder's range holds at least one value. A fold
/// with no binder emits no loop at all and so runs its body once.
fn function_loop_runs_body<'dae>(
    view: dae::DaeView<'dae>,
    fold: dae::FunctionFoldId<'dae>,
) -> bool {
    let Some(fold_view) = view.function_fold(fold) else {
        return false;
    };
    let Some(domain) = view.domain(fold_view.domain()) else {
        return false;
    };
    domain
        .structured()
        .binders
        .iter()
        .all(|binder| match binder.step.signum() {
            1 => binder.lower <= binder.upper,
            -1 => binder.lower >= binder.upper,
            _ => false,
        })
}

fn function_loop_names(
    fold: dae::FunctionFoldId<'_>,
    domain: dae::DomainView<'_>,
) -> Vec<gast::Name> {
    domain
        .structured()
        .binders
        .iter()
        .enumerate()
        .map(|(ordinal, binder)| {
            gast::Name::ident(format!(
                "rumoca_loop_{}_{}_{}_{}",
                fold.function().index(),
                fold.ordinal(),
                binder.id,
                ordinal
            ))
        })
        .collect()
}

fn wrap_function_loops(
    domain: dae::DomainView<'_>,
    names: Vec<gast::Name>,
    mut body: Vec<gast::Spanned<gast::Statement>>,
    span: Span,
) -> Vec<gast::Spanned<gast::Statement>> {
    for (binder, name) in domain.structured().binders.iter().zip(names).rev() {
        body = vec![gast::Spanned::new(
            gast::Statement::for_loop(gast::ForLoop::new(
                Some(name),
                gast::Expression::Integer(binder.lower),
                (binder.step != 1).then_some(gast::Expression::Integer(binder.step)),
                gast::Expression::Integer(binder.upper),
                body,
            )),
            span,
        )];
    }
    body
}

struct TensorAssignment<'a, 'dae> {
    target: gast::Name,
    target_type: &'a dae::ValueType,
    expression: dae::ExprId<'dae>,
    record_field: Option<usize>,
    /// The storage the emitted element loop writes, when `target` names a
    /// whole function value and the right-hand side is that value's own
    /// assignment. `None` where the target is a materialization local, whose
    /// storage no right-hand side can name and which therefore cannot alias.
    aggregate: Option<UpdatedAggregate<'dae>>,
    span: Span,
}

struct LoweredTensorAssignment {
    before: Vec<gast::Spanned<gast::Statement>>,
    nested: Option<gast::Spanned<gast::Statement>>,
}

/// One elementwise leg of a tensor assignment: the per-axis iterator names,
/// their index expressions, the coerced element value, and the element scalar
/// type.
struct TensorElementValue {
    names: Vec<gast::Name>,
    indices: Vec<gast::Expression>,
    value: gast::Expression,
    scalar: gast::ScalarType,
}

fn lower_tensor_element_value<'a, 'dae>(
    assignment: &TensorAssignment<'_, 'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Result<TensorElementValue, GalecTargetError> {
    materialize_eager_aggregate_calls(assignment.expression, lowerer)?;
    let scalar = scalar_type(
        assignment.target_type.scalar_type(),
        assignment.target.lexeme(),
        assignment.span,
    )?;
    let names = assignment
        .target_type
        .dimensions()
        .iter()
        .enumerate()
        .map(|(axis, _)| {
            gast::Name::ident(format!(
                "rumoca_tensor_{}_{}_{}",
                lowerer.temporary_namespace, lowerer.temporary_counter, axis
            ))
        })
        .collect::<Vec<_>>();
    lowerer.temporary_counter += 1;
    let bounds_depth = lowerer.loop_index_bounds.len();
    for (name, &extent) in names.iter().zip(assignment.target_type.dimensions()) {
        lowerer.loop_index_bounds.push(LoopIndexBound {
            name: name.clone(),
            minimum: 1,
            maximum: i64::from(extent),
        });
    }
    let indices = names
        .iter()
        .cloned()
        .map(|name| gast::Expression::Ref(gast::Reference::local(name)))
        .collect::<Vec<_>>();
    let value = match assignment.record_field {
        Some(field) => lowerer.lower_record_field_at(
            assignment.expression,
            field,
            &indices,
            scalar,
            assignment.span,
        ),
        None => lowerer.lower_at(assignment.expression, &indices),
    };
    lowerer.loop_index_bounds.truncate(bounds_depth);
    let value = coerce(value?, scalar, assignment.span)?;
    Ok(TensorElementValue {
        names,
        indices,
        value,
        scalar,
    })
}

/// Materialize aggregate calls before scalar projection introduces a tensor
/// loop or index-selection branches.
///
/// The DAE call owner proves one source invocation. Eager traversal stops at
/// lazy control-flow owners, so moving these calls to the assignment prefix
/// preserves branch execution while ensuring every projection reads the same
/// materialized result.
pub(super) fn materialize_eager_aggregate_calls<'a, 'dae>(
    expression: dae::ExprId<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Result<(), GalecTargetError> {
    let mut seen_expressions = HashSet::new();
    let mut seen_owners = HashSet::new();
    let mut calls = Vec::new();
    expression_functions::for_each_eager_call(
        lowerer.view,
        expression,
        &mut seen_expressions,
        &|_| None,
        &mut |call, owner| {
            let node = lowerer
                .view
                .expression(call)
                .expect("checked eager call resolves");
            let dae::ExpressionOperation::Call { function, .. } = node.operation() else {
                unreachable!("eager call traversal reports only calls")
            };
            let aggregate =
                node.value_type().is_record() || !node.value_type().dimensions().is_empty();
            if aggregate
                && is_directly_lowerable(lowerer.view, function)
                && seen_owners.insert(owner)
            {
                calls.push(call);
            }
        },
    );
    for call in calls {
        lowerer.materialize_eager_call(call)?;
    }
    Ok(())
}

/// The assignment whose right-hand side is already one whole aggregate, moved
/// without an element loop.
///
/// A whole-array assignment reads its source in full before it stores, so a
/// source that is the target itself is a self-copy and needs no snapshot.
fn lower_direct_aggregate_move<'a, 'dae>(
    assignment: &TensorAssignment<'_, 'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Result<Option<LoweredTensorAssignment>, GalecTargetError> {
    let direct = match assignment.record_field {
        Some(field) => lowerer.direct_aggregate_record_field(assignment.expression, field)?,
        None => lowerer.direct_whole_aggregate_reference(assignment.expression)?,
    };
    Ok(direct.map(|value| LoweredTensorAssignment {
        before: lowerer.drain_prefix_statements(),
        nested: Some(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(assignment.target.clone()),
                value,
            },
            assignment.span,
        )),
    }))
}

/// Which axes of the target this assignment stores at a single coordinate.
///
/// This is the one fact the emitted statements no longer carry: projection
/// replaces the source subscripts with coordinate arithmetic over the loop
/// iterators, so it is read off the checked DAE. The reads it exempts are
/// counted after projection instead, where the ones hoisted out of the loop
/// have already left.
///
/// A target without a named aggregate gets no exemption and is proven through
/// the identity rule alone: the region walk is stated over one whole function
/// value, so a record field's own region is not among the things it can name,
/// and a materialization local is not storage any right-hand side can name at
/// all.
fn stored_single_coordinate_axes<'dae>(
    assignment: &TensorAssignment<'_, 'dae>,
    view: dae::DaeView<'dae>,
) -> Vec<bool> {
    assignment.aggregate.as_ref().map_or_else(
        || vec![false; assignment.target_type.dimensions().len()],
        |aggregate| update_aliasing::single_coordinate_axes(view, aggregate, assignment.expression),
    )
}

fn lower_tensor_function_assignment<'a, 'dae>(
    assignment: TensorAssignment<'_, 'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Result<Option<LoweredTensorAssignment>, GalecTargetError> {
    if assignment.target_type.dimensions().is_empty() {
        return Ok(None);
    }
    if let Some(moved) = lower_direct_aggregate_move(&assignment, lowerer)? {
        return Ok(Some(moved));
    }
    let single_coordinate = stored_single_coordinate_axes(&assignment, lowerer.view);
    let TensorElementValue {
        names,
        indices,
        mut value,
        scalar,
    } = lower_tensor_element_value(&assignment, lowerer)?;
    let (mut before, mut body) =
        partition_tensor_prefixes(lowerer.drain_prefix_statements(), &names);
    let prologue = update_aliasing::snapshot_prologue(
        lowerer,
        &update_aliasing::SnapshotRewrite {
            target: &assignment.target,
            indices: &indices,
            single_coordinate: &single_coordinate,
        },
        update_aliasing::SnapshotShape {
            extents: assignment.target_type.dimensions(),
            scalar,
            span: assignment.span,
        },
        (&mut value, &mut body),
    );
    if body.is_empty()
        && let Some(source) = whole_array_move::provable_whole_array_move(
            lowerer,
            &value,
            &indices,
            assignment.target_type.dimensions(),
            scalar,
        )
    {
        return Ok(Some(update_aliasing::after_snapshot(
            prologue,
            LoweredTensorAssignment {
                before,
                nested: Some(gast::Spanned::new(
                    gast::Statement::Assignment {
                        target: gast::Reference::local(assignment.target),
                        value: gast::Expression::Ref(source),
                    },
                    assignment.span,
                )),
            },
        )));
    }
    body.push(gast::Spanned::new(
        gast::Statement::Assignment {
            target: gast::Reference::Local(gast::RefPart {
                name: assignment.target,
                subscripts: indices,
                span: assignment.span,
            }),
            value,
        },
        assignment.span,
    ));
    if let Some(fused) = fuse_guarded_tensor_loop(
        before.clone(),
        body.clone(),
        &names,
        assignment.target_type.dimensions(),
        assignment.span,
    ) {
        return Ok(Some(update_aliasing::after_snapshot(
            prologue,
            LoweredTensorAssignment {
                before: fused,
                nested: None,
            },
        )));
    }
    let span = assignment.span;
    let mut nest = nest_tensor_loops(
        body,
        &names,
        &expression_projection::AxisBounds {
            extents: assignment.target_type.dimensions(),
            proven: &|index, extent| lowerer.prove_dynamic_index(index, extent, span).is_ok(),
        },
        span,
    );
    let outer = nest.pop().expect("tensor assignment has one outer loop");
    before.extend(nest);
    Ok(Some(update_aliasing::after_snapshot(
        prologue,
        LoweredTensorAssignment {
            before,
            nested: Some(outer),
        },
    )))
}

pub(super) fn fuse_guarded_tensor_loop(
    before: Vec<gast::Spanned<gast::Statement>>,
    body: Vec<gast::Spanned<gast::Statement>>,
    iterators: &[gast::Name],
    extents: &[u32],
    span: Span,
) -> Option<Vec<gast::Spanned<gast::Statement>>> {
    let [before] = before.as_slice() else {
        return None;
    };
    let (body_guard, tail) = body.split_first()?;
    let before = flatten_total_guard(before)?;
    let body = flatten_total_guard(body_guard)?;
    if before.branches.len() != body.branches.len()
        || before
            .branches
            .iter()
            .zip(&body.branches)
            .any(|(lhs, rhs)| lhs.condition != rhs.condition)
    {
        return None;
    }
    let branches = before
        .branches
        .into_iter()
        .zip(body.branches)
        .map(|(before, body)| gast::IfBranch {
            condition: before.condition,
            body: guarded_tensor_branch(before.body, body.body, tail, iterators, extents, span),
            span: before.span,
        })
        .collect();
    let else_body = guarded_tensor_branch(
        before.fallback,
        body.fallback,
        tail,
        iterators,
        extents,
        span,
    );
    Some(vec![gast::Spanned::new(
        gast::Statement::If(gast::IfStatement {
            branches,
            else_body: Some(else_body),
        }),
        span,
    )])
}

struct TotalGuard {
    branches: Vec<gast::IfBranch>,
    fallback: Vec<gast::Spanned<gast::Statement>>,
}

fn flatten_total_guard(statement: &gast::Spanned<gast::Statement>) -> Option<TotalGuard> {
    let gast::Statement::If(value) = &statement.node else {
        return None;
    };
    let mut branches = value.branches.clone();
    let fallback = value.else_body.clone()?;
    if let Some((nested, suffix)) = fallback.split_first()
        && matches!(nested.node, gast::Statement::If(_))
    {
        let mut nested = flatten_total_guard(nested)?;
        for branch in &mut nested.branches {
            branch.body.extend_from_slice(suffix);
        }
        nested.fallback.extend_from_slice(suffix);
        branches.extend(nested.branches);
        return Some(TotalGuard {
            branches,
            fallback: nested.fallback,
        });
    }
    Some(TotalGuard { branches, fallback })
}

/// Merge `statement` into the newest earlier guard it can legally join.
///
/// A merge is legal only when the guards are equivalent and `statement` may
/// cross every statement between them, so the move cannot change order of
/// effects. Returns whether `statement` was consumed.
fn merge_into_equivalent_guard(
    merged: &mut [gast::Spanned<gast::Statement>],
    statement: &gast::Spanned<gast::Statement>,
) -> bool {
    for destination in (0..merged.len()).rev() {
        if !equivalent_total_guards(&merged[destination], statement)
            || !merged[destination + 1..]
                .iter()
                .all(|middle| guard_crosses(statement, middle))
        {
            continue;
        }
        merge_total_guard(&mut merged[destination], statement);
        return true;
    }
    false
}

/// Whether the reorderable guard `guard` may move to a position before
/// `middle`.
///
/// Two statements that commute may be exchanged, which is the general answer
/// and the one [`statements_commute`] gives. It requires both statements to be
/// reorderable, and a call is not: a call cannot be moved, because moving it
/// past another call would reorder two callees' error signals.
///
/// But "may this call move" is not the question a merge asks. The question is
/// whether a *guard* may move across the call, and for that the call's own
/// mobility is irrelevant. What matters is what the two can observe of each
/// other. A reorderable guard reads and writes named locals only: it carries no
/// call, no `limit`, no `signal`, and no signal-check condition, so the block's
/// error signal status is neither read nor written by it, provided it raises
/// no signal of its own, which is [`raises_signal`]'s clause. Such a guard is
/// invisible to a callee, so crossing one call leaves only the value flow, and
/// the value flow is what the dependence test below decides.
///
/// The crossing is admitted for a multi-assignment and nothing else. That is
/// the one call form whose written and read names are both complete in the
/// statement: its targets are the names it writes and its arguments are the
/// names it reads. `Call`, `limit` and `signal` state their effects elsewhere,
/// so a dependence test over them would be reading a name set that is not the
/// whole story, and they keep the conservative answer.
///
/// # Why this matters
///
/// One checked DAE conditional reaches GALEC once per value read out of it, so
/// a conditional whose arms define two values that are consumed at two points
/// is projected as two guards repeating one test. When a call stands between
/// them and an arm of the first guard materialized a value the matching arm of
/// the second reads, the two are still total and the read is still guarded by
/// the condition that produced it, but no single emitted statement says so,
/// and a C compiler reading the result reports a local that may be used
/// uninitialized. Under the assurance preflight, which is `-Werror`, that is a
/// build failure. Merging the two guards puts the definition and the use in one
/// arm of one conditional, where definite assignment is visible.
fn guard_crosses(
    guard: &gast::Spanned<gast::Statement>,
    middle: &gast::Spanned<gast::Statement>,
) -> bool {
    if statements_commute(guard, middle) {
        return true;
    }
    let gast::Statement::MultiAssignment { targets, .. } = &middle.node else {
        return false;
    };
    // Stated here rather than inherited from the caller: the argument above is
    // about what a *reorderable* guard can observe, and a function that assumed
    // the premise instead of checking it would answer the wrong question if it
    // ever gained a second caller.
    reorderable_guard(guard)
        && targets
            .iter()
            .all(|target| matches!(target, gast::Reference::Local(_)))
        && !raises_signal(guard)
        && independent(guard, middle)
}

/// Whether neither statement reads or writes a local the other writes.
fn independent(lhs: &gast::Spanned<gast::Statement>, rhs: &gast::Spanned<gast::Statement>) -> bool {
    let mut lhs_definitions = Vec::new();
    let mut rhs_definitions = Vec::new();
    collect_defined_names(lhs, &mut lhs_definitions);
    collect_defined_names(rhs, &mut rhs_definitions);
    !statement_depends_on(lhs, &rhs_definitions) && !statement_depends_on(rhs, &lhs_definitions)
}

/// Whether executing `statement` can raise an error signal.
///
/// Deliberately coarse in one direction: every comparison counts, though only a
/// comparison with a Real operand signals in the emitted C, because a GALEC
/// statement carries no scalar type and this projection would have to guess
/// one. A refusal here costs a guard merge; a wrong answer would move a signal
/// raise across a callee that can read the status, so the guess is refused.
///
/// The other signalling forms (`limit`, `signal`, and the `integer` builtin)
/// need no clause of their own: [`reorderable_statement`] already refuses the
/// first two outright and refuses any expression carrying a call, which is what
/// `integer` is. This function is only ever asked about a statement that test
/// has already accepted.
fn raises_signal(statement: &gast::Spanned<gast::Statement>) -> bool {
    match &statement.node {
        gast::Statement::Assignment { value, .. } => expression_compares(value),
        gast::Statement::If(value) => {
            value.branches.iter().any(|branch| {
                matches!(&branch.condition, gast::Condition::Expression(condition)
                    if expression_compares(condition))
                    || branch.body.iter().any(raises_signal)
            }) || value
                .else_body
                .as_ref()
                .is_some_and(|body| body.iter().any(raises_signal))
        }
        gast::Statement::For(value) => {
            expression_compares(&value.start)
                || value.step.as_ref().is_some_and(expression_compares)
                || expression_compares(&value.stop)
                || value.body.iter().any(raises_signal)
        }
        gast::Statement::MultiAssignment { .. }
        | gast::Statement::Call(_)
        | gast::Statement::Limit(_)
        | gast::Statement::Signal(_) => true,
    }
}

/// Whether the expression applies a comparison operator anywhere.
fn expression_compares(expression: &gast::Expression) -> bool {
    match expression {
        gast::Expression::Bool(_) | gast::Expression::Integer(_) | gast::Expression::Real(_) => {
            false
        }
        gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
            reference_compares(reference)
        }
        gast::Expression::Size { array, dimension } => {
            reference_compares(array) || expression_compares(dimension)
        }
        gast::Expression::Call(call) => call.arguments.iter().any(expression_compares),
        gast::Expression::Paren(value) | gast::Expression::Not(value) => expression_compares(value),
        gast::Expression::If(value) => {
            value.branches.iter().any(|(condition, branch)| {
                expression_compares(condition) || expression_compares(branch)
            }) || expression_compares(&value.else_value)
        }
        gast::Expression::Array(values) => values.iter().any(expression_compares),
        gast::Expression::Binary { op, lhs, rhs } => {
            matches!(
                op,
                gast::BinaryOp::Lt
                    | gast::BinaryOp::Gt
                    | gast::BinaryOp::Le
                    | gast::BinaryOp::Ge
                    | gast::BinaryOp::Eq
                    | gast::BinaryOp::Ne
            ) || expression_compares(lhs)
                || expression_compares(rhs)
        }
    }
}

fn reference_compares(reference: &gast::Reference) -> bool {
    let parts = match reference {
        gast::Reference::Local(part) => std::slice::from_ref(part),
        gast::Reference::State(parts) => parts,
    };
    parts
        .iter()
        .flat_map(|part| &part.subscripts)
        .any(expression_compares)
}

fn guarded_tensor_branch(
    mut before: Vec<gast::Spanned<gast::Statement>>,
    mut body: Vec<gast::Spanned<gast::Statement>>,
    tail: &[gast::Spanned<gast::Statement>],
    iterators: &[gast::Name],
    extents: &[u32],
    span: Span,
) -> Vec<gast::Spanned<gast::Statement>> {
    body.extend_from_slice(tail);
    for (iterator, &extent) in iterators.iter().zip(extents).rev() {
        body = vec![gast::Spanned::new(
            gast::Statement::for_loop(gast::ForLoop::new(
                Some(iterator.clone()),
                gast::Expression::Integer(1),
                None,
                gast::Expression::Integer(i64::from(extent)),
                body,
            )),
            span,
        )];
    }
    before.extend(body);
    before
}

pub(super) fn merge_guarded_tensor_loops(
    statements: Vec<gast::Spanned<gast::Statement>>,
) -> Vec<gast::Spanned<gast::Statement>> {
    let mut merged: Vec<gast::Spanned<gast::Statement>> = Vec::new();
    for statement in statements {
        let mut consumed = false;
        for candidate in merged.iter_mut().rev() {
            if matches!(candidate.node, gast::Statement::If(_))
                && merge_guarded_tensor_loop(candidate, &statement)
            {
                consumed = true;
                break;
            }
        }
        if !consumed {
            merged.push(statement);
        }
    }
    merged
}

/// Fuse repeated projections of one checked conditional activation.
///
/// Nested function conditionals are value DAGs inside their enclosing atomic
/// assignment group. Different target projections can therefore arrive here
/// as separate, structurally identical guards even though a branch-local call
/// produced values consumed by all of them. A later guard may move beside the
/// first only across statements that provably commute with it; calls, signals,
/// limits, and expression calls are barriers. The resulting branch owns both
/// the lazy producer and its consumers, so generated C has lexical
/// definite-assignment evidence as well as the DAE activation proof.
pub(super) fn coalesce_correlated_guards(
    statements: Vec<gast::Spanned<gast::Statement>>,
) -> Vec<gast::Spanned<gast::Statement>> {
    let statements = statements
        .into_iter()
        .map(coalesce_nested_correlated_guards)
        .collect::<Vec<_>>();
    let mut merged: Vec<gast::Spanned<gast::Statement>> = Vec::new();
    for statement in statements {
        let consumed =
            reorderable_guard(&statement) && merge_into_equivalent_guard(&mut merged, &statement);
        if !consumed {
            merged.push(statement);
        }
    }
    merged
}

fn coalesce_nested_correlated_guards(
    mut statement: gast::Spanned<gast::Statement>,
) -> gast::Spanned<gast::Statement> {
    match &mut statement.node {
        gast::Statement::If(value) => {
            for branch in &mut value.branches {
                branch.body = coalesce_correlated_guards(std::mem::take(&mut branch.body));
            }
            if let Some(body) = &mut value.else_body {
                *body = coalesce_correlated_guards(std::mem::take(body));
            }
        }
        gast::Statement::For(value) => {
            value.body = coalesce_correlated_guards(std::mem::take(&mut value.body));
        }
        gast::Statement::Assignment { .. }
        | gast::Statement::MultiAssignment { .. }
        | gast::Statement::Call(_)
        | gast::Statement::Limit(_)
        | gast::Statement::Signal(_) => {}
    }
    statement
}

fn equivalent_total_guards(
    lhs: &gast::Spanned<gast::Statement>,
    rhs: &gast::Spanned<gast::Statement>,
) -> bool {
    let (Some(lhs), Some(rhs)) = (flatten_total_guard(lhs), flatten_total_guard(rhs)) else {
        return false;
    };
    lhs.branches.len() == rhs.branches.len()
        && lhs
            .branches
            .iter()
            .zip(rhs.branches)
            .all(|(lhs, rhs)| lhs.condition == rhs.condition)
}

fn merge_total_guard(
    destination: &mut gast::Spanned<gast::Statement>,
    source: &gast::Spanned<gast::Statement>,
) {
    let mut destination_guard =
        flatten_total_guard(destination).expect("equivalent destination is a total guard");
    let source_guard = flatten_total_guard(source).expect("equivalent source is a total guard");
    for (destination, source) in destination_guard
        .branches
        .iter_mut()
        .zip(source_guard.branches)
    {
        destination.body.extend(source.body);
    }
    destination_guard.fallback.extend(source_guard.fallback);
    destination.node = gast::Statement::If(gast::IfStatement {
        branches: destination_guard.branches,
        else_body: Some(destination_guard.fallback),
    });
}

fn statements_commute(
    lhs: &gast::Spanned<gast::Statement>,
    rhs: &gast::Spanned<gast::Statement>,
) -> bool {
    if !reorderable_statement(lhs) || !reorderable_statement(rhs) {
        return false;
    }
    independent(lhs, rhs)
}

fn reorderable_guard(statement: &gast::Spanned<gast::Statement>) -> bool {
    matches!(&statement.node, gast::Statement::If(_)) && reorderable_statement(statement)
}

fn reorderable_statement(statement: &gast::Spanned<gast::Statement>) -> bool {
    match &statement.node {
        gast::Statement::Assignment { target, value } => {
            matches!(target, gast::Reference::Local(_)) && !expression_has_call(value)
        }
        gast::Statement::If(value) => {
            value.branches.iter().all(|branch| {
                matches!(branch.condition, gast::Condition::Expression(ref condition)
                    if !expression_has_call(condition))
                    && branch.body.iter().all(reorderable_statement)
            }) && value
                .else_body
                .as_ref()
                .is_some_and(|body| body.iter().all(reorderable_statement))
        }
        gast::Statement::For(value) => {
            !expression_has_call(&value.start)
                && value
                    .step
                    .as_ref()
                    .is_none_or(|step| !expression_has_call(step))
                && !expression_has_call(&value.stop)
                && value.body.iter().all(reorderable_statement)
        }
        gast::Statement::MultiAssignment { .. }
        | gast::Statement::Call(_)
        | gast::Statement::Limit(_)
        | gast::Statement::Signal(_) => false,
    }
}

fn expression_has_call(expression: &gast::Expression) -> bool {
    match expression {
        gast::Expression::Call(_) => true,
        gast::Expression::Bool(_) | gast::Expression::Integer(_) | gast::Expression::Real(_) => {
            false
        }
        gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
            reference_has_call(reference)
        }
        gast::Expression::Size { array, dimension } => {
            reference_has_call(array) || expression_has_call(dimension)
        }
        gast::Expression::Paren(value) | gast::Expression::Not(value) => expression_has_call(value),
        gast::Expression::If(value) => {
            value.branches.iter().any(|(condition, branch)| {
                expression_has_call(condition) || expression_has_call(branch)
            }) || expression_has_call(&value.else_value)
        }
        gast::Expression::Array(values) => values.iter().any(expression_has_call),
        gast::Expression::Binary { lhs, rhs, .. } => {
            expression_has_call(lhs) || expression_has_call(rhs)
        }
    }
}

fn reference_has_call(reference: &gast::Reference) -> bool {
    let parts = match reference {
        gast::Reference::Local(part) => std::slice::from_ref(part),
        gast::Reference::State(parts) => parts,
    };
    parts
        .iter()
        .flat_map(|part| &part.subscripts)
        .any(expression_has_call)
}

#[derive(Clone)]
struct TensorLoopShell {
    loop_statement: gast::ForLoop,
    span: Span,
}

fn merge_guarded_tensor_loop(
    destination: &mut gast::Spanned<gast::Statement>,
    nested: &gast::Spanned<gast::Statement>,
) -> bool {
    let Some((shells, loop_body)) = tensor_loop_shells(nested) else {
        return false;
    };
    let Some((body_guard, tail)) = loop_body.split_first() else {
        return false;
    };
    let Some(mut destination_guard) = flatten_total_guard(destination) else {
        return false;
    };
    let Some(body_guard) = flatten_total_guard(body_guard) else {
        return false;
    };
    if destination_guard.branches.len() != body_guard.branches.len()
        || destination_guard
            .branches
            .iter()
            .zip(&body_guard.branches)
            .any(|(lhs, rhs)| lhs.condition != rhs.condition)
    {
        return false;
    }
    for (destination, source) in destination_guard
        .branches
        .iter_mut()
        .zip(body_guard.branches)
    {
        let mut body = source.body;
        body.extend_from_slice(tail);
        destination.body.push(wrap_tensor_loop(&shells, body));
    }
    let mut fallback = body_guard.fallback;
    fallback.extend_from_slice(tail);
    destination_guard
        .fallback
        .push(wrap_tensor_loop(&shells, fallback));
    destination.node = gast::Statement::If(gast::IfStatement {
        branches: destination_guard.branches,
        else_body: Some(destination_guard.fallback),
    });
    true
}

fn tensor_loop_shells(
    statement: &gast::Spanned<gast::Statement>,
) -> Option<(Vec<TensorLoopShell>, Vec<gast::Spanned<gast::Statement>>)> {
    let mut shells = Vec::new();
    let mut current = statement;
    loop {
        let gast::Statement::For(loop_statement) = &current.node else {
            return None;
        };
        shells.push(TensorLoopShell {
            loop_statement: (**loop_statement).clone(),
            span: current.span,
        });
        let [nested] = loop_statement.body.as_slice() else {
            return Some((shells, loop_statement.body.clone()));
        };
        if !matches!(nested.node, gast::Statement::For(_)) {
            return Some((shells, loop_statement.body.clone()));
        }
        current = nested;
    }
}

fn wrap_tensor_loop(
    shells: &[TensorLoopShell],
    mut body: Vec<gast::Spanned<gast::Statement>>,
) -> gast::Spanned<gast::Statement> {
    for shell in shells.iter().rev() {
        let mut loop_statement = shell.loop_statement.clone();
        loop_statement.body = body;
        body = vec![gast::Spanned::new(
            gast::Statement::for_loop(loop_statement),
            shell.span,
        )];
    }
    body.pop().expect("tensor loop owns one outer shell")
}

pub(super) fn partition_tensor_prefixes(
    statements: Vec<gast::Spanned<gast::Statement>>,
    outer_indices: &[gast::Name],
) -> (
    Vec<gast::Spanned<gast::Statement>>,
    Vec<gast::Spanned<gast::Statement>>,
) {
    let statements = split_loop_invariant_guards(statements, outer_indices);
    partition_by_dependence(statements, outer_indices)
}

/// Split `statements` into those whose values cannot change with
/// `outer_indices` and those that can, leaving every statement intact.
///
/// A statement joins the dependent side as soon as it reads or writes a
/// dependent name, and its own targets become dependent in turn, so an
/// accumulator reset whose value is a literal still travels with the loop that
/// updates the accumulator.
pub(super) fn partition_by_dependence(
    statements: Vec<gast::Spanned<gast::Statement>>,
    outer_indices: &[gast::Name],
) -> (
    Vec<gast::Spanned<gast::Statement>>,
    Vec<gast::Spanned<gast::Statement>>,
) {
    let mut dependent_names = outer_indices.to_vec();
    let mut dependent = vec![false; statements.len()];
    loop {
        let mut changed = false;
        for (index, statement) in statements.iter().enumerate() {
            if dependent[index] || !statement_depends_on(statement, &dependent_names) {
                continue;
            }
            dependent[index] = true;
            collect_defined_names(statement, &mut dependent_names);
            changed = true;
        }
        if !changed {
            break;
        }
    }
    let mut before = Vec::new();
    let mut body = Vec::new();
    for (statement, is_dependent) in statements.into_iter().zip(dependent) {
        if is_dependent {
            body.push(statement);
        } else {
            before.push(statement);
        }
    }
    (before, body)
}

fn split_loop_invariant_guards(
    statements: Vec<gast::Spanned<gast::Statement>>,
    outer_indices: &[gast::Name],
) -> Vec<gast::Spanned<gast::Statement>> {
    let mut split = Vec::new();
    for statement in statements {
        let Some(partition) = split_loop_invariant_guard(&statement, outer_indices) else {
            split.push(statement);
            continue;
        };
        split.extend(partition.before);
        split.extend(partition.body);
    }
    split
}

struct GuardPartition {
    before: Option<gast::Spanned<gast::Statement>>,
    body: Option<gast::Spanned<gast::Statement>>,
}

fn split_loop_invariant_guard(
    statement: &gast::Spanned<gast::Statement>,
    outer_indices: &[gast::Name],
) -> Option<GuardPartition> {
    let gast::Statement::If(value) = &statement.node else {
        return None;
    };
    if value
        .branches
        .iter()
        .any(|branch| !repeatable_loop_invariant_condition(&branch.condition, outer_indices))
    {
        return None;
    }
    let mut defined = Vec::new();
    for branch in &value.branches {
        for nested in &branch.body {
            collect_defined_names(nested, &mut defined);
        }
    }
    if let Some(else_body) = &value.else_body {
        for nested in else_body {
            collect_defined_names(nested, &mut defined);
        }
    }
    if value
        .branches
        .iter()
        .any(|branch| condition_depends_on(&branch.condition, &defined))
    {
        return None;
    }

    let mut has_before = false;
    let mut has_body = false;
    let mut before_branches = Vec::with_capacity(value.branches.len());
    let mut body_branches = Vec::with_capacity(value.branches.len());
    for branch in &value.branches {
        let (before, body) = partition_tensor_prefixes(branch.body.clone(), outer_indices);
        has_before |= !before.is_empty();
        has_body |= !body.is_empty();
        before_branches.push(gast::IfBranch {
            condition: branch.condition.clone(),
            body: before,
            span: branch.span,
        });
        body_branches.push(gast::IfBranch {
            condition: branch.condition.clone(),
            body,
            span: branch.span,
        });
    }
    let (before_else, body_else) = value
        .else_body
        .as_ref()
        .map(|else_body| partition_tensor_prefixes(else_body.clone(), outer_indices))
        .map_or((None, None), |(before, body)| {
            has_before |= !before.is_empty();
            has_body |= !body.is_empty();
            (Some(before), Some(body))
        });
    if !has_before || !has_body {
        return None;
    }
    if splits_a_target(
        &before_branches,
        before_else.as_deref(),
        &body_branches,
        body_else.as_deref(),
    ) {
        return None;
    }
    let before = gast::Spanned::new(
        gast::Statement::If(gast::IfStatement {
            branches: before_branches,
            else_body: before_else,
        }),
        statement.span,
    );
    let body = gast::Spanned::new(
        gast::Statement::If(gast::IfStatement {
            branches: body_branches,
            else_body: body_else,
        }),
        statement.span,
    );
    Some(GuardPartition {
        before: Some(before),
        body: Some(body),
    })
}

/// Whether splitting this guard would distribute one variable's assignments
/// over the two halves.
///
/// The split copies the guard's conditions into two conditionals and sends each
/// branch's loop-invariant statements to the first and the rest to the second.
/// When one branch assigns a target with a loop-invariant value and another
/// assigns the same target with a loop-dependent one, the two assignments land
/// in different conditionals: the target is then still assigned on every path,
/// but no single conditional shows it, because the proof now needs the two
/// guards to agree and nothing in the emitted C says they must.
///
/// That is a real cost even though the values are unchanged. A C compiler
/// reading the result sees a variable that some path may leave unset -- the
/// generated code sits between the two guards, so it cannot assume the second
/// test repeats the first -- and reports it. Under the assurance preflight,
/// which is `-Werror`, the report is a build failure rather than a note, so a
/// hoist that saves nothing here costs the whole translation unit.
///
/// Refusing the split keeps the original conditional, where every branch
/// assigns the target in the branch that produces it and definite assignment is
/// visible in one statement. Nothing else about the guard changes, and a guard
/// whose halves assign disjoint targets still splits.
fn splits_a_target(
    before_branches: &[gast::IfBranch],
    before_else: Option<&[gast::Spanned<gast::Statement>]>,
    body_branches: &[gast::IfBranch],
    body_else: Option<&[gast::Spanned<gast::Statement>]>,
) -> bool {
    let before = assigned_targets(before_branches, before_else);
    let body = assigned_targets(body_branches, body_else);
    before.iter().any(|name| body.contains(name))
}

/// Every local a half of a split guard assigns, over all of its branches.
fn assigned_targets(
    branches: &[gast::IfBranch],
    else_body: Option<&[gast::Spanned<gast::Statement>]>,
) -> Vec<gast::Name> {
    let mut names = Vec::new();
    for statement in branches
        .iter()
        .flat_map(|branch| &branch.body)
        .chain(else_body.into_iter().flatten())
    {
        collect_defined_names(statement, &mut names);
    }
    names
}

fn repeatable_loop_invariant_condition(
    condition: &gast::Condition,
    outer_indices: &[gast::Name],
) -> bool {
    match condition {
        gast::Condition::Expression(expression) => {
            !expression_depends_on(expression, outer_indices) && !expression_has_call(expression)
        }
        gast::Condition::SignalCheck(_) => false,
    }
}

pub(super) fn collect_defined_names(
    statement: &gast::Spanned<gast::Statement>,
    names: &mut Vec<gast::Name>,
) {
    match &statement.node {
        gast::Statement::Assignment { target, .. } => collect_defined_reference(target, names),
        gast::Statement::MultiAssignment { targets, .. } => {
            for target in targets {
                collect_defined_reference(target, names);
            }
        }
        gast::Statement::If(value) => {
            for branch in &value.branches {
                for statement in &branch.body {
                    collect_defined_names(statement, names);
                }
            }
            if let Some(body) = &value.else_body {
                for statement in body {
                    collect_defined_names(statement, names);
                }
            }
        }
        gast::Statement::For(value) => {
            for statement in &value.body {
                collect_defined_names(statement, names);
            }
        }
        gast::Statement::Call(_) | gast::Statement::Limit(_) | gast::Statement::Signal(_) => {}
    }
}

fn collect_defined_reference(reference: &gast::Reference, names: &mut Vec<gast::Name>) {
    let gast::Reference::Local(part) = reference else {
        return;
    };
    if !names.contains(&part.name) {
        names.push(part.name.clone());
    }
}

pub(super) fn statement_depends_on(
    statement: &gast::Spanned<gast::Statement>,
    names: &[gast::Name],
) -> bool {
    match &statement.node {
        gast::Statement::Assignment { target, value } => {
            reference_depends_on(target, names) || expression_depends_on(value, names)
        }
        gast::Statement::MultiAssignment { targets, call } => {
            targets
                .iter()
                .any(|target| reference_depends_on(target, names))
                || call_depends_on(call, names)
        }
        gast::Statement::Call(call) => call_depends_on(call, names),
        gast::Statement::If(value) => {
            value.branches.iter().any(|branch| {
                condition_depends_on(&branch.condition, names)
                    || branch
                        .body
                        .iter()
                        .any(|statement| statement_depends_on(statement, names))
            }) || value.else_body.as_ref().is_some_and(|body| {
                body.iter()
                    .any(|statement| statement_depends_on(statement, names))
            })
        }
        gast::Statement::For(value) => {
            expression_depends_on(&value.start, names)
                || value
                    .step
                    .as_ref()
                    .is_some_and(|step| expression_depends_on(step, names))
                || expression_depends_on(&value.stop, names)
                || value
                    .body
                    .iter()
                    .any(|statement| statement_depends_on(statement, names))
        }
        gast::Statement::Limit(targets) => targets.iter().any(|target| match target {
            gast::LimitTarget::SelfState => false,
            gast::LimitTarget::Reference(reference) => reference_depends_on(reference, names),
        }),
        gast::Statement::Signal(_) => false,
    }
}

fn condition_depends_on(condition: &gast::Condition, names: &[gast::Name]) -> bool {
    match condition {
        gast::Condition::Expression(expression) => expression_depends_on(expression, names),
        gast::Condition::SignalCheck(check) => check
            .fallback
            .as_ref()
            .is_some_and(|fallback| expression_depends_on(fallback, names)),
    }
}

fn call_depends_on(call: &gast::FunctionCall, names: &[gast::Name]) -> bool {
    call.arguments
        .iter()
        .any(|argument| expression_depends_on(argument, names))
}

pub(super) fn expression_depends_on(expression: &gast::Expression, names: &[gast::Name]) -> bool {
    match expression {
        gast::Expression::Bool(_) | gast::Expression::Integer(_) | gast::Expression::Real(_) => {
            false
        }
        gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
            reference_depends_on(reference, names)
        }
        gast::Expression::Size { array, dimension } => {
            reference_depends_on(array, names) || expression_depends_on(dimension, names)
        }
        gast::Expression::Call(call) => call_depends_on(call, names),
        gast::Expression::Paren(value) | gast::Expression::Not(value) => {
            expression_depends_on(value, names)
        }
        gast::Expression::If(value) => {
            value.branches.iter().any(|(condition, branch)| {
                expression_depends_on(condition, names) || expression_depends_on(branch, names)
            }) || expression_depends_on(&value.else_value, names)
        }
        gast::Expression::Array(values) => values
            .iter()
            .any(|value| expression_depends_on(value, names)),
        gast::Expression::Binary { lhs, rhs, .. } => {
            expression_depends_on(lhs, names) || expression_depends_on(rhs, names)
        }
    }
}

fn reference_depends_on(reference: &gast::Reference, names: &[gast::Name]) -> bool {
    let parts = match reference {
        gast::Reference::Local(part) => std::slice::from_ref(part),
        gast::Reference::State(parts) => parts,
    };
    parts.iter().any(|part| {
        names.contains(&part.name)
            || part
                .subscripts
                .iter()
                .any(|subscript| expression_depends_on(subscript, names))
    })
}
