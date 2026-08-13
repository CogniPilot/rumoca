//! Array-native projection of reachable Modelica functions into GALEC.
// SPEC_0021 file-size exception: this module still owns both function-body
// control-flow projection and value materialization. split plan: move checked
// correlation materialization into its own lower::user_functions submodule.

use super::*;

mod indexed_update;

use indexed_update::{lower_indexed_function_update, preserves_function_target};

pub(super) fn lower_reachable<'dae>(
    view: dae::DaeView<'dae>,
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
        let (function, calls) = lower_function(view, id)?;
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
    id: dae::FunctionId<'dae>,
) -> Result<(gast::UserFunction, HashSet<u32>), GalecTargetError> {
    if !is_directly_lowerable(view, id) {
        return Err(unsupported(
            "galec-user-function",
            "reachable Modelica function does not have primitive results and a loop-free body"
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
    let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
    lowerer.function_scope = Some(id);
    let parameters = function_parameters(view, function)?;
    let locals = function_locals(view, function)?;
    let mut statements = Vec::new();
    for statement in function.statements() {
        lower_function_statement(view, statement, &mut lowerer, &mut statements)?;
    }
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
                for definition in definitions.iter() {
                    lower_function_assignment(view, definition, lowerer, statements)?;
                }
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
            lower_function_for(view, fold, body, provenance.span(), lowerer, statements)?;
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

fn lower_function_conditional_group<'a, 'dae>(
    view: dae::DaeView<'dae>,
    definitions: dae::FunctionDefinitionValues<'dae>,
    conditional: dae::FunctionConditionalView<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let definitions = definitions.iter().collect::<Vec<_>>();
    let conditions = conditional.conditions().collect::<Vec<_>>();
    let activation_operands = conditions
        .iter()
        .map(|condition| condition.index())
        .collect::<Vec<_>>();
    let entry_materialization = lowerer.conditional_materialization_snapshot();
    let mut branches = Vec::with_capacity(conditions.len());
    for (ordinal, condition_id) in conditions.into_iter().enumerate() {
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
            .expect("checked function conditional branch resolves");
        for (definition, value) in definitions.iter().copied().zip(values) {
            let target = view
                .function(definition.id().function())
                .expect("checked function identity resolves")
                .values()
                .find(|candidate| candidate.id() == definition.target())
                .expect("checked function conditional target resolves");
            lower_function_value_assignment(
                view,
                target,
                value,
                definition.provenance().span(),
                lowerer,
                &mut body,
            )?;
        }
        lowerer.conditional_activation_path.pop();
        lowerer.restore_conditional_materialization(&condition_materialization);
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
    let mut fallback = Vec::new();
    for (definition, value) in definitions.iter().copied().zip(conditional.fallback()) {
        let target = view
            .function(definition.id().function())
            .expect("checked function identity resolves")
            .values()
            .find(|candidate| candidate.id() == definition.target())
            .expect("checked function conditional target resolves");
        lower_function_value_assignment(
            view,
            target,
            value,
            definition.provenance().span(),
            lowerer,
            &mut fallback,
        )?;
    }
    lowerer.conditional_activation_path.pop();
    lowerer.restore_conditional_materialization(&entry_materialization);
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
        return lower_conditional_function_value_assignment(
            view, target, operands, span, lowerer, statements,
        );
    }
    if preserves_function_target(view, target, expression) {
        return Ok(());
    }
    if let Some(updates) =
        lower_indexed_function_update(view, target, target_type, expression, lowerer, span)?
    {
        statements.extend(lowerer.drain_prefix_statements());
        statements.extend(updates);
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
        )
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
    let mut branches = Vec::with_capacity(operands.len() / 2);
    for ordinal in (0..operands.len() - 1).step_by(2) {
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
            gast::Statement::For(gast::ForLoop {
                iterator: Some(name),
                start: gast::Expression::Integer(binder.lower),
                step: (binder.step != 1).then_some(gast::Expression::Integer(binder.step)),
                stop: gast::Expression::Integer(binder.upper),
                body,
            }),
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
    span: Span,
}

struct LoweredTensorAssignment {
    before: Vec<gast::Spanned<gast::Statement>>,
    nested: Option<gast::Spanned<gast::Statement>>,
}

fn lower_tensor_function_assignment<'a, 'dae>(
    assignment: TensorAssignment<'_, 'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Result<Option<LoweredTensorAssignment>, GalecTargetError> {
    if assignment.target_type.dimensions().is_empty() {
        return Ok(None);
    }
    if let Some(direct) = lower_direct_tensor_assignment(&assignment, lowerer)? {
        return Ok(Some(direct));
    }
    let scalar = scalar_type(
        assignment.target_type.scalar_type(),
        assignment.target.lexeme(),
        assignment.span,
    )?;
    let names = tensor_assignment_loop_names(&assignment, lowerer);
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
    let (before, mut body) = partition_tensor_prefixes(lowerer.drain_prefix_statements(), &names);
    if body.is_empty()
        && let Some(source) = provable_whole_array_move(
            lowerer,
            &value,
            &indices,
            assignment.target_type.dimensions(),
            scalar,
        )
    {
        return Ok(Some(LoweredTensorAssignment {
            before,
            nested: Some(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: gast::Reference::local(assignment.target),
                    value: gast::Expression::Ref(source),
                },
                assignment.span,
            )),
        }));
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
        return Ok(Some(LoweredTensorAssignment {
            before: fused,
            nested: None,
        }));
    }
    for (name, &extent) in names.iter().zip(assignment.target_type.dimensions()).rev() {
        body = vec![gast::Spanned::new(
            gast::Statement::For(gast::ForLoop {
                iterator: Some(name.clone()),
                start: gast::Expression::Integer(1),
                step: None,
                stop: gast::Expression::Integer(i64::from(extent)),
                body,
            }),
            assignment.span,
        )];
    }
    Ok(Some(LoweredTensorAssignment {
        before,
        nested: Some(body.pop().expect("tensor assignment has one outer loop")),
    }))
}

fn lower_direct_tensor_assignment<'a, 'dae>(
    assignment: &TensorAssignment<'_, 'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Result<Option<LoweredTensorAssignment>, GalecTargetError> {
    let direct = match assignment.record_field {
        Some(field) => lowerer.direct_aggregate_record_field(assignment.expression, field)?,
        None => lowerer.direct_aggregate_function_argument(assignment.expression)?,
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

fn tensor_assignment_loop_names<'a, 'dae>(
    assignment: &TensorAssignment<'_, 'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Vec<gast::Name> {
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
    names
}

/// Recover the whole-array source of an elementwise projection that turned out
/// to be the identity.
///
/// A tensor assignment lowers its right-hand side once, at the coordinate
/// `indices` names, and only then decides how to iterate. When that lowering
/// hands back exactly `source[i][j]` for the very index expressions it was
/// given, the assignment does not compute anything per element: it moves a
/// whole array. This is asked before any loop is constructed, so the loop is
/// never built rather than built and recognised again -- the projection's own
/// result is the evidence, not the shape of emitted statements.
///
/// The subscripts must be the projection indices themselves, in order. An
/// offset walk (`source[7 + i - 1]`), a strided walk or a transposed one all
/// fail that equality and keep their loop, which is where they belong.
///
/// Subscript identity is necessary but NOT sufficient: the caller must also
/// prove, via [`declared_shape`], that the source's declared extents equal
/// the target's and the element types match. Per-element identity at the
/// index names says nothing about the objects' shapes — a projection can hand
/// back `source[i][j]` for a `source` declared with different extents than
/// the target (the rank-position regressions produce exactly that: an inner
/// slice loop compacted over a transposed or re-ranked object), and a
/// whole-array copy between differently-shaped objects visits elements in an
/// order the scalar loop never wrote.
///
/// Earlier parts of a state path must be unsubscripted: dropping the last
/// part's subscripts has to leave a reference that still denotes the whole
/// array, and a subscripted prefix could itself depend on a projection index.
fn whole_array_projection_source(
    value: &gast::Expression,
    indices: &[gast::Expression],
) -> Option<gast::Reference> {
    if indices.is_empty() {
        return None;
    }
    let gast::Expression::Ref(reference) = value else {
        return None;
    };
    match reference {
        gast::Reference::Local(part) if part.subscripts == indices => {
            Some(gast::Reference::Local(gast::RefPart {
                name: part.name.clone(),
                subscripts: Vec::new(),
                span: part.span,
            }))
        }
        gast::Reference::State(parts) => {
            let (last, prefix) = parts.split_last()?;
            if last.subscripts != indices || prefix.iter().any(|part| !part.subscripts.is_empty()) {
                return None;
            }
            let mut parts = prefix.to_vec();
            parts.push(gast::RefPart {
                name: last.name.clone(),
                subscripts: Vec::new(),
                span: last.span,
            });
            Some(gast::Reference::State(parts))
        }
        gast::Reference::Local(_) => None,
    }
}

/// The whole decision, both conjuncts, one place: an elementwise projection
/// may be flattened into a whole-array move exactly when the projection is
/// the subscript identity ([`whole_array_projection_source`]) AND the
/// source's declared shape equals the target's, element type included
/// ([`declared_shape`]). Returns the subscript-stripped source on success.
///
/// The shape conjunct deserves its own defense, because on today's corpus it
/// is a barrier, not a repair: every front-end path that currently produces
/// an identity projection reads an object declared from the target's own
/// type, and the shapes that WOULD go wrong (`y := s[1:2]` from a longer
/// `s`, a comprehension over a leading sub-range) reach this point with
/// their range arithmetic unfolded (`s[1 + (i - 1)]`), so the subscript
/// check already rejects them. One projection-folding improvement — folding
/// `1 + (i - 1)` to `i`, which is a natural cleanup — turns each of those
/// into a subscript-identity projection whose flattening writes a
/// whole-array assignment between differently-shaped objects. The unit test
/// `whole_array_move_needs_shape_equality_not_just_subscript_identity`
/// constructs exactly that situation and fails if this function ever
/// collapses it.
pub(super) fn provable_whole_array_move<'a, 'dae>(
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    value: &gast::Expression,
    indices: &[gast::Expression],
    target_extents: &[u32],
    target_scalar: gast::ScalarType,
) -> Option<gast::Reference> {
    let source = whole_array_projection_source(value, indices)?;
    let (extents, scalar) = declared_shape(lowerer, &source)?;
    (extents.as_slice() == target_extents && scalar == target_scalar).then_some(source)
}

/// Declared extents and element type of an identity-projection source, when a
/// declaration for it is visible to this lowerer.
///
/// This is the shape half of the whole-array-move evidence (the subscript
/// half is [`whole_array_projection_source`]): only when the source's
/// declared extents equal the target's, element type included, does
/// per-element identity at the index names imply whole-array identity.
///
/// Resolution is by declaration site:
/// - `Local` names: the lowerer's materialized temporaries (newest first, so
///   a name resolves to the declaration actually in scope), then the function
///   scope's parameters, outputs and locals — the same flattened declarations
///   `function_parameters` / `function_locals` emit, under the same names.
/// - `State` paths: the classified block variables, single-part paths only.
///   A multi-part path's leaf shape is not resolved here.
///
/// `None` means "no visible declaration proves the shape", and the caller
/// keeps the loop — the fallback is the semantics-preserving direction, so an
/// unresolvable name can only cost a collapse, never correctness.
fn declared_shape<'a, 'dae>(
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    reference: &gast::Reference,
) -> Option<(Vec<u32>, gast::ScalarType)> {
    match reference {
        gast::Reference::Local(part) => {
            if let Some(declaration) = lowerer
                .temporary_locals
                .iter()
                .rev()
                .find(|declaration| declaration.name.lexeme() == part.name.lexeme())
            {
                return declared_variable_shape(declaration);
            }
            let function = lowerer.view.function(lowerer.function_scope?)?;
            function_scope_shape(lowerer.view, function, part.name.lexeme())
        }
        gast::Reference::State(parts) => {
            let [part] = parts.as_slice() else {
                return None;
            };
            lowerer.state_shape(part.name.lexeme())
        }
    }
}

/// The shape a [`gast::VariableDeclaration`] declares, when it is a primitive
/// with fully-literal extents. Every declaration this lowering emits is —
/// a non-literal or derived dimension yields `None`, and the caller keeps
/// its loop.
fn declared_variable_shape(
    declaration: &gast::VariableDeclaration,
) -> Option<(Vec<u32>, gast::ScalarType)> {
    let gast::TypeRef::Primitive(scalar) = declaration.ty else {
        return None;
    };
    let mut extents = Vec::with_capacity(declaration.dimensions.len());
    for dimension in &declaration.dimensions {
        let gast::Dimension::Expr(expression) = dimension else {
            return None;
        };
        extents.push(u32::try_from(constant_integer(expression)?).ok()?);
    }
    Some((extents, scalar))
}

/// The declared shape of `name` among a function scope's parameters, outputs
/// and locals, resolved under the same flattened names
/// `function_parameters` / `function_locals` declare them with (record
/// fields included).
fn function_scope_shape<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    name: &str,
) -> Option<(Vec<u32>, gast::ScalarType)> {
    for parameter in function.parameters() {
        let ty = view.value_type(parameter.value_type())?;
        if !ty.is_record() {
            if parameter_name(parameter).is_ok_and(|candidate| candidate.lexeme() == name) {
                return value_type_shape(ty, name, parameter.declaration().span());
            }
            continue;
        }
        for ordinal in 0..ty.record_field_count() {
            let (field_name, field_type) = view.record_field(parameter.value_type(), ordinal)?;
            if record_parameter_field_name(parameter, field_name)
                .is_ok_and(|candidate| candidate.lexeme() == name)
            {
                return value_type_shape(
                    view.value_type(field_type)?,
                    name,
                    parameter.declaration().span(),
                );
            }
        }
    }
    for value in function.values() {
        let ty = view.value_type(value.value_type())?;
        if !ty.is_record() {
            if value_name(value).is_ok_and(|candidate| candidate.lexeme() == name) {
                return value_type_shape(ty, name, value.declaration().span());
            }
            continue;
        }
        for ordinal in 0..ty.record_field_count() {
            let (field_name, field_type) = view.record_field(value.value_type(), ordinal)?;
            if record_value_field_name(value, field_name)
                .is_ok_and(|candidate| candidate.lexeme() == name)
            {
                return value_type_shape(
                    view.value_type(field_type)?,
                    name,
                    value.declaration().span(),
                );
            }
        }
    }
    None
}

/// A checked `ValueType`'s shape as (extents, element type), through the same
/// scalar mapping every declaration uses (so Enumeration compares as the
/// Integer it is declared as).
fn value_type_shape(
    ty: &dae::ValueType,
    name: &str,
    span: Span,
) -> Option<(Vec<u32>, gast::ScalarType)> {
    let scalar = scalar_type(ty.scalar_type(), name, span).ok()?;
    Some((ty.dimensions().to_vec(), scalar))
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
            gast::Statement::For(gast::ForLoop {
                iterator: Some(iterator.clone()),
                start: gast::Expression::Integer(1),
                step: None,
                stop: gast::Expression::Integer(i64::from(extent)),
                body,
            }),
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
            loop_statement: loop_statement.clone(),
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
            gast::Statement::For(loop_statement),
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

fn repeatable_loop_invariant_condition(
    condition: &gast::Condition,
    outer_indices: &[gast::Name],
) -> bool {
    match condition {
        gast::Condition::Expression(gast::Expression::Bool(_)) => true,
        gast::Condition::Expression(gast::Expression::Ref(reference)) => {
            !reference_depends_on(reference, outer_indices)
        }
        gast::Condition::Expression(_) | gast::Condition::SignalCheck(_) => false,
    }
}

fn collect_defined_names(statement: &gast::Spanned<gast::Statement>, names: &mut Vec<gast::Name>) {
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
