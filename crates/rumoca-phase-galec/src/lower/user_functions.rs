//! Array-native projection of reachable Modelica functions into GALEC.

use super::*;

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
        dae::FunctionStatementView::AssignmentGroup { definitions } => {
            for definition in definitions.iter() {
                lower_function_assignment(view, definition, lowerer, statements)?;
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
    let target_type = view
        .value_type(target.value_type())
        .expect("checked function target type resolves");
    if preserves_function_target(view, target, definition.rhs()) {
        return Ok(());
    }
    if let Some(statement) =
        lower_indexed_function_update(view, target, target_type, definition, lowerer)?
    {
        statements.extend(lowerer.drain_prefix_statements());
        statements.push(statement);
        return Ok(());
    }
    if target_type.is_record() {
        lower_record_function_assignment(view, target, target_type, definition, lowerer, statements)
    } else {
        lower_primitive_function_assignment(target, target_type, definition, lowerer, statements)
    }
}

fn lower_record_function_assignment<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    definition: dae::FunctionDefinitionView<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let mut record_statements = Vec::new();
    for ordinal in 0..target_type.record_field_count() {
        lower_record_function_field(
            view,
            target,
            definition,
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
    definition: dae::FunctionDefinitionView<'dae>,
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
    let span = definition.provenance().span();
    if let Some(lowered) = lower_tensor_function_assignment(
        TensorAssignment {
            target: record_value_field_name(target, field_name)?,
            target_type: field_view,
            expression: definition.rhs(),
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
    let value = lowerer.lower_aggregate_record_field(definition.rhs(), ordinal, field_type)?;
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
    definition: dae::FunctionDefinitionView<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let span = definition.provenance().span();
    let target_name = value_name(target)?;
    if let Some(call) = lowerer.lower_direct_aggregate_call_assignment(
        definition.rhs(),
        target_name.clone(),
        span,
    )? {
        statements.extend(lowerer.drain_prefix_statements());
        statements.push(call);
        return Ok(());
    }
    if let Some(lowered) = lower_tensor_function_assignment(
        TensorAssignment {
            target: target_name.clone(),
            target_type,
            expression: definition.rhs(),
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
    let value = lowerer.lower_aggregate_expression_as(definition.rhs(), target_scalar)?;
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
    let (before, mut body) = partition_tensor_prefixes(lowerer.drain_prefix_statements(), &names);
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

fn lower_indexed_function_update<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    definition: dae::FunctionDefinitionView<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
) -> Result<Option<gast::Spanned<gast::Statement>>, GalecTargetError> {
    if target_type.is_record() {
        return Ok(None);
    }
    let node = view
        .expression(definition.rhs())
        .expect("checked function assignment rhs resolves");
    if let dae::ExpressionOperation::Conditional(operands) = node.operation()
        && operands.len() == 3
    {
        return lower_conditional_indexed_function_update(
            view,
            target,
            target_type,
            operands,
            lowerer,
            definition.provenance().span(),
        );
    }
    lower_indexed_function_update_expression(
        view,
        target,
        target_type,
        definition.rhs(),
        lowerer,
        definition.provenance().span(),
    )
}

fn lower_conditional_indexed_function_update<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    operands: dae::ExpressionOperands<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    span: Span,
) -> Result<Option<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let prefix_start = lowerer.pending_prefix_statements.len();
    let condition = lowerer.lower(
        operands
            .get(0)
            .expect("checked conditional update condition"),
    )?;
    require_boolean(&condition, span)?;

    let body_start = lowerer.pending_prefix_statements.len();
    let Some(update) = lower_indexed_function_update_tree(
        view,
        target,
        target_type,
        operands.get(1).expect("checked conditional update value"),
        lowerer,
        span,
    )?
    else {
        lowerer.pending_prefix_statements.truncate(prefix_start);
        return Ok(None);
    };
    let mut body = lowerer.pending_prefix_statements.split_off(body_start);
    body.push(update);

    let else_start = lowerer.pending_prefix_statements.len();
    let fallback = operands
        .get(2)
        .expect("checked conditional update fallback");
    let else_body = match lower_indexed_function_update_tree(
        view,
        target,
        target_type,
        fallback,
        lowerer,
        span,
    )? {
        Some(update) => {
            let mut statements = lowerer.pending_prefix_statements.split_off(else_start);
            statements.push(update);
            Some(statements)
        }
        None if preserves_function_target(view, target, fallback) => {
            lowerer.pending_prefix_statements.truncate(else_start);
            None
        }
        None => {
            lowerer.pending_prefix_statements.truncate(prefix_start);
            return Ok(None);
        }
    };
    Ok(Some(gast::Spanned::new(
        gast::Statement::If(gast::IfStatement {
            branches: vec![gast::IfBranch {
                condition: gast::Condition::Expression(condition.expression),
                body,
                span,
            }],
            else_body,
        }),
        span,
    )))
}

fn lower_indexed_function_update_tree<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    expression: dae::ExprId<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    span: Span,
) -> Result<Option<gast::Spanned<gast::Statement>>, GalecTargetError> {
    if let dae::ExpressionOperation::Conditional(operands) = view
        .expression(expression)
        .expect("checked indexed update tree resolves")
        .operation()
        && operands.len() == 3
    {
        return lower_conditional_indexed_function_update(
            view,
            target,
            target_type,
            operands,
            lowerer,
            span,
        );
    }
    lower_indexed_function_update_expression(view, target, target_type, expression, lowerer, span)
}

fn preserves_function_target<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    match view
        .expression(expression)
        .expect("checked function fallback resolves")
        .operation()
    {
        dae::ExpressionOperation::FunctionValue { definition, .. } => {
            definition.target() == target.id()
        }
        dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. }
        | dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } => {
            view.function_fold(fold)
                .and_then(|fold| fold.targets().nth(carried as usize))
                == Some(target.id())
        }
        dae::ExpressionOperation::Record(fields) => {
            fields.iter().enumerate().all(|(field, expression)| {
                preserves_function_target_field(view, target, expression, field)
            })
        }
        _ => false,
    }
}

fn preserves_function_target_field<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    expression: dae::ExprId<'dae>,
    expected_field: usize,
) -> bool {
    let dae::ExpressionOperation::Field { base, field } = view
        .expression(expression)
        .expect("checked function record field resolves")
        .operation()
    else {
        return false;
    };
    field as usize == expected_field && preserves_function_target(view, target, base)
}

fn lower_indexed_function_update_expression<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    expression: dae::ExprId<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    span: Span,
) -> Result<Option<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let dae::ExpressionOperation::ArrayUpdate {
        value, subscripts, ..
    } = view
        .expression(expression)
        .expect("checked indexed function update resolves")
        .operation()
    else {
        return Ok(None);
    };
    if subscripts.len() != target_type.dimensions().len()
        || subscripts
            .iter()
            .any(|subscript| !matches!(subscript, dae::SubscriptView::Index { .. }))
    {
        return Ok(None);
    }
    let mut lowered_subscripts = Vec::with_capacity(subscripts.len());
    for (subscript, extent) in subscripts.iter().zip(target_type.dimensions()) {
        let dae::SubscriptView::Index { expression, .. } = subscript else {
            unreachable!("all-index update was checked above")
        };
        let index = lowerer.lower(expression)?.expression;
        if constant_integer(&index).is_none() {
            lowerer.prove_dynamic_index(&index, *extent, span)?;
        }
        lowered_subscripts.push(index);
    }
    let scalar_type = scalar_type(
        target_type.scalar_type(),
        target.name().as_str(),
        target.declaration().span(),
    )?;
    let value = coerce(lowerer.lower(value)?, scalar_type, span)?;
    Ok(Some(gast::Spanned::new(
        gast::Statement::Assignment {
            target: gast::Reference::Local(gast::RefPart {
                name: value_name(target)?,
                subscripts: lowered_subscripts,
                span,
            }),
            value,
        },
        span,
    )))
}
