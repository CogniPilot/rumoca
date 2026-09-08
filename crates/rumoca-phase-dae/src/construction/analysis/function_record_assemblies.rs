use super::*;
use rumoca_core::{ExpressionVisitor, Reference, row_major_coordinates};

#[cfg(test)]
mod tests;

pub(super) fn plan_staged_record_assemblies(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
) -> Result<
    (
        HashMap<usize, FunctionRecordFieldAssemblyPlan>,
        HashSet<usize>,
    ),
    ToDaeError,
> {
    let assignments = staged_record_assignment_groups(statements, context.function)?;
    let mut plans = HashMap::new();
    let mut members = HashSet::new();
    for (target, indices) in assignments {
        if indices.windows(2).all(|pair| pair[1] == pair[0] + 1) {
            continue;
        }
        plan_staged_record(
            statements,
            context,
            target,
            &indices,
            &mut plans,
            &mut members,
        )?;
    }
    Ok((plans, members))
}

/// Group staged record writes by resolved function-value identity while
/// retaining the first source occurrence of each target.
///
/// The `Vec` is intentional: a hash-map iteration would make the order in
/// which competing target defects are reported depend on the randomized hash
/// seed, while display spelling cannot distinguish two exact declarations.
fn staged_record_assignment_groups<'scope>(
    statements: &'scope [rumoca_core::Statement],
    function: &'scope rumoca_core::Function,
) -> Result<Vec<(&'scope rumoca_core::FunctionParam, Vec<usize>)>, ToDaeError> {
    let mut assignments: Vec<(&rumoca_core::FunctionParam, Vec<usize>)> = Vec::new();
    for (index, statement) in statements.iter().enumerate() {
        if let Some((target, _)) = record_assignment_target(statement, function)? {
            if let Some((_, indices)) = assignments
                .iter_mut()
                .find(|(candidate, _)| candidate.def_id == target.def_id)
            {
                indices.push(index);
            } else {
                assignments.push((target, vec![index]));
            }
        }
    }
    Ok(assignments)
}

fn plan_staged_record(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
    target: &rumoca_core::FunctionParam,
    indices: &[usize],
    plans: &mut HashMap<usize, FunctionRecordFieldAssemblyPlan>,
    members: &mut HashSet<usize>,
) -> Result<(), ToDaeError> {
    let constructor = record_constructor(target, context)?;
    let fields = resolved_constructor_fields(&target.name, constructor)?;
    let target_def_id = function_value_def_id(target, context.function)?;
    require_group_constructor_fields(statements, indices, context.function, &fields)?;
    let final_index = *indices.last().expect("staged record has assignments");
    for (field, resolved_field) in constructor.inputs.iter().zip(&fields) {
        let mut field_indices = Vec::new();
        for index in indices.iter().copied() {
            let Some((_, part)) = record_assignment_target(&statements[index], context.function)?
            else {
                continue;
            };
            if assignment_part_matches_field(part, resolved_field)? {
                field_indices.push(index);
            }
        }
        let first = *field_indices.first().ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record output assembly",
                format!("`{}.{}` is left undefined", target.name, field.name),
                field.span,
            )
        })?;
        if field_indices.windows(2).any(|pair| pair[1] != pair[0] + 1) {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{}.{}` has non-contiguous partial writes whose statement-time values cannot yet be staged",
                    target.name, field.name
                ),
                field.span,
            ));
        }
        let group = field_indices
            .iter()
            .map(|index| statements[*index].clone())
            .collect::<Vec<_>>();
        let mut available_fields = Vec::new();
        for resolved_candidate in &fields {
            let last = last_matching_field_assignment(
                indices
                    .iter()
                    .copied()
                    .map(|index| (index, &statements[index])),
                context.function,
                resolved_candidate,
            )?;
            if last.is_some_and(|last| last < first) {
                available_fields.push(resolved_candidate.clone());
            }
        }
        let field_plan = validate_field_assembly(
            &group,
            &target.name,
            target_def_id,
            field,
            resolved_field.def_id,
            context,
            &available_fields,
        )?;
        members.extend(field_indices.iter().copied().skip(1));
        plans.insert(
            first,
            FunctionRecordFieldAssemblyPlan {
                target: VarName::new(&target.name),
                target_def_id,
                statement_count: field_indices.len(),
                field: field_plan,
                available_fields,
                finalize_fields: (field_indices.last() == Some(&final_index))
                    .then(|| fields.clone()),
            },
        );
    }
    Ok(())
}

pub(super) fn validate_record_output_assembly(
    statements: &[rumoca_core::Statement],
    start: usize,
    context: FunctionValidationContext<'_>,
) -> Result<Option<(FunctionRecordAssemblyPlan, usize)>, ToDaeError> {
    let Some((target, field)) = record_assignment_target(&statements[start], context.function)?
    else {
        return Ok(None);
    };
    let target_def_id = function_value_def_id(target, context.function)?;
    let staged_field = FunctionRecordFieldIdentity {
        target: target_def_id,
        field: field.def_id,
    };
    if context.staged_record_fields.contains(&staged_field) {
        return Ok(None);
    }
    let mut count = 0;
    for statement in &statements[start..] {
        let Some((candidate, _)) = record_assignment_target(statement, context.function)? else {
            break;
        };
        if function_value_def_id(candidate, context.function)? != target_def_id {
            break;
        }
        count += 1;
    }
    let group = &statements[start..start + count];
    let constructor = record_constructor(target, context)?;
    let resolved_fields = resolved_constructor_fields(&target.name, constructor)?;
    let group_indices = (start..start + count).collect::<Vec<_>>();
    require_group_constructor_fields(
        statements,
        &group_indices,
        context.function,
        &resolved_fields,
    )?;
    let mut fields = Vec::with_capacity(constructor.inputs.len());
    for (field, resolved_field) in constructor.inputs.iter().zip(&resolved_fields) {
        let mut first = group.len();
        for (index, statement) in group.iter().enumerate() {
            let Some((_, part)) = record_assignment_target(statement, context.function)? else {
                continue;
            };
            if assignment_part_matches_field(part, resolved_field)? {
                first = index;
                break;
            }
        }
        let mut available_fields = Vec::new();
        for resolved_candidate in &resolved_fields {
            let last = last_matching_field_assignment(
                group.iter().enumerate(),
                context.function,
                resolved_candidate,
            )?;
            if last.is_some_and(|last| last < first) {
                available_fields.push(resolved_candidate.clone());
            }
        }
        fields.push(validate_field_assembly(
            group,
            &target.name,
            target_def_id,
            field,
            resolved_field.def_id,
            context,
            &available_fields,
        )?);
    }
    Ok(Some((
        FunctionRecordAssemblyPlan {
            target: VarName::new(&target.name),
            target_def_id,
            statement_count: count,
            fields,
            seed: None,
        },
        count,
    )))
}

fn last_matching_field_assignment<'statement>(
    assignments: impl IntoIterator<Item = (usize, &'statement rumoca_core::Statement)>,
    function: &rumoca_core::Function,
    field: &ResolvedFunctionRecordField,
) -> Result<Option<usize>, ToDaeError> {
    let mut last = None;
    for (index, statement) in assignments {
        let Some((_, part)) = record_assignment_target(statement, function)? else {
            continue;
        };
        if assignment_part_matches_field(part, field)? {
            last = Some(index);
        }
    }
    Ok(last)
}

fn record_assignment_target<'scope>(
    statement: &'scope rumoca_core::Statement,
    function: &'scope rumoca_core::Function,
) -> Result<
    Option<(
        &'scope rumoca_core::FunctionParam,
        &'scope rumoca_core::ComponentRefPart,
    )>,
    ToDaeError,
> {
    let rumoca_core::Statement::Assignment { comp, .. } = statement else {
        return Ok(None);
    };
    let [root, field] = comp.parts() else {
        return Ok(None);
    };
    // MLS §12.2 gives protected locals the same declaration status as results,
    // so a record local is assembled from its field assignments exactly like a
    // record result: neither can be updated field-by-field in the checked DAE,
    // because a partial update would have to read the value's undefined fields.
    let Some(value) = resolved_record_value(root, function)? else {
        return Ok(None);
    };
    if value.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(None);
    }
    Ok(Some((value, field)))
}

pub(super) fn resolved_record_value<'scope>(
    root: &rumoca_core::ComponentRefPart,
    function: &'scope rumoca_core::Function,
) -> Result<Option<&'scope rumoca_core::FunctionParam>, ToDaeError> {
    let mut resolved_values = function
        .outputs
        .iter()
        .chain(&function.locals)
        .filter(|value| value.def_id == Some(root.def_id));
    let Some(value) = resolved_values.next() else {
        if let Some(candidate) = function
            .outputs
            .iter()
            .chain(&function.locals)
            .find(|value| {
                value.name == root.ident && value.type_class == Some(rumoca_core::ClassType::Record)
            })
        {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{}.{}` retains field syntax but its root identity {} does not match declaration {}",
                    function.name,
                    root.ident,
                    root.def_id.index(),
                    candidate
                        .def_id
                        .map_or_else(|| "<missing>".to_string(), |id| id.index().to_string())
                ),
                root.span,
            ));
        }
        return Ok(None);
    };
    if resolved_values.next().is_some() {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "function `{}` repeats resolved value identity {}",
                function.name,
                root.def_id.index()
            ),
            root.span,
        ));
    }
    if value.name != root.ident {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "resolved function value identity {} is spelled `{}` in the declaration but `{}` at the assignment",
                root.def_id.index(),
                value.name,
                root.ident
            ),
            root.span,
        ));
    }
    Ok(Some(value))
}

pub(super) fn function_value_def_id(
    value: &rumoca_core::FunctionParam,
    function: &rumoca_core::Function,
) -> Result<rumoca_core::DefId, ToDaeError> {
    value
        .def_id
        .filter(|identity| identity.index() != 0)
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{}.{}` has no resolved function-value identity",
                    function.name, value.name
                ),
                value.span,
            )
        })
}

fn resolved_record_field(
    output: &str,
    field: &rumoca_core::FunctionParam,
) -> Result<ResolvedFunctionRecordField, ToDaeError> {
    let def_id = field
        .def_id
        .filter(|identity| identity.index() != 0)
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record output assembly",
                format!("`{output}.{}` has no resolved field identity", field.name),
                field.span,
            )
        })?;
    Ok(ResolvedFunctionRecordField {
        name: VarName::new(&field.name),
        def_id,
    })
}

pub(super) fn resolved_constructor_fields(
    output: &str,
    constructor: &rumoca_core::Function,
) -> Result<Vec<ResolvedFunctionRecordField>, ToDaeError> {
    let mut identities = HashSet::new();
    let mut names = HashSet::new();
    constructor
        .inputs
        .iter()
        .map(|field| {
            let resolved = resolved_record_field(output, field)?;
            if !identities.insert(resolved.def_id) {
                return Err(ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!(
                        "`{output}` constructor repeats resolved field identity {}",
                        resolved.def_id.index()
                    ),
                    field.span,
                ));
            }
            if !names.insert(resolved.name.clone()) {
                return Err(ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!("`{output}` constructor repeats field `{}`", resolved.name),
                    field.span,
                ));
            }
            Ok(resolved)
        })
        .collect()
}

fn assignment_part_matches_field(
    part: &rumoca_core::ComponentRefPart,
    field: &ResolvedFunctionRecordField,
) -> Result<bool, ToDaeError> {
    if part.def_id == field.def_id {
        if part.ident == field.name.as_str() {
            return Ok(true);
        }
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "resolved record field identity {} is spelled `{}` in the constructor but `{}` at the assignment",
                field.def_id.index(),
                field.name,
                part.ident
            ),
            part.span,
        ));
    }
    if part.ident == field.name.as_str() {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "record field `{}` has constructor identity {} but assignment identity {}",
                field.name,
                field.def_id.index(),
                part.def_id.index()
            ),
            part.span,
        ));
    }
    Ok(false)
}

fn require_group_constructor_fields(
    statements: &[rumoca_core::Statement],
    indices: &[usize],
    function: &rumoca_core::Function,
    fields: &[ResolvedFunctionRecordField],
) -> Result<(), ToDaeError> {
    for index in indices.iter().copied() {
        let Some((target, part)) = record_assignment_target(&statements[index], function)? else {
            unreachable!("record assembly indices retain exact two-part record targets")
        };
        require_constructor_field(&target.name, part, fields)?;
    }
    Ok(())
}

pub(super) fn require_constructor_field<'fields>(
    output: &str,
    part: &rumoca_core::ComponentRefPart,
    fields: &'fields [ResolvedFunctionRecordField],
) -> Result<&'fields ResolvedFunctionRecordField, ToDaeError> {
    for field in fields {
        if assignment_part_matches_field(part, field)? {
            return Ok(field);
        }
    }
    Err(ToDaeError::unsupported_flat(
        "record output assembly",
        format!(
            "`{output}.{}` carries identity {} but resolves to no constructor field",
            part.ident,
            part.def_id.index()
        ),
        part.span,
    ))
}

pub(super) fn record_constructor<'scope>(
    output: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'scope>,
) -> Result<&'scope rumoca_core::Function, ToDaeError> {
    let type_def_id = output.type_def_id.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{}.{}` has no resolved record type identity",
                context.function.name, output.name
            ),
            output.span,
        )
    })?;
    rumoca_core::resolve_record_constructor(
        context.flat.functions.values(),
        &output.type_name,
        type_def_id,
    )
    .map_err(|error| {
        ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{}.{}` has no resolved constructor layout: {error}",
                context.function.name, output.name
            ),
            output.span,
        )
    })
}

/// The declared extents of one record output field and its scalar count.
fn field_scalar_layout(
    output: &str,
    field: &rumoca_core::FunctionParam,
) -> Result<(Vec<u32>, usize), ToDaeError> {
    let dimensions = field
        .dimensions()
        .iter()
        .map(|extent| {
            u32::try_from(*extent).ok().ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!("`{output}.{}` has invalid extent `{extent}`", field.name),
                    field.span,
                )
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let scalar_count = dimensions
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{output}.{}` exceeds the checked scalar domain",
                    field.name
                ),
                field.span,
            )
        })?;
    Ok((dimensions, scalar_count))
}

fn validate_field_assembly(
    statements: &[rumoca_core::Statement],
    output: &str,
    output_def_id: rumoca_core::DefId,
    field: &rumoca_core::FunctionParam,
    field_def_id: rumoca_core::DefId,
    context: FunctionValidationContext<'_>,
    available_fields: &[ResolvedFunctionRecordField],
) -> Result<FunctionRecordFieldAssembly, ToDaeError> {
    if field.type_class == Some(rumoca_core::ClassType::Record) {
        return validate_aggregate_field_assembly(
            statements,
            output,
            output_def_id,
            field,
            field_def_id,
            context,
            available_fields,
        );
    }
    let (dimensions, scalar_count) = field_scalar_layout(output, field)?;
    let scalars = collect_field_scalar_sources(
        statements,
        FieldScalarCollection {
            output,
            output_def_id,
            field,
            field_def_id,
            context,
            dimensions: &dimensions,
            scalar_count,
            available_fields,
        },
    )?;
    let scalars = require_total_field_scalars(scalars, output, field)?;
    Ok(FunctionRecordFieldAssembly {
        name: VarName::new(&field.name),
        def_id: field_def_id,
        scalar_type: Some(
            effective_function_scalar_type(context.flat, field).ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!(
                        "`{output}.{}` has no scalar tensor element type",
                        field.name
                    ),
                    field.span,
                )
            })?,
        ),
        dimensions,
        scalars,
        aggregate_statement: None,
    })
}

struct FieldScalarCollection<'scope> {
    output: &'scope str,
    output_def_id: rumoca_core::DefId,
    field: &'scope rumoca_core::FunctionParam,
    field_def_id: rumoca_core::DefId,
    context: FunctionValidationContext<'scope>,
    dimensions: &'scope [u32],
    scalar_count: usize,
    available_fields: &'scope [ResolvedFunctionRecordField],
}

fn collect_field_scalar_sources(
    statements: &[rumoca_core::Statement],
    input: FieldScalarCollection<'_>,
) -> Result<Vec<Option<FunctionRecordScalarSource>>, ToDaeError> {
    let FieldScalarCollection {
        output,
        output_def_id,
        field,
        field_def_id,
        context,
        dimensions,
        scalar_count,
        available_fields,
    } = input;
    let mut scalars = vec![None; scalar_count];
    for (statement_offset, statement) in statements.iter().enumerate() {
        let rumoca_core::Statement::Assignment { value, span, .. } = statement else {
            unreachable!("record assembly group contains assignments")
        };
        let Some((_, target)) = record_assignment_target(statement, context.function)? else {
            unreachable!("record assembly group has validated two-part record targets")
        };
        let resolved_field = ResolvedFunctionRecordField {
            name: VarName::new(&field.name),
            def_id: field_def_id,
        };
        if !assignment_part_matches_field(target, &resolved_field)? {
            continue;
        }
        require_span(*span, "record field assignment")?;
        validate_function_subscripts(&target.subs, context)?;
        validate_function_expression_with_roles(
            value,
            context.roles,
            context.flat,
            context.shapes,
        )?;
        reject_record_self_reference(value, output, output_def_id, available_fields, *span)?;
        let selection = field_selection(dimensions, &target.subs, *span)?;
        let found_shape = context
            .shape_analysis
            .expression_shape(value, context.shapes)?;
        if found_shape != selection.value_dimensions {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{output}.{}` selection shape {:?} does not match value shape {:?}",
                    field.name, selection.value_dimensions, found_shape
                ),
                *span,
            ));
        }
        for (base_scalar, scalar_source) in scalars.iter_mut().enumerate() {
            let base_coordinates = row_major_coordinates(dimensions, base_scalar)
                .expect("validated record field scalar is in range");
            let Some(value_coordinates) = selection.selected_value_coordinates(&base_coordinates)
            else {
                continue;
            };
            if scalar_source
                .replace(FunctionRecordScalarSource {
                    statement_offset,
                    value_coordinates,
                })
                .is_some()
            {
                return Err(ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!("`{output}.{}` is assigned more than once", field.name),
                    *span,
                ));
            }
        }
    }
    Ok(scalars)
}

fn require_total_field_scalars(
    scalars: Vec<Option<FunctionRecordScalarSource>>,
    output: &str,
    field: &rumoca_core::FunctionParam,
) -> Result<Vec<FunctionRecordScalarSource>, ToDaeError> {
    scalars
        .into_iter()
        .enumerate()
        .map(|(scalar, source)| {
            source.ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!(
                        "`{output}.{}` leaves scalar {} undefined",
                        field.name,
                        scalar + 1
                    ),
                    field.span,
                )
            })
        })
        .collect()
}

fn validate_aggregate_field_assembly(
    statements: &[rumoca_core::Statement],
    output: &str,
    output_def_id: rumoca_core::DefId,
    field: &rumoca_core::FunctionParam,
    field_def_id: rumoca_core::DefId,
    context: FunctionValidationContext<'_>,
    available_fields: &[ResolvedFunctionRecordField],
) -> Result<FunctionRecordFieldAssembly, ToDaeError> {
    let mut source = None;
    for (statement_offset, statement) in statements.iter().enumerate() {
        let rumoca_core::Statement::Assignment { value, span, .. } = statement else {
            unreachable!("record assembly group contains assignments")
        };
        let Some((_, target)) = record_assignment_target(statement, context.function)? else {
            unreachable!("record assembly group has validated two-part record targets")
        };
        let resolved_field = ResolvedFunctionRecordField {
            name: VarName::new(&field.name),
            def_id: field_def_id,
        };
        if !assignment_part_matches_field(target, &resolved_field)? {
            continue;
        }
        require_span(*span, "record aggregate field assignment")?;
        if !target.subs.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{output}.{}` is a record field and must be assigned whole",
                    field.name
                ),
                *span,
            ));
        }
        validate_function_expression_with_roles(
            value,
            context.roles,
            context.flat,
            context.shapes,
        )?;
        reject_record_self_reference(value, output, output_def_id, available_fields, *span)?;
        if source.replace(statement_offset).is_some() {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!("`{output}.{}` is assigned more than once", field.name),
                *span,
            ));
        }
    }
    let aggregate_statement = source.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record output assembly",
            format!("`{output}.{}` is left undefined", field.name),
            field.span,
        )
    })?;
    Ok(FunctionRecordFieldAssembly {
        name: VarName::new(&field.name),
        def_id: field_def_id,
        scalar_type: None,
        dimensions: Vec::new(),
        scalars: Vec::new(),
        aggregate_statement: Some(aggregate_statement),
    })
}

struct FieldSelection {
    axes: Vec<Option<u32>>,
    value_dimensions: Vec<u32>,
}

impl FieldSelection {
    fn selected_value_coordinates(&self, base: &[u32]) -> Option<Vec<u32>> {
        let mut value = Vec::with_capacity(self.value_dimensions.len());
        for (coordinate, selected) in base.iter().copied().zip(&self.axes) {
            match selected {
                Some(expected) if *expected != coordinate => return None,
                Some(_) => {}
                None => value.push(coordinate),
            }
        }
        Some(value)
    }
}

fn field_selection(
    dimensions: &[u32],
    subscripts: &[Subscript],
    span: Span,
) -> Result<FieldSelection, ToDaeError> {
    if subscripts.len() > dimensions.len() {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            "field assignment has more subscripts than its rank",
            span,
        ));
    }
    let mut axes = Vec::with_capacity(dimensions.len());
    let mut value_dimensions = Vec::new();
    for (axis, extent) in dimensions.iter().copied().enumerate() {
        let selected = match subscripts.get(axis) {
            Some(Subscript::Index { value, .. }) => Some(*value),
            Some(Subscript::Expr { expr, .. }) => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } => Some(*value),
                _ => {
                    return Err(ToDaeError::unsupported_flat(
                        "record output assembly",
                        "field coverage requires literal Integer indices or whole axes",
                        span,
                    ));
                }
            },
            Some(Subscript::Colon { .. }) | None => None,
        };
        match selected {
            Some(index) => {
                let coordinate = u32::try_from(index)
                    .ok()
                    .and_then(|index| index.checked_sub(1))
                    .filter(|index| *index < extent)
                    .ok_or_else(|| {
                        ToDaeError::unsupported_flat(
                            "record output assembly",
                            format!("field index `{index}` exceeds axis extent `{extent}`"),
                            span,
                        )
                    })?;
                axes.push(Some(coordinate));
            }
            None => {
                axes.push(None);
                value_dimensions.push(extent);
            }
        }
    }
    Ok(FieldSelection {
        axes,
        value_dimensions,
    })
}

fn reject_record_self_reference(
    value: &Expression,
    output: &str,
    output_def_id: rumoca_core::DefId,
    available_fields: &[ResolvedFunctionRecordField],
    span: Span,
) -> Result<(), ToDaeError> {
    let mut checker = RecordSelfReadChecker {
        output,
        output_def_id,
        available: available_fields,
        unavailable: None,
        identity_error: None,
        fallback_span: span,
    };
    checker.visit_expression(value);
    if let Some((error, error_span)) = checker.identity_error {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            error,
            error_span,
        ));
    }
    if let Some((reference, reference_span)) = checker.unavailable {
        let available = available_fields
            .iter()
            .map(|field| field.name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{reference}` is read before that record field is constructed; fields proven available here: [{available}]"
            ),
            reference_span,
        ));
    }
    Ok(())
}

struct RecordSelfReadChecker<'scope> {
    output: &'scope str,
    output_def_id: rumoca_core::DefId,
    available: &'scope [ResolvedFunctionRecordField],
    unavailable: Option<(String, Span)>,
    identity_error: Option<(String, Span)>,
    fallback_span: Span,
}

impl ExpressionVisitor for RecordSelfReadChecker<'_> {
    fn visit_expression(&mut self, expression: &Expression) {
        if let Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } = expression
        {
            let Some(name) = expression_root_reference(base) else {
                self.walk_expression(expression);
                return;
            };
            if !self.reference_is_output_candidate(name) {
                self.walk_expression(expression);
                return;
            }
            if !self.require_output_root_identity(name, *span) {
                return;
            }
            let Expression::VarRef { subscripts, .. } = base.as_ref() else {
                self.refuse_nested_self_read(expression, *span);
                return;
            };
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
            self.check_field(field, *field_def_id, expression, *span);
            return;
        }
        self.walk_expression(expression);
    }

    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
        let reference_span = name.span().unwrap_or(self.fallback_span);
        if !self.reference_is_output_candidate(name)
            || !self.require_output_root_identity(name, reference_span)
        {
            return;
        }
        let parts = name.parts();
        if parts.len() == 1 {
            self.unavailable
                .get_or_insert_with(|| (name.as_str().to_string(), reference_span));
            return;
        }
        if parts.len() > 2 {
            self.identity_error.get_or_insert_with(|| {
                (
                    format!(
                        "nested self-read `{}` is not yet represented by a complete staged record-field identity path",
                        name.as_str()
                    ),
                    reference_span,
                )
            });
            return;
        }
        self.check_field(&parts[1].ident, parts[1].def_id, name, reference_span);
    }
}

impl RecordSelfReadChecker<'_> {
    fn refuse_nested_self_read(&mut self, expression: &Expression, span: Span) {
        if self.identity_error.is_some() {
            return;
        }
        let reference = rumoca_core::flat_expression_component_path(expression).map_or_else(
            || format!("{}.<nested>", self.output),
            |path| path.to_string(),
        );
        self.identity_error = Some((
            format!(
                "nested self-read `{reference}` is not yet represented by a complete staged record-field identity path"
            ),
            span,
        ));
    }
}

impl RecordSelfReadChecker<'_> {
    fn reference_has_output_spelling(&self, reference: &Reference) -> bool {
        reference.as_str() == self.output
            || reference.as_str().starts_with(&format!("{}.", self.output))
            || reference.as_str().starts_with(&format!("{}[", self.output))
    }

    fn reference_is_output_candidate(&self, reference: &Reference) -> bool {
        self.reference_has_output_spelling(reference)
            || reference.root_def_id() == Some(self.output_def_id)
    }

    fn require_output_root_identity(&mut self, reference: &Reference, span: Span) -> bool {
        let Some(root) = reference.parts().first() else {
            self.identity_error.get_or_insert_with(|| {
                (
                    format!(
                        "`{}` has record-output spelling but no structured resolved identity",
                        reference.as_str()
                    ),
                    span,
                )
            });
            return false;
        };
        if root.ident == self.output
            && root.def_id == self.output_def_id
            && self.reference_has_output_spelling(reference)
        {
            return true;
        }
        self.identity_error.get_or_insert_with(|| {
            (
                format!(
                    "`{}` has record-output spelling but root `{}` identity {} does not match `{}` identity {}",
                    reference.as_str(),
                    root.ident,
                    root.def_id.index(),
                    self.output,
                    self.output_def_id.index()
                ),
                span,
            )
        });
        false
    }

    fn check_field(
        &mut self,
        spelling: &str,
        identity: rumoca_core::DefId,
        reference: &impl std::fmt::Debug,
        span: Span,
    ) {
        if let Some(field) = self.available.iter().find(|field| field.def_id == identity) {
            if field.name.as_str() == spelling {
                return;
            }
            self.identity_error.get_or_insert_with(|| {
                (
                    format!(
                        "record field identity {} is retained as `{}` but read as `{spelling}` in {reference:?}",
                        identity.index(),
                        field.name
                    ),
                    span,
                )
            });
            return;
        }
        if let Some(field) = self
            .available
            .iter()
            .find(|field| field.name.as_str() == spelling)
        {
            self.identity_error.get_or_insert_with(|| {
                (
                    format!(
                        "record field `{spelling}` has identity {} but the read carries identity {} in {reference:?}",
                        field.def_id.index(),
                        identity.index()
                    ),
                    span,
                )
            });
            return;
        }
        self.unavailable
            .get_or_insert_with(|| (format!("{}.{spelling}", self.output), span));
    }
}

fn expression_root_reference(expression: &Expression) -> Option<&Reference> {
    match expression {
        Expression::VarRef { name, .. } => Some(name),
        Expression::Index { base, .. } | Expression::FieldAccess { base, .. } => {
            expression_root_reference(base)
        }
        _ => None,
    }
}
