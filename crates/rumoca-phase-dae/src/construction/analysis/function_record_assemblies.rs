use super::*;
use rumoca_core::{ExpressionVisitor, Reference};

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
    let mut assignments: HashMap<VarName, Vec<usize>> = HashMap::new();
    for (index, statement) in statements.iter().enumerate() {
        if let Some((target, _)) = record_assignment_target(statement, context.function) {
            assignments
                .entry(VarName::new(&target.name))
                .or_default()
                .push(index);
        }
    }
    let mut plans = HashMap::new();
    let mut members = HashSet::new();
    for (target, indices) in assignments {
        if indices.windows(2).all(|pair| pair[1] == pair[0] + 1) {
            continue;
        }
        plan_staged_record(
            statements,
            context,
            &target,
            &indices,
            &mut plans,
            &mut members,
        )?;
    }
    Ok((plans, members))
}

fn plan_staged_record(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
    target: &VarName,
    indices: &[usize],
    plans: &mut HashMap<usize, FunctionRecordFieldAssemblyPlan>,
    members: &mut HashSet<usize>,
) -> Result<(), ToDaeError> {
    let declaration = context
        .function
        .outputs
        .iter()
        .chain(&context.function.locals)
        .find(|value| value.name == target.as_str())
        .expect("record assignment target resolves its declaration");
    let constructor = record_constructor(declaration, context)?;
    let field_names = constructor
        .inputs
        .iter()
        .map(|field| VarName::new(&field.name))
        .collect::<Vec<_>>();
    let final_index = *indices.last().expect("staged record has assignments");
    for field in &constructor.inputs {
        let field_indices = indices
            .iter()
            .copied()
            .filter(|index| {
                record_assignment_target(&statements[*index], context.function)
                    .is_some_and(|(_, part)| part.ident == field.name)
            })
            .collect::<Vec<_>>();
        let first = *field_indices.first().ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record output assembly",
                format!("`{target}.{}` is left undefined", field.name),
                field.span,
            )
        })?;
        if field_indices.windows(2).any(|pair| pair[1] != pair[0] + 1) {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{target}.{}` has non-contiguous partial writes whose statement-time values cannot yet be staged",
                    field.name
                ),
                field.span,
            ));
        }
        let group = field_indices
            .iter()
            .map(|index| statements[*index].clone())
            .collect::<Vec<_>>();
        let available_fields = constructor
            .inputs
            .iter()
            .filter(|candidate| {
                indices
                    .iter()
                    .copied()
                    .filter(|index| {
                        record_assignment_target(&statements[*index], context.function)
                            .is_some_and(|(_, part)| part.ident == candidate.name)
                    })
                    .max()
                    .is_some_and(|last| last < first)
            })
            .map(|candidate| VarName::new(&candidate.name))
            .collect::<Vec<_>>();
        let field_plan =
            validate_field_assembly(&group, target.as_str(), field, context, &available_fields)?;
        members.extend(field_indices.iter().copied().skip(1));
        plans.insert(
            first,
            FunctionRecordFieldAssemblyPlan {
                target: target.clone(),
                statement_count: field_indices.len(),
                field: field_plan,
                available_fields,
                finalize_fields: (field_indices.last() == Some(&final_index))
                    .then(|| field_names.clone()),
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
    let Some((target, field)) = record_assignment_target(&statements[start], context.function)
    else {
        return Ok(None);
    };
    let staged_field = FunctionRecordFieldCoordinate {
        target: VarName::new(&target.name),
        field: VarName::new(&field.ident),
    };
    if context.staged_record_fields.contains(&staged_field) {
        return Ok(None);
    }
    let count = statements[start..]
        .iter()
        .take_while(|statement| {
            record_assignment_target(statement, context.function)
                .is_some_and(|(candidate, _)| candidate.name == target.name)
        })
        .count();
    let group = &statements[start..start + count];
    let constructor = record_constructor(target, context)?;
    let mut fields = Vec::with_capacity(constructor.inputs.len());
    for field in &constructor.inputs {
        let first = group
            .iter()
            .position(|statement| {
                record_assignment_target(statement, context.function)
                    .is_some_and(|(_, part)| part_matches_record_field(part, &field.name))
            })
            .unwrap_or(group.len());
        let available_fields = constructor
            .inputs
            .iter()
            .filter(|candidate| {
                group
                    .iter()
                    .enumerate()
                    .filter(|(_, statement)| {
                        record_assignment_target(statement, context.function).is_some_and(
                            |(_, part)| part_matches_record_field(part, &candidate.name),
                        )
                    })
                    .map(|(index, _)| index)
                    .max()
                    .is_some_and(|last| last < first)
            })
            .map(|candidate| VarName::new(&candidate.name))
            .collect::<Vec<_>>();
        fields.push(validate_field_assembly(
            group,
            &target.name,
            field,
            context,
            &available_fields,
        )?);
    }
    Ok(Some((
        FunctionRecordAssemblyPlan {
            target: VarName::new(&target.name),
            statement_count: count,
            fields,
            seed: None,
        },
        count,
    )))
}

fn part_matches_record_field(part: &rumoca_core::ComponentRefPart, field: &str) -> bool {
    part.ident == field
        || field
            .strip_prefix(part.ident.as_str())
            .is_some_and(|suffix| suffix.starts_with('_'))
}

fn record_assignment_target<'scope>(
    statement: &'scope rumoca_core::Statement,
    function: &'scope rumoca_core::Function,
) -> Option<(
    &'scope rumoca_core::FunctionParam,
    &'scope rumoca_core::ComponentRefPart,
)> {
    let rumoca_core::Statement::Assignment { comp, .. } = statement else {
        return None;
    };
    let [root, field] = comp.parts() else {
        return None;
    };
    // MLS §12.2 gives protected locals the same declaration status as results,
    // so a record local is assembled from its field assignments exactly like a
    // record result: neither can be updated field-by-field in the checked DAE,
    // because a partial update would have to read the value's undefined fields.
    let value = function
        .outputs
        .iter()
        .chain(&function.locals)
        .find(|value| {
            value.name == root.ident && value.type_class == Some(rumoca_core::ClassType::Record)
        })?;
    Some((value, field))
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
    field: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'_>,
    available_fields: &[VarName],
) -> Result<FunctionRecordFieldAssembly, ToDaeError> {
    if field.type_class == Some(rumoca_core::ClassType::Record) {
        return validate_aggregate_field_assembly(
            statements,
            output,
            field,
            context,
            available_fields,
        );
    }
    let (dimensions, scalar_count) = field_scalar_layout(output, field)?;
    let scalars = collect_field_scalar_sources(
        statements,
        output,
        field,
        context,
        &dimensions,
        scalar_count,
        available_fields,
    )?;
    let scalars = require_total_field_scalars(scalars, output, field)?;
    Ok(FunctionRecordFieldAssembly {
        name: VarName::new(&field.name),
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

fn collect_field_scalar_sources(
    statements: &[rumoca_core::Statement],
    output: &str,
    field: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'_>,
    dimensions: &[u32],
    scalar_count: usize,
    available_fields: &[VarName],
) -> Result<Vec<Option<FunctionRecordScalarSource>>, ToDaeError> {
    let mut scalars = vec![None; scalar_count];
    for (statement_offset, statement) in statements.iter().enumerate() {
        let rumoca_core::Statement::Assignment { value, span, .. } = statement else {
            unreachable!("record assembly group contains assignments")
        };
        let Some((_, target)) = record_assignment_target(statement, context.function) else {
            unreachable!("record assembly group has validated two-part record targets")
        };
        let value_field = if target.ident == field.name {
            None
        } else if let Some(nested) = field
            .name
            .strip_prefix(target.ident.as_str())
            .and_then(|suffix| suffix.strip_prefix('_'))
            .filter(|suffix| !suffix.is_empty())
        {
            Some(VarName::new(nested))
        } else {
            continue;
        };
        require_span(*span, "record field assignment")?;
        validate_function_subscripts(&target.subs, context)?;
        validate_function_expression_with_roles(
            value,
            context.roles,
            context.flat,
            context.shapes,
        )?;
        reject_record_self_reference(value, output, available_fields, *span)?;
        let selection = field_selection(dimensions, &target.subs, *span)?;
        let found_shape = if let Some(value_field) = &value_field {
            let projected = Expression::FieldAccess {
                base: Box::new(value.clone()),
                field: value_field.as_str().to_string(),
                field_def_id: field.def_id.ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "record output assembly",
                        format!("`{output}.{}` has no exact field identity", field.name),
                        field.span,
                    )
                })?,
                span: *span,
            };
            context
                .shape_analysis
                .expression_shape(&projected, context.shapes)?
        } else {
            context
                .shape_analysis
                .expression_shape(value, context.shapes)?
        };
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
                    value_field: value_field.clone(),
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
    field: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'_>,
    available_fields: &[VarName],
) -> Result<FunctionRecordFieldAssembly, ToDaeError> {
    let mut source = None;
    for (statement_offset, statement) in statements.iter().enumerate() {
        let rumoca_core::Statement::Assignment { value, span, .. } = statement else {
            unreachable!("record assembly group contains assignments")
        };
        let Some((_, target)) = record_assignment_target(statement, context.function) else {
            unreachable!("record assembly group has validated two-part record targets")
        };
        if target.ident != field.name {
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
        reject_record_self_reference(value, output, available_fields, *span)?;
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
    available_fields: &[VarName],
    span: Span,
) -> Result<(), ToDaeError> {
    let available = available_fields.iter().cloned().collect::<HashSet<_>>();
    let mut checker = RecordSelfReadChecker {
        output,
        available: &available,
        unavailable: None,
    };
    checker.visit_expression(value);
    if let Some(reference) = checker.unavailable {
        let available = available_fields
            .iter()
            .map(VarName::as_str)
            .collect::<Vec<_>>()
            .join(", ");
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{reference}` is read before that record field is constructed; fields proven available here: [{available}]"
            ),
            span,
        ));
    }
    Ok(())
}

struct RecordSelfReadChecker<'scope> {
    output: &'scope str,
    available: &'scope HashSet<VarName>,
    unavailable: Option<String>,
}

impl ExpressionVisitor for RecordSelfReadChecker<'_> {
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
        self.check_reference(name.as_str());
    }

    fn visit_field_access(&mut self, base: &Expression, field: &str) {
        if let Some(base_path) = rumoca_core::flat_expression_component_path(base)
            && (base_path.as_str() == self.output
                || base_path.as_str().starts_with(&format!("{}.", self.output))
                || base_path.as_str().starts_with(&format!("{}[", self.output)))
        {
            self.check_reference(&format!("{base_path}.{field}"));
            return;
        }
        self.visit_expression(base);
    }
}

impl RecordSelfReadChecker<'_> {
    fn check_reference(&mut self, reference: &str) {
        if reference == self.output || reference.starts_with(&format!("{}[", self.output)) {
            self.unavailable
                .get_or_insert_with(|| reference.to_string());
            return;
        }
        let Some(field) = reference
            .strip_prefix(self.output)
            .and_then(|suffix| suffix.strip_prefix('.'))
            .and_then(|suffix| suffix.split(['.', '[']).next())
        else {
            return;
        };
        if !self.available.contains(&VarName::new(field)) {
            self.unavailable
                .get_or_insert_with(|| reference.to_string());
        }
    }
}

fn row_major_coordinates(dimensions: &[u32], scalar: usize) -> Option<Vec<u32>> {
    let count = dimensions
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))?;
    if scalar >= count {
        return None;
    }
    let mut remainder = scalar;
    let mut coordinates = Vec::with_capacity(dimensions.len());
    for extent in dimensions.iter().rev() {
        coordinates.push(u32::try_from(remainder % *extent as usize).ok()?);
        remainder /= *extent as usize;
    }
    coordinates.reverse();
    Some(coordinates)
}
