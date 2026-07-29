use super::*;

pub(super) fn validate_record_output_assembly(
    statements: &[rumoca_core::Statement],
    start: usize,
    context: FunctionValidationContext<'_>,
) -> Result<Option<(FunctionRecordAssemblyPlan, usize)>, ToDaeError> {
    let Some((target, _)) = record_assignment_target(&statements[start], context.function) else {
        return Ok(None);
    };
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
        fields.push(validate_field_assembly(
            group,
            &target.name,
            field,
            context,
        )?);
    }
    Ok(Some((
        FunctionRecordAssemblyPlan {
            target: VarName::new(&target.name),
            statement_count: count,
            fields,
        },
        count,
    )))
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
    let [root, field] = comp.parts.as_slice() else {
        return None;
    };
    let output = function.outputs.iter().find(|output| {
        output.name == root.ident && output.type_class == Some(rumoca_core::ClassType::Record)
    })?;
    Some((output, field))
}

fn record_constructor<'scope>(
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

fn validate_field_assembly(
    statements: &[rumoca_core::Statement],
    output: &str,
    field: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionRecordFieldAssembly, ToDaeError> {
    let dimensions = field
        .dims
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
    let mut scalars = vec![None; scalar_count];
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
        require_span(*span, "record field assignment")?;
        validate_function_subscripts(&target.subs, context)?;
        validate_function_expression_with_roles(value, context.roles, context.flat)?;
        reject_record_self_reference(value, output, *span)?;
        let selection = field_selection(&dimensions, &target.subs, *span)?;
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
            let base_coordinates = row_major_coordinates(&dimensions, base_scalar)
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
    let scalars = scalars
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
        .collect::<Result<Vec<_>, _>>()?;
    Ok(FunctionRecordFieldAssembly {
        name: VarName::new(&field.name),
        dimensions,
        scalars,
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
    span: Span,
) -> Result<(), ToDaeError> {
    let mut references = Vec::new();
    value.collect_var_refs(&mut references);
    let prefix = format!("{output}.");
    if references
        .iter()
        .any(|reference| reference.as_str() == output || reference.as_str().starts_with(&prefix))
    {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!("`{output}` is read before its complete value is constructed"),
            span,
        ));
    }
    Ok(())
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
