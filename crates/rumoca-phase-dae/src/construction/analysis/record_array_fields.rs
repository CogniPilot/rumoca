use super::*;
use rumoca_core::ExpressionRewriter;

#[derive(Clone)]
pub(in crate::construction) struct RecordArrayFieldPlan {
    pub(in crate::construction) coordinates: Vec<VarName>,
    pub(in crate::construction) subscripts: Vec<Subscript>,
}

pub(super) fn analyze_record_array_fields<'expression>(
    flat: &flat::Model,
    expressions: impl IntoIterator<Item = &'expression Expression>,
) -> Result<HashMap<Span, RecordArrayFieldPlan>, ToDaeError> {
    let mut plans = HashMap::new();
    for expression in expressions {
        collect_plans(flat, expression, &mut plans)?;
    }
    Ok(plans)
}

fn collect_plans(
    flat: &flat::Model,
    expression: &Expression,
    plans: &mut HashMap<Span, RecordArrayFieldPlan>,
) -> Result<(), ToDaeError> {
    if let Some(plan) = plan_field_projection(flat, expression)? {
        let span = expression_span(expression)?;
        if let Some(previous) = plans.insert(span, plan)
            && (previous.coordinates != plans[&span].coordinates
                || previous.subscripts != plans[&span].subscripts)
        {
            return Err(ToDaeError::unsupported_flat(
                "record-array member slice",
                "one source occurrence produced incompatible projection certificates",
                span,
            ));
        }
        return Ok(());
    }
    for child in expression_children(expression) {
        collect_plans(flat, child, plans)?;
    }
    Ok(())
}

fn plan_field_projection(
    flat: &flat::Model,
    expression: &Expression,
) -> Result<Option<RecordArrayFieldPlan>, ToDaeError> {
    let Expression::FieldAccess { base, field, span } = expression else {
        return Ok(None);
    };
    let Expression::Index {
        base, subscripts, ..
    } = base.as_ref()
    else {
        return Ok(None);
    };
    let Expression::VarRef { name, .. } = base.as_ref() else {
        return Ok(None);
    };
    if subscripts.is_empty() {
        return Ok(None);
    }
    let Some(_definition) = name.target_def_id() else {
        return Err(ToDaeError::unresolved_reference(name.as_str(), *span));
    };
    let rank = subscripts.len();
    let mut elements = flat
        .variables
        .iter()
        .filter_map(|(coordinate, variable)| {
            match projected_variable_indices(name, field, variable, rank, *span) {
                Ok(Some(indices)) => Some(Ok((
                    indices,
                    coordinate.clone(),
                    variable.type_id,
                    variable.dims.clone(),
                ))),
                Ok(None) => None,
                Err(error) => Some(Err(error)),
            }
        })
        .collect::<Result<Vec<_>, ToDaeError>>()?;
    if elements.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            "the sliced record identity has no materialized Flat elements",
            *span,
        ));
    }
    elements.sort_by(|(lhs, ..), (rhs, ..)| lhs.cmp(rhs));
    validate_rectangular_elements(&elements, rank, *span)?;
    Ok(Some(RecordArrayFieldPlan {
        coordinates: elements
            .into_iter()
            .map(|(_, coordinate, _, _)| coordinate)
            .collect(),
        subscripts: subscripts.clone(),
    }))
}

fn projected_variable_indices(
    base: &rumoca_core::Reference,
    field: &str,
    variable: &flat::Variable,
    rank: usize,
    span: Span,
) -> Result<Option<Vec<i64>>, ToDaeError> {
    let Some(reference) = variable.component_ref.as_ref() else {
        return Ok(None);
    };
    let base_parts = base.parts();
    if base_parts.is_empty()
        || reference.parts.len() != base_parts.len() + 1
        || !reference.parts[..base_parts.len()]
            .iter()
            .zip(base_parts)
            .all(|(candidate, source)| candidate.ident == source.ident)
        || reference.parts.last().map(|part| part.ident.as_str()) != Some(field)
        || !reference
            .parts
            .last()
            .is_some_and(|part| part.subs.is_empty())
    {
        return Ok(None);
    }
    if base_parts.iter().any(|part| !part.subs.is_empty())
        || reference.parts[..base_parts.len() - 1]
            .iter()
            .any(|part| !part.subs.is_empty())
    {
        return Ok(None);
    }
    let indices = reference.parts[base_parts.len() - 1]
        .subs
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } if *value >= 1 => Ok(*value),
            Subscript::Index { .. } | Subscript::Expr { .. } | Subscript::Colon { .. } => {
                Err(ToDaeError::unsupported_flat(
                    "record-array member slice",
                    "materialized record elements require positive literal indices",
                    span,
                ))
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    if indices.len() != rank {
        return Ok(None);
    }
    Ok(Some(indices))
}

fn validate_rectangular_elements(
    elements: &[(Vec<i64>, VarName, rumoca_core::TypeId, Vec<i64>)],
    rank: usize,
    span: Span,
) -> Result<(), ToDaeError> {
    let first_type = elements[0].2;
    let first_shape = &elements[0].3;
    if elements
        .iter()
        .any(|(_, _, ty, shape)| *ty != first_type || shape != first_shape)
    {
        return Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            "projected fields do not share one canonical value type and shape",
            span,
        ));
    }
    let mut extents = vec![0i64; rank];
    for (indices, ..) in elements {
        for (ordinal, index) in indices.iter().copied().enumerate() {
            extents[ordinal] = extents[ordinal].max(index);
        }
    }
    let scalar_count = extents.iter().try_fold(1usize, |count, extent| {
        usize::try_from(*extent)
            .ok()
            .and_then(|extent| count.checked_mul(extent))
    });
    if scalar_count != Some(elements.len())
        || elements.windows(2).any(|pair| pair[0].0 == pair[1].0)
    {
        return Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            "materialized record elements do not form one dense rectangular domain",
            span,
        ));
    }
    Ok(())
}

pub(super) fn expression_for_validation(
    expression: &Expression,
    plans: &HashMap<Span, RecordArrayFieldPlan>,
) -> Expression {
    struct ProjectionEraser<'plans> {
        plans: &'plans HashMap<Span, RecordArrayFieldPlan>,
    }

    impl ExpressionRewriter for ProjectionEraser<'_> {
        fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
            if matches!(expression, Expression::FieldAccess { .. })
                && expression
                    .span()
                    .is_some_and(|span| self.plans.contains_key(&span))
            {
                let span = expression
                    .span()
                    .expect("planned field access has provenance");
                let base = Expression::Literal {
                    value: Literal::Real(0.0),
                    span,
                };
                return Expression::Index {
                    base: Box::new(base),
                    subscripts: self.plans[&span].subscripts.clone(),
                    span,
                };
            }
            self.walk_expression(expression)
        }
    }

    ProjectionEraser { plans }.rewrite_expression(expression)
}
