use super::*;

pub(super) fn collapse_overlapping_array_assignments(
    dae: &Dae,
    assignments: IndexMap<VarName, AlgorithmAssignment>,
) -> Result<IndexMap<VarName, AlgorithmAssignment>, String> {
    let scalar_bases = scalar_assignment_bases(assignments.keys());
    let mut collapsed = IndexMap::with_capacity(assignments.len());

    for (target, assignment) in assignments {
        let Some(dims) = overlapping_array_dims(dae, &target, &scalar_bases) else {
            collapsed.insert(target, assignment);
            continue;
        };
        for scalar_assignment in expand_array_assignment(&target, &dims, &assignment)? {
            collapsed.insert(scalar_assignment.0.clone(), scalar_assignment);
        }
    }

    Ok(collapsed)
}

fn scalar_assignment_bases<'a>(targets: impl Iterator<Item = &'a VarName>) -> HashSet<VarName> {
    targets
        .filter_map(|target| {
            let base = dae::component_base_name(target.as_str())?;
            (base != target.as_str()).then(|| VarName::new(base))
        })
        .collect()
}

fn overlapping_array_dims(
    dae: &Dae,
    target: &VarName,
    scalar_bases: &HashSet<VarName>,
) -> Option<Vec<i64>> {
    if !scalar_bases.contains(target) {
        return None;
    }
    let dims = algorithm_variable_dims(dae, target)?;
    (array_scalar_count(&dims)? > 1).then_some(dims)
}

fn array_scalar_count(dims: &[i64]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    dims.iter().try_fold(1usize, |count, dim| {
        let dim = usize::try_from(*dim).ok()?;
        (dim > 0).then_some(count.checked_mul(dim)?)
    })
}

fn expand_array_assignment(
    target: &VarName,
    dims: &[i64],
    assignment: &AlgorithmAssignment,
) -> Result<Vec<AlgorithmAssignment>, String> {
    let (_, value, span, origin) = assignment;
    let Some(count) = array_scalar_count(dims) else {
        return Ok(vec![assignment.clone()]);
    };
    if let Ok(max_index) = usize::try_from(i64::MAX)
        && count > max_index
    {
        return Err(format!(
            "array assignment `{target}` has {count} scalar elements, which cannot be \
             represented as generated i64 indices at span {span:?}"
        ));
    }

    let mut scalar_assignments = Vec::with_capacity(count);
    for flat_index in 0..count {
        let scalar_name = VarName::new(dae::scalar_name_text_for_flat_index(
            target.as_str(),
            dims,
            flat_index,
        ));
        let generated_index = i64::try_from(flat_index + 1).map_err(|_| {
            format!(
                "array assignment `{target}` scalar index {} cannot be represented as i64 at \
                 span {span:?}",
                flat_index + 1
            )
        })?;
        let scalar_value = match scalar_array_value(value, dims, flat_index, *span)? {
            Some(value) => value,
            None => Expression::Index {
                base: Box::new(value.clone()),
                subscripts: vec![
                    Subscript::try_generated_index(
                        generated_index,
                        *span,
                        "overlapping array assignment fallback subscript",
                    )
                    .map_err(|err| err.to_string())?,
                ],
                span: *span,
            },
        };
        scalar_assignments.push((scalar_name, scalar_value, *span, origin.clone()));
    }
    Ok(scalar_assignments)
}

fn scalar_array_value(
    value: &Expression,
    dims: &[i64],
    flat_index: usize,
    assignment_span: Span,
) -> Result<Option<Expression>, String> {
    let Some(subscripts) = dae::flat_index_to_subscripts(dims, flat_index) else {
        return Ok(None);
    };
    let subscript_span = scalar_value_subscript_span(value, assignment_span);
    let mut flat_subscripts = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let index = i64::try_from(subscript).map_err(|_| {
            format!(
                "array assignment scalar value subscript {subscript} cannot be represented as \
                 i64 at span {subscript_span:?}"
            )
        })?;
        flat_subscripts.push(
            Subscript::try_generated_index(
                index,
                subscript_span,
                "overlapping array scalar value subscript",
            )
            .map_err(|err| err.to_string())?,
        );
    }
    Ok(Some(current_value_subscript_expr(value, &flat_subscripts)?))
}

fn scalar_value_subscript_span(value: &Expression, assignment_span: Span) -> Span {
    match value.span() {
        Some(span) => span,
        None => assignment_span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("overlapping_array_assignments_fixture.mo"),
            7,
            19,
        )
    }

    fn literal(value: f64) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span: test_span(),
        }
    }

    fn array(values: impl IntoIterator<Item = Expression>) -> Expression {
        Expression::Array {
            elements: values.into_iter().collect(),
            is_matrix: false,
            span: test_span(),
        }
    }

    fn make_assignment(target: &str, value: Expression) -> (VarName, AlgorithmAssignment) {
        let target = VarName::new(target);
        (
            target.clone(),
            (
                target,
                value,
                test_span(),
                "algorithm assignment".to_string(),
            ),
        )
    }

    fn array_dae() -> Dae {
        let mut dae = Dae::new();
        let name = VarName::new("x");
        let mut var = dae::Variable::new(
            flat_to_dae_var_name(&name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        var.dims = vec![2];
        dae.variables.discrete_valued.insert(var.name.clone(), var);
        dae
    }

    #[test]
    fn later_scalar_assignment_overrides_whole_array_component() {
        let dae = array_dae();
        let mut assignments = IndexMap::new();
        let (target, assignment) = make_assignment("x", array([literal(1.0), literal(2.0)]));
        assignments.insert(target, assignment);
        let (target, assignment) = make_assignment("x[1]", literal(5.0));
        assignments.insert(target, assignment);

        let collapsed = collapse_overlapping_array_assignments(&dae, assignments)
            .unwrap_or_else(|err| panic!("array assignments should collapse: {err}"));

        assert_eq!(collapsed.len(), 2);
        assert!(matches!(
            collapsed
                .get(&VarName::new("x[1]"))
                .map(|(_, value, _, _)| value),
            Some(Expression::Literal {
                value: Literal::Real(5.0),
                ..
            })
        ));
        assert!(matches!(
            collapsed
                .get(&VarName::new("x[2]"))
                .map(|(_, value, _, _)| value),
            Some(Expression::Literal {
                value: Literal::Real(2.0),
                ..
            })
        ));
    }

    #[test]
    fn later_whole_array_assignment_overrides_earlier_scalar_component() {
        let dae = array_dae();
        let mut assignments = IndexMap::new();
        let (target, assignment) = make_assignment("x[1]", literal(5.0));
        assignments.insert(target, assignment);
        let (target, assignment) = make_assignment("x", array([literal(1.0), literal(2.0)]));
        assignments.insert(target, assignment);

        let collapsed = collapse_overlapping_array_assignments(&dae, assignments)
            .unwrap_or_else(|err| panic!("array assignments should collapse: {err}"));

        assert_eq!(collapsed.len(), 2);
        assert!(matches!(
            collapsed
                .get(&VarName::new("x[1]"))
                .map(|(_, value, _, _)| value),
            Some(Expression::Literal {
                value: Literal::Real(1.0),
                ..
            })
        ));
        assert!(matches!(
            collapsed
                .get(&VarName::new("x[2]"))
                .map(|(_, value, _, _)| value),
            Some(Expression::Literal {
                value: Literal::Real(2.0),
                ..
            })
        ));
    }
}
