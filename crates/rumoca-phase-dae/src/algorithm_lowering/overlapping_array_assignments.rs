use super::*;

pub(super) fn collapse_overlapping_array_assignments(
    dae: &Dae,
    assignments: IndexMap<VarName, AlgorithmAssignment>,
) -> IndexMap<VarName, AlgorithmAssignment> {
    let scalar_bases = scalar_assignment_bases(assignments.keys());
    let mut collapsed = IndexMap::with_capacity(assignments.len());

    for (target, assignment) in assignments {
        let Some(dims) = overlapping_array_dims(dae, &target, &scalar_bases) else {
            collapsed.insert(target, assignment);
            continue;
        };
        for scalar_assignment in expand_array_assignment(&target, &dims, &assignment) {
            collapsed.insert(scalar_assignment.0.clone(), scalar_assignment);
        }
    }

    collapsed
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
) -> Vec<AlgorithmAssignment> {
    let (_, value, span, origin) = assignment;
    let Some(count) = array_scalar_count(dims) else {
        return vec![assignment.clone()];
    };

    let mut scalar_assignments = Vec::with_capacity(count);
    for flat_index in 0..count {
        let scalar_name = VarName::new(dae::scalar_name_text_for_flat_index(
            target.as_str(),
            dims,
            flat_index,
        ));
        let scalar_value =
            scalar_array_value(value, dims, flat_index).unwrap_or_else(|| Expression::Index {
                base: Box::new(value.clone()),
                subscripts: vec![Subscript::generated_index(
                    i64::try_from(flat_index + 1)
                        .expect("array assignment flat index must fit in i64"),
                    *span,
                )],
                span: *span,
            });
        scalar_assignments.push((scalar_name, scalar_value, *span, origin.clone()));
    }
    scalar_assignments
}

fn scalar_array_value(value: &Expression, dims: &[i64], flat_index: usize) -> Option<Expression> {
    let subscripts = dae::flat_index_to_subscripts(dims, flat_index)?;
    let mut flat_subscripts = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        flat_subscripts.push(Subscript::generated_index(
            i64::try_from(subscript).ok()?,
            value.span().unwrap_or(Span::DUMMY),
        ));
    }
    Some(current_value_subscript_expr(value, &flat_subscripts))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn literal(value: f64) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span: Span::DUMMY,
        }
    }

    fn array(values: impl IntoIterator<Item = Expression>) -> Expression {
        Expression::Array {
            elements: values.into_iter().collect(),
            is_matrix: false,
            span: Span::DUMMY,
        }
    }

    fn make_assignment(target: &str, value: Expression) -> (VarName, AlgorithmAssignment) {
        let target = VarName::new(target);
        (
            target.clone(),
            (
                target,
                value,
                Span::DUMMY,
                "algorithm assignment".to_string(),
            ),
        )
    }

    fn array_dae() -> Dae {
        let mut dae = Dae::new();
        let name = VarName::new("x");
        let mut var = dae::Variable::new(flat_to_dae_var_name(&name));
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

        let collapsed = collapse_overlapping_array_assignments(&dae, assignments);

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

        let collapsed = collapse_overlapping_array_assignments(&dae, assignments);

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
