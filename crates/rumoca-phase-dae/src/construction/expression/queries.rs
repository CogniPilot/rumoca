//! Read-only expression traversal, provenance and variable-attribute queries.

use super::*;

pub(in crate::construction) fn planned_input_variability(
    variable: &flat::Variable,
) -> dae::InputVariability {
    if matches!(variable.variability, Variability::Discrete(_)) || variable.is_discrete_type {
        dae::InputVariability::Discrete
    } else {
        dae::InputVariability::Continuous
    }
}

pub(in crate::construction) fn expression_span(
    expression: &Expression,
) -> Result<Span, ToDaeError> {
    expression
        .span()
        .ok_or_else(|| ToDaeError::MissingProvenance {
            owner: format!("{expression:?}"),
        })
}

pub(in crate::construction) fn require_span(
    span: Span,
    owner: impl Into<String>,
) -> Result<(), ToDaeError> {
    if span.is_dummy() {
        return Err(ToDaeError::MissingProvenance {
            owner: owner.into(),
        });
    }
    Ok(())
}

pub(in crate::construction) fn variable_attribute_expressions(
    variable: &flat::Variable,
) -> impl Iterator<Item = &Expression> {
    [
        variable.start.as_ref(),
        variable.min.as_ref(),
        variable.max.as_ref(),
        variable.nominal.as_ref(),
        variable.binding.as_ref(),
    ]
    .into_iter()
    .flatten()
}

pub(in crate::construction) fn all_model_expressions(
    flat: &flat::Model,
) -> impl Iterator<Item = &Expression> {
    flat.variables
        .values()
        .flat_map(variable_attribute_expressions)
        .chain(flat.equations.iter().map(|equation| &equation.residual))
        .chain(
            flat.initial_equations
                .iter()
                .map(|equation| &equation.residual),
        )
}

pub(in crate::construction) fn expression_children(expression: &Expression) -> Vec<&Expression> {
    match expression {
        Expression::Binary { lhs, rhs, .. } => vec![lhs, rhs],
        Expression::Unary { rhs, .. } => vec![rhs],
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().collect()
        }
        Expression::StringConversion { value, format, .. } => std::iter::once(value.as_ref())
            .chain(format.operands())
            .collect(),
        Expression::If {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .flat_map(|(condition, value)| [condition, value])
            .chain(std::iter::once(else_branch.as_ref()))
            .collect(),
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            elements.iter().collect()
        }
        Expression::Range {
            start, step, end, ..
        } => std::iter::once(start.as_ref())
            .chain(step.as_deref())
            .chain(std::iter::once(end.as_ref()))
            .collect(),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => std::iter::once(expr.as_ref())
            .chain(indices.iter().map(|index| &index.range))
            .chain(filter.as_deref())
            .collect(),
        Expression::Index {
            base, subscripts, ..
        } => std::iter::once(base.as_ref())
            .chain(subscripts.iter().filter_map(subscript_expression))
            .collect(),
        Expression::VarRef { subscripts, .. } => {
            subscripts.iter().filter_map(subscript_expression).collect()
        }
        Expression::FieldAccess { base, .. } => vec![base],
        Expression::Literal { .. } | Expression::Empty { .. } => Vec::new(),
    }
}
