use rumoca_core::{ComponentReference, Expression, Reference, Subscript, VarName};
use rumoca_ir_dae as dae;

use crate::StructuralError;
use crate::variable_scope::{DaeVariableScope, scalar_count_from_dims};

use super::exact_reference_expr_name_in_dae;

pub(super) fn expression_is_scalar_after_subscripts(
    expr: &Expression,
    dae: &dae::Dae,
) -> Result<bool, StructuralError> {
    if let Some(exact_name) = exact_reference_expr_name_in_dae(dae, expr)
        && let Some(var) = DaeVariableScope::new(dae).exact(&exact_name)
    {
        return Ok(scalar_count_from_dims(&exact_name, &var.dims)? == 1);
    }
    match expr {
        Expression::Literal { .. } | Expression::Empty { .. } => Ok(true),
        Expression::VarRef {
            name,
            subscripts,
            span,
        } => var_ref_is_scalar_after_subscripts(name, subscripts, *span, dae),
        Expression::Unary { rhs, .. } => expression_is_scalar_after_subscripts(rhs, dae),
        Expression::Binary { lhs, rhs, .. } => Ok(expression_is_scalar_after_subscripts(lhs, dae)?
            && expression_is_scalar_after_subscripts(rhs, dae)?),
        Expression::BuiltinCall { args, .. } => args
            .iter()
            .map(|arg| expression_is_scalar_after_subscripts(arg, dae))
            .try_fold(true, |all_scalar, arg_scalar| Ok(all_scalar && arg_scalar?)),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let branches_scalar = branches
                .iter()
                .map(|(_, value)| expression_is_scalar_after_subscripts(value, dae))
                .try_fold(true, |all_scalar, value_scalar| {
                    Ok(all_scalar && value_scalar?)
                })?;
            Ok(branches_scalar && expression_is_scalar_after_subscripts(else_branch, dae)?)
        }
        Expression::FunctionCall { .. }
        | Expression::Array { .. }
        | Expression::Tuple { .. }
        | Expression::Range { .. }
        | Expression::ArrayComprehension { .. }
        | Expression::Index { .. }
        | Expression::FieldAccess { .. } => Ok(false),
    }
}

pub(super) fn var_ref_is_scalar_after_subscripts(
    name: &Reference,
    subscripts: &[Subscript],
    reference_span: rumoca_core::Span,
    dae: &dae::Dae,
) -> Result<bool, StructuralError> {
    let scope = DaeVariableScope::new(dae);
    if !subscripts.is_empty() {
        let expr = Expression::VarRef {
            name: name.clone(),
            subscripts: subscripts.to_vec(),
            span: reference_span,
        };
        if let Some(exact_name) = exact_reference_expr_name_in_dae(dae, &expr)
            && let Some(var) = DaeVariableScope::new(dae).exact(&exact_name)
        {
            return Ok(scalar_count_from_dims(&exact_name, &var.dims)? == 1);
        }
    }
    let dims = match scope.dims_for_reference(name) {
        Ok(Some(dims)) => dims,
        Ok(None) => return Ok(false),
        Err(StructuralError::ContractViolation { reason, .. })
        | Err(StructuralError::UnspannedContractViolation { reason })
            if reason.contains("missing DAE variable metadata") =>
        {
            return Ok(false);
        }
        Err(err) => return Err(err),
    };
    let subscript_offset = if subscripts.len() > dims.len() {
        component_scalar_selection_count(name).min(subscripts.len())
    } else {
        0
    };
    let remaining_dims = dims_after_subscripts(
        name.var_name(),
        &dims,
        &subscripts[subscript_offset..],
        reference_span,
    )?;
    Ok(scalar_count_from_dims(name.var_name(), &remaining_dims)? == 1)
}

fn component_scalar_selection_count(name: &Reference) -> usize {
    name.component_ref()
        .map(component_ref_scalar_selection_count)
        .unwrap_or(0)
}

fn component_ref_scalar_selection_count(component_ref: &ComponentReference) -> usize {
    component_ref
        .parts
        .iter()
        .flat_map(|part| &part.subs)
        .filter(|subscript| positive_subscript_index(subscript).is_some())
        .count()
}

fn positive_subscript_index(subscript: &Subscript) -> Option<i64> {
    match subscript {
        Subscript::Index { value, .. } if *value > 0 => Some(*value),
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } if *value > 0 => Some(*value),
            Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } if value.is_finite() && value.fract() == 0.0 && *value > 0.0 => Some(*value as i64),
            _ => None,
        },
        Subscript::Colon { .. } => None,
        _ => None,
    }
}

fn dims_after_subscripts(
    name: &VarName,
    dims: &[i64],
    subscripts: &[Subscript],
    reference_span: rumoca_core::Span,
) -> Result<Vec<i64>, StructuralError> {
    if subscripts.len() > dims.len() {
        return Err(StructuralError::ContractViolation {
            reason: format!(
                "indexed DAE reference `{}` has {} subscripts for dimensions {:?}",
                name.as_str(),
                subscripts.len(),
                dims
            ),
            span: subscripts
                .first()
                .map(Subscript::span)
                .filter(|span| !span.is_dummy())
                .unwrap_or(reference_span),
        });
    }
    let mut remaining = Vec::new();
    for (index, dim) in subscripts.iter().zip(dims) {
        match index {
            Subscript::Index { value, span } => {
                if *value < 1 || *value > *dim {
                    return Err(StructuralError::ContractViolation {
                        reason: format!(
                            "indexed DAE reference `{}` subscript {} is outside dimension {}",
                            name.as_str(),
                            value,
                            dim
                        ),
                        span: *span,
                    });
                }
            }
            Subscript::Colon { .. } => remaining.push(*dim),
            Subscript::Expr { .. } => return Ok(dims.to_vec()),
        }
    }
    remaining.extend_from_slice(&dims[subscripts.len()..]);
    Ok(remaining)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ComponentRefPart, ComponentReference, Literal, Span};

    fn index(value: i64) -> Subscript {
        Subscript::Index {
            value,
            span: Span::DUMMY,
        }
    }

    fn component_ref(parts: Vec<ComponentRefPart>) -> ComponentReference {
        ComponentReference {
            local: false,
            span: Span::DUMMY,
            parts,
            def_id: None,
        }
    }

    fn part(ident: &str, subs: Vec<Subscript>) -> ComponentRefPart {
        ComponentRefPart {
            ident: ident.to_string(),
            span: Span::DUMMY,
            subs,
        }
    }

    #[test]
    fn component_selected_scalar_reference_consumes_outer_projection_subscripts() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            VarName::new("machine.inertia[1,1].w"),
            dae::Variable {
                name: VarName::new("machine.inertia[1,1].w"),
                dims: Vec::new(),
                component_ref: Some(component_ref(vec![
                    part("machine", Vec::new()),
                    part("inertia", vec![index(1), index(1)]),
                    part("w", Vec::new()),
                ])),
                ..dae::Variable::empty_with_span(Span::DUMMY)
            },
        );
        let reference = Reference::from_component_reference(component_ref(vec![
            part("machine", Vec::new()),
            part("inertia", vec![index(1), index(1)]),
            part("w", Vec::new()),
        ]));

        assert!(
            var_ref_is_scalar_after_subscripts(
                &reference,
                &[index(1), index(1)],
                Span::DUMMY,
                &dae_model
            )
            .expect("component-selected scalar should remain scalar")
        );
    }

    #[test]
    fn plain_scalar_reference_still_rejects_extra_subscripts() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            VarName::new("x"),
            dae::Variable {
                name: VarName::new("x"),
                dims: Vec::new(),
                ..dae::Variable::empty_with_span(Span::DUMMY)
            },
        );
        let reference = Reference::from_var_name(VarName::new("x"));

        assert!(matches!(
            var_ref_is_scalar_after_subscripts(&reference, &[index(1)], Span::DUMMY, &dae_model),
            Err(StructuralError::ContractViolation { reason, .. })
                if reason.contains("indexed DAE reference `x` has 1 subscripts for dimensions []")
        ));
    }

    #[test]
    fn real_literal_component_subscript_counts_as_scalar_selection() {
        let reference = Reference::from_component_reference(component_ref(vec![part(
            "x",
            vec![Subscript::Expr {
                expr: Box::new(Expression::Literal {
                    value: Literal::Real(1.0),
                    span: Span::DUMMY,
                }),
                span: Span::DUMMY,
            }],
        )]));

        assert_eq!(component_scalar_selection_count(&reference), 1);
    }
}
