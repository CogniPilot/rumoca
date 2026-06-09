use super::{BuiltinFunction, Dae, Expression, StructuralError, VarName};
use crate::variable_scope::{DaeVariableScope, scalar_count_from_dims};

#[cfg(test)]
use super::{Subscript, Variable};

pub(super) fn dae_variable_size(
    dae: &Dae,
    name: &VarName,
) -> Result<Option<usize>, StructuralError> {
    DaeVariableScope::new(dae)
        .exact(name)
        .map(|var| var.dims.clone())
        .map(|dims| scalar_count_from_dims(name, &dims))
        .transpose()
}

pub(super) fn required_dae_variable_size(
    dae: &Dae,
    name: &VarName,
) -> Result<usize, StructuralError> {
    DaeVariableScope::new(dae).size(name)
}

pub(super) fn residual_scalar_width(
    dae: &Dae,
    expr: &Expression,
) -> Result<usize, StructuralError> {
    match expression_dims_for_row_count(dae, expr)? {
        Some(dims) => scalar_count_from_dims(&VarName::new("<residual>"), &dims),
        None => Ok(1),
    }
}

fn expression_dims_for_row_count(
    dae: &Dae,
    expr: &Expression,
) -> Result<Option<Vec<i64>>, StructuralError> {
    let scope = DaeVariableScope::new(dae);
    Ok(match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => scope
            .dims_for_reference(name)?
            .map(|dims| dims_after_subscript_count(dims, subscripts.len())),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } => match args.first() {
            Some(arg) => expression_dims_for_row_count(dae, arg)?,
            None => None,
        },
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => array_dims_for_row_count(elements, *is_matrix),
        Expression::Binary { lhs, rhs, .. } => expression_dims_for_row_count(dae, lhs)?
            .filter(|dims| !dims.is_empty())
            .or(expression_dims_for_row_count(dae, rhs)?.filter(|dims| !dims.is_empty())),
        Expression::Unary { rhs, .. } => expression_dims_for_row_count(dae, rhs)?,
        Expression::Index {
            base, subscripts, ..
        } => expression_dims_for_row_count(dae, base)?
            .map(|dims| dims_after_subscript_count(dims, subscripts.len())),
        Expression::FieldAccess { base, field, .. } => {
            field_access_dims_for_row_count(dae, base, field)?
        }
        _ => None,
    })
}

fn field_access_dims_for_row_count(
    dae: &Dae,
    base: &Expression,
    field: &str,
) -> Result<Option<Vec<i64>>, StructuralError> {
    if let Some(dims) = DaeVariableScope::new(dae).indexed_component_field_dims(base, field) {
        return Ok(Some(dims));
    }
    expression_dims_for_row_count(dae, base)
}

fn dims_after_subscript_count(dims: Vec<i64>, n_subscripts: usize) -> Vec<i64> {
    if n_subscripts >= dims.len() {
        Vec::new()
    } else {
        dims.into_iter().skip(n_subscripts).collect()
    }
}

fn array_dims_for_row_count(elements: &[Expression], is_matrix: bool) -> Option<Vec<i64>> {
    if !is_matrix {
        return Some(vec![elements.len() as i64]);
    }
    let cols = match elements.first()? {
        Expression::Array { elements: row, .. } => row.len(),
        _ => return Some(vec![elements.len() as i64]),
    };
    Some(vec![elements.len() as i64, cols as i64])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn part(ident: &str, subs: Vec<Subscript>) -> rumoca_core::ComponentRefPart {
        rumoca_core::ComponentRefPart {
            ident: ident.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs,
        }
    }

    fn component_ref(parts: Vec<rumoca_core::ComponentRefPart>) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts,
            def_id: None,
        }
    }

    fn reference(parts: Vec<rumoca_core::ComponentRefPart>) -> rumoca_core::Reference {
        rumoca_core::Reference::from_component_reference(component_ref(parts))
    }

    fn pin_field_var(index: i64, field: &str) -> Variable {
        let mut var = Variable::new(VarName::new(format!("inverter.ac.pin[{index}].{field}")));
        var.component_ref = Some(component_ref(vec![
            part("inverter", vec![]),
            part("ac", vec![]),
            part(
                "pin",
                vec![Subscript::generated_index(index, rumoca_core::Span::DUMMY)],
            ),
            part(field, vec![]),
        ]));
        var
    }

    #[test]
    fn row_shape_counts_indexed_connector_array_field_access() {
        let mut dae = Dae::default();
        for index in 1..=3 {
            let var = pin_field_var(index, "v");
            dae.variables.algebraics.insert(var.name.clone(), var);
        }
        let expr = Expression::FieldAccess {
            base: Box::new(Expression::Index {
                base: Box::new(Expression::VarRef {
                    name: reference(vec![
                        part("inverter", vec![]),
                        part("ac", vec![]),
                        part("pin", vec![]),
                    ]),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                subscripts: vec![Subscript::generated_colon(rumoca_core::Span::DUMMY)],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "v".to_string(),
            span: rumoca_core::Span::DUMMY,
        };

        assert_eq!(residual_scalar_width(&dae, &expr).unwrap(), 3);
    }
}
