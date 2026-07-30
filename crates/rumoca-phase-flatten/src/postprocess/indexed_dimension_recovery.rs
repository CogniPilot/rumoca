//! Recover array dimensions that only the flattened equations still witness.
//!
//! A component declared with a shape that instantiation could not evaluate
//! reaches Flat as a scalar, yet the equations it produced still index it (or
//! equate it whole against a known array). The subscripts and residual scalar
//! counts are the surviving evidence of the declared shape, so they are folded
//! back onto the variable before shape-sensitive passes run.

use super::*;

pub(crate) fn recover_indexed_lhs_dimensions(flat: &mut flat::Model) {
    let mut recovered: Vec<(rumoca_core::VarName, Vec<i64>)> = Vec::new();
    for equation in &flat.equations {
        let Some((name, dims)) = indexed_lhs_dimensions(&equation.residual) else {
            continue;
        };
        merge_recovered_dims(&mut recovered, name, dims);
    }
    recover_whole_var_equality_dimensions(flat, &mut recovered);

    for (name, dims) in recovered {
        let Some(var) = flat.variables.get_mut(&name) else {
            continue;
        };
        if should_replace_dims(&var.dims, &dims) {
            var.dims = dims;
        }
        if matches!(
            &var.variability,
            rumoca_core::Variability::Parameter(token) if token.text.is_empty()
        ) && var.binding.is_none()
        {
            var.variability = rumoca_core::Variability::Empty;
        }
    }
}

fn indexed_lhs_dimensions(
    residual: &rumoca_core::Expression,
) -> Option<(rumoca_core::VarName, Vec<i64>)> {
    let rumoca_core::Expression::Binary { op, lhs, .. } = residual else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    let (name, subscripts) = indexed_var_ref(lhs.as_ref())?;
    let dims = subscript_upper_bounds(subscripts)?;
    Some((name.var_name().clone(), dims))
}

fn indexed_var_ref(
    expr: &rumoca_core::Expression,
) -> Option<(&rumoca_core::Reference, &[rumoca_core::Subscript])> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if !subscripts.is_empty() => Some((name, subscripts)),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef { name, .. } = base.as_ref() else {
                return None;
            };
            Some((name, subscripts))
        }
        _ => None,
    }
}

fn subscript_upper_bounds(subscripts: &[rumoca_core::Subscript]) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let value = match subscript {
            rumoca_core::Subscript::Index { value, .. } => *value,
            rumoca_core::Subscript::Expr { expr, .. } => constant_integer_bound(expr)?,
            rumoca_core::Subscript::Colon { .. } => return None,
        };
        if value <= 0 {
            return None;
        }
        dims.push(value);
    }
    Some(dims)
}

/// Compile-time integer bound of a declared or subscripted extent.
pub(super) fn constant_integer_bound(expr: &rumoca_core::Expression) -> Option<i64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            ..
        } => size_call_bound(args),
        _ => None,
    }
}

fn size_call_bound(args: &[rumoca_core::Expression]) -> Option<i64> {
    let [array, dim] = args else {
        return None;
    };
    if constant_integer_bound(dim)? != 1 {
        return None;
    }
    let rumoca_core::Expression::Array { elements, .. } = array else {
        return None;
    };
    i64::try_from(elements.len()).ok()
}

fn recover_whole_var_equality_dimensions(
    flat: &flat::Model,
    recovered: &mut Vec<(rumoca_core::VarName, Vec<i64>)>,
) {
    for equation in &flat.equations {
        let Some((lhs, rhs)) = whole_var_equality(&equation.residual) else {
            continue;
        };
        let scalar_count = i64::try_from(equation.scalar_count).ok();
        if let Some(dim) = scalar_count.filter(|dim| *dim > 1)
            && whole_var_can_accept_1d_recovery(flat, lhs, rhs)
        {
            merge_recovered_dims(recovered, lhs.clone(), vec![dim]);
            merge_recovered_dims(recovered, rhs.clone(), vec![dim]);
        }
    }
}

fn whole_var_equality(
    residual: &rumoca_core::Expression,
) -> Option<(&rumoca_core::VarName, &rumoca_core::VarName)> {
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = residual else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    let lhs = whole_var_ref(lhs.as_ref())?;
    let rhs = whole_var_ref(rhs.as_ref())?;
    Some((lhs, rhs))
}

fn whole_var_ref(expr: &rumoca_core::Expression) -> Option<&rumoca_core::VarName> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    subscripts.is_empty().then(|| name.var_name())
}

fn whole_var_can_accept_1d_recovery(
    flat: &flat::Model,
    lhs: &rumoca_core::VarName,
    rhs: &rumoca_core::VarName,
) -> bool {
    [lhs, rhs].into_iter().all(|name| {
        flat.variables
            .get(name)
            .is_some_and(|var| var.dims.len() <= 1)
    })
}

fn merge_recovered_dims(
    recovered: &mut Vec<(rumoca_core::VarName, Vec<i64>)>,
    name: rumoca_core::VarName,
    dims: Vec<i64>,
) {
    if let Some((_, existing)) = recovered
        .iter_mut()
        .find(|(candidate, _)| candidate == &name)
    {
        for (index, dim) in dims.into_iter().enumerate() {
            if index >= existing.len() {
                existing.push(dim);
            } else {
                existing[index] = existing[index].max(dim);
            }
        }
        return;
    }
    recovered.push((name, dims));
}

fn should_replace_dims(current: &[i64], recovered: &[i64]) -> bool {
    if recovered.is_empty() {
        return false;
    }
    current.len() < recovered.len()
        || current
            .iter()
            .zip(recovered.iter())
            .any(|(current, recovered)| *recovered > *current)
}
