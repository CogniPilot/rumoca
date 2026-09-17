//! Recognize source equalities that define non-Real initialization parameters.

use super::*;

pub(in crate::construction) struct InitialParameterEquation {
    pub(in crate::construction) row: usize,
    pub(in crate::construction) target: VarName,
    pub(in crate::construction) value: Expression,
    pub(in crate::construction) span: Span,
}

pub(super) fn analyze(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<Vec<InitialParameterEquation>, ToDaeError> {
    let mut definitions = Vec::new();
    for (row, equation) in flat.initial_equations.iter().enumerate() {
        let Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } = &equation.residual
        else {
            continue;
        };
        let selected = if let Some(target) = target(flat, roles, lhs)? {
            Some((target, rhs.as_ref()))
        } else {
            target(flat, roles, rhs)?.map(|target| (target, lhs.as_ref()))
        };
        if let Some((target, value)) = selected {
            definitions.push(InitialParameterEquation {
                row,
                target,
                value: value.clone(),
                span: equation.span,
            });
        }
    }
    Ok(definitions)
}

fn target(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    expression: &Expression,
) -> Result<Option<VarName>, ToDaeError> {
    let Expression::VarRef {
        name,
        subscripts,
        span,
    } = expression
    else {
        return Ok(None);
    };
    if !subscripts.is_empty() || !matches!(roles.get(name.var_name()), Some(PlannedRole::Parameter))
    {
        return Ok(None);
    }
    let Some(variable) = flat.variables.get(name.var_name()) else {
        return Ok(None);
    };
    // This target is a parameter; its `fixed` is uniform (flatten refuses
    // non-uniform parameter arrays, EF033), so the reduction is exact.
    if variable.fixed_uniform() != Some(false) || variable.binding.is_some() {
        return Ok(None);
    }
    let Some(scalar) = effective_variable_scalar_type(flat, variable) else {
        return Ok(None);
    };
    if scalar == dae::ScalarType::Real {
        return Ok(None);
    }
    if name.instance_id() != Some(variable.instance_id) {
        return Err(ToDaeError::unsupported_flat(
            "initial parameter identity",
            "the defining reference must identify the exact parameter occurrence",
            *span,
        ));
    }
    Ok(Some(variable.name.clone()))
}
