use super::*;

pub(super) fn source_balance(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    clock_equation_rows: &HashSet<usize>,
    record_equations: &HashMap<usize, RecordEquationPlan>,
) -> Result<BalanceDetail, ToDaeError> {
    let mut detail = BalanceDetail::default();
    let assigned_targets = defined_discrete_targets(flat, roles)?;
    for (name, role) in roles {
        let variable = &flat.variables[name];
        let scalar_count = checked_shape_size(name, variable)?;
        match role {
            PlannedRole::Parameter
            | PlannedRole::Constant
            | PlannedRole::Input
            | PlannedRole::Clock => {}
            PlannedRole::State => detail.state_unknowns += scalar_count,
            PlannedRole::Algebraic => detail.algebraic_unknowns += scalar_count,
            PlannedRole::Output => detail.output_unknowns += scalar_count,
            PlannedRole::DiscreteReal | PlannedRole::DiscreteValue
                if variable.binding.is_none() && !assigned_targets.contains(name) => {}
            PlannedRole::DiscreteReal => detail.discrete_real_unknowns += scalar_count,
            PlannedRole::DiscreteValue => detail.discrete_value_unknowns += scalar_count,
            PlannedRole::EnumerationLiteral | PlannedRole::Aggregate => {
                unreachable!("expression-only roles are not model variables")
            }
        }
    }
    for (row, equation) in flat.equations.iter().enumerate() {
        if clock_equation_rows.contains(&row) {
            continue;
        }
        match equation_partition(equation, roles)? {
            EquationPartition::Continuous => {
                detail.continuous_equations += if let Some(plan) = record_equations.get(&row) {
                    record_equation_scalar_count(flat, equation, plan)?
                } else {
                    equation.scalar_count
                };
            }
            EquationPartition::DiscreteReal { .. } => {
                detail.discrete_real_equations += equation.scalar_count;
            }
            EquationPartition::DiscreteValue { .. } => {
                detail.discrete_assignments += equation.scalar_count;
            }
        }
    }
    for (name, variable) in &flat.variables {
        if variable.binding.is_none() {
            continue;
        }
        let scalar_count = checked_shape_size(name, variable)?;
        match roles[name] {
            PlannedRole::Parameter
            | PlannedRole::Constant
            | PlannedRole::Input
            | PlannedRole::Clock => {}
            PlannedRole::State | PlannedRole::Algebraic | PlannedRole::Output => {
                detail.continuous_equations += scalar_count;
            }
            PlannedRole::DiscreteReal => detail.discrete_real_equations += scalar_count,
            PlannedRole::DiscreteValue => detail.discrete_assignments += scalar_count,
            PlannedRole::EnumerationLiteral | PlannedRole::Aggregate => {
                unreachable!("expression-only roles are not model variables")
            }
        }
    }
    for target in when_clause_targets(flat) {
        add_algorithm_target(&mut detail, flat, roles, &target)?;
    }
    for algorithm in &flat.algorithms {
        for target in model_algorithm_targets(flat, algorithm) {
            add_algorithm_target(&mut detail, flat, roles, &target)?;
        }
    }
    Ok(detail)
}

fn add_algorithm_target(
    detail: &mut BalanceDetail,
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    target: &VarName,
) -> Result<(), ToDaeError> {
    let scalar_count = checked_shape_size(target, &flat.variables[target])?;
    match roles[target] {
        PlannedRole::State | PlannedRole::Algebraic | PlannedRole::Output => {
            detail.continuous_equations += scalar_count;
        }
        PlannedRole::DiscreteReal => detail.discrete_real_equations += scalar_count,
        PlannedRole::DiscreteValue => detail.discrete_assignments += scalar_count,
        PlannedRole::Parameter
        | PlannedRole::Constant
        | PlannedRole::Input
        | PlannedRole::Clock
        | PlannedRole::EnumerationLiteral
        | PlannedRole::Aggregate => {}
    }
    Ok(())
}

fn record_equation_scalar_count(
    flat: &flat::Model,
    equation: &flat::Equation,
    plan: &RecordEquationPlan,
) -> Result<usize, ToDaeError> {
    plan.fields.iter().try_fold(0usize, |count, field| {
        count
            .checked_add(checked_shape_size(
                &field.coordinate,
                &flat.variables[&field.coordinate],
            )?)
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "record equation shape",
                    "record field scalar count overflowed",
                    equation.span,
                )
            })
    })
}
