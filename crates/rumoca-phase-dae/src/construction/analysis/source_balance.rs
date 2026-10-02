use super::*;

pub(super) struct SourceBalanceInput<'scope> {
    pub(super) flat: &'scope flat::Model,
    pub(super) roles: &'scope HashMap<VarName, PlannedRole>,
    pub(super) assigned_targets: &'scope HashSet<VarName>,
    pub(super) excluded_equation_rows: &'scope HashSet<usize>,
    pub(super) record_equations: &'scope HashMap<usize, RecordEquationPlan>,
    pub(super) multi_output_equations: &'scope HashMap<usize, MultiOutputEquationPlan>,
    pub(super) connection_ranks: &'scope HashMap<VarName, usize>,
    pub(super) aggregate_connections: &'scope AggregateDiscreteConnections,
}

pub(super) fn source_balance(input: SourceBalanceInput<'_>) -> Result<BalanceDetail, ToDaeError> {
    let SourceBalanceInput {
        flat,
        roles,
        assigned_targets,
        excluded_equation_rows,
        record_equations,
        multi_output_equations,
        connection_ranks,
        aggregate_connections,
    } = input;
    let mut detail = BalanceDetail::default();
    for (name, role) in roles {
        let variable = &flat.variables[name];
        let scalar_count = checked_shape_size(name, variable)?;
        match role {
            PlannedRole::UnusedExpandable
            | PlannedRole::Parameter
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
        if excluded_equation_rows.contains(&row) {
            continue;
        }
        match equation_partition(
            flat,
            row,
            equation,
            roles,
            connection_ranks,
            aggregate_connections,
        )? {
            EquationPartition::Continuous => {
                if let Some(plan) = record_equations.get(&row) {
                    add_record_equation_counts(&mut detail, flat, equation, plan, roles)?;
                    continue;
                }
                detail.continuous_equations += if let Some(plan) = multi_output_equations.get(&row)
                {
                    multi_output_equation_scalar_count(flat, equation, plan)?
                } else {
                    equation.scalar_count
                };
            }
            EquationPartition::DiscreteReal { .. } => {
                detail.discrete_real_equations += equation.scalar_count;
            }
            EquationPartition::DiscreteValue(plan) => {
                detail.discrete_value_definitions +=
                    plan.scalar_count.unwrap_or(equation.scalar_count);
            }
            EquationPartition::DiscreteElements(plans) => {
                detail.discrete_value_definitions += plans.len();
            }
            EquationPartition::MultiOutput { receivers, .. } => {
                add_multi_output_receivers(&mut detail, flat, roles, &receivers)?;
            }
            EquationPartition::ConsumedDiscreteValue => {}
        }
    }
    add_binding_counts(&mut detail, flat, roles)?;
    for target in when_chain_targets(flat) {
        add_algorithm_target(&mut detail, flat, roles, &target)?;
    }
    for algorithm in &flat.algorithms {
        for target in model_algorithm_targets(flat, algorithm) {
            add_algorithm_target(&mut detail, flat, roles, &target)?;
        }
    }
    Ok(detail)
}

/// Each receiver of a multi-result equation is defined by that equation in
/// the system its role selects.
fn add_multi_output_receivers(
    detail: &mut BalanceDetail,
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    receivers: &[&VarName],
) -> Result<(), ToDaeError> {
    for receiver in receivers {
        let scalar_count = checked_shape_size(receiver, &flat.variables[*receiver])?;
        match roles[*receiver] {
            PlannedRole::DiscreteReal => detail.discrete_real_equations += scalar_count,
            PlannedRole::DiscreteValue => detail.discrete_value_definitions += scalar_count,
            _ => detail.continuous_equations += scalar_count,
        }
    }
    Ok(())
}

fn multi_output_equation_scalar_count(
    flat: &flat::Model,
    equation: &flat::Equation,
    plan: &MultiOutputEquationPlan,
) -> Result<usize, ToDaeError> {
    plan.outputs
        .iter()
        .flatten()
        .try_fold(0usize, |count, target| {
            count
                .checked_add(checked_shape_size(target, &flat.variables[target])?)
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "multi-output equation",
                        "receiving tuple scalar count overflowed",
                        equation.span,
                    )
                })
        })
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
        PlannedRole::DiscreteValue => detail.discrete_value_definitions += scalar_count,
        PlannedRole::UnusedExpandable
        | PlannedRole::Parameter
        | PlannedRole::Constant
        | PlannedRole::Input
        | PlannedRole::Clock
        | PlannedRole::EnumerationLiteral
        | PlannedRole::Aggregate => {}
    }
    Ok(())
}

/// Count each field equation of a record equation in the system its target
/// belongs to.
fn add_record_equation_counts(
    detail: &mut BalanceDetail,
    flat: &flat::Model,
    equation: &flat::Equation,
    plan: &RecordEquationPlan,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<(), ToDaeError> {
    for (field, system) in plan.field_systems(roles) {
        let size = checked_shape_size(&field.target, &flat.variables[&field.target])?;
        let count = match system {
            RecordFieldSystem::Continuous => &mut detail.continuous_equations,
            RecordFieldSystem::DiscreteReal => &mut detail.discrete_real_equations,
            RecordFieldSystem::DiscreteValue => &mut detail.discrete_value_definitions,
        };
        *count = count.checked_add(size).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record equation shape",
                "record field scalar count overflowed",
                equation.span,
            )
        })?;
    }
    Ok(())
}

fn checked_shape_size(name: &VarName, variable: &flat::Variable) -> Result<usize, ToDaeError> {
    variable.shape_size().map_err(|_| {
        ToDaeError::unsupported_flat(
            "variable shape",
            format!("`{name}` has a shape whose scalar cardinality cannot be represented"),
            variable.source_span,
        )
    })
}

/// Count the definitions variable bindings contribute (MLS §4.4.1).
fn add_binding_counts(
    detail: &mut BalanceDetail,
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<(), ToDaeError> {
    for (name, variable) in &flat.variables {
        if variable.binding.is_none() {
            continue;
        }
        let scalar_count = checked_shape_size(name, variable)?;
        match roles[name] {
            PlannedRole::UnusedExpandable
            | PlannedRole::Parameter
            | PlannedRole::Constant
            | PlannedRole::Input
            | PlannedRole::Clock => {}
            PlannedRole::State | PlannedRole::Algebraic | PlannedRole::Output => {
                detail.continuous_equations += scalar_count;
            }
            PlannedRole::DiscreteReal => detail.discrete_real_equations += scalar_count,
            PlannedRole::DiscreteValue => detail.discrete_value_definitions += scalar_count,
            PlannedRole::EnumerationLiteral | PlannedRole::Aggregate => {
                unreachable!("expression-only roles are not model variables")
            }
        }
    }
    Ok(())
}
