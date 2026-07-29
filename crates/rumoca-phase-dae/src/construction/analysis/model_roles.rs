use super::*;

pub(super) struct ModelRoles {
    pub(super) states: HashSet<VarName>,
    pub(super) variables: HashMap<VarName, PlannedRole>,
    pub(super) expressions: HashMap<VarName, PlannedRole>,
}

pub(super) fn analyze_model_roles(
    flat: &flat::Model,
    sampled_values: &HashMap<VarName, SampledValuePlan>,
) -> Result<ModelRoles, ToDaeError> {
    let mut states = HashSet::new();
    for equation in flat.equations.iter().chain(&flat.initial_equations) {
        collect_derivative_targets(&equation.residual, &mut states)?;
    }
    for expression in flat
        .variables
        .values()
        .flat_map(variable_attribute_expressions)
    {
        collect_derivative_targets(expression, &mut states)?;
    }
    let mut assigned_discrete = event_and_algorithm_targets(flat);
    assigned_discrete.extend(sampled_values.keys().cloned());
    let roles = flat
        .variables
        .iter()
        .map(|(name, variable)| {
            validate_variable(flat, name, variable, &states, &assigned_discrete)
                .map(|role| (name.clone(), role))
        })
        .collect::<Result<HashMap<_, _>, _>>()?;
    let mut expression_roles = roles.clone();
    expression_roles.extend(
        flat.enum_literal_ordinals
            .keys()
            .map(|literal| (VarName::new(literal), PlannedRole::EnumerationLiteral)),
    );
    expression_roles.extend(
        flat.record_instances
            .keys()
            .cloned()
            .map(|name| (name, PlannedRole::Aggregate)),
    );
    Ok(ModelRoles {
        states,
        variables: roles,
        expressions: expression_roles,
    })
}
