use super::*;

pub(super) struct ModelRoles {
    pub(super) states: HashSet<VarName>,
    pub(super) variables: HashMap<VarName, PlannedRole>,
    pub(super) expressions: HashMap<VarName, PlannedRole>,
}

pub(super) fn analyze_model_roles(
    flat: &flat::Model,
    sampled_values: &HashMap<InstanceId, Span>,
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
    let mut assigned_discrete = event_targets(flat);
    assigned_discrete.extend(
        flat.variables
            .iter()
            .filter(|(_, variable)| sampled_values.contains_key(&variable.instance_id))
            .map(|(name, _)| name.clone()),
    );
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

fn collect_derivative_targets(
    expression: &Expression,
    states: &mut HashSet<VarName>,
) -> Result<(), ToDaeError> {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args,
        span,
    } = expression
    {
        require_span(*span, "derivative expression")?;
        let [argument] = args.as_slice() else {
            return Err(ToDaeError::unsupported_flat(
                "derivative expression",
                "der(...) must have exactly one resolved variable-reference operand",
                *span,
            ));
        };
        let Some((name, _)) = derivative_reference(argument) else {
            return Err(ToDaeError::unsupported_flat(
                "derivative expression",
                "der(...) must have exactly one resolved variable-reference operand",
                *span,
            ));
        };
        states.insert(name.var_name().clone());
    }
    for child in expression_children(expression) {
        collect_derivative_targets(child, states)?;
    }
    Ok(())
}

fn validate_variable(
    flat: &flat::Model,
    name: &VarName,
    variable: &flat::Variable,
    states: &HashSet<VarName>,
    assigned_discrete: &HashSet<VarName>,
) -> Result<PlannedRole, ToDaeError> {
    let external_input = is_external_input(flat, name, variable)?;
    if is_predefined_clock_variable(flat, variable)? {
        require_span(variable.source_span, format!("clock declaration `{name}`"))?;
        if !variable.dims.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "clock ownership proof",
                format!("clock coordinate `{name}` must be scalar"),
                variable.source_span,
            ));
        }
        return Ok(PlannedRole::Clock);
    }
    let scalar_type = validate_variable_header(flat, name, variable)?;
    let role = classify_variable_role(
        name,
        variable,
        states,
        assigned_discrete,
        scalar_type,
        external_input,
    );
    validate_variable_role(name, variable, role, scalar_type)?;
    Ok(role)
}

pub(super) fn is_predefined_clock_variable(
    flat: &flat::Model,
    variable: &flat::Variable,
) -> Result<bool, ToDaeError> {
    let effective = flat.effective_types.get(&variable.type_id).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "effective type identity",
            format!(
                "coordinate `{}` has no exact effective type metadata",
                variable.name
            ),
            variable.source_span,
        )
    })?;
    Ok(effective.canonical_type() == flat.predefined_types.clock)
}

fn validate_variable_header(
    flat: &flat::Model,
    name: &VarName,
    variable: &flat::Variable,
) -> Result<dae::ScalarType, ToDaeError> {
    require_span(
        variable.source_span,
        format!("variable declaration `{name}`"),
    )?;
    let scalar_type = effective_variable_scalar_type(flat, variable).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "non-primitive coordinate",
            format!(
                "`{name}` has unsupported exact canonical type `{}`",
                variable.type_id
            ),
            variable.source_span,
        )
    })?;
    if !variable.is_primitive {
        return Err(ToDaeError::unsupported_flat(
            "non-primitive coordinate",
            format!("`{name}` must be expanded or enter a typed aggregate arena"),
            variable.source_span,
        ));
    }
    if variable.type_id.is_unknown() {
        return Err(ToDaeError::unsupported_flat(
            "effective variable type",
            format!("`{name}` retains TypeId::UNKNOWN"),
            variable.source_span,
        ));
    }
    for &extent in &variable.dims {
        if u32::try_from(extent).is_err() {
            return Err(ToDaeError::unsupported_flat(
                "array extent",
                format!("`{name}` has an extent outside the DAE u32 domain"),
                variable.source_span,
            ));
        }
    }
    Ok(scalar_type)
}

fn classify_variable_role(
    name: &VarName,
    variable: &flat::Variable,
    states: &HashSet<VarName>,
    assigned_discrete: &HashSet<VarName>,
    scalar_type: dae::ScalarType,
    external_input: bool,
) -> PlannedRole {
    if external_input {
        PlannedRole::Input
    } else if matches!(variable.variability, Variability::Constant(_)) {
        PlannedRole::Constant
    } else if matches!(variable.variability, Variability::Parameter(_)) {
        PlannedRole::Parameter
    } else if states.contains(name) {
        PlannedRole::State
    } else if assigned_discrete.contains(name)
        || matches!(variable.variability, Variability::Discrete(_))
        || variable.is_discrete_type
    {
        if scalar_type == dae::ScalarType::Real {
            PlannedRole::DiscreteReal
        } else {
            PlannedRole::DiscreteValue
        }
    } else if matches!(variable.causality, Causality::Output(_)) {
        PlannedRole::Output
    } else {
        PlannedRole::Algebraic
    }
}

fn is_external_input(
    flat: &flat::Model,
    name: &VarName,
    variable: &flat::Variable,
) -> Result<bool, ToDaeError> {
    if !matches!(variable.causality, Causality::Input(_)) {
        return Ok(false);
    }
    let root = variable
        .component_ref
        .as_ref()
        .and_then(|reference| reference.parts().first())
        .map(|part| part.ident.as_str())
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "input ownership",
                format!("input `{name}` has no resolved component-reference root"),
                variable.source_span,
            )
        })?;
    Ok(flat.top_level_input_components.contains(root) || flat.top_level_connectors.contains(root))
}

fn validate_variable_role(
    name: &VarName,
    variable: &flat::Variable,
    role: PlannedRole,
    scalar_type: dae::ScalarType,
) -> Result<(), ToDaeError> {
    if matches!(
        role,
        PlannedRole::State | PlannedRole::Algebraic | PlannedRole::Output
    ) && scalar_type != dae::ScalarType::Real
    {
        return Err(ToDaeError::unsupported_flat(
            "continuous non-Real coordinate",
            format!("`{name}` must be classified as a discrete value"),
            variable.source_span,
        ));
    }
    if matches!(role, PlannedRole::State)
        && !matches!(
            variable.variability,
            Variability::Empty | Variability::Continuous(_)
        )
    {
        return Err(ToDaeError::unsupported_flat(
            "derivative target",
            format!("`{name}` is not a continuous Real coordinate"),
            variable.source_span,
        ));
    }
    if matches!(role, PlannedRole::DiscreteReal)
        && !variable.dims.is_empty()
        && variable.binding.is_some()
    {
        return Err(ToDaeError::unsupported_flat(
            "array discrete-Real definition",
            "B.1b structured families are not yet part of the canonical DAE grammar",
            variable.source_span,
        ));
    }
    Ok(())
}
