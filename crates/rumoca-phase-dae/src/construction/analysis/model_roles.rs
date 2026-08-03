use super::*;

pub(super) struct ModelRoles {
    pub(super) states: HashSet<VarName>,
    pub(super) variables: HashMap<VarName, PlannedRole>,
    pub(super) expressions: HashMap<VarName, PlannedRole>,
}

pub(super) fn analyze_model_roles(
    flat: &flat::Model,
    sampled_values: &HashMap<InstanceId, SampledTarget>,
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
    collect_previous_operands(flat, &mut assigned_discrete);
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

/// MLS §16.5 Operator 16.5 `previous(u)`: `u` is a clocked discrete-time
/// variable.
///
/// A clocked partition written without a `when` clause (MLS §16.5.1 clock
/// inference) declares its state coordinates with no `discrete` prefix, so the
/// operator's own typing rule is the only exact evidence that they hold values
/// between clock ticks. `states` still wins in [`classify_variable_role`], so a
/// `der(...)` target passed to `previous(...)` keeps its continuous role and is
/// rejected by the expression validator rather than silently reclassified.
fn collect_previous_operands(flat: &flat::Model, discrete: &mut HashSet<VarName>) {
    for expression in all_model_expressions(flat) {
        collect_previous_operand_names(expression, discrete);
    }
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            collect_previous_operand_names(&branch.condition, discrete);
            collect_when_previous_operands(&branch.equations, discrete);
        }
    }
}

fn collect_when_previous_operands(
    equations: &[flat::WhenEquation],
    discrete: &mut HashSet<VarName>,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                collect_previous_operand_names(value, discrete);
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                collect_previous_operand_names(condition, discrete);
                collect_previous_operand_names(message, discrete);
                if let Some(level) = level {
                    collect_previous_operand_names(level, discrete);
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                collect_previous_operand_names(message, discrete);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, equations) in branches {
                    collect_previous_operand_names(condition, discrete);
                    collect_when_previous_operands(equations, discrete);
                }
                if let Some(equations) = else_branch {
                    collect_when_previous_operands(equations, discrete);
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                collect_previous_operand_names(function, discrete);
            }
        }
    }
}

fn collect_previous_operand_names(expression: &Expression, discrete: &mut HashSet<VarName>) {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Previous,
        args,
        ..
    } = expression
        && let [argument] = args.as_slice()
        && let Some((name, _)) = derivative_reference(argument)
    {
        discrete.insert(name.var_name().clone());
    }
    for child in expression_children(expression) {
        collect_previous_operand_names(child, discrete);
    }
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

/// MLS §16.5.1: every variable of a clocked partition is a clocked
/// discrete-time variable, whatever variability its declaration carries.
///
/// Partition membership is a property of the clock-inference fixed point, so it
/// is not known when [`analyze_model_roles`] plans the first role: a coordinate
/// that joins a clocked partition only through a connection — the
/// `connect(sample.y, assignClock.u)` shape of the `Modelica.Clocked` examples
/// — has no `sample`, `previous`, or `when` occurrence of its own and is
/// planned continuous. Correcting the plan from the proven partition owners is
/// what keeps the coordinate out of the continuous unknown set, whose equations
/// all belong to the clocked partition instead.
///
/// Only the two continuous non-state roles are corrected. A `der(...)` target
/// keeps [`PlannedRole::State`] so an illegal continuous state inside a clocked
/// partition is still reported by the expression validator rather than being
/// silently reclassified, and the non-runtime roles are never partition members
/// to begin with.
pub(super) fn apply_clocked_partition_roles(
    flat: &flat::Model,
    coordinate_owners: &HashMap<InstanceId, ClockPlan>,
    roles: &mut HashMap<VarName, PlannedRole>,
    expression_roles: &mut HashMap<VarName, PlannedRole>,
) -> Result<bool, ToDaeError> {
    let mut corrected = false;
    for (name, variable) in &flat.variables {
        if !coordinate_owners.contains_key(&variable.instance_id)
            || !matches!(
                roles.get(name),
                Some(PlannedRole::Algebraic | PlannedRole::Output)
            )
        {
            continue;
        }
        let scalar_type = validate_variable_header(flat, name, variable)?;
        let role = discrete_role(scalar_type);
        validate_variable_role(name, variable, role, scalar_type)?;
        roles.insert(name.clone(), role);
        expression_roles.insert(name.clone(), role);
        corrected = true;
    }
    Ok(corrected)
}

fn validate_variable(
    flat: &flat::Model,
    name: &VarName,
    variable: &flat::Variable,
    states: &HashSet<VarName>,
    assigned_discrete: &HashSet<VarName>,
) -> Result<PlannedRole, ToDaeError> {
    if variable.from_expandable_connector && !variable.connected && variable.binding.is_none() {
        validate_variable_header(flat, name, variable)?;
        return Ok(PlannedRole::UnusedExpandable);
    }
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
        discrete_role(scalar_type)
    } else if matches!(variable.causality, Causality::Output(_)) {
        PlannedRole::Output
    } else {
        PlannedRole::Algebraic
    }
}

/// The role a discrete-time coordinate of `scalar_type` is planned with: MLS
/// Appendix B keeps discrete `Real` coordinates (B.1b) in a different arena from
/// the discrete values (B.1c) every other scalar type lands in.
const fn discrete_role(scalar_type: dae::ScalarType) -> PlannedRole {
    match scalar_type {
        dae::ScalarType::Real => PlannedRole::DiscreteReal,
        dae::ScalarType::Integer
        | dae::ScalarType::Boolean
        | dae::ScalarType::String
        | dae::ScalarType::Enumeration
        | dae::ScalarType::Record => PlannedRole::DiscreteValue,
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
