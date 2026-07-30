use super::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::construction) struct ClockPlan {
    pub(in crate::construction) lattice: ClockLattice,
    pub(in crate::construction) constructor_span: Span,
}

#[derive(Clone, Copy)]
pub(in crate::construction) struct ClockedValuePlan {
    pub(in crate::construction) clock: ClockPlan,
    pub(in crate::construction) ownership_span: Span,
}

pub(super) struct ClockAnalysis {
    pub(super) plans: HashMap<InstanceId, ClockPlan>,
    pub(super) equation_rows: HashSet<usize>,
    pub(super) sampled_targets: HashMap<InstanceId, Span>,
}

pub(super) struct ClockDomainAnalysis {
    pub(super) equation_owners: HashMap<usize, ClockPlan>,
    pub(super) value_owners: HashMap<InstanceId, ClockedValuePlan>,
}

/// The exact `Clock` coordinates of one Flat model.
///
/// A Flat expression occurrence names its coordinate through the model's
/// variable catalog: `Reference::instance_id` carries the *enclosing* class
/// occurrence, not the referenced declaration, so clock ownership resolves
/// through the same catalog every other construction path uses and then keys
/// its plans on the resolved variable's own occurrence identity.
struct ClockCoordinates<'flat> {
    ordered: Vec<&'flat flat::Variable>,
    by_name: HashMap<&'flat VarName, &'flat flat::Variable>,
}

impl<'flat> ClockCoordinates<'flat> {
    fn resolve(&self, name: &VarName) -> Option<&'flat flat::Variable> {
        self.by_name.get(name).copied()
    }

    fn contains(&self, name: &VarName) -> bool {
        self.by_name.contains_key(name)
    }

    fn len(&self) -> usize {
        self.ordered.len()
    }
}

pub(super) fn analyze_clocks(
    flat: &flat::Model,
    constants: &EvalContext,
) -> Result<ClockAnalysis, ToDaeError> {
    let clocks = exact_clock_coordinates(flat)?;
    let mut plans = HashMap::new();
    let mut aliases = Vec::new();
    let mut equation_rows = HashSet::new();
    derive_bound_clock_plans(constants, &clocks, &mut plans)?;
    for (row, equation) in flat.equations.iter().enumerate() {
        let Some((lhs, rhs)) = subtraction_operands(&equation.residual) else {
            if expression_mentions_clock(&equation.residual, &clocks) {
                return Err(unsupported_clock_equation(equation));
            }
            continue;
        };
        let lhs_clock = whole_clock_reference(lhs, &clocks);
        let rhs_clock = whole_clock_reference(rhs, &clocks);
        match (lhs_clock, rhs_clock, periodic_constructor(rhs, constants)?) {
            (Some(target), None, Some(plan)) => {
                insert_plan(&mut plans, target, plan, equation.span)?;
                equation_rows.insert(row);
            }
            (Some(lhs), Some(rhs), None) => {
                aliases.push((lhs, rhs, equation.span));
                equation_rows.insert(row);
            }
            _ if expression_mentions_clock(&equation.residual, &clocks) => {
                return Err(unsupported_clock_equation(equation));
            }
            _ => {}
        }
    }

    propagate_aliases(&mut plans, &aliases)?;
    for variable in &clocks.ordered {
        if !plans.contains_key(&variable.instance_id) {
            return Err(ToDaeError::unresolved_clock_schedule(
                variable.name.as_str(),
                "no unique constructor reaches this clock coordinate through its aliases",
                variable.source_span,
            ));
        }
    }
    let sampled_targets = analyze_sampled_targets(flat, &equation_rows)?;
    Ok(ClockAnalysis {
        plans,
        equation_rows,
        sampled_targets,
    })
}

fn exact_clock_coordinates(flat: &flat::Model) -> Result<ClockCoordinates<'_>, ToDaeError> {
    let clock_type = flat.predefined_types.clock;
    let mut ordered = Vec::new();
    let mut by_name = HashMap::new();
    for (name, variable) in &flat.variables {
        if !is_predefined_clock_variable(flat, variable)? {
            continue;
        }
        ordered.push(variable);
        by_name.insert(name, variable);
    }
    debug_assert!(
        ordered
            .iter()
            .all(|variable| flat.effective_types[&variable.type_id].canonical_type() == clock_type)
    );
    Ok(ClockCoordinates { ordered, by_name })
}

fn derive_bound_clock_plans(
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
    plans: &mut HashMap<InstanceId, ClockPlan>,
) -> Result<(), ToDaeError> {
    for _ in 0..clocks.len() {
        let mut progress = false;
        for variable in &clocks.ordered {
            if plans.contains_key(&variable.instance_id) {
                continue;
            }
            let Some(binding) = variable.binding.as_ref() else {
                continue;
            };
            let Some(plan) = bound_clock_plan(binding, constants, clocks, plans)? else {
                continue;
            };
            insert_plan(plans, variable, plan, expression_span(binding)?)?;
            progress = true;
        }
        if !progress {
            break;
        }
    }
    Ok(())
}

fn bound_clock_plan(
    expression: &Expression,
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
    plans: &HashMap<InstanceId, ClockPlan>,
) -> Result<Option<ClockPlan>, ToDaeError> {
    if let Some(plan) = periodic_constructor(expression, constants)? {
        return Ok(Some(plan));
    }
    if let Some(source) = whole_clock_reference(expression, clocks) {
        return Ok(plans.get(&source.instance_id).copied());
    }
    let Expression::BuiltinCall {
        function,
        args,
        span,
    } = expression
    else {
        return Err(ToDaeError::unresolved_clock_schedule(
            "Clock binding",
            "a Clock binding must be a constructor, alias, or exact derived clock",
            expression_span(expression)?,
        ));
    };
    let operator = function.name();
    let Some(source) = args.first() else {
        return Err(invalid_clock_operator(
            operator,
            "requires a source clock",
            *span,
        ));
    };
    let Some(source_variable) = whole_clock_reference(source, clocks) else {
        return Err(invalid_clock_operator(
            operator,
            "requires a whole Clock coordinate as its first argument",
            *span,
        ));
    };
    let Some(source_plan) = plans.get(&source_variable.instance_id).copied() else {
        return Ok(None);
    };
    let lattice = match (function, args.as_slice()) {
        (BuiltinFunction::SubSample, [_, factor]) => source_plan
            .lattice
            .sub_sample(clock_integer(factor, constants, operator, *span)?),
        (BuiltinFunction::SuperSample, [_, factor]) => source_plan
            .lattice
            .super_sample(clock_integer(factor, constants, operator, *span)?),
        (BuiltinFunction::ShiftSample, [_, counter]) => source_plan
            .lattice
            .shift_sample(clock_integer(counter, constants, operator, *span)?, 1),
        (BuiltinFunction::ShiftSample, [_, counter, resolution]) => {
            source_plan.lattice.shift_sample(
                clock_integer(counter, constants, operator, *span)?,
                clock_integer(resolution, constants, operator, *span)?,
            )
        }
        (BuiltinFunction::BackSample, [_, counter]) => source_plan
            .lattice
            .back_sample(clock_integer(counter, constants, operator, *span)?, 1),
        (BuiltinFunction::BackSample, [_, counter, resolution]) => source_plan.lattice.back_sample(
            clock_integer(counter, constants, operator, *span)?,
            clock_integer(resolution, constants, operator, *span)?,
        ),
        (BuiltinFunction::NoClock, [_]) => {
            return Err(invalid_clock_operator(
                operator,
                "has no exact periodic lattice for checked clock ownership",
                *span,
            ));
        }
        (
            BuiltinFunction::SubSample
            | BuiltinFunction::SuperSample
            | BuiltinFunction::ShiftSample
            | BuiltinFunction::BackSample
            | BuiltinFunction::NoClock,
            _,
        ) => {
            return Err(invalid_clock_operator(
                operator,
                "has invalid clock conversion arity",
                *span,
            ));
        }
        _ => {
            return Err(ToDaeError::unresolved_clock_schedule(
                "Clock binding",
                "a Clock binding must use an exact predefined clock conversion",
                *span,
            ));
        }
    }
    .map_err(|error| {
        ToDaeError::unsupported_runtime_operator(operator, error.to_string(), *span)
    })?;
    Ok(Some(ClockPlan {
        lattice,
        constructor_span: *span,
    }))
}

fn clock_integer(
    expression: &Expression,
    constants: &EvalContext,
    operator: &str,
    span: Span,
) -> Result<i64, ToDaeError> {
    eval_expr(expression, constants)
        .ok()
        .and_then(|value| value.as_integer())
        .ok_or_else(|| {
            invalid_clock_operator(
                operator,
                "requires parameter-evaluable Integer conversion arguments",
                span,
            )
        })
}

fn invalid_clock_operator(operator: &str, detail: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_runtime_operator(operator, detail, span)
}

fn analyze_sampled_targets(
    flat: &flat::Model,
    clock_equation_rows: &HashSet<usize>,
) -> Result<HashMap<InstanceId, Span>, ToDaeError> {
    let mut sampled = HashMap::new();
    for (row, equation) in flat.equations.iter().enumerate() {
        if clock_equation_rows.contains(&row) {
            continue;
        }
        if let Some((target, sample_span)) = sampled_value_target(&equation.residual, flat) {
            if sampled.insert(target.instance_id, sample_span).is_some() {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    format!(
                        "sampled coordinate `{}` has more than one definition",
                        target.name
                    ),
                    equation.span,
                ));
            }
        } else if expression_mentions_value_sample(&equation.residual) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                "sample(value) must be the complete right-hand side of one coordinate definition",
                equation.span,
            ));
        }
    }
    Ok(sampled)
}

pub(super) fn analyze_clock_domains(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    plans: &HashMap<InstanceId, ClockPlan>,
    clock_equation_rows: &HashSet<usize>,
    sampled_targets: &HashMap<InstanceId, Span>,
) -> Result<ClockDomainAnalysis, ToDaeError> {
    let no_sampled_targets = HashMap::new();
    for equation in &flat.initial_equations {
        if let Some(span) = required_clock_owner_span(&equation.residual, flat, &no_sampled_targets)
        {
            return Err(ToDaeError::unsupported_flat(
                "initial clocked equation ownership",
                "clock-owned sample/previous semantics are not valid in an initial equation",
                span,
            ));
        }
    }
    let ordinals = flat
        .variables
        .iter()
        .filter(|(name, _)| {
            roles
                .get(*name)
                .is_some_and(|role| is_clock_runtime_role(*role))
        })
        .enumerate()
        .map(|(ordinal, (_, variable))| (variable.instance_id, ordinal))
        .collect::<HashMap<_, _>>();
    let mut domains = DisjointDomains::new(ordinals.len());
    let mut occurrences = vec![None; ordinals.len()];
    let mut equation_members = vec![Vec::new(); flat.equations.len()];
    for (row, equation) in flat.equations.iter().enumerate() {
        if clock_equation_rows.contains(&row) {
            continue;
        }
        let incidence = expression_clock_incidence(&equation.residual, flat, roles);
        equation_members[row] =
            register_incidence(&incidence, &ordinals, &mut occurrences, &mut domains);
    }
    let seeds = clocked_when_seeds(
        flat,
        roles,
        plans,
        &ordinals,
        &mut occurrences,
        &mut domains,
    );
    let mut owners = assign_domain_owners(&mut domains, seeds)?;
    infer_sampled_clock_owners(
        flat,
        plans,
        sampled_targets,
        &ordinals,
        &mut domains,
        &mut owners,
    )?;
    let equation_owners = assign_equation_owners(
        flat,
        &equation_members,
        &mut domains,
        &owners,
        sampled_targets,
    )?;
    validate_clocked_value_samples(flat, plans, &equation_owners)?;
    let value_owners = assign_value_owners(
        flat,
        roles,
        &ordinals,
        &mut domains,
        &owners,
        &occurrences,
        sampled_targets,
    )?;
    Ok(ClockDomainAnalysis {
        equation_owners,
        value_owners,
    })
}

fn is_clock_runtime_role(role: PlannedRole) -> bool {
    !matches!(
        role,
        PlannedRole::Parameter
            | PlannedRole::Constant
            | PlannedRole::Clock
            | PlannedRole::EnumerationLiteral
            | PlannedRole::Aggregate
    )
}

#[derive(Default)]
struct ClockIncidence {
    variables: Vec<(InstanceId, Span)>,
    seen: HashSet<InstanceId>,
}

impl ClockIncidence {
    fn insert(&mut self, instance: InstanceId, span: Span) {
        if self.seen.insert(instance) {
            self.variables.push((instance, span));
        }
    }
}

fn expression_clock_incidence(
    expression: &Expression,
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> ClockIncidence {
    let mut incidence = ClockIncidence::default();
    collect_expression_clock_incidence(expression, flat, roles, &mut incidence);
    incidence
}

fn collect_expression_clock_incidence(
    expression: &Expression,
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    incidence: &mut ClockIncidence,
) {
    if let Expression::VarRef {
        name,
        subscripts,
        span,
    } = expression
    {
        register_runtime_coordinate(flat, name.var_name(), *span, roles, incidence);
        for subscript in subscripts {
            if let Subscript::Expr { expr, .. } = subscript {
                collect_expression_clock_incidence(expr, flat, roles, incidence);
            }
        }
        return;
    }
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample | BuiltinFunction::Clock | BuiltinFunction::Hold,
            args,
            ..
        } => {
            for argument in args.iter().skip(1) {
                collect_expression_clock_incidence(argument, flat, roles, incidence);
            }
        }
        _ => {
            for child in expression_children(expression) {
                collect_expression_clock_incidence(child, flat, roles, incidence);
            }
        }
    }
}

fn register_incidence(
    incidence: &ClockIncidence,
    ordinals: &HashMap<InstanceId, usize>,
    occurrences: &mut [Option<Span>],
    domains: &mut DisjointDomains,
) -> Vec<usize> {
    let members = incidence
        .variables
        .iter()
        .filter_map(|(instance, span)| {
            let ordinal = ordinals.get(instance).copied()?;
            occurrences[ordinal].get_or_insert(*span);
            Some(ordinal)
        })
        .collect::<Vec<_>>();
    if let Some((&first, rest)) = members.split_first() {
        for &member in rest {
            domains.union(first, member);
        }
    }
    members
}

#[derive(Clone, Copy)]
struct ClockDomainSeed {
    member: usize,
    clock: ClockPlan,
    span: Span,
}

fn clocked_when_seeds(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    plans: &HashMap<InstanceId, ClockPlan>,
    ordinals: &HashMap<InstanceId, usize>,
    occurrences: &mut [Option<Span>],
    domains: &mut DisjointDomains,
) -> Vec<ClockDomainSeed> {
    let mut seeds = Vec::new();
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            let Some(clock) = clock_condition_plan(&branch.condition, flat, plans) else {
                continue;
            };
            let mut incidence = ClockIncidence::default();
            collect_when_clock_incidence(flat, &branch.equations, roles, &mut incidence);
            let members = register_incidence(&incidence, ordinals, occurrences, domains);
            if let Some(&member) = members.first() {
                seeds.push(ClockDomainSeed {
                    member,
                    clock,
                    span: branch.span,
                });
            }
        }
    }
    seeds
}

fn clock_condition_plan(
    condition: &Expression,
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
) -> Option<ClockPlan> {
    let Expression::VarRef {
        name, subscripts, ..
    } = condition
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let variable = flat.variables.get(name.var_name())?;
    plans.get(&variable.instance_id).copied()
}

fn collect_when_clock_incidence(
    flat: &flat::Model,
    equations: &[flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
    incidence: &mut ClockIncidence,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign {
                target,
                value,
                span,
                ..
            }
            | flat::WhenEquation::Reinit {
                state: target,
                value,
                span,
                ..
            } => {
                register_runtime_coordinate(flat, target, *span, roles, incidence);
                collect_expression_clock_incidence(value, flat, roles, incidence);
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                collect_expression_clock_incidence(condition, flat, roles, incidence);
                collect_expression_clock_incidence(message, flat, roles, incidence);
                if let Some(level) = level {
                    collect_expression_clock_incidence(level, flat, roles, incidence);
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                collect_expression_clock_incidence(message, flat, roles, incidence);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, equations) in branches {
                    collect_expression_clock_incidence(condition, flat, roles, incidence);
                    collect_when_clock_incidence(flat, equations, roles, incidence);
                }
                if let Some(equations) = else_branch {
                    collect_when_clock_incidence(flat, equations, roles, incidence);
                }
            }
            flat::WhenEquation::FunctionCallOutputs {
                outputs,
                function,
                span,
                ..
            } => {
                for output in outputs {
                    register_runtime_coordinate(flat, output, *span, roles, incidence);
                }
                collect_expression_clock_incidence(function, flat, roles, incidence);
            }
        }
    }
}

fn register_runtime_coordinate(
    flat: &flat::Model,
    name: &VarName,
    span: Span,
    roles: &HashMap<VarName, PlannedRole>,
    incidence: &mut ClockIncidence,
) {
    if roles
        .get(name)
        .is_some_and(|role| is_clock_runtime_role(*role))
        && let Some(variable) = flat.variables.get(name)
    {
        incidence.insert(variable.instance_id, span);
    }
}

fn assign_domain_owners(
    domains: &mut DisjointDomains,
    seeds: Vec<ClockDomainSeed>,
) -> Result<HashMap<usize, (ClockPlan, Span)>, ToDaeError> {
    let mut owners = HashMap::new();
    for seed in seeds {
        let root = domains.find(seed.member);
        match owners.get(&root) {
            Some((clock, _)) if *clock != seed.clock => {
                return Err(ToDaeError::unsupported_flat(
                    "clocked equation ownership proof",
                    "one equation component is connected to distinct clock owners",
                    seed.span,
                ));
            }
            Some(_) => {}
            None => {
                owners.insert(root, (seed.clock, seed.span));
            }
        }
    }
    Ok(owners)
}

/// MLS §16.5.1 clock inference for `sample(u)`.
///
/// A value sample that is not connected to a clocked `when` partition still
/// belongs to exactly one clock partition. The partition is proven only when
/// the model owns exactly one clock constructor; two independent constructors
/// leave the sample owner ambiguous even when their lattices coincide, so the
/// inference fails instead of picking one.
fn infer_sampled_clock_owners(
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    sampled_targets: &HashMap<InstanceId, Span>,
    ordinals: &HashMap<InstanceId, usize>,
    domains: &mut DisjointDomains,
    owners: &mut HashMap<usize, (ClockPlan, Span)>,
) -> Result<(), ToDaeError> {
    for variable in flat.variables.values() {
        let Some(&span) = sampled_targets.get(&variable.instance_id) else {
            continue;
        };
        let Some(&ordinal) = ordinals.get(&variable.instance_id) else {
            continue;
        };
        let root = domains.find(ordinal);
        if owners.contains_key(&root) {
            continue;
        }
        let plan = unique_clock_plan(plans, span)?;
        owners.insert(root, (plan, span));
    }
    Ok(())
}

fn unique_clock_plan(
    plans: &HashMap<InstanceId, ClockPlan>,
    span: Span,
) -> Result<ClockPlan, ToDaeError> {
    let mut unique: Option<ClockPlan> = None;
    for plan in plans.values() {
        match unique {
            Some(existing) if existing == *plan => {}
            Some(_) => {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    "sample(value) has more than one possible inferred clock",
                    span,
                ));
            }
            None => unique = Some(*plan),
        }
    }
    unique.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "clocked sample ownership proof",
            "sample(value) has no inferred clock constructor",
            span,
        )
    })
}

/// MLS §16.3 `sample(u, c)`: the explicit clock operand must be the clock that
/// owns the partition the sample occurs in.
///
/// Lowering keeps only the sampled value, so an operand naming a different
/// clock would silently change the sampling rate. Proving the match here is
/// what lets [`is_whole_clock_coordinate`] accept the two-operand form at the
/// lowering boundary.
fn validate_clocked_value_samples(
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    equation_owners: &HashMap<usize, ClockPlan>,
) -> Result<(), ToDaeError> {
    for (row, equation) in flat.equations.iter().enumerate() {
        validate_value_sample_operands(
            &equation.residual,
            flat,
            plans,
            equation_owners.get(&row).copied(),
        )?;
    }
    for equation in &flat.initial_equations {
        validate_value_sample_operands(&equation.residual, flat, plans, None)?;
    }
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            let owner = clock_condition_plan(&branch.condition, flat, plans);
            validate_when_value_sample_operands(&branch.equations, flat, plans, owner)?;
        }
    }
    Ok(())
}

fn validate_value_sample_operands(
    expression: &Expression,
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    owner: Option<ClockPlan>,
) -> Result<(), ToDaeError> {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Sample,
        args,
        span,
    } = expression
        && let [value, clock] = args.as_slice()
        && let Some(named) = clock_condition_plan(clock, flat, plans)
    {
        if owner != Some(named) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                "sample(value, clock) must name the clock that owns its partition",
                *span,
            ));
        }
        return validate_value_sample_operands(value, flat, plans, owner);
    }
    for child in expression_children(expression) {
        validate_value_sample_operands(child, flat, plans, owner)?;
    }
    Ok(())
}

fn validate_when_value_sample_operands(
    equations: &[flat::WhenEquation],
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    owner: Option<ClockPlan>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                validate_value_sample_operands(value, flat, plans, owner)?;
            }
            flat::WhenEquation::Assert { message, level, .. } => {
                validate_value_sample_operands(message, flat, plans, owner)?;
                if let Some(level) = level {
                    validate_value_sample_operands(level, flat, plans, owner)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                validate_value_sample_operands(message, flat, plans, owner)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, equations) in branches {
                    validate_when_value_sample_operands(equations, flat, plans, owner)?;
                }
                if let Some(equations) = else_branch {
                    validate_when_value_sample_operands(equations, flat, plans, owner)?;
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                validate_value_sample_operands(function, flat, plans, owner)?;
            }
        }
    }
    Ok(())
}

/// True when `expression` names one whole predefined `Clock` coordinate.
pub(in crate::construction) fn is_whole_clock_coordinate(
    flat: &flat::Model,
    expression: &Expression,
) -> bool {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return false;
    };
    subscripts.is_empty()
        && flat.variables.get(name.var_name()).is_some_and(|variable| {
            flat.effective_types
                .get(&variable.type_id)
                .is_some_and(|effective| effective.canonical_type() == flat.predefined_types.clock)
        })
}

fn assign_equation_owners(
    flat: &flat::Model,
    equation_members: &[Vec<usize>],
    domains: &mut DisjointDomains,
    owners: &HashMap<usize, (ClockPlan, Span)>,
    sampled_targets: &HashMap<InstanceId, Span>,
) -> Result<HashMap<usize, ClockPlan>, ToDaeError> {
    let mut equation_owners = HashMap::new();
    for (row, members) in equation_members.iter().enumerate() {
        let owner = members
            .first()
            .map(|&member| domains.find(member))
            .and_then(|root| owners.get(&root))
            .map(|(clock, _)| *clock);
        if let Some(owner) = owner {
            equation_owners.insert(row, owner);
            continue;
        }
        let equation = &flat.equations[row];
        if let Some(span) = required_clock_owner_span(&equation.residual, flat, sampled_targets) {
            return Err(ToDaeError::unsupported_flat(
                "clocked equation ownership proof",
                "clocked expression has no exact connected clock owner",
                span,
            ));
        }
    }
    Ok(equation_owners)
}

fn required_clock_owner_span(
    expression: &Expression,
    flat: &flat::Model,
    sampled_targets: &HashMap<InstanceId, Span>,
) -> Option<Span> {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Previous | BuiltinFunction::Interval,
            span,
            ..
        } => Some(*span),
        Expression::Binary { lhs, .. } => {
            if let Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
                && subscripts.is_empty()
                && let Some(variable) = flat.variables.get(name.var_name())
                && let Some(span) = sampled_targets.get(&variable.instance_id)
            {
                return Some(*span);
            }
            expression_children(expression)
                .into_iter()
                .find_map(|child| required_clock_owner_span(child, flat, sampled_targets))
        }
        _ => expression_children(expression)
            .into_iter()
            .find_map(|child| required_clock_owner_span(child, flat, sampled_targets)),
    }
}

fn assign_value_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    ordinals: &HashMap<InstanceId, usize>,
    domains: &mut DisjointDomains,
    owners: &HashMap<usize, (ClockPlan, Span)>,
    occurrences: &[Option<Span>],
    sampled_targets: &HashMap<InstanceId, Span>,
) -> Result<HashMap<InstanceId, ClockedValuePlan>, ToDaeError> {
    let mut value_owners = HashMap::new();
    for (name, variable) in &flat.variables {
        let instance = variable.instance_id;
        let Some(&ordinal) = ordinals.get(&instance) else {
            continue;
        };
        if !roles.get(name).is_some_and(|role| {
            matches!(role, PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        }) {
            continue;
        }
        let Some((clock, owner_span)) = owners.get(&domains.find(ordinal)).copied() else {
            continue;
        };
        let ownership_span = sampled_targets
            .get(&instance)
            .copied()
            .or(occurrences[ordinal])
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "clocked value ownership proof",
                    format!(
                        "clocked coordinate `{name}` has an owner but no exact source occurrence"
                    ),
                    owner_span,
                )
            })?;
        require_span(
            ownership_span,
            format!("clock ownership occurrence for `{name}`"),
        )?;
        value_owners.insert(
            instance,
            ClockedValuePlan {
                clock,
                ownership_span,
            },
        );
    }
    for (name, variable) in &flat.variables {
        let Some(span) = sampled_targets.get(&variable.instance_id) else {
            continue;
        };
        if !value_owners.contains_key(&variable.instance_id) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                format!("sampled coordinate `{name}` has no exact connected clock owner"),
                *span,
            ));
        }
    }
    Ok(value_owners)
}

struct DisjointDomains {
    parent: Vec<usize>,
}

impl DisjointDomains {
    fn new(len: usize) -> Self {
        Self {
            parent: (0..len).collect(),
        }
    }

    fn find(&mut self, member: usize) -> usize {
        let mut root = member;
        while self.parent[root] != root {
            root = self.parent[root];
        }
        let mut cursor = member;
        while self.parent[cursor] != cursor {
            let next = self.parent[cursor];
            self.parent[cursor] = root;
            cursor = next;
        }
        root
    }

    fn union(&mut self, lhs: usize, rhs: usize) {
        let lhs = self.find(lhs);
        let rhs = self.find(rhs);
        if lhs != rhs {
            self.parent[rhs] = lhs;
        }
    }
}

fn sampled_value_target<'flat>(
    expression: &Expression,
    flat: &'flat flat::Model,
) -> Option<(&'flat flat::Variable, Span)> {
    let (lhs, rhs) = subtraction_operands(expression)?;
    let Expression::VarRef {
        name, subscripts, ..
    } = lhs
    else {
        return None;
    };
    let Expression::BuiltinCall {
        function: BuiltinFunction::Sample,
        args,
        span,
        ..
    } = rhs
    else {
        return None;
    };
    if !subscripts.is_empty() || args.len() != 1 {
        return None;
    }
    flat.variables
        .get(name.var_name())
        .map(|variable| (variable, *span))
}

fn expression_mentions_value_sample(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            ..
        } if args.len() == 1
    ) || expression_children(expression)
        .into_iter()
        .any(expression_mentions_value_sample)
}

fn subtraction_operands(expression: &Expression) -> Option<(&Expression, &Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return None;
    };
    Some((lhs, rhs))
}

fn whole_clock_reference<'flat>(
    expression: &Expression,
    clocks: &ClockCoordinates<'flat>,
) -> Option<&'flat flat::Variable> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    subscripts
        .is_empty()
        .then(|| clocks.resolve(name.var_name()))
        .flatten()
}

fn periodic_constructor(
    expression: &Expression,
    constants: &EvalContext,
) -> Result<Option<ClockPlan>, ToDaeError> {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args,
        span,
    } = expression
    else {
        return Ok(None);
    };
    let [interval] = args.as_slice() else {
        return Err(ToDaeError::unsupported_runtime_operator(
            "Clock",
            "the canonical clock proof currently requires Clock(interval)",
            *span,
        ));
    };
    let seconds = evaluate_clock_seconds(interval, constants, "Clock interval", *span)?;
    let period = ClockRational::from_seconds(seconds).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("Clock", error.to_string(), *span)
    })?;
    let lattice = ClockLattice::new(period, ClockRational::ZERO).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("Clock", error.to_string(), *span)
    })?;
    Ok(Some(ClockPlan {
        lattice,
        constructor_span: *span,
    }))
}

fn insert_plan(
    plans: &mut HashMap<InstanceId, ClockPlan>,
    target: &flat::Variable,
    plan: ClockPlan,
    span: Span,
) -> Result<(), ToDaeError> {
    if let Some(existing) = plans.get(&target.instance_id)
        && existing.lattice != plan.lattice
    {
        return Err(ToDaeError::unresolved_clock_schedule(
            target.name.as_str(),
            "conflicting constructors bind this clock coordinate",
            span,
        ));
    }
    plans.insert(target.instance_id, plan);
    Ok(())
}

fn propagate_aliases(
    plans: &mut HashMap<InstanceId, ClockPlan>,
    aliases: &[(&flat::Variable, &flat::Variable, Span)],
) -> Result<(), ToDaeError> {
    loop {
        let mut progress = false;
        for (lhs, rhs, span) in aliases {
            match (
                plans.get(&lhs.instance_id).copied(),
                plans.get(&rhs.instance_id).copied(),
            ) {
                (Some(lhs_plan), Some(rhs_plan)) if lhs_plan.lattice != rhs_plan.lattice => {
                    return Err(ToDaeError::unresolved_clock_schedule(
                        format!("{} = {}", lhs.name, rhs.name),
                        "this clock alias joins conflicting constructors",
                        *span,
                    ));
                }
                (Some(plan), None) => {
                    plans.insert(rhs.instance_id, plan);
                    progress = true;
                }
                (None, Some(plan)) => {
                    plans.insert(lhs.instance_id, plan);
                    progress = true;
                }
                (Some(_), Some(_)) | (None, None) => {}
            }
        }
        if !progress {
            return Ok(());
        }
    }
}

fn expression_mentions_clock(expression: &Expression, clocks: &ClockCoordinates<'_>) -> bool {
    matches!(
        expression,
        Expression::VarRef { name, .. } if clocks.contains(name.var_name())
    ) || matches!(
        expression,
        Expression::BuiltinCall {
            function: BuiltinFunction::Clock,
            ..
        }
    ) || expression_children(expression)
        .into_iter()
        .any(|child| expression_mentions_clock(child, clocks))
}

fn unsupported_clock_equation(equation: &flat::Equation) -> ToDaeError {
    ToDaeError::unresolved_clock_schedule(
        "clock equation",
        "a clock equation must be an exact whole-coordinate constructor or alias",
        equation.span,
    )
}
