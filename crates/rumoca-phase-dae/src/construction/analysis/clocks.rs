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

pub(super) fn analyze_clocks(
    flat: &flat::Model,
    constants: &EvalContext,
) -> Result<ClockAnalysis, ToDaeError> {
    let clock_instances = exact_clock_instances(flat)?;
    let mut plans = HashMap::new();
    let mut aliases = Vec::new();
    let mut equation_rows = HashSet::new();
    derive_bound_clock_plans(constants, &clock_instances, &mut plans)?;
    for (row, equation) in flat.equations.iter().enumerate() {
        let Some((lhs, rhs)) = subtraction_operands(&equation.residual) else {
            if expression_mentions_clock(&equation.residual, &clock_instances) {
                return Err(unsupported_clock_equation(equation));
            }
            continue;
        };
        let lhs_clock = whole_clock_reference(lhs, &clock_instances);
        let rhs_clock = whole_clock_reference(rhs, &clock_instances);
        match (lhs_clock, rhs_clock, periodic_constructor(rhs, constants)?) {
            (Some(target), None, Some(plan)) => {
                insert_plan(&mut plans, target, plan, equation.span)?;
                equation_rows.insert(row);
            }
            (Some(lhs), Some(rhs), None) => {
                aliases.push((lhs, rhs, equation.span));
                equation_rows.insert(row);
            }
            _ if expression_mentions_clock(&equation.residual, &clock_instances) => {
                return Err(unsupported_clock_equation(equation));
            }
            _ => {}
        }
    }

    propagate_aliases(&mut plans, &aliases)?;
    for (instance, variable) in &clock_instances {
        if !plans.contains_key(instance) {
            return Err(ToDaeError::unsupported_flat(
                "clock ownership proof",
                format!(
                    "clock coordinate `{}` has no unique constructor through its aliases",
                    variable.name
                ),
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

fn exact_clock_instances(
    flat: &flat::Model,
) -> Result<HashMap<InstanceId, &flat::Variable>, ToDaeError> {
    let clock_type = flat.predefined_types.clock;
    let mut clocks = HashMap::new();
    for variable in flat.variables.values() {
        if is_predefined_clock_variable(flat, variable)? {
            clocks.insert(variable.instance_id, variable);
        }
    }
    debug_assert!(clocks.values().all(|variable| {
        flat.effective_types[&variable.type_id].canonical_type() == clock_type
    }));
    Ok(clocks)
}

fn derive_bound_clock_plans(
    constants: &EvalContext,
    clock_instances: &HashMap<InstanceId, &flat::Variable>,
    plans: &mut HashMap<InstanceId, ClockPlan>,
) -> Result<(), ToDaeError> {
    for _ in 0..clock_instances.len() {
        let mut progress = false;
        for (&instance, variable) in clock_instances {
            if plans.contains_key(&instance) {
                continue;
            }
            let Some(binding) = variable.binding.as_ref() else {
                continue;
            };
            let Some(plan) = bound_clock_plan(binding, constants, clock_instances, plans)? else {
                continue;
            };
            insert_plan(plans, instance, plan, expression_span(binding)?)?;
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
    clock_instances: &HashMap<InstanceId, &flat::Variable>,
    plans: &HashMap<InstanceId, ClockPlan>,
) -> Result<Option<ClockPlan>, ToDaeError> {
    if let Some(plan) = periodic_constructor(expression, constants)? {
        return Ok(Some(plan));
    }
    if let Some(instance) = whole_clock_reference(expression, clock_instances) {
        return Ok(plans.get(&instance).copied());
    }
    let Expression::BuiltinCall {
        function,
        args,
        span,
    } = expression
    else {
        return Err(ToDaeError::unsupported_flat(
            "clock ownership proof",
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
    let Some(source_instance) = whole_clock_reference(source, clock_instances) else {
        return Err(invalid_clock_operator(
            operator,
            "requires a whole Clock coordinate as its first argument",
            *span,
        ));
    };
    let Some(source_plan) = plans.get(&source_instance).copied() else {
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
            return Err(ToDaeError::unsupported_flat(
                "clock ownership proof",
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
        if let Some((target, target_name, sample_span)) = sampled_value_target(&equation.residual) {
            if sampled.insert(target, sample_span).is_some() {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    format!("sampled coordinate `{target_name}` has more than one definition"),
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
        if let Some(span) = required_clock_owner_span(&equation.residual, &no_sampled_targets) {
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
        let incidence = expression_clock_incidence(&equation.residual, roles);
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
    let owners = assign_domain_owners(&mut domains, seeds)?;
    let equation_owners = assign_equation_owners(
        flat,
        &equation_members,
        &mut domains,
        &owners,
        sampled_targets,
    )?;
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
    roles: &HashMap<VarName, PlannedRole>,
) -> ClockIncidence {
    let mut incidence = ClockIncidence::default();
    collect_expression_clock_incidence(expression, roles, &mut incidence);
    incidence
}

fn collect_expression_clock_incidence(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    incidence: &mut ClockIncidence,
) {
    if let Expression::VarRef {
        name,
        subscripts,
        span,
    } = expression
    {
        if roles
            .get(name.var_name())
            .is_some_and(|role| is_clock_runtime_role(*role))
            && let Some(instance) = name.instance_id()
        {
            incidence.insert(instance, *span);
        }
        for subscript in subscripts {
            if let Subscript::Expr { expr, .. } = subscript {
                collect_expression_clock_incidence(expr, roles, incidence);
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
                collect_expression_clock_incidence(argument, roles, incidence);
            }
        }
        _ => {
            for child in expression_children(expression) {
                collect_expression_clock_incidence(child, roles, incidence);
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
            let Some(clock) = clock_condition_plan(&branch.condition, plans) else {
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
    plans: &HashMap<InstanceId, ClockPlan>,
) -> Option<ClockPlan> {
    let Expression::VarRef {
        name, subscripts, ..
    } = condition
    else {
        return None;
    };
    subscripts
        .is_empty()
        .then(|| {
            name.instance_id()
                .and_then(|instance| plans.get(&instance).copied())
        })
        .flatten()
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
                if roles
                    .get(target)
                    .is_some_and(|role| is_clock_runtime_role(*role))
                    && let Some(variable) = flat.variables.get(target)
                {
                    incidence.insert(variable.instance_id, *span);
                }
                collect_expression_clock_incidence(value, roles, incidence);
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                collect_expression_clock_incidence(condition, roles, incidence);
                collect_expression_clock_incidence(message, roles, incidence);
                if let Some(level) = level {
                    collect_expression_clock_incidence(level, roles, incidence);
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                collect_expression_clock_incidence(message, roles, incidence);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, equations) in branches {
                    collect_expression_clock_incidence(condition, roles, incidence);
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
                collect_expression_clock_incidence(function, roles, incidence);
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
        if let Some(span) = required_clock_owner_span(&equation.residual, sampled_targets) {
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
                && let Some(instance) = name.instance_id()
                && let Some(span) = sampled_targets.get(&instance)
            {
                return Some(*span);
            }
            expression_children(expression)
                .into_iter()
                .find_map(|child| required_clock_owner_span(child, sampled_targets))
        }
        _ => expression_children(expression)
            .into_iter()
            .find_map(|child| required_clock_owner_span(child, sampled_targets)),
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
    let names_by_instance = flat
        .variables
        .iter()
        .map(|(name, variable)| (variable.instance_id, name))
        .collect::<HashMap<_, _>>();
    for (&instance, &ordinal) in ordinals {
        let Some(&name) = names_by_instance.get(&instance) else {
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
    for (instance, span) in sampled_targets {
        if !value_owners.contains_key(instance) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                format!("sampled coordinate `{instance}` has no exact connected clock owner"),
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

fn sampled_value_target(expression: &Expression) -> Option<(InstanceId, &VarName, Span)> {
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
    (subscripts.is_empty() && args.len() == 1)
        .then(|| {
            name.instance_id()
                .map(|instance| (instance, name.var_name(), *span))
        })
        .flatten()
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

fn whole_clock_reference(
    expression: &Expression,
    clock_instances: &HashMap<InstanceId, &flat::Variable>,
) -> Option<InstanceId> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    let instance = name.instance_id()?;
    (subscripts.is_empty() && clock_instances.contains_key(&instance)).then_some(instance)
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
    target: InstanceId,
    plan: ClockPlan,
    span: Span,
) -> Result<(), ToDaeError> {
    if let Some(existing) = plans.get(&target)
        && existing.lattice != plan.lattice
    {
        return Err(ToDaeError::unsupported_flat(
            "clock ownership proof",
            format!("clock coordinate `{target}` has conflicting constructors"),
            span,
        ));
    }
    plans.insert(target, plan);
    Ok(())
}

fn propagate_aliases(
    plans: &mut HashMap<InstanceId, ClockPlan>,
    aliases: &[(InstanceId, InstanceId, Span)],
) -> Result<(), ToDaeError> {
    loop {
        let mut progress = false;
        for (lhs, rhs, span) in aliases {
            match (plans.get(lhs).copied(), plans.get(rhs).copied()) {
                (Some(lhs_plan), Some(rhs_plan)) if lhs_plan.lattice != rhs_plan.lattice => {
                    return Err(ToDaeError::unsupported_flat(
                        "clock ownership proof",
                        format!("clock alias `{lhs} = {rhs}` joins conflicting constructors"),
                        *span,
                    ));
                }
                (Some(plan), None) => {
                    plans.insert(*rhs, plan);
                    progress = true;
                }
                (None, Some(plan)) => {
                    plans.insert(*lhs, plan);
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

fn expression_mentions_clock(
    expression: &Expression,
    clock_instances: &HashMap<InstanceId, &flat::Variable>,
) -> bool {
    matches!(
        expression,
        Expression::VarRef { name, .. }
            if name
                .instance_id()
                .is_some_and(|instance| clock_instances.contains_key(&instance))
    ) || matches!(
        expression,
        Expression::BuiltinCall {
            function: BuiltinFunction::Clock,
            ..
        }
    ) || expression_children(expression)
        .into_iter()
        .any(|child| expression_mentions_clock(child, clock_instances))
}

fn unsupported_clock_equation(equation: &flat::Equation) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "clock ownership proof",
        "a clock equation must be an exact whole-coordinate constructor or alias",
        equation.span,
    )
}
