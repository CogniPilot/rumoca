use super::*;
use std::borrow::Cow;

#[derive(Clone)]
pub(in crate::construction) enum EquationPartition<'flat> {
    Continuous,
    DiscreteReal { target: &'flat VarName },
    DiscreteValue(DiscreteValueAssignmentPlan<'flat>),
    ConsumedDiscreteValue,
}

#[derive(Clone)]
pub(in crate::construction) struct DiscreteValueAssignmentPlan<'flat> {
    pub(in crate::construction) target: &'flat VarName,
    pub(in crate::construction) value: Cow<'flat, Expression>,
    pub(in crate::construction) generated: bool,
    pub(in crate::construction) scalar_count: Option<usize>,
    /// The owner was constructed from exact, row-major element coverage.
    ///
    /// This is only permission for the typed DAE constructor to attempt its
    /// independent strictly-backward self-dependency proof. It is not itself
    /// evidence that a recurrence is valid.
    pub(in crate::construction) ordered_scalar_self_dependencies: bool,
}

#[derive(Default)]
pub(in crate::construction) struct AggregateDiscreteConnections {
    owners: HashMap<usize, AggregateDiscreteConnection>,
    members: HashSet<usize>,
}

struct AggregateDiscreteConnection {
    target: VarName,
    value: Expression,
    scalar_count: usize,
    ordered_scalar_self_dependencies: bool,
}

pub(in crate::construction) fn equation_partition<'flat>(
    flat: &'flat flat::Model,
    row: usize,
    equation: &'flat flat::Equation,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &HashMap<VarName, usize>,
    aggregate_connections: &'flat AggregateDiscreteConnections,
) -> Result<EquationPartition<'flat>, ToDaeError> {
    if let Some(plan) = aggregate_connections.owners.get(&row) {
        return Ok(EquationPartition::DiscreteValue(
            DiscreteValueAssignmentPlan {
                target: &plan.target,
                value: Cow::Borrowed(&plan.value),
                generated: true,
                scalar_count: Some(plan.scalar_count),
                ordered_scalar_self_dependencies: plan.ordered_scalar_self_dependencies,
            },
        ));
    }
    if aggregate_connections.members.contains(&row) {
        return Ok(EquationPartition::ConsumedDiscreteValue);
    }
    if let Some(plan) = discrete_connection_assignment(flat, equation, roles, connection_ranks)? {
        return Ok(EquationPartition::DiscreteValue(plan));
    }
    if let Some(plan) = discrete_value_assignment(&equation.residual, roles, equation.span)? {
        return Ok(EquationPartition::DiscreteValue(plan));
    }
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs: _,
        ..
    } = &equation.residual
    else {
        return Ok(EquationPartition::Continuous);
    };
    let targets = assignment_target_names(lhs);
    let discrete_targets = targets
        .iter()
        .filter(|target| {
            matches!(
                roles.get(*target),
                Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
            )
        })
        .collect::<Vec<_>>();
    if discrete_targets.is_empty() {
        return Ok(EquationPartition::Continuous);
    }
    if discrete_targets.len() != targets.len() || targets.len() != 1 {
        return Err(ToDaeError::unsupported_flat(
            "mixed discrete equation",
            "one equation owner cannot mix continuous and discrete targets or define multiple discrete coordinates",
            equation.span,
        ));
    }
    let target = discrete_targets[0];
    match roles[target] {
        PlannedRole::DiscreteReal => Ok(EquationPartition::DiscreteReal { target }),
        PlannedRole::DiscreteValue => {
            unreachable!("discrete-value equations are classified before residual equations")
        }
        PlannedRole::UnusedExpandable
        | PlannedRole::Parameter
        | PlannedRole::Constant
        | PlannedRole::Input
        | PlannedRole::State
        | PlannedRole::Algebraic
        | PlannedRole::Output
        | PlannedRole::Clock
        | PlannedRole::EnumerationLiteral
        | PlannedRole::Aggregate => {
            unreachable!("the target was selected as a discrete coordinate")
        }
    }
}

fn discrete_connection_assignment<'flat>(
    flat: &'flat flat::Model,
    equation: &'flat flat::Equation,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &HashMap<VarName, usize>,
) -> Result<Option<DiscreteValueAssignmentPlan<'flat>>, ToDaeError> {
    let Some((target, target_subscripts, value)) =
        oriented_discrete_connection(flat, equation, roles, connection_ranks)
    else {
        return Ok(None);
    };
    let value = full_aggregate_connection_value(
        &flat.variables[target],
        target_subscripts,
        value,
        equation.span,
    )?;
    Ok(Some(DiscreteValueAssignmentPlan {
        target,
        generated: matches!(&value, Cow::Owned(_)),
        value,
        scalar_count: None,
        ordered_scalar_self_dependencies: false,
    }))
}

fn oriented_discrete_connection<'flat>(
    flat: &'flat flat::Model,
    equation: &'flat flat::Equation,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &HashMap<VarName, usize>,
) -> Option<(&'flat VarName, &'flat [Subscript], &'flat Expression)> {
    if !matches!(equation.origin, flat::EquationOrigin::Connection { .. }) {
        return None;
    }
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return None;
    };
    let (lhs_name, lhs_subscripts) = discrete_value_base_reference(lhs, roles)?;
    let (rhs_name, rhs_subscripts) = discrete_value_base_reference(rhs, roles)?;
    let lhs_variable = flat.variables.get(lhs_name)?;
    let rhs_variable = flat.variables.get(rhs_name)?;
    let lhs_rank = connection_ranks.get(lhs_name).copied();
    let rhs_rank = connection_ranks.get(rhs_name).copied();
    let (target, target_subscripts, value) = match (lhs_rank, rhs_rank) {
        (Some(lhs_rank), Some(rhs_rank)) if lhs_rank < rhs_rank => {
            (rhs_name, rhs_subscripts, lhs.as_ref())
        }
        (Some(lhs_rank), Some(rhs_rank)) if rhs_rank < lhs_rank => {
            (lhs_name, lhs_subscripts, rhs.as_ref())
        }
        (Some(_), None) => (rhs_name, rhs_subscripts, lhs.as_ref()),
        (None, Some(_)) => (lhs_name, lhs_subscripts, rhs.as_ref()),
        _ => match (&lhs_variable.causality, &rhs_variable.causality) {
            (Causality::Output(_), Causality::Input(_)) => (rhs_name, rhs_subscripts, lhs.as_ref()),
            (Causality::Input(_), Causality::Output(_)) => (lhs_name, lhs_subscripts, rhs.as_ref()),
            _ => return None,
        },
    };
    Some((target, target_subscripts, value))
}

pub(super) fn aggregate_discrete_connections(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &HashMap<VarName, usize>,
) -> Result<AggregateDiscreteConnections, ToDaeError> {
    let mut groups = HashMap::<VarName, AggregateConnectionGroup>::new();
    for (row, equation) in flat.equations.iter().enumerate() {
        let (target, subscripts, value, ordered_scalar_self_dependencies) =
            if let Some((target, subscripts, value)) =
                oriented_discrete_connection(flat, equation, roles, connection_ranks)
            {
                (target, subscripts, value, false)
            } else if let Some((target, subscripts, value)) =
                discrete_element_assignment(equation, roles)
            {
                (target, subscripts, value, true)
            } else {
                continue;
            };
        if subscripts.is_empty()
            || selection_denotes_whole_aggregate(&flat.variables[target], subscripts)
        {
            continue;
        }
        let variable = &flat.variables[target];
        let prefix = selected_prefix_index(variable, subscripts, equation.span)?;
        let group = groups.entry(target.clone()).or_insert_with(|| {
            AggregateConnectionGroup::new(variable, prefix.extents.clone(), equation)
        });
        group.insert(
            row,
            prefix,
            value,
            ordered_scalar_self_dependencies,
            equation,
        )?;
    }
    let mut result = AggregateDiscreteConnections::default();
    for target in flat.variables.keys() {
        if let Some(group) = groups.remove(target) {
            group.finish(target.clone(), &mut result)?;
        }
    }
    debug_assert!(groups.is_empty(), "every group names a Flat variable");
    Ok(result)
}

struct SelectedPrefix {
    ordinal: usize,
    extents: Vec<usize>,
}

struct AggregateConnectionMember {
    row: usize,
    value: Expression,
    span: Span,
    ordered_scalar_self_dependencies: bool,
}

struct AggregateConnectionGroup {
    target_dims: Vec<i64>,
    prefix_extents: Vec<usize>,
    members: Vec<Option<AggregateConnectionMember>>,
    first_span: Span,
}

impl AggregateConnectionGroup {
    fn new(
        variable: &flat::Variable,
        prefix_extents: Vec<usize>,
        equation: &flat::Equation,
    ) -> Self {
        let count = prefix_extents.iter().product();
        let mut members = Vec::with_capacity(count);
        members.resize_with(count, || None);
        Self {
            target_dims: variable.dims.clone(),
            prefix_extents,
            members,
            first_span: equation.span,
        }
    }

    fn insert(
        &mut self,
        row: usize,
        prefix: SelectedPrefix,
        value: &Expression,
        ordered_scalar_self_dependencies: bool,
        equation: &flat::Equation,
    ) -> Result<(), ToDaeError> {
        if prefix.extents != self.prefix_extents {
            return Err(invalid_discrete_lhs(equation.span));
        }
        let Some(slot) = self.members.get_mut(prefix.ordinal) else {
            return Err(invalid_discrete_lhs(equation.span));
        };
        if slot.is_some() {
            return Err(ToDaeError::discrete_solved_form_violation(
                "overlapping element assignments cannot define one discrete coordinate",
                equation.span,
            ));
        }
        *slot = Some(AggregateConnectionMember {
            row,
            value: value.clone(),
            span: equation.span,
            ordered_scalar_self_dependencies,
        });
        Ok(())
    }

    fn finish(
        self,
        target: VarName,
        result: &mut AggregateDiscreteConnections,
    ) -> Result<(), ToDaeError> {
        if self.members.iter().any(Option::is_none) {
            return Err(ToDaeError::discrete_solved_form_violation(
                "element assignments must cover a discrete coordinate exactly once",
                self.first_span,
            ));
        }
        let members = self
            .members
            .into_iter()
            .map(|member| member.expect("complete coverage has one member per prefix coordinate"))
            .collect::<Vec<_>>();
        let owner_row = members[0].row;
        let owner_span = members[0].span;
        let values = members
            .iter()
            .map(|member| member.value.clone())
            .collect::<Vec<_>>();
        let value = pack_connection_prefix(&values, &self.prefix_extents, owner_span);
        let scalar_count = checked_connection_shape_size(&self.target_dims, owner_span)?;
        let ordered_scalar_self_dependencies = members
            .iter()
            .any(|member| member.ordered_scalar_self_dependencies);
        result
            .members
            .extend(members.iter().map(|member| member.row));
        result.owners.insert(
            owner_row,
            AggregateDiscreteConnection {
                target,
                value,
                scalar_count,
                ordered_scalar_self_dependencies,
            },
        );
        Ok(())
    }
}

/// Select an ordinary element equation whose left side names one exact
/// discrete coordinate. Aggregate construction later proves that all selected
/// prefixes cover the declared coordinate exactly once before any member is
/// consumed.
fn discrete_element_assignment<'flat>(
    equation: &'flat flat::Equation,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<(&'flat VarName, &'flat [Subscript], &'flat Expression)> {
    if matches!(equation.origin, flat::EquationOrigin::Connection { .. }) {
        return None;
    }
    discrete_element_expression(&equation.residual, roles)
}

fn discrete_element_expression<'flat>(
    expression: &'flat Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<(&'flat VarName, &'flat [Subscript], &'flat Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return None;
    };
    let (target, subscripts) = discrete_value_base_reference(lhs, roles)?;
    (!subscripts.is_empty()).then_some((target, subscripts, rhs.as_ref()))
}

/// Whether every body is an exact discrete element assignment whose
/// materialized rows must be validated by the aggregate coverage analysis.
pub(in crate::construction) fn structured_discrete_element_assignments(
    bodies: &[Expression],
    roles: &HashMap<VarName, PlannedRole>,
) -> bool {
    !bodies.is_empty()
        && bodies
            .iter()
            .all(|body| discrete_element_expression(body, roles).is_some())
}

fn selected_prefix_index(
    variable: &flat::Variable,
    subscripts: &[Subscript],
    span: Span,
) -> Result<SelectedPrefix, ToDaeError> {
    if subscripts.is_empty() || subscripts.len() > variable.dims.len() {
        return Err(invalid_discrete_lhs(span));
    }
    let mut ordinal = 0usize;
    let mut extents = Vec::with_capacity(subscripts.len());
    for (subscript, extent) in subscripts.iter().zip(&variable.dims) {
        let Ok(extent) = usize::try_from(*extent) else {
            return Err(invalid_discrete_lhs(span));
        };
        let Subscript::Index { value, .. } = subscript else {
            return Err(invalid_discrete_lhs(span));
        };
        let Ok(index) = usize::try_from(*value) else {
            return Err(invalid_discrete_lhs(span));
        };
        if index == 0 || index > extent {
            return Err(invalid_discrete_lhs(span));
        }
        ordinal = ordinal
            .checked_mul(extent)
            .and_then(|base| base.checked_add(index - 1))
            .ok_or_else(|| invalid_discrete_lhs(span))?;
        extents.push(extent);
    }
    Ok(SelectedPrefix { ordinal, extents })
}

fn checked_connection_shape_size(dims: &[i64], span: Span) -> Result<usize, ToDaeError> {
    dims.iter().try_fold(1usize, |size, extent| {
        usize::try_from(*extent)
            .ok()
            .and_then(|extent| size.checked_mul(extent))
            .ok_or_else(|| invalid_discrete_lhs(span))
    })
}

fn pack_connection_prefix(values: &[Expression], extents: &[usize], span: Span) -> Expression {
    let Some((&outer, inner_extents)) = extents.split_first() else {
        return values[0].clone();
    };
    let inner_count = inner_extents.iter().product::<usize>();
    let elements = values
        .chunks_exact(inner_count)
        .take(outer)
        .map(|chunk| pack_connection_prefix(chunk, inner_extents, span))
        .collect();
    Expression::Array {
        elements,
        is_matrix: false,
        span,
    }
}

/// Prove the exact discrete coordinates that already have a semantic owner
/// outside the connection graph.
///
/// An `output` connector may either produce a value or merely forward one to
/// an enclosing connector. Causality therefore cannot orient output-to-output
/// edges by itself. The source owners are the exact evidence: bindings,
/// ordinary equations, algorithms, and when chains define producers, while
/// connection equations do not. A multi-source graph walk then assigns every
/// pass-through coordinate its minimum distance from a proven producer, so a
/// chain is oriented outward without rendered-name or model-specific rules.
/// Building the ranks once keeps connection classification linear in the model
/// size.
pub(super) fn discrete_connection_ranks(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> HashMap<VarName, usize> {
    let mut producers = flat
        .variables
        .iter()
        .filter(|(name, variable)| {
            variable.binding.is_some() && matches!(roles[*name], PlannedRole::DiscreteValue)
        })
        .map(|(name, _)| name.clone())
        .collect::<HashSet<_>>();
    for equation in &flat.equations {
        if matches!(equation.origin, flat::EquationOrigin::Connection { .. }) {
            continue;
        }
        if let Ok(Some(plan)) = discrete_value_assignment(&equation.residual, roles, equation.span)
        {
            producers.insert(plan.target.clone());
        }
    }
    producers.extend(event_targets(flat));
    producers.extend(algorithm_targets(flat));

    let mut neighbors = HashMap::<VarName, Vec<VarName>>::new();
    for equation in &flat.equations {
        if !matches!(equation.origin, flat::EquationOrigin::Connection { .. }) {
            continue;
        }
        let Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } = &equation.residual
        else {
            continue;
        };
        let Some((lhs, _)) = discrete_value_base_reference(lhs, roles) else {
            continue;
        };
        let Some((rhs, _)) = discrete_value_base_reference(rhs, roles) else {
            continue;
        };
        neighbors.entry(lhs.clone()).or_default().push(rhs.clone());
        neighbors.entry(rhs.clone()).or_default().push(lhs.clone());
    }
    let mut ranks = producers
        .into_iter()
        .map(|producer| (producer, 0usize))
        .collect::<HashMap<_, _>>();
    let mut frontier = ranks.keys().cloned().collect::<Vec<_>>();
    let mut cursor = 0usize;
    while let Some(current) = frontier.get(cursor).cloned() {
        cursor += 1;
        let next_rank = ranks[&current] + 1;
        for neighbor in neighbors.get(&current).into_iter().flatten() {
            if ranks.contains_key(neighbor) {
                continue;
            }
            ranks.insert(neighbor.clone(), next_rank);
            frontier.push(neighbor.clone());
        }
    }
    ranks
}

/// Turn an element connection into a whole-coordinate definition only when
/// every selected leading axis is a singleton selected at its sole index.
/// This is a construction proof that the element denotes the entire aggregate,
/// not a relaxation of Appendix B.1c's one-coordinate ownership rule.
fn full_aggregate_connection_value<'flat>(
    target: &flat::Variable,
    subscripts: &[Subscript],
    value: &'flat Expression,
    owner: Span,
) -> Result<Cow<'flat, Expression>, ToDaeError> {
    if subscripts.is_empty() {
        return Ok(Cow::Borrowed(value));
    }
    if !selection_denotes_whole_aggregate(target, subscripts) {
        return Err(invalid_discrete_lhs(owner));
    }
    let mut aggregate = value.clone();
    for _ in subscripts.iter().rev() {
        aggregate = Expression::Array {
            elements: vec![aggregate],
            is_matrix: false,
            span: owner,
        };
    }
    Ok(Cow::Owned(aggregate))
}

fn selection_denotes_whole_aggregate(target: &flat::Variable, subscripts: &[Subscript]) -> bool {
    subscripts.len() <= target.dims.len()
        && subscripts
            .iter()
            .zip(&target.dims)
            .all(|(subscript, extent)| {
                *extent == 1 && matches!(subscript, Subscript::Index { value: 1, .. })
            })
}

fn discrete_value_base_reference<'flat>(
    expression: &'flat Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<(&'flat VarName, &'flat [Subscript])> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    let name = name.var_name();
    matches!(roles.get(name), Some(PlannedRole::DiscreteValue))
        .then_some((name, subscripts.as_slice()))
}

pub(in crate::construction) fn discrete_value_assignment<'flat>(
    expression: &'flat Expression,
    roles: &HashMap<VarName, PlannedRole>,
    owner: Span,
) -> Result<Option<DiscreteValueAssignmentPlan<'flat>>, ToDaeError> {
    match expression {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            if !assignment_side_mentions_discrete_value(lhs, roles) {
                return Ok(None);
            }
            let Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
            else {
                return Err(invalid_discrete_lhs(owner));
            };
            if !subscripts.is_empty()
                || !matches!(roles.get(name.var_name()), Some(PlannedRole::DiscreteValue))
            {
                return Err(invalid_discrete_lhs(owner));
            }
            Ok(Some(DiscreteValueAssignmentPlan {
                target: name.var_name(),
                value: Cow::Borrowed(rhs),
                generated: false,
                scalar_count: None,
                ordered_scalar_self_dependencies: false,
            }))
        }
        Expression::If {
            branches,
            else_branch,
            span,
        } => {
            let fallback = discrete_value_assignment(else_branch, roles, owner)?;
            let mut branch_plans = Vec::with_capacity(branches.len());
            for (condition, branch) in branches {
                branch_plans.push((condition, discrete_value_assignment(branch, roles, owner)?));
            }
            if fallback.is_none() && branch_plans.iter().all(|(_, plan)| plan.is_none()) {
                return Ok(None);
            }
            let Some(fallback) = fallback else {
                return Err(ToDaeError::discrete_solved_form_violation(
                    "all branches of a discrete-valued equation must assign the same coordinate",
                    owner,
                ));
            };
            let mut values = Vec::with_capacity(branches.len());
            for (condition, branch) in branch_plans {
                let Some(branch) = branch else {
                    return Err(ToDaeError::discrete_solved_form_violation(
                        "all branches of a discrete-valued equation must assign the same coordinate",
                        owner,
                    ));
                };
                if branch.target != fallback.target {
                    return Err(ToDaeError::discrete_solved_form_violation(
                        "all branches of a discrete-valued equation must assign the same coordinate",
                        owner,
                    ));
                }
                values.push((condition.clone(), branch.value.into_owned()));
            }
            Ok(Some(DiscreteValueAssignmentPlan {
                target: fallback.target,
                value: Cow::Owned(Expression::If {
                    branches: values,
                    else_branch: Box::new(fallback.value.into_owned()),
                    span: *span,
                }),
                generated: true,
                scalar_count: None,
                ordered_scalar_self_dependencies: false,
            }))
        }
        _ if expression_mentions_discrete_value(expression, roles) => {
            Err(invalid_discrete_lhs(owner))
        }
        _ => Ok(None),
    }
}

pub(in crate::construction) fn structured_discrete_assignments<'flat>(
    bodies: &'flat [Expression],
    roles: &HashMap<VarName, PlannedRole>,
    owner: Span,
) -> Result<Option<Vec<DiscreteValueAssignmentPlan<'flat>>>, ToDaeError> {
    let assignments = bodies
        .iter()
        .map(|body| discrete_value_assignment(body, roles, owner))
        .collect::<Result<Vec<_>, _>>()?;
    if assignments.iter().all(Option::is_none) {
        return Ok(None);
    }
    if assignments.iter().any(Option::is_none) {
        return Err(ToDaeError::unsupported_flat(
            "mixed structured equation partition",
            "one structured family cannot mix continuous residual and discrete-value bodies",
            owner,
        ));
    }
    Ok(Some(
        assignments
            .into_iter()
            .map(|assignment| assignment.expect("the complete family is discrete-valued"))
            .collect(),
    ))
}

fn expression_mentions_discrete_value(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> bool {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references
        .iter()
        .any(|name| matches!(roles.get(name), Some(PlannedRole::DiscreteValue)))
}

fn assignment_side_mentions_discrete_value(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> bool {
    match expression {
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches
                .iter()
                .any(|(_, value)| assignment_side_mentions_discrete_value(value, roles))
                || assignment_side_mentions_discrete_value(else_branch, roles)
        }
        _ => expression_mentions_discrete_value(expression, roles),
    }
}

fn invalid_discrete_lhs(span: Span) -> ToDaeError {
    ToDaeError::discrete_solved_form_violation(
        "a discrete-valued equation must have one unsubscripted resolved coordinate as its left-hand side",
        span,
    )
}

fn assignment_target_names(expression: &Expression) -> Vec<&VarName> {
    let mut targets = Vec::new();
    collect_assignment_target_names(expression, &mut targets);
    targets
}

fn collect_assignment_target_names<'flat>(
    expression: &'flat Expression,
    targets: &mut Vec<&'flat VarName>,
) {
    match expression {
        Expression::VarRef { name, .. } => targets.push(name.var_name()),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1 => collect_assignment_target_names(&args[0], targets),
        Expression::Unary { rhs, .. } | Expression::Index { base: rhs, .. } => {
            collect_assignment_target_names(rhs, targets);
        }
        Expression::Tuple { elements, .. } | Expression::Array { elements, .. } => {
            for element in elements {
                collect_assignment_target_names(element, targets);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (_, value) in branches {
                collect_assignment_target_names(value, targets);
            }
            collect_assignment_target_names(else_branch, targets);
        }
        Expression::Literal { .. }
        | Expression::Binary { .. }
        | Expression::BuiltinCall { .. }
        | Expression::FunctionCall { .. }
        | Expression::StringConversion { .. }
        | Expression::ArrayComprehension { .. }
        | Expression::Range { .. }
        | Expression::FieldAccess { .. }
        | Expression::Empty { .. } => {}
    }
}

pub(super) fn defined_discrete_targets(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &HashMap<VarName, usize>,
    aggregate_connections: &AggregateDiscreteConnections,
) -> Result<HashSet<VarName>, ToDaeError> {
    let mut targets = event_targets(flat);
    targets.extend(algorithm_targets(flat).into_iter().filter(|target| {
        matches!(
            roles.get(target),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        )
    }));
    for (row, equation) in flat.equations.iter().enumerate() {
        match equation_partition(
            flat,
            row,
            equation,
            roles,
            connection_ranks,
            aggregate_connections,
        )? {
            EquationPartition::Continuous => continue,
            EquationPartition::DiscreteReal { target } => {
                targets.insert(target.clone());
            }
            EquationPartition::DiscreteValue(plan) => {
                targets.insert(plan.target.clone());
            }
            EquationPartition::ConsumedDiscreteValue => {}
        }
        // Every discrete coordinate the equation touches is an unknown of the
        // discrete system it belongs to, not a held value. The classified
        // target is only the side the solved form assigns; a connection row
        // (`a - b`) defines the other side just as much, and dropping it would
        // count the equation without counting its unknown.
        targets.extend(discrete_references(&equation.residual, roles));
    }
    Ok(targets)
}

fn discrete_references(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> Vec<VarName> {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references.retain(|name| {
        matches!(
            roles.get(name),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        )
    });
    references
}
