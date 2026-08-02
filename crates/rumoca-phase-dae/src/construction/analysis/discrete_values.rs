use super::*;
use std::collections::BTreeSet;

#[derive(Debug)]
pub(in crate::construction) struct DiscreteValueTopologyPlan {
    ordered_owners: Vec<Vec<VarName>>,
    order_by_target: HashMap<VarName, TargetOrder>,
    held_targets: Vec<HeldTargetPlan>,
}

#[derive(Clone, Copy, Debug)]
pub(in crate::construction) struct TargetOrder {
    pub(in crate::construction) owner: usize,
    pub(in crate::construction) target: usize,
}

#[derive(Debug)]
pub(in crate::construction) struct HeldTargetPlan {
    pub(in crate::construction) name: VarName,
    pub(in crate::construction) declaration_span: Span,
}

impl DiscreteValueTopologyPlan {
    pub(in crate::construction) fn ordered_owners(&self) -> &[Vec<VarName>] {
        &self.ordered_owners
    }

    pub(in crate::construction) fn target_order(&self, target: &VarName) -> Option<TargetOrder> {
        self.order_by_target.get(target).copied()
    }

    pub(in crate::construction) fn held_targets(&self) -> &[HeldTargetPlan] {
        &self.held_targets
    }

    pub(in crate::construction) fn matches_owner_targets(
        &self,
        owner: usize,
        target_count: usize,
        target_ordinals: impl Iterator<Item = usize>,
    ) -> bool {
        self.ordered_owners.get(owner).is_some_and(|expected| {
            expected.len() == target_count
                && target_ordinals
                    .enumerate()
                    .all(|(ordinal, target)| target == ordinal)
        })
    }
}

struct SourceOwner {
    targets: Vec<SourceTarget>,
    span: Span,
}

struct SourceTarget {
    name: VarName,
    dependencies: HashSet<VarName>,
    span: Span,
}

fn require_target_occurrence(
    target: &VarName,
    owner_kind: &'static str,
    owner_span: Span,
    occurrence: Option<Span>,
) -> Result<Span, ToDaeError> {
    occurrence.ok_or_else(|| {
        ToDaeError::discrete_solved_form_violation(
            format!(
                "{owner_kind} planned discrete-value target `{target}` without a source action \
                 occurrence"
            ),
            owner_span,
        )
    })
}

pub(super) fn analyze_discrete_value_topology(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &HashMap<VarName, usize>,
    aggregate_connections: &AggregateDiscreteConnections,
) -> Result<DiscreteValueTopologyPlan, ToDaeError> {
    let mut owners = Vec::new();
    collect_binding_owners(flat, roles, &mut owners)?;
    collect_equation_owners(
        flat,
        roles,
        connection_ranks,
        aggregate_connections,
        &mut owners,
    )?;
    collect_algorithm_owners(flat, roles, &mut owners)?;
    collect_when_owners(flat, roles, &mut owners)?;
    let held_targets = add_held_owners(flat, roles, &mut owners);
    order_owners(owners, held_targets)
}

fn collect_binding_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    owners: &mut Vec<SourceOwner>,
) -> Result<(), ToDaeError> {
    for (name, variable) in &flat.variables {
        let Some(binding) = &variable.binding else {
            continue;
        };
        if !matches!(roles[name], PlannedRole::DiscreteValue) {
            continue;
        }
        owners.push(SourceOwner {
            targets: vec![SourceTarget {
                name: name.clone(),
                dependencies: current_discrete_dependencies(binding, roles),
                span: expression_span(binding)?,
            }],
            span: variable.source_span,
        });
    }
    Ok(())
}

fn collect_equation_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &HashMap<VarName, usize>,
    aggregate_connections: &AggregateDiscreteConnections,
    owners: &mut Vec<SourceOwner>,
) -> Result<(), ToDaeError> {
    for (row, equation) in flat.equations.iter().enumerate() {
        let EquationPartition::DiscreteValue(plan) = equation_partition(
            flat,
            row,
            equation,
            roles,
            connection_ranks,
            aggregate_connections,
        )?
        else {
            continue;
        };
        owners.push(SourceOwner {
            targets: vec![SourceTarget {
                name: plan.target.clone(),
                dependencies: current_discrete_dependencies(plan.value.as_ref(), roles),
                span: equation.span,
            }],
            span: equation.span,
        });
    }
    Ok(())
}

fn collect_algorithm_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    owners: &mut Vec<SourceOwner>,
) -> Result<(), ToDaeError> {
    for algorithm in &flat.algorithms {
        let target_names =
            stable_discrete_targets(flat, roles, model_algorithm_targets(flat, algorithm));
        if target_names.is_empty() {
            continue;
        }
        let control_dependencies = statement_control_dependencies(&algorithm.statements, roles);
        let targets = target_names
            .into_iter()
            .map(|name| {
                let mut dependencies = control_dependencies.clone();
                let occurrence = collect_statement_target_dependencies(
                    &algorithm.statements,
                    &name,
                    roles,
                    &mut dependencies,
                );
                let span = require_target_occurrence(
                    &name,
                    "model algorithm",
                    algorithm.span,
                    occurrence,
                )?;
                Ok(SourceTarget {
                    name,
                    dependencies,
                    span,
                })
            })
            .collect::<Result<Vec<_>, ToDaeError>>()?;
        owners.push(SourceOwner {
            targets,
            span: algorithm.span,
        });
    }
    Ok(())
}

fn collect_when_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    owners: &mut Vec<SourceOwner>,
) -> Result<(), ToDaeError> {
    for chain in &flat.when_chains {
        let mut written = HashSet::new();
        for branch in chain.branches() {
            let mut branch_written = HashSet::new();
            validate_when_equation_targets(&branch.equations, roles, &mut branch_written)?;
            written.extend(branch_written);
        }
        let target_names = stable_discrete_targets(flat, roles, written);
        if target_names.is_empty() {
            continue;
        }
        let control_dependencies = when_control_dependencies(chain, roles);
        let targets = target_names
            .into_iter()
            .map(|name| {
                let mut dependencies = control_dependencies.clone();
                let occurrence =
                    collect_when_target_dependencies(chain, &name, roles, &mut dependencies);
                let span =
                    require_target_occurrence(&name, "when chain", chain.span(), occurrence)?;
                Ok(SourceTarget {
                    name,
                    dependencies,
                    span,
                })
            })
            .collect::<Result<Vec<_>, ToDaeError>>()?;
        owners.push(SourceOwner {
            targets,
            span: chain.span(),
        });
    }
    Ok(())
}

fn add_held_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    owners: &mut Vec<SourceOwner>,
) -> Vec<HeldTargetPlan> {
    let defined = owners
        .iter()
        .flat_map(|owner| owner.targets.iter().map(|target| target.name.clone()))
        .collect::<HashSet<_>>();
    let mut held_targets = Vec::new();
    for (name, variable) in &flat.variables {
        if matches!(roles[name], PlannedRole::DiscreteValue) && !defined.contains(name) {
            held_targets.push(HeldTargetPlan {
                name: name.clone(),
                declaration_span: variable.source_span,
            });
            owners.push(SourceOwner {
                targets: vec![SourceTarget {
                    name: name.clone(),
                    dependencies: HashSet::new(),
                    span: variable.source_span,
                }],
                span: variable.source_span,
            });
        }
    }
    held_targets
}

fn order_owners(
    mut owners: Vec<SourceOwner>,
    held_targets: Vec<HeldTargetPlan>,
) -> Result<DiscreteValueTopologyPlan, ToDaeError> {
    let mut owner_by_target = HashMap::new();
    for (owner_index, owner) in owners.iter().enumerate() {
        for target in &owner.targets {
            if owner_by_target
                .insert(target.name.clone(), owner_index)
                .is_some()
            {
                return Err(ToDaeError::discrete_solved_form_violation(
                    format!(
                        "`{}` has more than one semantic definition owner",
                        target.name
                    ),
                    owner.span,
                ));
            }
        }
    }
    for owner in &mut owners {
        order_owner_targets(owner)?;
    }

    let mut outgoing = vec![BTreeSet::new(); owners.len()];
    let mut indegree = vec![0usize; owners.len()];
    for (owner_index, owner) in owners.iter().enumerate() {
        let predecessors = external_predecessors(owner_index, owner, &owner_by_target);
        indegree[owner_index] = predecessors.len();
        for predecessor in predecessors {
            outgoing[predecessor].insert(owner_index);
        }
    }

    let mut ready = indegree
        .iter()
        .enumerate()
        .filter_map(|(index, &count)| (count == 0).then_some(index))
        .collect::<BTreeSet<_>>();
    let mut order = Vec::with_capacity(owners.len());
    while let Some(index) = ready.pop_first() {
        order.push(index);
        for &dependent in &outgoing[index] {
            indegree[dependent] -= 1;
            if indegree[dependent] == 0 {
                ready.insert(dependent);
            }
        }
    }
    if order.len() != owners.len() {
        let target = cyclic_external_target(&owners, &owner_by_target, &indegree);
        return Err(ToDaeError::discrete_solved_form_violation(
            format!(
                "`{}` participates in a current-value dependency cycle",
                target.name
            ),
            target.span,
        ));
    }

    let ordered_owners = order
        .into_iter()
        .map(|index| {
            owners[index]
                .targets
                .iter()
                .map(|target| target.name.clone())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let order_by_target = ordered_owners
        .iter()
        .enumerate()
        .flat_map(|(owner, targets)| {
            targets
                .iter()
                .cloned()
                .enumerate()
                .map(move |(target, name)| (name, TargetOrder { owner, target }))
        })
        .collect();
    Ok(DiscreteValueTopologyPlan {
        ordered_owners,
        order_by_target,
        held_targets,
    })
}

fn external_predecessors(
    owner_index: usize,
    owner: &SourceOwner,
    owner_by_target: &HashMap<VarName, usize>,
) -> BTreeSet<usize> {
    owner
        .targets
        .iter()
        .flat_map(|target| &target.dependencies)
        .filter_map(|dependency| owner_by_target.get(dependency).copied())
        .filter(|&predecessor| predecessor != owner_index)
        .collect()
}

fn order_owner_targets(owner: &mut SourceOwner) -> Result<(), ToDaeError> {
    let target_by_name = owner
        .targets
        .iter()
        .enumerate()
        .map(|(index, target)| (target.name.clone(), index))
        .collect::<HashMap<_, _>>();
    let mut outgoing = vec![BTreeSet::new(); owner.targets.len()];
    let mut indegree = vec![0usize; owner.targets.len()];
    for (target_index, target) in owner.targets.iter().enumerate() {
        let predecessors = target
            .dependencies
            .iter()
            .filter_map(|dependency| target_by_name.get(dependency).copied())
            .collect::<BTreeSet<_>>();
        indegree[target_index] = predecessors.len();
        for predecessor in predecessors {
            outgoing[predecessor].insert(target_index);
        }
    }
    let mut ready = indegree
        .iter()
        .enumerate()
        .filter_map(|(index, &count)| (count == 0).then_some(index))
        .collect::<BTreeSet<_>>();
    let mut order = Vec::with_capacity(owner.targets.len());
    while let Some(index) = ready.pop_first() {
        order.push(index);
        for &dependent in &outgoing[index] {
            indegree[dependent] -= 1;
            if indegree[dependent] == 0 {
                ready.insert(dependent);
            }
        }
    }
    if order.len() != owner.targets.len() {
        let target = owner
            .targets
            .iter()
            .zip(indegree)
            .find_map(|(target, count)| (count != 0).then_some(target))
            .expect("a short target order retains one cyclic target");
        return Err(ToDaeError::discrete_solved_form_violation(
            format!(
                "`{}` participates in an internal current-value dependency cycle",
                target.name
            ),
            target.span,
        ));
    }
    let mut targets = std::mem::take(&mut owner.targets)
        .into_iter()
        .map(Some)
        .collect::<Vec<_>>();
    owner.targets = order
        .into_iter()
        .map(|index| {
            targets[index]
                .take()
                .expect("target order is a permutation")
        })
        .collect();
    Ok(())
}

fn cyclic_external_target<'owner>(
    owners: &'owner [SourceOwner],
    owner_by_target: &HashMap<VarName, usize>,
    indegree: &[usize],
) -> &'owner SourceTarget {
    owners
        .iter()
        .enumerate()
        .filter(|(owner, _)| indegree[*owner] != 0)
        .flat_map(|(owner, source)| {
            source.targets.iter().filter(move |target| {
                has_cyclic_external_predecessor(owner, target, owner_by_target, indegree)
            })
        })
        .next()
        .expect("a short owner order retains one target with an unissued predecessor")
}

fn has_cyclic_external_predecessor(
    owner: usize,
    target: &SourceTarget,
    owner_by_target: &HashMap<VarName, usize>,
    indegree: &[usize],
) -> bool {
    target
        .dependencies
        .iter()
        .filter_map(|dependency| owner_by_target.get(dependency).copied())
        .any(|predecessor| predecessor != owner && indegree[predecessor] != 0)
}

fn stable_discrete_targets(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    targets: impl IntoIterator<Item = VarName>,
) -> Vec<VarName> {
    let targets = targets.into_iter().collect::<HashSet<_>>();
    flat.variables
        .keys()
        .filter(|name| {
            targets.contains(*name) && matches!(roles[*name], PlannedRole::DiscreteValue)
        })
        .cloned()
        .collect()
}

fn current_discrete_dependencies(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> HashSet<VarName> {
    let mut dependencies = HashSet::new();
    collect_current_discrete_dependencies(expression, roles, &mut dependencies);
    dependencies
}

fn collect_current_discrete_dependencies(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<VarName>,
) {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            ..
        } => {}
        Expression::BuiltinCall {
            function: BuiltinFunction::Previous,
            ..
        } => {}
        Expression::VarRef { name, .. } => {
            if matches!(roles.get(name.var_name()), Some(PlannedRole::DiscreteValue)) {
                dependencies.insert(name.var_name().clone());
            }
            for child in expression_children(expression) {
                collect_current_discrete_dependencies(child, roles, dependencies);
            }
        }
        _ => {
            for child in expression_children(expression) {
                collect_current_discrete_dependencies(child, roles, dependencies);
            }
        }
    }
}

fn statement_control_dependencies(
    statements: &[rumoca_core::Statement],
    roles: &HashMap<VarName, PlannedRole>,
) -> HashSet<VarName> {
    let mut dependencies = HashSet::new();
    collect_statement_control_dependencies(statements, roles, &mut dependencies);
    dependencies
}

fn collect_statement_control_dependencies(
    statements: &[rumoca_core::Statement],
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<VarName>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_current_discrete_dependencies(&block.cond, roles, dependencies);
                    collect_statement_control_dependencies(&block.stmts, roles, dependencies);
                }
                if let Some(fallback) = else_block {
                    collect_statement_control_dependencies(fallback, roles, dependencies);
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    collect_current_discrete_dependencies(&block.cond, roles, dependencies);
                    collect_statement_control_dependencies(&block.stmts, roles, dependencies);
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_statement_control_dependencies(equations, roles, dependencies);
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_current_discrete_dependencies(&block.cond, roles, dependencies);
                collect_statement_control_dependencies(&block.stmts, roles, dependencies);
            }
            _ => {}
        }
    }
}

fn collect_statement_target_dependencies(
    statements: &[rumoca_core::Statement],
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<VarName>,
) -> Option<Span> {
    let mut first_action = None;
    for statement in statements {
        let action = match statement {
            rumoca_core::Statement::Assignment {
                comp, value, span, ..
            } => {
                let written = rumoca_core::component_ref_to_base_reference(comp)
                    .var_name()
                    .clone();
                source_target_contains(&written, target).then(|| {
                    collect_current_discrete_dependencies(value, roles, dependencies);
                    *span
                })
            }
            rumoca_core::Statement::FunctionCall {
                args,
                outputs,
                span,
                ..
            } => outputs
                .iter()
                .flatten()
                .any(|output| source_target_contains(&output.to_var_name(), target))
                .then(|| {
                    for argument in args {
                        collect_current_discrete_dependencies(argument, roles, dependencies);
                    }
                    *span
                }),
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                let mut nested = None;
                for block in cond_blocks {
                    nested = nested.or(collect_statement_target_dependencies(
                        &block.stmts,
                        target,
                        roles,
                        dependencies,
                    ));
                }
                if let Some(fallback) = else_block {
                    nested = nested.or(collect_statement_target_dependencies(
                        fallback,
                        target,
                        roles,
                        dependencies,
                    ));
                }
                nested
            }
            rumoca_core::Statement::When { blocks, .. } => {
                let mut nested = None;
                for block in blocks {
                    nested = nested.or(collect_statement_target_dependencies(
                        &block.stmts,
                        target,
                        roles,
                        dependencies,
                    ));
                }
                nested
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_statement_target_dependencies(equations, target, roles, dependencies)
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_statement_target_dependencies(&block.stmts, target, roles, dependencies)
            }
            _ => None,
        };
        first_action = first_action.or(action);
    }
    first_action
}

fn source_target_contains(source: &VarName, concrete: &VarName) -> bool {
    source == concrete
        || concrete
            .as_str()
            .strip_prefix(source.as_str())
            .is_some_and(|suffix| suffix.starts_with('.'))
}

fn validate_when_equation_targets(
    equations: &[flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
    written: &mut HashSet<VarName>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, span, .. } => {
                claim_when_equation_target(target, *span, roles, written)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                let mut branch_targets = HashSet::new();
                for (_, equations) in branches {
                    let mut branch_written = written.clone();
                    validate_when_equation_targets(equations, roles, &mut branch_written)?;
                    branch_targets.extend(branch_written.difference(written).cloned());
                }
                if let Some(else_branch) = else_branch {
                    let mut branch_written = written.clone();
                    validate_when_equation_targets(else_branch, roles, &mut branch_written)?;
                    branch_targets.extend(branch_written.difference(written).cloned());
                }
                written.extend(branch_targets);
            }
            flat::WhenEquation::FunctionCallOutputs { outputs, span, .. } => {
                for output in outputs {
                    claim_when_equation_target(output, *span, roles, written)?;
                }
            }
            flat::WhenEquation::Reinit { .. }
            | flat::WhenEquation::Assert { .. }
            | flat::WhenEquation::Terminate { .. } => {}
        }
    }
    Ok(())
}

fn claim_when_equation_target(
    target: &VarName,
    span: Span,
    roles: &HashMap<VarName, PlannedRole>,
    written: &mut HashSet<VarName>,
) -> Result<(), ToDaeError> {
    if !matches!(roles.get(target), Some(PlannedRole::DiscreteValue))
        || written.insert(target.clone())
    {
        return Ok(());
    }
    Err(ToDaeError::discrete_solved_form_violation(
        format!("`{target}` is assigned more than once on one when branch path"),
        span,
    ))
}

fn when_control_dependencies(
    chain: &flat::WhenChain,
    roles: &HashMap<VarName, PlannedRole>,
) -> HashSet<VarName> {
    let mut dependencies = HashSet::new();
    for branch in chain.branches() {
        collect_current_discrete_dependencies(&branch.condition, roles, &mut dependencies);
        collect_when_control_dependencies(&branch.equations, roles, &mut dependencies);
    }
    dependencies
}

fn collect_when_control_dependencies(
    equations: &[flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<VarName>,
) {
    for equation in equations {
        if let flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } = equation
        {
            for (condition, equations) in branches {
                collect_current_discrete_dependencies(condition, roles, dependencies);
                collect_when_control_dependencies(equations, roles, dependencies);
            }
            if let Some(else_branch) = else_branch {
                collect_when_control_dependencies(else_branch, roles, dependencies);
            }
        }
    }
}

fn collect_when_target_dependencies(
    chain: &flat::WhenChain,
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<VarName>,
) -> Option<Span> {
    let mut first_action = None;
    for branch in chain.branches() {
        first_action = first_action.or(collect_when_equation_target_dependencies(
            &branch.equations,
            target,
            roles,
            dependencies,
        ));
    }
    first_action
}

fn collect_when_equation_target_dependencies(
    equations: &[flat::WhenEquation],
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<VarName>,
) -> Option<Span> {
    let mut first_action = None;
    for equation in equations {
        let action = match equation {
            flat::WhenEquation::Assign {
                target: written,
                value,
                span,
                ..
            } if written == target => {
                collect_current_discrete_dependencies(value, roles, dependencies);
                Some(*span)
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                let mut nested = None;
                for (_, equations) in branches {
                    nested = nested.or(collect_when_equation_target_dependencies(
                        equations,
                        target,
                        roles,
                        dependencies,
                    ));
                }
                if let Some(else_branch) = else_branch {
                    nested = nested.or(collect_when_equation_target_dependencies(
                        else_branch,
                        target,
                        roles,
                        dependencies,
                    ));
                }
                nested
            }
            flat::WhenEquation::FunctionCallOutputs {
                outputs,
                function,
                span,
                ..
            } if outputs.contains(target) => {
                collect_current_discrete_dependencies(function, roles, dependencies);
                Some(*span)
            }
            flat::WhenEquation::Assign { .. }
            | flat::WhenEquation::Reinit { .. }
            | flat::WhenEquation::Assert { .. }
            | flat::WhenEquation::Terminate { .. }
            | flat::WhenEquation::FunctionCallOutputs { .. } => None,
        };
        first_action = first_action.or(action);
    }
    first_action
}

#[cfg(test)]
#[test]
fn missing_planned_target_occurrence_is_a_typed_owner_error() {
    let text = "algorithm m := true; end algorithm";
    let mut sources = SourceMap::new();
    let source = sources.add("missing_discrete_action.mo", text);
    let owner_span = Span::from_offsets(source, 0, text.len());
    let target = VarName::new("m");

    for owner_kind in ["model algorithm", "when chain"] {
        let error = require_target_occurrence(&target, owner_kind, owner_span, None).unwrap_err();
        assert!(matches!(
            error,
            ToDaeError::DiscreteSolvedFormViolation { .. }
        ));
        if let ToDaeError::DiscreteSolvedFormViolation { detail, span } = error {
            assert_eq!(span, owner_span);
            assert!(detail.contains(owner_kind));
            assert!(detail.contains("`m`"));
            assert!(detail.contains("without a source action occurrence"));
        }
    }
}
