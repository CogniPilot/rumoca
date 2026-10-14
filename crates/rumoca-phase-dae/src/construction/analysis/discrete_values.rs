use super::discrete_time_definitions::{DefinitionTime, discrete_definition_time};
use super::observed_reads::LazyModelReads;
use super::*;
use std::collections::BTreeSet;

#[derive(Debug)]
pub(in crate::construction) struct DiscreteValueTopologyPlan {
    ordered_owners: Vec<Vec<DiscreteTargetKey>>,
    held_targets: Vec<HeldTargetPlan>,
    /// Per ordered owner: every target is an unread observation whose
    /// definition is not a discrete-time expression.
    observed_owners: Vec<bool>,
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
    pub(in crate::construction) selection: Option<SelectedWhenTarget>,
}

impl DiscreteValueTopologyPlan {
    pub(in crate::construction) fn ordered_owners(&self) -> &[Vec<DiscreteTargetKey>] {
        &self.ordered_owners
    }

    pub(in crate::construction) fn held_targets(&self) -> &[HeldTargetPlan] {
        &self.held_targets
    }

    /// Whether ordered owner `owner` defines only unread observations.
    pub(in crate::construction) fn owner_is_observed(&self, owner: usize) -> bool {
        self.observed_owners.get(owner).copied().unwrap_or(false)
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
    dependencies: HashSet<DiscreteTargetKey>,
    span: Span,
    ordered_scalar_self_dependencies: bool,
    selection: Option<SelectedWhenTarget>,
}

impl SourceTarget {
    fn key(&self, flat: &flat::Model) -> DiscreteTargetKey {
        self.selection.as_ref().map_or_else(
            || DiscreteTargetKey::Whole(flat.variables[&self.name].instance_id),
            SelectedWhenTarget::key,
        )
    }
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
    templates: &TemplateSelection,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &DiscreteConnectionRanks,
    aggregate_connections: &AggregateDiscreteConnections,
    record_equations: &HashMap<usize, RecordEquationPlan>,
) -> Result<DiscreteValueTopologyPlan, ToDaeError> {
    let mut owners = Vec::new();
    let reads = LazyModelReads::new(flat, templates);
    let mut observed = HashSet::new();
    collect_binding_owners(flat, roles, &reads, &mut observed, &mut owners)?;
    collect_record_equation_owners(flat, roles, record_equations, &mut owners);
    collect_equation_owners(
        flat,
        roles,
        connection_ranks,
        aggregate_connections,
        &reads,
        &mut observed,
        &mut owners,
    )?;
    collect_algorithm_owners(flat, roles, &mut owners)?;
    collect_when_owners(flat, roles, &mut owners)?;
    let held_targets = add_held_owners(flat, roles, &mut owners)?;
    order_owners(flat, owners, held_targets, &observed)
}

/// One owner per record equation with discrete-valued fields: the fields are
/// assigned together from the equation's right side, in row order.
fn collect_record_equation_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    record_equations: &HashMap<usize, RecordEquationPlan>,
    owners: &mut Vec<SourceOwner>,
) {
    let mut rows = record_equations.keys().copied().collect::<Vec<_>>();
    rows.sort_unstable();
    for row in rows {
        let equation = &flat.equations[row];
        let Expression::Binary { rhs, .. } = &equation.residual else {
            continue;
        };
        let plan = &record_equations[&row];
        let mut dependencies = current_discrete_dependencies(flat, rhs, roles);
        // A record-to-record equation reads its source fields by coordinate.
        dependencies.extend(plan.fields.iter().filter_map(|field| match &field.value {
            RecordEquationFieldValue::Coordinate(source)
                if matches!(roles.get(source), Some(PlannedRole::DiscreteValue)) =>
            {
                Some(DiscreteTargetKey::Whole(flat.variables[source].instance_id))
            }
            _ => None,
        }));
        let targets = plan
            .discrete_value_targets(roles)
            .map(|target| SourceTarget {
                name: target.clone(),
                dependencies: dependencies.clone(),
                span: equation.span,
                ordered_scalar_self_dependencies: false,
                selection: None,
            })
            .collect::<Vec<_>>();
        if !targets.is_empty() {
            owners.push(SourceOwner {
                targets,
                span: equation.span,
            });
        }
    }
}

fn collect_binding_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    reads: &LazyModelReads<'_>,
    observed: &mut HashSet<VarName>,
    owners: &mut Vec<SourceOwner>,
) -> Result<(), ToDaeError> {
    for (name, variable) in &flat.variables {
        let Some(binding) = &variable.binding else {
            continue;
        };
        if !matches!(roles[name], PlannedRole::DiscreteValue) {
            continue;
        }
        if discrete_definition_time(
            flat,
            roles,
            reads,
            &[name],
            binding,
            None,
            expression_span(binding)?,
        )? == DefinitionTime::Observed
        {
            observed.insert(name.clone());
        }
        owners.push(SourceOwner {
            targets: vec![SourceTarget {
                name: name.clone(),
                dependencies: current_discrete_dependencies(flat, binding, roles),
                span: expression_span(binding)?,
                ordered_scalar_self_dependencies: false,
                selection: None,
            }],
            span: variable.source_span,
        });
    }
    Ok(())
}

fn collect_equation_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &DiscreteConnectionRanks,
    aggregate_connections: &AggregateDiscreteConnections,
    reads: &LazyModelReads<'_>,
    observed: &mut HashSet<VarName>,
    owners: &mut Vec<SourceOwner>,
) -> Result<(), ToDaeError> {
    for (row, equation) in flat.equations.iter().enumerate() {
        let plans = match equation_partition(
            flat,
            row,
            equation,
            roles,
            connection_ranks,
            aggregate_connections,
        )? {
            EquationPartition::DiscreteValue(plan) => vec![plan],
            EquationPartition::DiscreteElements(plans) => plans,
            EquationPartition::MultiOutput { receivers, call } => {
                let targets = receivers
                    .iter()
                    .filter(|receiver| {
                        matches!(roles.get(*receiver), Some(PlannedRole::DiscreteValue))
                    })
                    .collect::<Vec<_>>();
                if !targets.is_empty()
                    && discrete_definition_time(
                        flat,
                        roles,
                        reads,
                        &targets,
                        call,
                        Some(row),
                        equation.span,
                    )? == DefinitionTime::Observed
                {
                    observed.extend(targets.into_iter().cloned());
                }
                push_multi_output_owner(flat, equation, call, &receivers, roles, owners);
                continue;
            }
            _ => continue,
        };
        for plan in plans {
            if discrete_definition_time(
                flat,
                roles,
                reads,
                &[plan.target],
                plan.value.as_ref(),
                Some(row),
                equation.span,
            )? == DefinitionTime::Observed
            {
                observed.insert(plan.target.clone());
            }
            owners.push(SourceOwner {
                targets: vec![SourceTarget {
                    name: plan.target.clone(),
                    dependencies: current_discrete_dependencies(flat, plan.value.as_ref(), roles),
                    span: equation.span,
                    ordered_scalar_self_dependencies: plan.ordered_scalar_self_dependencies,
                    selection: None,
                }],
                span: equation.span,
            });
        }
    }
    Ok(())
}

/// One MLS §12.4.3 multi-result equation defines all its discrete-valued
/// receivers from one call, so they share one owner whose dependencies are the
/// call's current discrete reads.
fn push_multi_output_owner(
    flat: &flat::Model,
    equation: &flat::Equation,
    call: &Expression,
    receivers: &[VarName],
    roles: &HashMap<VarName, PlannedRole>,
    owners: &mut Vec<SourceOwner>,
) {
    let dependencies = current_discrete_dependencies(flat, call, roles);
    let targets = receivers
        .iter()
        .filter(|receiver| matches!(roles.get(*receiver), Some(PlannedRole::DiscreteValue)))
        .map(|receiver| SourceTarget {
            name: receiver.clone(),
            dependencies: dependencies.clone(),
            span: equation.span,
            ordered_scalar_self_dependencies: false,
            selection: None,
        })
        .collect::<Vec<_>>();
    if !targets.is_empty() {
        owners.push(SourceOwner {
            targets,
            span: equation.span,
        });
    }
}

fn collect_algorithm_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    owners: &mut Vec<SourceOwner>,
) -> Result<(), ToDaeError> {
    for algorithm in &flat.algorithms {
        let target_names =
            stable_discrete_targets(flat, roles, model_algorithm_targets(flat, algorithm));
        let target_instances = target_names
            .iter()
            .map(|name| flat.variables[name].instance_id)
            .collect::<HashSet<_>>();
        let event_algorithm =
            super::model_algorithms::contains_event_control(&algorithm.statements);
        if target_names.is_empty() {
            continue;
        }
        let targets = target_names
            .into_iter()
            .map(|name| {
                let mut dependencies = HashSet::new();
                let occurrence = collect_statement_target_dependencies(
                    flat,
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
                dependencies = dependencies
                    .into_iter()
                    .map(|dependency| algorithm_owned_dependency(dependency, &target_instances))
                    .collect();
                // MLS 3.7 §11.1.2: an event algorithm starts every discrete
                // target at its `pre` value (lowering seeds it so), so a read
                // before the target's own definition is a history read, not a
                // current-value self-dependency (`last := f(last)` in a `when`).
                let ordered_scalar_self_dependencies = dependencies
                    .contains(&DiscreteTargetKey::Whole(flat.variables[&name].instance_id))
                    && (event_algorithm
                        || !algorithm_reads_target_before_definition(
                            flat,
                            &algorithm.statements,
                            &name,
                            roles,
                            false,
                        )
                        .0);
                Ok(SourceTarget {
                    name,
                    dependencies,
                    span,
                    ordered_scalar_self_dependencies,
                    selection: None,
                })
            })
            .collect::<Result<Vec<_>, ToDaeError>>()?;
        if event_algorithm && owns_targets_separately(flat, &algorithm.statements, &targets) {
            owners.extend(targets.into_iter().map(|target| SourceOwner {
                targets: vec![target],
                span: algorithm.span,
            }));
        } else {
            owners.push(SourceOwner {
                targets,
                span: algorithm.span,
            });
        }
    }
    Ok(())
}

/// A model algorithm owns its written declarations as complete SSA values.
/// Keep exact selections for reads of declarations outside that algorithm.
fn algorithm_owned_dependency(
    dependency: DiscreteTargetKey,
    targets: &HashSet<InstanceId>,
) -> DiscreteTargetKey {
    if targets.contains(&dependency.instance()) {
        DiscreteTargetKey::Whole(dependency.instance())
    } else {
        dependency
    }
}

/// Whether the discrete-valued targets of one event algorithm own their
/// values one target at a time.
///
/// Every target's B.1c branches already carry its own values (statements
/// read each other through their SSA values, not through the variables), so
/// the targets own themselves separately whenever their current-value reads
/// order them acyclically and no condition reads a value a `when` body
/// writes (that value is resolved by event iteration within one owner, as in
/// the equation form). Separate owners let simultaneously active statements
/// that write disjoint targets each define their own (MLS 3.7 §11.1.2), and
/// let a `when` condition that reads a target see it issued before the
/// guarded targets. One owner, whose first active branch defines every
/// target, remains only for targets whose reads are cyclic.
fn owns_targets_separately(
    flat: &flat::Model,
    statements: &[rumoca_core::Statement],
    targets: &[SourceTarget],
) -> bool {
    let names = targets
        .iter()
        .map(|target| target.name.clone())
        .collect::<HashSet<_>>();
    let mut condition_reads = Vec::new();
    let mut body_writes = HashSet::new();
    collect_when_condition_reads(statements, &mut condition_reads, &mut body_writes, false);
    let read_targets = condition_reads
        .iter()
        .filter(|name| names.contains(*name))
        .collect::<Vec<_>>();
    if read_targets.iter().any(|name| body_writes.contains(*name)) {
        return false;
    }
    targets_order_acyclically(flat, targets)
}

fn collect_when_condition_reads(
    statements: &[rumoca_core::Statement],
    reads: &mut Vec<VarName>,
    body_writes: &mut HashSet<VarName>,
    in_when: bool,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } if in_when => {
                body_writes.insert(
                    rumoca_core::component_ref_to_base_reference(comp)
                        .var_name()
                        .clone(),
                );
            }
            rumoca_core::Statement::FunctionCall { outputs, .. } if in_when => {
                body_writes.extend(outputs.iter().flatten().map(|output| output.to_var_name()));
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    block.cond.collect_var_refs(reads);
                    collect_when_condition_reads(&block.stmts, reads, body_writes, true);
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_when_condition_reads(&block.stmts, reads, body_writes, in_when);
                }
                if let Some(block) = else_block {
                    collect_when_condition_reads(block, reads, body_writes, in_when);
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_when_condition_reads(equations, reads, body_writes, in_when);
            }
            _ => {}
        }
    }
}

/// Whether the current-value reads among one algorithm's targets admit an order.
fn targets_order_acyclically(flat: &flat::Model, targets: &[SourceTarget]) -> bool {
    let names = targets
        .iter()
        .map(|target| target.key(flat))
        .collect::<HashSet<_>>();
    let mut remaining = targets
        .iter()
        .map(|target| {
            let reads = target
                .dependencies
                .iter()
                .filter(|dependency| {
                    *dependency != &target.key(flat) && names.contains(*dependency)
                })
                .cloned()
                .collect::<HashSet<_>>();
            (target.key(flat), reads)
        })
        .collect::<Vec<_>>();
    while !remaining.is_empty() {
        let Some(ready) = remaining.iter().position(|(_, reads)| reads.is_empty()) else {
            return false;
        };
        let (issued, _) = remaining.swap_remove(ready);
        for (_, reads) in &mut remaining {
            reads.remove(&issued);
        }
    }
    true
}

/// Prove whether a model-algorithm target reads its entry value before an SSA
/// definition dominates that read.
///
/// The returned pair is `(entry_read, definitely_written_on_exit)`.  Reads
/// hidden beneath `pre`/`previous` are historical and therefore absent from
/// `current_discrete_dependencies`.  Branch exits join with logical `and`, so
/// a later read is admitted only when every path supplied a value.
fn algorithm_reads_target_before_definition(
    flat: &flat::Model,
    statements: &[rumoca_core::Statement],
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    mut written: bool,
) -> (bool, bool) {
    let mut entry_read = false;
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                entry_read |=
                    !written && expression_reads_current_target(flat, value, target, roles);
                if rumoca_core::component_ref_to_base_reference(comp).var_name() == target {
                    written = true;
                }
            }
            rumoca_core::Statement::FunctionCall { args, outputs, .. } => {
                entry_read |= !written
                    && args.iter().any(|argument| {
                        expression_reads_current_target(flat, argument, target, roles)
                    });
                if outputs
                    .iter()
                    .flatten()
                    .any(|output| source_target_contains(&output.to_var_name(), target))
                {
                    written = true;
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                entry_read |= !written
                    && cond_blocks.iter().any(|block| {
                        expression_reads_current_target(flat, &block.cond, target, roles)
                    });
                let mut exits = Vec::with_capacity(cond_blocks.len() + 1);
                for block in cond_blocks {
                    let (branch_read, branch_written) = algorithm_reads_target_before_definition(
                        flat,
                        &block.stmts,
                        target,
                        roles,
                        written,
                    );
                    entry_read |= branch_read;
                    exits.push(branch_written);
                }
                match else_block {
                    Some(branch) => {
                        let (branch_read, branch_written) =
                            algorithm_reads_target_before_definition(
                                flat, branch, target, roles, written,
                            );
                        entry_read |= branch_read;
                        exits.push(branch_written);
                    }
                    None => exits.push(written),
                }
                written = exits.into_iter().all(|branch| branch);
            }
            rumoca_core::Statement::When { blocks, .. } => {
                entry_read |= !written
                    && blocks.iter().any(|block| {
                        expression_reads_current_target(flat, &block.cond, target, roles)
                    });
                for block in blocks {
                    let (branch_read, _) = algorithm_reads_target_before_definition(
                        flat,
                        &block.stmts,
                        target,
                        roles,
                        written,
                    );
                    entry_read |= branch_read;
                }
                // A when-body is atomic and its SSA values do not escape the
                // event occurrence into the surrounding statement sequence.
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                entry_read |= !written
                    && indices.iter().any(|index| {
                        expression_reads_current_target(flat, &index.range, target, roles)
                    });
                let (loop_read, _) = algorithm_reads_target_before_definition(
                    flat, equations, target, roles, written,
                );
                entry_read |= loop_read;
            }
            _ => {}
        }
    }
    (entry_read, written)
}

fn expression_reads_current_target(
    flat: &flat::Model,
    expression: &Expression,
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
) -> bool {
    current_discrete_dependencies(flat, expression, roles)
        .iter()
        .any(|dependency| dependency.instance() == flat.variables[target].instance_id)
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
        let mut element_targets = collect_element_when_targets(flat, chain, roles)?;
        if target_names.is_empty() && element_targets.is_empty() {
            continue;
        }
        let control_dependencies = when_control_dependencies(flat, chain, roles);
        let mut targets = target_names
            .into_iter()
            .map(|name| {
                let mut dependencies = control_dependencies.clone();
                let occurrence =
                    collect_when_target_dependencies(flat, chain, &name, roles, &mut dependencies);
                let span =
                    require_target_occurrence(&name, "when chain", chain.span(), occurrence)?;
                Ok(SourceTarget {
                    name,
                    dependencies,
                    span,
                    ordered_scalar_self_dependencies: false,
                    selection: None,
                })
            })
            .collect::<Result<Vec<_>, ToDaeError>>()?;
        targets.append(&mut element_targets);
        owners.push(SourceOwner {
            targets,
            span: chain.span(),
        });
    }
    Ok(())
}

fn collect_element_when_targets(
    flat: &flat::Model,
    chain: &flat::WhenChain,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<Vec<SourceTarget>, ToDaeError> {
    let mut selections = Vec::new();
    for branch in chain.branches() {
        collect_element_targets(flat, &branch.equations, roles, &mut selections)?;
    }
    let control = when_control_dependencies(flat, chain, roles);
    let mut targets: Vec<SourceTarget> = Vec::new();
    for (selection, value) in selections {
        let key = selection.key();
        let mut dependencies = current_discrete_dependencies(flat, value, roles);
        dependencies.extend(control.iter().cloned());
        if let Some(previous) = targets.iter_mut().find(|target| target.key(flat) == key) {
            previous.dependencies.extend(dependencies);
        } else {
            targets.push(SourceTarget {
                name: selection.name.clone(),
                span: selection.span,
                dependencies,
                ordered_scalar_self_dependencies: false,
                selection: Some(selection),
            });
        }
    }
    Ok(targets)
}

fn collect_element_targets<'a>(
    flat: &flat::Model,
    equations: &'a [flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
    selections: &mut Vec<(SelectedWhenTarget, &'a Expression)>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::AssignElement {
                target,
                subscripts,
                value,
                span,
                ..
            } => {
                let selected = when_elements::selected_target(flat, target, subscripts, *span)?;
                if matches!(roles.get(&selected.name), Some(PlannedRole::DiscreteValue)) {
                    selections.push((selected, value));
                }
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, equations) in branches {
                    collect_element_targets(flat, equations, roles, selections)?;
                }
                if let Some(equations) = else_branch {
                    collect_element_targets(flat, equations, roles, selections)?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn add_held_elements(
    flat: &flat::Model,
    owners: &mut Vec<SourceOwner>,
    held: &mut Vec<HeldTargetPlan>,
) -> Result<(), ToDaeError> {
    let mut selected = HashMap::<InstanceId, HashSet<Box<[i64]>>>::new();
    for target in owners.iter().flat_map(|owner| &owner.targets) {
        if let Some(selection) = &target.selection {
            selected
                .entry(selection.instance)
                .or_default()
                .insert(selection.indices.clone());
        }
    }
    for variable in flat.variables.values() {
        let Some(defined) = selected.get(&variable.instance_id) else {
            continue;
        };
        let invalid_extent = || {
            ToDaeError::unsupported_flat(
                "selected discrete array",
                "requires representable nonnegative scalar extents and scalar count",
                variable.source_span,
            )
        };
        let extents = variable
            .dims
            .iter()
            .map(|&extent| usize::try_from(extent).map_err(|_| invalid_extent()))
            .collect::<Result<Vec<_>, _>>()?;
        let count = extents.iter().try_fold(1usize, |count, &extent| {
            count.checked_mul(extent).ok_or_else(invalid_extent)
        })?;
        for scalar in 0..count {
            let mut remainder = scalar;
            let mut indices = vec![0; variable.dims.len()];
            for (index, &extent) in indices.iter_mut().zip(&variable.dims).rev() {
                *index = (remainder % extent as usize + 1) as i64;
                remainder /= extent as usize;
            }
            if defined.contains(indices.as_slice()) {
                continue;
            }
            let selection = SelectedWhenTarget {
                name: variable.name.clone(),
                instance: variable.instance_id,
                indices: indices.into_boxed_slice(),
                span: variable.source_span,
            };
            held.push(HeldTargetPlan {
                name: variable.name.clone(),
                declaration_span: variable.source_span,
                selection: Some(selection.clone()),
            });
            owners.push(SourceOwner {
                targets: vec![SourceTarget {
                    name: variable.name.clone(),
                    dependencies: HashSet::new(),
                    span: variable.source_span,
                    ordered_scalar_self_dependencies: false,
                    selection: Some(selection),
                }],
                span: variable.source_span,
            });
        }
    }
    Ok(())
}

fn add_held_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    owners: &mut Vec<SourceOwner>,
) -> Result<Vec<HeldTargetPlan>, ToDaeError> {
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
                selection: None,
            });
            owners.push(SourceOwner {
                targets: vec![SourceTarget {
                    name: name.clone(),
                    dependencies: HashSet::new(),
                    span: variable.source_span,
                    ordered_scalar_self_dependencies: false,
                    selection: None,
                }],
                span: variable.source_span,
            });
        }
    }
    add_held_elements(flat, owners, &mut held_targets)?;
    Ok(held_targets)
}

fn order_owners(
    flat: &flat::Model,
    mut owners: Vec<SourceOwner>,
    held_targets: Vec<HeldTargetPlan>,
    observed: &HashSet<VarName>,
) -> Result<DiscreteValueTopologyPlan, ToDaeError> {
    let mut owner_by_target = HashMap::new();
    for (owner_index, owner) in owners.iter().enumerate() {
        for target in &owner.targets {
            if owner_by_target
                .insert(target.key(flat), owner_index)
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
    expand_selected_dependencies(flat, &mut owners);
    for owner in &mut owners {
        order_owner_targets(flat, owner)?;
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

    let observed_owners = order
        .iter()
        .map(|&index| {
            owners[index]
                .targets
                .iter()
                .all(|target| observed.contains(&target.name))
        })
        .collect();
    let ordered_owners = order
        .into_iter()
        .map(|index| {
            owners[index]
                .targets
                .iter()
                .map(|target| target.key(flat))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    Ok(DiscreteValueTopologyPlan {
        ordered_owners,
        held_targets,
        observed_owners,
    })
}

fn expand_selected_dependencies(flat: &flat::Model, owners: &mut [SourceOwner]) {
    let mut selected: HashMap<InstanceId, Vec<DiscreteTargetKey>> = HashMap::new();
    let mut whole = HashSet::new();
    for target in owners.iter().flat_map(|owner| &owner.targets) {
        if target.selection.is_some() {
            selected
                .entry(flat.variables[&target.name].instance_id)
                .or_default()
                .push(target.key(flat));
        } else {
            whole.insert(flat.variables[&target.name].instance_id);
        }
    }
    for target in owners.iter_mut().flat_map(|owner| &mut owner.targets) {
        let mut dependencies = HashSet::new();
        for dependency in target.dependencies.drain() {
            match dependency {
                DiscreteTargetKey::Whole(ref name)
                    if selected.contains_key(name) && !whole.contains(name) =>
                {
                    dependencies.extend(selected[name].iter().cloned());
                }
                DiscreteTargetKey::Element { instance, .. } if whole.contains(&instance) => {
                    dependencies.insert(DiscreteTargetKey::Whole(instance));
                }
                other => {
                    dependencies.insert(other);
                }
            }
        }
        target.dependencies = dependencies;
    }
}

fn external_predecessors(
    owner_index: usize,
    owner: &SourceOwner,
    owner_by_target: &HashMap<DiscreteTargetKey, usize>,
) -> BTreeSet<usize> {
    owner
        .targets
        .iter()
        .flat_map(|target| &target.dependencies)
        .filter_map(|dependency| owner_by_target.get(dependency).copied())
        .filter(|&predecessor| predecessor != owner_index)
        .collect()
}

fn order_owner_targets(flat: &flat::Model, owner: &mut SourceOwner) -> Result<(), ToDaeError> {
    let target_by_name = owner
        .targets
        .iter()
        .enumerate()
        .map(|(index, target)| (target.key(flat), index))
        .collect::<HashMap<_, _>>();
    let mut outgoing = vec![BTreeSet::new(); owner.targets.len()];
    let mut indegree = vec![0usize; owner.targets.len()];
    for (target_index, target) in owner.targets.iter().enumerate() {
        let predecessors = target
            .dependencies
            .iter()
            .filter_map(|dependency| target_by_name.get(dependency).copied())
            .filter(|&predecessor| {
                predecessor != target_index || !target.ordered_scalar_self_dependencies
            })
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
    owner_by_target: &HashMap<DiscreteTargetKey, usize>,
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
    owner_by_target: &HashMap<DiscreteTargetKey, usize>,
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
    flat: &flat::Model,
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> HashSet<DiscreteTargetKey> {
    let mut dependencies = HashSet::new();
    collect_current_discrete_dependencies(flat, expression, roles, &mut dependencies);
    dependencies
}

fn collect_current_discrete_dependencies(
    flat: &flat::Model,
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<DiscreteTargetKey>,
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
        // MLS §16.5.1: `sample(u, c)` has "the value of the left limit of u
        // when c is active", so sampling a variable reads its pre value and no
        // current value of this instant.
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            ..
        } if args
            .first()
            .is_some_and(|value| matches!(value, Expression::VarRef { .. })) => {}
        Expression::Index {
            base, subscripts, ..
        } => {
            if let Expression::VarRef { name, .. } = base.as_ref()
                && matches!(roles.get(name.var_name()), Some(PlannedRole::DiscreteValue))
            {
                dependencies.insert(DiscreteTargetKey::from_reference(flat, name, expression));
            } else {
                collect_current_discrete_dependencies(flat, base, roles, dependencies);
            }
            for expr in subscripts.iter().filter_map(|subscript| match subscript {
                Subscript::Expr { expr, .. } => Some(expr.as_ref()),
                _ => None,
            }) {
                collect_current_discrete_dependencies(flat, expr, roles, dependencies);
            }
        }
        Expression::VarRef { name, .. } => {
            if matches!(roles.get(name.var_name()), Some(PlannedRole::DiscreteValue)) {
                dependencies.insert(DiscreteTargetKey::from_reference(flat, name, expression));
            }
            for child in expression_children(expression) {
                collect_current_discrete_dependencies(flat, child, roles, dependencies);
            }
        }
        _ => {
            for child in expression_children(expression) {
                collect_current_discrete_dependencies(flat, child, roles, dependencies);
            }
        }
    }
}

fn collect_statement_target_dependencies(
    flat: &flat::Model,
    statements: &[rumoca_core::Statement],
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<DiscreteTargetKey>,
) -> Option<Span> {
    statements.iter().fold(None, |first_action, statement| {
        let action =
            collect_statement_target_dependency(flat, statement, target, roles, dependencies);
        first_action.or(action)
    })
}

fn collect_statement_target_dependency(
    flat: &flat::Model,
    statement: &rumoca_core::Statement,
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<DiscreteTargetKey>,
) -> Option<Span> {
    match statement {
        rumoca_core::Statement::Assignment {
            comp, value, span, ..
        } => {
            let written = rumoca_core::component_ref_to_base_reference(comp)
                .var_name()
                .clone();
            source_target_contains(&written, target).then(|| {
                collect_current_discrete_dependencies(flat, value, roles, dependencies);
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
                    collect_current_discrete_dependencies(flat, argument, roles, dependencies);
                }
                *span
            }),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => collect_conditional_target_dependencies(
            flat,
            cond_blocks,
            else_block.as_deref(),
            target,
            roles,
            dependencies,
        ),
        rumoca_core::Statement::When { blocks, .. } => {
            collect_conditional_target_dependencies(flat, blocks, None, target, roles, dependencies)
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => collect_loop_target_dependencies(
            flat,
            equations,
            indices.iter().map(|index| &index.range),
            target,
            roles,
            dependencies,
        ),
        rumoca_core::Statement::While { block, .. } => collect_loop_target_dependencies(
            flat,
            &block.stmts,
            std::iter::once(&block.cond),
            target,
            roles,
            dependencies,
        ),
        _ => None,
    }
}

fn collect_conditional_target_dependencies(
    flat: &flat::Model,
    blocks: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<DiscreteTargetKey>,
) -> Option<Span> {
    let mut first_action = None;
    let mut prior_conditions = Vec::new();
    for block in blocks {
        let branch = collect_branch_target_dependencies(
            flat,
            &block.stmts,
            prior_conditions
                .iter()
                .copied()
                .chain(std::iter::once(&block.cond)),
            target,
            roles,
            dependencies,
        );
        first_action = first_action.or(branch);
        prior_conditions.push(&block.cond);
    }
    let fallback = fallback.and_then(|statements| {
        collect_branch_target_dependencies(
            flat,
            statements,
            prior_conditions.iter().copied(),
            target,
            roles,
            dependencies,
        )
    });
    first_action.or(fallback)
}

fn collect_branch_target_dependencies<'a>(
    flat: &flat::Model,
    statements: &[rumoca_core::Statement],
    conditions: impl Iterator<Item = &'a Expression>,
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<DiscreteTargetKey>,
) -> Option<Span> {
    let mut branch_dependencies = HashSet::new();
    let action = collect_statement_target_dependencies(
        flat,
        statements,
        target,
        roles,
        &mut branch_dependencies,
    )?;
    for condition in conditions {
        collect_current_discrete_dependencies(flat, condition, roles, dependencies);
    }
    dependencies.extend(branch_dependencies);
    Some(action)
}

fn collect_loop_target_dependencies<'a>(
    flat: &flat::Model,
    statements: &[rumoca_core::Statement],
    controls: impl Iterator<Item = &'a Expression>,
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<DiscreteTargetKey>,
) -> Option<Span> {
    collect_branch_target_dependencies(flat, statements, controls, target, roles, dependencies)
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
            flat::WhenEquation::AssignElement { .. } => {}
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
    if !matches!(
        roles.get(target),
        Some(PlannedRole::DiscreteValue | PlannedRole::DiscreteReal)
    ) || written.insert(target.clone())
    {
        return Ok(());
    }
    Err(ToDaeError::discrete_solved_form_violation(
        format!("`{target}` is assigned more than once on one when branch path"),
        span,
    ))
}

fn when_control_dependencies(
    flat: &flat::Model,
    chain: &flat::WhenChain,
    roles: &HashMap<VarName, PlannedRole>,
) -> HashSet<DiscreteTargetKey> {
    let mut dependencies = HashSet::new();
    for branch in chain.branches() {
        collect_current_discrete_dependencies(flat, &branch.condition, roles, &mut dependencies);
        collect_when_control_dependencies(flat, &branch.equations, roles, &mut dependencies);
    }
    dependencies
}

fn collect_when_control_dependencies(
    flat: &flat::Model,
    equations: &[flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<DiscreteTargetKey>,
) {
    for equation in equations {
        if let flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } = equation
        {
            for (condition, equations) in branches {
                collect_current_discrete_dependencies(flat, condition, roles, dependencies);
                collect_when_control_dependencies(flat, equations, roles, dependencies);
            }
            if let Some(else_branch) = else_branch {
                collect_when_control_dependencies(flat, else_branch, roles, dependencies);
            }
        }
    }
}

fn collect_when_target_dependencies(
    flat: &flat::Model,
    chain: &flat::WhenChain,
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<DiscreteTargetKey>,
) -> Option<Span> {
    let mut first_action = None;
    for branch in chain.branches() {
        first_action = first_action.or(collect_when_equation_target_dependencies(
            flat,
            &branch.equations,
            target,
            roles,
            dependencies,
        ));
    }
    first_action
}

fn collect_when_equation_target_dependencies(
    flat: &flat::Model,
    equations: &[flat::WhenEquation],
    target: &VarName,
    roles: &HashMap<VarName, PlannedRole>,
    dependencies: &mut HashSet<DiscreteTargetKey>,
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
                collect_current_discrete_dependencies(flat, value, roles, dependencies);
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
                        flat,
                        equations,
                        target,
                        roles,
                        dependencies,
                    ));
                }
                if let Some(else_branch) = else_branch {
                    nested = nested.or(collect_when_equation_target_dependencies(
                        flat,
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
                collect_current_discrete_dependencies(flat, function, roles, dependencies);
                Some(*span)
            }
            flat::WhenEquation::Assign { .. }
            | flat::WhenEquation::AssignElement { .. }
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
fn algorithm_dependencies_preserve_external_selections_and_normalize_owned_values() {
    let owned = InstanceId::new(41);
    let external = InstanceId::new(42);
    let targets = HashSet::from([owned]);
    for index in [1, 2] {
        let selection = DiscreteTargetKey::Element {
            instance: owned,
            indices: vec![index].into_boxed_slice(),
        };
        assert_eq!(selection.instance(), owned);
        assert_eq!(
            algorithm_owned_dependency(selection, &targets),
            DiscreteTargetKey::Whole(owned)
        );
        let external_selection = DiscreteTargetKey::Element {
            instance: external,
            indices: vec![index].into_boxed_slice(),
        };
        assert_eq!(
            algorithm_owned_dependency(external_selection.clone(), &targets),
            external_selection
        );
    }
}

#[cfg(test)]
#[test]
fn indexed_current_target_reads_retain_entry_value_classification() {
    let mut sources = SourceMap::new();
    let text = "x[1]";
    let source = sources.add("indexed_entry.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let instance = InstanceId::new(41);
    let name = VarName::new("x");
    let mut flat = flat::Model::new();
    let mut variable = flat::Variable::empty_with_span(span);
    variable.name = name.clone();
    variable.instance_id = instance;
    variable.dims = vec![2];
    flat.add_variable(name.clone(), variable);
    let roles = HashMap::from([(name.clone(), PlannedRole::DiscreteValue)]);
    let expression = Expression::VarRef {
        name: rumoca_core::Reference::new("x").with_instance_id(instance),
        subscripts: vec![Subscript::Index { value: 1, span }],
        span,
    };
    assert!(expression_reads_current_target(
        &flat,
        &expression,
        &name,
        &roles
    ));
    let pre = Expression::BuiltinCall {
        function: BuiltinFunction::Pre,
        args: vec![expression],
        span,
    };
    assert!(!expression_reads_current_target(&flat, &pre, &name, &roles));
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
