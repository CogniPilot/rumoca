use super::*;
use std::collections::hash_map::Entry;

#[derive(Clone, Copy)]
pub(super) struct DiscreteValueOwnerHandle {
    first: usize,
    end: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum BranchActivation<'dae> {
    Always,
    When {
        trigger: dae::ConditionId<'dae>,
        guard: dae::ConditionId<'dae>,
    },
}

struct StagedBranch<'dae> {
    activation: BranchActivation<'dae>,
    values: Vec<Option<(dae::ExprId<'dae>, dae::DaeProvenance)>>,
    /// The targets this branch writes itself, as opposed to values it
    /// inherits from its parent branch or retains.
    written: Vec<bool>,
    /// The top-level algorithm statement the branch belongs to.
    statement: Option<Span>,
    provenance: dae::DaeProvenance,
}

struct StagedOwner<'dae> {
    targets: Vec<dae::DiscreteValueId<'dae>>,
    branches: Vec<StagedBranch<'dae>>,
    structure: Option<StagedStructure<'dae>>,
    provenance: dae::DaeProvenance,
    rank: usize,
    parents: HashMap<BranchActivation<'dae>, Option<BranchActivation<'dae>>>,
}

#[derive(Clone, Copy)]
struct StagedStructure<'dae> {
    domain: dae::DomainId<'dae>,
    scalar_view: rumoca_core::ComprehensionScalarView,
}

pub(super) struct DiscreteWhenAssignment<'dae> {
    pub(super) owner: DiscreteValueOwnerHandle,
    pub(super) trigger: dae::ConditionId<'dae>,
    pub(super) guard: dae::ConditionId<'dae>,
    pub(super) parent: Option<ParentActivation<'dae>>,
    pub(super) statement: Option<Span>,
    pub(super) target: dae::DiscreteValueId<'dae>,
    pub(super) value: dae::ExprId<'dae>,
    pub(super) branch_provenance: dae::DaeProvenance,
    pub(super) action_provenance: dae::DaeProvenance,
}

struct StagedAssignment<'dae> {
    owner: DiscreteValueOwnerHandle,
    activation: BranchActivation<'dae>,
    parent: Option<BranchActivation<'dae>>,
    statement: Option<Span>,
    target: dae::DiscreteValueId<'dae>,
    value: dae::ExprId<'dae>,
    branch_provenance: dae::DaeProvenance,
    action_provenance: dae::DaeProvenance,
}

pub(super) struct DiscreteValueStaging<'dae> {
    owners: Vec<StagedOwner<'dae>>,
    owner_by_target: HashMap<dae::DiscreteValueId<'dae>, usize>,
}

impl<'dae> DiscreteValueStaging<'dae> {
    pub(super) fn new() -> Self {
        Self {
            owners: Vec::new(),
            owner_by_target: HashMap::new(),
        }
    }

    pub(super) fn owner(
        &mut self,
        provenance: dae::DaeProvenance,
        target_names: impl IntoIterator<Item = VarName>,
        coordinates: &HashMap<VarName, Coordinate<'dae>>,
        plan: &DiscreteValueTopologyPlan,
    ) -> Result<Option<DiscreteValueOwnerHandle>, dae::DaeConstructionError> {
        self.build_owner(provenance, target_names, coordinates, plan, None)
    }

    pub(super) fn structured_owner(
        &mut self,
        provenance: dae::DaeProvenance,
        domain: dae::DomainId<'dae>,
        scalar_view: rumoca_core::ComprehensionScalarView,
        target_names: impl IntoIterator<Item = VarName>,
        coordinates: &HashMap<VarName, Coordinate<'dae>>,
        plan: &DiscreteValueTopologyPlan,
    ) -> Result<Option<DiscreteValueOwnerHandle>, dae::DaeConstructionError> {
        self.build_owner(
            provenance,
            target_names,
            coordinates,
            plan,
            Some(StagedStructure {
                domain,
                scalar_view,
            }),
        )
    }

    fn build_owner(
        &mut self,
        provenance: dae::DaeProvenance,
        target_names: impl IntoIterator<Item = VarName>,
        coordinates: &HashMap<VarName, Coordinate<'dae>>,
        plan: &DiscreteValueTopologyPlan,
        structure: Option<StagedStructure<'dae>>,
    ) -> Result<Option<DiscreteValueOwnerHandle>, dae::DaeConstructionError> {
        let mut targets = target_names
            .into_iter()
            .filter_map(|name| {
                let Coordinate::DiscreteValue(target) = coordinates[&name] else {
                    return None;
                };
                plan.target_order(&name).map(|order| (target, order))
            })
            .collect::<Vec<_>>();
        if targets.is_empty() {
            return Ok(None);
        }
        targets.sort_by_key(|(_, order)| (order.owner, order.target));
        targets.dedup_by_key(|(target, _)| target.index());
        let first = self.owners.len();
        let mut start = 0usize;
        while start < targets.len() {
            let rank = targets[start].1.owner;
            let end = targets[start..]
                .iter()
                .position(|(_, order)| order.owner != rank)
                .map_or(targets.len(), |offset| start + offset);
            let group = &targets[start..end];
            if !plan.matches_owner_targets(
                rank,
                group.len(),
                group.iter().map(|(_, order)| order.target),
            ) {
                return Err(dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
                    target: group[0].0.index(),
                    span: provenance.span(),
                });
            }
            let owner_index = self.owners.len();
            if let Err(target) =
                register_owner_targets(&mut self.owner_by_target, group, owner_index)
            {
                return Err(dae::DaeConstructionError::DuplicateDefinition {
                    kind: "B.1c semantic owner",
                    index: target.index(),
                    span: provenance.span(),
                });
            }
            self.owners.push(StagedOwner {
                targets: group.iter().map(|(target, _)| *target).collect(),
                branches: Vec::new(),
                structure,
                provenance,
                rank,
                parents: HashMap::new(),
            });
            start = end;
        }
        Ok(Some(DiscreteValueOwnerHandle {
            first,
            end: self.owners.len(),
        }))
    }

    pub(super) fn always(
        &mut self,
        owner: DiscreteValueOwnerHandle,
        target: dae::DiscreteValueId<'dae>,
        value: dae::ExprId<'dae>,
        branch_provenance: dae::DaeProvenance,
        action_provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        self.assign(StagedAssignment {
            owner,
            activation: BranchActivation::Always,
            parent: None,
            statement: None,
            target,
            value,
            branch_provenance,
            action_provenance,
        })
    }

    pub(super) fn when(
        &mut self,
        request: DiscreteWhenAssignment<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        let DiscreteWhenAssignment {
            owner,
            trigger,
            guard,
            parent,
            statement,
            target,
            value,
            branch_provenance,
            action_provenance,
        } = request;
        self.assign(StagedAssignment {
            owner,
            activation: BranchActivation::When { trigger, guard },
            parent: parent.map(|parent| match parent {
                ParentActivation::Section => BranchActivation::Always,
                ParentActivation::When { trigger, guard } => {
                    BranchActivation::When { trigger, guard }
                }
            }),
            statement,
            target,
            value,
            branch_provenance,
            action_provenance,
        })
    }

    fn assign(
        &mut self,
        assignment: StagedAssignment<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        let StagedAssignment {
            owner,
            activation,
            parent,
            statement,
            target,
            value,
            branch_provenance,
            action_provenance,
        } = assignment;
        let owner_index = self.owner_by_target.get(&target).copied().ok_or(
            dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
                target: target.index(),
                span: action_provenance.span(),
            },
        )?;
        if !(owner.first..owner.end).contains(&owner_index) {
            return Err(dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
                target: target.index(),
                span: action_provenance.span(),
            });
        }
        let owner = &mut self.owners[owner_index];
        let Some(target_ordinal) = owner
            .targets
            .iter()
            .position(|candidate| *candidate == target)
        else {
            return Err(dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
                target: target.index(),
                span: action_provenance.span(),
            });
        };
        register_branch_parent(
            &mut owner.parents,
            activation,
            parent,
            target.index(),
            action_provenance,
        )?;
        let branch_ordinal = match owner
            .branches
            .iter()
            .position(|branch| branch.activation == activation)
        {
            Some(ordinal) => ordinal,
            None => {
                let values = inherited_values(owner, activation)
                    .unwrap_or_else(|| vec![None; owner.targets.len()]);
                owner.branches.push(StagedBranch {
                    activation,
                    values,
                    written: vec![false; owner.targets.len()],
                    statement,
                    provenance: branch_provenance,
                });
                owner.branches.len() - 1
            }
        };
        let affected = owner
            .branches
            .iter()
            .enumerate()
            .filter_map(|(ordinal, branch)| {
                (ordinal == branch_ordinal || is_descendant(owner, branch.activation, activation))
                    .then_some(ordinal)
            })
            .collect::<Vec<_>>();
        for ordinal in affected {
            owner.branches[ordinal].values[target_ordinal] = Some((value, action_provenance));
        }
        owner.branches[branch_ordinal].written[target_ordinal] = true;
        Ok(())
    }

    pub(super) fn add_holds(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        coordinates: &HashMap<VarName, Coordinate<'dae>>,
        plan: &DiscreteValueTopologyPlan,
    ) -> Result<(), dae::DaeConstructionError> {
        for held in plan.held_targets() {
            let Some(Coordinate::DiscreteValue(target)) = coordinates.get(&held.name).copied()
            else {
                return Err(dae::DaeConstructionError::InvalidVariableRole {
                    name: held.name.clone(),
                    span: held.declaration_span,
                });
            };
            if self.owner_by_target.contains_key(&target) {
                return Err(dae::DaeConstructionError::DuplicateDefinition {
                    kind: "B.1c held semantic owner",
                    index: target.index(),
                    span: held.declaration_span,
                });
            }
            let provenance = dae::DaeProvenance::generated(
                dae::DaeGeneration::DiscreteUpdate,
                held.declaration_span,
            )?;
            let Some(owner) = self.owner(provenance, [held.name.clone()], coordinates, plan)?
            else {
                return Err(dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
                    target: target.index(),
                    span: held.declaration_span,
                });
            };
            let value = construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::PreDiscreteValue(target))
            })?;
            self.always(owner, target, value, provenance, provenance)?;
        }
        Ok(())
    }

    pub(super) fn finish(
        mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        plan: &DiscreteValueTopologyPlan,
    ) -> Result<(), dae::DaeConstructionError> {
        self.fill_retained_values(construction)?;
        self.owners.sort_by_key(|owner| owner.rank);
        debug_assert_eq!(self.owners.len(), plan.ordered_owners().len());
        let topology = self
            .owners
            .iter()
            .flat_map(|owner| owner.targets.iter().copied())
            .collect::<Vec<_>>();
        construction.b1c(topology, |b1c| {
            for owner in self.owners {
                let observed = plan.owner_is_observed(owner.rank);
                append_owner(b1c, owner, observed)?;
            }
            Ok(())
        })
    }

    fn fill_retained_values(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        for owner in &mut self.owners {
            fill_owner_retained_values(construction, owner)?;
        }
        Ok(())
    }
}

fn register_owner_targets<T, Order>(
    owner_by_target: &mut HashMap<T, usize>,
    targets: &[(T, Order)],
    owner: usize,
) -> Result<(), T>
where
    T: Copy + Eq + std::hash::Hash,
{
    if let Some((target, _)) = targets
        .iter()
        .find(|(target, _)| owner_by_target.contains_key(target))
    {
        return Err(*target);
    }
    for (target, _) in targets {
        owner_by_target.insert(*target, owner);
    }
    Ok(())
}

fn register_branch_parent<Activation>(
    parents: &mut HashMap<Activation, Option<Activation>>,
    activation: Activation,
    parent: Option<Activation>,
    target: u32,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError>
where
    Activation: Copy + Eq + std::hash::Hash,
{
    match parents.entry(activation) {
        Entry::Vacant(slot) => {
            slot.insert(parent);
            Ok(())
        }
        Entry::Occupied(slot) if *slot.get() == parent => Ok(()),
        Entry::Occupied(_) => Err(dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
            target,
            span: provenance.span(),
        }),
    }
}

fn append_owner<'dae>(
    topology: &mut dae::DiscreteValueTopology<'_, 'dae>,
    owner: StagedOwner<'dae>,
    observed: bool,
) -> Result<(), dae::DaeConstructionError> {
    let StagedOwner {
        targets,
        branches,
        structure,
        provenance,
        ..
    } = owner;
    let append = |definition: &mut dae::DiscreteValueOwner<'_, 'dae>| {
        for branch in branches {
            append_branch(definition, branch)?;
        }
        Ok(())
    };
    match structure {
        Some(_) if observed => {
            return Err(dae::DaeConstructionError::InvalidObservedDiscreteOwner {
                span: provenance.span(),
            });
        }
        Some(structure) => topology.structured_owner(
            provenance,
            structure.domain,
            structure.scalar_view,
            targets,
            append,
        )?,
        None if observed => topology.observed_owner(provenance, targets, append)?,
        None => topology.owner(provenance, targets, append)?,
    };
    Ok(())
}

fn append_branch<'dae>(
    definition: &mut dae::DiscreteValueOwner<'_, 'dae>,
    branch: StagedBranch<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let values = branch
        .values
        .into_iter()
        .map(|value| value.expect("retained B.1c values are filled before construction"))
        .collect::<Vec<_>>();
    match branch.activation {
        BranchActivation::Always => definition.always(branch.provenance, values),
        BranchActivation::When { trigger, guard } => {
            definition.when(trigger, guard, branch.provenance, values)
        }
    }
}

fn fill_owner_retained_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    owner: &mut StagedOwner<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    if owner.branches.is_empty() {
        return Err(dae::DaeConstructionError::EmptyDiscreteValueOwner {
            span: owner.provenance.span(),
        });
    }
    for branch in &mut owner.branches {
        fill_branch_retained_values(construction, &owner.targets, branch)?;
    }
    order_owner_branches(owner)?;
    activate_section_branch(construction, owner)
}

/// Order the branches of one owner by priority: the first active branch
/// defines every target.
///
/// A nested branch precedes the branch it refines. Among the branches of
/// separate top-level statements of one algorithm section, a later
/// statement precedes an earlier one, because its assignments run after and
/// override the earlier ones when both are active (MLS §11.1.2); the
/// branches of one `when`/`elsewhen` or `if`/`elseif` chain keep their
/// textual priority. That ordering is exact only when the later statement
/// writes every target the earlier one writes; otherwise their simultaneous
/// activation would drop the earlier writes, so it is rejected.
fn order_owner_branches(owner: &mut StagedOwner<'_>) -> Result<(), dae::DaeConstructionError> {
    let mut statements: Vec<Span> = Vec::new();
    for branch in &owner.branches {
        if let Some(statement) = branch.statement
            && !statements.contains(&statement)
        {
            statements.push(statement);
        }
    }
    let rank = |statement: Option<Span>| {
        statement.map_or(0, |statement| {
            statements
                .iter()
                .position(|candidate| *candidate == statement)
                .map_or(0, |position| position + 1)
        })
    };
    for (earlier, branch) in owner.branches.iter().enumerate() {
        for later in &owner.branches[earlier + 1..] {
            let separate = matches!(
                (branch.statement, later.statement),
                (Some(first), Some(second)) if first != second
            );
            let (before, after) = if rank(branch.statement) <= rank(later.statement) {
                (branch, later)
            } else {
                (later, branch)
            };
            let overridden = before
                .written
                .iter()
                .zip(&after.written)
                .all(|(earlier_writes, later_writes)| !earlier_writes || *later_writes);
            if separate && !overridden {
                return Err(dae::DaeConstructionError::UnorderedSimultaneousStatements {
                    span: after.provenance.span(),
                });
            }
        }
    }
    let parents = &owner.parents;
    owner.branches.sort_by_key(|branch| {
        (
            std::cmp::Reverse(branch_depth(parents, branch.activation)),
            std::cmp::Reverse(rank(branch.statement)),
        )
    });
    Ok(())
}

/// Give the section-level branch of an owner that also has guarded branches
/// the always-active condition of the section.
///
/// The unconditional statements of an algorithm section run whenever the
/// section runs. Beneath the guarded branches of its `when` and `if`
/// statements they are the lowest-priority branch, active whenever no
/// guarded branch is, which is exactly a branch guarded by the section's
/// `Always` condition (it has no edge memory, so its edge is its level).
fn activate_section_branch<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    owner: &mut StagedOwner<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let Some(section) = owner
        .branches
        .iter()
        .position(|branch| branch.activation == BranchActivation::Always)
    else {
        return Ok(());
    };
    if owner.branches.len() == 1 {
        return Ok(());
    }
    if section + 1 != owner.branches.len() {
        return Err(dae::DaeConstructionError::InvalidDiscreteBranchSet {
            span: owner.branches[section].provenance.span(),
        });
    }
    let always = always_condition(construction, owner.branches[section].provenance.span())?;
    owner.branches[section].activation = BranchActivation::When {
        trigger: always,
        guard: always,
    };
    Ok(())
}

fn fill_branch_retained_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    targets: &[dae::DiscreteValueId<'dae>],
    branch: &mut StagedBranch<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    for (target, value) in targets.iter().copied().zip(&mut branch.values) {
        if value.is_some() {
            continue;
        }
        let provenance = dae::DaeProvenance::generated(
            dae::DaeGeneration::DiscreteUpdate,
            branch.provenance.span(),
        )?;
        let retained = construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::PreDiscreteValue(target))
        })?;
        *value = Some((retained, provenance));
    }
    Ok(())
}

fn inherited_values<'dae>(
    owner: &StagedOwner<'dae>,
    activation: BranchActivation<'dae>,
) -> Option<Vec<Option<(dae::ExprId<'dae>, dae::DaeProvenance)>>> {
    let mut parent = owner.parents.get(&activation).copied().flatten();
    while let Some(activation) = parent {
        if let Some(branch) = owner
            .branches
            .iter()
            .find(|branch| branch.activation == activation)
        {
            return Some(branch.values.clone());
        }
        parent = owner.parents.get(&activation).copied().flatten();
    }
    None
}

fn is_descendant<'dae>(
    owner: &StagedOwner<'dae>,
    candidate: BranchActivation<'dae>,
    ancestor: BranchActivation<'dae>,
) -> bool {
    let mut parent = owner.parents.get(&candidate).copied().flatten();
    while let Some(activation) = parent {
        if activation == ancestor {
            return true;
        }
        parent = owner.parents.get(&activation).copied().flatten();
    }
    false
}

fn branch_depth<'dae>(
    parents: &HashMap<BranchActivation<'dae>, Option<BranchActivation<'dae>>>,
    activation: BranchActivation<'dae>,
) -> usize {
    let mut depth = 0;
    let mut parent = parents.get(&activation).copied().flatten();
    while let Some(activation) = parent {
        depth += 1;
        parent = parents.get(&activation).copied().flatten();
    }
    depth
}

#[cfg(test)]
#[test]
fn owner_target_registration_is_failure_atomic() {
    let mut owner_by_target = HashMap::from([(2_u32, 0_usize)]);
    let before = owner_by_target.clone();

    let result = register_owner_targets(&mut owner_by_target, &[(1, ()), (2, ())], 1);

    assert_eq!(result, Err(2));
    assert_eq!(owner_by_target, before);
}

#[cfg(test)]
#[test]
fn repeated_branch_activation_rejects_a_conflicting_parent_without_mutation() {
    let text = "first parent; second parent";
    let mut sources = SourceMap::new();
    let source = sources.add("conflicting_branch_parent.mo", text);
    let start = text.find("second parent").unwrap();
    let conflict_span = Span::from_offsets(source, start, start + "second parent".len());
    let provenance = dae::DaeProvenance::source(conflict_span).unwrap();
    let mut parents = HashMap::new();

    register_branch_parent(&mut parents, 7_u32, Some(1), 9, provenance).unwrap();
    register_branch_parent(&mut parents, 7_u32, Some(1), 9, provenance).unwrap();
    let before_conflict = parents.clone();
    let error = register_branch_parent(&mut parents, 7_u32, Some(2), 9, provenance).unwrap_err();

    assert_eq!(parents, before_conflict);
    let phase_error = ToDaeError::from(error);
    assert_eq!(phase_error.source_span(), Some(conflict_span));
    assert!(matches!(
        phase_error,
        ToDaeError::Construction {
            source: dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
                target: 9,
                span,
            },
            span: phase_span,
        } if span == conflict_span && phase_span == conflict_span
    ));
}
