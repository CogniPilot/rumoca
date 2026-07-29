use super::*;
use std::collections::hash_map::Entry;

#[derive(Clone, Copy)]
pub(super) struct DiscreteValueOwnerHandle(usize);

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
    provenance: dae::DaeProvenance,
}

struct StagedOwner<'dae> {
    targets: Vec<dae::DiscreteValueId<'dae>>,
    branches: Vec<StagedBranch<'dae>>,
    provenance: dae::DaeProvenance,
    rank: usize,
    parents: HashMap<BranchActivation<'dae>, Option<BranchActivation<'dae>>>,
}

pub(super) struct DiscreteWhenAssignment<'dae> {
    pub(super) owner: DiscreteValueOwnerHandle,
    pub(super) trigger: dae::ConditionId<'dae>,
    pub(super) guard: dae::ConditionId<'dae>,
    pub(super) parent: Option<(dae::ConditionId<'dae>, dae::ConditionId<'dae>)>,
    pub(super) target: dae::DiscreteValueId<'dae>,
    pub(super) value: dae::ExprId<'dae>,
    pub(super) branch_provenance: dae::DaeProvenance,
    pub(super) action_provenance: dae::DaeProvenance,
}

struct StagedAssignment<'dae> {
    owner: DiscreteValueOwnerHandle,
    activation: BranchActivation<'dae>,
    parent: Option<BranchActivation<'dae>>,
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
        let rank = targets[0].1.owner;
        if targets.iter().any(|(_, order)| order.owner != rank) {
            return Err(dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
                target: targets[0].0.index(),
                span: provenance.span(),
            });
        }
        if !plan.matches_owner_targets(
            rank,
            targets.len(),
            targets.iter().map(|(_, order)| order.target),
        ) {
            return Err(dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
                target: targets[0].0.index(),
                span: provenance.span(),
            });
        }
        let owner_index = self.owners.len();
        if let Err(target) =
            register_owner_targets(&mut self.owner_by_target, &targets, owner_index)
        {
            return Err(dae::DaeConstructionError::DuplicateDefinition {
                kind: "B.1c semantic owner",
                index: target.index(),
                span: provenance.span(),
            });
        }
        self.owners.push(StagedOwner {
            targets: targets.into_iter().map(|(target, _)| target).collect(),
            branches: Vec::new(),
            provenance,
            rank,
            parents: HashMap::new(),
        });
        Ok(Some(DiscreteValueOwnerHandle(owner_index)))
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
            target,
            value,
            branch_provenance,
            action_provenance,
        } = request;
        self.assign(StagedAssignment {
            owner,
            activation: BranchActivation::When { trigger, guard },
            parent: parent.map(|(trigger, guard)| BranchActivation::When { trigger, guard }),
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
            target,
            value,
            branch_provenance,
            action_provenance,
        } = assignment;
        let owner = &mut self.owners[owner.0];
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
        Ok(())
    }

    pub(super) fn add_holds(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        flat: &flat::Model,
        coordinates: &HashMap<VarName, Coordinate<'dae>>,
        plan: &DiscreteValueTopologyPlan,
    ) -> Result<(), dae::DaeConstructionError> {
        for (name, variable) in &flat.variables {
            let Coordinate::DiscreteValue(target) = coordinates[name] else {
                continue;
            };
            if self.owner_by_target.contains_key(&target) {
                continue;
            }
            let provenance = dae::DaeProvenance::generated(
                dae::DaeGeneration::DiscreteUpdate,
                variable.source_span,
            )?;
            let owner = self
                .owner(provenance, [name.clone()], coordinates, plan)?
                .expect("a planned discrete-value coordinate creates an owner");
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
                append_owner(b1c, owner)?;
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
) -> Result<(), dae::DaeConstructionError> {
    topology.owner(owner.provenance, owner.targets, |definition| {
        for branch in owner.branches {
            append_branch(definition, branch)?;
        }
        Ok(())
    })?;
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
    let parents = &owner.parents;
    owner
        .branches
        .sort_by_key(|branch| std::cmp::Reverse(branch_depth(parents, branch.activation)));
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
