use std::marker::PhantomData;

use rumoca_core::ComprehensionScalarView;
use rustc_hash::FxHashSet;

use crate::conditions::{ConditionNode, condition_owner_clock};
use crate::expression::{Coordinate, ExprNode, PackedSubscriptKind};
use crate::model::{Storage, check_provenance, checked_u32, unknown};
use crate::{
    ConditionId, DaeConstructionError, DaeProvenance, DiscreteValueId, ExprId, VariableRole,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum DiscreteBranchActivationEntry {
    Always,
    When { trigger: u32, guard: u32 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub(crate) struct PackedRange {
    pub(crate) start: u32,
    pub(crate) len: u32,
}

impl PackedRange {
    pub(crate) fn indices(self) -> std::ops::Range<usize> {
        let start = self.start as usize;
        start..start + self.len as usize
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct DiscreteValueOwnerEntry {
    pub(crate) targets: PackedRange,
    pub(crate) branches: PackedRange,
    pub(crate) structure: Option<StructuredDiscreteValueEntry>,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct StructuredDiscreteValueEntry {
    pub(crate) domain: u32,
    pub(crate) scalar_view: ComprehensionScalarView,
    #[serde(skip_serializing)]
    pub(crate) scalar_rows: u32,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct DiscreteValueBranchEntry {
    pub(crate) activation: DiscreteBranchActivationEntry,
    pub(crate) values: PackedRange,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy)]
pub enum DiscreteBranchActivation<'dae> {
    Always,
    When {
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct DiscreteValueOwnerView<'dae> {
    pub(crate) targets: DiscreteValueTargets<'dae>,
    pub(crate) branches: DiscreteValueBranches<'dae>,
    pub(crate) structure: Option<StructuredDiscreteValueView<'dae>>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> DiscreteValueOwnerView<'dae> {
    pub const fn targets(self) -> DiscreteValueTargets<'dae> {
        self.targets
    }

    pub const fn branches(self) -> DiscreteValueBranches<'dae> {
        self.branches
    }

    pub const fn structure(self) -> Option<StructuredDiscreteValueView<'dae>> {
        self.structure
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructuredDiscreteValueView<'dae> {
    pub(crate) domain: crate::DomainId<'dae>,
    pub(crate) scalar_view: ComprehensionScalarView,
    pub(crate) scalar_rows: u32,
}

impl<'dae> StructuredDiscreteValueView<'dae> {
    pub const fn domain(self) -> crate::DomainId<'dae> {
        self.domain
    }

    pub const fn scalar_view(self) -> ComprehensionScalarView {
        self.scalar_view
    }

    pub const fn scalar_rows(self) -> u32 {
        self.scalar_rows
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DiscreteValueTargets<'dae> {
    pub(crate) raw: &'dae [u32],
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DiscreteValueTargets<'dae> {
    pub const fn len(self) -> usize {
        self.raw.len()
    }

    pub const fn is_empty(self) -> bool {
        self.raw.is_empty()
    }

    pub fn get(self, index: usize) -> Option<DiscreteValueId<'dae>> {
        self.raw.get(index).copied().map(DiscreteValueId::from_raw)
    }

    pub fn iter(self) -> impl ExactSizeIterator<Item = DiscreteValueId<'dae>> {
        self.raw.iter().copied().map(DiscreteValueId::from_raw)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DiscreteValueBranches<'dae> {
    pub(crate) entries: &'dae [DiscreteValueBranchEntry],
    pub(crate) values: &'dae [u32],
    pub(crate) value_provenance: &'dae [DaeProvenance],
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DiscreteValueBranches<'dae> {
    pub const fn len(self) -> usize {
        self.entries.len()
    }

    pub const fn is_empty(self) -> bool {
        self.entries.is_empty()
    }

    pub fn get(self, index: usize) -> Option<DiscreteValueBranchView<'dae>> {
        let entry = self.entries.get(index)?;
        let value_indices = entry.values.indices();
        Some(DiscreteValueBranchView {
            activation: match entry.activation {
                DiscreteBranchActivationEntry::Always => DiscreteBranchActivation::Always,
                DiscreteBranchActivationEntry::When { trigger, guard } => {
                    DiscreteBranchActivation::When {
                        trigger: ConditionId::from_raw(trigger),
                        guard: ConditionId::from_raw(guard),
                    }
                }
            },
            values: DiscreteValueBranchValues {
                raw: self.values.get(value_indices.clone())?,
                provenance: self.value_provenance.get(value_indices)?,
                marker: PhantomData,
            },
            provenance: entry.provenance,
        })
    }

    pub fn iter(self) -> impl ExactSizeIterator<Item = DiscreteValueBranchView<'dae>> {
        (0..self.len()).map(move |index| {
            self.get(index)
                .expect("finalized B.1c branch ranges resolve")
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DiscreteValueBranchView<'dae> {
    activation: DiscreteBranchActivation<'dae>,
    values: DiscreteValueBranchValues<'dae>,
    provenance: DaeProvenance,
}

impl<'dae> DiscreteValueBranchView<'dae> {
    pub const fn activation(self) -> DiscreteBranchActivation<'dae> {
        self.activation
    }

    pub const fn values(self) -> DiscreteValueBranchValues<'dae> {
        self.values
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DiscreteValueBranchValues<'dae> {
    raw: &'dae [u32],
    provenance: &'dae [DaeProvenance],
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DiscreteValueBranchValues<'dae> {
    pub const fn len(self) -> usize {
        self.raw.len()
    }

    pub const fn is_empty(self) -> bool {
        self.raw.is_empty()
    }

    pub fn get(self, index: usize) -> Option<(ExprId<'dae>, DaeProvenance)> {
        Some((
            ExprId::from_raw(*self.raw.get(index)?),
            *self.provenance.get(index)?,
        ))
    }

    pub fn iter(self) -> impl ExactSizeIterator<Item = (ExprId<'dae>, DaeProvenance)> {
        self.raw
            .iter()
            .copied()
            .zip(self.provenance.iter().copied())
            .map(|(raw, provenance)| (ExprId::from_raw(raw), provenance))
    }
}

pub struct DiscreteValueOwner<'storage, 'dae> {
    source_map: &'storage rumoca_core::SourceMap,
    storage: &'storage mut Storage,
    target_count: usize,
    branch_start: usize,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DiscreteValueOwner<'_, 'dae> {
    pub fn always(
        &mut self,
        provenance: DaeProvenance,
        values: impl IntoIterator<Item = (ExprId<'dae>, DaeProvenance)>,
    ) -> Result<(), DaeConstructionError> {
        self.branch(DiscreteBranchActivation::Always, provenance, values)
    }

    pub fn when(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        provenance: DaeProvenance,
        values: impl IntoIterator<Item = (ExprId<'dae>, DaeProvenance)>,
    ) -> Result<(), DaeConstructionError> {
        self.branch(
            DiscreteBranchActivation::When { trigger, guard },
            provenance,
            values,
        )
    }

    fn branch(
        &mut self,
        activation: DiscreteBranchActivation<'dae>,
        provenance: DaeProvenance,
        values: impl IntoIterator<Item = (ExprId<'dae>, DaeProvenance)>,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let activation = match activation {
            DiscreteBranchActivation::Always => DiscreteBranchActivationEntry::Always,
            DiscreteBranchActivation::When { trigger, guard } => {
                expect_complete_condition(self.storage, trigger.index(), provenance)?;
                expect_complete_condition(self.storage, guard.index(), provenance)?;
                DiscreteBranchActivationEntry::When {
                    trigger: trigger.index(),
                    guard: guard.index(),
                }
            }
        };
        let values = values
            .into_iter()
            .map(|(value, action_provenance)| {
                check_provenance(self.source_map, action_provenance)?;
                self.storage
                    .expressions
                    .nodes
                    .get(value.index() as usize)
                    .ok_or_else(|| unknown("expression", value.index(), action_provenance))?;
                Ok((value.index(), action_provenance))
            })
            .collect::<Result<Vec<_>, DaeConstructionError>>()?;
        if values.len() != self.target_count {
            return Err(DaeConstructionError::InvalidArity {
                expected: self.target_count,
                found: values.len(),
                span: provenance.span(),
            });
        }
        let branches = &self.storage.discrete_value_branches[self.branch_start..];
        if matches!(activation, DiscreteBranchActivationEntry::Always) && !branches.is_empty()
            || branches
                .iter()
                .any(|branch| matches!(branch.activation, DiscreteBranchActivationEntry::Always))
        {
            return Err(DaeConstructionError::InvalidDiscreteBranchSet {
                span: provenance.span(),
            });
        }
        let value_start = checked_u32(
            self.storage.discrete_value_branch_values.len(),
            "B.1c value buffer",
            provenance,
        )?;
        let value_len = checked_u32(values.len(), "B.1c value buffer", provenance)?;
        expect_packed_capacity(value_start, value_len, "B.1c value buffer", provenance)?;
        checked_u32(
            self.storage.discrete_value_branches.len(),
            "B.1c branch arena",
            provenance,
        )?;
        for (value, value_provenance) in values {
            self.storage.discrete_value_branch_values.push(value);
            self.storage
                .discrete_value_branch_value_provenance
                .push(value_provenance);
        }
        self.storage
            .discrete_value_branches
            .push(DiscreteValueBranchEntry {
                activation,
                values: PackedRange {
                    start: value_start,
                    len: value_len,
                },
                provenance,
            });
        Ok(())
    }
}

pub struct DiscreteValueTopology<'storage, 'dae> {
    source_map: &'storage rumoca_core::SourceMap,
    storage: &'storage mut Storage,
    plan: Vec<u32>,
    issued: FxHashSet<u32>,
    cursor: usize,
    marker: PhantomData<&'dae mut &'dae ()>,
}

#[derive(Debug, Clone, Copy)]
struct DiscreteValueTopologyCheckpoint {
    owners: usize,
    targets: usize,
    branches: usize,
    branch_values: usize,
    branch_value_provenance: usize,
    complete: bool,
}

impl DiscreteValueTopologyCheckpoint {
    fn capture(storage: &Storage) -> Self {
        Self {
            owners: storage.discrete_value_owners.len(),
            targets: storage.discrete_value_targets.len(),
            branches: storage.discrete_value_branches.len(),
            branch_values: storage.discrete_value_branch_values.len(),
            branch_value_provenance: storage.discrete_value_branch_value_provenance.len(),
            complete: storage.discrete_value_topology_complete,
        }
    }

    fn rollback(self, storage: &mut Storage) {
        storage.discrete_value_owners.truncate(self.owners);
        storage.discrete_value_targets.truncate(self.targets);
        storage.discrete_value_branches.truncate(self.branches);
        storage
            .discrete_value_branch_values
            .truncate(self.branch_values);
        storage
            .discrete_value_branch_value_provenance
            .truncate(self.branch_value_provenance);
        storage.discrete_value_topology_complete = self.complete;
    }
}

impl<'dae> DiscreteValueTopology<'_, 'dae> {
    pub fn owner(
        &mut self,
        provenance: DaeProvenance,
        targets: impl IntoIterator<Item = DiscreteValueId<'dae>>,
        build: impl FnOnce(&mut DiscreteValueOwner<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<crate::DiscreteValueOwnerId<'dae>, DaeConstructionError> {
        self.build_owner(provenance, targets, None, build)
    }

    pub fn structured_owner(
        &mut self,
        provenance: DaeProvenance,
        domain: crate::DomainId<'dae>,
        scalar_view: ComprehensionScalarView,
        targets: impl IntoIterator<Item = DiscreteValueId<'dae>>,
        build: impl FnOnce(&mut DiscreteValueOwner<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<crate::DiscreteValueOwnerId<'dae>, DaeConstructionError> {
        let scalar_count = self.storage.domain_scalar_count(domain, provenance)?;
        self.build_owner(
            provenance,
            targets,
            Some((domain, scalar_view, scalar_count)),
            build,
        )
    }

    fn build_owner(
        &mut self,
        provenance: DaeProvenance,
        targets: impl IntoIterator<Item = DiscreteValueId<'dae>>,
        structure: Option<(crate::DomainId<'dae>, ComprehensionScalarView, usize)>,
        build: impl FnOnce(&mut DiscreteValueOwner<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<crate::DiscreteValueOwnerId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let targets = targets
            .into_iter()
            .map(DiscreteValueId::index)
            .collect::<Vec<_>>();
        if targets.is_empty() {
            return Err(DaeConstructionError::EmptyDiscreteValueOwner {
                span: provenance.span(),
            });
        }
        let end = self.cursor.saturating_add(targets.len());
        let expected = self.plan.get(self.cursor..end).unwrap_or(&[]);
        if expected != targets {
            return Err(DaeConstructionError::InvalidDiscreteTargetOrder {
                expected: expected.first().copied(),
                found: targets.first().copied(),
                span: provenance.span(),
            });
        }
        for &target in &targets {
            self.storage
                .expect_discrete_value_target_id(target, provenance)?;
        }
        if let Some((domain, _, _)) = structure {
            self.validate_structured_targets(domain, &targets, provenance)?;
        }

        let owner = checked_u32(
            self.storage.discrete_value_owners.len(),
            "B.1c owner arena",
            provenance,
        )?;
        let target_start = checked_u32(
            self.storage.discrete_value_targets.len(),
            "B.1c target buffer",
            provenance,
        )?;
        let target_len = checked_u32(targets.len(), "B.1c target buffer", provenance)?;
        expect_packed_capacity(target_start, target_len, "B.1c target buffer", provenance)?;
        let branch_start = self.storage.discrete_value_branches.len();
        let checkpoint = DiscreteValueTopologyCheckpoint::capture(self.storage);
        self.storage
            .discrete_value_targets
            .extend_from_slice(&targets);
        let result = build(&mut DiscreteValueOwner {
            source_map: self.source_map,
            storage: self.storage,
            target_count: targets.len(),
            branch_start,
            marker: PhantomData,
        })
        .and_then(|()| {
            self.commit_owner(
                provenance,
                &targets,
                structure,
                target_start,
                target_len,
                branch_start,
            )
        });
        if let Err(error) = result {
            checkpoint.rollback(self.storage);
            return Err(error);
        }
        for &target in &targets {
            self.issued.insert(target);
        }
        self.cursor = end;
        Ok(crate::DiscreteValueOwnerId::from_raw(owner))
    }

    fn commit_owner(
        &mut self,
        provenance: DaeProvenance,
        targets: &[u32],
        structure: Option<(crate::DomainId<'dae>, ComprehensionScalarView, usize)>,
        target_start: u32,
        target_len: u32,
        branch_start: usize,
    ) -> Result<(), DaeConstructionError> {
        let branch_len = self.storage.discrete_value_branches.len() - branch_start;
        if branch_len == 0 {
            return Err(DaeConstructionError::EmptyDiscreteValueOwner {
                span: provenance.span(),
            });
        }
        self.validate_owner(targets, branch_start, structure)?;
        let branch_start = checked_u32(branch_start, "B.1c branch arena", provenance)?;
        let branch_len = checked_u32(branch_len, "B.1c branch arena", provenance)?;
        expect_packed_capacity(branch_start, branch_len, "B.1c branch arena", provenance)?;
        let structure = structure
            .map(|(domain, scalar_view, scalar_count)| {
                let scalar_rows = scalar_count.checked_mul(targets.len()).ok_or(
                    DaeConstructionError::CapacityExceeded {
                        arena: "structured B.1c scalar rows",
                        attempted_index: usize::MAX,
                        span: provenance.span(),
                    },
                )?;
                Ok(StructuredDiscreteValueEntry {
                    domain: domain.index(),
                    scalar_view,
                    scalar_rows: checked_u32(
                        scalar_rows,
                        "structured B.1c scalar rows",
                        provenance,
                    )?,
                })
            })
            .transpose()?;
        self.storage
            .discrete_value_owners
            .push(DiscreteValueOwnerEntry {
                targets: PackedRange {
                    start: target_start,
                    len: target_len,
                },
                branches: PackedRange {
                    start: branch_start,
                    len: branch_len,
                },
                structure,
                provenance,
            });
        Ok(())
    }

    fn validate_owner(
        &self,
        targets: &[u32],
        branch_start: usize,
        structure: Option<(crate::DomainId<'dae>, ComprehensionScalarView, usize)>,
    ) -> Result<(), DaeConstructionError> {
        let branches = &self.storage.discrete_value_branches[branch_start..];
        for branch in branches {
            self.validate_branch_activation(targets, branch)?;
        }

        let mut issued_prefix = FxHashSet::default();
        issued_prefix.reserve(targets.len());
        for (ordinal, &target) in targets.iter().enumerate() {
            for branch in branches {
                let value_index = branch.values.start as usize + ordinal;
                let value = self.storage.discrete_value_branch_values[value_index];
                let provenance = self.storage.discrete_value_branch_value_provenance[value_index];
                match structure {
                    Some((domain, scalar_view, _)) => self.expect_structured_value(
                        target,
                        value,
                        domain,
                        scalar_view,
                        provenance,
                    )?,
                    None => self
                        .storage
                        .expect_discrete_value_raw(target, value, provenance)?,
                }
                self.expect_expression_dependencies_issued(
                    value,
                    target,
                    &issued_prefix,
                    provenance,
                )?;
            }
            issued_prefix.insert(target);
        }
        Ok(())
    }

    fn validate_structured_targets(
        &self,
        domain: crate::DomainId<'dae>,
        targets: &[u32],
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let extents = self.storage.domain_extents(domain, provenance)?;
        for &target in targets {
            let variable = self
                .storage
                .variables
                .get(target as usize)
                .ok_or_else(|| unknown("variable", target, provenance))?;
            let value_type = self
                .storage
                .value_type_at(variable.value_type, provenance)?;
            if value_type.dimensions() != extents {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: provenance.span(),
                });
            }
        }
        Ok(())
    }

    fn expect_structured_value(
        &self,
        target: u32,
        value: u32,
        domain: crate::DomainId<'dae>,
        scalar_view: ComprehensionScalarView,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let value = ExprId::from_raw(value);
        self.storage
            .expect_domain_expression(value, domain, provenance)?;
        let variable = self
            .storage
            .variables
            .get(target as usize)
            .ok_or_else(|| unknown("variable", target, provenance))?;
        let expected = self
            .storage
            .value_type_at(variable.value_type, provenance)?;
        let found = self.storage.expr_type(value, provenance)?;
        if expected.scalar_type() != found.scalar_type() {
            return Err(DaeConstructionError::TypeMismatch {
                expected: expected.scalar_type(),
                found: found.scalar_type(),
                span: provenance.span(),
            });
        }
        let extents = self.storage.domain_extents(domain, provenance)?;
        let domain_scalar_count = self.storage.domain_scalar_count(domain, provenance)?;
        let shape_matches = match scalar_view {
            ComprehensionScalarView::BinderSubstitution => found.is_scalar(),
            // Row-major ownership is by scalar ordinal. Preserve the value's
            // tensor rank (including singleton axes) while proving that every
            // domain point owns exactly one scalar and no scalar is omitted.
            ComprehensionScalarView::RowMajorProjection => {
                found.scalar_count() == Some(domain_scalar_count)
            }
            ComprehensionScalarView::BinderPrefixProjection { binder_count } => {
                extents.get(usize::try_from(binder_count).unwrap_or(usize::MAX)..)
                    == Some(found.dimensions())
            }
        };
        if !shape_matches {
            return Err(DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            });
        }
        Ok(())
    }

    fn validate_branch_activation(
        &self,
        targets: &[u32],
        branch: &DiscreteValueBranchEntry,
    ) -> Result<(), DaeConstructionError> {
        let DiscreteBranchActivationEntry::When { trigger, guard } = branch.activation else {
            return Ok(());
        };
        for &target in targets {
            expect_clock_ownership(self.storage, trigger, target, branch.provenance)?;
            expect_clock_ownership(self.storage, guard, target, branch.provenance)?;
        }
        let no_owner_targets = FxHashSet::default();
        self.expect_condition_dependencies_issued(
            trigger,
            targets[0],
            &no_owner_targets,
            branch.provenance,
        )?;
        self.expect_condition_dependencies_issued(
            guard,
            targets[0],
            &no_owner_targets,
            branch.provenance,
        )
    }

    fn expect_condition_dependencies_issued(
        &self,
        condition: u32,
        target: u32,
        issued_prefix: &FxHashSet<u32>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let mut conditions = vec![condition];
        let mut seen_conditions = FxHashSet::default();
        let mut expressions = Vec::new();
        while let Some(index) = conditions.pop() {
            if !seen_conditions.insert(index) {
                continue;
            }
            let node = self
                .storage
                .conditions
                .get(index as usize)
                .and_then(|entry| entry.node)
                .ok_or_else(|| unknown("condition", index, provenance))?;
            match node {
                ConditionNode::Initial | ConditionNode::Always | ConditionNode::Clock(_) => {}
                ConditionNode::Relation(relation) => {
                    let expression = self
                        .storage
                        .relations
                        .get(relation as usize)
                        .ok_or_else(|| unknown("relation", relation, provenance))?
                        .expression;
                    expressions.push(expression);
                }
                ConditionNode::Discrete(expression) => expressions.push(expression),
                ConditionNode::Not(inner) => conditions.push(inner),
                ConditionNode::And { lhs, rhs }
                | ConditionNode::Or { lhs, rhs }
                | ConditionNode::AnyRise { lhs, rhs } => {
                    conditions.extend([lhs, rhs]);
                }
            }
        }
        for expression in expressions {
            self.expect_expression_dependencies_issued(
                expression,
                target,
                issued_prefix,
                provenance,
            )?;
        }
        Ok(())
    }

    fn expect_expression_dependencies_issued(
        &self,
        expression: u32,
        target: u32,
        issued_prefix: &FxHashSet<u32>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let ordered_scalar_self_dependencies =
            self.proves_ordered_scalar_self_dependencies(expression, target, provenance)?;
        let mut pending = vec![expression];
        let mut visited = FxHashSet::default();
        while let Some(index) = pending.pop() {
            if !visited.insert(index) {
                continue;
            }
            let node = self
                .storage
                .expressions
                .nodes
                .get(index as usize)
                .ok_or_else(|| unknown("expression", index, provenance))?;
            match node {
                ExprNode::Coordinate(Coordinate::DiscreteValue(dependency))
                    if *dependency != target || !ordered_scalar_self_dependencies =>
                {
                    self.expect_dependency_issued(*dependency, target, issued_prefix, provenance)?;
                }
                ExprNode::Coordinate(Coordinate::Condition(condition)) => {
                    self.expect_condition_dependencies_issued(
                        *condition,
                        target,
                        issued_prefix,
                        provenance,
                    )?;
                }
                _ => {}
            }
            node.for_each_child(&self.storage.expressions, |child| pending.push(child));
        }
        Ok(())
    }

    /// Independently prove that an aggregate value's current-target reads are
    /// strictly earlier than the scalar segment being defined.
    ///
    /// Phase lowering may use exact element coverage to construct an aggregate
    /// owner, but this typed constructor does not trust that source-side fact.
    /// It replays the row-major proof from typed expression nodes, so wire
    /// decoding and direct IR construction enforce the same invariant.
    fn proves_ordered_scalar_self_dependencies(
        &self,
        expression: u32,
        target: u32,
        provenance: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        if !matches!(
            self.storage.expressions.nodes.get(expression as usize),
            Some(ExprNode::Array { .. })
        ) {
            return Ok(false);
        }
        let variable = self
            .storage
            .variables
            .get(target as usize)
            .ok_or_else(|| unknown("variable", target, provenance))?;
        let dimensions = self
            .storage
            .value_type_at(variable.value_type, provenance)?
            .dimensions()
            .to_vec();
        let mut output_start = 0usize;
        self.proves_ordered_array_segments(
            expression,
            target,
            &dimensions,
            &mut output_start,
            provenance,
        )
    }

    fn proves_ordered_array_segments(
        &self,
        expression: u32,
        target: u32,
        target_dimensions: &[u32],
        output_start: &mut usize,
        provenance: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        let node = self
            .storage
            .expressions
            .nodes
            .get(expression as usize)
            .ok_or_else(|| unknown("expression", expression, provenance))?;
        if let ExprNode::Array { operands } = node {
            return self.storage.expressions.operands[operands.indices()]
                .iter()
                .try_fold(true, |valid, &operand| match valid {
                    false => Ok(false),
                    true => self.proves_ordered_array_segments(
                        operand,
                        target,
                        target_dimensions,
                        output_start,
                        provenance,
                    ),
                });
        }
        if !self.proves_segment_dependencies_precede(
            expression,
            target,
            target_dimensions,
            *output_start,
            provenance,
        )? {
            return Ok(false);
        }
        let Some(scalar_count) = self.expression_scalar_count(expression, provenance)? else {
            return Ok(false);
        };
        let Some(next) = output_start.checked_add(scalar_count) else {
            return Ok(false);
        };
        *output_start = next;
        Ok(true)
    }

    fn proves_segment_dependencies_precede(
        &self,
        expression: u32,
        target: u32,
        target_dimensions: &[u32],
        output_start: usize,
        provenance: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        let node = self
            .storage
            .expressions
            .nodes
            .get(expression as usize)
            .ok_or_else(|| unknown("expression", expression, provenance))?;
        if let ExprNode::Index { base, subscripts } = node
            && matches!(
                self.storage.expressions.nodes.get(*base as usize),
                Some(ExprNode::Coordinate(Coordinate::DiscreteValue(dependency)))
                    if *dependency == target
            )
        {
            if !self.selected_target_range_precedes(*subscripts, target_dimensions, output_start) {
                return Ok(false);
            }
            return self.proves_subscript_dependencies_precede(
                *subscripts,
                target,
                target_dimensions,
                output_start,
                provenance,
            );
        }
        if matches!(
            node,
            ExprNode::Coordinate(Coordinate::DiscreteValue(dependency)) if *dependency == target
        ) {
            return Ok(false);
        }
        let mut valid = Ok(true);
        node.for_each_child(&self.storage.expressions, |child| {
            if matches!(valid, Ok(true)) {
                valid = self.proves_segment_dependencies_precede(
                    child,
                    target,
                    target_dimensions,
                    output_start,
                    provenance,
                );
            }
        });
        valid
    }

    fn proves_subscript_dependencies_precede(
        &self,
        subscripts: crate::expression::OperandRange,
        target: u32,
        target_dimensions: &[u32],
        output_start: usize,
        provenance: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        self.storage.expressions.subscripts[subscripts.indices()]
            .iter()
            .filter_map(|subscript| match subscript.kind {
                PackedSubscriptKind::Index(index) | PackedSubscriptKind::Slice(index) => {
                    Some(index)
                }
                PackedSubscriptKind::Whole => None,
            })
            .try_fold(true, |valid, index| {
                if !valid {
                    return Ok(false);
                }
                self.proves_segment_dependencies_precede(
                    index,
                    target,
                    target_dimensions,
                    output_start,
                    provenance,
                )
            })
    }

    fn selected_target_range_precedes(
        &self,
        subscripts: crate::expression::OperandRange,
        dimensions: &[u32],
        output_start: usize,
    ) -> bool {
        let packed = &self.storage.expressions.subscripts[subscripts.indices()];
        if packed.is_empty() || packed.len() > dimensions.len() {
            return false;
        }
        let mut ordinal = 0usize;
        for (subscript, &extent) in packed.iter().zip(dimensions) {
            let PackedSubscriptKind::Index(index) = subscript.kind else {
                return false;
            };
            let Some(ExprNode::Literal(crate::DaeLiteral::Integer(value))) =
                self.storage.expressions.nodes.get(index as usize)
            else {
                return false;
            };
            let Ok(value) = usize::try_from(*value) else {
                return false;
            };
            let extent = extent as usize;
            if value == 0 || value > extent {
                return false;
            }
            let Some(next) = ordinal
                .checked_mul(extent)
                .and_then(|base| base.checked_add(value - 1))
            else {
                return false;
            };
            ordinal = next;
        }
        let Some(selected_count) = dimensions[packed.len()..]
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
        else {
            return false;
        };
        ordinal
            .checked_mul(selected_count)
            .and_then(|start| start.checked_add(selected_count))
            .is_some_and(|end| end <= output_start)
    }

    fn expression_scalar_count(
        &self,
        expression: u32,
        provenance: DaeProvenance,
    ) -> Result<Option<usize>, DaeConstructionError> {
        let value_type = self
            .storage
            .expressions
            .value_types
            .get(expression as usize)
            .copied()
            .ok_or_else(|| unknown("expression", expression, provenance))?;
        Ok(self
            .storage
            .value_type_at(value_type, provenance)?
            .scalar_count())
    }

    fn expect_dependency_issued(
        &self,
        dependency: u32,
        target: u32,
        issued_prefix: &FxHashSet<u32>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let variable = self
            .storage
            .variables
            .get(dependency as usize)
            .ok_or_else(|| unknown("variable", dependency, provenance))?;
        if variable.is_discrete_value_input() {
            return Ok(());
        }
        if self.issued.contains(&dependency) || issued_prefix.contains(&dependency) {
            return Ok(());
        }
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target,
            dependency,
            span: provenance.span(),
        })
    }

    fn complete(self) -> Result<(), DaeConstructionError> {
        if self.cursor != self.plan.len() {
            let target = self.plan[self.cursor];
            let declaration = self.storage.variables[target as usize].declaration();
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "B.1c target",
                index: target,
                span: declaration.span(),
            });
        }
        self.storage.discrete_value_topology_complete = true;
        Ok(())
    }
}

fn expect_packed_capacity(
    start: u32,
    len: u32,
    arena: &'static str,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let end = u64::from(start) + u64::from(len);
    if end <= u64::from(u32::MAX) + 1 {
        return Ok(());
    }
    Err(DaeConstructionError::CapacityExceeded {
        arena,
        attempted_index: usize::try_from(end).unwrap_or(usize::MAX),
        span: provenance.span(),
    })
}

pub(crate) fn build_discrete_value_topology<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    plan: impl IntoIterator<Item = DiscreteValueId<'dae>>,
    build: impl FnOnce(&mut DiscreteValueTopology<'_, 'dae>) -> Result<(), DaeConstructionError>,
) -> Result<(), DaeConstructionError> {
    if storage.discrete_value_topology_complete {
        let Some((target, provenance)) = storage.first_required_discrete_value else {
            return Err(DaeConstructionError::DuplicateTopology {
                kind: "B.1c topology",
                span: None,
            });
        };
        return Err(DaeConstructionError::DuplicateDefinition {
            kind: "B.1c topology",
            index: target,
            span: provenance.span(),
        });
    }
    let plan = checked_plan(storage, plan)?;
    let checkpoint = DiscreteValueTopologyCheckpoint::capture(storage);
    let result = {
        let mut topology = DiscreteValueTopology {
            source_map,
            issued: FxHashSet::default(),
            storage,
            plan,
            cursor: 0,
            marker: PhantomData,
        };
        match build(&mut topology) {
            Ok(()) => topology.complete(),
            Err(error) => Err(error),
        }
    };
    if result.is_err() {
        checkpoint.rollback(storage);
    }
    result
}

fn checked_plan<'dae>(
    storage: &Storage,
    plan: impl IntoIterator<Item = DiscreteValueId<'dae>>,
) -> Result<Vec<u32>, DaeConstructionError> {
    let plan = plan
        .into_iter()
        .map(DiscreteValueId::index)
        .collect::<Vec<_>>();
    let mut present = FxHashSet::default();
    for &target in &plan {
        let variable = storage.variables.get(target as usize).ok_or_else(|| {
            let at = storage
                .first_required_discrete_value
                .map(|(_, provenance)| provenance)
                .expect("an invalid branded variable needs a DAE source owner");
            unknown("variable", target, at)
        })?;
        let provenance = variable.declaration();
        if variable.attributes_missing() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "variable",
                index: target,
                span: provenance.span(),
            });
        }
        if !variable.requires_discrete_value_owner() || !present.insert(target) {
            return Err(DaeConstructionError::InvalidDiscreteTopologyPlan {
                target,
                span: provenance.span(),
            });
        }
    }
    for (target, variable) in storage.variables.iter().enumerate() {
        if variable.role == VariableRole::DiscreteValue && variable.attributes_missing() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "variable",
                index: target as u32,
                span: variable.declaration().span(),
            });
        }
        if variable.requires_discrete_value_owner() && !present.contains(&(target as u32)) {
            return Err(DaeConstructionError::InvalidDiscreteTopologyPlan {
                target: target as u32,
                span: variable.declaration().span(),
            });
        }
    }
    Ok(plan)
}

fn expect_complete_condition(
    storage: &Storage,
    condition: u32,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let entry = storage
        .conditions
        .get(condition as usize)
        .ok_or_else(|| unknown("condition", condition, provenance))?;
    if entry.node.is_none() {
        return Err(DaeConstructionError::IncompleteDefinition {
            kind: "B.1c branch condition",
            index: condition,
            span: provenance.span(),
        });
    }
    Ok(())
}

fn expect_clock_ownership(
    storage: &Storage,
    guard: u32,
    variable: u32,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let Some(clock) = condition_owner_clock(storage, guard, provenance)? else {
        return Ok(());
    };
    if storage
        .clock_ownership_by_variable
        .get(&variable)
        .and_then(|&index| storage.clock_ownerships.get(index as usize))
        .is_some_and(|ownership| ownership.clock == clock)
    {
        return Ok(());
    }
    Err(DaeConstructionError::MissingClockOwnership {
        variable,
        clock,
        span: provenance.span(),
    })
}
