use std::{collections::BTreeSet, marker::PhantomData};

use rumoca_core::{OperationContractKey, Span};
use rumoca_ir_dae::{ExprId, ExpressionOperation};

use super::contract::{callable_value_type, checked_id};
use super::plan::{
    CallableOperationDetail, OperationEntry, OperationSource, StructuredRegionDetail,
    StructuredRegionEntry, ValueEntry, ValueProducer,
};
use super::{
    CallableConditionalSource, CallableExpressionSource, CallablePlanConstruction,
    ConditionalGroupConstruction, ConstructionScopeId, ConstructionValueId, PendingScope,
    PlanConstructionError, RegionValueClaim,
};

/// The complete result claim for closing one atomic conditional group.
#[derive(Clone, Copy)]
struct ConditionalGroupCloseClaim<'a, 'plan> {
    conditions: &'a [ConstructionValueId<'plan>],
    branch_results: &'a [Vec<ConstructionValueId<'plan>>],
    fallback_results: &'a [ConstructionValueId<'plan>],
}

impl<'plan, 'dae> CallablePlanConstruction<'plan, 'dae> {
    pub fn open_conditional_group(
        &mut self,
        parent: ConstructionScopeId<'plan>,
        source: CallableConditionalSource<'plan, 'dae>,
    ) -> Result<ConditionalGroupConstruction<'plan, 'dae>, PlanConstructionError> {
        self.transaction(|construction| construction.try_open_conditional_group(parent, source))
    }

    fn try_open_conditional_group(
        &mut self,
        parent: ConstructionScopeId<'plan>,
        source: CallableConditionalSource<'plan, 'dae>,
    ) -> Result<ConditionalGroupConstruction<'plan, 'dae>, PlanConstructionError> {
        let span = source.provenance.span();
        let owner = self.scope_owner(parent, span)?;
        if source.function != self.scope_owner_function(parent, span)?
            || !self.outstanding_conditionals.contains(&source.conditional)
            || !self.open_conditional_groups.insert(source.conditional)
        {
            return Err(PlanConstructionError::InvalidSourceOccurrence { span });
        }
        let data = self.conditional_source(source)?;
        self.validate_conditional_group_source(&data)?;
        let child_count = data
            .branches
            .len()
            .checked_add(1)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        let first = self.scopes.len();
        let _last = checked_id(
            first
                .checked_add(child_count - 1)
                .ok_or(PlanConstructionError::IdentityOverflow { span })?,
            span,
        )?;
        let branches = (0..data.branches.len())
            .map(|ordinal| ConstructionScopeId {
                raw: (first + ordinal) as u32,
                marker: PhantomData,
            })
            .collect::<Box<[_]>>();
        let fallback = ConstructionScopeId {
            raw: (first + data.branches.len()) as u32,
            marker: PhantomData,
        };
        let parent_open_children = self.scopes[parent.raw as usize]
            .open_children
            .checked_add(child_count)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        self.scopes.extend((0..child_count).map(|_| PendingScope {
            owner,
            parent: Some(parent.raw),
            operations: Vec::new(),
            open_children: 0,
            closed: false,
        }));
        self.scopes[parent.raw as usize].open_children = parent_open_children;
        Ok(ConditionalGroupConstruction {
            source,
            parent,
            branches,
            fallback,
        })
    }

    pub fn close_conditional_group(
        &mut self,
        region: ConditionalGroupConstruction<'plan, 'dae>,
        conditions: &[ConstructionValueId<'plan>],
        branch_results: &[Vec<ConstructionValueId<'plan>>],
        fallback_results: &[ConstructionValueId<'plan>],
    ) -> Result<Box<[ConstructionValueId<'plan>]>, PlanConstructionError> {
        self.transaction(|construction| {
            construction.try_close_conditional_group(
                region,
                conditions,
                branch_results,
                fallback_results,
            )
        })
    }

    fn try_close_conditional_group(
        &mut self,
        region: ConditionalGroupConstruction<'plan, 'dae>,
        conditions: &[ConstructionValueId<'plan>],
        branch_results: &[Vec<ConstructionValueId<'plan>>],
        fallback_results: &[ConstructionValueId<'plan>],
    ) -> Result<Box<[ConstructionValueId<'plan>]>, PlanConstructionError> {
        let claim = ConditionalGroupCloseClaim {
            conditions,
            branch_results,
            fallback_results,
        };
        let (data, joined) = self.authenticate_conditional_group_close(&region, claim)?;
        let captures = self.conditional_group_captures(&region, conditions)?;
        self.record_conditional_group_close(&region, claim, &data, &joined, captures)
    }

    /// Authenticate that the claimed values reproduce, in their exact scopes,
    /// every operand of every joined conditional the group publishes.
    ///
    /// Returns the authenticated source group and the exact joined conditional
    /// expression of each published definition.
    fn authenticate_conditional_group_close(
        &mut self,
        region: &ConditionalGroupConstruction<'plan, 'dae>,
        claim: ConditionalGroupCloseClaim<'_, 'plan>,
    ) -> Result<(super::ConditionalSource<'dae>, Box<[ExprId<'dae>]>), PlanConstructionError> {
        let span = region.source.provenance.span();
        let parent = self.scope_allow_open(region.parent, span)?;
        let owner = parent.owner;
        if !self
            .open_conditional_groups
            .contains(&region.source.conditional)
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let data = self.conditional_source(region.source)?;
        let width = data.definitions.len();
        if claim.conditions.len() != data.conditions.len()
            || claim.branch_results.len() != data.branches.len()
            || claim
                .branch_results
                .iter()
                .any(|branch| branch.len() != width)
            || claim.fallback_results.len() != width
            || region.branches.len() != data.branches.len()
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let joined = data
            .definitions
            .iter()
            .map(|definition| self.dae.exact_function_definition(*definition).rhs())
            .collect::<Box<[_]>>();
        for (condition_ordinal, condition) in claim.conditions.iter().copied().enumerate() {
            self.check_region_value(RegionValueClaim {
                value: condition,
                owner,
                scope: region.parent.raw,
                consumer: self.dae.exact_expression(joined[0]),
                operand_ordinal: condition_ordinal * 2,
                expected: data.conditions[condition_ordinal],
                exact_scope: false,
                span,
            })?;
        }
        for (branch_ordinal, branch) in claim.branch_results.iter().enumerate() {
            for (result_ordinal, result) in branch.iter().copied().enumerate() {
                self.check_region_value(RegionValueClaim {
                    value: result,
                    owner,
                    scope: region.branches[branch_ordinal].raw,
                    consumer: self.dae.exact_expression(joined[result_ordinal]),
                    operand_ordinal: branch_ordinal * 2 + 1,
                    expected: data.branches[branch_ordinal][result_ordinal],
                    exact_scope: true,
                    span,
                })?;
            }
        }
        for (result_ordinal, result) in claim.fallback_results.iter().copied().enumerate() {
            self.check_region_value(RegionValueClaim {
                value: result,
                owner,
                scope: region.fallback.raw,
                consumer: self.dae.exact_expression(joined[result_ordinal]),
                operand_ordinal: data.conditions.len() * 2,
                expected: data.fallback[result_ordinal],
                exact_scope: true,
                span,
            })?;
        }
        for child in region.child_scopes() {
            let child = self.scope_allow_open(child, span)?;
            if child.parent != Some(region.parent.raw) || child.open_children != 0 || child.closed {
                return Err(PlanConstructionError::InvalidOperation { span });
            }
        }
        Ok((data, joined))
    }

    /// Values the group body reads from outside itself, minus the conditions,
    /// which are already explicit operands of the group operation.
    fn conditional_group_captures(
        &self,
        region: &ConditionalGroupConstruction<'plan, 'dae>,
        conditions: &[ConstructionValueId<'plan>],
    ) -> Result<BTreeSet<u32>, PlanConstructionError> {
        let span = region.source.provenance.span();
        let mut captures = BTreeSet::new();
        for child in region.child_scopes() {
            self.collect_region_captures(child.raw, &mut captures, span)?;
        }
        for condition in conditions {
            captures.remove(&condition.raw);
        }
        Ok(captures)
    }

    fn record_conditional_group_close(
        &mut self,
        region: &ConditionalGroupConstruction<'plan, 'dae>,
        claim: ConditionalGroupCloseClaim<'_, 'plan>,
        data: &super::ConditionalSource<'dae>,
        joined: &[ExprId<'dae>],
        captures: BTreeSet<u32>,
    ) -> Result<Box<[ConstructionValueId<'plan>]>, PlanConstructionError> {
        let span = region.source.provenance.span();
        let owner = self.scope_allow_open(region.parent, span)?.owner;
        let child_count = region
            .branches
            .len()
            .checked_add(1)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        let remaining_open_children = self.scopes[region.parent.raw as usize]
            .open_children
            .checked_sub(child_count)
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        let operation = checked_id(self.operations.len(), span)?;
        let destinations = self.published_group_values(joined.len(), span)?;
        if !self
            .outstanding_conditionals
            .remove(&region.source.conditional)
        {
            return Err(PlanConstructionError::DuplicateOccurrence { span });
        }
        for expression in joined.iter().copied() {
            self.claim_expression(CallableExpressionSource {
                function: region.source.function,
                expression,
                provenance: self.dae.exact_expression(expression).provenance(),
                marker: PhantomData,
            })?;
        }
        let operands = claim
            .conditions
            .iter()
            .map(|value| value.raw)
            .chain(captures.iter().copied())
            .collect();
        self.operations.push(OperationEntry {
            owner,
            scope: region.parent.raw,
            source: OperationSource::ConditionalGroup {
                expressions: joined.iter().map(|expression| expression.index()).collect(),
                definitions: data
                    .definitions
                    .iter()
                    .map(|definition| definition.ordinal())
                    .collect(),
            },
            contract: OperationContractKey::Conditional,
            operands,
            results: destinations.clone(),
            detail: CallableOperationDetail::Plain,
            provenance: region.source.provenance,
        });
        for expression in joined.iter().copied() {
            self.values.push(ValueEntry {
                owner,
                scope: region.parent.raw,
                source_expression: expression.index(),
                value_type: callable_value_type(
                    self.dae,
                    self.dae.exact_expression(expression).value_type_id(),
                    span,
                )?,
                producer: ValueProducer::Operation(operation),
            });
        }
        self.regions.push(StructuredRegionEntry {
            owner,
            operation,
            contract: OperationContractKey::Conditional,
            captures: captures.into_iter().collect(),
            results: destinations.clone(),
            detail: conditional_group_region_detail(region, claim),
            domain: None,
            provenance: region.source.provenance,
        });
        for child in region.child_scopes() {
            self.scopes[child.raw as usize].closed = true;
        }
        self.scopes[region.parent.raw as usize].open_children = remaining_open_children;
        self.scopes[region.parent.raw as usize]
            .operations
            .push(operation);
        self.owners[owner as usize].operations.push(operation);
        self.open_conditional_groups
            .remove(&region.source.conditional);
        Ok(destinations
            .iter()
            .copied()
            .map(|raw| ConstructionValueId {
                raw,
                marker: PhantomData,
            })
            .collect())
    }

    /// Reserve the contiguous value identities the group publishes, one per
    /// joined definition.
    fn published_group_values(
        &self,
        width: usize,
        span: Span,
    ) -> Result<Box<[u32]>, PlanConstructionError> {
        let first_value = self.values.len();
        (0..width)
            .map(|ordinal| {
                checked_id(
                    first_value
                        .checked_add(ordinal)
                        .ok_or(PlanConstructionError::IdentityOverflow { span })?,
                    span,
                )
            })
            .collect()
    }

    fn conditional_source(
        &self,
        source: CallableConditionalSource<'plan, 'dae>,
    ) -> Result<super::ConditionalSource<'dae>, PlanConstructionError> {
        self.conditionals
            .iter()
            .find(|item| item.id == source.conditional && item.provenance == source.provenance)
            .cloned()
            .ok_or(PlanConstructionError::InvalidSourceOccurrence {
                span: source.provenance.span(),
            })
    }

    fn validate_conditional_group_source(
        &self,
        data: &super::ConditionalSource<'dae>,
    ) -> Result<(), PlanConstructionError> {
        let span = data.provenance.span();
        let width = data.definitions.len();
        if width == 0
            || data.conditions.is_empty()
            || data.conditions.len() != data.branches.len()
            || data.branches.iter().any(|branch| branch.len() != width)
            || data.fallback.len() != width
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        for (result_ordinal, definition) in data.definitions.iter().copied().enumerate() {
            let definition = self.dae.exact_function_definition(definition);
            let expression = self.dae.exact_expression(definition.rhs());
            let ExpressionOperation::Conditional(operands) = expression.operation() else {
                return Err(PlanConstructionError::InvalidOperation { span });
            };
            let expected = data
                .conditions
                .iter()
                .copied()
                .zip(data.branches.iter().map(|branch| branch[result_ordinal]))
                .flat_map(|(condition, branch)| [condition, branch])
                .chain(std::iter::once(data.fallback[result_ordinal]));
            if !operands.iter().eq(expected) {
                return Err(PlanConstructionError::InvalidOperation { span });
            }
        }
        Ok(())
    }
}

/// The retained branch, fallback, condition, and result topology of one closed
/// atomic conditional group.
fn conditional_group_region_detail(
    region: &ConditionalGroupConstruction<'_, '_>,
    claim: ConditionalGroupCloseClaim<'_, '_>,
) -> StructuredRegionDetail {
    StructuredRegionDetail::Conditional {
        branch_scopes: region.branches.iter().map(|scope| scope.raw).collect(),
        fallback_scope: region.fallback.raw,
        conditions: claim.conditions.iter().map(|value| value.raw).collect(),
        branch_results: claim
            .branch_results
            .iter()
            .map(|branch| branch.iter().map(|value| value.raw).collect())
            .collect(),
        fallback_results: claim
            .fallback_results
            .iter()
            .map(|value| value.raw)
            .collect(),
    }
}
