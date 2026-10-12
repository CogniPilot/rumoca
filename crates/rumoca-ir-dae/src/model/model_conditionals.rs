//! Model conditional arms own the activation of their event-producing values.
//!
//! The open owner issues arm contexts; its checked close is the only operation
//! that discharges those contexts. MLS §3.6.5 selects one value expression.

use super::*;
use crate::UnaryOperator;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ModelConditionalEntry {
    pub(crate) condition: u32,
    pub(crate) arms: [u32; 2],
    pub(super) closed: Option<ClosedModelConditional>,
    pub(crate) provenance: DaeProvenance,
    guard_operations: [Option<u32>; 3],
}

/// Private proof issued by the checked close, consumed by the frozen view.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct ClosedModelConditional {
    pub(super) condition: u32,
    pub(super) arms: [u32; 2],
    pub(super) expression: u32,
    pub(super) provenance: DaeProvenance,
    pub(super) guard_operations: [Option<u32>; 3],
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) struct ModelArmEntry {
    pub(crate) owner: u32,
    pub(crate) parent: Option<u32>,
    pub(crate) guard: u32,
}

/// Project an exact guard from construction-issued expression scope.
pub(super) fn activation_guard(
    scopes: &[Option<u32>],
    arms: &[ModelArmEntry],
    expression: u32,
) -> Option<u32> {
    scopes[expression as usize].map(|arm| arms[arm as usize].guard)
}

pub(super) fn expression_roots(
    scopes: &[Option<u32>],
    arms: &[ModelArmEntry],
    expression: u32,
) -> impl Iterator<Item = u32> {
    [Some(expression), activation_guard(scopes, arms, expression)]
        .into_iter()
        .flatten()
}

/// One in-flight model conditional. The fields cannot name a foreign arm.
pub struct ModelConditional<'dae> {
    owner: u32,
    provenance: DaeProvenance,
    marker: PhantomData<&'dae mut &'dae ()>,
}

/// One exact arm of a construction-issued conditional owner.
#[derive(Clone, Copy)]
pub struct ModelArm<'dae> {
    pub(crate) index: u32,
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl ModelArm<'_> {
    pub const fn index(self) -> u32 {
        self.index
    }
}

impl<'dae> ModelConditional<'dae> {
    pub fn arm(&self, construction: &DaeConstruction<'dae>, selected: bool) -> ModelArm<'dae> {
        ModelArm {
            index: construction.storage.model_conditionals[self.owner as usize].arms
                [usize::from(!selected)],
            marker: PhantomData,
        }
    }
}

impl<'dae> DaeConstruction<'dae> {
    /// Read the exact arm guard issued for a checked model expression.
    pub fn model_activation_guard(
        &self,
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<Option<ExprId<'dae>>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .expect_relation_model_arm(expression, provenance)?;
        Ok(activation_guard(
            &self.storage.expressions.model_arm_scopes,
            &self.storage.model_arms,
            expression.index(),
        )
        .map(ExprId::from_raw))
    }

    pub(super) fn without_model_arm<T>(
        &mut self,
        build: impl FnOnce(&mut Self) -> Result<T, DaeConstructionError>,
    ) -> Result<T, DaeConstructionError> {
        let prior_function = self.storage.active_function_boundary;
        self.storage.active_function_boundary = true;
        let parent = self.storage.active_model_arm.take();
        let ancestors = std::mem::take(&mut self.storage.active_model_ancestors);
        let result = build(self);
        self.storage.active_model_arm = parent;
        self.storage.active_model_ancestors = ancestors;
        self.storage.active_function_boundary = prior_function;
        result
    }
    /// Issue both arms from the actual checked condition and current parent.
    pub fn begin_model_conditional(
        &mut self,
        condition: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ModelConditional<'dae>, DaeConstructionError> {
        self.expressions(|expressions| expressions.begin_model_conditional(condition, provenance))
    }

    pub(crate) fn issue_model_conditional(
        &mut self,
        condition: ExprId<'dae>,
        guards: [ExprId<'dae>; 2],
        parent: Option<u32>,
        provenance: DaeProvenance,
    ) -> Result<ModelConditional<'dae>, DaeConstructionError> {
        self.expressions(|expressions| {
            expressions.issue_model_conditional(condition, guards, parent, provenance)
        })
    }

    /// Replay the same semantic owner from already rebuilt canonical guards.
    pub fn replay_model_conditional(
        &mut self,
        condition: ExprId<'dae>,
        guards: [ExprId<'dae>; 2],
        provenance: DaeProvenance,
    ) -> Result<ModelConditional<'dae>, DaeConstructionError> {
        self.issue_model_conditional(condition, guards, self.storage.active_model_arm, provenance)
    }

    /// Advance one reconstruction operation's lexical context. A transition
    /// crosses at most one parent edge, or swaps two arms of that parent.
    pub fn replay_model_arm_context(
        &mut self,
        arm: Option<ModelArm<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let next = arm.map(|arm| arm.index);
        if self.storage.active_function_boundary && next.is_some() {
            return Err(DaeConstructionError::InvalidModelActivationScope {
                span: provenance.span(),
            });
        }
        if next == self.storage.active_model_arm {
            return Ok(());
        }
        let parent = |scope: Option<u32>| {
            scope
                .and_then(|scope| self.storage.model_arms.get(scope as usize))
                .and_then(|entry| entry.parent)
        };
        if let Some(next) = next {
            let entry = self
                .storage
                .model_arms
                .get(next as usize)
                .ok_or_else(|| unknown("model arm", next, provenance))?;
            if self.storage.model_conditionals[entry.owner as usize]
                .closed
                .is_some()
            {
                return Err(DaeConstructionError::InvalidModelActivationScope {
                    span: provenance.span(),
                });
            }
        }
        let current = self.storage.active_model_arm;
        if !(parent(next) == current || parent(current) == next || parent(next) == parent(current))
        {
            return Err(DaeConstructionError::InvalidModelActivationScope {
                span: provenance.span(),
            });
        }
        if let Some(current) = current
            && parent(next) != Some(current)
        {
            self.storage.active_model_ancestors.remove(&current);
        }
        if let Some(next) = next {
            self.storage.active_model_ancestors.insert(next);
        }
        self.storage.active_model_arm = next;
        Ok(())
    }

    /// Evaluate construction in one issued arm; restore the parent on error.
    pub fn model_conditional_arm<T>(
        &mut self,
        owner: &ModelConditional<'dae>,
        selected: bool,
        build: impl FnOnce(&mut Self) -> Result<T, DaeConstructionError>,
    ) -> Result<T, DaeConstructionError> {
        self.expressions(|expressions| {
            expressions.model_conditional_arm_context(owner, Some(selected))
        })?;
        let result = build(self);
        self.expressions(|expressions| expressions.model_conditional_arm_context(owner, None))?;
        result
    }

    /// Consume the actual then/else arms in their source order.
    pub fn finish_model_conditional(
        &mut self,
        owner: ModelConditional<'dae>,
        values: [ExprId<'dae>; 2],
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        self.expressions(|expressions| expressions.finish_model_conditional(owner, values))
    }
}

impl<'storage, 'dae> Expressions<'storage, 'dae> {
    pub fn begin_model_conditional(
        &mut self,
        condition: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ModelConditional<'dae>, DaeConstructionError> {
        self.validate_model_condition(condition, provenance)?;
        let parent = self.storage.active_model_arm;
        let parent_guard = parent.map(|arm| self.storage.model_arms[arm as usize].guard);
        let guards = (|| {
            let expressions = &mut *self;
            let not = expressions
                .at(provenance)
                .unary(UnaryOperator::Not, condition)?;
            match parent_guard {
                None => Ok([condition, not]),
                Some(parent) => {
                    let no = expressions
                        .at(provenance)
                        .literal(DaeLiteral::Boolean(false))?;
                    let then_guard = expressions
                        .at(provenance)
                        .conditional([(ExprId::from_raw(parent), condition)], no)?;
                    let else_guard = expressions
                        .at(provenance)
                        .conditional([(ExprId::from_raw(parent), not)], no)?;
                    Ok([then_guard, else_guard])
                }
            }
        })()?;
        self.issue_model_conditional(condition, guards, parent, provenance)
    }
    fn validate_model_condition(
        &self,
        condition: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        if self.storage.active_function_boundary {
            return Err(DaeConstructionError::InvalidModelActivationScope {
                span: provenance.span(),
            });
        }
        let ty = self.storage.expr_type(condition, provenance)?;
        if !ty.is_scalar() || ty.scalar_type() != ScalarType::Boolean {
            return Err(DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            });
        }
        self.storage
            .expect_model_arm_visible(condition.index(), provenance)?;
        Ok(())
    }
    pub(crate) fn issue_model_conditional(
        &mut self,
        condition: ExprId<'dae>,
        guards: [ExprId<'dae>; 2],
        parent: Option<u32>,
        provenance: DaeProvenance,
    ) -> Result<ModelConditional<'dae>, DaeConstructionError> {
        self.validate_model_condition(condition, provenance)?;
        let guard_operations = self
            .storage
            .validate_model_guards(condition, guards, parent, provenance)?;
        let owner = checked_u32(
            self.storage.model_conditionals.len(),
            "model conditional arena",
            provenance,
        )?;
        let first = checked_u32(
            self.storage.model_arms.len(),
            "model conditional arm arena",
            provenance,
        )?;
        let second = first
            .checked_add(1)
            .ok_or_else(|| unknown("model conditional arm", first, provenance))?;
        self.storage.model_conditionals.push(ModelConditionalEntry {
            condition: condition.index(),
            arms: [first, second],
            closed: None,
            provenance,
            guard_operations,
        });
        self.storage
            .model_arms
            .extend(guards.map(|guard| ModelArmEntry {
                owner,
                parent,
                guard: guard.index(),
            }));
        Ok(ModelConditional {
            owner,
            provenance,
            marker: PhantomData,
        })
    }
    pub fn finish_model_conditional(
        &mut self,
        owner: ModelConditional<'dae>,
        values: [ExprId<'dae>; 2],
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let entry = self.storage.model_conditionals[owner.owner as usize].clone();
        if self.storage.active_function_boundary {
            return Err(DaeConstructionError::InvalidModelActivationScope {
                span: entry.provenance.span(),
            });
        }
        if entry.closed.is_some() {
            return Err(duplicate(
                "model conditional",
                owner.owner,
                entry.provenance,
            ));
        }
        if self.storage.active_model_arm != self.storage.model_arms[entry.arms[0] as usize].parent {
            return Err(DaeConstructionError::InvalidModelActivationScope {
                span: entry.provenance.span(),
            });
        }
        for (value, arm) in values.into_iter().zip(entry.arms) {
            self.storage.expr_type(value, entry.provenance)?;
            let scope = self.storage.expressions.model_arm_scopes[value.index() as usize];
            if scope.is_some_and(|scope| {
                scope != arm && !self.storage.active_model_ancestors.contains(&scope)
            }) {
                return Err(DaeConstructionError::InvalidModelActivationScope {
                    span: entry.provenance.span(),
                });
            }
        }
        self.storage.closing_model_conditional = Some(owner.owner);
        let result = self
            .at(entry.provenance)
            .conditional([(ExprId::from_raw(entry.condition), values[0])], values[1]);
        self.storage.closing_model_conditional = None;
        let result = result?;
        self.storage.model_conditionals[owner.owner as usize].closed =
            Some(ClosedModelConditional {
                condition: entry.condition,
                arms: entry.arms,
                expression: result.index(),
                provenance: entry.provenance,
                guard_operations: entry.guard_operations,
            });
        Ok(result)
    }

    /// Enter one arm of an open checked owner, or restore its parent. A
    /// reconstruction must restore the parent on both success and error.
    pub fn model_conditional_arm_context(
        &mut self,
        owner: &ModelConditional<'dae>,
        selected: Option<bool>,
    ) -> Result<(), DaeConstructionError> {
        let entry = self
            .storage
            .model_conditionals
            .get(owner.owner as usize)
            .ok_or_else(|| unknown("model conditional", owner.owner, owner.provenance))?;
        let at = entry.provenance;
        if self.storage.active_function_boundary {
            return Err(DaeConstructionError::InvalidModelActivationScope { span: at.span() });
        }
        if entry.closed.is_some() {
            return Err(duplicate("closed model conditional", owner.owner, at));
        }
        let parent = self.storage.model_arms[entry.arms[0] as usize].parent;
        match selected {
            Some(selected) => {
                if self.storage.active_model_arm != parent {
                    return Err(DaeConstructionError::InvalidModelActivationScope {
                        span: at.span(),
                    });
                }
                let arm = entry.arms[usize::from(!selected)];
                self.storage.active_model_arm = Some(arm);
                self.storage.active_model_ancestors.insert(arm);
            }
            None => {
                let current = self.storage.active_model_arm;
                if !entry.arms.into_iter().any(|arm| current == Some(arm)) {
                    return Err(DaeConstructionError::InvalidModelActivationScope {
                        span: at.span(),
                    });
                }
                if let Some(current) = current {
                    self.storage.active_model_ancestors.remove(&current);
                }
                self.storage.active_model_arm = parent;
            }
        }
        Ok(())
    }
    /// Exact construction scope of a value, including values first rebuilt
    /// while transforming an arm. Global mappings can be shared across arms.
    pub fn model_arm_of(
        &self,
        expression: ExprId<'dae>,
        at: DaeProvenance,
    ) -> Result<Option<ModelArm<'dae>>, DaeConstructionError> {
        let scope = self
            .storage
            .expressions
            .model_arm_scopes
            .get(expression.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", expression.index(), at))?;
        Ok(scope.map(|index| ModelArm {
            index,
            marker: PhantomData,
        }))
    }
    /// The exact current arm is part of a scoped reconstruction cache key.
    pub fn model_arm_context(&self) -> Option<ModelArm<'dae>> {
        self.storage.active_model_arm.map(|index| ModelArm {
            index,
            marker: PhantomData,
        })
    }
}

impl Storage {
    fn validate_model_guards<'dae>(
        &self,
        condition: ExprId<'dae>,
        guards: [ExprId<'dae>; 2],
        parent: Option<u32>,
        at: DaeProvenance,
    ) -> Result<[Option<u32>; 3], DaeConstructionError> {
        if parent != self.active_model_arm {
            return Err(DaeConstructionError::InvalidModelActivationScope { span: at.span() });
        }
        self.expect_model_arm_visible(condition.index(), at)?;
        for guard in guards {
            self.expect_model_arm_visible(guard.index(), at)?;
        }
        let not_condition = |raw| matches!(self.expressions.nodes.get(raw as usize), Some(ExprNode::Unary { operator: UnaryOperator::Not, operand }) if *operand == condition.index());
        let conditional_guard = |raw, negated| {
            let Some(ExprNode::Conditional { operands }) = self.expressions.nodes.get(raw as usize)
            else {
                return None;
            };
            let [parent_guard, selected, fallback] = &self.expressions.operands[operands.indices()]
            else {
                return None;
            };
            let valid = parent
                .is_some_and(|parent| *parent_guard == self.model_arms[parent as usize].guard)
                && (if negated {
                    not_condition(*selected)
                } else {
                    *selected == condition.index()
                })
                && matches!(
                    self.expressions.nodes.get(*fallback as usize),
                    Some(ExprNode::Literal(DaeLiteral::Boolean(false)))
                );
            valid.then_some(*selected)
        };
        let invalid = || DaeConstructionError::InvalidModelActivationScope { span: at.span() };
        if parent.is_none() {
            if guards[0] != condition || !not_condition(guards[1].index()) {
                return Err(invalid());
            }
            return Ok([Some(guards[1].index()), None, None]);
        }
        conditional_guard(guards[0].index(), false).ok_or_else(invalid)?;
        let negated = conditional_guard(guards[1].index(), true).ok_or_else(invalid)?;
        Ok([
            Some(negated),
            Some(guards[0].index()),
            Some(guards[1].index()),
        ])
    }
    pub(crate) fn model_expression_roots(&self, expression: u32) -> impl Iterator<Item = u32> {
        expression_roots(
            &self.expressions.model_arm_scopes,
            &self.model_arms,
            expression,
        )
    }

    /// MLS §3.6.5 / DAE-C22: a relation is observed only under its
    /// construction-issued arm guard.
    /// Live construction requires current lexical visibility. Whole-model
    /// replay may use an arm only after its exact conditional was closed.
    pub(crate) fn expect_relation_model_arm(
        &self,
        expression: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let scope = self
            .expressions
            .model_arm_scopes
            .get(expression.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", expression.index(), at))?;
        let Some(scope) = scope else {
            return Ok(());
        };
        if self.active_function_boundary {
            return Err(DaeConstructionError::InvalidModelActivationScope { span: at.span() });
        }
        if self.active_model_arm.is_some() {
            return self.expect_model_arm_visible(expression.index(), at);
        }
        let owner = self.model_arms[scope as usize].owner;
        if self.model_conditionals[owner as usize].closed.is_none() {
            return Err(DaeConstructionError::InvalidModelActivationScope { span: at.span() });
        }
        Ok(())
    }
    pub(crate) fn expect_discharged_model_arm(
        &self,
        expression: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let scope = self
            .expressions
            .model_arm_scopes
            .get(expression.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", expression.index(), at))?;
        if scope.is_some() && self.owned_quotient_surface.is_none() {
            return Err(DaeConstructionError::InvalidModelActivationScope { span: at.span() });
        }
        Ok(())
    }
    pub(crate) fn expect_model_arm_visible(
        &self,
        expression: u32,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let scope = self
            .expressions
            .model_arm_scopes
            .get(expression as usize)
            .copied()
            .ok_or_else(|| unknown("expression", expression, at))?;
        if let Some(scope) = scope {
            let own_closing_arm = self
                .closing_model_conditional
                .is_some_and(|owner| self.model_arms[scope as usize].owner == owner);
            if !own_closing_arm && !self.active_model_ancestors.contains(&scope) {
                return Err(DaeConstructionError::InvalidModelActivationScope { span: at.span() });
            }
        }
        Ok(())
    }
}
