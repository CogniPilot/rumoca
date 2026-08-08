use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::ScalarCompiler;
use crate::LowerError;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(in crate::lower) fn condition_program(
        mut self,
        condition: dae::ConditionId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let output = self.condition(condition)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    /// Compile a condition that lives inside `clock`'s partition.
    ///
    /// A clocked relation reads `previous(...)` and clock-owned declarations, which only
    /// resolve while their owning schedule is the active one (MLS §16.5).
    pub(in crate::lower) fn clocked_condition_program(
        mut self,
        clock: dae::ClockId<'dae>,
        condition: dae::ConditionId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        let output = self.condition(condition)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(in crate::lower) fn edge_condition_program(
        mut self,
        trigger: dae::ConditionId<'dae>,
        guard: dae::ConditionId<'dae>,
        trigger_memory: usize,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let edge = self.trigger_edge(trigger, trigger_memory, span)?;
        let guard = self.condition(guard)?;
        let output = self.binary(dae::BinaryOperator::And, edge, guard, span)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(in crate::lower) fn clocked_action_condition_program(
        mut self,
        clock: dae::ClockId<'dae>,
        guard: dae::ConditionId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        let guard = self.condition(guard)?;
        let activation = self
            .layout
            .clock_activations
            .get(clock.index() as usize)
            .copied()
            .ok_or_else(|| {
                LowerError::contract("clocked action has no activation parameter", span)
            })?;
        let activation = self.load_slot(solve::scalar_slot_p(activation), span)?;
        let output = self.binary(dae::BinaryOperator::And, activation, guard, span)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(in crate::lower) fn root_program(
        self,
        relation: dae::RelationId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let relation = self
            .view
            .relation(relation)
            .expect("checked relation identity resolves");
        self.root_expression_program(relation.expression(), relation.provenance().span())
    }

    pub(in crate::lower) fn root_expression_program(
        mut self,
        expression: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let node = self.node(expression);
        let output = match node.operation() {
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Less | dae::BinaryOperator::LessEqual,
                lhs,
                rhs,
            } => {
                let lhs = self.expression(lhs, 0)?;
                let rhs = self.expression(rhs, 0)?;
                self.binary(dae::BinaryOperator::Subtract, lhs, rhs, span)?
            }
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Greater | dae::BinaryOperator::GreaterEqual,
                lhs,
                rhs,
            } => {
                let lhs = self.expression(lhs, 0)?;
                let rhs = self.expression(rhs, 0)?;
                self.binary(dae::BinaryOperator::Subtract, rhs, lhs, span)?
            }
            _ => {
                let condition = self.expression(expression, 0)?;
                let when_true = self.constant(-1.0, span)?;
                let when_false = self.constant(1.0, span)?;
                self.select(condition, when_true, when_false, span)?
            }
        };
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(in crate::lower) fn guarded_assignments_program(
        mut self,
        branches: &[(
            dae::ConditionId<'dae>,
            dae::ConditionId<'dae>,
            dae::ExprId<'dae>,
            usize,
            usize,
        )],
        target: solve::ScalarSlot,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut selected = self.load_slot(target, span)?;
        for &(trigger, guard, value, scalar, trigger_memory) in branches.iter().rev() {
            let edge = self.trigger_edge(trigger, trigger_memory, span)?;
            let guard = self.condition(guard)?;
            let condition = self.binary(dae::BinaryOperator::And, edge, guard, span)?;
            let value = self.expression(value, scalar)?;
            selected = self.select(condition, value, selected, span)?;
        }
        self.ops
            .push(solve::LinearOp::StoreOutput { src: selected });
        Ok(self.ops)
    }

    pub(in crate::lower) fn clocked_guarded_assignments_program(
        mut self,
        clock: dae::ClockId<'dae>,
        branches: &[(
            dae::ConditionId<'dae>,
            dae::ConditionId<'dae>,
            dae::ExprId<'dae>,
            usize,
            usize,
        )],
        target: solve::ScalarSlot,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        let mut selected = self.load_slot(target, span)?;
        for &(_, guard, value, scalar, _) in branches.iter().rev() {
            let condition = self.condition(guard)?;
            let value = self.expression(value, scalar)?;
            selected = self.select(condition, value, selected, span)?;
        }
        self.ops
            .push(solve::LinearOp::StoreOutput { src: selected });
        Ok(self.ops)
    }

    /// The rising edge that activates `trigger`.
    ///
    /// MLS §8.3.5.1 gives a `when` the activation `edge(b)` over one `Boolean b`
    /// holding the condition, and a *vector* `when` one `bi` per element with
    /// the activation `edge(b1) or … or edge(bn)`. The two are different
    /// functions of the same operands — `{u, not u}` rises at every switch of
    /// `u` while `u or not u` never rises — so a vector activation is expanded
    /// leaf by leaf here rather than read as a single buffered disjunction.
    fn trigger_edge(
        &mut self,
        trigger: dae::ConditionId<'dae>,
        trigger_memory: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if matches!(
            self.view
                .condition(trigger)
                .expect("checked condition identity resolves")
                .operation(),
            dae::ConditionOperation::AnyRise(_, _)
        ) {
            return self.any_element_edge(trigger, span);
        }
        self.buffered_edge(trigger, trigger_memory, span)
    }

    /// `edge(b1) or … or edge(bn)` over the leaves of an `AnyRise` tree.
    fn any_element_edge(
        &mut self,
        condition: dae::ConditionId<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let operation = self
            .view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation();
        let dae::ConditionOperation::AnyRise(lhs, rhs) = operation else {
            // A leaf of the vector: its own buffer, its own edge.
            let memory = crate::lower::events::condition_memory(self.layout, condition, span)?;
            return self.buffered_edge(condition, memory, span);
        };
        let lhs = self.any_element_edge(lhs, span)?;
        let rhs = self.any_element_edge(rhs, span)?;
        self.binary(dae::BinaryOperator::Or, lhs, rhs, span)
    }

    fn buffered_edge(
        &mut self,
        trigger: dae::ConditionId<'dae>,
        trigger_memory: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let current = self.condition(trigger)?;
        let previous = self.load_slot(solve::scalar_slot_p(trigger_memory), span)?;
        let not_previous = self.unary(dae::UnaryOperator::Not, previous, span)?;
        self.binary(dae::BinaryOperator::And, current, not_previous, span)
    }

    pub(super) fn condition(
        &mut self,
        condition: dae::ConditionId<'dae>,
    ) -> Result<solve::Reg, LowerError> {
        let condition = self
            .view
            .condition(condition)
            .expect("checked condition identity resolves");
        let span = condition.provenance().span();
        match condition.operation() {
            dae::ConditionOperation::Initial => {
                let index = self
                    .layout
                    .solve_layout
                    .initial_event_parameter_index
                    .ok_or_else(|| {
                        LowerError::non_computable(
                            "initial condition has no checked Solve storage",
                            span,
                        )
                    })?;
                self.load_slot(solve::scalar_slot_p(index), span)
            }
            dae::ConditionOperation::Relation(relation) => {
                let expression = self
                    .view
                    .relation(relation)
                    .expect("checked condition relation resolves")
                    .expression();
                self.expression(expression, 0)
            }
            dae::ConditionOperation::Discrete(expression) => self.expression(expression, 0),
            dae::ConditionOperation::Clock(clock) => match self.active_clock {
                Some(active) if active == clock => self.constant(1.0, span),
                Some(_) => Err(LowerError::non_computable(
                    "clocked condition refers to a different activation owner",
                    span,
                )),
                None => {
                    let index = self
                        .layout
                        .clock_activations
                        .get(clock.index() as usize)
                        .copied()
                        .ok_or_else(|| {
                            LowerError::contract(
                                "clock condition has no derived Solve activation lane",
                                span,
                            )
                        })?;
                    self.load_slot(solve::scalar_slot_p(index), span)
                }
            },
            dae::ConditionOperation::Not(operand) => {
                let operand = self.condition(operand)?;
                self.unary(dae::UnaryOperator::Not, operand, span)
            }
            dae::ConditionOperation::And(lhs, rhs) => {
                let lhs = self.condition(lhs)?;
                let rhs = self.condition(rhs)?;
                self.binary(dae::BinaryOperator::And, lhs, rhs, span)
            }
            // The *level* of a vector activation is the disjunction of its
            // elements; only its edge is per-element (`trigger_edge`). Reading
            // it as a level is what a guard does, and `edge(bi)` implies `bi`,
            // so the guard never narrows the activation it accompanies.
            dae::ConditionOperation::Or(lhs, rhs) | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                let lhs = self.condition(lhs)?;
                let rhs = self.condition(rhs)?;
                self.binary(dae::BinaryOperator::Or, lhs, rhs, span)
            }
            // An unguarded algorithm section and a section-level `assert` are
            // not `when`s; they carry no §8.5 buffer, so `edge` over this reads
            // the level itself (see `lower_condition_memory`).
            dae::ConditionOperation::Always => self.constant(1.0, span),
        }
    }
}
