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

    pub(in crate::lower) fn root_program(
        mut self,
        relation: dae::RelationId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let relation = self
            .view
            .relation(relation)
            .expect("checked relation identity resolves");
        let node = self.node(relation.expression());
        let span = relation.provenance().span();
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
                let condition = self.expression(relation.expression(), 0)?;
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

    fn trigger_edge(
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

    fn condition(&mut self, condition: dae::ConditionId<'dae>) -> Result<solve::Reg, LowerError> {
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
                None => Err(LowerError::unsupported(
                    "clock condition has no owning Solve schedule",
                    span,
                )),
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
            dae::ConditionOperation::Or(lhs, rhs) => {
                let lhs = self.condition(lhs)?;
                let rhs = self.condition(rhs)?;
                self.binary(dae::BinaryOperator::Or, lhs, rhs, span)
            }
        }
    }
}
