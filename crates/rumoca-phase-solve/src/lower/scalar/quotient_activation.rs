//! Lazy projection of an issued model-arm quotient (MLS §3.6.5 / §8.5).
//!
//! Only safe domain coordinates and the private held step are captures. The
//! ratio, including potentially invalid arithmetic, belongs to the selected
//! region and is never evaluated to construct the conditional's captures.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn guarded_quotient_step(
        &mut self,
        memory: crate::layout::QuotientMemory<'dae>,
        guard: dae::ExprId<'dae>,
        held: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut selected = self.selected_arm_region(&[(guard, true)], span)?;
        let ratio = selected.expression(memory.ratio, 0)?;
        let step = selected.quotient_step(memory.builtin, ratio, span)?;
        selected
            .ops
            .push(solve::LinearOp::StoreOutput { src: step });
        let mut fallback = self.selected_arm_region(&[(guard, false)], span)?;
        let count = self.model_arm_captures().len();
        let output = fallback.register(span)?;
        fallback
            .ops
            .push(solve::LinearOp::LoadFunctionConditionalCapture {
                dst: output,
                index: count,
            });
        fallback
            .ops
            .push(solve::LinearOp::StoreOutput { src: output });
        let mut captures = self.model_arm_captures();
        captures.push(held);
        self.emit_activated_region(guard, captures, selected.ops, fallback.ops, span)
    }

    pub(super) fn activated_root_expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        guard: Option<dae::ExprId<'dae>>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let Some(guard) = guard else {
            return self.root_expression(expression, span);
        };
        let mut selected = self.selected_arm_region(&[(guard, true)], span)?;
        let output = selected.root_expression(expression, span)?;
        selected
            .ops
            .push(solve::LinearOp::StoreOutput { src: output });
        let mut fallback = self.selected_arm_region(&[(guard, false)], span)?;
        let output = fallback.constant(1.0, span)?;
        fallback
            .ops
            .push(solve::LinearOp::StoreOutput { src: output });
        self.emit_activated_region(
            guard,
            self.model_arm_captures(),
            selected.ops,
            fallback.ops,
            span,
        )
    }

    fn model_arm_captures(&self) -> Vec<solve::Reg> {
        self.symbolic_domain_points
            .iter()
            .flat_map(|(_, registers)| registers.iter().copied())
            .collect()
    }

    fn emit_activated_region(
        &mut self,
        guard: dae::ExprId<'dae>,
        captures: Vec<solve::Reg>,
        selected: Vec<solve::LinearOp>,
        fallback: Vec<solve::LinearOp>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut condition = self.selected_arm_region(&[], span)?;
        let output =
            condition.with_relation_observation(|compiler| compiler.expression(guard, 0))?;
        condition
            .ops
            .push(solve::LinearOp::StoreOutput { src: output });
        let program = solve::FunctionConditionalProgram::checked(
            captures.len(),
            vec![1],
            vec![(condition.ops, selected)],
            fallback,
        )
        .map_err(|error| {
            LowerError::contract(format!("quotient activation proof failed: {error}"), span)
        })?;
        self.emit_conditional_program(captures, program, span)
    }
}
