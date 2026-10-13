//! MLS §8.6 relation truth at each initialization iterate.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn initialization_relation(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        memory: usize,
    ) -> Result<solve::Reg, LowerError> {
        if let Some(register) = self.cached_dominating_expression(expression, scalar) {
            return Ok(register);
        }
        let span = self.node(expression).provenance().span();
        let initial = self
            .layout
            .solve_layout
            .initial_event_parameter_index
            .ok_or_else(|| {
                LowerError::contract("buffered relation has no initialization mode", span)
            })?;
        let captures = self
            .symbolic_domain_points
            .iter()
            .flat_map(|(_, registers)| registers.iter().copied())
            .collect::<Vec<_>>();
        let mut selected = self.selected_arm_region(&[], span)?;
        let value = selected.with_relation_observation(|compiler| {
            compiler.unbuffered_expression(expression, scalar)
        })?;
        selected.emit(solve::LinearOp::StoreOutput { src: value })?;
        // The literal relation is evaluated only during initialization. In
        // transient mode even a faulting operand must remain unevaluated until
        // its root observation, exactly as with the original memory read.
        let program = solve::FunctionConditionalProgram::checked(
            captures.len(),
            vec![1],
            vec![(parameter_program(initial), selected.finish_operations()?)],
            parameter_program(memory),
        )
        .map_err(|error| {
            LowerError::contract(
                format!("initialization relation proof failed: {error}"),
                span,
            )
        })?;
        let value = self.emit_conditional_program(captures, program, span)?;
        self.expression_cache
            .insert((self.context_id, expression, scalar), value);
        Ok(value)
    }
}

fn parameter_program(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}
