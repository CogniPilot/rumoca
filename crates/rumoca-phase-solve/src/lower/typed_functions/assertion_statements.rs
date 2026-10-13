//! Source-positioned checks and failed-only message captures.

use super::*;

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn assertion_statement(
        &mut self,
        condition: dae::ExprId<'dae>,
        provenance: rumoca_core::Span,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let index = self.next_direct_assertion;
        let declaration = self
            .direct_assertions
            .get(index)
            .cloned()
            .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
        if index >= self.direct_assertion_count
            || declaration.predicate_output != self.assertion_output_base + index
        {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let predicate = self.expression(condition)?.only_register(provenance)?;
        let expressions = declaration
            .message_values
            .iter()
            .map(|&(value, _)| value)
            .collect::<Vec<_>>();
        let (captures, environment) =
            self.capture_environment_for_message(expressions.iter().copied())?;
        let context = self.region_context();
        let assertion = self
            .builder
            .assertion_output(declaration.predicate_output, provenance)?;
        let values = self.builder.check_assertion(
            assertion,
            predicate,
            &captures,
            provenance,
            move |builder, inputs, outputs| {
                let mut lowerer =
                    load_region_lowerer(builder, inputs, &environment, &context, provenance)?;
                for (expression, output) in expressions.iter().zip(outputs) {
                    let value = lowerer.expression(*expression)?.only_register(provenance)?;
                    lowerer.builder.store(*output, value, provenance)?;
                }
                Ok(())
            },
        )?;
        if values.len() != declaration.message_values.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        self.predicate_values[index] = Some(predicate);
        for ((_, output), value) in declaration.message_values.iter().zip(values) {
            let slot = output
                .checked_sub(self.assertion_output_base)
                .and_then(|slot| self.predicate_values.get_mut(slot))
                .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
            *slot = Some(value);
        }
        self.next_direct_assertion += 1;
        Ok(())
    }
}
