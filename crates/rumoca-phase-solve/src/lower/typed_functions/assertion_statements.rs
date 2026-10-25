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

    /// The gate after one more error-level assertion: its predicate, conjoined
    /// with the gate of the error-level assertions before it.
    pub(super) fn extend_assertion_gate(
        &mut self,
        gate: Option<solve::ProgramRegister<'program>>,
        index: usize,
        provenance: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let predicate = self
            .predicate_values
            .get(index)
            .copied()
            .flatten()
            .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
        match gate {
            None => Ok(predicate),
            Some(gate) => {
                self.builder
                    .binary(solve::SolveBinaryOperator::And, gate, predicate, provenance)
            }
        }
    }

    /// An assignment statement's value; under a gate, the value is computed
    /// only in the region the gate selects, and is a neutral value otherwise.
    /// The neutral value is never observed: a false gate means an earlier
    /// error-level assertion failed and the call reports that assertion.
    pub(super) fn gated_function_definition_value(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        gate: Option<solve::ProgramRegister<'program>>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let Some(gate) = gate else {
            self.function_definition_value(definition)?;
            return Ok(());
        };
        if self.function_values.contains_key(&definition.id()) {
            return Ok(());
        }
        let provenance = definition.provenance().span();
        let rhs = definition.rhs();
        let value_type = function_value_type(self.view, definition.target(), provenance)?;
        let pending = self.pending_predicates(std::iter::once(rhs))?;
        let leaf_types = lower_value_type_leaves(self.view, value_type, arithmetic_profile())?;
        let value_leaf_count = leaf_types.len();
        let mut output_types = leaf_types.clone();
        output_types.extend(self.pending_slot_types(&pending));
        let (captures, environment) = self.capture_environment_for(std::iter::once(rhs))?;
        let context = self.region_context();
        let true_environment = environment.clone();
        let true_context = context.clone();
        let true_pending = pending.clone();
        let false_pending = pending.clone();
        let destinations = self.builder.conditional(
            gate,
            &captures,
            output_types,
            provenance,
            move |builder, inputs, outputs| {
                lower_region_conditional(
                    builder,
                    inputs,
                    outputs,
                    &true_environment,
                    &true_context,
                    RegionConditional {
                        value_type,
                        operands: vec![rhs],
                        pending: true_pending,
                        provenance,
                    },
                )
            },
            move |builder, inputs, outputs| {
                let mut lowerer =
                    load_region_lowerer(builder, inputs, &environment, &context, provenance)?;
                let mut values = Vec::with_capacity(outputs.len());
                for leaf in &leaf_types {
                    values.push(neutral_leaf(&mut *lowerer.builder, leaf, provenance)?);
                }
                for slot in false_pending {
                    values.push(lowerer.inactive_slot(slot, provenance)?);
                }
                if values.len() != outputs.len() {
                    return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance,
                    });
                }
                for (output, source) in outputs.iter().zip(values) {
                    lowerer.builder.store(*output, source, provenance)?;
                }
                Ok(())
            },
        )?;
        for slot in pending {
            let predicate = self.inactive_slot(slot, provenance)?;
            let value = self
                .predicate_values
                .get_mut(slot)
                .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
            if value.replace(predicate).is_some() {
                return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
            }
        }
        self.function_values.insert(
            definition.id(),
            LoweredValue {
                value_type,
                leaves: destinations[..value_leaf_count].to_vec(),
            },
        );
        Ok(())
    }
}

/// A zero, false or 0 constant of `leaf`'s element type, filled to its shape.
fn neutral_leaf<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    leaf: &solve::SolveValueType,
    provenance: rumoca_core::Span,
) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
    let value = match leaf.element_type() {
        solve::SolveScalarType::Real { .. } => solve::SolveValue::real(arithmetic_profile(), 0.0),
        solve::SolveScalarType::Integer(_) => solve::SolveValue::integer(arithmetic_profile(), 0)
            .map_err(|_| {
            solve::SolveProgramConstructionError::InvalidCallOutput { provenance }
        })?,
        solve::SolveScalarType::Boolean => solve::SolveValue::boolean(false),
    };
    let scalar = builder.constant(value, provenance)?;
    if leaf.dimensions().is_empty() {
        Ok(scalar)
    } else {
        builder.fill(scalar, leaf.dimensions().to_vec(), provenance)
    }
}
