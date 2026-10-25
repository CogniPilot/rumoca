//! Source-positioned checks and failed-only message captures.

use super::total_conditionals;
use super::*;
use crate::lower::dae_faults::FaultMemo;

/// The error-level predicates before a statement of one statement list. Their
/// conjunction is built only when a statement that can fault needs it.
#[derive(Default)]
pub(super) struct AssertionGate<'program> {
    predicates: Vec<solve::ProgramRegister<'program>>,
    conjunction: Option<solve::ProgramRegister<'program>>,
}

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

    /// Add one more error-level assertion's predicate to the gate.
    pub(super) fn extend_assertion_gate(
        &self,
        gate: &mut AssertionGate<'program>,
        index: usize,
        provenance: rumoca_core::Span,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let predicate = self
            .predicate_values
            .get(index)
            .copied()
            .flatten()
            .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
        gate.predicates.push(predicate);
        gate.conjunction = None;
        Ok(())
    }

    /// The conjunction of the gate's predicates, built once per extension.
    fn assertion_gate_register(
        &mut self,
        gate: &mut AssertionGate<'program>,
        provenance: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        if let Some(conjunction) = gate.conjunction {
            return Ok(conjunction);
        }
        let (&first, rest) = gate
            .predicates
            .split_first()
            .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
        let mut conjunction = first;
        for &predicate in rest {
            conjunction = self.builder.binary(
                solve::SolveBinaryOperator::And,
                conjunction,
                predicate,
                provenance,
            )?;
        }
        gate.conjunction = Some(conjunction);
        Ok(conjunction)
    }

    /// Whether every checked subscript holds a value inside its extent. Each
    /// subscript cannot fault, so evaluating it here is safe.
    fn subscripts_in_bounds(
        &mut self,
        checks: &[(dae::ExprId<'dae>, u32)],
        provenance: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let integer = |value: i64| {
            solve::SolveValue::integer(arithmetic_profile(), value)
                .map_err(|_| solve::SolveProgramConstructionError::InvalidCallOutput { provenance })
        };
        let mut all = None;
        for &(subscript, extent) in checks {
            let index = self.expression(subscript)?.only_register(provenance)?;
            let one = self.builder.constant(integer(1)?, provenance)?;
            let last = self
                .builder
                .constant(integer(i64::from(extent))?, provenance)?;
            let low = self.builder.compare(
                solve::SolveCompareOperator::GreaterEqual,
                index,
                one,
                provenance,
            )?;
            let high = self.builder.compare(
                solve::SolveCompareOperator::LessEqual,
                index,
                last,
                provenance,
            )?;
            let inside =
                self.builder
                    .binary(solve::SolveBinaryOperator::And, low, high, provenance)?;
            all = Some(match all {
                None => inside,
                Some(all) => {
                    self.builder
                        .binary(solve::SolveBinaryOperator::And, all, inside, provenance)?
                }
            });
        }
        all.ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })
    }

    /// An assignment statement's value. After a failed error-level assertion
    /// the call is aborted (MLS 3.7 §8.3.7), but solver, formal-derivative and
    /// state-selection evaluation still read the value and its derivative, so
    /// it is computed as before wherever that is valid. Only where a checked
    /// subscript is out of range after a failed assertion does the statement
    /// take a neutral value, so that fault cannot preempt the assertion, which
    /// the call reports. A right-hand side that cannot fault, or that can fault
    /// some other way, is computed exactly as before.
    pub(super) fn gated_function_definition_value(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        gate: &mut AssertionGate<'program>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let rhs = definition.rhs();
        let checks = if gate.predicates.is_empty() {
            Vec::new()
        } else {
            guardable_index_checks(self.view, &mut self.totality, rhs).unwrap_or_default()
        };
        if checks.is_empty() {
            self.function_definition_value(definition)?;
            return Ok(());
        }
        if self.function_values.contains_key(&definition.id()) {
            return Ok(());
        }
        let provenance = definition.provenance().span();
        let gate = self.assertion_gate_register(gate, provenance)?;
        let in_bounds = self.subscripts_in_bounds(&checks, provenance)?;
        let gate =
            self.builder
                .binary(solve::SolveBinaryOperator::Or, gate, in_bounds, provenance)?;
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

/// The scalar subscripts whose range checks are the only way evaluating `rhs`
/// can fail, with the extent each one indexes. `None` when `rhs` can fail some
/// other way (checked Integer arithmetic or conversion, a call, a slice) or
/// inside a conditional arm, which this guard does not cover; such a
/// right-hand side is evaluated as before.
fn guardable_index_checks<'dae>(
    view: dae::DaeView<'dae>,
    totality: &mut FaultMemo<'dae>,
    rhs: dae::ExprId<'dae>,
) -> Option<Vec<(dae::ExprId<'dae>, u32)>> {
    let mut checks = Vec::new();
    let mut pending = vec![rhs];
    let mut seen = std::collections::HashSet::new();
    while let Some(expression) = pending.pop() {
        if !seen.insert(expression) || total_conditionals::is_total(view, totality, expression) {
            continue;
        }
        let node = view.expression(expression)?;
        // Checked Integer arithmetic and conversions fault some other way.
        let non_integer = node.value_type().scalar_type() != dae::ScalarType::Integer;
        match node.operation() {
            dae::ExpressionOperation::Index { base, subscripts } => {
                checks.extend(index_checks(view, totality, base, subscripts)?);
                pending.push(base);
            }
            dae::ExpressionOperation::Binary { lhs, rhs, .. } if non_integer => {
                pending.extend([lhs, rhs]);
            }
            dae::ExpressionOperation::Unary { operand, .. } if non_integer => {
                pending.push(operand);
            }
            dae::ExpressionOperation::Builtin { arguments, .. } if non_integer => {
                pending.extend(arguments.iter());
            }
            dae::ExpressionOperation::Field { base, .. } => pending.push(base),
            dae::ExpressionOperation::Array(operands)
            | dae::ExpressionOperation::Record(operands) => {
                pending.extend(operands.iter());
            }
            _ => return None,
        }
    }
    Some(checks)
}

/// The range checks of one access, one per indexed axis.
fn index_checks<'dae>(
    view: dae::DaeView<'dae>,
    totality: &mut FaultMemo<'dae>,
    base: dae::ExprId<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
) -> Option<Vec<(dae::ExprId<'dae>, u32)>> {
    let extents = view.expression(base)?.value_type().dimensions();
    if subscripts.len() > extents.len() {
        return None;
    }
    let mut checks = Vec::new();
    for (subscript, &extent) in subscripts.iter().zip(extents) {
        checks.extend(guardable_subscript(view, totality, subscript, extent)?);
    }
    Some(checks)
}

/// One subscript's range check against the extent `DaeFaults` proves bounds
/// with: a scalar Integer subscript that cannot fault itself, so hoisting it
/// adds no fault. A whole-axis subscript needs no check; `None` when the
/// subscript cannot be checked this way.
fn guardable_subscript<'dae>(
    view: dae::DaeView<'dae>,
    totality: &mut FaultMemo<'dae>,
    subscript: dae::SubscriptView<'dae>,
    extent: u32,
) -> Option<Option<(dae::ExprId<'dae>, u32)>> {
    match subscript {
        dae::SubscriptView::Whole { .. } => Some(None),
        dae::SubscriptView::Index { expression, .. } => {
            let node = view.expression(expression)?;
            (node.value_type().dimensions().is_empty()
                && node.value_type().scalar_type() == dae::ScalarType::Integer
                && total_conditionals::is_total(view, totality, expression))
            .then_some(Some((expression, extent)))
        }
        dae::SubscriptView::Slice { .. } => None,
    }
}
