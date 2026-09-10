//! Compact gather/scatter lowering for vector and strided slice subscripts.

use super::*;

enum SelectionAxis {
    Index(usize),
    Whole(usize),
    Range {
        binder: usize,
        start: i64,
        step: i64,
    },
    Vector {
        binder: usize,
        capture: usize,
    },
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn indexed_slice(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let base = self.expression(base)?.only_register(at)?;
        let result_types = lower_value_type_leaves(self.view, value_type, arithmetic_profile())?;
        let [result_type] = result_types.as_slice() else {
            return Err(solve::SolveProgramConstructionError::InvalidProjection { provenance: at });
        };
        let value = self.gather_slice(base, result_type, subscripts, at)?;
        Ok(LoweredValue::scalar(value_type, value))
    }

    pub(super) fn needs_indexed_slice(&self, subscripts: dae::SubscriptsView<'dae>) -> bool {
        subscripts.iter().any(|subscript| {
            let dae::SubscriptView::Slice { expression, .. } = subscript else {
                return false;
            };
            !self.view.expression(expression).is_some_and(|node| {
                matches!(node.operation(), dae::ExpressionOperation::Range(range)
                    if range.effective_step() == 1)
            })
        })
    }

    pub(super) fn gather_slice(
        &mut self,
        base: solve::ProgramRegister<'program>,
        result_type: &solve::SolveValueType,
        subscripts: dae::SubscriptsView<'dae>,
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let (indices, axes) = self.selection_inputs(subscripts, at)?;
        let mut captures = vec![base];
        captures.extend(indices);
        let zero = match result_type.element_type() {
            solve::SolveScalarType::Real { .. } => {
                solve::SolveValue::real(arithmetic_profile(), 0.0)
            }
            solve::SolveScalarType::Integer(_) => {
                solve::SolveValue::integer(arithmetic_profile(), 0).map_err(|_| {
                    solve::SolveProgramConstructionError::ProfileMismatch { provenance: at }
                })?
            }
            solve::SolveScalarType::Boolean => solve::SolveValue::boolean(false),
        };
        let zero = self.builder.constant(zero, at)?;
        let initial = self
            .builder
            .fill(zero, result_type.dimensions().to_vec(), at)?;
        let results = self.builder.fold(
            selection_domain(result_type.dimensions()),
            &[initial],
            &captures,
            at,
            move |builder, carried, captures, binders, outputs| {
                let base = builder.load(captures[0], at)?;
                let coordinates =
                    selection_coordinates(builder, &axes, &captures[1..], binders, at)?;
                let value = builder.project_element_dynamic(base, &coordinates, at)?;
                let result = builder.load(carried[0], at)?;
                let destination = binders
                    .iter()
                    .map(|binder| builder.load(*binder, at))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = builder.update_element(result, value, &destination, at)?;
                builder.store(outputs[0], result, at)
            },
        )?;
        results
            .first()
            .copied()
            .ok_or(solve::SolveProgramConstructionError::InvalidProjection { provenance: at })
    }

    pub(super) fn scatter_slice(
        &mut self,
        base: solve::ProgramRegister<'program>,
        value: solve::ProgramRegister<'program>,
        dimensions: &[u32],
        subscripts: dae::SubscriptsView<'dae>,
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let (indices, axes) = self.selection_inputs(subscripts, at)?;
        // Capture the complete RHS before any selected coordinate is updated.
        let mut captures = vec![value];
        captures.extend(indices);
        let values = self.builder.fold(
            selection_domain(dimensions),
            &[base],
            &captures,
            at,
            move |builder, carried, captures, binders, outputs| {
                let base = builder.load(carried[0], at)?;
                let values = builder.load(captures[0], at)?;
                let value_coordinates = binders
                    .iter()
                    .map(|binder| builder.load(*binder, at))
                    .collect::<Result<Vec<_>, _>>()?;
                let value = builder.project_element_dynamic(values, &value_coordinates, at)?;
                let coordinates =
                    selection_coordinates(builder, &axes, &captures[1..], binders, at)?;
                let updated = builder.update_element(base, value, &coordinates, at)?;
                builder.store(outputs[0], updated, at)
            },
        )?;
        values
            .first()
            .copied()
            .ok_or(solve::SolveProgramConstructionError::InvalidProjection { provenance: at })
    }

    fn selection_inputs(
        &mut self,
        subscripts: dae::SubscriptsView<'dae>,
        at: rumoca_core::Span,
    ) -> Result<
        (Vec<solve::ProgramRegister<'program>>, Vec<SelectionAxis>),
        solve::SolveProgramConstructionError,
    > {
        let mut captures = Vec::new();
        let mut axes = Vec::new();
        let mut binder = 0;
        for subscript in subscripts.iter() {
            match subscript {
                dae::SubscriptView::Index { expression, .. } => {
                    axes.push(SelectionAxis::Index(captures.len()));
                    captures.push(self.expression(expression)?.only_register(at)?);
                }
                dae::SubscriptView::Whole { .. } => {
                    axes.push(SelectionAxis::Whole(binder));
                    binder += 1;
                }
                dae::SubscriptView::Slice { expression, .. } => {
                    let axis = self.selection_slice_axis(expression, binder, &mut captures, at)?;
                    axes.push(axis);
                    binder += 1;
                }
            }
        }
        Ok((captures, axes))
    }

    fn selection_slice_axis(
        &mut self,
        expression: dae::ExprId<'dae>,
        binder: usize,
        captures: &mut Vec<solve::ProgramRegister<'program>>,
        at: rumoca_core::Span,
    ) -> Result<SelectionAxis, solve::SolveProgramConstructionError> {
        let node = self
            .view
            .expression(expression)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        if let dae::ExpressionOperation::Range(range) = node.operation() {
            return Ok(SelectionAxis::Range {
                binder,
                start: range.start().value(),
                step: range.effective_step(),
            });
        }
        let capture = captures.len();
        captures.push(self.expression(expression)?.only_register(at)?);
        Ok(SelectionAxis::Vector { binder, capture })
    }
}

fn selection_domain(dimensions: &[u32]) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: dimensions
            .iter()
            .enumerate()
            .map(|(id, extent)| rumoca_core::StructuredIndexBinder {
                id,
                display_name: format!("slice_axis_{id}"),
                lower: 1,
                upper: i64::from(*extent),
                step: 1,
            })
            .collect(),
    }
}

fn selection_coordinates<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    axes: &[SelectionAxis],
    captures: &[solve::ProgramSlot<'program>],
    binders: &[solve::ProgramSlot<'program>],
    at: rumoca_core::Span,
) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
    axes.iter()
        .map(|axis| match *axis {
            SelectionAxis::Index(capture) => builder.load(captures[capture], at),
            SelectionAxis::Whole(binder) => builder.load(binders[binder], at),
            SelectionAxis::Vector { binder, capture } => {
                let vector = builder.load(captures[capture], at)?;
                let index = builder.load(binders[binder], at)?;
                builder.project_element_dynamic(vector, &[index], at)
            }
            SelectionAxis::Range {
                binder,
                start,
                step,
            } => {
                let index = builder.load(binders[binder], at)?;
                let literal = |value| {
                    solve::SolveValue::integer(arithmetic_profile(), value).map_err(|_| {
                        solve::SolveProgramConstructionError::ProfileMismatch { provenance: at }
                    })
                };
                let one = builder.constant(literal(1)?, at)?;
                let start = builder.constant(literal(start)?, at)?;
                let step = builder.constant(literal(step)?, at)?;
                let zero_based =
                    builder.binary(solve::SolveBinaryOperator::Subtract, index, one, at)?;
                let offset =
                    builder.binary(solve::SolveBinaryOperator::Multiply, zero_based, step, at)?;
                builder.binary(solve::SolveBinaryOperator::Add, start, offset, at)
            }
        })
        .collect()
}
