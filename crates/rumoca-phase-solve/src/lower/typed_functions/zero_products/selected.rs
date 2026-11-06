//! Source-coordinate selection before arithmetic demand. Operator semantics
//! remain in the shared typed lowerer's register issuer.

use super::*;

#[derive(Clone)]
struct Arm<'dae> {
    expression: dae::ExprId<'dae>,
    origin: u32,
    rank: usize,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn selected_expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        coordinates: &[solve::ProgramRegister<'program>],
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let source = SourceExpression::construct(self.view, expression)?;
        let node = source.node;
        let at = node.provenance().span();
        if let Some(value) = self.cache.get(&expression).cloned() {
            return self.project_selected(value.only_register(at)?, coordinates, at);
        }
        let key = (expression, coordinates.to_vec());
        if let Some(&value) = self.selected_values.get(&key) {
            return Ok(value);
        }
        if let Some(value) = self.selected_call(expression, coordinates, at)? {
            self.selected_values.insert(key, value);
            return Ok(value);
        }
        if node.value_type().is_scalar() {
            return self.expression(expression)?.only_register(at);
        }
        let value = self.selected_operation(source, coordinates)?;
        self.selected_values.insert(key, value);
        Ok(value)
    }

    pub(super) fn selected_operation(
        &mut self,
        source: SourceExpression<'dae>,
        coordinates: &[solve::ProgramRegister<'program>],
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let expression = source.id;
        let node = source.node;
        let at = node.provenance().span();
        match node.operation() {
            dae::ExpressionOperation::Unary { operator, operand } => {
                let value = self.selected_expression(operand, coordinates)?;
                self.unary_register(operator, value, at)
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.selected_binary(node.value_type_id(), operator, lhs, rhs, coordinates, at)
            }
            dae::ExpressionOperation::Array(elements) => {
                let arms = elements
                    .iter()
                    .enumerate()
                    .map(|(index, expression)| Arm {
                        expression,
                        origin: index as u32,
                        rank: coordinates.len() - 1,
                    })
                    .collect::<Vec<_>>();
                self.selected_arms(node.value_type_id(), &arms, coordinates, None, at)
            }
            dae::ExpressionOperation::Builtin {
                builtin: builtin @ (dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2),
                arguments,
            } => {
                let axis = usize::from(builtin == dae::PureBuiltin::PromotedCat2);
                let mut origin = 0;
                let arms = arguments
                    .iter()
                    .map(|expression| {
                        let source = SourceExpression::construct(self.view, expression)?;
                        let dimensions = source.node.value_type().dimensions();
                        let extent = dimensions.get(axis).copied().unwrap_or(1);
                        let arm = Arm {
                            expression,
                            origin,
                            rank: dimensions.len(),
                        };
                        origin += extent;
                        Ok(arm)
                    })
                    .collect::<Result<Vec<_>, solve::SolveProgramConstructionError>>()?;
                self.selected_arms(node.value_type_id(), &arms, coordinates, Some(axis), at)
            }
            dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::Transpose,
                arguments,
            } if coordinates.len() == 2 => {
                let base = arguments
                    .get(0)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
                self.selected_expression(base, &[coordinates[1], coordinates[0]])
            }
            dae::ExpressionOperation::Builtin {
                builtin: builtin @ (dae::PureBuiltin::Smooth | dae::PureBuiltin::NoEvent),
                arguments,
            } => {
                let base = arguments
                    .get(usize::from(builtin == dae::PureBuiltin::Smooth))
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
                self.selected_expression(base, coordinates)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.selected_index(node.value_type_id(), base, subscripts, coordinates, at)
            }
            _ => {
                // An opaque owner remains one ordinary invocation, including
                // its complete validation/observation contract.
                let value = self.expression(expression)?.only_register(at)?;
                self.project_selected(value, coordinates, at)
            }
        }
    }

    fn project_selected(
        &mut self,
        value: solve::ProgramRegister<'program>,
        coordinates: &[solve::ProgramRegister<'program>],
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        if coordinates.is_empty() {
            Ok(value)
        } else {
            self.builder.project_element_dynamic(value, coordinates, at)
        }
    }

    fn selected_binary(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        coordinates: &[solve::ProgramRegister<'program>],
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let lhs_source = SourceExpression::construct(self.view, lhs)?;
        let rhs_source = SourceExpression::construct(self.view, rhs)?;
        let lhs_shape = lhs_source.node.value_type().dimensions();
        let rhs_shape = rhs_source.node.value_type().dimensions();
        if operator == dae::BinaryOperator::Multiply
            && !lhs_shape.is_empty()
            && !rhs_shape.is_empty()
        {
            return self.product_point(value_type, operator, lhs, rhs, coordinates, at);
        }
        let left = self.selected_expression(
            lhs,
            if lhs_shape.is_empty() {
                &[]
            } else {
                coordinates
            },
        )?;
        let right = self.selected_expression(
            rhs,
            if rhs_shape.is_empty() {
                &[]
            } else {
                coordinates
            },
        )?;
        self.binary_operands(value_type, operator, (lhs, rhs), (left, right), at)?
            .only_register(at)
    }

    fn selected_index(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        coordinates: &[solve::ProgramRegister<'program>],
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        if rumoca_eval_dae::literal_tensor_selection(self.view, base, subscripts).is_none() {
            let value = self
                .index(value_type, base, subscripts, at)?
                .only_register(at)?;
            return self.project_selected(value, coordinates, at);
        }
        let base_source = SourceExpression::construct(self.view, base)?;
        let rank = base_source.node.value_type().dimensions().len();
        let mut result_axis = 0;
        let mut selected = Vec::with_capacity(rank);
        for axis in 0..rank {
            match subscripts.get(axis) {
                Some(dae::SubscriptView::Index { expression, .. }) => {
                    selected.push(self.expression(expression)?.only_register(at)?)
                }
                Some(dae::SubscriptView::Whole { .. }) | None => {
                    selected.push(coordinates[result_axis]);
                    result_axis += 1;
                }
                Some(dae::SubscriptView::Slice { .. }) => {
                    let value = self
                        .index(value_type, base, subscripts, at)?
                        .only_register(at)?;
                    return self.project_selected(value, coordinates, at);
                }
            }
        }
        self.selected_expression(base, &selected)
    }

    fn selected_arms(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        arms: &[Arm<'dae>],
        coordinates: &[solve::ProgramRegister<'program>],
        axis: Option<usize>,
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        if arms.is_empty() {
            return Err(solve::SolveProgramConstructionError::InvalidTensorAlgebra {
                provenance: at,
            });
        }
        if let [arm] = arms {
            let mut coordinates = if axis.is_none() {
                coordinates[1..].to_vec()
            } else {
                coordinates.to_vec()
            };
            if let Some(axis) = axis
                && axis < arm.rank
                && arm.origin != 0
            {
                let offset = proof::integer(self.builder, i64::from(arm.origin), at)?;
                coordinates[axis] = self.builder.binary(
                    solve::SolveBinaryOperator::Subtract,
                    coordinates[axis],
                    offset,
                    at,
                )?;
            }
            coordinates.truncate(arm.rank);
            let value = self.selected_expression(arm.expression, &coordinates)?;
            let scalar =
                lower_primitive_type(self.view, value_type, arithmetic_profile())?.element_type();
            return self
                .builder
                .coerce_to(value, &solve::SolveValueType::scalar(scalar), at);
        }
        let split = arms.len() / 2;
        let boundary = arms[split].origin;
        let bound = proof::integer(self.builder, i64::from(boundary), at)?;
        let condition = self.builder.compare(
            solve::SolveCompareOperator::LessEqual,
            coordinates[axis.unwrap_or(0)],
            bound,
            at,
        )?;
        let (mut captures, environment) =
            self.capture_environment_for_message(arms.iter().map(|arm| arm.expression))?;
        let context = self.region_context();
        let count = captures.len();
        captures.extend_from_slice(coordinates);
        let output_type = solve::SolveValueType::scalar(
            lower_primitive_type(self.view, value_type, arithmetic_profile())?.element_type(),
        );
        let left = SelectionRegion {
            environment: environment.clone(),
            context: context.clone(),
            count,
            value_type,
            arms: arms[..split].to_vec(),
            axis,
            at,
        };
        let right = SelectionRegion {
            environment,
            context,
            count,
            value_type,
            arms: arms[split..].to_vec(),
            axis,
            at,
        };
        let result = self.builder.conditional(
            condition,
            &captures,
            vec![output_type],
            at,
            move |builder, inputs, outputs| left.lower(builder, inputs, outputs),
            move |builder, inputs, outputs| right.lower(builder, inputs, outputs),
        )?;
        Ok(result[0])
    }
}

// A branch reads captures through the same region owner as every other typed
// expression; only the selected coordinate list is additional context.
struct SelectionRegion<'dae> {
    environment: EnvironmentLayout<'dae>,
    context: RegionContext<'dae>,
    count: usize,
    value_type: dae::ValueTypeId<'dae>,
    arms: Vec<Arm<'dae>>,
    axis: Option<usize>,
    at: rumoca_core::Span,
}

impl SelectionRegion<'_> {
    fn lower<'program>(
        &self,
        builder: &mut solve::TypedProgramBuilder<'program>,
        inputs: &[solve::ProgramSlot<'program>],
        outputs: &[solve::ProgramSlot<'program>],
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let coordinates = inputs[self.count..]
            .iter()
            .map(|&slot| builder.load(slot, self.at))
            .collect::<Result<Vec<_>, _>>()?;
        let mut lowerer = load_region_lowerer(
            builder,
            &inputs[..self.count],
            &self.environment,
            &self.context,
            self.at,
        )?;
        let value = lowerer.selected_arms(
            self.value_type,
            &self.arms,
            &coordinates,
            self.axis,
            self.at,
        )?;
        lowerer.builder.store(outputs[0], value, self.at)
    }
}
