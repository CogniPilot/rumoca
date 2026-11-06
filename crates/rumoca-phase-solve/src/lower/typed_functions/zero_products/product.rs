//! Compact source products, selecting proof before demanding numeric terms.

use super::*;
use rumoca_eval_dae::ZeroCoefficientPattern;

#[derive(Clone, Copy)]
enum Direct {
    Elementwise,
    ScaleLeft,
    ScaleRight,
}

#[derive(Clone, Copy)]
enum Contraction {
    Dot,
    MatrixVector,
    VectorMatrix,
    Matrices,
}

#[derive(Clone, Copy)]
enum Form {
    Direct(Direct),
    Contract { kind: Contraction, extent: u32 },
}

impl Direct {
    fn coordinates<'program>(
        &self,
        output: &[solve::ProgramRegister<'program>],
    ) -> (
        Vec<solve::ProgramRegister<'program>>,
        Vec<solve::ProgramRegister<'program>>,
    ) {
        match self {
            Self::Elementwise => (output.to_vec(), output.to_vec()),
            Self::ScaleLeft => (vec![], output.to_vec()),
            Self::ScaleRight => (output.to_vec(), vec![]),
        }
    }
}

impl Contraction {
    fn coordinates<'program>(
        &self,
        output: &[solve::ProgramRegister<'program>],
        k: solve::ProgramRegister<'program>,
        at: rumoca_core::Span,
    ) -> Result<
        (
            Vec<solve::ProgramRegister<'program>>,
            Vec<solve::ProgramRegister<'program>>,
        ),
        solve::SolveProgramConstructionError,
    > {
        match (self, output) {
            (Self::Dot, []) => Ok((vec![k], vec![k])),
            (Self::MatrixVector, [row]) => Ok((vec![*row, k], vec![k])),
            (Self::VectorMatrix, [column]) => Ok((vec![k], vec![k, *column])),
            (Self::Matrices, [row, column]) => Ok((vec![*row, k], vec![k, *column])),
            _ => Err(solve::SolveProgramConstructionError::InvalidTensorAlgebra { provenance: at }),
        }
    }
}

#[derive(Clone)]
struct Product<'dae> {
    value_type: dae::ValueTypeId<'dae>,
    operator: dae::BinaryOperator,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
    form: Form,
    dimensions: Vec<u32>,
    scalar: solve::SolveScalarType,
    at: rumoca_core::Span,
}

impl<'dae> Product<'dae> {
    fn construct(
        view: dae::DaeView<'dae>,
        value_type: dae::ValueTypeId<'dae>,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        at: rumoca_core::Span,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let lhs_source = SourceExpression::construct(view, lhs)?;
        let rhs_source = SourceExpression::construct(view, rhs)?;
        let left = lhs_source.node.value_type().dimensions();
        let right = rhs_source.node.value_type().dimensions();
        let form = match (left, right) {
            ([], []) => Form::Direct(Direct::Elementwise),
            ([], _) => Form::Direct(Direct::ScaleLeft),
            (_, []) => Form::Direct(Direct::ScaleRight),
            _ if operator == dae::BinaryOperator::ElementwiseMultiply => {
                Form::Direct(Direct::Elementwise)
            }
            ([n], [m]) if n == m => Form::Contract {
                kind: Contraction::Dot,
                extent: *n,
            },
            ([_, n], [m]) if n == m => Form::Contract {
                kind: Contraction::MatrixVector,
                extent: *n,
            },
            ([n], [m, _]) if n == m => Form::Contract {
                kind: Contraction::VectorMatrix,
                extent: *n,
            },
            ([_, n], [m, _]) if n == m => Form::Contract {
                kind: Contraction::Matrices,
                extent: *n,
            },
            _ => {
                return Err(solve::SolveProgramConstructionError::InvalidTensorAlgebra {
                    provenance: at,
                });
            }
        };
        let ty = lower_primitive_type(view, value_type, arithmetic_profile())?;
        Ok(Self {
            value_type,
            operator,
            lhs,
            rhs,
            form,
            dimensions: ty.dimensions().to_vec(),
            scalar: ty.element_type(),
            at,
        })
    }
}

#[derive(Clone)]
struct Region<'dae> {
    product: Product<'dae>,
    environment: EnvironmentLayout<'dae>,
    context: RegionContext<'dae>,
    environment_count: usize,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn partial_zero_product(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        patterns: [Option<ZeroCoefficientPattern>; 2],
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        self.retain_opaque_product_operands(lhs, rhs, &patterns)?;
        if self.is_zero_size(value_type)? {
            return Ok(LoweredValue::empty(value_type));
        }
        let product = Product::construct(self.view, value_type, operator, lhs, rhs, at)?;
        let masks = issue_patterns(self.builder, &patterns, at)?;
        if product.dimensions.is_empty() {
            let value = self.issue_product_point(&product, masks, &[])?;
            return Ok(LoweredValue::scalar(value_type, value));
        }
        let (mut captures, environment) = self.capture_environment_for_message([lhs, rhs])?;
        let environment_count = captures.len();
        captures.extend(masks);
        let region = Region {
            product: product.clone(),
            environment,
            context: self.region_context(),
            environment_count,
        };
        let result = self.builder.map(
            domain(&product.dimensions),
            &captures,
            solve::SolveValueType::scalar(product.scalar),
            at,
            move |builder, inputs, binders, output| {
                let coordinates = loads(builder, binders, at)?;
                let masks = loads(builder, &inputs[environment_count..], at)?;
                let mut lowerer = load_region_lowerer(
                    builder,
                    &inputs[..environment_count],
                    &region.environment,
                    &region.context,
                    at,
                )?;
                let value = lowerer.issue_product_point(
                    &region.product,
                    [masks[0], masks[1]],
                    &coordinates,
                )?;
                lowerer.builder.store(output, value, at)
            },
        )?;
        Ok(LoweredValue::scalar(value_type, result))
    }

    /// A nested source matrix product selected by its original output tuple.
    pub(super) fn product_point(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        coordinates: &[solve::ProgramRegister<'program>],
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let product = Product::construct(self.view, value_type, operator, lhs, rhs, at)?;
        let patterns = [
            self.zero_coefficients.omission_pattern(self.view, rhs, lhs),
            self.zero_coefficients.omission_pattern(self.view, lhs, rhs),
        ];
        self.retain_opaque_product_operands(lhs, rhs, &patterns)?;
        let masks = issue_patterns(self.builder, &patterns, at)?;
        self.issue_product_point(&product, masks, coordinates)
    }

    /// Absence of a proof means that operand carries retained validation or
    /// invocation ownership. Evaluate it once in original lhs/rhs order. Only
    /// the incidence-certified total operand is selected inside product regions.
    fn retain_opaque_product_operands(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        patterns: &[Option<ZeroCoefficientPattern>; 2],
    ) -> Result<(), solve::SolveProgramConstructionError> {
        for (operand, proof) in [(lhs, &patterns[1]), (rhs, &patterns[0])] {
            if proof.is_none() {
                self.expression(operand)?;
            }
        }
        Ok(())
    }

    fn issue_product_point(
        &mut self,
        product: &Product<'dae>,
        masks: [solve::ProgramRegister<'program>; 2],
        coordinates: &[solve::ProgramRegister<'program>],
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let at = product.at;
        let zero = zero(self.builder, product.scalar, at)?;
        let absent = self
            .builder
            .constant(solve::SolveValue::boolean(false), at)?;
        let (kind, inner) = match product.form {
            Form::Direct(kind) => {
                let pair = kind.coordinates(coordinates);
                return self
                    .product_term(product, masks, &pair, (zero, absent))
                    .map(|values| values[0]);
            }
            Form::Contract { kind, extent } => (kind, extent),
        };
        let (mut captures, environment) =
            self.capture_environment_for_message([product.lhs, product.rhs])?;
        let environment_count = captures.len();
        captures.extend(masks);
        captures.extend_from_slice(coordinates);
        let region = Region {
            product: product.clone(),
            environment,
            context: self.region_context(),
            environment_count,
        };
        let result = self.builder.fold(
            domain(&[inner]),
            &[zero, absent],
            &captures,
            at,
            move |builder, carried, inputs, binders, outputs| {
                let state = loads(builder, carried, at)?;
                let extra = loads(builder, &inputs[environment_count..], at)?;
                let k = builder.load(binders[0], at)?;
                let pair = kind.coordinates(&extra[2..], k, at)?;
                let mut lowerer = load_region_lowerer(
                    builder,
                    &inputs[..environment_count],
                    &region.environment,
                    &region.context,
                    at,
                )?;
                let values = lowerer.product_term(
                    &region.product,
                    [extra[0], extra[1]],
                    &pair,
                    (state[0], state[1]),
                )?;
                for (&output, value) in outputs.iter().zip(values) {
                    lowerer.builder.store(output, value, at)?;
                }
                Ok(())
            },
        )?;
        Ok(result[0])
    }

    fn product_term(
        &mut self,
        product: &Product<'dae>,
        masks: [solve::ProgramRegister<'program>; 2],
        coordinates: &(
            Vec<solve::ProgramRegister<'program>>,
            Vec<solve::ProgramRegister<'program>>,
        ),
        state: (
            solve::ProgramRegister<'program>,
            solve::ProgramRegister<'program>,
        ),
    ) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
        let at = product.at;
        let left = mask_at(self.builder, masks[0], &coordinates.0, at)?;
        let right = mask_at(self.builder, masks[1], &coordinates.1, at)?;
        let omitted = self
            .builder
            .binary(solve::SolveBinaryOperator::Or, left, right, at)?;
        let (mut captures, environment) =
            self.capture_environment_for_message([product.lhs, product.rhs])?;
        let count = captures.len();
        captures.extend([state.0, state.1]);
        captures.extend_from_slice(&coordinates.0);
        captures.extend_from_slice(&coordinates.1);
        let lhs_rank = coordinates.0.len();
        let region = Region {
            product: product.clone(),
            environment,
            context: self.region_context(),
            environment_count: count,
        };
        self.builder.conditional(
            omitted,
            &captures,
            vec![
                solve::SolveValueType::scalar(product.scalar),
                solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            ],
            at,
            move |builder, inputs, outputs| {
                let state = loads(builder, &inputs[count..count + 2], at)?;
                for (&output, value) in outputs.iter().zip(state) {
                    builder.store(output, value, at)?;
                }
                Ok(())
            },
            move |builder, inputs, outputs| kept_term(builder, inputs, outputs, &region, lhs_rank),
        )
    }
}

fn kept_term<'program, 'dae>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    inputs: &[solve::ProgramSlot<'program>],
    outputs: &[solve::ProgramSlot<'program>],
    region: &Region<'dae>,
    lhs_rank: usize,
) -> Result<(), solve::SolveProgramConstructionError> {
    let product = &region.product;
    let at = product.at;
    let count = region.environment_count;
    let extra = loads(builder, &inputs[count..], at)?;
    let mut lowerer = load_region_lowerer(
        builder,
        &inputs[..count],
        &region.environment,
        &region.context,
        at,
    )?;
    let lhs = lowerer.selected_expression(product.lhs, &extra[2..2 + lhs_rank])?;
    let rhs = lowerer.selected_expression(product.rhs, &extra[2 + lhs_rank..])?;
    let term = lowerer
        .binary_registers(product.value_type, product.operator, lhs, rhs, at)?
        .only_register(at)?;
    let value = if matches!(product.form, Form::Contract { .. }) {
        append_term(
            lowerer.builder,
            extra[0],
            extra[1],
            term,
            product.scalar,
            at,
        )?
    } else {
        term
    };
    lowerer.builder.store(outputs[0], value, at)?;
    let present = lowerer
        .builder
        .constant(solve::SolveValue::boolean(true), at)?;
    lowerer.builder.store(outputs[1], present, at)
}

fn append_term<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    sum: solve::ProgramRegister<'program>,
    present: solve::ProgramRegister<'program>,
    term: solve::ProgramRegister<'program>,
    scalar: solve::SolveScalarType,
    at: rumoca_core::Span,
) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
    let values = builder.conditional(
        present,
        &[sum, term],
        vec![solve::SolveValueType::scalar(scalar)],
        at,
        |builder, inputs, outputs| {
            let values = loads(builder, inputs, at)?;
            let sum = builder.binary(solve::SolveBinaryOperator::Add, values[0], values[1], at)?;
            builder.store(outputs[0], sum, at)
        },
        |builder, inputs, outputs| {
            let value = builder.load(inputs[1], at)?;
            builder.store(outputs[0], value, at)
        },
    )?;
    Ok(values[0])
}

fn issue_patterns<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    patterns: &[Option<ZeroCoefficientPattern>; 2],
    at: rumoca_core::Span,
) -> Result<[solve::ProgramRegister<'program>; 2], solve::SolveProgramConstructionError> {
    let mut memo = HashMap::new();
    let mut values = Vec::new();
    for pattern in patterns {
        values.push(match pattern {
            Some(pattern) => proof::issue(builder, pattern, &mut memo, at)?,
            None => builder.constant(solve::SolveValue::boolean(false), at)?,
        });
    }
    Ok([values[0], values[1]])
}

fn mask_at<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    mask: solve::ProgramRegister<'program>,
    coordinates: &[solve::ProgramRegister<'program>],
    at: rumoca_core::Span,
) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
    if builder.value_type_of(mask, at)?.dimensions().is_empty() {
        Ok(mask)
    } else {
        builder.project_element_dynamic(mask, coordinates, at)
    }
}

fn loads<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    slots: &[solve::ProgramSlot<'program>],
    at: rumoca_core::Span,
) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
    slots.iter().map(|&slot| builder.load(slot, at)).collect()
}

fn zero<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    scalar: solve::SolveScalarType,
    at: rumoca_core::Span,
) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
    let value = if matches!(scalar, solve::SolveScalarType::Integer(_)) {
        solve::SolveValue::integer(arithmetic_profile(), 0)
            .map_err(|_| solve::SolveProgramConstructionError::ProfileMismatch { provenance: at })?
    } else {
        solve::SolveValue::real(arithmetic_profile(), 0.0)
    };
    builder.constant(value, at)
}

fn domain(dimensions: &[u32]) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: dimensions
            .iter()
            .enumerate()
            .map(|(id, &extent)| rumoca_core::StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id,
                display_name: format!("product_axis_{id}"),
                lower: 1,
                upper: i64::from(extent),
                step: 1,
            })
            .collect(),
    }
}
