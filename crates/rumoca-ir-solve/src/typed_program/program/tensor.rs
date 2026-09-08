//! Checked compact tensor-algebra construction.

use super::*;
use crate::typed_program::reduction::SolveMatrixMultiplyPlanIssuanceError;

impl<'program> TypedProgramBuilder<'program> {
    pub fn broadcast_binary(
        &mut self,
        operator: SolveBinaryOperator,
        aggregate: ProgramRegister<'program>,
        scalar: ProgramRegister<'program>,
        scalar_on_lhs: bool,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?.clone();
        let scalar_type = self.register_type(scalar, provenance)?;
        if aggregate_type.dimensions().is_empty()
            || !scalar_type.dimensions().is_empty()
            || aggregate_type.element_type() != scalar_type.element_type()
            || !binary_operator_accepts(operator, aggregate_type.element_type())
        {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        if integer_binary_result_range_is_unproved(operator, aggregate_type.element_type()) {
            return Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance });
        }
        let destination = self.issue_register(aggregate_type, provenance)?;
        self.push(
            SolveOperation::BroadcastBinary {
                destination: destination.id,
                operator,
                aggregate: aggregate.id,
                scalar: scalar.id,
                scalar_on_lhs,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn project_view(
        &mut self,
        aggregate: ProgramRegister<'program>,
        axes: &[ProgramTensorViewAxis<'program>],
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?.clone();
        let dimensions = self.tensor_view_dimensions(&aggregate_type, axes, provenance)?;
        let result_type = SolveValueType::tensor(aggregate_type.element_type(), dimensions)
            .map_err(|_| SolveProgramConstructionError::InvalidProjection { provenance })?;
        let destination = self.issue_register(result_type, provenance)?;
        self.push(
            SolveOperation::ProjectView {
                destination: destination.id,
                aggregate: aggregate.id,
                axes: encode_view_axes(axes),
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn update_view(
        &mut self,
        aggregate: ProgramRegister<'program>,
        value: ProgramRegister<'program>,
        axes: &[ProgramTensorViewAxis<'program>],
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?.clone();
        let value_type = self.register_type(value, provenance)?;
        let dimensions = self.tensor_view_dimensions(&aggregate_type, axes, provenance)?;
        if value_type.element_type() != aggregate_type.element_type()
            || value_type.dimensions() != dimensions
        {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let destination = self.issue_register(aggregate_type, provenance)?;
        self.push(
            SolveOperation::UpdateView {
                destination: destination.id,
                aggregate: aggregate.id,
                value: value.id,
                axes: encode_view_axes(axes),
            },
            provenance,
        );
        Ok(destination)
    }

    fn tensor_view_dimensions(
        &self,
        aggregate_type: &SolveValueType,
        axes: &[ProgramTensorViewAxis<'program>],
        provenance: Span,
    ) -> Result<Vec<u32>, SolveProgramConstructionError> {
        if axes.len() != aggregate_type.dimensions().len() {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let mut dimensions = Vec::new();
        for (axis, base_extent) in axes.iter().zip(aggregate_type.dimensions()) {
            match *axis {
                ProgramTensorViewAxis::Index(index) => {
                    self.require_scalar_view_index(index, provenance)?;
                }
                ProgramTensorViewAxis::Span { origin, extent }
                    if extent > 0
                        && origin
                            .checked_add(extent)
                            .is_some_and(|end| end <= *base_extent) =>
                {
                    dimensions.push(extent);
                }
                ProgramTensorViewAxis::Span { .. } => {
                    return Err(SolveProgramConstructionError::InvalidProjection { provenance });
                }
            }
        }
        if dimensions.is_empty() {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        Ok(dimensions)
    }

    /// Requires that a view index axis addresses one scalar integer register.
    fn require_scalar_view_index(
        &self,
        index: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let index_type = self.register_type(index, provenance)?;
        if !index_type.dimensions().is_empty()
            || !matches!(index_type.element_type(), SolveScalarType::Integer(_))
        {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        Ok(())
    }

    pub fn diagonal(
        &mut self,
        operand: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let operand_type = self.register_type(operand, provenance)?;
        let [extent] = operand_type.dimensions() else {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        };
        if !operand_type.element_type().is_numeric() {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        if *extent >= 2
            && matches!(
                operand_type.element_type(),
                SolveScalarType::Integer(domain) if !domain.contains(0)
            )
        {
            return Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance });
        }
        let result_type =
            SolveValueType::tensor(operand_type.element_type(), vec![*extent, *extent])
                .map_err(|_| SolveProgramConstructionError::InvalidTensorAlgebra { provenance })?;
        let destination = self.issue_register(result_type, provenance)?;
        self.push(
            SolveOperation::Diagonal {
                destination: destination.id,
                operand: operand.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn identity(
        &mut self,
        element_type: SolveScalarType,
        extent: u32,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        if !element_type.belongs_to(self.arithmetic) || element_type == SolveScalarType::Boolean {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        if let SolveScalarType::Integer(domain) = element_type {
            let result_range_is_unproved = match extent {
                0 => false,
                1 => !domain.contains(1),
                _ => !domain.contains(0) || !domain.contains(1),
            };
            if result_range_is_unproved {
                return Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance });
            }
        }
        let result_type = SolveValueType::tensor(element_type, vec![extent, extent])
            .map_err(|_| SolveProgramConstructionError::InvalidTensorAlgebra { provenance })?;
        let destination = self.issue_register(result_type, provenance)?;
        self.push(
            SolveOperation::Identity {
                destination: destination.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn concatenate(
        &mut self,
        axis: u32,
        operands: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let first = operands
            .first()
            .ok_or(SolveProgramConstructionError::InvalidTensorAlgebra { provenance })?;
        let first_type = self.register_type(*first, provenance)?.clone();
        let rank = operands.iter().try_fold(2usize, |rank, operand| {
            self.register_type(*operand, provenance)
                .map(|value_type| rank.max(value_type.dimensions().len()))
        })?;
        let first_dimensions = promoted_concatenate_dimensions(&first_type, rank);
        let axis = axis as usize;
        if axis >= first_dimensions.len() {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        let mut dimensions = first_dimensions;
        let mut axis_extent = 0u32;
        for operand in operands {
            let value_type = self.register_type(*operand, provenance)?;
            let operand_dimensions = promoted_concatenate_dimensions(value_type, rank);
            if value_type.element_type() != first_type.element_type()
                || operand_dimensions.len() != dimensions.len()
                || operand_dimensions
                    .iter()
                    .enumerate()
                    .any(|(ordinal, extent)| ordinal != axis && *extent != dimensions[ordinal])
            {
                return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
            }
            axis_extent = axis_extent
                .checked_add(operand_dimensions[axis])
                .ok_or(SolveProgramConstructionError::InvalidTensorAlgebra { provenance })?;
        }
        dimensions[axis] = axis_extent;
        let result_type = SolveValueType::tensor(first_type.element_type(), dimensions)
            .map_err(|_| SolveProgramConstructionError::InvalidTensorAlgebra { provenance })?;
        let destination = self.issue_register(result_type, provenance)?;
        self.push(
            SolveOperation::Concatenate {
                destination: destination.id,
                axis: axis as u32,
                operands: operands
                    .iter()
                    .map(|operand| operand.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn scale(
        &mut self,
        aggregate: ProgramRegister<'program>,
        scalar: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?.clone();
        let scalar_type = self.register_type(scalar, provenance)?;
        if aggregate_type.dimensions().is_empty()
            || !scalar_type.dimensions().is_empty()
            || aggregate_type.element_type() != scalar_type.element_type()
            || !aggregate_type.element_type().is_numeric()
        {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        if integer_binary_result_range_is_unproved(
            SolveBinaryOperator::Multiply,
            aggregate_type.element_type(),
        ) {
            return Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance });
        }
        let destination = self.issue_register(aggregate_type, provenance)?;
        self.push(
            SolveOperation::Scale {
                destination: destination.id,
                aggregate: aggregate.id,
                scalar: scalar.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn transpose(
        &mut self,
        operand: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let operand_type = self.register_type(operand, provenance)?.clone();
        if operand_type.dimensions().len() < 2 {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        let mut dimensions = operand_type.dimensions().to_vec();
        dimensions.swap(0, 1);
        let result_type = SolveValueType::tensor(operand_type.element_type(), dimensions)
            .map_err(|_| SolveProgramConstructionError::InvalidTensorAlgebra { provenance })?;
        let destination = self.issue_register(result_type, provenance)?;
        self.push(
            SolveOperation::Transpose {
                destination: destination.id,
                operand: operand.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn matrix_multiply(
        &mut self,
        lhs: ProgramRegister<'program>,
        rhs: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let lhs_type = self.register_type(lhs, provenance)?.clone();
        let rhs_type = self.register_type(rhs, provenance)?;
        let (plan, result_type) =
            SolveMatrixMultiplyPlan::issued(self.arithmetic, &lhs_type, rhs_type).map_err(
                |error| match error {
                    SolveMatrixMultiplyPlanIssuanceError::InvalidAlgebra => {
                        SolveProgramConstructionError::InvalidTensorAlgebra { provenance }
                    }
                    SolveMatrixMultiplyPlanIssuanceError::UnsupportedArithmetic => {
                        SolveProgramConstructionError::MissingReductionContract { provenance }
                    }
                    SolveMatrixMultiplyPlanIssuanceError::EmptyFirstProductDomain => {
                        SolveProgramConstructionError::EmptyFirstProductDomain { provenance }
                    }
                },
            )?;
        let destination = self.issue_register(result_type, provenance)?;
        self.push(
            SolveOperation::MatrixMultiply {
                destination: destination.id,
                lhs: lhs.id,
                rhs: rhs.id,
                plan,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn cross(
        &mut self,
        lhs: ProgramRegister<'program>,
        rhs: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        self.register_type(lhs, provenance)?;
        self.register_type(rhs, provenance)?;
        Err(SolveProgramConstructionError::MissingReductionContract { provenance })
    }

    pub fn reduce(
        &mut self,
        operator: SolveReductionOperator,
        operand: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let operand_type = self.register_type(operand, provenance)?;
        if operator != SolveReductionOperator::All {
            return Err(SolveProgramConstructionError::MissingReductionContract { provenance });
        }
        if operand_type.dimensions().is_empty()
            || operand_type.element_type() != SolveScalarType::Boolean
        {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        let destination =
            self.issue_register(SolveValueType::scalar(SolveScalarType::Boolean), provenance)?;
        self.push(
            SolveOperation::Reduce {
                destination: destination.id,
                operator,
                operand: operand.id,
            },
            provenance,
        );
        Ok(destination)
    }
}

fn encode_view_axes(axes: &[ProgramTensorViewAxis<'_>]) -> Box<[SolveTensorViewAxis]> {
    axes.iter()
        .map(|axis| match *axis {
            ProgramTensorViewAxis::Index(index) => SolveTensorViewAxis::Index(index.id),
            ProgramTensorViewAxis::Span { origin, extent } => {
                SolveTensorViewAxis::Span { origin, extent }
            }
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

/// Promote `value_type` to `rank` axes the way MLS 10.4.2.1 `promote` does, by
/// appending trailing unit extents.
///
/// Single owner of the concatenate promotion rule (SPEC_0041 §1). Backends and
/// evaluators that need the promoted shape of a `Concatenate` operand or result
/// MUST call this rather than re-deriving it; a second copy silently disagrees
/// about element order.
#[must_use]
pub fn promoted_concatenate_dimensions(value_type: &SolveValueType, rank: usize) -> Vec<u32> {
    value_type
        .dimensions()
        .iter()
        .copied()
        .chain(std::iter::repeat_n(1, rank - value_type.dimensions().len()))
        .collect()
}
