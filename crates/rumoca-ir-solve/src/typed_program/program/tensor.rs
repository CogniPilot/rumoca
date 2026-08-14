//! Checked compact tensor-algebra construction.

use super::*;

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
                axes: build_view_axes(axes),
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
                axes: build_view_axes(axes),
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
        let integer_identity_is_representable = match element_type {
            SolveScalarType::Integer(domain) => domain.contains(0) && domain.contains(1),
            SolveScalarType::Real { .. } => true,
            SolveScalarType::Boolean => false,
        };
        if !element_type.belongs_to(self.arithmetic) || !integer_identity_is_representable {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
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
        let first_dimensions = promoted_concatenate_dimensions(&first_type);
        let axis = axis as usize;
        if operands.len() < 2 || axis >= first_dimensions.len() {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        let mut dimensions = first_dimensions;
        let mut axis_extent = 0u32;
        for operand in operands {
            let value_type = self.register_type(*operand, provenance)?;
            let operand_dimensions = promoted_concatenate_dimensions(value_type);
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
        let dimensions = matrix_product_dimensions(&lhs_type, rhs_type)
            .ok_or(SolveProgramConstructionError::InvalidTensorAlgebra { provenance })?;
        let result_type = if dimensions.is_empty() {
            SolveValueType::scalar(lhs_type.element_type())
        } else {
            SolveValueType::tensor(lhs_type.element_type(), dimensions)
                .map_err(|_| SolveProgramConstructionError::InvalidTensorAlgebra { provenance })?
        };
        let destination = self.issue_register(result_type, provenance)?;
        self.push(
            SolveOperation::MatrixMultiply {
                destination: destination.id,
                lhs: lhs.id,
                rhs: rhs.id,
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
        let lhs_type = self.register_type(lhs, provenance)?.clone();
        let rhs_type = self.register_type(rhs, provenance)?;
        if lhs_type.dimensions() != [3]
            || rhs_type.dimensions() != [3]
            || lhs_type.element_type() != rhs_type.element_type()
            || !lhs_type.element_type().is_numeric()
        {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        let destination = self.issue_register(lhs_type, provenance)?;
        self.push(
            SolveOperation::Cross {
                destination: destination.id,
                lhs: lhs.id,
                rhs: rhs.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn reduce(
        &mut self,
        operator: SolveReductionOperator,
        operand: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let operand_type = self.register_type(operand, provenance)?;
        let valid_element = match operator {
            SolveReductionOperator::All => operand_type.element_type() == SolveScalarType::Boolean,
            SolveReductionOperator::Sum
            | SolveReductionOperator::Product
            | SolveReductionOperator::Minimum
            | SolveReductionOperator::Maximum => operand_type.element_type().is_numeric(),
        };
        if operand_type.dimensions().is_empty() || !valid_element {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        let result_type = SolveValueType::scalar(operand_type.element_type());
        let destination = self.issue_register(result_type, provenance)?;
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

fn build_view_axes(axes: &[ProgramTensorViewAxis<'_>]) -> Box<[SolveTensorViewAxis]> {
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

fn promoted_concatenate_dimensions(value_type: &SolveValueType) -> Vec<u32> {
    match value_type.dimensions() {
        [] => vec![1, 1],
        [extent] => vec![1, *extent],
        dimensions => dimensions.to_vec(),
    }
}

fn matrix_product_dimensions(lhs: &SolveValueType, rhs: &SolveValueType) -> Option<Vec<u32>> {
    if lhs.element_type() != rhs.element_type() || !lhs.element_type().is_numeric() {
        return None;
    }
    match (lhs.dimensions(), rhs.dimensions()) {
        ([inner_lhs], [inner_rhs]) if inner_lhs == inner_rhs => Some(Vec::new()),
        ([rows, inner_lhs], [inner_rhs]) if inner_lhs == inner_rhs => Some(vec![*rows]),
        ([inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => Some(vec![*columns]),
        ([rows, inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => {
            Some(vec![*rows, *columns])
        }
        _ => None,
    }
}
