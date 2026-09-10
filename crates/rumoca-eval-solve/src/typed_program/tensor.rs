//! Boundary-local execution of compact checked tensor-algebra operations.

use super::*;

impl EvalFrame<'_, '_> {
    pub(super) fn eval_broadcast_binary(
        &mut self,
        destination: SolveRegisterId,
        operator: SolveBinaryOperator,
        aggregate: SolveRegisterId,
        scalar: SolveRegisterId,
        scalar_on_lhs: bool,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let aggregate = self.read(aggregate, provenance)?;
        let scalar = scalar_element(self.read(scalar, provenance)?, provenance)?;
        let element_type = aggregate.value_type.element_type();
        let elements = aggregate
            .elements
            .iter()
            .copied()
            .map(|element| {
                let (lhs, rhs) = if scalar_on_lhs {
                    (scalar, element)
                } else {
                    (element, scalar)
                };
                eval_binary_element(operator, lhs, rhs, element_type, provenance)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    pub(super) fn eval_diagonal(
        &mut self,
        destination: SolveRegisterId,
        operand: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let operand = self.read(operand, provenance)?;
        let [extent] = operand.value_type.dimensions() else {
            return invalid("diagonal tensor", provenance);
        };
        let zero = match operand.value_type.element_type() {
            SolveScalarType::Real {
                format: SolveRealFormat::Binary32,
                ..
            } => SolveValueKind::Real32(0.0_f32.to_bits()),
            SolveScalarType::Real {
                format: SolveRealFormat::Binary64,
                ..
            } => SolveValueKind::Real64(0.0_f64.to_bits()),
            SolveScalarType::Integer(domain) if domain.contains(0) => SolveValueKind::Integer(0),
            SolveScalarType::Integer(_) | SolveScalarType::Boolean => {
                return invalid("diagonal tensor", provenance);
            }
        };
        let extent = *extent as usize;
        let mut elements = vec![zero; extent * extent];
        for (ordinal, value) in operand.elements.iter().copied().enumerate() {
            elements[ordinal * extent + ordinal] = value;
        }
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    pub(super) fn eval_identity(
        &mut self,
        destination: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let value_type = self.destination_type(destination, provenance)?.clone();
        let [rows, columns] = value_type.dimensions() else {
            return invalid("identity tensor", provenance);
        };
        if rows != columns {
            return invalid("identity tensor", provenance);
        }
        let (zero, one) = match value_type.element_type() {
            SolveScalarType::Real {
                format: SolveRealFormat::Binary32,
                ..
            } => (
                SolveValueKind::Real32(0.0_f32.to_bits()),
                SolveValueKind::Real32(1.0_f32.to_bits()),
            ),
            SolveScalarType::Real {
                format: SolveRealFormat::Binary64,
                ..
            } => (
                SolveValueKind::Real64(0.0_f64.to_bits()),
                SolveValueKind::Real64(1.0_f64.to_bits()),
            ),
            SolveScalarType::Integer(domain) if domain.contains(0) && domain.contains(1) => {
                (SolveValueKind::Integer(0), SolveValueKind::Integer(1))
            }
            SolveScalarType::Integer(_) | SolveScalarType::Boolean => {
                return invalid("identity tensor", provenance);
            }
        };
        let extent = *rows as usize;
        let elements = (0..extent)
            .flat_map(|row| (0..extent).map(move |column| if row == column { one } else { zero }))
            .collect();
        self.write(
            destination,
            TypedValue::checked(value_type, elements, provenance)?,
            provenance,
        )
    }

    pub(super) fn eval_concatenate(
        &mut self,
        destination: SolveRegisterId,
        axis: u32,
        operands: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let value_type = self.destination_type(destination, provenance)?.clone();
        let axis = axis as usize;
        if axis >= value_type.dimensions().len() || operands.is_empty() {
            return invalid("concatenate tensors", provenance);
        }
        let inner_width = value_type.dimensions()[axis + 1..]
            .iter()
            .try_fold(1usize, |width, extent| width.checked_mul(*extent as usize))
            .ok_or(invalid_error("concatenate tensors", provenance))?;
        let outer_count = value_type.dimensions()[..axis]
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or(invalid_error("concatenate tensors", provenance))?;
        let operands = operands
            .iter()
            .map(|operand| self.read(*operand, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        let mut elements = Vec::with_capacity(value_type.scalar_count() as usize);
        for outer in 0..outer_count {
            for operand in &operands {
                let axis_extent = operand
                    .value_type
                    .dimensions()
                    .get(axis)
                    .copied()
                    .unwrap_or(1);
                let block_width = (axis_extent as usize)
                    .checked_mul(inner_width)
                    .ok_or(invalid_error("concatenate tensors", provenance))?;
                let start = outer
                    .checked_mul(block_width)
                    .ok_or(invalid_error("concatenate tensors", provenance))?;
                let end = start
                    .checked_add(block_width)
                    .ok_or(invalid_error("concatenate tensors", provenance))?;
                elements.extend_from_slice(
                    operand
                        .elements
                        .get(start..end)
                        .ok_or(invalid_error("concatenate tensors", provenance))?,
                );
            }
        }
        self.write(
            destination,
            TypedValue::checked(value_type, elements, provenance)?,
            provenance,
        )
    }

    pub(super) fn eval_scale(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        scalar: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let aggregate = self.read(aggregate, provenance)?;
        let scalar = scalar_element(self.read(scalar, provenance)?, provenance)?;
        let element_type = aggregate.value_type.element_type();
        let elements = aggregate
            .elements
            .iter()
            .copied()
            .map(|element| {
                eval_binary_element(
                    SolveBinaryOperator::Multiply,
                    element,
                    scalar,
                    element_type,
                    provenance,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    pub(super) fn eval_transpose(
        &mut self,
        destination: SolveRegisterId,
        operand: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let operand = self.read(operand, provenance)?;
        let [rows, columns, trailing @ ..] = operand.value_type.dimensions() else {
            return invalid("transpose tensor", provenance);
        };
        let element_width = trailing
            .iter()
            .try_fold(1usize, |width, extent| width.checked_mul(*extent as usize))
            .ok_or(invalid_error("transpose tensor", provenance))?;
        let mut elements = Vec::with_capacity(operand.elements.len());
        for column in 0..*columns as usize {
            for row in 0..*rows as usize {
                let start = (row * *columns as usize + column)
                    .checked_mul(element_width)
                    .ok_or(invalid_error("transpose tensor", provenance))?;
                let end = start
                    .checked_add(element_width)
                    .ok_or(invalid_error("transpose tensor", provenance))?;
                elements.extend_from_slice(
                    operand
                        .elements
                        .get(start..end)
                        .ok_or(invalid_error("transpose tensor", provenance))?,
                );
            }
        }
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    pub(super) fn eval_matrix_multiply(
        &mut self,
        destination: SolveRegisterId,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let lhs = self.read(lhs, provenance)?;
        let rhs = self.read(rhs, provenance)?;
        let (rows, inner, columns) = matrix_product_extents(
            lhs.value_type.dimensions(),
            rhs.value_type.dimensions(),
            provenance,
        )?;
        let mut elements = Vec::with_capacity(rows * columns);
        for row in 0..rows {
            for column in 0..columns {
                elements.push(matrix_product_element(
                    lhs, rhs, row, column, inner, columns, provenance,
                )?);
            }
        }
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    pub(super) fn eval_cross(
        &mut self,
        destination: SolveRegisterId,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let lhs = self.read(lhs, provenance)?;
        let rhs = self.read(rhs, provenance)?;
        if lhs.value_type.dimensions() != [3]
            || rhs.value_type != lhs.value_type
            || lhs.elements.len() != 3
            || rhs.elements.len() != 3
        {
            return invalid("cross product", provenance);
        }
        let scalar = lhs.value_type.element_type();
        let component = |first: usize,
                         second: usize,
                         other_first: usize,
                         other_second: usize|
         -> Result<SolveValueKind, TypedProgramEvalError> {
            let positive = eval_binary_element(
                SolveBinaryOperator::Multiply,
                lhs.elements[first],
                rhs.elements[second],
                scalar,
                provenance,
            )?;
            let negative = eval_binary_element(
                SolveBinaryOperator::Multiply,
                lhs.elements[other_first],
                rhs.elements[other_second],
                scalar,
                provenance,
            )?;
            eval_binary_element(
                SolveBinaryOperator::Subtract,
                positive,
                negative,
                scalar,
                provenance,
            )
        };
        let elements = vec![
            component(1, 2, 2, 1)?,
            component(2, 0, 0, 2)?,
            component(0, 1, 1, 0)?,
        ];
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    pub(super) fn eval_reduce(
        &mut self,
        destination: SolveRegisterId,
        operator: SolveReductionOperator,
        operand: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let operand = self.read(operand, provenance)?;
        let binary = match operator {
            SolveReductionOperator::Sum => SolveBinaryOperator::Add,
            SolveReductionOperator::Product => SolveBinaryOperator::Multiply,
            SolveReductionOperator::Minimum => SolveBinaryOperator::Min,
            SolveReductionOperator::Maximum => SolveBinaryOperator::Max,
            SolveReductionOperator::All => SolveBinaryOperator::And,
        };
        let scalar = operand.value_type.element_type();
        let mut elements = operand.elements.iter().copied();
        let mut value = elements
            .next()
            .ok_or(invalid_error("reduce tensor", provenance))?;
        for element in elements {
            value = eval_binary_element(binary, value, element, scalar, provenance)?;
        }
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            vec![value],
            provenance,
        )?;
        self.write(destination, value, provenance)
    }
}

fn matrix_product_element(
    lhs: &TypedValue,
    rhs: &TypedValue,
    row: usize,
    column: usize,
    inner: usize,
    columns: usize,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    let mut shared = 0..inner;
    let first = shared
        .next()
        .ok_or(invalid_error("multiply matrices", provenance))?;
    let mut value = matrix_product_term(lhs, rhs, row, column, first, columns, provenance)?;
    for shared in shared {
        let term = matrix_product_term(lhs, rhs, row, column, shared, columns, provenance)?;
        value = eval_binary_element(
            SolveBinaryOperator::Add,
            value,
            term,
            lhs.value_type.element_type(),
            provenance,
        )?;
    }
    Ok(value)
}

fn matrix_product_term(
    lhs: &TypedValue,
    rhs: &TypedValue,
    row: usize,
    column: usize,
    shared: usize,
    columns: usize,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    let lhs_index = matrix_lhs_index(lhs.value_type.dimensions(), row, shared);
    let rhs_index = matrix_rhs_index(rhs.value_type.dimensions(), shared, column, columns);
    eval_binary_element(
        SolveBinaryOperator::Multiply,
        lhs.elements[lhs_index],
        rhs.elements[rhs_index],
        lhs.value_type.element_type(),
        provenance,
    )
}

fn matrix_product_extents(
    lhs: &[u32],
    rhs: &[u32],
    provenance: Span,
) -> Result<(usize, usize, usize), TypedProgramEvalError> {
    let extents = match (lhs, rhs) {
        ([inner_lhs], [inner_rhs]) if inner_lhs == inner_rhs => (1, *inner_lhs as usize, 1),
        ([rows, inner_lhs], [inner_rhs]) if inner_lhs == inner_rhs => {
            (*rows as usize, *inner_lhs as usize, 1)
        }
        ([inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => {
            (1, *inner_lhs as usize, *columns as usize)
        }
        ([rows, inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => {
            (*rows as usize, *inner_lhs as usize, *columns as usize)
        }
        _ => return invalid("resolve matrix product extents", provenance),
    };
    Ok(extents)
}

fn matrix_lhs_index(dimensions: &[u32], row: usize, shared: usize) -> usize {
    if dimensions.len() == 1 {
        shared
    } else {
        row * dimensions[1] as usize + shared
    }
}

fn matrix_rhs_index(dimensions: &[u32], shared: usize, column: usize, columns: usize) -> usize {
    if dimensions.len() == 1 {
        shared
    } else {
        shared * columns + column
    }
}
