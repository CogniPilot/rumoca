use super::*;

pub(crate) fn element_matches_type(value: SolveValueKind, scalar: SolveScalarType) -> bool {
    match (value, scalar) {
        (
            SolveValueKind::Real32(_),
            SolveScalarType::Real {
                format: SolveRealFormat::Binary32,
                ..
            },
        )
        | (
            SolveValueKind::Real64(_),
            SolveScalarType::Real {
                format: SolveRealFormat::Binary64,
                ..
            },
        )
        | (SolveValueKind::Boolean(_), SolveScalarType::Boolean) => true,
        (SolveValueKind::Integer(value), SolveScalarType::Integer(domain)) => {
            domain.contains(value)
        }
        _ => false,
    }
}

pub(super) fn scalar_element(
    value: &TypedValue,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    if !value.value_type.dimensions().is_empty() || value.elements.len() != 1 {
        return invalid("read scalar value", provenance);
    }
    Ok(value.elements[0])
}

pub(super) fn scalar_boolean(
    value: &TypedValue,
    provenance: Span,
) -> Result<bool, TypedProgramEvalError> {
    match scalar_element(value, provenance)? {
        SolveValueKind::Boolean(value) => Ok(value),
        _ => invalid("read Boolean condition", provenance),
    }
}

pub(super) fn scalar_integer(
    value: &TypedValue,
    provenance: Span,
) -> Result<i64, TypedProgramEvalError> {
    match scalar_element(value, provenance)? {
        SolveValueKind::Integer(value) => Ok(value),
        _ => invalid("read Integer index", provenance),
    }
}

pub(super) fn row_major_offset(dimensions: &[u32], indices: &[u32]) -> Option<usize> {
    if dimensions.len() != indices.len() {
        return None;
    }
    dimensions
        .iter()
        .copied()
        .zip(indices.iter().copied())
        .try_fold(0usize, |offset, (extent, index)| {
            if index >= extent {
                return None;
            }
            offset
                .checked_mul(extent as usize)?
                .checked_add(index as usize)
        })
}

pub(super) fn select_modelica_element(
    value: &TypedValue,
    indices: &[i64],
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    let dimensions = value.value_type.dimensions();
    if dimensions.len() != indices.len() {
        return invalid("project aggregate element", provenance);
    }
    let mut offset = 0usize;
    // MLS §10.5: source indices are one-based Integers within each declared axis.
    for (axis, (&extent, &index)) in dimensions.iter().zip(indices).enumerate() {
        if index < 1 || index > i64::from(extent) {
            return Err(TypedProgramEvalError::TensorIndexOutOfBounds {
                axis: axis + 1,
                index,
                extent,
                provenance,
            });
        }
        let coordinate = usize::try_from(index - 1)
            .map_err(|_| invalid_error("project aggregate element", provenance))?;
        offset = offset
            .checked_mul(extent as usize)
            .and_then(|offset| offset.checked_add(coordinate))
            .ok_or(invalid_error("project aggregate element", provenance))?;
    }
    value
        .elements
        .get(offset)
        .copied()
        .ok_or(invalid_error("project aggregate element", provenance))
}

pub(super) fn project_slice(
    aggregate: &TypedValue,
    origin: &[u32],
    destination_type: &SolveValueType,
    provenance: Span,
) -> Result<Vec<SolveValueKind>, TypedProgramEvalError> {
    let dimensions = destination_type.dimensions();
    let mut coordinate = vec![0u32; dimensions.len()];
    let mut elements = Vec::with_capacity(destination_type.scalar_count() as usize);
    for _ in 0..destination_type.scalar_count() {
        let source = coordinate
            .iter()
            .zip(origin)
            .map(|(coordinate, origin)| coordinate.checked_add(*origin))
            .collect::<Option<Vec<_>>>()
            .ok_or(invalid_error("project aggregate slice", provenance))?;
        let offset = row_major_offset(aggregate.value_type.dimensions(), &source)
            .ok_or(invalid_error("project aggregate slice", provenance))?;
        elements.push(
            aggregate
                .elements
                .get(offset)
                .copied()
                .ok_or(invalid_error("project aggregate slice", provenance))?,
        );
        increment_coordinate(&mut coordinate, dimensions);
    }
    Ok(elements)
}

#[derive(Clone, Copy)]
pub(super) enum ResolvedTensorViewAxis {
    Index(u32),
    Span { origin: u32 },
}

pub(super) fn project_tensor_view(
    aggregate: &TypedValue,
    axes: &[ResolvedTensorViewAxis],
    destination_type: &SolveValueType,
    provenance: Span,
) -> Result<Vec<SolveValueKind>, TypedProgramEvalError> {
    let dimensions = destination_type.dimensions();
    let mut coordinate = vec![0u32; dimensions.len()];
    let mut elements = Vec::with_capacity(destination_type.scalar_count() as usize);
    for _ in 0..destination_type.scalar_count() {
        let source = tensor_view_coordinate(axes, &coordinate, provenance)?;
        let offset = row_major_offset(aggregate.value_type.dimensions(), &source)
            .ok_or(invalid_error("project aggregate view", provenance))?;
        elements.push(
            aggregate
                .elements
                .get(offset)
                .copied()
                .ok_or(invalid_error("project aggregate view", provenance))?,
        );
        increment_coordinate(&mut coordinate, dimensions);
    }
    Ok(elements)
}

pub(super) fn tensor_view_coordinate(
    axes: &[ResolvedTensorViewAxis],
    coordinate: &[u32],
    provenance: Span,
) -> Result<Vec<u32>, TypedProgramEvalError> {
    let mut retained = coordinate.iter().copied();
    let result = axes
        .iter()
        .map(|axis| match *axis {
            ResolvedTensorViewAxis::Index(index) => Some(index),
            ResolvedTensorViewAxis::Span { origin } => origin.checked_add(retained.next()?),
        })
        .collect::<Option<Vec<_>>>()
        .ok_or(invalid_error(
            "resolve aggregate view coordinate",
            provenance,
        ))?;
    if retained.next().is_some() {
        return invalid("resolve aggregate view coordinate", provenance);
    }
    Ok(result)
}

pub(super) fn increment_coordinate(coordinate: &mut [u32], dimensions: &[u32]) {
    for axis in (0..coordinate.len()).rev() {
        coordinate[axis] += 1;
        if coordinate[axis] < dimensions[axis] {
            return;
        }
        coordinate[axis] = 0;
    }
}

pub(super) fn invalid<T>(
    operation: &'static str,
    provenance: Span,
) -> Result<T, TypedProgramEvalError> {
    Err(invalid_error(operation, provenance))
}

pub(super) fn invalid_error(operation: &'static str, provenance: Span) -> TypedProgramEvalError {
    TypedProgramEvalError::InvalidCheckedProgram {
        operation,
        provenance,
    }
}
