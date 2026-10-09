//! Ordered compact lifting of the canonical scalar maximum relation.
use super::pointwise::{load_capture, load_indices, tensor_domain};
use super::*;

impl<'program> DirectionalBuilder<'_, 'program> {
    pub(super) fn reduce_maximum_tangent(
        &mut self,
        operand: Directional<ProgramRegister<'program>>,
        operand_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let Some(tangent) = operand.tangent else {
            return Ok(None);
        };
        let origin = vec![0; operand_type.dimensions().len()];
        let first_primal =
            self.builder
                .project_element(operand.primal, origin.clone(), provenance)?;
        let first_tangent = self.builder.project_element(tangent, origin, provenance)?;
        let scalar = SolveValueType::scalar(operand_type.element_type());
        let result = self.builder.fold(
            tensor_domain(operand_type.dimensions()),
            &[first_primal, first_tangent],
            &[operand.primal, tangent],
            provenance,
            |builder, carried, captures, binders, outputs| {
                let indices = load_indices(builder, binders, provenance)?;
                let first = is_first_point(builder, &indices, provenance)?;
                let prefix = load_capture(
                    builder,
                    carried,
                    Directional {
                        primal: 0,
                        tangent: Some(1),
                    },
                    None,
                    provenance,
                )?;
                let cell = load_capture(
                    builder,
                    captures,
                    Directional {
                        primal: 0,
                        tangent: Some(1),
                    },
                    Some(&indices),
                    provenance,
                )?;
                let arguments = [
                    prefix.primal,
                    prefix
                        .tangent
                        .ok_or(SolveProgramConstructionError::InvalidFold { provenance })?,
                    cell.primal,
                    cell.tangent
                        .ok_or(SolveProgramConstructionError::InvalidFold { provenance })?,
                ];
                let result = builder.conditional(
                    first,
                    &arguments,
                    vec![scalar.clone(), scalar.clone()],
                    provenance,
                    |builder, captures, outputs| {
                        keep_prefix(builder, captures, outputs, provenance)
                    },
                    |builder, captures, outputs| {
                        maximum_step(builder, captures, outputs, provenance)
                    },
                )?;
                for (output, value) in outputs.iter().zip(result) {
                    builder.store(*output, value, provenance)?;
                }
                Ok(())
            },
        )?;
        Ok(Some(result[1]))
    }
}

fn is_first_point<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    indices: &[ProgramRegister<'program>],
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let one = SolveValue::integer(builder.arithmetic, 1)
        .map_err(|_| SolveProgramConstructionError::InvalidFold { provenance })?;
    let one = builder.constant(one, provenance)?;
    let mut first = builder.constant(SolveValue::boolean(true), provenance)?;
    for &index in indices {
        let equal = builder.compare(SolveCompareOperator::Equal, index, one, provenance)?;
        first = builder.binary(SolveBinaryOperator::And, first, equal, provenance)?;
    }
    Ok(first)
}

fn keep_prefix<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    captures: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    for (input, output) in captures.iter().zip(outputs) {
        let value = builder.load(*input, provenance)?;
        builder.store(*output, value, provenance)?;
    }
    Ok(())
}

fn maximum_step<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    captures: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let lhs = load_capture(
        builder,
        captures,
        Directional {
            primal: 0,
            tangent: Some(1),
        },
        None,
        provenance,
    )?;
    let rhs = load_capture(
        builder,
        captures,
        Directional {
            primal: 2,
            tangent: Some(3),
        },
        None,
        provenance,
    )?;
    let scalar = builder.register_type(lhs.primal, provenance)?.clone();
    let mut arithmetic = DirectionalArithmetic { builder };
    let result =
        arithmetic.derive_binary(SolveBinaryOperator::Max, lhs, rhs, &scalar, provenance)?;
    let tangent = arithmetic.tangent_or_zero(result, &scalar, provenance)?;
    builder.store(outputs[0], result.primal, provenance)?;
    builder.store(outputs[1], tangent, provenance)
}
