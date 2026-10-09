//! Compact pointwise lifting of the canonical scalar directional arithmetic.

use super::*;
use rumoca_core::StructuredIndexBinder;

impl<'program> DirectionalBuilder<'_, 'program> {
    pub(super) fn derive_unary(
        &mut self,
        operator: SolveUnaryOperator,
        operand: Directional<ProgramRegister<'program>>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
        if operator != SolveUnaryOperator::Abs || value_type.dimensions().is_empty() {
            return DirectionalArithmetic {
                builder: self.builder,
            }
            .derive_unary(operator, operand, value_type, provenance);
        }
        let primal = self.builder.unary(operator, operand.primal, provenance)?;
        let tangent = self.pointwise_unary(operator, operand, primal, value_type, provenance)?;
        Ok(Directional { primal, tangent })
    }

    fn pointwise_unary(
        &mut self,
        operator: SolveUnaryOperator,
        operand: Directional<ProgramRegister<'program>>,
        primal: ProgramRegister<'program>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        if operand.tangent.is_none() {
            return Ok(None);
        }
        let mut captures = Vec::new();
        let operand = capture(&mut captures, operand);
        let primal = capture(
            &mut captures,
            Directional {
                primal,
                tangent: None,
            },
        );
        let body_type = SolveValueType::scalar(value_type.element_type());
        let tangent = self.builder.map(
            tensor_domain(value_type.dimensions()),
            &captures,
            body_type.clone(),
            provenance,
            |builder, captures, binders, output| {
                let indices = load_indices(builder, binders, provenance)?;
                let operand = load_capture(builder, captures, operand, Some(&indices), provenance)?;
                let primal =
                    load_capture(builder, captures, primal, Some(&indices), provenance)?.primal;
                let mut arithmetic = DirectionalArithmetic { builder };
                let value = arithmetic
                    .derive_unary_from_primal(operator, operand, primal, &body_type, provenance)?;
                let tangent = arithmetic.tangent_or_zero(value, &body_type, provenance)?;
                builder.store(output, tangent, provenance)
            },
        )?;
        Ok(Some(tangent))
    }

    pub(super) fn pointwise_power_broadcast(
        &mut self,
        aggregate: Directional<ProgramRegister<'program>>,
        scalar: Directional<ProgramRegister<'program>>,
        scalar_on_lhs: bool,
        primal: ProgramRegister<'program>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        if aggregate.tangent.is_none() && scalar.tangent.is_none() {
            return Ok(None);
        }
        let mut captures = Vec::new();
        let aggregate = capture(&mut captures, aggregate);
        let scalar = capture(&mut captures, scalar);
        let primal = capture(
            &mut captures,
            Directional {
                primal,
                tangent: None,
            },
        );
        let body_type = SolveValueType::scalar(value_type.element_type());
        let domain = tensor_domain(value_type.dimensions());
        let tangent = self.builder.map(
            domain,
            &captures,
            body_type.clone(),
            provenance,
            |builder, captures, binders, output| {
                let indices = load_indices(builder, binders, provenance)?;
                let aggregate =
                    load_capture(builder, captures, aggregate, Some(&indices), provenance)?;
                let scalar = load_capture(builder, captures, scalar, None, provenance)?;
                let (lhs, rhs) = if scalar_on_lhs {
                    (scalar, aggregate)
                } else {
                    (aggregate, scalar)
                };
                let primal =
                    load_capture(builder, captures, primal, Some(&indices), provenance)?.primal;
                let mut arithmetic = DirectionalArithmetic { builder };
                let zero = arithmetic.zero(&body_type, provenance)?;
                let lhs_tangent = arithmetic.tangent_or_zero(lhs, &body_type, provenance)?;
                let rhs_tangent = arithmetic.tangent_or_zero(rhs, &body_type, provenance)?;
                let tangent = arithmetic.derive_power(
                    lhs,
                    rhs,
                    primal,
                    [lhs_tangent, rhs_tangent],
                    (&body_type, zero),
                    provenance,
                )?;
                builder.store(output, tangent, provenance)
            },
        )?;
        Ok(Some(tangent))
    }
}

fn capture<T: Copy>(captures: &mut Vec<T>, value: Directional<T>) -> Directional<usize> {
    let primal = captures.len();
    captures.push(value.primal);
    let tangent = value.tangent.map(|tangent| {
        let index = captures.len();
        captures.push(tangent);
        index
    });
    Directional { primal, tangent }
}

pub(super) fn load_capture<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    captures: &[ProgramSlot<'program>],
    value: Directional<usize>,
    indices: Option<&[ProgramRegister<'program>]>,
    provenance: Span,
) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
    let mut load = |index| {
        let value = builder.load(captures[index], provenance)?;
        match indices {
            Some(indices) => builder.project_element_dynamic(value, indices, provenance),
            None => Ok(value),
        }
    };
    let primal = load(value.primal)?;
    let tangent = value.tangent.map(load).transpose()?;
    Ok(Directional { primal, tangent })
}

pub(super) fn load_indices<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    binders: &[ProgramSlot<'program>],
    provenance: Span,
) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
    binders
        .iter()
        .map(|binder| builder.load(*binder, provenance))
        .collect()
}

pub(super) fn tensor_domain(dimensions: &[u32]) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: dimensions
            .iter()
            .enumerate()
            .map(|(axis, extent)| StructuredIndexBinder {
                id: axis,
                display_name: format!("axis{axis}"),
                lower: 1,
                upper: i64::from(*extent),
                step: 1,
            })
            .collect(),
    }
}
