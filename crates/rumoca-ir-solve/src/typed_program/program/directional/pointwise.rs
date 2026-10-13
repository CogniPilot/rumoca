//! Compact pointwise lifting of the canonical scalar directional arithmetic.

use super::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder};

impl<'program> DirectionalBuilder<'_, 'program> {
    pub(super) fn derive_unary(
        &mut self,
        operator: SolveUnaryOperator,
        operand: Directional<ProgramRegister<'program>>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
        if !pointwise_unary_operator(operator) || value_type.dimensions().is_empty() {
            return DirectionalArithmetic {
                builder: self.builder,
            }
            .derive_unary(operator, operand, value_type, provenance);
        }
        let primal = self.builder.unary(operator, operand.primal, provenance)?;
        let tangent = self.pointwise_unary(operator, operand, primal, value_type, provenance)?;
        Ok(Directional { primal, tangent })
    }

    pub(super) fn derive_binary(
        &mut self,
        operator: SolveBinaryOperator,
        lhs: Directional<ProgramRegister<'program>>,
        rhs: Directional<ProgramRegister<'program>>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
        if !pointwise_binary_operator(operator)
            || value_type.dimensions().is_empty()
            || !is_real(value_type)
        {
            return DirectionalArithmetic {
                builder: self.builder,
            }
            .derive_binary(operator, lhs, rhs, value_type, provenance);
        }
        let primal = self
            .builder
            .binary(operator, lhs.primal, rhs.primal, provenance)?;
        let tangent = if lhs.tangent.is_none() && rhs.tangent.is_none() {
            None
        } else {
            Some(self.pointwise_binary_tangent(
                operator,
                [(lhs, true), (rhs, true)],
                value_type,
                provenance,
            )?)
        };
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

    /// `aggregate op scalar` (or `scalar op aggregate`) under the scalar
    /// directional arithmetic of `operator`, lifted over the aggregate domain.
    pub(super) fn pointwise_broadcast(
        &mut self,
        operator: SolveBinaryOperator,
        aggregate: Directional<ProgramRegister<'program>>,
        scalar: Directional<ProgramRegister<'program>>,
        scalar_on_lhs: bool,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        if aggregate.tangent.is_none() && scalar.tangent.is_none() {
            return Ok(None);
        }
        let operands = if scalar_on_lhs {
            [(scalar, false), (aggregate, true)]
        } else {
            [(aggregate, true), (scalar, false)]
        };
        self.pointwise_binary_tangent(operator, operands, value_type, provenance)
            .map(Some)
    }

    /// The elementwise tangent of a tensor binary operation, one scalar
    /// directional relation per element; each operand is flagged as indexed
    /// by the domain or as a scalar shared by every element.
    fn pointwise_binary_tangent(
        &mut self,
        operator: SolveBinaryOperator,
        operands: [(Directional<ProgramRegister<'program>>, bool); 2],
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let mut captures = Vec::new();
        let [(lhs, lhs_indexed), (rhs, rhs_indexed)] = operands;
        let lhs = capture(&mut captures, lhs);
        let rhs = capture(&mut captures, rhs);
        let body_type = SolveValueType::scalar(value_type.element_type());
        self.builder.map(
            tensor_domain(value_type.dimensions()),
            &captures,
            body_type.clone(),
            provenance,
            |builder, captures, binders, output| {
                let indices = load_indices(builder, binders, provenance)?;
                let lhs = load_capture(
                    builder,
                    captures,
                    lhs,
                    lhs_indexed.then_some(indices.as_slice()),
                    provenance,
                )?;
                let rhs = load_capture(
                    builder,
                    captures,
                    rhs,
                    rhs_indexed.then_some(indices.as_slice()),
                    provenance,
                )?;
                let mut arithmetic = DirectionalArithmetic { builder };
                let value = arithmetic.derive_binary(operator, lhs, rhs, &body_type, provenance)?;
                let tangent = arithmetic.tangent_or_zero(value, &body_type, provenance)?;
                builder.store(output, tangent, provenance)
            },
        )
    }
}

/// Unary operators whose aggregate tangent is lifted elementwise.
pub(super) const fn pointwise_unary_operator(operator: SolveUnaryOperator) -> bool {
    matches!(
        operator,
        SolveUnaryOperator::Abs
            | SolveUnaryOperator::Sqrt
            | SolveUnaryOperator::Asin
            | SolveUnaryOperator::Acos
            | SolveUnaryOperator::Log
            | SolveUnaryOperator::Log10
    )
}

/// Binary operators whose aggregate tangent is lifted elementwise: the
/// quotient, power, extremum and arc tangent relations, which are not
/// expressible as aggregate arithmetic.
pub(super) const fn pointwise_binary_operator(operator: SolveBinaryOperator) -> bool {
    matches!(
        operator,
        SolveBinaryOperator::Divide
            | SolveBinaryOperator::Power
            | SolveBinaryOperator::Min
            | SolveBinaryOperator::Max
            | SolveBinaryOperator::Atan2
    )
}

/// A broadcast whose tangent is lifted elementwise: every pointwise operator,
/// so an aggregate and its scalar share one checked map domain.
pub(super) const fn pointwise_broadcast_operator(operator: SolveBinaryOperator) -> bool {
    pointwise_binary_operator(operator)
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

/// Whether an aggregate of `value_type` is a map domain the arithmetic profile
/// admits, so its pointwise tangent can be built as one compact map.
pub(super) fn domain_admitted(
    arithmetic: SolveArithmeticProfile,
    value_type: &SolveValueType,
    provenance: Span,
) -> bool {
    require_map_domain(
        &tensor_domain(value_type.dimensions()),
        arithmetic,
        provenance,
    )
    .is_ok()
}

pub(super) fn tensor_domain(dimensions: &[u32]) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: dimensions
            .iter()
            .enumerate()
            .map(|(axis, extent)| StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id: axis,
                display_name: format!("axis{axis}"),
                lower: 1,
                upper: i64::from(*extent),
                step: 1,
            })
            .collect(),
    }
}
