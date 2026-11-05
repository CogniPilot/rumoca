//! Several aggregate directions share one compact coordinate body.
//!
//! Scalar derivative rules remain in DirectionalArithmetic. This constructor
//! changes neither their association nor the original aggregate occurrence.
//! Only pure numeric operations are fused; calls, assertions and lazy regions
//! are deliberately not hoisted into this body.

#[cfg(test)]
mod tests;

use super::*;
use crate::typed_program::program::directional::pointwise::{
    load_indices, pointwise_binary_operator, pointwise_unary_operator, tensor_domain,
};

/// Constructor-issued aggregate directions, with the direction axis last.
/// Canonical absent results stay distinct from present results containing zeros.
/// An absent input may still produce a present result under the existing scalar
/// IEEE relation (for example zero times a non-finite sine coefficient).
/// Absent columns have canonical positive-zero padding in an existing packed
/// value; when all columns are absent, no map or packed value is emitted.
/// This result is numerical construction output, not a detached sparsity proof.
///
/// ```compile_fail,E0451
/// use rumoca_ir_solve::SolveAggregateDirectionalColumns;
/// let _ = SolveAggregateDirectionalColumns {
///     packed: None, present: vec![true].into_boxed_slice(),
/// };
/// ```
#[derive(Clone, Debug)]
pub struct SolveAggregateDirectionalColumns<'program> {
    packed: Option<ProgramRegister<'program>>,
    present: Box<[bool]>,
}

impl<'program> SolveAggregateDirectionalColumns<'program> {
    #[must_use]
    pub fn packed(&self) -> Option<ProgramRegister<'program>> {
        self.packed
    }

    #[must_use]
    pub fn present(&self) -> &[bool] {
        &self.present
    }
}

#[derive(Clone, Copy)]
enum Rule {
    Unary(SolveUnaryOperator),
    Binary(SolveBinaryOperator),
}

struct Source<'program> {
    rule: Rule,
    operands: Vec<ProgramRegister<'program>>,
    primal: ProgramRegister<'program>,
    provenance: Span,
}

impl<'program> TypedProgramBuilder<'program> {
    /// Share the exact issued unary primal and its coordinate coefficients
    /// across directions. Shape and kind are checked before emitting a map.
    pub fn aggregate_unary_directional_columns(
        &mut self,
        source: SolveAggregateUnaryDirectionalSource<'program>,
        directions: &[Option<ProgramRegister<'program>>],
    ) -> Result<SolveAggregateDirectionalColumns<'program>, SolveProgramConstructionError> {
        let columns = directions
            .iter()
            .map(|direction| [*direction, None])
            .collect::<Vec<_>>();
        construct_columns(
            self,
            Source {
                rule: Rule::Unary(source.operator),
                operands: vec![source.operand],
                primal: source.primal,
                provenance: source.provenance,
            },
            &columns,
        )
    }

    /// One checked body for all directions of the issued ordered binary
    /// operands. This API accepts no replacement primal or derivative rules.
    pub fn aggregate_binary_directional_columns(
        &mut self,
        source: SolveAggregateBinaryDirectionalSource<'program>,
        directions: &[[Option<ProgramRegister<'program>>; 2]],
    ) -> Result<SolveAggregateDirectionalColumns<'program>, SolveProgramConstructionError> {
        construct_columns(
            self,
            Source {
                rule: Rule::Binary(source.operator),
                operands: source.operands.to_vec(),
                primal: source.primal,
                provenance: source.provenance,
            },
            directions,
        )
    }
}

fn checked_types<'program>(
    builder: &TypedProgramBuilder<'program>,
    source: &Source<'program>,
    directions: &[[Option<ProgramRegister<'program>>; 2]],
) -> Result<(SolveValueType, SolveValueType), SolveProgramConstructionError> {
    let provenance = source.provenance;
    let invalid = || SolveProgramConstructionError::InvalidTensorAlgebra { provenance };
    let columns = u32::try_from(directions.len()).map_err(|_| invalid())?;
    let ty = builder.value_type_of(source.primal, provenance)?.clone();
    if columns == 0 || ty.dimensions().is_empty() || !is_real(&ty) {
        return Err(invalid());
    }
    // Check the entire packed shape, including overflow, before any emission.
    let mut shape = ty.dimensions().to_vec();
    shape.push(columns);
    SolveValueType::tensor(ty.element_type(), shape).map_err(|_| invalid())?;
    let body = SolveValueType::tensor(ty.element_type(), vec![columns]).map_err(|_| invalid())?;
    for column in directions {
        for (primal, direction) in source.operands.iter().zip(column) {
            checked_direction(builder, *primal, *direction, provenance)?;
        }
    }
    Ok((ty, body))
}

fn construct_columns<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    source: Source<'program>,
    directions: &[[Option<ProgramRegister<'program>>; 2]],
) -> Result<SolveAggregateDirectionalColumns<'program>, SolveProgramConstructionError> {
    let (ty, body_type) = checked_types(builder, &source, directions)?;
    let present: Box<[_]> = directions
        .iter()
        .map(|column| {
            source.rule.has_output(
                column
                    .iter()
                    .take(source.operands.len())
                    .any(Option::is_some),
            )
        })
        .collect();
    if !present.iter().any(|value| *value) {
        return Ok(SolveAggregateDirectionalColumns {
            packed: None,
            present,
        });
    }
    let (captures, column_inputs) = capture_columns(&source, directions);
    let primal_index = source.operands.len();
    let scalar_type = SolveValueType::scalar(ty.element_type());
    let provenance = source.provenance;
    let packed = builder.map(
        tensor_domain(ty.dimensions()),
        &captures,
        body_type.clone(),
        provenance,
        |builder, captures, binders, output| {
            let indices = load_indices(builder, binders, provenance)?;
            let operands = (0..primal_index)
                .map(|index| load_coordinate(builder, captures, &indices, index, provenance))
                .collect::<Result<Vec<_>, _>>()?;
            let primal = load_coordinate(builder, captures, &indices, primal_index, provenance)?;
            let mut results = Vec::with_capacity(column_inputs.len());
            for column in &column_inputs {
                let values =
                    load_column(builder, captures, &indices, &operands, column, provenance)?;
                results.push(source.rule.tangent(
                    builder,
                    &values,
                    primal,
                    &scalar_type,
                    provenance,
                )?);
            }
            let packed = builder.construct_aggregate(
                &results,
                body_type.dimensions().to_vec(),
                provenance,
            )?;
            builder.store(output, packed, provenance)
        },
    )?;
    Ok(SolveAggregateDirectionalColumns {
        packed: Some(packed),
        present,
    })
}

impl Rule {
    // These are the canonical aggregate admission branches in pointwise.rs,
    // not a new sparsity rule: non-pointwise arithmetic retains materialized
    // zero directions, including their signed-zero/non-finite behavior.
    fn has_output(self, has_direction: bool) -> bool {
        has_direction
            || match self {
                Self::Unary(operator) => !pointwise_unary_operator(operator),
                Self::Binary(operator) => !pointwise_binary_operator(operator),
            }
    }

    fn tangent<'program>(
        self,
        builder: &mut TypedProgramBuilder<'program>,
        operands: &[Directional<ProgramRegister<'program>>],
        primal: ProgramRegister<'program>,
        ty: &SolveValueType,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let mut arithmetic = DirectionalArithmetic::dense(builder);
        if !self.has_output(operands.iter().any(|operand| operand.tangent.is_some())) {
            return arithmetic.zero(ty, provenance);
        }
        let value = match self {
            Self::Unary(operator) => arithmetic.derive_unary_from_primal(
                operator,
                operands[0],
                primal,
                ty,
                provenance,
            )?,
            Self::Binary(operator) => arithmetic.derive_binary_from_primal(
                operator,
                operands[0],
                operands[1],
                primal,
                ty,
                provenance,
            )?,
        };
        arithmetic.tangent_or_zero(value, ty, provenance)
    }
}

fn load_coordinate<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    captures: &[ProgramSlot<'program>],
    indices: &[ProgramRegister<'program>],
    index: usize,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let value = builder.load(captures[index], provenance)?;
    builder.project_element_dynamic(value, indices, provenance)
}

fn capture_columns<'program>(
    source: &Source<'program>,
    directions: &[[Option<ProgramRegister<'program>>; 2]],
) -> (Vec<ProgramRegister<'program>>, Vec<[Option<usize>; 2]>) {
    let mut captures = source.operands.clone();
    captures.push(source.primal);
    let column_inputs = directions
        .iter()
        .map(|column| {
            let mut indices = [None; 2];
            for (index, direction) in column.iter().take(source.operands.len()).enumerate() {
                if let Some(direction) = direction {
                    indices[index] = Some(captures.len());
                    captures.push(*direction);
                }
            }
            indices
        })
        .collect::<Vec<_>>();
    (captures, column_inputs)
}

fn load_column<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    captures: &[ProgramSlot<'program>],
    indices: &[ProgramRegister<'program>],
    operands: &[ProgramRegister<'program>],
    column: &[Option<usize>; 2],
    provenance: Span,
) -> Result<Vec<Directional<ProgramRegister<'program>>>, SolveProgramConstructionError> {
    operands
        .iter()
        .enumerate()
        .map(|(index, primal)| {
            let tangent = column[index]
                .map(|index| load_coordinate(builder, captures, indices, index, provenance))
                .transpose()?;
            Ok(Directional {
                primal: *primal,
                tangent,
            })
        })
        .collect()
}
