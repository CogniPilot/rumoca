//! Expansion of compact BLT family blocks into the scalar blocks they stand for.

use indexmap::IndexMap;

use crate::types::{BltBlock, EquationRef, StructuralError, StructuredScalarBlock, UnknownId};

/// Replace every compact family block with the run of scalar blocks it stands
/// for, so Phase B eliminates the family's rows exactly as it did before the
/// compact representation existed.
///
/// Elimination is inherently per-equation — it solves one row symbolically and
/// substitutes the result — so a compact block has no elimination of its own to
/// perform. Dropping it instead would silently leave the family's rows in the
/// DAE, changing the emitted equations; expanding keeps the decomposition and
/// the elimination in agreement. `sorted.matching` is the authority on which
/// unknown each row was matched to, and `sort_dae` only returns after proving
/// the matching perfect, so every row has an entry.
pub(super) fn expand_compact_family_blocks(
    blocks: Vec<BltBlock>,
    matching: &[(EquationRef, UnknownId)],
) -> Result<Vec<BltBlock>, StructuralError> {
    if !blocks
        .iter()
        .any(|block| matches!(block, BltBlock::StructuredScalar(..)))
    {
        return Ok(blocks);
    }
    let unknown_of_row: IndexMap<usize, &UnknownId> = matching
        .iter()
        .map(|(equation, unknown)| (equation.0, unknown))
        .collect();
    let mut expanded = Vec::with_capacity(blocks.len());
    for block in blocks {
        match block {
            BltBlock::StructuredScalar(family) => {
                expand_one_family_block(&family, &unknown_of_row, &mut expanded)?;
            }
            other => expanded.push(other),
        }
    }
    Ok(expanded)
}

fn expand_one_family_block(
    family: &StructuredScalarBlock,
    unknown_of_row: &IndexMap<usize, &UnknownId>,
    expanded: &mut Vec<BltBlock>,
) -> Result<(), StructuralError> {
    for row in family.scalar_rows() {
        let (equation, _) = row?;
        let unknown = unknown_of_row.get(&equation.0).ok_or_else(|| {
            StructuralError::ContractViolation {
                reason: format!(
                    "compact family block row {equation} is absent from the matching it was sorted with"
                ),
                span: family.span,
            }
        })?;
        expanded.push(BltBlock::Scalar {
            equation,
            unknown: (*unknown).clone(),
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests;
