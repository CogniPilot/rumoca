use super::*;

pub(super) fn map_expression_operands<'dae>(
    wire: &StorageWire,
    ids: &WireIds<'dae>,
    range: OperandRangeWire,
    at: DaeProvenance,
) -> Result<Vec<ExprId<'dae>>, DaeConstructionError> {
    let operands = wire_operands(wire, range, at)?;
    map_many(&ids.expressions, operands, "expression", at)
}

pub(super) fn wire_operands(
    wire: &StorageWire,
    range: OperandRangeWire,
    at: DaeProvenance,
) -> Result<&[u32], DaeConstructionError> {
    wire.expressions
        .operands
        .get(range.indices().ok_or(DaeConstructionError::MalformedWire {
            column: "operand range",
        })?)
        .ok_or_else(|| unknown("operand range", range.start, at))
}

pub(super) fn mapped<T: Copy>(
    ids: &[T],
    raw: u32,
    kind: &'static str,
    at: DaeProvenance,
) -> Result<T, DaeConstructionError> {
    ids.get(raw as usize)
        .copied()
        .ok_or_else(|| unknown(kind, raw, at))
}

pub(super) fn map_many<T: Copy>(
    ids: &[T],
    raw: &[u32],
    kind: &'static str,
    at: DaeProvenance,
) -> Result<Vec<T>, DaeConstructionError> {
    raw.iter().map(|id| mapped(ids, *id, kind, at)).collect()
}

pub(super) fn expect_ordinal(
    kind: &'static str,
    expected: usize,
    found: u32,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if usize::try_from(found) == Ok(expected) {
        return Ok(());
    }
    Err(duplicate(kind, found, at))
}

pub(super) fn invalid_arity(
    expected: usize,
    found: usize,
    at: DaeProvenance,
) -> DaeConstructionError {
    DaeConstructionError::InvalidArity {
        expected,
        found,
        span: at.span(),
    }
}
