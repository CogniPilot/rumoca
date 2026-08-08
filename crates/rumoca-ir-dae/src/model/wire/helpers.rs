use super::*;

pub(super) fn map_expression_operands<'dae>(
    wire: &StorageWire,
    ids: &mut WireIds<'dae>,
    count: u32,
    at: DaeProvenance,
) -> Result<Vec<ExprId<'dae>>, DaeConstructionError> {
    let operands = wire_operands(wire, &mut ids.next_operand, count)?;
    map_many(&ids.expressions, operands, "expression", at)
}

pub(super) fn wire_operands<'wire>(
    wire: &'wire StorageWire,
    cursor: &mut usize,
    count: u32,
) -> Result<&'wire [u32], DaeConstructionError> {
    take_packed(
        &wire.expressions.operands,
        cursor,
        count,
        "expressions.operands",
    )
}

pub(super) fn take_packed<'wire, T>(
    buffer: &'wire [T],
    cursor: &mut usize,
    count: u32,
    column: &'static str,
) -> Result<&'wire [T], DaeConstructionError> {
    let count =
        usize::try_from(count).map_err(|_| DaeConstructionError::MalformedWire { column })?;
    let end = cursor
        .checked_add(count)
        .ok_or(DaeConstructionError::MalformedWire { column })?;
    let values = buffer
        .get(*cursor..end)
        .ok_or(DaeConstructionError::MalformedWire { column })?;
    *cursor = end;
    Ok(values)
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
