use super::*;

pub(crate) fn check_provenance(
    source_map: &SourceMap,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let span = provenance.span();
    let Some((_, source)) = source_map.get_source(span.source) else {
        return Err(DaeConstructionError::UnknownSource { span });
    };
    let range = span.start.0..span.end.0;
    if range.start > range.end
        || range.end > source.len()
        || !source.is_char_boundary(range.start)
        || !source.is_char_boundary(range.end)
    {
        return Err(DaeConstructionError::InvalidSourceRange {
            span,
            source_len: source.len(),
        });
    }
    Ok(())
}

pub(crate) fn check_type_capacity(
    ty: &ValueType,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if ty.is_record() || ty.scalar_count().is_some() {
        return Ok(());
    }
    Err(DaeConstructionError::CapacityExceeded {
        arena: "value type scalar layout",
        attempted_index: usize::MAX,
        span: at.span(),
    })
}

pub(crate) fn checked_u32(
    value: usize,
    arena: &'static str,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    u32::try_from(value).map_err(|_| DaeConstructionError::CapacityExceeded {
        arena,
        attempted_index: value,
        span: at.span(),
    })
}

pub(crate) fn function_definition_rhs<'dae>(
    storage: &Storage,
    value: FunctionValueId<'dae>,
    definition: FunctionDefinitionId<'dae>,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    if definition.function() != value.function() {
        return Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: Some(value.function().index()),
            found_function: definition.function().index(),
            span: provenance.span(),
        });
    }
    let entry = function_definition_entry(storage, definition, provenance)?;
    if entry.target != value.ordinal() {
        return Err(DaeConstructionError::InvalidFunctionValueRead {
            value: value.ordinal(),
            expected_definition: None,
            found_definition: definition.ordinal(),
            span: provenance.span(),
        });
    }
    Ok(ExprId::from_raw(entry.rhs))
}

pub(crate) fn unknown(
    kind: &'static str,
    index: u32,
    at: DaeProvenance,
) -> DaeConstructionError {
    DaeConstructionError::UnknownId {
        kind,
        index,
        span: at.span(),
    }
}

pub(crate) fn invalid_arity(
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

pub(crate) fn duplicate(
    kind: &'static str,
    index: u32,
    at: DaeProvenance,
) -> DaeConstructionError {
    DaeConstructionError::DuplicateDefinition {
        kind,
        index,
        span: at.span(),
    }
}

pub(crate) fn incomplete(
    kind: &'static str,
    index: usize,
    at: DaeProvenance,
) -> DaeConstructionError {
    DaeConstructionError::IncompleteDefinition {
        kind,
        index: u32::try_from(index)
            .expect("a decoded DAE arena cannot exceed addressable u32 capacity"),
        span: at.span(),
    }
}
