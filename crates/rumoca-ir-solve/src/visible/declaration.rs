use super::*;

pub(super) fn check_declaration(
    problem: &SolveProblem,
    source: &VisibleDeclarationInput,
) -> Result<usize, VisibleProjectionError> {
    let span = Some(source.attributes.source_span);
    source
        .attributes
        .source_span
        .require_provenance("visible declaration")
        .map_err(|error_value| error(error_value.to_string(), span))?;
    let declaration = problem
        .solve_layout
        .variable_declarations
        .get(source.declaration)
        .ok_or_else(|| error("foreign declaration ordinal", span))?;
    let run = problem
        .solve_layout
        .variable_storage_runs
        .get(source.declaration)
        .ok_or_else(|| error("missing original storage run", span))?;
    if declaration.role() != run.role || declaration.value_kind() != run.value_kind {
        return Err(error(
            "declaration role or type differs from original storage",
            span,
        ));
    }
    if matches!(
        run.role,
        crate::SolveVariableStorageRole::Parameter | crate::SolveVariableStorageRole::Constant
    ) || run.value_kind == crate::SolveVariableValueKind::String
    {
        return Err(error("declaration has no visible numeric role", span));
    }
    let logical = problem
        .layout
        .shape(source.binding.as_str())
        .map_or(Some(1), |shape| {
            shape
                .iter()
                .try_fold(1usize, |count, dim| count.checked_mul(*dim))
        })
        .ok_or_else(|| error("shape extent overflow", span))?;
    if logical != run.scalar_count {
        return Err(error("shape differs from original storage extent", span));
    }
    if logical != 0 {
        let base = problem
            .layout
            .bindings()
            .get(&source.binding)
            .ok_or_else(|| error("missing original layout binding", span))?;
        if *base != run.base || problem.layout.has_indexed_shape_binding(&source.binding) {
            return Err(error(
                "binding differs from original contiguous allocation",
                span,
            ));
        }
    }
    if logical != 0
        && [0, logical - 1].into_iter().any(|point| {
            problem
                .layout
                .scalar_declaration_name(&source.binding, point)
                .is_none()
        })
    {
        return Err(error(
            "visible declaration has no checked name projection",
            span,
        ));
    }
    if let Some(original) = problem.layout.shape_span(source.binding.as_str())
        && original != source.attributes.source_span
    {
        return Err(error("declaration has a foreign shape span", span));
    }
    check_storage_range(problem, run, span)?;
    validate_fact_ordinals(
        source.state_coordinates.iter().map(|fact| fact.0),
        logical,
        span,
    )?;
    validate_fact_ordinals(source.phasors.iter().map(|fact| fact.0), logical, span)?;
    Ok(logical)
}

fn check_storage_range(
    problem: &SolveProblem,
    run: &crate::SolveVariableStorageRun,
    span: Option<Span>,
) -> Result<(), VisibleProjectionError> {
    let (index, byte_offset, capacity) = match run.base {
        ScalarSlot::Y { index, byte_offset } => (index, byte_offset, problem.layout.y_scalars()),
        ScalarSlot::P { index, byte_offset } => (index, byte_offset, problem.layout.p_scalars()),
        _ => return Err(error("visible declaration has no writable storage", span)),
    };
    if index.checked_mul(crate::layout::F64_BYTES) != Some(byte_offset)
        || index
            .checked_add(run.scalar_count)
            .is_none_or(|end| end > capacity)
    {
        return Err(error(
            "visible declaration has foreign byte offset or storage extent",
            span,
        ));
    }
    Ok(())
}
