//! Lower an admitted continuous algebraic family as one call program
//! (SPEC_0043 §6c, SOLVE-C70).

use super::*;
use typed_functions::AlgebraicFamilyForm;

/// Whether the family was lowered as one call program: its admission proof
/// holds, its owner body has a directional relation, and its rows are matched,
/// in domain order, to the target's scalars.
pub(super) fn lower_algebraic_family_call<'dae>(
    context: ContinuousContext<'_, 'dae>,
    output: &mut ContinuousOutput,
    first_row: usize,
    id: dae::ContinuousFamilyId<'dae>,
    family: dae::StructuredFamilyView<'dae>,
) -> Result<bool, LowerError> {
    let Some(form) = AlgebraicFamilyForm::admit(context.view, family) else {
        return Ok(false);
    };
    let span = form.provenance;
    let rows = first_row
        ..checked_ordinal_add(
            first_row,
            form.scalar_count,
            "continuous row ordinal overflow",
            span,
        )?;
    let compact = has_paired_row_run(context, id, form.target, &rows)?;
    let matched = compact
        || rows.clone().enumerate().all(|(scalar, row)| {
            matches!(
                context.matching.get(&row),
                Some(UnknownId::Algebraic { variable, scalar: matched })
                    if variable == form.target && matched as usize == scalar
            )
        });
    if !matched {
        return Ok(false);
    }
    let Some(program) = ScalarCompiler::new(context.view, context.layout, None)
        .with_function_conditional_owners(context.function_conditional_owners)
        .with_derivative_definitions(context.derivatives)
        .algebraic_family_program(id, &form)?
    else {
        return Ok(false);
    };
    output.residual.push_output_range(program, span, rows);
    Ok(true)
}

fn has_paired_row_run<'dae>(
    context: ContinuousContext<'_, 'dae>,
    source: dae::ContinuousFamilyId<'dae>,
    target: dae::AlgebraicId<'dae>,
    rows: &std::ops::Range<usize>,
) -> Result<bool, LowerError> {
    for stage in context.projection_stages {
        let MatchedProjection::Family(family) = stage else {
            continue;
        };
        if let Some(run) =
            family.declaration_row_run(context.view, context.layout, source, target, rows)?
        {
            if run.rows() != *rows || run.y_range().len() != rows.len() {
                return Err(LowerError::contract(
                    "paired source run disagrees with emitted declaration",
                    first_model_span(context.view),
                ));
            }
            return Ok(true);
        }
    }
    Ok(false)
}
