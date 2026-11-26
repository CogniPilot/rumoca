//! Lower an admitted continuous algebraic family as one call program
//! (SPEC_0043 §6c, SOLVE-C70).

use super::*;
use typed_functions::AlgebraicFamilyForm;

/// Retain one structured algebraic family as an affine `Map` residual.
///
/// The scalar compiler remains authoritative for each domain point. The map
/// is admitted only when all points are algebraic rows and their complete
/// programs prove one affine shape, including parameter and input strides.
/// This preserves source loops such as dense neural-network layers through
/// Solve IR so native backends can emit one bounded outer loop.
pub(super) fn lower_algebraic_family_map<'dae>(
    context: ContinuousContext<'_, 'dae>,
    output: &mut ContinuousOutput,
    first_row: usize,
    family: dae::StructuredFamilyView<'dae>,
) -> Result<bool, LowerError> {
    if family.bodies().len() != 1 {
        return Ok(false);
    }
    let checked_domain = context.view.domain(family.domain()).ok_or_else(|| {
        LowerError::contract(
            "structured algebraic family domain does not resolve",
            family.provenance().span(),
        )
    })?;
    let domain = checked_domain.structured();
    let points = domain.index_tuples().map_err(|error| {
        LowerError::contract(
            format!("structured algebraic domain is invalid: {error}"),
            family.provenance().span(),
        )
    })?;
    let span = family.provenance().span();
    let body = family
        .bodies()
        .get(0)
        .ok_or_else(|| LowerError::contract("structured algebraic family has no body", span))?;
    let mut has_sum_reduction = false;
    dae::for_each_expression(context.view, body, |_, expression| {
        has_sum_reduction |= matches!(
            expression.operation(),
            dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::Sum,
                ..
            }
        );
    });
    if !has_sum_reduction {
        // Ordinary structured algebraic families retain their synthesized
        // call owner (SOLVE-C70). A source sum identifies the contraction
        // families whose authored loops must remain compact for GPU codegen.
        return Ok(false);
    }
    let mut programs = Vec::with_capacity(points.len());
    for (point, values) in points.iter().enumerate() {
        let row = checked_ordinal_add(first_row, point, "continuous row ordinal overflow", span)?;
        if !matches!(
            context.matching.get(&row),
            Some(UnknownId::Algebraic { .. })
        ) {
            return Ok(false);
        }
        let scalar = family
            .scalar_view()
            .body_scalar(point, checked_domain.extents())
            .ok_or_else(|| {
                LowerError::contract("structured algebraic scalar view overflow", span)
            })?;
        let affine_derivatives = context.affine_derivatives.ok_or_else(|| {
            LowerError::contract("continuous context has no affine derivative systems", span)
        })?;
        let program = ScalarCompiler::new(
            context.view,
            context.layout,
            Some((family.domain(), values)),
        )
        .with_function_conditional_owners(context.function_conditional_owners)
        .with_derivative_definitions(context.derivatives)
        .with_affine_derivative_systems(affine_derivatives)
        .program(body, scalar)?;
        if program.iter().any(|operation| {
            matches!(
                operation,
                solve::LinearOp::Compare { .. }
                    | solve::LinearOp::Select { .. }
                    | solve::LinearOp::FunctionConditional { .. }
                    | solve::LinearOp::LoadIndexedRegister { .. }
                    | solve::LinearOp::PureCall { .. }
            )
        }) {
            // An affine certificate over the operation stream cannot prove
            // that a runtime-dependent selection keeps the same branch for
            // every parameter value. Preserve the checked call owner instead.
            return Ok(false);
        }
        programs.push(program);
    }
    let Some(base_point) = points.first() else {
        return Ok(false);
    };
    let Ok((base_ops, load_strides, const_strides)) =
        events::structured::derive_affine_program_certificate(
            domain, base_point, &points, &programs, span,
        )
    else {
        return Ok(false);
    };
    let output_map = solve::TensorOutputMap::dense_contiguous(first_row, domain)
        .map_err(|_| LowerError::contract("structured residual output map overflow", span))?;
    output.residual.push_tensor(solve::ComputeNode::Map {
        domain: domain.clone(),
        output_map,
        base_ops,
        load_strides,
        const_strides,
        metadata: solve::TensorNodeMetadata::default(),
        span,
    })?;
    Ok(true)
}

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
