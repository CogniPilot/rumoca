use super::*;

/// GPU preparation deliberately accepts only direct, regular initial families.
/// It builds one base row plus one corner per binder, never a vector of scalar
/// rows. Runtime initialization keeps its complete scalar/general path.
pub(super) fn lower_gpu_initialization_system(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<solve::InitializationSolveSystem, LowerError> {
    if dae_model.initialization.equations.is_empty() {
        return Ok(solve::InitializationSolveSystem::default());
    }
    let mut expected = 0usize;
    let mut nodes = Vec::new();
    let mut families = Vec::new();
    let mut residual_start = 0usize;
    for family in &dae_model.initialization.structured_equations {
        let Some(_regular) = family.regular.as_ref() else {
            return Err(gpu_initial_unsupported(
                "GPU initial projection requires a regular structured initial family",
                family.span,
            ));
        };
        let Some(template) = family.template.as_ref() else {
            return Err(gpu_initial_unsupported(
                "GPU initial projection requires a structured initial template",
                family.span,
            ));
        };
        let Some(body_count) = family.common_iteration_equation_count() else {
            return Err(gpu_initial_unsupported(
                "GPU initial projection requires a nonempty uniform structured initial family",
                family.span,
            ));
        };
        if body_count == 0 || template.body.len() != body_count {
            return Err(gpu_initial_unsupported(
                "GPU initial projection requires one uniform template body per family cell",
                family.span,
            ));
        }
        let cells = family
            .domain
            .scalar_count()
            .map_err(|error| LowerError::contract_violation(error.to_string(), family.span))?;
        expected = expected
            .checked_add(cells.checked_mul(body_count).ok_or_else(|| {
                LowerError::contract_violation("GPU initial family size overflow", family.span)
            })?)
            .ok_or_else(|| {
                LowerError::contract_violation("GPU initial residual size overflow", family.span)
            })?;
        for position in 0..body_count {
            let direct = lower_gpu_direct_family(
                dae_model,
                layout,
                family,
                position,
                body_count,
                residual_start,
            )?;
            residual_start = residual_start.checked_add(cells).ok_or_else(|| {
                LowerError::contract_violation("GPU initial residual range overflow", family.span)
            })?;
            nodes.push(direct.residual);
            let node_index = nodes.len() - 1;
            let direct = solve::InitializationDirectFamily {
                node_index,
                targets: direct.targets,
                residual_sign: direct.residual_sign,
                span: direct.span,
            };
            families.push(direct);
        }
    }
    let required_user_initial_rows = required_user_initial_rows(dae_model)?;
    if expected != required_user_initial_rows {
        return Err(gpu_initial_unsupported_optional(
            "GPU initial projection requires complete structured coverage; mixed or nonstructured initial rows are unsupported",
            first_uncovered_user_initial_span(dae_model),
        ));
    }
    let (required_target_ranges, fixed_target_ranges) =
        require_complete_gpu_initial_target_coverage(dae_model, layout, &families)?;
    Ok(solve::InitializationSolveSystem {
        residual: solve::ComputeBlock { nodes },
        direct_families: families,
        required_target_ranges,
        fixed_target_ranges,
        ..Default::default()
    })
}

fn required_user_initial_rows(dae_model: &dae::Dae) -> Result<usize, LowerError> {
    if dae_model.initialization.equation_provenance.len()
        != dae_model.initialization.equations.len()
    {
        let span = dae_model
            .initialization
            .equations
            .get(dae_model.initialization.equation_provenance.len())
            .or_else(|| dae_model.initialization.equations.first())
            .map(|equation| equation.span);
        return Err(gpu_initial_unsupported_optional(
            "GPU initial projection requires typed provenance for every initial equation",
            span,
        ));
    }
    dae_model
        .initialization
        .equations
        .iter()
        .zip(&dae_model.initialization.equation_provenance)
        .filter(|(_, provenance)| **provenance != dae::InitializationEquationProvenance::FixedStart)
        .map(|(equation, _)| equation)
        .try_fold(0usize, |total, equation| {
            total
                .checked_add(equation.scalar_count.max(1))
                .ok_or_else(|| {
                    LowerError::contract_violation(
                        "GPU initial user-row count overflow",
                        equation.span,
                    )
                })
        })
}

fn first_uncovered_user_initial_span(dae_model: &dae::Dae) -> Option<rumoca_core::Span> {
    let mut covered = vec![false; dae_model.initialization.equations.len()];
    for family in &dae_model.initialization.structured_equations {
        let equation_len = family.equation_counts.iter().copied().sum::<usize>();
        let end = family
            .first_equation_index
            .saturating_add(equation_len)
            .min(covered.len());
        covered[family.first_equation_index.min(end)..end].fill(true);
    }
    dae_model
        .initialization
        .equations
        .iter()
        .zip(&dae_model.initialization.equation_provenance)
        .enumerate()
        .find(|(index, (_, provenance))| {
            **provenance != dae::InitializationEquationProvenance::FixedStart && !covered[*index]
        })
        .or_else(|| {
            dae_model
                .initialization
                .equations
                .iter()
                .zip(&dae_model.initialization.equation_provenance)
                .enumerate()
                .find(|(_, (_, provenance))| {
                    **provenance != dae::InitializationEquationProvenance::FixedStart
                })
        })
        .map(|(_, (equation, _))| equation.span)
}

fn gpu_initial_unsupported(reason: impl Into<String>, span: rumoca_core::Span) -> LowerError {
    LowerError::UnsupportedAt {
        reason: reason.into(),
        contexts: Vec::new(),
        span,
    }
}

fn gpu_initial_unsupported_optional(
    reason: impl Into<String>,
    span: Option<rumoca_core::Span>,
) -> LowerError {
    let reason = reason.into();
    match span {
        Some(span) => gpu_initial_unsupported(reason, span),
        None => LowerError::Unsupported { reason },
    }
}

fn require_complete_gpu_initial_target_coverage(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    families: &[solve::InitializationDirectFamily],
) -> Result<
    (
        Vec<solve::InitializationTargetRange>,
        Vec<solve::InitializationTargetRange>,
    ),
    LowerError,
> {
    let mut direct_ranges = Vec::with_capacity(families.len());
    for (structured, direct) in dae_model
        .initialization
        .structured_equations
        .iter()
        .flat_map(|structured| {
            (0..structured.common_iteration_equation_count().unwrap_or(0)).map(move |_| structured)
        })
        .zip(families)
    {
        let dense =
            solve::TensorOutputMap::dense_contiguous(direct.targets.start, &structured.domain)
                .map_err(|error| {
                    LowerError::contract_violation(format!("{error:?}"), direct.span)
                })?;
        if direct.targets.strides != dense.strides {
            return Err(LowerError::contract_violation(
                "GPU initial target map must be dense and contiguous",
                direct.span,
            ));
        }
        let count = structured
            .domain
            .scalar_count()
            .map_err(|error| LowerError::contract_violation(error.to_string(), direct.span))?;
        let end = direct.targets.start.checked_add(count).ok_or_else(|| {
            LowerError::contract_violation("GPU initial target range overflow", direct.span)
        })?;
        direct_ranges.push(solve::InitializationTargetRange {
            start: direct.targets.start,
            end,
            span: Some(direct.span),
        });
    }
    let mut fixed_ranges = Vec::new();
    for (equation, provenance) in dae_model
        .initialization
        .equations
        .iter()
        .zip(&dae_model.initialization.equation_provenance)
    {
        if *provenance != dae::InitializationEquationProvenance::FixedStart {
            continue;
        }
        let target = lower_contiguous_y_target_range_for_equation(dae_model, equation, layout)?;
        fixed_ranges.push(solve::InitializationTargetRange {
            start: target.start,
            end: target.end,
            span: Some(equation.span),
        });
    }
    let fixed_ranges = normalize_gpu_target_ranges(fixed_ranges, layout.y_scalars())?;
    direct_ranges.extend(fixed_ranges.iter().copied());
    let actual = normalize_gpu_target_ranges(direct_ranges, layout.y_scalars())?;
    let required = if layout.y_scalars() == 0 {
        Vec::new()
    } else {
        vec![solve::InitializationTargetRange {
            start: 0,
            end: layout.y_scalars(),
            span: actual.first().and_then(|range| range.span),
        }]
    };
    if !same_gpu_target_coverage(&actual, &required) {
        let span = actual
            .first()
            .and_then(|range| range.span)
            .or_else(|| required.first().and_then(|range| range.span));
        return Err(gpu_target_range_error(
            "GPU initial projection requires the union of user equations and fixed starts to cover every solver Y slot",
            span,
        ));
    }
    Ok((required, fixed_ranges))
}

pub(super) fn normalize_gpu_target_ranges(
    mut ranges: Vec<solve::InitializationTargetRange>,
    upper_bound: usize,
) -> Result<Vec<solve::InitializationTargetRange>, LowerError> {
    ranges.sort_unstable_by_key(|range| (range.start, range.end));
    let mut normalized: Vec<solve::InitializationTargetRange> = Vec::with_capacity(ranges.len());
    for range in ranges {
        if range.start >= range.end || range.end > upper_bound {
            return Err(gpu_target_range_error(
                "GPU initial target range is empty or outside the solver Y vector",
                range.span,
            ));
        }
        if let Some(last) = normalized.last_mut() {
            if range.start < last.end {
                return Err(gpu_target_range_error(
                    "GPU initial target ranges overlap",
                    range.span.or(last.span),
                ));
            }
            if range.start == last.end {
                last.end = range.end;
                continue;
            }
        }
        normalized.push(range);
    }
    Ok(normalized)
}

fn gpu_target_range_error(reason: &'static str, span: Option<rumoca_core::Span>) -> LowerError {
    span.map_or_else(
        || LowerError::Unsupported {
            reason: reason.to_string(),
        },
        |span| LowerError::contract_violation(reason, span),
    )
}

fn same_gpu_target_coverage(
    left: &[solve::InitializationTargetRange],
    right: &[solve::InitializationTargetRange],
) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.start == right.start && left.end == right.end)
}

fn lower_gpu_direct_family(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    family: &dae::StructuredEquationFamily,
    position: usize,
    body_count: usize,
    residual_start: usize,
) -> Result<GpuLoweredDirectFamily, LowerError> {
    let canonical_domain = canonical_gpu_initial_domain(&family.domain, family.span)?;
    let base_cell = gpu_canonical_base_cell_index(&family.domain, family.span)?;
    let base_index = family
        .first_equation_index
        .checked_add(base_cell.checked_mul(body_count).ok_or_else(|| {
            LowerError::contract_violation("GPU initial base equation index overflow", family.span)
        })?)
        .and_then(|value| value.checked_add(position))
        .ok_or_else(|| {
            LowerError::contract_violation("GPU initial base equation index overflow", family.span)
        })?;
    let base_equation = dae_model
        .initialization
        .equations
        .get(base_index)
        .ok_or_else(|| {
            LowerError::contract_violation("GPU initial base equation is missing", family.span)
        })?;
    let base_ops = lower_initial_residual_cell(
        dae_model,
        layout,
        dae_model.continuous.equations.len() + base_index,
        base_equation,
    )?;
    let base_target = direct_initial_target(dae_model, layout, base_equation, family.span)?;
    reject_nondeterministic_gpu_initial_ops(&base_ops, base_equation.span)?;
    let sign = direct_initial_assignment_sign(&base_ops, base_target).ok_or_else(|| {
        gpu_initial_unsupported(
            "GPU initial projection requires a direct target-minus-rhs structured row",
            base_equation.span,
        )
    })?;
    let strides = lower_gpu_direct_family_strides(
        dae_model,
        layout,
        family,
        position,
        body_count,
        GpuDirectFamilyBase {
            equation: base_equation,
            ops: &base_ops,
            target: base_target,
        },
    )?;
    prove_gpu_direct_family_affine(
        dae_model,
        layout,
        family,
        position,
        body_count,
        GpuDirectFamilyBase {
            equation: base_equation,
            ops: &base_ops,
            target: base_target,
        },
        &strides,
    )?;
    Ok(GpuLoweredDirectFamily {
        residual: solve::ComputeNode::Map {
            domain: canonical_domain.clone(),
            output_map: solve::TensorOutputMap::dense_contiguous(residual_start, &canonical_domain)
                .map_err(|error| {
                    LowerError::contract_violation(format!("{error:?}"), family.span)
                })?,
            base_ops,
            load_strides: strides.loads,
            const_strides: strides.constants,
            metadata: solve::TensorNodeMetadata::default(),
            span: family.span,
        },
        targets: solve::TensorOutputMap {
            start: base_target,
            strides: strides.targets,
        },
        residual_sign: sign,
        span: family.span,
    })
}

struct GpuLoweredDirectFamily {
    residual: solve::ComputeNode,
    targets: solve::TensorOutputMap,
    residual_sign: i8,
    span: rumoca_core::Span,
}

struct GpuDirectFamilyStrides {
    loads: Vec<solve::AffineStencilLoadStride>,
    constants: Vec<solve::AffineStencilConstStride>,
    targets: Vec<solve::AffineStencilIndexStrideTerm>,
}

struct GpuDirectFamilyBase<'a> {
    equation: &'a dae::Equation,
    ops: &'a [solve::LinearOp],
    target: usize,
}

struct GpuDirectFamilyProof<'a> {
    dae_model: &'a dae::Dae,
    layout: &'a solve::VarLayout,
    family: &'a dae::StructuredEquationFamily,
    base: GpuDirectFamilyBase<'a>,
    strides: &'a GpuDirectFamilyStrides,
}

fn lower_gpu_direct_family_strides(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    family: &dae::StructuredEquationFamily,
    position: usize,
    body_count: usize,
    base: GpuDirectFamilyBase<'_>,
) -> Result<GpuDirectFamilyStrides, LowerError> {
    let mut load_strides = Vec::new();
    let mut const_strides = Vec::new();
    let mut target_strides = Vec::new();
    for (dimension, binder) in family.domain.binders.iter().enumerate() {
        if gpu_binder_value_count(binder, family.span)? == 1 {
            continue;
        }
        let corner_index = gpu_direct_family_corner_index(family, position, body_count, dimension)?;
        let corner_equation = dae_model
            .initialization
            .equations
            .get(corner_index)
            .ok_or_else(|| {
                LowerError::contract_violation(
                    "GPU initial corner equation is missing",
                    family.span,
                )
            })?;
        let corner_ops = lower_initial_residual_cell(
            dae_model,
            layout,
            dae_model.continuous.equations.len() + corner_index,
            corner_equation,
        )?;
        if !stencil::dae_equation_body_shapes_match(base.equation, corner_equation)? {
            return Err(gpu_initial_unsupported(
                "GPU initial projection requires identical conservative equation body shapes",
                corner_equation.span,
            ));
        }
        let corner_target = direct_initial_target(dae_model, layout, corner_equation, family.span)?;
        target_strides.push(solve::AffineStencilIndexStrideTerm {
            dimension,
            stride: gpu_initial_stride(corner_target, base.target, family.span, "target")?,
        });
        append_gpu_corner_strides(
            base.ops,
            &corner_ops,
            dimension,
            &mut load_strides,
            &mut const_strides,
            family.span,
        )?;
    }
    Ok(GpuDirectFamilyStrides {
        loads: load_strides,
        constants: const_strides,
        targets: target_strides,
    })
}

fn prove_gpu_direct_family_affine(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    family: &dae::StructuredEquationFamily,
    position: usize,
    body_count: usize,
    base: GpuDirectFamilyBase<'_>,
    strides: &GpuDirectFamilyStrides,
) -> Result<(), LowerError> {
    let proof = GpuDirectFamilyProof {
        dae_model,
        layout,
        family,
        base,
        strides,
    };
    if !family.interiors_materialized {
        return Err(gpu_initial_unsupported(
            "GPU initial projection cannot prove affine direct-family values without materialized interiors",
            family.span,
        ));
    }
    let tuples = family
        .domain
        .index_tuples()
        .map_err(|error| LowerError::contract_violation(error.to_string(), family.span))?;
    let mut equations = Vec::with_capacity(tuples.len());
    for cell in 0..tuples.len() {
        let equation_index = family
            .first_equation_index
            .checked_add(cell.checked_mul(body_count).ok_or_else(|| {
                LowerError::contract_violation("GPU initial proof row index overflow", family.span)
            })?)
            .and_then(|index| index.checked_add(position))
            .ok_or_else(|| {
                LowerError::contract_violation("GPU initial proof row index overflow", family.span)
            })?;
        let equation = dae_model
            .initialization
            .equations
            .get(equation_index)
            .ok_or_else(|| {
                LowerError::contract_violation(
                    "GPU initial affine proof equation is missing",
                    family.span,
                )
            })?;
        equations.push((
            dae_model.continuous.equations.len() + equation_index,
            equation,
        ));
    }
    let rows = lower::lower_initial_residual_cells(
        dae_model,
        layout,
        equations
            .iter()
            .map(|(index, equation)| (*index, *equation)),
    )?;
    if rows.len() != equations.len() {
        return Err(LowerError::contract_violation(
            "GPU initial affine proof must lower one residual row per family cell",
            family.span,
        ));
    }
    for (((_, equation), ops), tuple) in equations.iter().zip(&rows).zip(&tuples) {
        prove_gpu_direct_family_cell(&proof, tuple, equation, ops)?;
    }
    Ok(())
}

fn prove_gpu_direct_family_cell(
    proof: &GpuDirectFamilyProof<'_>,
    tuple: &[i64],
    equation: &dae::Equation,
    ops: &[solve::LinearOp],
) -> Result<(), LowerError> {
    if !stencil::dae_equation_body_shapes_match(proof.base.equation, equation)? {
        return Err(gpu_initial_unsupported(
            "GPU initial projection requires identical conservative equation body shapes",
            equation.span,
        ));
    }
    let ordinals = gpu_canonical_ordinals(&proof.family.domain, tuple, equation.span)?;
    reject_nondeterministic_gpu_initial_ops(ops, equation.span)?;
    prove_gpu_affine_ops(proof.base.ops, ops, &ordinals, proof.strides, equation.span)?;
    let target = direct_initial_target(proof.dae_model, proof.layout, equation, equation.span)?;
    let expected = affine_gpu_target(
        proof.base.target,
        &ordinals,
        &proof.strides.targets,
        equation.span,
    )?;
    if target != expected {
        return Err(gpu_initial_unsupported(
            "GPU initial projection target is not affine across the complete family domain",
            equation.span,
        ));
    }
    Ok(())
}

fn gpu_canonical_ordinals(
    domain: &rumoca_core::StructuredIndexDomain,
    tuple: &[i64],
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    domain
        .binders
        .iter()
        .zip(tuple)
        .map(|(binder, value)| {
            let lower = binder.lower.min(binder.upper);
            let distance = value.checked_sub(lower).ok_or_else(|| {
                LowerError::contract_violation("GPU initial proof tuple is out of bounds", span)
            })?;
            let step = i64::try_from(binder.step.unsigned_abs()).map_err(|_| {
                LowerError::contract_violation("GPU initial proof step exceeds host range", span)
            })?;
            usize::try_from(distance / step).map_err(|_| {
                LowerError::contract_violation("GPU initial proof ordinal exceeds host range", span)
            })
        })
        .collect()
}

fn prove_gpu_affine_ops(
    base: &[solve::LinearOp],
    actual: &[solve::LinearOp],
    ordinals: &[usize],
    strides: &GpuDirectFamilyStrides,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if base.len() != actual.len() {
        return Err(gpu_initial_unsupported(
            "GPU initial projection operation shape is not uniform across the complete family domain",
            span,
        ));
    }
    for (position, (base_op, actual_op)) in base.iter().zip(actual).enumerate() {
        if !gpu_affine_op_matches(position, base_op, actual_op, ordinals, strides, span)? {
            return Err(gpu_initial_unsupported(
                "GPU initial projection operation values are not affine across the complete family domain",
                span,
            ));
        }
    }
    Ok(())
}

fn gpu_affine_op_matches(
    position: usize,
    base: &solve::LinearOp,
    actual: &solve::LinearOp,
    ordinals: &[usize],
    strides: &GpuDirectFamilyStrides,
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    match (base, actual) {
        (
            solve::LinearOp::LoadY {
                dst: base_dst,
                index: base,
            },
            solve::LinearOp::LoadY {
                dst: actual_dst,
                index: actual,
            },
        )
        | (
            solve::LinearOp::LoadP {
                dst: base_dst,
                index: base,
            },
            solve::LinearOp::LoadP {
                dst: actual_dst,
                index: actual,
            },
        ) => Ok(base_dst == actual_dst
            && affine_gpu_index(*base, position, ordinals, &strides.loads, span)? == *actual),
        (
            solve::LinearOp::Const {
                dst: base_dst,
                value: base,
            },
            solve::LinearOp::Const {
                dst: actual_dst,
                value: actual,
            },
        ) => Ok(base_dst == actual_dst
            && affine_gpu_constant(*base, position, ordinals, &strides.constants, span)?.to_bits()
                == actual.to_bits()),
        _ => Ok(base == actual),
    }
}

fn affine_gpu_index(
    base: usize,
    position: usize,
    ordinals: &[usize],
    strides: &[solve::AffineStencilLoadStride],
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let offset = strides
        .iter()
        .filter(|stride| stride.op_position == position)
        .flat_map(|stride| &stride.terms)
        .try_fold(0isize, |total, term| {
            let ordinal = isize::try_from(ordinals[term.dimension]).ok()?;
            total.checked_add(term.stride.checked_mul(ordinal)?)
        })
        .ok_or_else(|| {
            LowerError::contract_violation("GPU initial affine index overflows", span)
        })?;
    base.checked_add_signed(offset)
        .ok_or_else(|| LowerError::contract_violation("GPU initial affine index overflows", span))
}

fn affine_gpu_constant(
    base: f64,
    position: usize,
    ordinals: &[usize],
    strides: &[solve::AffineStencilConstStride],
    span: rumoca_core::Span,
) -> Result<f64, LowerError> {
    let value = strides
        .iter()
        .filter(|stride| stride.op_position == position)
        .flat_map(|stride| &stride.terms)
        .fold(base, |value, term| {
            value + term.stride * ordinals[term.dimension] as f64
        });
    value.is_finite().then_some(value).ok_or_else(|| {
        LowerError::contract_violation("GPU initial affine constant is not finite", span)
    })
}

fn affine_gpu_target(
    base: usize,
    ordinals: &[usize],
    strides: &[solve::AffineStencilIndexStrideTerm],
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let offset = strides.iter().try_fold(0isize, |total, term| {
        let ordinal = isize::try_from(ordinals[term.dimension]).ok()?;
        total.checked_add(term.stride.checked_mul(ordinal)?)
    });
    offset
        .and_then(|offset| base.checked_add_signed(offset))
        .ok_or_else(|| LowerError::contract_violation("GPU initial affine target overflows", span))
}

pub(super) fn reject_nondeterministic_gpu_initial_ops(
    ops: &[solve::LinearOp],
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if ops.iter().any(|op| {
        matches!(
            op,
            solve::LinearOp::RandomInitialState { .. }
                | solve::LinearOp::RandomResult { .. }
                | solve::LinearOp::RandomState { .. }
                | solve::LinearOp::ImpureRandomInit { .. }
                | solve::LinearOp::ImpureRandom { .. }
                | solve::LinearOp::ImpureRandomInteger { .. }
        )
    }) {
        return Err(gpu_initial_unsupported(
            "GPU initial projection rejects random or impure operations because apply and verification must be deterministic",
            span,
        ));
    }
    Ok(())
}

fn gpu_direct_family_corner_index(
    family: &dae::StructuredEquationFamily,
    position: usize,
    body_count: usize,
    dimension: usize,
) -> Result<usize, LowerError> {
    let corner_cell = gpu_corner_cell_index(&family.domain, dimension, family.span)?;
    family
        .first_equation_index
        .checked_add(corner_cell.checked_mul(body_count).ok_or_else(|| {
            LowerError::contract_violation(
                "GPU initial corner equation index overflow",
                family.span,
            )
        })?)
        .and_then(|value| value.checked_add(position))
        .ok_or_else(|| {
            LowerError::contract_violation(
                "GPU initial corner equation index overflow",
                family.span,
            )
        })
}

fn gpu_initial_stride(
    corner: usize,
    base: usize,
    span: rumoca_core::Span,
    kind: &'static str,
) -> Result<isize, LowerError> {
    isize::try_from(corner)
        .ok()
        .and_then(|value| value.checked_sub(isize::try_from(base).ok()?))
        .ok_or_else(|| {
            LowerError::contract_violation(format!("GPU initial {kind} stride overflows"), span)
        })
}

fn direct_initial_target(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    equation: &dae::Equation,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let targets = lower_continuous_row_targets_for_equation(dae_model, equation, layout, 1)?;
    match targets.as_slice() {
        [Some(solve::ScalarSlot::Y { index, .. })] => Ok(*index),
        _ => Err(LowerError::contract_violation(
            "GPU initial projection requires one Y target per direct family row",
            span,
        )),
    }
}

pub(super) fn gpu_corner_cell_index(
    domain: &rumoca_core::StructuredIndexDomain,
    dimension: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let selected = domain.binders.get(dimension).ok_or_else(|| {
        LowerError::contract_violation("GPU initial corner dimension is missing", span)
    })?;
    if gpu_binder_value_count(selected, span)? < 2 {
        return Err(gpu_initial_unsupported(
            "GPU initial projection requires a non-degenerate structured binder",
            span,
        ));
    }
    domain
        .binders
        .iter()
        .enumerate()
        .try_fold(0usize, |ordinal, (index, binder)| {
            let count = gpu_binder_value_count(binder, span)?;
            let coordinate = match (binder.step < 0, index == dimension) {
                (false, false) => 0,
                (false, true) => 1,
                (true, false) => count - 1,
                (true, true) => count - 2,
            };
            ordinal
                .checked_mul(count)
                .and_then(|value| value.checked_add(coordinate))
                .ok_or_else(|| {
                    LowerError::contract_violation("GPU initial corner stride overflow", span)
                })
        })
}

fn gpu_canonical_base_cell_index(
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    domain.binders.iter().try_fold(0usize, |ordinal, binder| {
        let count = gpu_binder_value_count(binder, span)?;
        let coordinate = if binder.step < 0 { count - 1 } else { 0 };
        ordinal
            .checked_mul(count)
            .and_then(|value| value.checked_add(coordinate))
            .ok_or_else(|| {
                LowerError::contract_violation("GPU initial base cell index overflow", span)
            })
    })
}

fn canonical_gpu_initial_domain(
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<rumoca_core::StructuredIndexDomain, LowerError> {
    let mut canonical = domain.clone();
    for binder in &mut canonical.binders {
        if binder.step < 0 {
            std::mem::swap(&mut binder.lower, &mut binder.upper);
            binder.step = binder.step.checked_neg().ok_or_else(|| {
                LowerError::contract_violation("GPU initial binder step overflows", span)
            })?;
        }
    }
    Ok(canonical)
}

fn gpu_binder_value_count(
    binder: &rumoca_core::StructuredIndexBinder,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    if binder.step == 0 {
        return Err(LowerError::contract_violation(
            "GPU initial binder step must be nonzero",
            span,
        ));
    }
    let distance = if binder.step > 0 {
        binder.upper.checked_sub(binder.lower)
    } else {
        binder.lower.checked_sub(binder.upper)
    }
    .ok_or_else(|| LowerError::contract_violation("GPU initial binder bounds are invalid", span))?;
    let step = binder.step.unsigned_abs();
    let count = distance
        .checked_div(i64::try_from(step).map_err(|_| {
            LowerError::contract_violation("GPU initial binder step overflow", span)
        })?)
        .and_then(|value| value.checked_add(1))
        .ok_or_else(|| LowerError::contract_violation("GPU initial binder count overflow", span))?;
    usize::try_from(count).map_err(|_| {
        LowerError::contract_violation("GPU initial binder count exceeds host range", span)
    })
}

pub(super) fn append_gpu_corner_strides(
    base: &[solve::LinearOp],
    corner: &[solve::LinearOp],
    dimension: usize,
    load_strides: &mut Vec<solve::AffineStencilLoadStride>,
    const_strides: &mut Vec<solve::AffineStencilConstStride>,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if base.len() != corner.len() {
        return Err(gpu_initial_unsupported(
            "GPU initial projection requires identical direct-family operation shapes",
            span,
        ));
    }
    for (op_position, (base_op, corner_op)) in base.iter().zip(corner).enumerate() {
        match (base_op, corner_op) {
            (
                solve::LinearOp::LoadY {
                    dst: base_dst,
                    index: base,
                },
                solve::LinearOp::LoadY {
                    dst: corner_dst,
                    index: corner,
                },
            ) if base_dst == corner_dst => {
                let stride = isize::try_from(*corner)
                    .ok()
                    .and_then(|value| value.checked_sub(isize::try_from(*base).ok()?))
                    .ok_or_else(|| {
                        LowerError::contract_violation("GPU initial Y stride overflows", span)
                    })?;
                if stride != 0 {
                    load_strides.push(solve::AffineStencilLoadStride {
                        op_position,
                        terms: vec![solve::AffineStencilIndexStrideTerm { dimension, stride }],
                    });
                }
            }
            (
                solve::LinearOp::LoadP {
                    dst: base_dst,
                    index: base,
                },
                solve::LinearOp::LoadP {
                    dst: corner_dst,
                    index: corner,
                },
            ) if base_dst == corner_dst => {
                let stride = isize::try_from(*corner)
                    .ok()
                    .and_then(|value| value.checked_sub(isize::try_from(*base).ok()?))
                    .ok_or_else(|| {
                        LowerError::contract_violation("GPU initial P stride overflows", span)
                    })?;
                if stride != 0 {
                    load_strides.push(solve::AffineStencilLoadStride {
                        op_position,
                        terms: vec![solve::AffineStencilIndexStrideTerm { dimension, stride }],
                    });
                }
            }
            (
                solve::LinearOp::Const {
                    dst: base_dst,
                    value: base,
                },
                solve::LinearOp::Const {
                    dst: corner_dst,
                    value: corner,
                },
            ) if base_dst == corner_dst => {
                let stride = corner - base;
                if !stride.is_finite() {
                    return Err(LowerError::contract_violation(
                        "GPU initial constant stride is not finite",
                        span,
                    ));
                }
                if stride != 0.0 {
                    const_strides.push(solve::AffineStencilConstStride {
                        op_position,
                        terms: vec![solve::AffineStencilConstStrideTerm { dimension, stride }],
                    });
                }
            }
            (
                solve::LinearOp::LoadY { .. }
                | solve::LinearOp::LoadP { .. }
                | solve::LinearOp::Const { .. },
                _,
            ) => {
                return Err(gpu_initial_unsupported(
                    "GPU initial projection requires uniform direct-family access kinds",
                    span,
                ));
            }
            _ if base_op == corner_op => {}
            _ => {
                return Err(gpu_initial_unsupported(
                    "GPU initial projection requires every non-affine operation and destination register to match exactly",
                    span,
                ));
            }
        }
    }
    Ok(())
}

fn direct_initial_assignment_sign(ops: &[solve::LinearOp], target_index: usize) -> Option<i8> {
    let solve::LinearOp::StoreOutput { src } = ops.last()? else {
        return None;
    };
    let solve::LinearOp::Binary {
        op: solve::BinaryOp::Sub,
        lhs,
        rhs,
        dst,
    } = ops
        .iter()
        .find(|op| matches!(op, solve::LinearOp::Binary { dst, .. } if dst == src))?
    else {
        return None;
    };
    let target_loads = ops
        .iter()
        .filter_map(|op| match op {
            solve::LinearOp::LoadY { dst, index } if *index == target_index => Some(*dst),
            solve::LinearOp::LoadY { .. } => Some(u32::MAX),
            _ => None,
        })
        .collect::<Vec<_>>();
    if target_loads.len() != 1 || target_loads[0] == u32::MAX || *dst != *src {
        return None;
    }
    let residual_sign = if target_loads[0] == *lhs {
        1
    } else if target_loads[0] == *rhs {
        -1
    } else {
        return None;
    };
    Some(residual_sign)
}
