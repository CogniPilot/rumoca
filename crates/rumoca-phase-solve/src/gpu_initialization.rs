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
            return Err(LowerError::Unsupported {
                reason: "GPU initial projection requires a regular structured initial family"
                    .to_string(),
            });
        };
        let Some(template) = family.template.as_ref() else {
            return Err(LowerError::Unsupported {
                reason: "GPU initial projection requires a structured initial template".to_string(),
            });
        };
        let Some(body_count) = family.common_iteration_equation_count() else {
            return Err(LowerError::Unsupported {
                reason:
                    "GPU initial projection requires a nonempty uniform structured initial family"
                        .to_string(),
            });
        };
        if body_count == 0 || template.body.len() != body_count {
            return Err(LowerError::Unsupported {
                reason: "GPU initial projection requires one uniform template body per family cell"
                    .to_string(),
            });
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
        return Err(LowerError::Unsupported { reason: "GPU initial projection requires complete structured coverage; mixed or nonstructured initial rows are unsupported".to_string() });
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
        return Err(LowerError::Unsupported {
            reason: "GPU initial projection requires typed provenance for every initial equation"
                .to_string(),
        });
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
        let targets = lower_continuous_row_targets_for_equation(
            dae_model,
            equation,
            layout,
            equation.scalar_count.max(1),
        )?;
        for target in targets {
            let Some(solve::ScalarSlot::Y { index, .. }) = target else {
                return Err(LowerError::contract_violation(
                    "GPU fixed-start initialization requires a Y target",
                    equation.span,
                ));
            };
            let Some(end) = index.checked_add(1) else {
                return Err(LowerError::contract_violation(
                    "GPU fixed-start target range overflow",
                    equation.span,
                ));
            };
            fixed_ranges.push(solve::InitializationTargetRange { start: index, end });
        }
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
        }]
    };
    if actual != required {
        return Err(LowerError::Unsupported {
            reason: "GPU initial projection requires the union of user equations and fixed starts to cover every solver Y slot".to_string(),
        });
    }
    Ok((required, fixed_ranges))
}

fn normalize_gpu_target_ranges(
    mut ranges: Vec<solve::InitializationTargetRange>,
    upper_bound: usize,
) -> Result<Vec<solve::InitializationTargetRange>, LowerError> {
    ranges.sort_unstable_by_key(|range| (range.start, range.end));
    let mut normalized: Vec<solve::InitializationTargetRange> = Vec::with_capacity(ranges.len());
    for range in ranges {
        if range.start >= range.end || range.end > upper_bound {
            return Err(LowerError::Unsupported {
                reason: "GPU initial target range is empty or outside the solver Y vector"
                    .to_string(),
            });
        }
        if let Some(last) = normalized.last_mut()
            && range.start <= last.end
        {
            last.end = last.end.max(range.end);
        } else {
            normalized.push(range);
        }
    }
    Ok(normalized)
}

fn lower_gpu_direct_family(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    family: &dae::StructuredEquationFamily,
    position: usize,
    body_count: usize,
    residual_start: usize,
) -> Result<GpuLoweredDirectFamily, LowerError> {
    let base_index = family
        .first_equation_index
        .checked_add(position)
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
    let sign = direct_initial_assignment_sign(&base_ops, base_target).ok_or_else(|| {
        LowerError::Unsupported {
            reason: "GPU initial projection requires a direct target-minus-rhs structured row"
                .to_string(),
        }
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
    Ok(GpuLoweredDirectFamily {
        residual: solve::ComputeNode::Map {
            domain: family.domain.clone(),
            output_map: solve::TensorOutputMap::dense_contiguous(residual_start, &family.domain)
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
    for dimension in 0..family.domain.binders.len() {
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
            return Err(LowerError::Unsupported {
                reason:
                    "GPU initial projection requires identical conservative equation body shapes"
                        .to_string(),
            });
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
    let binder = domain.binders.get(dimension).ok_or_else(|| {
        LowerError::contract_violation("GPU initial corner dimension is missing", span)
    })?;
    if gpu_binder_value_count(binder, span)? < 2 {
        return Err(LowerError::Unsupported {
            reason: "GPU initial projection requires a non-degenerate structured binder"
                .to_string(),
        });
    }
    domain.binders[dimension + 1..]
        .iter()
        .try_fold(1usize, |stride, later| {
            let count = gpu_binder_value_count(later, span)?;
            stride.checked_mul(count).ok_or_else(|| {
                LowerError::contract_violation("GPU initial corner stride overflow", span)
            })
        })
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
        return Err(LowerError::Unsupported {
            reason: "GPU initial projection requires identical direct-family operation shapes"
                .to_string(),
        });
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
                return Err(LowerError::Unsupported {
                    reason: "GPU initial projection requires uniform direct-family access kinds"
                        .to_string(),
                });
            }
            _ if base_op == corner_op => {}
            _ => {
                return Err(LowerError::Unsupported {
                    reason: "GPU initial projection requires every non-affine operation and destination register to match exactly".to_string(),
                });
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
