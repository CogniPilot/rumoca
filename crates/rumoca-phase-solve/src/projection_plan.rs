use super::*;

pub(super) fn lower_algebraic_projection_plan(
    rows: &[Vec<solve::LinearOp>],
    row_targets: &[Option<solve::ScalarSlot>],
    state_scalar_count: usize,
    solver_scalar_count: usize,
    context_span: rumoca_core::Span,
) -> Result<solve::AlgebraicProjectionPlan, LowerError> {
    let projection_count = solver_scalar_count
        .checked_sub(state_scalar_count)
        .ok_or_else(|| {
            lower_contract_violation(
                "algebraic projection range starts after solver scalar count".to_string(),
                context_span,
            )
        })?;
    let mut projection_indices = lower_vec_with_capacity(
        projection_count,
        "algebraic projection index count",
        context_span,
    )?;
    projection_indices.extend(state_scalar_count..solver_scalar_count);
    lower_projection_plan(
        rows,
        row_targets,
        &projection_indices,
        state_scalar_count..solver_scalar_count,
        ProjectionPlanPolicy {
            include_explicit_row_targets: true,
            require_complete_algebraic_coverage: true,
        },
        None,
        context_span,
    )
}

#[derive(Clone, Copy)]
pub(super) struct ProjectionPlanPolicy {
    pub(super) include_explicit_row_targets: bool,
    pub(super) require_complete_algebraic_coverage: bool,
}

// SPEC_0021: Exception - projection planning keeps explicit target,
// initialization incidence, and identity-row ownership decisions together.
#[allow(clippy::excessive_nesting)]
pub(super) fn lower_projection_plan(
    rows: &[Vec<solve::LinearOp>],
    row_targets: &[Option<solve::ScalarSlot>],
    projection_indices: &[usize],
    row_indices: std::ops::Range<usize>,
    policy: ProjectionPlanPolicy,
    implicit_incidence_rows: Option<&BTreeSet<usize>>,
    context_span: rumoca_core::Span,
) -> Result<solve::AlgebraicProjectionPlan, LowerError> {
    let mut row_to_vars = BTreeMap::<usize, BTreeSet<usize>>::new();
    let projection_set = projection_indices.iter().copied().collect::<BTreeSet<_>>();
    let row_indices = row_indices.collect::<Vec<_>>();
    let identity_projection_rows = row_indices
        .iter()
        .filter_map(|row_idx| {
            identity_projection_y_index(rows.get(*row_idx)?.as_slice(), &projection_set)
                .map(|y_idx| (y_idx, *row_idx))
        })
        .collect::<BTreeMap<_, _>>();

    for &row_idx in &row_indices {
        let mut y_indices = if implicit_incidence_rows.is_none_or(|rows| rows.contains(&row_idx)) {
            collect_algebraic_y_indices_for_row(rows[row_idx].as_slice(), &projection_set)
        } else {
            BTreeSet::new()
        };
        let explicit_target = if let Some(solve::ScalarSlot::Y { index, .. }) =
            row_targets.get(row_idx).copied().flatten()
            && projection_set.contains(&index)
        {
            if policy.include_explicit_row_targets {
                y_indices.insert(index);
            } else {
                y_indices.clear();
                y_indices.insert(index);
            }
            true
        } else {
            false
        };
        if !explicit_target {
            if let Some(index) =
                identity_projection_y_index(rows[row_idx].as_slice(), &projection_set)
            {
                y_indices.clear();
                y_indices.insert(index);
            } else {
                y_indices.retain(|index| {
                    projection_index_not_claimed_by_identity(
                        &identity_projection_rows,
                        *index,
                        row_idx,
                    )
                });
            }
        }
        if y_indices.is_empty() {
            continue;
        }
        row_to_vars.insert(row_idx, y_indices);
    }

    let projection_incidence = algebraic_projection_incidence(&row_to_vars, context_span)?;
    let (blocks, dropped_equations) = projection_blt_blocks(&projection_incidence)?;
    let mut blocks =
        lower_blt_projection_blocks(&blocks, row_targets, &projection_incidence, context_span)?;
    if policy.require_complete_algebraic_coverage {
        blocks = retain_dropped_projection_rows(
            blocks,
            &dropped_equations,
            &projection_incidence,
            context_span,
        )?;
        validate_complete_algebraic_projection_plan(
            &blocks,
            rows,
            &row_indices,
            &projection_incidence,
            context_span,
        )?;
    }
    Ok(solve::AlgebraicProjectionPlan { blocks })
}

pub(super) fn projection_index_not_claimed_by_identity(
    identity_projection_rows: &BTreeMap<usize, usize>,
    index: usize,
    row_idx: usize,
) -> bool {
    identity_projection_rows
        .get(&index)
        .is_none_or(|identity_row| *identity_row == row_idx)
}

pub(super) fn identity_projection_y_index(
    row: &[solve::LinearOp],
    projection_set: &BTreeSet<usize>,
) -> Option<usize> {
    let [
        solve::LinearOp::LoadY {
            dst: load_dst,
            index,
        },
        solve::LinearOp::StoreOutput { src },
    ] = row
    else {
        return None;
    };
    (*load_dst == *src && projection_set.contains(index)).then_some(*index)
}

pub(super) fn projection_blt_blocks(
    projection_incidence: &ProjectionIncidence,
) -> Result<(Vec<BltBlock>, Vec<EquationRef>), LowerError> {
    if projection_incidence.incidence.n_eq == 0 && projection_incidence.incidence.n_var == 0 {
        return Ok((Vec::new(), Vec::new()));
    }
    let regular =
        rumoca_phase_structural::maximum_regular_subsystem(&projection_incidence.incidence)
            .map_err(|err| LowerError::Unsupported {
                reason: format!("lower algebraic projection BLT: {err}"),
            })?;
    let blocks =
        rumoca_phase_structural::build_blt_from_incidence(&regular.incidence).map_err(|err| {
            LowerError::Unsupported {
                reason: format!("lower algebraic projection BLT: {err}"),
            }
        })?;
    Ok((blocks, regular.dropped_equations))
}

pub(super) fn retain_dropped_projection_rows(
    mut blocks: Vec<solve::AlgebraicProjectionBlock>,
    dropped_equations: &[EquationRef],
    projection_incidence: &ProjectionIncidence,
    context_span: rumoca_core::Span,
) -> Result<Vec<solve::AlgebraicProjectionBlock>, LowerError> {
    for equation in dropped_equations {
        let row_y_indices = projection_row_y_indices(equation.0, projection_incidence);
        let mut retained = lower_vec_with_capacity(
            blocks.len(),
            "retained algebraic projection block count",
            context_span,
        )?;
        let mut insertion_index = None;
        let mut merged = None;
        for block in blocks {
            if block
                .y_indices
                .iter()
                .any(|index| row_y_indices.contains(index))
            {
                insertion_index.get_or_insert(retained.len());
                merged = Some(match merged {
                    Some(previous) => combine_projection_blocks(previous, block, context_span)?,
                    None => block,
                });
            } else {
                retained.push(block);
            }
        }
        if let (Some(mut block), Some(index)) = (merged, insertion_index) {
            reserve_lower_capacity(
                &mut block.rows,
                1,
                "retained algebraic projection row count",
                context_span,
            )?;
            block.rows.push(equation.0);
            block.rows.sort_unstable();
            block.rows.dedup();
            retained.insert(index, block);
        }
        blocks = retained;
    }
    Ok(blocks)
}

pub(super) fn projection_row_y_indices(
    row: usize,
    projection_incidence: &ProjectionIncidence,
) -> BTreeSet<usize> {
    let Some(position) = projection_incidence
        .incidence
        .equation_refs
        .iter()
        .position(|equation| equation.0 == row)
    else {
        return BTreeSet::new();
    };
    projection_incidence.incidence.eq_unknowns[position]
        .iter()
        .filter_map(|unknown_index| {
            projection_incidence
                .incidence
                .unknown_names
                .get(*unknown_index)
                .and_then(|unknown| projection_y_index(unknown, projection_incidence))
        })
        .collect()
}

pub(super) fn validate_complete_algebraic_projection_plan(
    blocks: &[solve::AlgebraicProjectionBlock],
    rows: &[Vec<solve::LinearOp>],
    expected_rows: &[usize],
    projection_incidence: &ProjectionIncidence,
    context_span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let covered_rows = blocks
        .iter()
        .flat_map(|block| block.rows.iter().copied())
        .collect::<BTreeSet<_>>();
    for row in expected_rows {
        if covered_rows.contains(row) {
            continue;
        }
        let has_projection_incidence = projection_incidence
            .incidence
            .equation_refs
            .iter()
            .any(|equation| equation.0 == *row);
        if has_projection_incidence || statically_nonzero_projection_row(&rows[*row]).is_some() {
            return Err(lower_contract_violation(
                format!("algebraic projection plan omits implicit row {row}"),
                context_span,
            ));
        }
    }

    let covered_y_indices = blocks
        .iter()
        .flat_map(|block| block.y_indices.iter().copied())
        .collect::<BTreeSet<_>>();
    for y_index in &projection_incidence.unknown_y_indices {
        if !covered_y_indices.contains(y_index) {
            return Err(lower_contract_violation(
                format!("algebraic projection plan omits residual target y[{y_index}]"),
                context_span,
            ));
        }
    }
    Ok(())
}

pub(super) fn statically_nonzero_projection_row(row: &[solve::LinearOp]) -> Option<f64> {
    if !row.iter().all(|op| {
        matches!(
            op,
            solve::LinearOp::Const { .. }
                | solve::LinearOp::Move { .. }
                | solve::LinearOp::Unary { .. }
                | solve::LinearOp::Binary { .. }
                | solve::LinearOp::Compare { .. }
                | solve::LinearOp::Select { .. }
                | solve::LinearOp::StoreOutput { .. }
        )
    }) {
        return None;
    }
    let value = rumoca_eval_solve::eval_row_with_context(
        row,
        &[],
        &[],
        0.0,
        rumoca_eval_solve::RowEvalContext::default(),
    )
    .ok()?;
    (value.is_finite() && value != 0.0).then_some(value)
}

pub(super) fn collect_algebraic_y_indices_for_row(
    row: &[solve::LinearOp],
    projection_set: &BTreeSet<usize>,
) -> BTreeSet<usize> {
    let mut defs = BTreeMap::<solve::Reg, RowDefUse>::new();
    let mut outputs = Vec::new();
    for op in row {
        match row_def_use(op) {
            RowDefUseOp::Def { dst, def_use } => {
                defs.insert(dst, def_use);
            }
            RowDefUseOp::Store { src } => outputs.push(src),
        }
    }
    let mut y_indices = BTreeSet::new();
    let mut visited = BTreeSet::new();
    let mut stack = outputs;
    while let Some(reg) = stack.pop() {
        if !visited.insert(reg) {
            continue;
        }
        let Some(def_use) = defs.get(&reg) else {
            continue;
        };
        if let Some(index) = def_use.loaded_y
            && projection_set.contains(&index)
        {
            y_indices.insert(index);
        }
        stack.extend(def_use.inputs.iter().copied());
    }
    y_indices
}

#[derive(Debug)]
pub(super) struct RowDefUse {
    loaded_y: Option<usize>,
    inputs: Vec<solve::Reg>,
}

pub(super) enum RowDefUseOp {
    Def { dst: solve::Reg, def_use: RowDefUse },
    Store { src: solve::Reg },
}

pub(super) fn row_def_use(op: &solve::LinearOp) -> RowDefUseOp {
    use solve::LinearOp as Op;
    match *op {
        Op::Const { dst, .. } | Op::LoadTime { dst } | Op::LoadP { dst, .. } => {
            def_use(dst, None, Vec::new())
        }
        Op::LoadY { dst, index } => def_use(dst, Some(index), Vec::new()),
        Op::LoadSeed { dst, .. } => def_use(dst, None, Vec::new()),
        Op::LoadIndexedP { dst, index, .. } | Op::LoadIndexedSeed { dst, index, .. } => {
            def_use(dst, None, vec![index])
        }
        Op::Move { dst, src } | Op::Unary { dst, arg: src, .. } => def_use(dst, None, vec![src]),
        Op::Binary { dst, lhs, rhs, .. } | Op::Compare { dst, lhs, rhs, .. } => {
            def_use(dst, None, vec![lhs, rhs])
        }
        Op::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => def_use(dst, None, vec![cond, if_true, if_false]),
        Op::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            ..
        } => def_use(
            dst,
            None,
            reg_range(matrix_start, n * n)
                .chain(reg_range(rhs_start, n))
                .collect(),
        ),
        Op::TableBounds { dst, table_id, .. } => def_use(dst, None, vec![table_id]),
        Op::TableLookup {
            dst,
            table_id,
            column,
            input,
        }
        | Op::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => def_use(dst, None, vec![table_id, column, input]),
        Op::TableNextEvent {
            dst,
            table_id,
            time,
        } => def_use(dst, None, vec![table_id, time]),
        Op::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => def_use(dst, None, vec![local_seed, global_seed]),
        Op::RandomResult {
            dst,
            state_start,
            state_len,
            ..
        }
        | Op::RandomState {
            dst,
            state_start,
            state_len,
            ..
        } => def_use(dst, None, reg_range(state_start, state_len).collect()),
        Op::ImpureRandomInit { dst, seed } => def_use(dst, None, vec![seed]),
        Op::ImpureRandom { dst, id, .. } => def_use(dst, None, vec![id]),
        Op::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => def_use(dst, None, vec![id, imin, imax]),
        Op::ExternalCall {
            dst,
            args,
            arg_count,
            ..
        } => def_use(dst, None, args.into_iter().take(arg_count).collect()),
        Op::StoreOutput { src } => RowDefUseOp::Store { src },
    }
}

pub(super) fn def_use(
    dst: solve::Reg,
    loaded_y: Option<usize>,
    inputs: Vec<solve::Reg>,
) -> RowDefUseOp {
    RowDefUseOp::Def {
        dst,
        def_use: RowDefUse { loaded_y, inputs },
    }
}

pub(super) fn reg_range(start: solve::Reg, len: usize) -> impl Iterator<Item = solve::Reg> {
    (0..len).filter_map(move |offset| start.checked_add(offset.try_into().ok()?))
}

pub(super) struct ProjectionIncidence {
    pub(super) incidence: Incidence,
    pub(super) unknown_y_indices: Vec<usize>,
}

pub(super) fn algebraic_projection_incidence(
    row_to_vars: &BTreeMap<usize, BTreeSet<usize>>,
    context_span: rumoca_core::Span,
) -> Result<ProjectionIncidence, LowerError> {
    let unknown_y_set = row_to_vars
        .values()
        .flat_map(|vars| vars.iter().copied())
        .collect::<BTreeSet<_>>();
    let mut unknown_y_indices = lower_vec_with_capacity(
        unknown_y_set.len(),
        "projection unknown index count",
        context_span,
    )?;
    unknown_y_indices.extend(unknown_y_set);

    let mut unknown_names = lower_vec_with_capacity(
        unknown_y_indices.len(),
        "projection unknown name count",
        context_span,
    )?;
    for y_idx in &unknown_y_indices {
        unknown_names.push(projection_unknown_id(*y_idx));
    }

    let unknown_positions = unknown_y_indices
        .iter()
        .copied()
        .enumerate()
        .map(|(local_idx, y_idx)| (y_idx, local_idx))
        .collect::<BTreeMap<_, _>>();

    let mut equation_refs = lower_vec_with_capacity(
        row_to_vars.len(),
        "projection equation ref count",
        context_span,
    )?;
    let mut eq_unknowns = lower_vec_with_capacity(
        row_to_vars.len(),
        "projection equation unknown count",
        context_span,
    )?;
    for (row_idx, vars) in row_to_vars {
        equation_refs.push(EquationRef(*row_idx));
        let mut unknowns =
            lower_hash_set_with_capacity(vars.len(), "projection row unknown count", context_span)?;
        for y_idx in vars {
            if let Some(local_idx) = unknown_positions.get(y_idx).copied() {
                unknowns.insert(local_idx);
            }
        }
        eq_unknowns.push(unknowns);
    }

    Ok(ProjectionIncidence {
        incidence: Incidence::new(eq_unknowns, equation_refs, unknown_names),
        unknown_y_indices,
    })
}

pub(super) fn projection_unknown_id(y_idx: usize) -> UnknownId {
    UnknownId::SolverY(y_idx)
}

pub(super) fn projection_y_index(
    unknown: &UnknownId,
    projection_incidence: &ProjectionIncidence,
) -> Option<usize> {
    projection_incidence
        .incidence
        .unknown_names
        .iter()
        .position(|candidate| candidate == unknown)
        .and_then(|idx| projection_incidence.unknown_y_indices.get(idx).copied())
}

pub(super) fn lower_blt_projection_blocks(
    blocks: &[BltBlock],
    row_targets: &[Option<solve::ScalarSlot>],
    projection_incidence: &ProjectionIncidence,
    context_span: rumoca_core::Span,
) -> Result<Vec<solve::AlgebraicProjectionBlock>, LowerError> {
    let mut lowered = lower_vec_with_capacity(
        blocks.len(),
        "algebraic projection block count",
        context_span,
    )?;
    for block in blocks {
        let block = match block {
            BltBlock::Scalar { equation, unknown } => {
                projection_y_index(unknown, projection_incidence)
                    .map(|y_index| {
                        scalar_projection_block(
                            equation.0,
                            y_index,
                            row_targets,
                            projection_incidence,
                            context_span,
                        )
                    })
                    .transpose()?
            }
            BltBlock::AlgebraicLoop {
                equations,
                unknowns,
            } => lower_algebraic_loop_projection_block(
                equations,
                unknowns,
                row_targets,
                projection_incidence,
                context_span,
            )?,
        };
        if let Some(block) = block {
            lowered.push(block);
        }
    }
    merge_overlapping_projection_blocks(lowered, context_span)
}

pub(super) fn merge_overlapping_projection_blocks(
    blocks: Vec<solve::AlgebraicProjectionBlock>,
    context_span: rumoca_core::Span,
) -> Result<Vec<solve::AlgebraicProjectionBlock>, LowerError> {
    let mut merged = lower_vec_with_capacity(
        blocks.len(),
        "merged algebraic projection block count",
        context_span,
    )?;
    for block in blocks {
        merge_projection_block(&mut merged, block, context_span)?;
    }
    Ok(merged)
}

pub(super) fn merge_projection_block(
    merged: &mut Vec<solve::AlgebraicProjectionBlock>,
    mut block: solve::AlgebraicProjectionBlock,
    context_span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let mut idx = 0;
    while idx < merged.len() {
        if projection_blocks_overlap(&merged[idx], &block) {
            let previous = merged.remove(idx);
            block = combine_projection_blocks(previous, block, context_span)?;
            idx = 0;
        } else {
            idx += 1;
        }
    }
    merged.push(block);
    Ok(())
}

pub(super) fn projection_blocks_overlap(
    lhs: &solve::AlgebraicProjectionBlock,
    rhs: &solve::AlgebraicProjectionBlock,
) -> bool {
    lhs.y_indices
        .iter()
        .any(|index| rhs.y_indices.binary_search(index).is_ok())
}

pub(super) fn combine_projection_blocks(
    lhs: solve::AlgebraicProjectionBlock,
    rhs: solve::AlgebraicProjectionBlock,
    context_span: rumoca_core::Span,
) -> Result<solve::AlgebraicProjectionBlock, LowerError> {
    let causal_step_count = lhs
        .causal_steps
        .len()
        .checked_add(rhs.causal_steps.len())
        .ok_or_else(|| {
            lower_contract_violation(
                "merged algebraic projection causal-step count overflows host index range"
                    .to_string(),
                context_span,
            )
        })?;
    let mut causal_steps = lower_vec_with_capacity(
        causal_step_count,
        "merged algebraic projection causal-step count",
        context_span,
    )?;
    causal_steps.extend(lhs.causal_steps);
    causal_steps.extend(rhs.causal_steps);
    Ok(solve::AlgebraicProjectionBlock {
        rows: merge_unique(
            lhs.rows,
            rhs.rows,
            "merged algebraic projection row count",
            context_span,
        )?,
        y_indices: merge_unique(
            lhs.y_indices,
            rhs.y_indices,
            "merged algebraic projection target count",
            context_span,
        )?,
        causal_steps,
    })
}

pub(super) fn merge_unique(
    lhs: Vec<usize>,
    rhs: Vec<usize>,
    context: &'static str,
    context_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let capacity = lhs.len().checked_add(rhs.len()).ok_or_else(|| {
        lower_contract_violation(
            format!("{context} overflows host index range"),
            context_span,
        )
    })?;
    let mut merged = lower_vec_with_capacity(capacity, context, context_span)?;
    merged.extend(lhs);
    merged.extend(rhs);
    merged.sort_unstable();
    merged.dedup();
    Ok(merged)
}

pub(super) fn scalar_projection_block(
    row: usize,
    y_index: usize,
    row_targets: &[Option<solve::ScalarSlot>],
    projection_incidence: &ProjectionIncidence,
    context_span: rumoca_core::Span,
) -> Result<solve::AlgebraicProjectionBlock, LowerError> {
    let mut rows = lower_vec_with_capacity(
        1,
        "scalar algebraic projection block row count",
        context_span,
    )?;
    rows.push(row);
    let mut target_set = BTreeSet::from([y_index]);
    if let Some(solve::ScalarSlot::Y { index, .. }) = row_targets.get(row).copied().flatten()
        && projection_incidence.unknown_y_indices.contains(&index)
    {
        target_set.insert(index);
    }
    let mut y_indices = lower_vec_with_capacity(
        target_set.len(),
        "scalar algebraic projection block target count",
        context_span,
    )?;
    y_indices.extend(target_set);
    let causal_target = row_targets
        .get(row)
        .copied()
        .flatten()
        .and_then(|target| match target {
            solve::ScalarSlot::Y { index, .. } => Some(index),
            _ => None,
        })
        .filter(|target| y_indices.contains(target));
    let causal_steps = if let Some(target) = causal_target {
        vec![solve::AlgebraicProjectionStep {
            row,
            y_index: target,
        }]
    } else {
        Vec::new()
    };
    Ok(solve::AlgebraicProjectionBlock {
        rows,
        y_indices,
        causal_steps,
    })
}

pub(super) fn sorted_set_values(
    values: BTreeSet<usize>,
    context: &'static str,
    context_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut out = lower_vec_with_capacity(values.len(), context, context_span)?;
    out.extend(values);
    Ok(out)
}

pub(super) fn collect_equation_rows(
    equations: &[EquationRef],
    context_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut rows = lower_vec_with_capacity(
        equations.len(),
        "algebraic loop projection row count",
        context_span,
    )?;
    for equation in equations {
        rows.push(equation.0);
    }
    Ok(rows)
}

pub(super) fn lower_algebraic_loop_projection_block(
    equations: &[EquationRef],
    unknowns: &[UnknownId],
    row_targets: &[Option<solve::ScalarSlot>],
    projection_incidence: &ProjectionIncidence,
    context_span: rumoca_core::Span,
) -> Result<Option<solve::AlgebraicProjectionBlock>, LowerError> {
    let rows = collect_equation_rows(equations, context_span)?;
    let y_indices = sorted_set_values(
        loop_projection_target_set(unknowns, row_targets, &rows, projection_incidence),
        "algebraic loop projection target count",
        context_span,
    )?;
    if rows.is_empty() || y_indices.is_empty() {
        return Ok(None);
    }
    Ok(Some(solve::AlgebraicProjectionBlock {
        rows,
        y_indices,
        causal_steps: Vec::new(),
    }))
}

pub(super) fn loop_projection_target_set(
    unknowns: &[UnknownId],
    row_targets: &[Option<solve::ScalarSlot>],
    rows: &[usize],
    projection_incidence: &ProjectionIncidence,
) -> BTreeSet<usize> {
    let mut y_indices = BTreeSet::new();
    for unknown in unknowns {
        if let Some(index) = projection_y_index(unknown, projection_incidence) {
            y_indices.insert(index);
        }
    }
    for row in rows {
        if let Some(solve::ScalarSlot::Y { index, .. }) = row_targets.get(*row).copied().flatten()
            && projection_incidence.unknown_y_indices.contains(&index)
        {
            y_indices.insert(index);
        }
    }
    y_indices
}
