use super::*;

pub(super) fn lower_discrete_value_owners<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
) -> Result<(), LowerError> {
    for index in 0..view.discrete_value_owner_count() {
        let id = view
            .discrete_value_owner_id(index)
            .expect("dense B.1c owner identity resolves");
        let owner = view
            .discrete_value_owner(id)
            .expect("checked B.1c owner resolves");
        let first = owner
            .branches()
            .get(0)
            .expect("checked B.1c owner has a nonempty branch set");
        if owner.structure().is_some() {
            lower_structured_discrete_value_owner(view, layout, clocks, rows, owner)?;
            continue;
        }
        match first.activation() {
            dae::DiscreteBranchActivation::Always => {
                lower_unconditional_discrete_value_owner(view, layout, clocks, rows, owner)?;
            }
            dae::DiscreteBranchActivation::When { .. } => {
                lower_conditional_discrete_value_owner(view, layout, clocks, rows, owner)?;
            }
        }
    }
    Ok(())
}

/// Lowers an always-active B.1c owner (`Integer`/`Boolean`/enumeration discretes).
///
/// A clocked partition can own such a target without any `when`: MLS §16.5 makes every
/// equation of a clocked partition active exactly on its partition's clock ticks, so
/// `counter = previous(counter) + 1` is an unconditional equation whose target carries a
/// clock ownership. The row therefore has to be compiled *under* that clock — the same
/// treatment [`lower_unconditional_discrete_real`] gives a clocked discrete `Real` — so
/// that `previous(...)` resolves against its owning schedule and the row is scheduled on
/// its clock's ticks instead of on every event.
fn lower_unconditional_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<(), LowerError> {
    let branch = owner
        .branches()
        .get(0)
        .expect("checked unconditional B.1c owner has one branch");
    debug_assert_eq!(owner.branches().len(), 1);
    for (target, (value, provenance)) in owner.targets().iter().zip(branch.values().iter()) {
        let expression = view
            .expression(value)
            .expect("checked B.1c value expression resolves");
        let span = provenance.span();
        let variable = dae::VariableId::from(target);
        let clock = clocks.variable_owner(variable);
        let sampled = clocks.variable_is_sampled(variable);
        for scalar in 0..expression
            .value_type()
            .scalar_count()
            .expect("checked B.1c value scalar capacity")
        {
            let program =
                match clock {
                    Some((clock, _)) if sampled => ScalarCompiler::new(view, layout, None)
                        .sampled_program(clock, value, scalar)?,
                    Some((clock, _)) => ScalarCompiler::new(view, layout, None)
                        .clocked_program(clock, value, scalar)?,
                    None => ScalarCompiler::new(view, layout, None).program(value, scalar)?,
                };
            let target = variable_scalar_slot(layout, target.index(), scalar, span)?;
            rows.claim_scalar_event_owner(variable, target, span)?;
            rows.relation_memory_owners
                .claim_exact_expression(value, target);
            let pre_mode = expression_pre_mode(view, value, sampled);
            if clock.is_none() && pre_mode == solve::DiscreteEventPreMode::FollowCurrent {
                rows.push_root_refresh_candidate(program.clone(), span, target);
            }
            rows.push(
                program,
                span,
                target,
                solve::DiscreteRowRole::Equation,
                pre_mode,
                clock.map(|(_, solve)| solve),
            );
        }
    }
    Ok(())
}

fn lower_structured_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<(), LowerError> {
    let structure = owner
        .structure()
        .expect("caller selects one checked structured B.1c owner");
    let branch = owner
        .branches()
        .get(0)
        .expect("checked structured B.1c owner has a branch");
    if owner.branches().len() != 1
        || !matches!(branch.activation(), dae::DiscreteBranchActivation::Always)
    {
        return Err(LowerError::non_computable(
            "conditional structured B.1c maps are not yet representable",
            owner.provenance().span(),
        ));
    }
    let domain = view
        .domain(structure.domain())
        .expect("checked structured B.1c domain resolves")
        .structured()
        .clone();
    let point_count = domain.scalar_count().map_err(|error| {
        LowerError::contract(
            format!("structured B.1c domain is invalid: {error}"),
            owner.provenance().span(),
        )
    })?;
    for (target, (value, provenance)) in owner.targets().iter().zip(branch.values().iter()) {
        let span = provenance.span();
        let variable = dae::VariableId::from(target);
        let clock = clocks.variable_owner(variable);
        let sampled = clocks.variable_is_sampled(variable);
        let (base_ops, load_strides, const_strides) =
            structured_map_program(StructuredMapProgramInput {
                view,
                layout,
                domain_id: structure.domain(),
                domain: &domain,
                scalar_view: structure.scalar_view(),
                value,
                clock,
                sampled,
                span,
            })?;
        let output_map =
            solve::TensorOutputMap::dense_contiguous(rows.structured_output_cursor, &domain)
                .map_err(|_| LowerError::contract("structured B.1c output map overflow", span))?;
        let node_index = rows.structured_rhs.nodes.len();
        rows.structured_rhs.nodes.push(solve::ComputeNode::Map {
            domain: domain.clone(),
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata: solve::TensorNodeMetadata::default(),
            span,
        });
        let base = variable_scalar_slot(layout, target.index(), 0, span)?;
        prove_contiguous_structured_target(layout, target.index(), base, point_count, span)?;
        let target_map = solve::TensorOutputMap::dense_contiguous(0, &domain)
            .map_err(|_| LowerError::contract("structured B.1c target map overflow", span))?;
        let update_index = rows.structured_updates.len();
        rows.structured_updates
            .push(solve::StructuredDiscreteUpdate {
                node_index,
                target: solve::StructuredDiscreteTargetMap {
                    base,
                    map: target_map,
                },
                role: solve::DiscreteRowRole::Equation,
                pre_mode: expression_pre_mode(view, value, sampled),
                observation_refresh: false,
                integrator_history_effect: solve::IntegratorHistoryEffect::Restart,
                clock_owner: clock.map(|(_, solve)| solve),
            });
        rows.claim_structured_event_owner(variable, update_index, span)?;
        rows.structured_output_cursor = rows
            .structured_output_cursor
            .checked_add(point_count)
            .ok_or_else(|| LowerError::contract("structured B.1c row count overflow", span))?;
    }
    Ok(())
}

struct StructuredMapProgramInput<'scope, 'dae> {
    view: dae::DaeView<'dae>,
    layout: &'scope LoweredLayout<'dae>,
    domain_id: dae::DomainId<'dae>,
    domain: &'scope rumoca_core::StructuredIndexDomain,
    scalar_view: rumoca_core::ComprehensionScalarView,
    value: dae::ExprId<'dae>,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
    sampled: bool,
    span: Span,
}

type AffineProgramCertificate = (
    Vec<solve::LinearOp>,
    Vec<solve::AffineStencilLoadStride>,
    Vec<solve::AffineStencilConstStride>,
);

fn structured_map_program(
    input: StructuredMapProgramInput<'_, '_>,
) -> Result<AffineProgramCertificate, LowerError> {
    let points = input.domain.index_tuples().map_err(|error| {
        LowerError::contract(
            format!("structured B.1c domain is invalid: {error}"),
            input.span,
        )
    })?;
    let Some(base_point) = points.first() else {
        return Err(LowerError::non_computable(
            "empty structured B.1c domain has no compact base program",
            input.span,
        ));
    };
    let extents = input
        .domain
        .extents()
        .map_err(|error| {
            LowerError::contract(
                format!("structured B.1c domain is invalid: {error}"),
                input.span,
            )
        })?
        .into_iter()
        .map(|extent| {
            u32::try_from(extent)
                .map_err(|_| LowerError::contract("structured B.1c extent overflow", input.span))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut programs = Vec::with_capacity(points.len());
    for (point, values) in points.iter().enumerate() {
        let scalar = input
            .scalar_view
            .body_scalar(point, &extents)
            .ok_or_else(|| {
                LowerError::contract("structured B.1c scalar view overflow", input.span)
            })?;
        let compiler =
            ScalarCompiler::new(input.view, input.layout, Some((input.domain_id, values)));
        programs.push(match input.clock {
            Some((clock, _)) if input.sampled => {
                compiler.sampled_program(clock, input.value, scalar)?
            }
            Some((clock, _)) => compiler.clocked_program(clock, input.value, scalar)?,
            None => compiler.program(input.value, scalar)?,
        });
    }
    derive_affine_program_certificate(input.domain, base_point, &points, &programs, input.span)
}

pub(in crate::lower) fn unclocked_structured_program<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    domain_id: dae::DomainId<'dae>,
    domain: &rumoca_core::StructuredIndexDomain,
    scalar_view: rumoca_core::ComprehensionScalarView,
    value: dae::ExprId<'dae>,
    span: Span,
) -> Result<AffineProgramCertificate, LowerError> {
    structured_map_program(StructuredMapProgramInput {
        view,
        layout,
        domain_id,
        domain,
        scalar_view,
        value,
        clock: None,
        sampled: false,
        span,
    })
}

fn derive_affine_program_certificate(
    domain: &rumoca_core::StructuredIndexDomain,
    base_point: &[i64],
    points: &[Vec<i64>],
    programs: &[Vec<solve::LinearOp>],
    span: Span,
) -> Result<AffineProgramCertificate, LowerError> {
    let base = &programs[0];
    if programs.iter().any(|program| program.len() != base.len()) {
        return Err(non_affine_structured_program(span));
    }
    let offsets = points
        .iter()
        .map(|point| domain_point_offsets(domain, base_point, point, span))
        .collect::<Result<Vec<_>, _>>()?;
    let mut load_strides = Vec::new();
    let mut const_strides = Vec::new();
    let evidence = AffineProgramEvidence {
        programs,
        offsets: &offsets,
        rank: domain.binders.len(),
        span,
    };
    for (op_position, base_op) in base.iter().enumerate() {
        if let Some((kind, dst, base_index)) = affine_load(base_op) {
            let terms = infer_load_terms(kind, dst, base_index, op_position, evidence)?;
            if !terms.is_empty() {
                load_strides.push(solve::AffineStencilLoadStride { op_position, terms });
            }
        } else if let solve::LinearOp::Const { dst, value } = base_op {
            let terms = infer_const_terms(*dst, *value, op_position, evidence)?;
            if !terms.is_empty() {
                const_strides.push(solve::AffineStencilConstStride { op_position, terms });
            }
        } else if programs
            .iter()
            .any(|program| program[op_position] != *base_op)
        {
            return Err(non_affine_structured_program(span));
        }
    }
    Ok((base.clone(), load_strides, const_strides))
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AffineLoadKind {
    Y,
    P,
    Seed,
}

#[derive(Clone, Copy)]
struct AffineProgramEvidence<'scope> {
    programs: &'scope [Vec<solve::LinearOp>],
    offsets: &'scope [Vec<isize>],
    rank: usize,
    span: Span,
}

fn affine_load(op: &solve::LinearOp) -> Option<(AffineLoadKind, solve::Reg, usize)> {
    match *op {
        solve::LinearOp::LoadY { dst, index } => Some((AffineLoadKind::Y, dst, index)),
        solve::LinearOp::LoadP { dst, index } => Some((AffineLoadKind::P, dst, index)),
        solve::LinearOp::LoadSeed { dst, index } => Some((AffineLoadKind::Seed, dst, index)),
        _ => None,
    }
}

fn domain_point_offsets(
    domain: &rumoca_core::StructuredIndexDomain,
    base: &[i64],
    point: &[i64],
    span: Span,
) -> Result<Vec<isize>, LowerError> {
    domain
        .binders
        .iter()
        .enumerate()
        .map(|(dimension, binder)| {
            isize::try_from((point[dimension] - base[dimension]) / binder.step)
                .map_err(|_| LowerError::contract("structured B.1c domain offset overflow", span))
        })
        .collect()
}

fn prove_contiguous_structured_target(
    layout: &LoweredLayout<'_>,
    variable: u32,
    base: solve::ScalarSlot,
    scalar_count: usize,
    span: Span,
) -> Result<(), LowerError> {
    for scalar in 0..scalar_count {
        let actual = variable_scalar_slot(layout, variable, scalar, span)?;
        let expected = match base {
            solve::ScalarSlot::Y { index, .. } => {
                index.checked_add(scalar).map(solve::scalar_slot_y)
            }
            solve::ScalarSlot::P { index, .. } => {
                index.checked_add(scalar).map(solve::scalar_slot_p)
            }
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
        }
        .ok_or_else(|| LowerError::contract("structured B.1c target map overflow", span))?;
        if actual != expected {
            return Err(LowerError::contract(
                "structured B.1c target does not own contiguous Solve storage",
                span,
            ));
        }
    }
    Ok(())
}

fn dimension_probe(offsets: &[Vec<isize>], dimension: usize) -> Option<usize> {
    offsets.iter().position(|offset| {
        offset[dimension] == 1
            && offset
                .iter()
                .enumerate()
                .all(|(other, value)| other == dimension || *value == 0)
    })
}

fn infer_load_terms(
    kind: AffineLoadKind,
    dst: solve::Reg,
    base_index: usize,
    op_position: usize,
    evidence: AffineProgramEvidence<'_>,
) -> Result<Vec<solve::AffineStencilIndexStrideTerm>, LowerError> {
    let mut coefficients = vec![0_isize; evidence.rank];
    for (dimension, coefficient) in coefficients.iter_mut().enumerate() {
        let Some(probe) = dimension_probe(evidence.offsets, dimension) else {
            continue;
        };
        let Some((probe_kind, probe_dst, probe_index)) =
            affine_load(&evidence.programs[probe][op_position])
        else {
            return Err(non_affine_structured_program(evidence.span));
        };
        if probe_kind != kind || probe_dst != dst {
            return Err(non_affine_structured_program(evidence.span));
        }
        *coefficient = isize::try_from(probe_index)
            .ok()
            .and_then(|value| value.checked_sub(isize::try_from(base_index).ok()?))
            .ok_or_else(|| non_affine_structured_program(evidence.span))?;
    }
    for (program, offset) in evidence.programs.iter().zip(evidence.offsets) {
        let Some((actual_kind, actual_dst, actual_index)) = affine_load(&program[op_position])
        else {
            return Err(non_affine_structured_program(evidence.span));
        };
        let expected = affine_index(base_index, &coefficients, offset)
            .ok_or_else(|| non_affine_structured_program(evidence.span))?;
        if actual_kind != kind || actual_dst != dst || actual_index != expected {
            return Err(non_affine_structured_program(evidence.span));
        }
    }
    Ok(coefficients
        .into_iter()
        .enumerate()
        .filter_map(|(dimension, stride)| {
            (stride != 0).then_some(solve::AffineStencilIndexStrideTerm { dimension, stride })
        })
        .collect())
}

fn affine_index(base: usize, coefficients: &[isize], offsets: &[isize]) -> Option<usize> {
    let mut value = isize::try_from(base).ok()?;
    for (&coefficient, &offset) in coefficients.iter().zip(offsets) {
        value = value.checked_add(coefficient.checked_mul(offset)?)?;
    }
    usize::try_from(value).ok()
}

fn infer_const_terms(
    dst: solve::Reg,
    base_value: f64,
    op_position: usize,
    evidence: AffineProgramEvidence<'_>,
) -> Result<Vec<solve::AffineStencilConstStrideTerm>, LowerError> {
    let mut coefficients = vec![0.0; evidence.rank];
    for (dimension, coefficient) in coefficients.iter_mut().enumerate() {
        let Some(probe) = dimension_probe(evidence.offsets, dimension) else {
            continue;
        };
        let solve::LinearOp::Const {
            dst: probe_dst,
            value,
        } = evidence.programs[probe][op_position]
        else {
            return Err(non_affine_structured_program(evidence.span));
        };
        if probe_dst != dst {
            return Err(non_affine_structured_program(evidence.span));
        }
        *coefficient = value - base_value;
    }
    for (program, offset) in evidence.programs.iter().zip(evidence.offsets) {
        let solve::LinearOp::Const {
            dst: actual_dst,
            value,
        } = program[op_position]
        else {
            return Err(non_affine_structured_program(evidence.span));
        };
        let expected = coefficients
            .iter()
            .zip(offset)
            .fold(base_value, |value, (coefficient, offset)| {
                value + coefficient * (*offset as f64)
            });
        if actual_dst != dst || value != expected {
            return Err(non_affine_structured_program(evidence.span));
        }
    }
    Ok(coefficients
        .into_iter()
        .enumerate()
        .filter_map(|(dimension, stride)| {
            (stride != 0.0).then_some(solve::AffineStencilConstStrideTerm { dimension, stride })
        })
        .collect())
}

fn non_affine_structured_program(span: Span) -> LowerError {
    LowerError::non_computable(
        "structured B.1c body does not have one proven affine scalar program",
        span,
    )
}

fn lower_conditional_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<(), LowerError> {
    for (target_ordinal, target) in owner.targets().iter().enumerate() {
        let target_variable = dae::VariableId::from(target);
        let variable = view
            .variable(target_variable)
            .expect("checked B.1c target resolves");
        for scalar in 0..variable.scalar_count() {
            let mut lowered = Vec::new();
            for branch in owner.branches().iter() {
                let branch = lower_checked_discrete_value_branch(
                    view,
                    layout,
                    clocks,
                    target,
                    target_ordinal,
                    scalar,
                    branch,
                )?;
                record_guarded_target(
                    &mut lowered,
                    dae::VariableId::from(target),
                    branch.target,
                    branch.assignment,
                    branch.clock,
                    branch.pre_mode,
                    branch.span,
                )?;
            }
            let [target] = lowered.as_slice() else {
                unreachable!("one B.1c target and scalar creates one guarded target")
            };
            let program = match target.clock {
                Some((clock, _)) => ScalarCompiler::new(view, layout, None)
                    .clocked_guarded_assignments_program(
                        clock,
                        &target.branches,
                        target.target,
                        target.span,
                    )?,
                None => ScalarCompiler::new(view, layout, None).guarded_assignments_program(
                    &target.branches,
                    target.target,
                    target.span,
                )?,
            };
            rows.claim_scalar_event_owner(target_variable, target.target, target.span)?;
            rows.push(
                program,
                target.span,
                target.target,
                solve::DiscreteRowRole::EventAction,
                target.pre_mode,
                target.clock.map(|(_, clock)| clock),
            );
        }
    }
    Ok(())
}

struct LoweredDiscreteValueBranch<'dae> {
    target: solve::ScalarSlot,
    assignment: GuardedAssignment<'dae>,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
    pre_mode: solve::DiscreteEventPreMode,
    span: Span,
}

fn lower_checked_discrete_value_branch<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    target: dae::DiscreteValueId<'dae>,
    target_ordinal: usize,
    scalar: usize,
    branch: dae::DiscreteValueBranchView<'dae>,
) -> Result<LoweredDiscreteValueBranch<'dae>, LowerError> {
    let dae::DiscreteBranchActivation::When { trigger, guard } = branch.activation() else {
        unreachable!("checked B.1c owner cannot mix always and when branches")
    };
    let (value, provenance) = branch
        .values()
        .get(target_ordinal)
        .expect("checked B.1c branch arity matches its target set");
    let span = provenance.span();
    let target = variable_scalar_slot(layout, target.index(), scalar, span)?;
    let clock = condition_clock_owner(view, guard)
        .map(|clock| clocks.clock(clock).map(|solve| (clock, solve)))
        .transpose()?;
    let trigger_memory = condition_memory(layout, trigger, span)?;
    let pre_mode = merge_pre_mode(
        expression_pre_mode(view, value, false),
        merge_pre_mode(
            condition_pre_mode(view, trigger),
            condition_pre_mode(view, guard),
        ),
    );
    Ok(LoweredDiscreteValueBranch {
        target,
        assignment: (trigger, guard, value, scalar, trigger_memory),
        clock,
        pre_mode,
        span,
    })
}
