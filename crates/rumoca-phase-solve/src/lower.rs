use std::collections::{BTreeSet, HashMap};

use rumoca_core::{ComprehensionScalarView, Span};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{BltBlock, EquationRef, UnknownId};

use crate::LowerError;
use crate::layout::{LoweredLayout, StorageClass, lower_layout};

mod clocks;
mod events;
mod scalar;
use scalar::{ScalarCompiler, ScalarSelector, ScaledDerivativeProgram};

pub(crate) fn lower_solve_problem<'dae>(
    view: dae::DaeView<'dae>,
    manifold: &[dae::ExprId<'dae>],
) -> Result<solve::SolveProblem, LowerError> {
    if view.variable_count() == 0
        && view.continuous_owner_count() == 0
        && view.initialization_owner_count() == 0
        && view.discrete_value_owner_count() == 0
    {
        return Err(LowerError::unspanned_non_computable(
            "the model has no variables or equations to simulate",
        ));
    }
    reject_unimplemented_systems(view)?;
    let lowered = lower_layout(view)?;
    let clocks = clocks::lower_clocks(view)?;
    let structural = structural_matching(view)?;
    let continuous = lower_continuous(view, &lowered, &structural, manifold)?;
    let initialization = lower_initialization(view, &lowered)?;
    let (discrete, events) = events::lower_discrete_and_events(view, &lowered, &clocks)?;
    Ok(solve::SolveProblem {
        schema_version: solve::SOLVE_SCHEMA_VERSION,
        layout: lowered.layout,
        solve_layout: lowered.solve_layout,
        continuous,
        initialization,
        discrete,
        events,
        clocks: clocks.partition,
    })
}

fn reject_unimplemented_systems(view: dae::DaeView<'_>) -> Result<(), LowerError> {
    let unsupported = [(view.terminal_count(), "terminal coordinates")];
    if let Some((_, semantics)) = unsupported.into_iter().find(|(count, _)| *count != 0) {
        return Err(LowerError::unsupported(
            format!("{semantics} do not yet have checked Solve lowering"),
            first_model_span(view),
        ));
    }
    Ok(())
}

struct StructuralMatching<'dae> {
    rows: HashMap<usize, UnknownId<'dae>>,
    algebraic_blocks: Vec<Vec<UnknownId<'dae>>>,
}

fn structural_matching<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<StructuralMatching<'dae>, LowerError> {
    let scalar_rows = continuous_scalar_row_count(view)?;
    let unknowns = view
        .variables()
        .filter(|(_, variable)| {
            matches!(
                variable.role(),
                dae::VariableRole::State | dae::VariableRole::Algebraic | dae::VariableRole::Output
            )
        })
        .map(|(_, variable)| variable.scalar_count())
        .sum::<usize>();
    if scalar_rows == 0 && unknowns == 0 {
        return Ok(StructuralMatching {
            rows: HashMap::new(),
            algebraic_blocks: Vec::new(),
        });
    }
    let sorted = rumoca_phase_structural::sort(view).map_err(|error| LowerError::Structural {
        reason: error.to_string(),
        span: error.source_span(),
    })?;
    let rows = sorted
        .matching
        .iter()
        .copied()
        .map(|(EquationRef(equation), unknown)| (equation, unknown))
        .collect::<HashMap<_, _>>();
    let algebraic_blocks = algebraic_projection_blocks(&sorted.blocks, &rows)?;
    Ok(StructuralMatching {
        rows,
        algebraic_blocks,
    })
}

fn algebraic_projection_blocks<'dae>(
    blocks: &[BltBlock<'dae>],
    rows: &HashMap<usize, UnknownId<'dae>>,
) -> Result<Vec<Vec<UnknownId<'dae>>>, LowerError> {
    let mut algebraic_blocks = Vec::new();
    for block in blocks {
        match block {
            BltBlock::Scalar { unknown, .. } if matches!(unknown, UnknownId::Algebraic { .. }) => {
                algebraic_blocks.push(vec![*unknown]);
            }
            BltBlock::AlgebraicLoop { unknowns, .. } => {
                let algebraic = unknowns
                    .iter()
                    .copied()
                    .filter(|unknown| matches!(unknown, UnknownId::Algebraic { .. }))
                    .collect::<Vec<_>>();
                if !algebraic.is_empty() {
                    algebraic_blocks.push(algebraic);
                }
            }
            BltBlock::StructuredScalar(family) => {
                append_structured_algebraic_blocks(family, rows, &mut algebraic_blocks)?;
            }
            BltBlock::Scalar { .. } => {}
        }
    }
    Ok(algebraic_blocks)
}

fn append_structured_algebraic_blocks<'dae>(
    family: &rumoca_phase_structural::StructuredScalarBlock,
    rows: &HashMap<usize, UnknownId<'dae>>,
    blocks: &mut Vec<Vec<UnknownId<'dae>>>,
) -> Result<(), LowerError> {
    for row in family.scalar_rows() {
        let (EquationRef(equation), _) = row.map_err(|error| LowerError::Structural {
            reason: error.to_string(),
            span: error.source_span(),
        })?;
        let Some(unknown @ UnknownId::Algebraic { .. }) = rows.get(&equation) else {
            continue;
        };
        blocks.push(vec![*unknown]);
    }
    Ok(())
}

fn continuous_scalar_row_count(view: dae::DaeView<'_>) -> Result<usize, LowerError> {
    view.continuous_owners().try_fold(0usize, |count, owner| {
        let rows = match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => view
                .expression(equation.residual())
                .expect("branded residual expression resolves")
                .value_type()
                .scalar_count()
                .expect("checked expression scalar capacity"),
            dae::ContinuousOwnerView::Structured { family, .. } => family.scalar_rows() as usize,
        };
        count.checked_add(rows).ok_or_else(|| {
            LowerError::contract(
                "continuous scalar row count overflow",
                owner_provenance(owner).span(),
            )
        })
    })
}

fn lower_continuous<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    structural: &StructuralMatching<'dae>,
    manifold: &[dae::ExprId<'dae>],
) -> Result<solve::ContinuousSolveSystem, LowerError> {
    let mut residual = ScalarRows::default();
    let mut derivative = DerivativeRows::default();
    let mut row = 0usize;
    for owner in view.continuous_owners() {
        match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                let count = scalar_count(view, equation.residual());
                if let Some(group) = lower_implicit_tensor_derivative(
                    view,
                    layout,
                    &structural.rows,
                    row,
                    equation.residual(),
                    equation.provenance().span(),
                )? {
                    row = checked_ordinal_add(
                        row,
                        group.rows,
                        "continuous row ordinal overflow",
                        equation.provenance().span(),
                    )?;
                    derivative.push_tensor(group);
                    continue;
                }
                for scalar in 0..count {
                    lower_continuous_row(
                        view,
                        layout,
                        &structural.rows,
                        row,
                        equation.residual(),
                        scalar,
                        None,
                        equation.provenance().span(),
                        &mut residual,
                        &mut derivative,
                    )?;
                    row += 1;
                }
            }
            dae::ContinuousOwnerView::Structured { family, .. } => {
                row = lower_continuous_family(
                    view,
                    layout,
                    &structural.rows,
                    row,
                    family,
                    &mut residual,
                    &mut derivative,
                )?;
            }
        }
    }
    let residual = residual.into_compute_block()?;
    let (implicit_row_targets, algebraic_projection_plan) =
        lower_algebraic_projection(view, layout, structural)?;
    let (manifold_residual, manifold_projection_plan) = lower_manifold(view, layout, manifold)?;
    Ok(solve::ContinuousSolveSystem {
        implicit_rhs: residual.clone(),
        implicit_row_targets,
        algebraic_projection_plan,
        residual,
        manifold_residual,
        manifold_projection_plan,
        derivative_rhs: derivative.into_compute_block(
            layout.solve_layout.state_scalar_count(),
            first_model_span(view),
        )?,
    })
}

fn lower_algebraic_projection<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    structural: &StructuralMatching<'dae>,
) -> Result<
    (
        Vec<Option<solve::ScalarSlot>>,
        solve::AlgebraicProjectionPlan,
    ),
    LowerError,
> {
    let solver_count = layout.solve_layout.solver_scalar_count();
    let state_count = layout.solve_layout.state_scalar_count();
    let implicit_output_count = if state_count == solver_count {
        0
    } else {
        solver_count
    };
    let mut targets = vec![None; implicit_output_count];
    for unknown in structural.rows.values().copied() {
        let UnknownId::Algebraic { variable, scalar } = unknown else {
            continue;
        };
        let span = first_model_span(view);
        let target = variable_scalar_slot(layout, variable.index(), scalar as usize, span)?;
        let solve::ScalarSlot::Y { index, .. } = target else {
            unreachable!("algebraic declarations are Y slots")
        };
        let entry = targets.get_mut(index).ok_or_else(|| {
            LowerError::contract("matched algebraic target is outside Solve Y storage", span)
        })?;
        if entry.replace(target).is_some() {
            return Err(LowerError::contract(
                "two continuous rows matched the same algebraic target",
                span,
            ));
        }
    }
    let algebraic_indices = state_count..solver_count;
    let missing = algebraic_indices
        .clone()
        .find(|index| targets[*index].is_none());
    if let Some(index) = missing {
        return Err(LowerError::non_computable(
            format!("structural proof omitted algebraic Solve slot {index}"),
            first_model_span(view),
        ));
    }
    let mut covered = vec![false; solver_count];
    let blocks = structural
        .algebraic_blocks
        .iter()
        .map(|unknowns| {
            let mut indices = Vec::with_capacity(unknowns.len());
            for unknown in unknowns {
                let UnknownId::Algebraic { variable, scalar } = *unknown else {
                    unreachable!("structural algebraic block contains only algebraics")
                };
                let solve::ScalarSlot::Y { index, .. } = variable_scalar_slot(
                    layout,
                    variable.index(),
                    scalar as usize,
                    first_model_span(view),
                )?
                else {
                    unreachable!("algebraic declarations are Y slots")
                };
                covered[index] = true;
                indices.push(index);
            }
            Ok(solve::AlgebraicProjectionBlock {
                rows: indices.clone(),
                y_indices: indices,
            })
        })
        .collect::<Result<Vec<_>, LowerError>>()?;
    if let Some(index) = algebraic_indices.clone().find(|index| !covered[*index]) {
        return Err(LowerError::non_computable(
            format!("BLT proof omitted algebraic Solve slot {index}"),
            first_model_span(view),
        ));
    }
    Ok((targets, solve::AlgebraicProjectionPlan { blocks }))
}

fn lower_manifold<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    expressions: &[dae::ExprId<'dae>],
) -> Result<(solve::ComputeBlock, solve::AlgebraicProjectionPlan), LowerError> {
    let mut residuals = ScalarRows::default();
    let mut row_states = Vec::with_capacity(expressions.len());
    for (row, expression) in expressions.iter().copied().enumerate() {
        let node = view
            .expression(expression)
            .expect("prepared manifold expression resolves");
        if !node.value_type().is_scalar() {
            return Err(LowerError::non_computable(
                "index-reduction manifold expression is not scalar",
                node.provenance().span(),
            ));
        }
        let program = ScalarCompiler::new(view, layout, None).program(expression, 0)?;
        residuals.push(program, node.provenance().span(), row);
        row_states.push(manifold_state_slots(view, layout, expression)?);
    }
    let plan = manifold_projection_plan(row_states, first_model_span(view))?;
    Ok((residuals.into_compute_block()?, plan))
}

fn manifold_state_slots<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    expression: dae::ExprId<'dae>,
) -> Result<BTreeSet<usize>, LowerError> {
    let mut states = BTreeSet::new();
    let mut failure = None;
    dae::for_each_expression(view, expression, |_, node| {
        let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
            return;
        };
        match coordinate {
            dae::CoordinateView::State(state) => {
                let variable = view
                    .variable(state.into())
                    .expect("manifold state declaration resolves");
                if let Err(error) = append_manifold_state_slots(
                    layout,
                    state,
                    variable.scalar_count(),
                    node.provenance().span(),
                    &mut states,
                ) {
                    failure = Some(error);
                }
            }
            dae::CoordinateView::Algebraic(_) | dae::CoordinateView::Derivative(_) => {
                failure = Some(LowerError::non_computable(
                    "retained manifold depends on an algebraic or derivative coordinate",
                    node.provenance().span(),
                ));
            }
            _ => {}
        }
    });
    match failure {
        Some(error) => Err(error),
        None if states.is_empty() => Err(LowerError::non_computable(
            "retained manifold has no state dependence",
            view.expression(expression)
                .expect("manifold expression resolves")
                .provenance()
                .span(),
        )),
        None => Ok(states),
    }
}

fn append_manifold_state_slots(
    layout: &LoweredLayout<'_>,
    state: dae::StateId<'_>,
    scalar_count: usize,
    span: Span,
    states: &mut BTreeSet<usize>,
) -> Result<(), LowerError> {
    for scalar in 0..scalar_count {
        let solve::ScalarSlot::Y { index, .. } =
            variable_scalar_slot(layout, state.index(), scalar, span)?
        else {
            unreachable!("state declarations are Y slots")
        };
        states.insert(index);
    }
    Ok(())
}

fn manifold_projection_plan(
    row_states: Vec<BTreeSet<usize>>,
    span: Span,
) -> Result<solve::AlgebraicProjectionPlan, LowerError> {
    let mut components = Vec::<(Vec<usize>, BTreeSet<usize>)>::new();
    for (row, states) in row_states.into_iter().enumerate() {
        let mut rows = vec![row];
        let mut states = states;
        let mut component = 0;
        while component < components.len() {
            if components[component].1.is_disjoint(&states) {
                component += 1;
                continue;
            }
            let (merged_rows, merged_states) = components.remove(component);
            rows.extend(merged_rows);
            states.extend(merged_states);
        }
        components.push((rows, states));
    }
    let mut blocks = Vec::with_capacity(components.len());
    for (mut rows, states) in components {
        rows.sort_unstable();
        if states.len() < rows.len() {
            return Err(LowerError::non_computable(
                "retained manifold has fewer state coordinates than residual rows",
                span,
            ));
        }
        blocks.push(solve::AlgebraicProjectionBlock {
            rows,
            y_indices: states.into_iter().collect(),
        });
    }
    Ok(solve::AlgebraicProjectionPlan { blocks })
}

fn lower_continuous_family<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    matching: &HashMap<usize, UnknownId<'dae>>,
    mut row: usize,
    family: dae::StructuredFamilyView<'dae>,
    residual: &mut ScalarRows,
    derivative: &mut DerivativeRows,
) -> Result<usize, LowerError> {
    let domain = view
        .domain(family.domain())
        .expect("checked family domain resolves");
    if family.scalar_view() == ComprehensionScalarView::RowMajorProjection
        && family.bodies().len() == 1
    {
        let body = family
            .bodies()
            .get(0)
            .expect("single checked family body resolves");
        if let Some(group) = lower_implicit_tensor_derivative(
            view,
            layout,
            matching,
            row,
            body,
            family.provenance().span(),
        )? {
            if group.rows != family.scalar_rows() as usize {
                return Err(LowerError::contract(
                    "array-equation projection row count differs from its implicit state system",
                    family.provenance().span(),
                ));
            }
            row = row.checked_add(group.rows).ok_or_else(|| {
                LowerError::contract(
                    "continuous row ordinal overflow",
                    family.provenance().span(),
                )
            })?;
            derivative.push_tensor(group);
            return Ok(row);
        }
    }
    for point in 0..domain.scalar_count() as usize {
        let values = domain
            .structured()
            .index_tuple_at(point)
            .expect("checked domain remains valid")
            .expect("checked point ordinal is in range");
        row = lower_continuous_family_point(
            view, layout, matching, row, family, point, &values, residual, derivative,
        )?;
    }
    Ok(row)
}

#[allow(clippy::too_many_arguments)]
fn lower_continuous_family_point<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    matching: &HashMap<usize, UnknownId<'dae>>,
    mut row: usize,
    family: dae::StructuredFamilyView<'dae>,
    point: usize,
    values: &[i64],
    residual: &mut ScalarRows,
    derivative: &mut DerivativeRows,
) -> Result<usize, LowerError> {
    let domain = view
        .domain(family.domain())
        .expect("checked family domain resolves");
    for body in family.bodies().iter() {
        let scalar = family
            .scalar_view()
            .body_scalar(point, domain.extents())
            .expect("checked family view projects its domain point");
        lower_continuous_row(
            view,
            layout,
            matching,
            row,
            body,
            scalar,
            Some((family.domain(), values)),
            family.provenance().span(),
            residual,
            derivative,
        )?;
        row += 1;
    }
    Ok(row)
}

#[allow(clippy::too_many_arguments)]
fn lower_continuous_row<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    matching: &HashMap<usize, UnknownId<'dae>>,
    row: usize,
    expression: dae::ExprId<'dae>,
    scalar: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    span: Span,
    residual: &mut ScalarRows,
    derivative: &mut DerivativeRows,
) -> Result<(), LowerError> {
    let unknown = matching.get(&row).copied().ok_or_else(|| {
        LowerError::non_computable("structural proof omitted a continuous row", span)
    })?;
    match unknown {
        UnknownId::Derivative {
            state,
            scalar: target,
        } => {
            let rhs = derivative_rhs(
                view,
                expression,
                scalar,
                domain_point,
                state,
                target as usize,
            )?;
            let program = match rhs {
                DerivativeRhs::Explicit { expression, scalar } => {
                    ScalarCompiler::new(view, layout, domain_point).program(expression, scalar)?
                }
                DerivativeRhs::Scaled {
                    numerator,
                    numerator_scalar,
                    coefficient,
                    coefficient_scalar,
                    span,
                } => ScalarCompiler::new(view, layout, domain_point).scaled_derivative_program(
                    ScaledDerivativeProgram {
                        numerator,
                        numerator_scalar,
                        coefficient,
                        coefficient_scalar,
                        negate: false,
                        span,
                    },
                )?,
            };
            let target = variable_scalar_slot(layout, state.index(), target as usize, span)?;
            let solve::ScalarSlot::Y { index, .. } = target else {
                unreachable!("state declarations are Y slots")
            };
            derivative.push_scalar(program, span, index);
        }
        UnknownId::Algebraic {
            variable,
            scalar: target,
        } => {
            let program =
                ScalarCompiler::new(view, layout, domain_point).program(expression, scalar)?;
            let target = variable_scalar_slot(layout, variable.index(), target as usize, span)?;
            let solve::ScalarSlot::Y { index, .. } = target else {
                unreachable!("algebraic declarations are Y slots")
            };
            residual.push(program, span, index);
        }
        UnknownId::Solver(_) | UnknownId::Unmatched { .. } => {
            return Err(LowerError::non_computable(
                "DAE structural matching returned a non-DAE unknown",
                span,
            ));
        }
    }
    Ok(())
}

struct ImplicitTensorDerivative {
    node: solve::ComputeNode,
    output_start: usize,
    rows: usize,
    span: Span,
}

struct ImplicitTensorForm<'dae> {
    matrix: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
    state: dae::StateId<'dae>,
    rows: usize,
}

fn lower_implicit_tensor_derivative<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    matching: &HashMap<usize, UnknownId<'dae>>,
    first_row: usize,
    residual: dae::ExprId<'dae>,
    span: Span,
) -> Result<Option<ImplicitTensorDerivative>, LowerError> {
    let Some(form) = recognize_implicit_tensor(view, residual)? else {
        return Ok(None);
    };
    if expression_contains_derivative(view, form.matrix)
        || expression_contains_derivative(view, form.rhs)
    {
        return Err(LowerError::non_computable(
            "implicit linear state system has derivative-dependent coefficients or right-hand side",
            span,
        ));
    }
    validate_implicit_tensor_matching(matching, first_row, form.state, form.rows, span)?;
    let output_start = contiguous_state_output(layout, form.state, form.rows, span)?;
    let (matrix_start, rhs_start, next_reg, setup_ops) =
        ScalarCompiler::new(view, layout, None).packed_pair(form.matrix, form.rhs)?;
    let provenance =
        solve::PatternProvenance::derived(solve::PatternDerivation::ConservativeFull, span)
            .map_err(|error| LowerError::non_computable(error.to_string(), span))?;
    let matrix_pattern = solve::StructuralPattern::full(form.rows, form.rows, provenance)
        .map_err(|error| LowerError::non_computable(error.to_string(), span))?;
    Ok(Some(ImplicitTensorDerivative {
        node: solve::ComputeNode::LinSolve {
            setup_ops,
            matrix_start,
            rhs_start,
            n: form.rows,
            next_reg,
            matrix_pattern,
            metadata: solve::TensorNodeMetadata::default(),
            span,
        },
        output_start,
        rows: form.rows,
        span,
    }))
}

fn recognize_implicit_tensor<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Result<Option<ImplicitTensorForm<'dae>>, LowerError> {
    let residual_node = view
        .expression(residual)
        .expect("branded residual expression resolves");
    let dae::ExpressionOperation::Binary { operator, lhs, rhs } = residual_node.operation() else {
        return Ok(None);
    };
    if operator != dae::BinaryOperator::Subtract {
        return Ok(None);
    }
    let lhs_node = view
        .expression(lhs)
        .expect("branded residual operand resolves");
    let dae::ExpressionOperation::Binary {
        operator,
        lhs: matrix,
        rhs: derivative,
    } = lhs_node.operation()
    else {
        return Ok(None);
    };
    if operator != dae::BinaryOperator::Multiply {
        return Ok(None);
    }
    let matrix_dimensions = view
        .expression(matrix)
        .expect("branded matrix expression resolves")
        .value_type()
        .dimensions();
    let derivative_dimensions = view
        .expression(derivative)
        .expect("branded derivative expression resolves")
        .value_type()
        .dimensions();
    let rhs_dimensions = view
        .expression(rhs)
        .expect("branded right-hand expression resolves")
        .value_type()
        .dimensions();
    let ([rows, columns], [derivative_count], [rhs_count]) =
        (matrix_dimensions, derivative_dimensions, rhs_dimensions)
    else {
        return Ok(None);
    };
    if rows != columns || columns != derivative_count || rows != rhs_count || *rows == 0 {
        return Ok(None);
    }
    let rows = *rows as usize;
    let selector = ScalarSelector::new(view, None);
    let Some((dae::CoordinateView::Derivative(state), 0)) = selector.coordinate(derivative, 0)?
    else {
        return Ok(None);
    };
    for scalar in 1..rows {
        if !matches!(
            selector.coordinate(derivative, scalar)?,
            Some((dae::CoordinateView::Derivative(found), found_scalar))
                if found == state && found_scalar == scalar
        ) {
            return Ok(None);
        }
    }
    Ok(Some(ImplicitTensorForm {
        matrix,
        rhs,
        state,
        rows,
    }))
}

fn validate_implicit_tensor_matching<'dae>(
    matching: &HashMap<usize, UnknownId<'dae>>,
    first_row: usize,
    state: dae::StateId<'dae>,
    rows: usize,
    span: Span,
) -> Result<(), LowerError> {
    let mut matched_scalars = vec![false; rows];
    for offset in 0..rows {
        let row = first_row
            .checked_add(offset)
            .ok_or_else(|| LowerError::contract("continuous row ordinal overflow", span))?;
        let Some(UnknownId::Derivative {
            state: found,
            scalar,
        }) = matching.get(&row).copied()
        else {
            return Err(LowerError::non_computable(
                "implicit linear state system has a row not matched to a derivative",
                span,
            ));
        };
        let scalar = scalar as usize;
        if found != state || scalar >= rows || std::mem::replace(&mut matched_scalars[scalar], true)
        {
            return Err(LowerError::non_computable(
                "implicit linear state system does not match each derivative component exactly once",
                span,
            ));
        }
    }
    Ok(())
}

fn contiguous_state_output<'dae>(
    layout: &LoweredLayout<'dae>,
    state: dae::StateId<'dae>,
    rows: usize,
    span: Span,
) -> Result<usize, LowerError> {
    let output_start = match variable_scalar_slot(layout, state.index(), 0, span)? {
        solve::ScalarSlot::Y { index, .. } => index,
        _ => unreachable!("state declarations are Y slots"),
    };
    for scalar in 1..rows {
        let slot = variable_scalar_slot(layout, state.index(), scalar, span)?;
        let expected = output_start
            .checked_add(scalar)
            .ok_or_else(|| LowerError::contract("implicit derivative slot overflow", span))?;
        if !matches!(
            slot,
            solve::ScalarSlot::Y { index, .. } if index == expected
        ) {
            return Err(LowerError::contract(
                "implicit derivative vector does not occupy contiguous Solve state slots",
                span,
            ));
        }
    }
    Ok(output_start)
}

fn expression_contains_derivative<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let mut pending = vec![expression];
    while let Some(expression) = pending.pop() {
        let node = view
            .expression(expression)
            .expect("branded expression resolves");
        match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(_)) => {
                return true;
            }
            dae::ExpressionOperation::Literal(_)
            | dae::ExpressionOperation::Coordinate(_)
            | dae::ExpressionOperation::FunctionFoldParameter { .. } => {}
            dae::ExpressionOperation::Range(range) => {
                pending.push(range.stop().expression());
                if let Some(step) = range.explicit_step() {
                    pending.push(step.expression());
                }
                pending.push(range.start().expression());
            }
            dae::ExpressionOperation::Unary { operand, .. } => pending.push(operand),
            dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
                pending.push(lhs);
                pending.push(rhs);
            }
            dae::ExpressionOperation::Conditional(operands)
            | dae::ExpressionOperation::Array(operands)
            | dae::ExpressionOperation::Record(operands) => pending.extend(operands.iter()),
            dae::ExpressionOperation::Field { base, .. } => pending.push(base),
            dae::ExpressionOperation::Comprehension { body, .. } => pending.push(body),
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                pending.push(definition.rhs());
            }
            dae::ExpressionOperation::FunctionFoldOutput { fold, .. } => {
                let fold = view
                    .function_fold(fold)
                    .expect("checked function fold identity resolves");
                pending.extend(fold.initial_values().rhs_iter());
                pending.extend(fold.update_values().rhs_iter());
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                pending.push(base);
                for subscript in subscripts.iter() {
                    push_subscript_expression(&mut pending, subscript);
                }
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                pending.extend([base, value]);
                for subscript in subscripts.iter() {
                    push_subscript_expression(&mut pending, subscript);
                }
            }
            dae::ExpressionOperation::Builtin { arguments, .. }
            | dae::ExpressionOperation::Call { arguments, .. } => {
                pending.extend(arguments.iter());
            }
            dae::ExpressionOperation::StringConversion { value, format, .. } => {
                pending.push(value);
                match format {
                    dae::StringConversionFormatView::Options {
                        minimum_length,
                        left_justified,
                        significant_digits,
                    } => {
                        pending.extend(minimum_length);
                        pending.extend(left_justified);
                        pending.extend(significant_digits);
                    }
                    dae::StringConversionFormatView::Format { value } => pending.push(value),
                }
            }
        }
    }
    false
}

fn push_subscript_expression<'dae>(
    pending: &mut Vec<dae::ExprId<'dae>>,
    subscript: dae::SubscriptView<'dae>,
) {
    if let dae::SubscriptView::Index { expression, .. }
    | dae::SubscriptView::Slice { expression, .. } = subscript
    {
        pending.push(expression);
    }
}

enum DerivativeRhs<'dae> {
    Explicit {
        expression: dae::ExprId<'dae>,
        scalar: usize,
    },
    Scaled {
        numerator: dae::ExprId<'dae>,
        numerator_scalar: usize,
        coefficient: dae::ExprId<'dae>,
        coefficient_scalar: usize,
        span: Span,
    },
}

fn derivative_rhs<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
    scalar: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    state: dae::StateId<'dae>,
    state_scalar: usize,
) -> Result<DerivativeRhs<'dae>, LowerError> {
    let selector = ScalarSelector::new(view, domain_point);
    let (residual, scalar) = selector.select_array_element(residual, scalar)?;
    let node = view
        .expression(residual)
        .expect("branded residual expression resolves");
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = node.operation()
    else {
        return Err(LowerError::non_computable(
            "state equation is not a subtractive derivative residual",
            node.provenance().span(),
        ));
    };
    let lhs_direct = is_target_derivative(&selector, lhs, scalar, state, state_scalar)?;
    let rhs_direct = is_target_derivative(&selector, rhs, scalar, state, state_scalar)?;
    if lhs_direct && !expression_contains_derivative(view, rhs) {
        return Ok(DerivativeRhs::Explicit {
            expression: rhs,
            scalar,
        });
    }
    if rhs_direct && !expression_contains_derivative(view, lhs) {
        return Ok(DerivativeRhs::Explicit {
            expression: lhs,
            scalar,
        });
    }
    let lhs_scaled = scaled_derivative_factor(&selector, lhs, scalar, state, state_scalar)?;
    let rhs_scaled = scaled_derivative_factor(&selector, rhs, scalar, state, state_scalar)?;
    match (lhs_scaled, rhs_scaled) {
        (Some((coefficient, coefficient_scalar)), None)
            if !expression_contains_derivative(view, rhs) =>
        {
            Ok(DerivativeRhs::Scaled {
                numerator: rhs,
                numerator_scalar: scalar,
                coefficient,
                coefficient_scalar,
                span: node.provenance().span(),
            })
        }
        (None, Some((coefficient, coefficient_scalar)))
            if !expression_contains_derivative(view, lhs) =>
        {
            Ok(DerivativeRhs::Scaled {
                numerator: lhs,
                numerator_scalar: scalar,
                coefficient,
                coefficient_scalar,
                span: node.provenance().span(),
            })
        }
        _ => Err(LowerError::non_computable(
            "matched derivative is not an isolated affine product",
            node.provenance().span(),
        )),
    }
}

fn is_target_derivative<'dae>(
    selector: &ScalarSelector<'dae>,
    expression: dae::ExprId<'dae>,
    scalar: usize,
    state: dae::StateId<'dae>,
    state_scalar: usize,
) -> Result<bool, LowerError> {
    Ok(matches!(
        selector.coordinate(expression, scalar)?,
        Some((dae::CoordinateView::Derivative(found), found_scalar))
            if found == state && found_scalar == state_scalar
    ))
}

fn scaled_derivative_factor<'dae>(
    selector: &ScalarSelector<'dae>,
    expression: dae::ExprId<'dae>,
    scalar: usize,
    state: dae::StateId<'dae>,
    state_scalar: usize,
) -> Result<Option<(dae::ExprId<'dae>, usize)>, LowerError> {
    let view = selector.view();
    let node = view
        .expression(expression)
        .expect("branded derivative term resolves");
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply,
        lhs,
        rhs,
    } = node.operation()
    else {
        return Ok(None);
    };
    let lhs_scalar = if scalar_count(view, lhs) == 1 {
        0
    } else {
        scalar
    };
    let rhs_scalar = if scalar_count(view, rhs) == 1 {
        0
    } else {
        scalar
    };
    let lhs_target = is_target_derivative(selector, lhs, lhs_scalar, state, state_scalar)?;
    let rhs_target = is_target_derivative(selector, rhs, rhs_scalar, state, state_scalar)?;
    let factor = match (lhs_target, rhs_target) {
        (true, false) => (rhs, rhs_scalar),
        (false, true) => (lhs, lhs_scalar),
        (true, true) => {
            return Err(LowerError::non_computable(
                "matched derivative occurs nonlinearly in a product",
                node.provenance().span(),
            ));
        }
        (false, false) => return Ok(None),
    };
    if expression_contains_derivative(view, factor.0) {
        return Err(LowerError::non_computable(
            "affine derivative coefficient contains another derivative",
            node.provenance().span(),
        ));
    }
    if selector.constant_real(factor.0, factor.1)? == 0.0 {
        return Err(LowerError::non_computable(
            "matched derivative has a zero affine coefficient",
            node.provenance().span(),
        ));
    }
    Ok(Some(factor))
}

fn lower_initialization<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
) -> Result<solve::InitializationSolveSystem, LowerError> {
    let mut rows = ScalarRows::default();
    for owner in view.initialization_owners() {
        match owner {
            dae::InitializationOwnerView::Residual { equation, .. } => {
                for scalar in 0..scalar_count(view, equation.residual()) {
                    let program = ScalarCompiler::new(view, layout, None)
                        .program(equation.residual(), scalar)?;
                    let output = rows.programs.len();
                    rows.push(program, equation.provenance().span(), output);
                }
            }
            dae::InitializationOwnerView::Structured { family, .. } => {
                lower_initialization_family(view, layout, family, &mut rows)?;
            }
        }
    }
    Ok(solve::InitializationSolveSystem {
        residual: rows.into_compute_block()?,
        ..solve::InitializationSolveSystem::default()
    })
}

fn lower_initialization_family<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    rows: &mut ScalarRows,
) -> Result<(), LowerError> {
    let domain = view
        .domain(family.domain())
        .expect("checked family domain resolves");
    for point in 0..domain.scalar_count() as usize {
        let values = domain
            .structured()
            .index_tuple_at(point)
            .expect("checked domain remains valid")
            .expect("checked point ordinal is in range");
        lower_initialization_family_point(view, layout, family, point, &values, rows)?;
    }
    Ok(())
}

fn lower_initialization_family_point<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    point: usize,
    values: &[i64],
    rows: &mut ScalarRows,
) -> Result<(), LowerError> {
    let domain = view
        .domain(family.domain())
        .expect("checked family domain resolves");
    for body in family.bodies().iter() {
        let scalar = family
            .scalar_view()
            .body_scalar(point, domain.extents())
            .expect("checked family view projects its domain point");
        let program = ScalarCompiler::new(view, layout, Some((family.domain(), values)))
            .program(body, scalar)?;
        let output = rows.programs.len();
        rows.push(program, family.provenance().span(), output);
    }
    Ok(())
}

#[derive(Default)]
pub(super) struct ScalarRows {
    programs: Vec<Vec<solve::LinearOp>>,
    spans: Vec<Span>,
    output_indices: Vec<usize>,
}

impl ScalarRows {
    pub(super) fn push(&mut self, program: Vec<solve::LinearOp>, span: Span, output: usize) {
        self.programs.push(program);
        self.spans.push(span);
        self.output_indices.push(output);
    }

    pub(super) fn into_scalar_block(self) -> Result<solve::ScalarProgramBlock, LowerError> {
        solve::ScalarProgramBlock::with_output_indices(
            self.programs,
            self.spans,
            self.output_indices,
        )
        .map_err(Into::into)
    }

    fn into_compute_block(self) -> Result<solve::ComputeBlock, LowerError> {
        Ok(solve::ComputeBlock::from_scalar_program_block(
            self.into_scalar_block()?,
        ))
    }
}

#[derive(Default)]
struct DerivativeRows {
    pieces: Vec<DerivativePiece>,
}

enum DerivativePiece {
    Scalar {
        program: Vec<solve::LinearOp>,
        span: Span,
        output: usize,
    },
    Tensor(Box<ImplicitTensorDerivative>),
}

impl DerivativePiece {
    fn output_start(&self) -> usize {
        match self {
            Self::Scalar { output, .. } => *output,
            Self::Tensor(group) => group.output_start,
        }
    }
}

impl DerivativeRows {
    fn push_scalar(&mut self, program: Vec<solve::LinearOp>, span: Span, output: usize) {
        self.pieces.push(DerivativePiece::Scalar {
            program,
            span,
            output,
        });
    }

    fn push_tensor(&mut self, group: ImplicitTensorDerivative) {
        self.pieces.push(DerivativePiece::Tensor(Box::new(group)));
    }

    fn into_compute_block(
        mut self,
        expected_outputs: usize,
        model_span: Span,
    ) -> Result<solve::ComputeBlock, LowerError> {
        self.pieces.sort_by_key(DerivativePiece::output_start);
        let mut nodes = Vec::new();
        let mut scalars = ScalarRows::default();
        let mut next_output = 0usize;
        for piece in self.pieces {
            let output_start = piece.output_start();
            if output_start != next_output {
                return Err(LowerError::non_computable(
                    format!(
                        "derivative programs must define every state scalar exactly once; expected output {next_output}, found {output_start}"
                    ),
                    piece_span(&piece),
                ));
            }
            match piece {
                DerivativePiece::Scalar {
                    program,
                    span,
                    output,
                } => {
                    scalars.push(program, span, output);
                    next_output = checked_ordinal_add(
                        next_output,
                        1,
                        "derivative output ordinal overflow",
                        span,
                    )?;
                }
                DerivativePiece::Tensor(group) => {
                    flush_scalar_rows(&mut scalars, &mut nodes)?;
                    next_output = checked_ordinal_add(
                        next_output,
                        group.rows,
                        "derivative output ordinal overflow",
                        group.span,
                    )?;
                    nodes.push(group.node);
                }
            }
        }
        flush_scalar_rows(&mut scalars, &mut nodes)?;
        if next_output != expected_outputs {
            return Err(LowerError::non_computable(
                format!(
                    "derivative programs define {next_output} state scalars, but the checked layout contains {expected_outputs}"
                ),
                model_span,
            ));
        }
        Ok(solve::ComputeBlock { nodes })
    }
}

fn checked_ordinal_add(
    value: usize,
    increment: usize,
    message: &'static str,
    span: Span,
) -> Result<usize, LowerError> {
    value
        .checked_add(increment)
        .ok_or_else(|| LowerError::contract(message, span))
}

fn piece_span(piece: &DerivativePiece) -> Span {
    match piece {
        DerivativePiece::Scalar { span, .. } => *span,
        DerivativePiece::Tensor(group) => group.span,
    }
}

fn flush_scalar_rows(
    rows: &mut ScalarRows,
    nodes: &mut Vec<solve::ComputeNode>,
) -> Result<(), LowerError> {
    if rows.programs.is_empty() {
        return Ok(());
    }
    let rows = std::mem::take(rows);
    nodes.push(solve::ComputeNode::ScalarPrograms(
        rows.into_scalar_block()?,
    ));
    Ok(())
}

pub(super) fn variable_scalar_slot(
    layout: &LoweredLayout<'_>,
    variable: u32,
    scalar: usize,
    span: Span,
) -> Result<solve::ScalarSlot, LowerError> {
    let entry = layout
        .variables
        .get(variable as usize)
        .copied()
        .ok_or_else(|| LowerError::contract("variable has no Solve layout entry", span))?;
    if scalar >= entry.count {
        return Err(LowerError::contract(
            format!(
                "variable scalar ordinal {scalar} exceeds its {}-scalar layout",
                entry.count
            ),
            span,
        ));
    }
    let index = entry
        .base
        .checked_add(scalar)
        .ok_or_else(|| LowerError::contract("variable scalar layout overflow", span))?;
    Ok(match entry.storage {
        StorageClass::Y => solve::scalar_slot_y(index),
        StorageClass::P => solve::scalar_slot_p(index),
    })
}

pub(super) fn pre_variable_scalar_slot(
    layout: &LoweredLayout<'_>,
    variable: u32,
    scalar: usize,
    span: Span,
) -> Result<solve::ScalarSlot, LowerError> {
    let entry = layout
        .variables
        .get(variable as usize)
        .copied()
        .ok_or_else(|| LowerError::contract("pre variable has no Solve layout entry", span))?;
    if scalar >= entry.count {
        return Err(LowerError::contract(
            format!(
                "pre variable scalar ordinal {scalar} exceeds its {}-scalar layout",
                entry.count
            ),
            span,
        ));
    }
    let base = layout
        .pre_variables
        .get(variable as usize)
        .copied()
        .flatten()
        .ok_or_else(|| LowerError::contract("variable has no checked pre-history lane", span))?;
    let index = base
        .checked_add(scalar)
        .ok_or_else(|| LowerError::contract("pre variable scalar layout overflow", span))?;
    Ok(solve::scalar_slot_p(index))
}

pub(super) fn previous_value_scalar_slot(
    layout: &LoweredLayout<'_>,
    previous: u32,
    scalar: usize,
    span: Span,
) -> Result<solve::ScalarSlot, LowerError> {
    let base = layout
        .previous_values
        .get(previous as usize)
        .copied()
        .ok_or_else(|| {
            LowerError::contract("previous value has no Solve history layout entry", span)
        })?;
    let index = base
        .checked_add(scalar)
        .ok_or_else(|| LowerError::contract("previous-value scalar layout overflow", span))?;
    Ok(solve::scalar_slot_p(index))
}

pub(super) fn delay_value_scalar_slot(
    layout: &LoweredLayout<'_>,
    delay: u32,
    scalar: usize,
    span: Span,
) -> Result<solve::ScalarSlot, LowerError> {
    let base = layout
        .delay_values
        .get(delay as usize)
        .copied()
        .ok_or_else(|| LowerError::contract("delay has no Solve value layout entry", span))?;
    let index = base
        .checked_add(scalar)
        .ok_or_else(|| LowerError::contract("delay-value scalar layout overflow", span))?;
    Ok(solve::scalar_slot_p(index))
}

fn coordinate_variable(coordinate: dae::CoordinateView<'_>) -> Option<u32> {
    match coordinate {
        dae::CoordinateView::Parameter(id) => Some(id.index()),
        dae::CoordinateView::Input(id) => Some(id.index()),
        dae::CoordinateView::State(id) | dae::CoordinateView::Derivative(id) => Some(id.index()),
        dae::CoordinateView::Algebraic(id) => Some(id.index()),
        dae::CoordinateView::DiscreteReal(id) => Some(id.index()),
        dae::CoordinateView::DiscreteValue(id) => Some(id.index()),
        dae::CoordinateView::Time
        | dae::CoordinateView::PreDiscreteReal(_)
        | dae::CoordinateView::PreDiscreteValue(_)
        | dae::CoordinateView::Condition(_)
        | dae::CoordinateView::Delay(_)
        | dae::CoordinateView::Previous(_)
        | dae::CoordinateView::Terminal(_)
        | dae::CoordinateView::Binder(_)
        | dae::CoordinateView::FunctionParameter(_) => None,
    }
}

fn pre_coordinate_variable(coordinate: dae::CoordinateView<'_>) -> Option<u32> {
    match coordinate {
        dae::CoordinateView::PreDiscreteReal(id) => Some(id.index()),
        dae::CoordinateView::PreDiscreteValue(id) => Some(id.index()),
        _ => None,
    }
}

fn compare_operator(operator: dae::BinaryOperator) -> solve::CompareOp {
    match operator {
        dae::BinaryOperator::Equal => solve::CompareOp::Eq,
        dae::BinaryOperator::NotEqual => solve::CompareOp::Ne,
        dae::BinaryOperator::Less => solve::CompareOp::Lt,
        dae::BinaryOperator::LessEqual => solve::CompareOp::Le,
        dae::BinaryOperator::Greater => solve::CompareOp::Gt,
        dae::BinaryOperator::GreaterEqual => solve::CompareOp::Ge,
        dae::BinaryOperator::Add
        | dae::BinaryOperator::Subtract
        | dae::BinaryOperator::Multiply
        | dae::BinaryOperator::Divide
        | dae::BinaryOperator::Power
        | dae::BinaryOperator::ElementwiseAdd
        | dae::BinaryOperator::ElementwiseSubtract
        | dae::BinaryOperator::ElementwiseMultiply
        | dae::BinaryOperator::ElementwiseDivide
        | dae::BinaryOperator::ElementwisePower
        | dae::BinaryOperator::And
        | dae::BinaryOperator::Or => unreachable!("non-comparison operator"),
    }
}

fn unary_builtin(builtin: dae::PureBuiltin) -> solve::UnaryOp {
    match builtin {
        dae::PureBuiltin::Abs => solve::UnaryOp::Abs,
        dae::PureBuiltin::Sign => solve::UnaryOp::Sign,
        dae::PureBuiltin::Sqrt => solve::UnaryOp::Sqrt,
        dae::PureBuiltin::Floor => solve::UnaryOp::Floor,
        dae::PureBuiltin::Ceil => solve::UnaryOp::Ceil,
        dae::PureBuiltin::Integer => solve::UnaryOp::Trunc,
        dae::PureBuiltin::Sin => solve::UnaryOp::Sin,
        dae::PureBuiltin::Cos => solve::UnaryOp::Cos,
        dae::PureBuiltin::Tan => solve::UnaryOp::Tan,
        dae::PureBuiltin::Asin => solve::UnaryOp::Asin,
        dae::PureBuiltin::Acos => solve::UnaryOp::Acos,
        dae::PureBuiltin::Atan => solve::UnaryOp::Atan,
        dae::PureBuiltin::Sinh => solve::UnaryOp::Sinh,
        dae::PureBuiltin::Cosh => solve::UnaryOp::Cosh,
        dae::PureBuiltin::Tanh => solve::UnaryOp::Tanh,
        dae::PureBuiltin::Exp => solve::UnaryOp::Exp,
        dae::PureBuiltin::Log => solve::UnaryOp::Log,
        dae::PureBuiltin::Log10 => solve::UnaryOp::Log10,
        dae::PureBuiltin::Atan2
        | dae::PureBuiltin::Div
        | dae::PureBuiltin::Mod
        | dae::PureBuiltin::Rem
        | dae::PureBuiltin::Smooth
        | dae::PureBuiltin::NoEvent
        | dae::PureBuiltin::Homotopy
        | dae::PureBuiltin::Min
        | dae::PureBuiltin::Max
        | dae::PureBuiltin::Sum
        | dae::PureBuiltin::Product
        | dae::PureBuiltin::Size
        | dae::PureBuiltin::Zeros
        | dae::PureBuiltin::Ones
        | dae::PureBuiltin::Fill
        | dae::PureBuiltin::Linspace
        | dae::PureBuiltin::Cross => unreachable!("non-unary builtin"),
    }
}

fn integer_binary(
    operator: dae::BinaryOperator,
    lhs: i64,
    rhs: i64,
    span: Span,
) -> Result<i64, LowerError> {
    let overflow = || LowerError::contract("integer evaluation overflow", span);
    match operator {
        dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => {
            lhs.checked_add(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
            lhs.checked_sub(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
            lhs.checked_mul(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide if rhs != 0 => {
            lhs.checked_div(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower if rhs >= 0 => lhs
            .checked_pow(u32::try_from(rhs).map_err(|_| overflow())?)
            .ok_or_else(overflow),
        _ => Err(LowerError::non_computable(
            "subscript expression is not integer arithmetic",
            span,
        )),
    }
}

fn checked_index(index: i64, extent: u32, span: Span) -> Result<u32, LowerError> {
    if index < 1 || index > i64::from(extent) {
        return Err(LowerError::non_computable(
            format!("Modelica index {index} is outside extent {extent}"),
            span,
        ));
    }
    Ok(u32::try_from(index - 1).expect("positive in-range u32 index"))
}

fn row_major_coordinates(extents: &[u32], index: usize) -> Option<Vec<u32>> {
    let count = extents
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))?;
    if index >= count {
        return None;
    }
    let mut remainder = index;
    let mut coordinates = Vec::with_capacity(extents.len());
    for extent in extents.iter().rev() {
        if *extent == 0 {
            return None;
        }
        coordinates.push(u32::try_from(remainder % *extent as usize).ok()?);
        remainder /= *extent as usize;
    }
    coordinates.reverse();
    Some(coordinates)
}

fn flatten_coordinates(extents: &[u32], coordinates: &[u32]) -> Option<usize> {
    if extents.len() != coordinates.len() {
        return None;
    }
    extents
        .iter()
        .zip(coordinates)
        .try_fold(0usize, |flat, (extent, coordinate)| {
            if coordinate >= extent {
                return None;
            }
            flat.checked_mul(*extent as usize)?
                .checked_add(*coordinate as usize)
        })
}

fn scalar_count<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> usize {
    view.expression(expression)
        .expect("branded expression resolves")
        .value_type()
        .scalar_count()
        .expect("checked expression scalar capacity")
}

fn owner_provenance(owner: dae::ContinuousOwnerView<'_>) -> dae::DaeProvenance {
    match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => equation.provenance(),
        dae::ContinuousOwnerView::Structured { family, .. } => family.provenance(),
    }
}

fn first_model_span(view: dae::DaeView<'_>) -> Span {
    view.responsible_span()
        .expect("nonempty checked DAE has responsible provenance")
}
