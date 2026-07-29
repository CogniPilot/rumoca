use std::collections::HashMap;

use rumoca_core::{ComprehensionScalarView, Span};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{EquationRef, UnknownId};

use crate::LowerError;
use crate::layout::{LoweredLayout, StorageClass, lower_layout};

mod clocks;
mod events;
mod scalar;
use scalar::{ScalarCompiler, ScalarSelector};

pub(crate) fn lower_solve_problem<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<solve::SolveProblem, LowerError> {
    if view.variable_count() == 0
        && view.continuous_owner_count() == 0
        && view.initialization_owner_count() == 0
        && view.discrete_assignment_count() == 0
    {
        return Err(LowerError::unspanned_non_computable(
            "the model has no variables or equations to simulate",
        ));
    }
    reject_unimplemented_systems(view)?;
    let lowered = lower_layout(view)?;
    let clocks = clocks::lower_clocks(view)?;
    let matching = structural_matching(view)?;
    let continuous = lower_continuous(view, &lowered, &matching)?;
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
    let unsupported = [
        (view.previous_value_count(), "previous-value history"),
        (view.terminal_count(), "terminal coordinates"),
        (view.delay_count(), "transport delays"),
    ];
    if let Some((_, semantics)) = unsupported.into_iter().find(|(count, _)| *count != 0) {
        return Err(LowerError::unsupported(
            format!("{semantics} do not yet have checked Solve lowering"),
            first_model_span(view),
        ));
    }
    Ok(())
}

fn structural_matching<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<HashMap<usize, UnknownId<'dae>>, LowerError> {
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
        return Ok(HashMap::new());
    }
    let sorted = rumoca_phase_structural::sort(view).map_err(|error| LowerError::Structural {
        reason: error.to_string(),
        span: error.source_span(),
    })?;
    Ok(sorted
        .matching
        .into_iter()
        .map(|(EquationRef(equation), unknown)| (equation, unknown))
        .collect())
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
    matching: &HashMap<usize, UnknownId<'dae>>,
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
                    matching,
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
                        matching,
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
                    matching,
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
        lower_algebraic_projection(view, layout, matching)?;
    Ok(solve::ContinuousSolveSystem {
        implicit_rhs: residual.clone(),
        implicit_row_targets,
        algebraic_projection_plan,
        residual,
        manifold_residual: solve::ComputeBlock::default(),
        manifold_projection_plan: solve::AlgebraicProjectionPlan::default(),
        derivative_rhs: derivative.into_compute_block(
            layout.solve_layout.state_scalar_count(),
            first_model_span(view),
        )?,
    })
}

fn lower_algebraic_projection<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    matching: &HashMap<usize, UnknownId<'dae>>,
) -> Result<
    (
        Vec<Option<solve::ScalarSlot>>,
        solve::AlgebraicProjectionPlan,
    ),
    LowerError,
> {
    let solver_count = layout.solve_layout.solver_scalar_count();
    let state_count = layout.solve_layout.state_scalar_count();
    let mut targets = vec![None; solver_count];
    for unknown in matching.values().copied() {
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
    let rows = algebraic_indices.clone().collect::<Vec<_>>();
    let blocks = if rows.is_empty() {
        Vec::new()
    } else {
        vec![solve::AlgebraicProjectionBlock {
            rows: rows.clone(),
            y_indices: rows,
        }]
    };
    Ok((targets, solve::AlgebraicProjectionPlan { blocks }))
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
    for body in family.bodies().iter() {
        let scalar = match family.scalar_view() {
            ComprehensionScalarView::BinderSubstitution => 0,
            ComprehensionScalarView::RowMajorProjection => point,
        };
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
            let rhs = explicit_derivative_rhs(
                view,
                expression,
                scalar,
                domain_point,
                state,
                target as usize,
            )?;
            let program = ScalarCompiler::new(view, layout, domain_point).program(rhs.0, rhs.1)?;
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
            | dae::ExpressionOperation::Range { .. }
            | dae::ExpressionOperation::FunctionFoldParameter { .. } => {}
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
                pending.push(definition);
            }
            dae::ExpressionOperation::FunctionFoldOutput { fold, .. } => {
                let fold = view
                    .function_fold(fold)
                    .expect("checked function fold identity resolves");
                pending.extend(fold.initial_values().iter());
                pending.extend(fold.update_values().iter());
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

fn explicit_derivative_rhs<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
    scalar: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    state: dae::StateId<'dae>,
    state_scalar: usize,
) -> Result<(dae::ExprId<'dae>, usize), LowerError> {
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
            "state equation is not an explicit `der(x) - rhs` residual",
            node.provenance().span(),
        ));
    };
    let selector = ScalarSelector::new(view, domain_point);
    if matches!(
        selector.coordinate(lhs, scalar)?,
        Some((dae::CoordinateView::Derivative(found), found_scalar))
            if found == state && found_scalar == state_scalar
    ) {
        return Ok((rhs, scalar));
    }
    if matches!(
        selector.coordinate(rhs, scalar)?,
        Some((dae::CoordinateView::Derivative(found), found_scalar))
            if found == state && found_scalar == state_scalar
    ) {
        return Err(LowerError::non_computable(
            "state equation uses `rhs - der(x)`; normalization is required upstream",
            node.provenance().span(),
        ));
    }
    Err(LowerError::non_computable(
        "matched derivative is not the explicit left operand",
        node.provenance().span(),
    ))
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
    for body in family.bodies().iter() {
        let scalar = match family.scalar_view() {
            ComprehensionScalarView::BinderSubstitution => 0,
            ComprehensionScalarView::RowMajorProjection => point,
        };
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
        | dae::PureBuiltin::Mod
        | dae::PureBuiltin::Smooth
        | dae::PureBuiltin::NoEvent
        | dae::PureBuiltin::Min
        | dae::PureBuiltin::Max
        | dae::PureBuiltin::Sum
        | dae::PureBuiltin::Product
        | dae::PureBuiltin::Size
        | dae::PureBuiltin::Zeros => unreachable!("non-unary builtin"),
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
