use indexmap::IndexMap;
use rumoca_core::{ExpressionRewriter, ExpressionScope, ExpressionVisitor};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::{lower, lower::LowerError, stencil};

mod ordering;

pub(crate) fn build_residual_compute_block(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    residual_rows: &[Vec<solve::LinearOp>],
    residual_targets: &[Option<solve::ScalarSlot>],
    residual_equations: &[(usize, &dae::Equation)],
    declines: &mut crate::tensor_declines::TensorDeclineJournal,
) -> Result<solve::ComputeBlock, LowerError> {
    let span = residual_context_span(dae_model, residual_equations);
    validate_residual_compute_block_contract(
        residual_rows.len(),
        residual_targets.len(),
        residual_equations,
        span,
    )?;
    let native_nodes =
        lower::lower_native_algebraic_residual_nodes(dae_model, layout, residual_equations)?;
    let (direct_nodes, compact_coverage) =
        build_direct_family_nodes(dae_model, layout, residual_targets, residual_equations)?;
    let mut rows = residual_vec_with_capacity(
        residual_rows.len(),
        "residual structured program row count",
        span,
    )?;
    let y_slot_ranges = stencil::structured_y_slot_ranges(layout)?;
    let structural_bindings = lower::structural_bindings_for_structured_access(dae_model)?;
    let mut residual_index = 0usize;
    for (equation_index, equation) in residual_equations {
        let scalar_count = equation.scalar_count.max(1);
        for row_offset in 0..scalar_count {
            let ops = residual_rows.get(residual_index).cloned().ok_or_else(|| {
                residual_contract_error(
                    format!("residual row {residual_index} disappeared after row-count validation"),
                    Some(equation.span),
                )
            })?;
            if compact_coverage
                .iter()
                .any(|coverage| coverage.contains(residual_index))
                || native_nodes.iter().any(|(start, count, _)| {
                    CompactOutputCoverage {
                        start: *start,
                        step: 1,
                        count: *count,
                    }
                    .contains(residual_index)
                })
            {
                residual_index += 1;
                continue;
            }
            let target = residual_targets.get(residual_index).copied().flatten();
            let pointwise_output_y_index = target_y_index(target);
            let producer_load_strides = stencil::producer_load_strides_for_dae_equation(
                layout,
                &dae_model.continuous.structured_equations,
                *equation_index,
                &ops,
                equation.span,
            )?;
            rows.push(stencil::StructuredProgram {
                load_y_ranges: stencil::structured_load_y_ranges(
                    &ops,
                    &y_slot_ranges,
                    equation.span,
                )?,
                ops,
                output_index: residual_index,
                pointwise_output_y_index,
                span: equation.span,
                output_y_range: residual_output_y_range(
                    target,
                    &y_slot_ranges,
                    residual_index,
                    equation.span,
                )?,
                dae_equation_index: Some(*equation_index),
                producer_load_strides,
                access_proof: residual_row_access_proof(
                    layout,
                    &structural_bindings,
                    &equation.rhs,
                    equation.span,
                    row_offset,
                    scalar_count,
                )?,
            });
            residual_index += 1;
        }
    }
    ordering::assemble_residual_compute_block(ordering::ResidualAssemblyInput {
        rows: &mut rows,
        native_nodes,
        direct_nodes,
        compact_coverage: &compact_coverage,
        structured_equations: &dae_model.continuous.structured_equations,
        dae_equations: &dae_model.continuous.equations,
        declines,
        expected_output_count: residual_rows.len(),
        span,
    })
}

#[derive(Debug, Clone, Copy)]
pub(super) struct CompactOutputCoverage {
    start: usize,
    step: usize,
    count: usize,
}

impl CompactOutputCoverage {
    fn contains(self, output: usize) -> bool {
        let Some(delta) = output.checked_sub(self.start) else {
            return false;
        };
        self.step > 0 && delta % self.step == 0 && delta / self.step < self.count
    }

    fn intersects(self, other: Self) -> bool {
        if self.count == 0 || other.count == 0 || self.step == 0 || other.step == 0 {
            return false;
        }
        let Some(self_end) = progression_end(self) else {
            return true;
        };
        let Some(other_end) = progression_end(other) else {
            return true;
        };
        let lower = self.start.max(other.start);
        let upper = self_end.min(other_end);
        if lower > upper {
            return false;
        }

        progression_common_value_at_or_after(self, other, lower)
            .is_some_and(|candidate| candidate <= upper)
    }
}

fn progression_end(coverage: CompactOutputCoverage) -> Option<usize> {
    coverage
        .count
        .checked_sub(1)?
        .checked_mul(coverage.step)?
        .checked_add(coverage.start)
}

/// Return the first common value at or after `lower` for two arithmetic
/// progressions, or `None` when their congruence classes are disjoint.
///
/// Direct structured-family outputs are interleaved progressions (one per
/// equation position). Checking their intersection must remain independent of
/// the number of family points: enumerating either progression here would
/// silently turn compact lowering back into an O(N) pass.
fn progression_common_value_at_or_after(
    lhs: CompactOutputCoverage,
    rhs: CompactOutputCoverage,
    lower: usize,
) -> Option<usize> {
    let lhs_start = i128::try_from(lhs.start).ok()?;
    let rhs_start = i128::try_from(rhs.start).ok()?;
    let lhs_step = i128::try_from(lhs.step).ok()?;
    let rhs_step = i128::try_from(rhs.step).ok()?;
    let lower = i128::try_from(lower).ok()?;
    let gcd = integer_gcd(lhs_step, rhs_step);
    let difference = rhs_start.checked_sub(lhs_start)?;
    if difference % gcd != 0 {
        return None;
    }

    let lhs_reduced = lhs_step / gcd;
    let rhs_reduced = rhs_step / gcd;
    let multiplier = if rhs_reduced == 1 {
        0
    } else {
        let inverse = modular_inverse(lhs_reduced.rem_euclid(rhs_reduced), rhs_reduced)?;
        difference
            .checked_div(gcd)?
            .rem_euclid(rhs_reduced)
            .checked_mul(inverse)?
            .rem_euclid(rhs_reduced)
    };
    let first = lhs_start.checked_add(lhs_step.checked_mul(multiplier)?)?;
    let period = lhs_step.checked_mul(rhs_reduced)?;
    let candidate = if first >= lower {
        first
    } else {
        let distance = lower.checked_sub(first)?;
        let periods = distance.checked_add(period.checked_sub(1)?)? / period;
        first.checked_add(periods.checked_mul(period)?)?
    };
    usize::try_from(candidate).ok()
}

fn integer_gcd(mut lhs: i128, mut rhs: i128) -> i128 {
    while rhs != 0 {
        (lhs, rhs) = (rhs, lhs % rhs);
    }
    lhs.abs()
}

fn modular_inverse(value: i128, modulus: i128) -> Option<i128> {
    let (mut old_remainder, mut remainder) = (value, modulus);
    let (mut old_coefficient, mut coefficient) = (1i128, 0i128);
    while remainder != 0 {
        let quotient = old_remainder / remainder;
        (old_remainder, remainder) = (
            remainder,
            old_remainder.checked_sub(quotient.checked_mul(remainder)?)?,
        );
        (old_coefficient, coefficient) = (
            coefficient,
            old_coefficient.checked_sub(quotient.checked_mul(coefficient)?)?,
        );
    }
    (old_remainder == 1).then(|| old_coefficient.rem_euclid(modulus))
}

#[derive(Debug, Clone, Copy)]
struct ResidualOutputSpan {
    start: usize,
    count: usize,
}

fn build_direct_family_nodes(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    residual_targets: &[Option<solve::ScalarSlot>],
    residual_equations: &[(usize, &dae::Equation)],
) -> Result<(Vec<solve::ComputeNode>, Vec<CompactOutputCoverage>), LowerError> {
    let output_spans = residual_output_spans(residual_equations)?;
    let y_slot_ranges = stencil::structured_y_slot_ranges(layout)?;
    let structural_bindings = lower::structural_bindings_for_structured_access(dae_model)?;
    let variables = ProjectionVariableIndex::new(dae_model);
    let position_context = DirectFamilyPositionContext {
        layout,
        residual_targets,
        output_spans: &output_spans,
        y_slot_ranges: &y_slot_ranges,
        structural_bindings: &structural_bindings,
    };
    let mut nodes = Vec::new();
    let mut coverage = Vec::new();
    for family in &dae_model.continuous.structured_equations {
        // This compute block may be built from a filtered view of the
        // continuous equations (for example, initialization excludes all
        // state-derivative rows). A family whose owner is absent from that
        // view is not a residual-family candidate here. Reject it before
        // lowering its template body so an intentionally excluded der()
        // expression cannot leak into generic residual lowering.
        if !output_spans.contains_key(&family.first_equation_index) {
            continue;
        }
        let Some(template) = family.template.as_ref() else {
            continue;
        };
        if !compact_direct_family_is_proven(family, template) {
            continue;
        }
        if template.body.len() != family.equations_per_point {
            return Err(residual_contract_error(
                "structured template body count does not match equations_per_point".to_string(),
                Some(family.span),
            ));
        }
        for equation_position in 0..family.equations_per_point {
            let Some((node, covered)) = direct_family_position_node(
                dae_model,
                &variables,
                &position_context,
                family,
                template,
                equation_position,
            )?
            else {
                continue;
            };
            if coverage
                .iter()
                .any(|existing: &CompactOutputCoverage| existing.intersects(covered))
            {
                return Err(residual_contract_error(
                    "structured family output maps overlap".to_string(),
                    Some(family.span),
                ));
            }
            nodes.push(node);
            coverage.push(covered);
        }
    }
    Ok((nodes, coverage))
}

fn compact_direct_family_is_proven(
    family: &dae::StructuredEquationFamily,
    template: &rumoca_core::ComprehensionTemplate,
) -> bool {
    if !direct_template_identity_is_safe(family, template) {
        return false;
    }
    match template.scalar_view {
        // Producer load strides are available only when Flat IR proved every
        // source subscript affine in the binders and binders do not otherwise
        // contribute values to the body. Without both guards, `u[i*i]` or
        // `x[i] = i*i` could not be represented by affine access metadata.
        rumoca_core::ComprehensionScalarView::BinderSubstitution => {
            family.regular.is_some()
                && template
                    .body
                    .iter()
                    .all(|body| binders_are_confined_to_subscripts(body, family))
        }
        // These indices are synthesized from the validated row-major domain,
        // rather than inferred from source subscript expressions.
        rumoca_core::ComprehensionScalarView::RowMajorProjection => true,
    }
}

fn direct_template_identity_is_safe(
    family: &dae::StructuredEquationFamily,
    template: &rumoca_core::ComprehensionTemplate,
) -> bool {
    template.body.iter().all(|body| {
        let mut proof = DirectTemplateIdentityProof {
            family_binders: &family.domain.binders,
            valid: true,
        };
        proof.visit_expression(body);
        proof.valid
    })
}

struct DirectTemplateIdentityProof<'a> {
    family_binders: &'a [rumoca_core::StructuredIndexBinder],
    valid: bool,
}

impl ExpressionVisitor for DirectTemplateIdentityProof<'_> {
    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if matches!(
            function,
            rumoca_core::BuiltinFunction::Terminal
                | rumoca_core::BuiltinFunction::Delay
                | rumoca_core::BuiltinFunction::Pre
                | rumoca_core::BuiltinFunction::Der
        ) {
            self.valid = false;
            return;
        }
        self.walk_builtin_call(function, args);
    }

    fn enter_scope(&mut self, scope: ExpressionScope<'_>) {
        let ExpressionScope::ArrayComprehension(indices) = scope;
        if indices.iter().any(|index| {
            self.family_binders
                .iter()
                .any(|binder| binder.display_name == index.name)
        }) {
            // Names are not sufficient to distinguish the outer family binder
            // from a same-named nested comprehension binder. Until binder IDs
            // are carried on every use, direct substitution must decline.
            self.valid = false;
        }
    }
}

fn binders_are_confined_to_subscripts(
    expression: &rumoca_core::Expression,
    family: &dae::StructuredEquationFamily,
) -> bool {
    let mut proof = BinderSubscriptUseProof {
        binders: &family.domain.binders,
        inside_subscript: false,
        valid: true,
    };
    proof.visit_expression(expression);
    proof.valid
}

struct BinderSubscriptUseProof<'a> {
    binders: &'a [rumoca_core::StructuredIndexBinder],
    inside_subscript: bool,
    valid: bool,
}

impl ExpressionVisitor for BinderSubscriptUseProof<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if !self.inside_subscript
            && self
                .binders
                .iter()
                .any(|binder| binder.display_name == name.as_str())
        {
            self.valid = false;
        }
        self.walk_var_ref(name, subscripts);
    }

    fn visit_subscript(&mut self, subscript: &rumoca_core::Subscript) {
        let was_inside = self.inside_subscript;
        self.inside_subscript = true;
        self.walk_subscript(subscript);
        self.inside_subscript = was_inside;
    }
}

struct DirectFamilyPositionContext<'a> {
    layout: &'a solve::VarLayout,
    residual_targets: &'a [Option<solve::ScalarSlot>],
    output_spans: &'a IndexMap<usize, ResidualOutputSpan>,
    y_slot_ranges: &'a stencil::YSlotRanges,
    structural_bindings: &'a IndexMap<String, f64>,
}

fn direct_family_position_node(
    dae_model: &dae::Dae,
    variables: &ProjectionVariableIndex<'_>,
    context: &DirectFamilyPositionContext<'_>,
    family: &dae::StructuredEquationFamily,
    template: &rumoca_core::ComprehensionTemplate,
    equation_position: usize,
) -> Result<Option<(solve::ComputeNode, CompactOutputCoverage)>, LowerError> {
    let point_count = family.point_count().map_err(|err| {
        residual_contract_error(
            format!("structured family domain is invalid: {err}"),
            Some(family.span),
        )
    })?;
    if point_count < 2 {
        return Ok(None);
    }
    let Some(corners) = direct_corner_expressions(
        family,
        template,
        equation_position,
        context.layout,
        variables,
    )?
    else {
        return Ok(None);
    };
    let corner_ops = lower::lower_compact_residual_expressions(
        dae_model,
        context.layout,
        &corners.expressions,
        family.span,
    )?;
    let mut programs = Vec::with_capacity(corners.ordinals.len());
    for ((ordinal, expression), ops) in corners
        .ordinals
        .iter()
        .zip(&corners.expressions)
        .zip(corner_ops)
    {
        let Some(program) = direct_corner_program(DirectCornerProgramInput {
            layout: context.layout,
            residual_targets: context.residual_targets,
            output_spans: context.output_spans,
            y_slot_ranges: context.y_slot_ranges,
            structural_bindings: context.structural_bindings,
            family,
            equation_position,
            ordinal: *ordinal,
            expression,
            ops,
        })?
        else {
            return Ok(None);
        };
        programs.push(program);
    }
    let Some(node) = stencil::tensor_node_from_compact_corners(
        &programs,
        &family.domain,
        family.span,
        family.regular.is_some(),
    )?
    else {
        return Ok(None);
    };
    let Some(start) = family_output_index(family, 0, equation_position, context.output_spans)?
    else {
        return Ok(None);
    };
    let Some(second) = family_output_index(family, 1, equation_position, context.output_spans)?
    else {
        return Ok(None);
    };
    let Some(step) = second.checked_sub(start).filter(|step| *step > 0) else {
        return Ok(None);
    };
    Ok(Some((
        node,
        CompactOutputCoverage {
            start,
            step,
            count: point_count,
        },
    )))
}

struct DirectCornerExpressions {
    ordinals: Vec<usize>,
    expressions: Vec<rumoca_core::Expression>,
}

fn direct_corner_expressions(
    family: &dae::StructuredEquationFamily,
    template: &rumoca_core::ComprehensionTemplate,
    equation_position: usize,
    layout: &solve::VarLayout,
    variables: &ProjectionVariableIndex<'_>,
) -> Result<Option<DirectCornerExpressions>, LowerError> {
    let ordinals = family.domain.corner_ordinals().map_err(|err| {
        residual_contract_error(
            format!("structured family domain is invalid: {err}"),
            Some(family.span),
        )
    })?;
    let mut expressions = Vec::with_capacity(ordinals.len());
    for ordinal in &ordinals {
        let Some(tuple) = family.domain.index_tuple_at(*ordinal).map_err(|err| {
            residual_contract_error(
                format!("structured family domain is invalid: {err}"),
                Some(family.span),
            )
        })?
        else {
            return Ok(None);
        };
        let Some(expression) = template_scalar_expression(
            family,
            template,
            equation_position,
            &tuple,
            layout,
            variables,
        )?
        else {
            return Ok(None);
        };
        expressions.push(expression);
    }
    Ok(Some(DirectCornerExpressions {
        ordinals,
        expressions,
    }))
}

struct DirectCornerProgramInput<'a> {
    layout: &'a solve::VarLayout,
    residual_targets: &'a [Option<solve::ScalarSlot>],
    output_spans: &'a IndexMap<usize, ResidualOutputSpan>,
    y_slot_ranges: &'a stencil::YSlotRanges,
    structural_bindings: &'a IndexMap<String, f64>,
    family: &'a dae::StructuredEquationFamily,
    equation_position: usize,
    ordinal: usize,
    expression: &'a rumoca_core::Expression,
    ops: Vec<solve::LinearOp>,
}

fn direct_corner_program(
    input: DirectCornerProgramInput<'_>,
) -> Result<Option<stencil::StructuredProgram>, LowerError> {
    let DirectCornerProgramInput {
        layout,
        residual_targets,
        output_spans,
        y_slot_ranges,
        structural_bindings,
        family,
        equation_position,
        ordinal,
        expression,
        ops,
    } = input;
    let Some(output_index) = family_output_index(family, ordinal, equation_position, output_spans)?
    else {
        return Ok(None);
    };
    let target = residual_targets.get(output_index).copied().flatten();
    let Some(access_proof) =
        residual_row_access_proof(layout, structural_bindings, expression, family.span, 0, 1)?
    else {
        return Ok(None);
    };
    let producer_load_strides =
        stencil::producer_load_strides_for_family_row(layout, family, ordinal, &ops, family.span)?;
    Ok(Some(stencil::StructuredProgram {
        load_y_ranges: stencil::structured_load_y_ranges(&ops, y_slot_ranges, family.span)?,
        ops,
        output_index,
        pointwise_output_y_index: target_y_index(target),
        span: family.span,
        output_y_range: residual_output_y_range(target, y_slot_ranges, output_index, family.span)?,
        dae_equation_index: None,
        producer_load_strides,
        access_proof: Some(access_proof),
    }))
}

fn residual_output_spans(
    residual_equations: &[(usize, &dae::Equation)],
) -> Result<IndexMap<usize, ResidualOutputSpan>, LowerError> {
    let mut spans = IndexMap::new();
    let mut start = 0usize;
    for (equation_index, equation) in residual_equations {
        let count = equation.scalar_count.max(1);
        spans.insert(*equation_index, ResidualOutputSpan { start, count });
        start = start.checked_add(count).ok_or_else(|| {
            residual_contract_error(
                "residual output span overflows host index".to_string(),
                Some(equation.span),
            )
        })?;
    }
    Ok(spans)
}

fn family_output_index(
    family: &dae::StructuredEquationFamily,
    ordinal: usize,
    equation_position: usize,
    output_spans: &IndexMap<usize, ResidualOutputSpan>,
) -> Result<Option<usize>, LowerError> {
    let point_count = family.point_count().map_err(|error| {
        residual_contract_error(
            format!("structured family domain is invalid: {error}"),
            Some(family.span),
        )
    })?;
    let expected_owner_count = point_count
        .checked_mul(family.equations_per_point)
        .ok_or_else(|| {
            residual_contract_error(
                "structured family owner scalar count overflows host index".to_string(),
                Some(family.span),
            )
        })?;
    let scalar_position = ordinal
        .checked_mul(family.equations_per_point)
        .and_then(|value| value.checked_add(equation_position))
        .ok_or_else(|| {
            residual_contract_error(
                "structured family scalar position overflows host index".to_string(),
                Some(family.span),
            )
        })?;
    let Some(owner) = output_spans.get(&family.first_equation_index) else {
        return Ok(None);
    };
    if owner.count == expected_owner_count {
        return owner
            .start
            .checked_add(scalar_position)
            .map(Some)
            .ok_or_else(|| {
                residual_contract_error(
                    "structured family output index overflows host index".to_string(),
                    Some(family.span),
                )
            });
    }

    // The aggregate owner was expanded into its explicit scalar view. Only in
    // this representation may subsequent equation indices be consulted; doing
    // so for a compact owner would claim unrelated neighboring equations.
    let scalar_equation_index = family
        .first_equation_index
        .checked_add(scalar_position)
        .ok_or_else(|| {
            residual_contract_error(
                "structured family equation index overflows host index".to_string(),
                Some(family.span),
            )
        })?;
    let Some(span) = output_spans.get(&scalar_equation_index) else {
        return Ok(None);
    };
    Ok((span.count == 1).then_some(span.start))
}

fn template_scalar_expression(
    family: &dae::StructuredEquationFamily,
    template: &rumoca_core::ComprehensionTemplate,
    equation_position: usize,
    tuple: &[i64],
    layout: &solve::VarLayout,
    variables: &ProjectionVariableIndex<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let Some(body) = template.body.get(equation_position) else {
        return Err(residual_contract_error(
            "structured template equation position is out of range".to_string(),
            Some(family.span),
        ));
    };
    match template.scalar_view {
        rumoca_core::ComprehensionScalarView::BinderSubstitution => {
            let values = family
                .domain
                .binders
                .iter()
                .zip(tuple)
                .map(|(binder, value)| (binder.display_name.clone(), *value))
                .collect();
            Ok(Some(
                TemplateBinderSubstituter { values }.rewrite_expression(body),
            ))
        }
        rumoca_core::ComprehensionScalarView::RowMajorProjection => {
            let mut indices = Vec::with_capacity(tuple.len());
            for (binder, value) in family.domain.binders.iter().zip(tuple) {
                let Some(position) = binder.position_of(*value).map_err(|err| {
                    residual_contract_error(
                        format!("structured family coordinate is invalid: {err}"),
                        Some(family.span),
                    )
                })?
                else {
                    return Err(residual_contract_error(
                        "structured family coordinate lies outside its domain".to_string(),
                        Some(family.span),
                    ));
                };
                let one_based_position = position.checked_add(1).ok_or_else(|| {
                    residual_contract_error(
                        "structured family projection index overflows host index".to_string(),
                        Some(family.span),
                    )
                })?;
                let index = i64::try_from(one_based_position).map_err(|_| {
                    residual_contract_error(
                        "structured family projection index exceeds i64".to_string(),
                        Some(family.span),
                    )
                })?;
                indices.push(index);
            }
            Ok(project_aggregate_element(body, &indices, layout, variables)
                .and_then(|projection| projection.family_shaped.then_some(projection.expression)))
        }
    }
}

struct AggregateProjection {
    expression: rumoca_core::Expression,
    family_shaped: bool,
}

fn project_aggregate_element(
    expression: &rumoca_core::Expression,
    indices: &[i64],
    layout: &solve::VarLayout,
    variables: &ProjectionVariableIndex<'_>,
) -> Option<AggregateProjection> {
    use rumoca_core::Expression;
    match expression {
        Expression::VarRef {
            name,
            subscripts,
            span,
        } => project_aggregate_var_ref(
            expression, name, subscripts, *span, indices, layout, variables,
        ),
        Expression::Binary { op, lhs, rhs, span } => {
            project_aggregate_binary(op, lhs, rhs, *span, indices, layout, variables)
        }
        Expression::Unary { op, rhs, span } => {
            let rhs = project_aggregate_element(rhs, indices, layout, variables)?;
            Some(AggregateProjection {
                expression: Expression::Unary {
                    op: op.clone(),
                    rhs: Box::new(rhs.expression),
                    span: *span,
                },
                family_shaped: rhs.family_shaped,
            })
        }
        Expression::BuiltinCall {
            function,
            args,
            span,
        } if aggregate_builtin_is_pointwise(*function, args.len()) => {
            let projected = args
                .iter()
                .map(|arg| project_aggregate_element(arg, indices, layout, variables))
                .collect::<Option<Vec<_>>>()?;
            let family_shaped = projected.iter().any(|arg| arg.family_shaped);
            Some(AggregateProjection {
                expression: Expression::BuiltinCall {
                    function: *function,
                    args: projected.into_iter().map(|arg| arg.expression).collect(),
                    span: *span,
                },
                family_shaped,
            })
        }
        Expression::If {
            branches,
            else_branch,
            span,
        } => project_aggregate_if(branches, else_branch, *span, indices, layout, variables),
        Expression::Literal { .. } | Expression::Empty { .. } => Some(AggregateProjection {
            expression: expression.clone(),
            family_shaped: false,
        }),
        Expression::FunctionCall { .. }
        | Expression::Array { .. }
        | Expression::Tuple { .. }
        | Expression::Range { .. }
        | Expression::ArrayComprehension { .. }
        | Expression::Index { .. }
        | Expression::FieldAccess { .. }
        | Expression::BuiltinCall { .. } => None,
    }
}

fn project_aggregate_var_ref(
    original: &rumoca_core::Expression,
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    indices: &[i64],
    layout: &solve::VarLayout,
    variables: &ProjectionVariableIndex<'_>,
) -> Option<AggregateProjection> {
    if !subscripts.is_empty() {
        if subscripts
            .iter()
            .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
            || layout
                .shape(name.as_str())
                .is_some_and(|shape| subscripts.len() != shape.len())
        {
            // A colon or partial subscript still denotes an array. Projecting
            // it unchanged into a scalar corner body would either miscompile
            // or turn an otherwise-supported scalar fallback into an error.
            return None;
        }
        return Some(AggregateProjection {
            expression: original.clone(),
            family_shaped: false,
        });
    }
    let Some(shape) = layout.shape(name.as_str()) else {
        if let Some(expression) =
            project_scalarized_aggregate_element(variables, layout, name, indices, span)
        {
            return Some(AggregateProjection {
                expression,
                family_shaped: true,
            });
        }
        return Some(AggregateProjection {
            expression: original.clone(),
            family_shaped: false,
        });
    };
    if shape.len() != indices.len()
        || shape.iter().zip(indices).any(|(extent, index)| {
            usize::try_from(*index)
                .map(|index| index == 0 || index > *extent)
                .unwrap_or(true)
        })
    {
        return None;
    }
    Some(AggregateProjection {
        expression: rumoca_core::Expression::VarRef {
            name: resolved_projection_reference(variables, name),
            subscripts: indices
                .iter()
                .map(|index| rumoca_core::Subscript::Index {
                    value: *index,
                    span,
                })
                .collect(),
            span,
        },
        family_shaped: true,
    })
}

fn resolved_projection_reference(
    variables: &ProjectionVariableIndex<'_>,
    source: &rumoca_core::Reference,
) -> rumoca_core::Reference {
    variables
        .named(source.as_str())
        .and_then(|variable| variable.component_ref.as_ref())
        .map(|component_ref| {
            if source.is_generated() {
                rumoca_core::Reference::generated_component_reference(component_ref.clone())
            } else {
                rumoca_core::Reference::from_component_reference(component_ref.clone())
            }
        })
        .unwrap_or_else(|| source.clone())
}

fn project_scalarized_aggregate_element(
    variables: &ProjectionVariableIndex<'_>,
    layout: &solve::VarLayout,
    aggregate: &rumoca_core::Reference,
    indices: &[i64],
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    variables
        .sharing_aggregate_path(aggregate)
        .iter()
        .find_map(|variable| {
            let component_ref = variable.component_ref.as_ref()?;
            let embedded_indices = component_reference_literal_indices(component_ref)?;
            if embedded_indices.len() > indices.len()
                || embedded_indices.as_slice() != &indices[..embedded_indices.len()]
            {
                return None;
            }
            let remaining_indices = &indices[embedded_indices.len()..];
            if !indices_fit_variable_dims(remaining_indices, &variable.dims)
                || !scalarized_projection_has_layout_binding(
                    layout,
                    variable.name.as_str(),
                    remaining_indices,
                )
            {
                return None;
            }
            let name = if aggregate.is_generated() {
                rumoca_core::Reference::generated_component_reference(component_ref.clone())
            } else {
                rumoca_core::Reference::from_component_reference(component_ref.clone())
            };
            Some(rumoca_core::Expression::VarRef {
                name,
                subscripts: remaining_indices
                    .iter()
                    .map(|index| rumoca_core::Subscript::Index {
                        value: *index,
                        span,
                    })
                    .collect(),
                span,
            })
        })
}

/// Name and aggregate-path indexes over the DAE's continuous and known
/// variables, built once per residual compute block.
///
/// Both projection helpers below used to rescan every variable for every
/// projected `VarRef`, rebuilding each candidate's base `Reference` (and its
/// path strings) on the way — O(variables) allocating work per reference, so
/// O(equations x variables) per block. On a model with thousands of structured
/// families that scan, not the lowering it feeds, dominated Solve-IR build time.
///
/// The lookups reproduce the scan's result exactly: `by_name` keeps the first
/// variable declaring a name, and each aggregate-path bucket holds its
/// variables in scan order, so a `find_map` over one bucket selects whatever
/// the full scan selected.
struct ProjectionVariableIndex<'a> {
    by_name: IndexMap<&'a str, &'a dae::Variable>,
    by_aggregate_path: IndexMap<rumoca_core::ComponentPath, Vec<&'a dae::Variable>>,
}

impl<'a> ProjectionVariableIndex<'a> {
    fn new(dae_model: &'a dae::Dae) -> Self {
        // RowMajorProjection templates retain the pre-instantiation aggregate
        // declaration, while scalar DAE descendants carry per-instance DefIds.
        // Their fully flattened component path is unique at this phase. Keep
        // that path structured in the lookup key; rendering it is only a
        // serialization/display concern.
        let mut by_name: IndexMap<&'a str, &'a dae::Variable> = IndexMap::new();
        let mut by_aggregate_path: IndexMap<rumoca_core::ComponentPath, Vec<&'a dae::Variable>> =
            IndexMap::new();
        for variable in continuous_and_known_variables(dae_model) {
            by_name.entry(variable.name.as_str()).or_insert(variable);
            if let Some(component_ref) = variable.component_ref.as_ref() {
                let base = rumoca_core::component_ref_to_base_reference(component_ref);
                by_aggregate_path
                    .entry(rumoca_core::ComponentPath::from_reference(&base))
                    .or_default()
                    .push(variable);
            }
        }
        Self {
            by_name,
            by_aggregate_path,
        }
    }

    /// First variable declaring `name`, matching the scan's `find` on name.
    fn named(&self, name: &str) -> Option<&'a dae::Variable> {
        self.by_name.get(name).copied()
    }

    /// Variables whose component reference canonicalizes to `aggregate`'s path,
    /// in scan order.
    fn sharing_aggregate_path(&self, aggregate: &rumoca_core::Reference) -> &[&'a dae::Variable] {
        self.by_aggregate_path
            .get(&rumoca_core::ComponentPath::from_reference(aggregate))
            .map_or(&[][..], Vec::as_slice)
    }
}

fn continuous_and_known_variables(dae_model: &dae::Dae) -> impl Iterator<Item = &dae::Variable> {
    dae_model
        .variables
        .states
        .values()
        .chain(dae_model.variables.algebraics.values())
        .chain(dae_model.variables.outputs.values())
        .chain(dae_model.variables.inputs.values())
        .chain(dae_model.variables.parameters.values())
        .chain(dae_model.variables.constants.values())
        .chain(dae_model.variables.discrete_reals.values())
        .chain(dae_model.variables.discrete_valued.values())
}

fn component_reference_literal_indices(
    component_ref: &rumoca_core::ComponentReference,
) -> Option<Vec<i64>> {
    component_ref
        .parts
        .iter()
        .flat_map(|part| &part.subs)
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => Some(*value),
            rumoca_core::Subscript::Colon { .. } | rumoca_core::Subscript::Expr { .. } => None,
        })
        .collect()
}

fn indices_fit_variable_dims(indices: &[i64], dims: &[i64]) -> bool {
    indices.len() == dims.len()
        && indices
            .iter()
            .zip(dims)
            .all(|(index, extent)| *index > 0 && *index <= *extent)
}

fn scalarized_projection_has_layout_binding(
    layout: &solve::VarLayout,
    variable: &str,
    indices: &[i64],
) -> bool {
    let indices = indices
        .iter()
        .copied()
        .map(usize::try_from)
        .collect::<Result<Vec<_>, _>>();
    let Ok(indices) = indices else {
        return false;
    };
    let key = if indices.is_empty() {
        variable.to_string()
    } else {
        dae::format_subscript_key(variable, &indices)
    };
    layout.binding(&key).is_some()
}

fn project_aggregate_binary(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    indices: &[i64],
    layout: &solve::VarLayout,
    variables: &ProjectionVariableIndex<'_>,
) -> Option<AggregateProjection> {
    let lhs = project_aggregate_element(lhs, indices, layout, variables)?;
    let rhs = project_aggregate_element(rhs, indices, layout, variables)?;
    if !aggregate_binary_is_pointwise(op, lhs.family_shaped, rhs.family_shaped) {
        return None;
    }
    Some(AggregateProjection {
        family_shaped: lhs.family_shaped || rhs.family_shaped,
        expression: rumoca_core::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(lhs.expression),
            rhs: Box::new(rhs.expression),
            span,
        },
    })
}

fn project_aggregate_if(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    span: rumoca_core::Span,
    indices: &[i64],
    layout: &solve::VarLayout,
    variables: &ProjectionVariableIndex<'_>,
) -> Option<AggregateProjection> {
    let branches = branches
        .iter()
        .map(|(condition, value)| {
            Some((
                project_aggregate_element(condition, indices, layout, variables)?,
                project_aggregate_element(value, indices, layout, variables)?,
            ))
        })
        .collect::<Option<Vec<_>>>()?;
    let else_branch = project_aggregate_element(else_branch, indices, layout, variables)?;
    let family_shaped = else_branch.family_shaped
        || branches
            .iter()
            .any(|(condition, value)| condition.family_shaped || value.family_shaped);
    Some(AggregateProjection {
        expression: rumoca_core::Expression::If {
            branches: branches
                .into_iter()
                .map(|(condition, value)| (condition.expression, value.expression))
                .collect(),
            else_branch: Box::new(else_branch.expression),
            span,
        },
        family_shaped,
    })
}

fn aggregate_binary_is_pointwise(
    op: &rumoca_core::OpBinary,
    lhs_array: bool,
    rhs_array: bool,
) -> bool {
    use rumoca_core::OpBinary;
    match op {
        OpBinary::Mul => !(lhs_array && rhs_array),
        OpBinary::Div => !rhs_array,
        // `A ^ n` is matrix exponentiation, not element-wise exponentiation.
        // Only the explicitly dotted operator may be projected per element.
        OpBinary::Exp => !(lhs_array || rhs_array),
        OpBinary::Empty => false,
        OpBinary::Add
        | OpBinary::Sub
        | OpBinary::AddElem
        | OpBinary::SubElem
        | OpBinary::MulElem
        | OpBinary::DivElem
        | OpBinary::ExpElem
        | OpBinary::Eq
        | OpBinary::Neq
        | OpBinary::Lt
        | OpBinary::Le
        | OpBinary::Gt
        | OpBinary::Ge
        | OpBinary::And
        | OpBinary::Or
        | OpBinary::Assign => true,
    }
}

fn aggregate_builtin_is_pointwise(
    function: rumoca_core::BuiltinFunction,
    argument_count: usize,
) -> bool {
    use rumoca_core::BuiltinFunction;
    if matches!(function, BuiltinFunction::Min | BuiltinFunction::Max) && argument_count == 1 {
        return false;
    }
    !matches!(
        function,
        BuiltinFunction::Sum
            | BuiltinFunction::Product
            | BuiltinFunction::Ndims
            | BuiltinFunction::Size
            | BuiltinFunction::Scalar
            | BuiltinFunction::Vector
            | BuiltinFunction::Matrix
            | BuiltinFunction::Identity
            | BuiltinFunction::Diagonal
            | BuiltinFunction::Zeros
            | BuiltinFunction::Ones
            | BuiltinFunction::Fill
            | BuiltinFunction::Linspace
            | BuiltinFunction::Transpose
            | BuiltinFunction::OuterProduct
            | BuiltinFunction::Symmetric
            | BuiltinFunction::Cross
            | BuiltinFunction::Skew
            | BuiltinFunction::Cat
    )
}

struct TemplateBinderSubstituter {
    values: IndexMap<String, i64>,
}

impl ExpressionRewriter for TemplateBinderSubstituter {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if subscripts.is_empty()
            && let Some(value) = self.values.get(name.as_str())
        {
            return rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(*value),
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

fn validate_residual_compute_block_contract(
    residual_row_count: usize,
    residual_target_count: usize,
    residual_equations: &[(usize, &dae::Equation)],
    fallback_span: Option<rumoca_core::Span>,
) -> Result<(), LowerError> {
    if residual_target_count != residual_row_count {
        return Err(residual_contract_error(
            format!(
                "residual target count {residual_target_count} does not match residual row count {residual_row_count}"
            ),
            residual_contract_error_span(residual_target_count, residual_equations)
                .or(fallback_span),
        ));
    }
    let expected_rows = residual_equation_scalar_count(residual_equations)?;
    if expected_rows != residual_row_count {
        return Err(residual_contract_error(
            format!(
                "residual equation scalar count {expected_rows} does not match residual row count {residual_row_count}"
            ),
            residual_contract_error_span(residual_row_count, residual_equations).or(fallback_span),
        ));
    }
    Ok(())
}

fn residual_contract_error(reason: String, span: Option<rumoca_core::Span>) -> LowerError {
    match span {
        Some(span) if !span.is_dummy() => LowerError::ContractViolation { reason, span },
        Some(_) | None => LowerError::UnspannedContractViolation { reason },
    }
}

fn residual_equation_scalar_count(
    residual_equations: &[(usize, &dae::Equation)],
) -> Result<usize, LowerError> {
    residual_equations
        .iter()
        .try_fold(0usize, |total, (_, equation)| {
            total
                .checked_add(equation.scalar_count.max(1))
                .ok_or_else(|| {
                    residual_contract_error(
                        "residual equation scalar count overflows usize".to_string(),
                        Some(equation.span),
                    )
                })
        })
}

fn residual_contract_error_span(
    row_count: usize,
    residual_equations: &[(usize, &dae::Equation)],
) -> Option<rumoca_core::Span> {
    let mut row_start = 0usize;
    for (_, equation) in residual_equations {
        let row_end = row_start.checked_add(equation.scalar_count.max(1))?;
        if row_count < row_end {
            return Some(equation.span);
        }
        row_start = row_end;
    }
    residual_equations.last().map(|(_, equation)| equation.span)
}

fn residual_context_span(
    dae_model: &dae::Dae,
    residual_equations: &[(usize, &dae::Equation)],
) -> Option<rumoca_core::Span> {
    residual_equations
        .first()
        .map(|(_, equation)| equation.span)
        .or_else(|| {
            dae_model
                .variables
                .states
                .values()
                .chain(dae_model.variables.algebraics.values())
                .chain(dae_model.variables.outputs.values())
                .chain(dae_model.variables.inputs.values())
                .chain(dae_model.variables.discrete_reals.values())
                .chain(dae_model.variables.discrete_valued.values())
                .chain(dae_model.variables.parameters.values())
                .chain(dae_model.variables.constants.values())
                .find_map(|var| (!var.source_span.is_dummy()).then_some(var.source_span))
        })
}

fn residual_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        residual_contract_error(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn target_y_index(target: Option<solve::ScalarSlot>) -> Option<usize> {
    match target {
        Some(solve::ScalarSlot::Y { index, .. }) => Some(index),
        _ => None,
    }
}

fn residual_output_y_range(
    target: Option<solve::ScalarSlot>,
    y_slot_ranges: &crate::stencil::YSlotRanges,
    fallback_index: usize,
    span: rumoca_core::Span,
) -> Result<std::ops::Range<usize>, LowerError> {
    let Some(index) = target_y_index(target) else {
        return checked_singleton_range(fallback_index, "residual fallback output", span);
    };
    if let Some(range) = y_slot_ranges.get(index) {
        return Ok(range);
    }
    checked_singleton_range(index, "residual target y output", span)
}

fn checked_singleton_range(
    index: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<std::ops::Range<usize>, LowerError> {
    let end = index.checked_add(1).ok_or_else(|| {
        residual_contract_error(
            format!("{context} index {index} overflows output range"),
            Some(span),
        )
    })?;
    Ok(index..end)
}

fn residual_row_access_proof(
    layout: &solve::VarLayout,
    structural_bindings: &IndexMap<String, f64>,
    expression: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
    row_offset: usize,
    scalar_count: usize,
) -> Result<Option<stencil::StructuredAccessProof>, LowerError> {
    let Some(expression) = residual_row_access_expression(expression, row_offset, scalar_count)
    else {
        return Ok(None);
    };
    let mut builder = stencil::StructuredAccessProofBuilder::new();
    let Some(access_span) = expression
        .span()
        .filter(|span| !span.is_dummy())
        .or_else(|| (!owner_span.is_dummy()).then_some(owner_span))
    else {
        return Ok(None);
    };
    let Some(()) =
        builder.collect_expression_result(&expression, |base, subscripts, span, operands| {
            let span = if span.is_dummy() { access_span } else { span };
            collect_residual_var_ref_access_operands(
                base,
                subscripts,
                layout,
                structural_bindings,
                span,
                operands,
            )
        })?
    else {
        return Ok(None);
    };
    Ok(Some(builder.finish()))
}

fn residual_row_access_expression(
    expression: &rumoca_core::Expression,
    row_offset: usize,
    scalar_count: usize,
) -> Option<rumoca_core::Expression> {
    if scalar_count <= 1 {
        return Some(expression.clone());
    }
    match expression {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements.get(row_offset).cloned(),
        _ => None,
    }
}

fn collect_residual_var_ref_access_operands(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    layout: &solve::VarLayout,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
    operands: &mut Vec<stencil::StructuredAccessOperand>,
) -> Result<Option<()>, LowerError> {
    if subscripts
        .iter()
        .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
    {
        return Ok(None);
    }
    let indices = lower::compile_time_subscript_indices_for_structured_access(
        subscripts,
        structural_bindings,
        owner_span,
    )?;
    let key = if indices.is_empty() {
        base.to_string()
    } else {
        dae::format_subscript_key(base, &indices)
    };
    let Some(slot) = layout.binding(&key) else {
        return Ok(None);
    };
    let Some(operand) = stencil::structured_access_operand_for_slot(slot) else {
        return Ok(None);
    };
    operands.push(operand);
    Ok(Some(()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn component_reference(
        parts: impl IntoIterator<Item = (&'static str, Vec<i64>)>,
        span: rumoca_core::Span,
    ) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference {
            local: false,
            span,
            parts: parts
                .into_iter()
                .map(|(ident, indices)| rumoca_core::ComponentRefPart {
                    ident: ident.to_string(),
                    span,
                    subs: indices
                        .into_iter()
                        .map(|value| rumoca_core::Subscript::Index { value, span })
                        .collect(),
                })
                .collect(),
            def_id: None,
        }
    }

    fn literal_zero(span: rumoca_core::Span) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span,
        }
    }

    fn direct_template_test_family(span: rumoca_core::Span) -> dae::StructuredEquationFamily {
        dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 4,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equations_per_point: 1,
            span,
            origin: "direct template proof fixture".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        }
    }

    fn unspanned_residual_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    #[test]
    fn residual_compute_block_contract_reports_target_mismatch_with_equation_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("target_mismatch.mo"),
            7,
            19,
        );
        let equation = dae::Equation::residual_array(literal_zero(span), span, "eq", 2);
        let err = validate_residual_compute_block_contract(2, 1, &[(0, &equation)], Some(span))
            .expect_err("target count mismatch should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.to_string().contains("residual target count"),
            "error should explain target mismatch: {err}"
        );
    }

    #[test]
    fn residual_compute_block_contract_reports_row_mismatch_with_equation_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("row_mismatch.mo"),
            11,
            23,
        );
        let equation = dae::Equation::residual_array(literal_zero(span), span, "eq", 2);
        let err = validate_residual_compute_block_contract(1, 1, &[(0, &equation)], Some(span))
            .expect_err("row count mismatch should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.to_string().contains("residual equation scalar count"),
            "error should explain row mismatch: {err}"
        );
    }

    #[test]
    fn residual_compute_block_contract_reports_missing_row_with_next_equation_span() {
        let first_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("boundary_mismatch.mo"),
            2,
            6,
        );
        let second_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("boundary_mismatch.mo"),
            9,
            16,
        );
        let first = dae::Equation::residual_array(literal_zero(first_span), first_span, "eq1", 1);
        let second =
            dae::Equation::residual_array(literal_zero(second_span), second_span, "eq2", 1);
        let err = validate_residual_compute_block_contract(
            1,
            1,
            &[(0, &first), (1, &second)],
            Some(first_span),
        )
        .expect_err("missing second row should fail");

        assert_eq!(err.source_span(), Some(second_span));
    }

    #[test]
    fn residual_compute_block_contract_does_not_fabricate_span_without_context() {
        let err = validate_residual_compute_block_contract(1, 0, &[], None)
            .expect_err("unmatched residual rows without provenance should fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason().contains("residual target count"),
            "error should explain target mismatch: {err}"
        );
    }

    #[test]
    fn residual_equation_scalar_count_does_not_fabricate_dummy_span() {
        let first = dae::Equation::residual_array(
            literal_zero(unspanned_residual_test_span()),
            unspanned_residual_test_span(),
            "first",
            usize::MAX,
        );
        let second = dae::Equation::residual_array(
            literal_zero(unspanned_residual_test_span()),
            unspanned_residual_test_span(),
            "second",
            1,
        );

        let err = residual_equation_scalar_count(&[(0, &first), (1, &second)])
            .expect_err("oversized residual scalar count should fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("residual equation scalar count overflows usize"),
            "error should explain residual scalar-count overflow: {err}"
        );
    }

    #[test]
    fn residual_output_y_range_uses_checked_scalar_fallback() -> Result<(), LowerError> {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("residual_output_range.mo"),
            1,
            5,
        );
        let range =
            residual_output_y_range(None, &crate::stencil::YSlotRanges::default(), 7, span)?;

        assert_eq!(range, 7..8);
        Ok(())
    }

    #[test]
    fn residual_output_y_range_reports_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("bad_residual_output.mo"),
            4,
            12,
        );
        let err = residual_output_y_range(
            None,
            &crate::stencil::YSlotRanges::default(),
            usize::MAX,
            span,
        )
        .expect_err("overflowing residual output range should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.to_string().contains("residual fallback output index"),
            "error should explain residual output overflow: {err}"
        );
    }

    #[test]
    fn residual_output_y_range_does_not_fabricate_dummy_span() {
        let err = residual_output_y_range(
            None,
            &crate::stencil::YSlotRanges::default(),
            usize::MAX,
            unspanned_residual_test_span(),
        )
        .expect_err("overflowing residual output range should fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason().contains("residual fallback output index"),
            "error should explain residual output overflow: {err}"
        );
    }

    #[test]
    fn compact_output_coverage_intersection_is_constant_time_and_exact() {
        let even = CompactOutputCoverage {
            start: 0,
            step: 2,
            count: 1_000_000,
        };
        let odd = CompactOutputCoverage {
            start: 1,
            step: 2,
            count: 1_000_000,
        };
        let sparse_overlap = CompactOutputCoverage {
            start: 999_999,
            step: 3,
            count: 8,
        };
        let after = CompactOutputCoverage {
            start: 2_000_001,
            step: 2,
            count: 4,
        };

        assert!(!even.intersects(odd));
        assert!(odd.intersects(sparse_overlap));
        assert!(!even.intersects(after));
    }

    #[test]
    fn compact_owner_output_mapping_does_not_claim_adjacent_equation() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("compact_owner_neighbor.mo"),
            1,
            2,
        );
        let family = dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "__array_i1".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equations_per_point: 1,
            span,
            origin: "whole-array owner".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        };
        let compact_spans = IndexMap::from([
            (0, ResidualOutputSpan { start: 0, count: 3 }),
            (1, ResidualOutputSpan { start: 3, count: 1 }),
        ]);
        let expanded_spans = IndexMap::from([
            (0, ResidualOutputSpan { start: 0, count: 1 }),
            (1, ResidualOutputSpan { start: 1, count: 1 }),
            (2, ResidualOutputSpan { start: 2, count: 1 }),
        ]);

        assert_eq!(
            family_output_index(&family, 1, 0, &compact_spans)
                .expect("compact mapping should be valid"),
            Some(1)
        );
        assert_eq!(
            family_output_index(&family, 1, 0, &expanded_spans)
                .expect("expanded mapping should be valid"),
            Some(1)
        );
    }

    #[test]
    fn compact_direct_source_family_requires_affine_access_proof() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("compact_affine_proof.mo"),
            1,
            2,
        );
        let family = direct_template_test_family(span);
        let source_template = rumoca_core::ComprehensionTemplate {
            body: vec![literal_zero(span)],
            scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
        };
        let aggregate_template = rumoca_core::ComprehensionTemplate {
            scalar_view: rumoca_core::ComprehensionScalarView::RowMajorProjection,
            ..source_template.clone()
        };

        assert!(!compact_direct_family_is_proven(&family, &source_template));
        assert!(compact_direct_family_is_proven(
            &family,
            &aggregate_template
        ));
        let mut affine_family = family.clone();
        affine_family.regular = Some(rumoca_core::RegularForFamily {
            binders: vec!["i".to_string()],
            accesses: Vec::new(),
        });
        assert!(compact_direct_family_is_proven(
            &affine_family,
            &source_template
        ));
        let nonlinear_binder_template = rumoca_core::ComprehensionTemplate {
            body: vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("i"),
                subscripts: Vec::new(),
                span,
            }],
            scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
        };
        assert!(!compact_direct_family_is_proven(
            &affine_family,
            &nonlinear_binder_template
        ));
        assert!(!aggregate_binary_is_pointwise(
            &rumoca_core::OpBinary::Exp,
            true,
            false
        ));
        assert!(aggregate_binary_is_pointwise(
            &rumoca_core::OpBinary::ExpElem,
            true,
            false
        ));
        assert!(!aggregate_builtin_is_pointwise(
            rumoca_core::BuiltinFunction::Min,
            1
        ));
        assert!(aggregate_builtin_is_pointwise(
            rumoca_core::BuiltinFunction::Min,
            2
        ));
    }

    #[test]
    fn direct_template_declines_shadowed_family_binder() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("compact_binder_capture.mo"),
            1,
            2,
        );
        let family = direct_template_test_family(span);
        let shadowed_binder_template = rumoca_core::ComprehensionTemplate {
            body: vec![rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("i"),
                    subscripts: Vec::new(),
                    span,
                }),
                indices: vec![rumoca_core::ComprehensionIndex {
                    name: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(literal_zero(span)),
                        step: None,
                        end: Box::new(literal_zero(span)),
                        span,
                    },
                }],
                filter: None,
                span,
            }],
            scalar_view: rumoca_core::ComprehensionScalarView::RowMajorProjection,
        };
        assert!(!compact_direct_family_is_proven(
            &family,
            &shadowed_binder_template
        ));
    }

    #[test]
    fn direct_template_declines_dae_identity_operators() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("compact_stale_dae_identity.mo"),
            1,
            2,
        );
        let family = direct_template_test_family(span);
        for function in [
            rumoca_core::BuiltinFunction::Terminal,
            rumoca_core::BuiltinFunction::Delay,
            rumoca_core::BuiltinFunction::Pre,
            rumoca_core::BuiltinFunction::Der,
        ] {
            let stale_dae_operator_template = rumoca_core::ComprehensionTemplate {
                body: vec![rumoca_core::Expression::BuiltinCall {
                    function,
                    args: vec![literal_zero(span)],
                    span,
                }],
                scalar_view: rumoca_core::ComprehensionScalarView::RowMajorProjection,
            };
            assert!(
                !compact_direct_family_is_proven(&family, &stale_dae_operator_template),
                "{function:?} must decline direct template compaction"
            );
        }
    }

    #[test]
    fn row_major_projection_selects_scalarized_component_array_descendant() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("component_array_projection.mo"),
            2,
            18,
        );
        let aggregate = rumoca_core::Reference::from_component_reference(component_reference(
            [
                ("vehicle", Vec::new()),
                ("motor", Vec::new()),
                ("omega_cmd", Vec::new()),
            ],
            span,
        ));
        let original = rumoca_core::Expression::VarRef {
            name: aggregate.clone(),
            subscripts: Vec::new(),
            span,
        };
        let mut dae_model = dae::Dae::new();
        let mut bindings = IndexMap::new();
        for index in 1..=4 {
            let reference = component_reference(
                [
                    ("vehicle", Vec::new()),
                    ("motor", vec![index]),
                    ("omega_cmd", Vec::new()),
                ],
                span,
            );
            let name = rumoca_core::Reference::from_component_reference(reference.clone())
                .as_str()
                .to_string();
            let mut variable = dae::Variable::empty_with_span(span);
            variable.name = rumoca_core::VarName::new(&name);
            variable.component_ref = Some(reference);
            dae_model
                .variables
                .algebraics
                .insert(variable.name.clone(), variable);
            let slot_index = usize::try_from(index - 1).expect("positive fixture index");
            bindings.insert(
                name,
                solve::ScalarSlot::Y {
                    index: slot_index,
                    byte_offset: slot_index * std::mem::size_of::<f64>(),
                },
            );
        }
        let layout = solve::VarLayout::from_parts(bindings, 4, 0);

        let variables = ProjectionVariableIndex::new(&dae_model);
        let projection =
            project_aggregate_var_ref(&original, &aggregate, &[], span, &[3], &layout, &variables)
                .expect("component-array aggregate should project");

        assert!(projection.family_shaped);
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = projection.expression
        else {
            panic!("projection should remain a variable reference");
        };
        assert_eq!(name.as_str(), "vehicle.motor[3].omega_cmd");
        assert!(subscripts.is_empty());
    }
}
