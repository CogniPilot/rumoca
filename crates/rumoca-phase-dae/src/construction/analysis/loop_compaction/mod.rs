mod accumulator_reductions;
mod dependent_domains;
mod loop_local_substitution;

use accumulator_reductions::compact_accumulator_loops;
use dependent_domains::{integer_range, rectangularize_dependent_loops};
use loop_local_substitution::{
    LocalSubstitution, compact_perfect_inner_element_loops, expression_reads_name,
    first_dependent_loop_range, inline_loop_local_prefixes, statement_with_equations,
    statements_partially_assign_name, statements_read_name, statements_read_nonrewritable_name,
};

use super::*;
use rumoca_core::{ExpressionRewriter, Reference, StatementRewriter, Subscript};
use std::collections::BTreeMap;

/// Normalize function loops without materializing their scalar iterations.
///
/// Exact slice compaction and dead loop-local substitution preserve compact
/// owners. A genuinely dependent domain is refused until its compact
/// transition representation exists; it must never fall through to statement
/// expansion as an alternate IR.
pub(super) fn compact_function_loops(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    function: &rumoca_core::Function,
    flat: &flat::Model,
    has_certified_output_seeds: bool,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let output_names = function
        .outputs
        .iter()
        .map(|output| VarName::new(&output.name))
        .collect::<HashSet<_>>();
    let scalar_local_names = function
        .locals
        .iter()
        .filter(|local| {
            local.effective_type.dimensions().is_empty()
                && effective_function_scalar_type(flat, local) != Some(dae::ScalarType::Record)
        })
        .map(|local| VarName::new(&local.name))
        .collect::<HashSet<_>>();
    let array_local_names = function
        .locals
        .iter()
        .filter(|local| {
            !local.effective_type.dimensions().is_empty()
                && effective_function_scalar_type(flat, local) != Some(dae::ScalarType::Record)
        })
        .map(|local| VarName::new(&local.name))
        .collect::<HashSet<_>>();
    let mut local_value_names = scalar_local_names;
    local_value_names.extend(array_local_names.iter().cloned());
    let branch_local_names = local_value_names
        .iter()
        .filter(|name| {
            !statements.iter().enumerate().any(|(ordinal, statement)| {
                matches!(statement, rumoca_core::Statement::For { .. })
                    && statements_read_incoming_name(&statements[ordinal + 1..], name)
            })
        })
        .cloned()
        .collect::<HashSet<_>>();
    let affine_slices = AffineSliceRanges.rewrite_statements(statements);
    let settled = inline_settled_shape_integer_locals(
        &affine_slices,
        static_integers,
        shapes,
        function,
        flat,
    )?;
    let mut bounded_shapes = shapes.clone();
    infer_function_integer_bounds(&settled, &mut bounded_shapes);
    let settled = rectangularize_bounded_slice_assignments(&settled, &bounded_shapes);
    let settled = compact_exhaustive_loop_conditionals(&settled, false);
    let compacted = compact_accumulator_loops(&settled, &bounded_shapes);
    let compacted =
        inline_dead_loop_scalar_locals(&compacted, &local_value_names, &branch_local_names);
    let compacted = inline_loop_local_prefixes(&compacted, &local_value_names, &output_names);
    let compacted = distribute_partitioned_outer_loops(&compacted);
    let defined_outputs = if has_certified_output_seeds {
        output_names.clone()
    } else {
        HashSet::new()
    };
    let compacted =
        push_invariant_conditions_to_fixpoint(compacted, &output_names, &defined_outputs);
    let compacted = compact_perfect_inner_element_loops(&compacted);
    let compacted = BoundedReductionComprehensions {
        shapes: &bounded_shapes,
    }
    .rewrite_statements(&compacted);
    let compacted = rectangularize_dependent_loops(&compacted, static_integers, &bounded_shapes)?;
    let compacted =
        push_invariant_conditions_to_fixpoint(compacted, &output_names, &defined_outputs);
    let Some(span) = first_dependent_loop_range(&compacted, static_integers, &bounded_shapes)?
    else {
        return Ok(compacted);
    };
    Err(ToDaeError::unsupported_flat(
        "function loop domain",
        format!(
            "`{}` requires a compact dependent-domain transition; scalar statement expansion is prohibited",
            function.name
        ),
        span,
    ))
}

struct AffineSliceRanges;

impl ExpressionRewriter for AffineSliceRanges {
    fn rewrite_subscript(&mut self, subscript: &Subscript) -> Subscript {
        let Subscript::Expr { expr, span } = subscript else {
            return subscript.clone();
        };
        let rewritten = self.rewrite_expression(expr);
        affine_slice_subscript(&rewritten, *span).unwrap_or(Subscript::Expr {
            expr: Box::new(rewritten),
            span: *span,
        })
    }
}

fn affine_slice_subscript(rewritten: &Expression, span: Span) -> Option<Subscript> {
    let Expression::Range {
        start,
        step,
        end,
        span: range_span,
    } = &rewritten
    else {
        return None;
    };
    let mut references = Vec::new();
    start.collect_var_refs(&mut references);
    end.collect_var_refs(&mut references);
    if references.is_empty() {
        return None;
    }
    let step_value = match step.as_deref() {
        None => 1,
        Some(Expression::Literal {
            value: Literal::Integer(value),
            ..
        }) if *value != 0 => *value,
        _ => {
            return None;
        }
    };
    let distance = affine_integer_difference(end, start)?;
    if distance.checked_rem(step_value) != Some(0) {
        return None;
    }
    let extent = distance
        .checked_div(step_value)
        .and_then(|value| value.checked_add(1))?;
    if extent <= 0 {
        return None;
    }
    let binder = rumoca_core::affine_slice_binder_name(range_span.start.0);
    let binder_reference = || Expression::VarRef {
        name: Reference::generated(&binder),
        subscripts: Vec::new(),
        span: *range_span,
    };
    let coordinate = Expression::Binary {
        op: OpBinary::Add,
        lhs: start.clone(),
        rhs: Box::new(Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(binder_reference()),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Integer(1),
                    span: *range_span,
                }),
                span: *range_span,
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Integer(step_value),
                span: *range_span,
            }),
            span: *range_span,
        }),
        span: *range_span,
    };
    Some(Subscript::Expr {
        expr: Box::new(Expression::ArrayComprehension {
            expr: Box::new(coordinate),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: binder,
                range: integer_range(1, extent, *range_span),
            }],
            filter: None,
            span: *range_span,
        }),
        span,
    })
}

impl StatementRewriter for AffineSliceRanges {}

#[derive(Default)]
struct AffineInteger {
    constant: i64,
    terms: BTreeMap<VarName, i64>,
}

fn affine_integer_difference(end: &Expression, start: &Expression) -> Option<i64> {
    let mut difference = affine_integer(end)?;
    difference.add_scaled(affine_integer(start)?, -1)?;
    difference
        .terms
        .values()
        .all(|coefficient| *coefficient == 0)
        .then_some(difference.constant)
}

fn affine_integer(expression: &Expression) -> Option<AffineInteger> {
    match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(AffineInteger {
            constant: *value,
            terms: BTreeMap::new(),
        }),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(AffineInteger {
            constant: 0,
            terms: BTreeMap::from([(name.var_name().clone(), 1)]),
        }),
        Expression::Unary {
            op: OpUnary::Plus,
            rhs,
            ..
        } => affine_integer(rhs),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => {
            let mut value = affine_integer(rhs)?;
            value.scale(-1)?;
            Some(value)
        }
        Expression::Binary { op, lhs, rhs, .. }
            if matches!(
                op,
                OpBinary::Add | OpBinary::AddElem | OpBinary::Sub | OpBinary::SubElem
            ) =>
        {
            let mut value = affine_integer(lhs)?;
            let scale = if matches!(op, OpBinary::Sub | OpBinary::SubElem) {
                -1
            } else {
                1
            };
            value.add_scaled(affine_integer(rhs)?, scale)?;
            Some(value)
        }
        Expression::Binary {
            op: OpBinary::Mul | OpBinary::MulElem,
            lhs,
            rhs,
            ..
        } => {
            if let Expression::Literal {
                value: Literal::Integer(scale),
                ..
            } = lhs.as_ref()
            {
                let mut value = affine_integer(rhs)?;
                value.scale(*scale)?;
                return Some(value);
            }
            let Expression::Literal {
                value: Literal::Integer(scale),
                ..
            } = rhs.as_ref()
            else {
                return None;
            };
            let mut value = affine_integer(lhs)?;
            value.scale(*scale)?;
            Some(value)
        }
        _ => None,
    }
}

impl AffineInteger {
    fn scale(&mut self, scale: i64) -> Option<()> {
        self.constant = self.constant.checked_mul(scale)?;
        for coefficient in self.terms.values_mut() {
            *coefficient = coefficient.checked_mul(scale)?;
        }
        Some(())
    }

    fn add_scaled(&mut self, mut other: Self, scale: i64) -> Option<()> {
        other.scale(scale)?;
        self.constant = self.constant.checked_add(other.constant)?;
        for (name, coefficient) in other.terms {
            let owned = self.terms.entry(name).or_default();
            *owned = owned.checked_add(coefficient)?;
        }
        Some(())
    }
}

struct BoundedReductionComprehensions<'shape> {
    shapes: &'shape ShapeEnvironment,
}

impl ExpressionRewriter for BoundedReductionComprehensions<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        let rewritten = self.walk_expression(expression);
        let Expression::BuiltinCall {
            function: function @ (BuiltinFunction::Sum | BuiltinFunction::Product),
            args,
            span,
        } = &rewritten
        else {
            return rewritten;
        };
        let [
            Expression::ArrayComprehension {
                expr,
                indices,
                filter: None,
                span: comprehension_span,
            },
        ] = args.as_slice()
        else {
            return rewritten;
        };
        let [index] = indices.as_slice() else {
            return rewritten;
        };
        let Some((range, term)) = bounded_reduction_comprehension(
            index,
            expr,
            *function,
            self.shapes,
            *comprehension_span,
        ) else {
            return rewritten;
        };
        Expression::BuiltinCall {
            function: *function,
            args: vec![Expression::ArrayComprehension {
                expr: Box::new(term),
                indices: vec![rumoca_core::ComprehensionIndex {
                    name: index.name.clone(),
                    range,
                }],
                filter: None,
                span: *comprehension_span,
            }],
            span: *span,
        }
    }
}

impl StatementRewriter for BoundedReductionComprehensions<'_> {}

fn bounded_reduction_comprehension(
    index: &rumoca_core::ComprehensionIndex,
    term: &Expression,
    function: BuiltinFunction,
    shapes: &ShapeEnvironment,
    span: Span,
) -> Option<(Expression, Expression)> {
    let Expression::Range {
        start,
        step,
        end,
        span: range_span,
    } = &index.range
    else {
        return None;
    };
    let step = step
        .as_deref()
        .map(|step| shapes.proven_extent(step))
        .unwrap_or(Some(1))?;
    if step != 1 {
        return None;
    }
    let start_exact = shapes.proven_extent(start);
    let end_exact = shapes.proven_extent(end);
    if start_exact.is_some() && end_exact.is_some() {
        return None;
    }
    let (start_lower, _) = shapes.proven_integer_bounds(start)?;
    let (_, end_upper) = shapes.proven_integer_bounds(end)?;
    let fixed_range = Expression::Range {
        start: Box::new(Expression::Literal {
            value: Literal::Integer(start_lower),
            span: *range_span,
        }),
        step: None,
        end: Box::new(Expression::Literal {
            value: Literal::Integer(end_upper),
            span: *range_span,
        }),
        span: *range_span,
    };
    let binder = || Expression::VarRef {
        name: Reference::new(&index.name),
        subscripts: Vec::new(),
        span,
    };
    let after_start = Expression::Binary {
        op: OpBinary::Ge,
        lhs: Box::new(binder()),
        rhs: start.clone(),
        span,
    };
    let before_end = Expression::Binary {
        op: OpBinary::Le,
        lhs: Box::new(binder()),
        rhs: end.clone(),
        span,
    };
    let active = Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(after_start),
        rhs: Box::new(before_end),
        span,
    };
    let identity = Expression::Literal {
        value: Literal::Integer(if function == BuiltinFunction::Product {
            1
        } else {
            0
        }),
        span,
    };
    Some((
        fixed_range,
        Expression::If {
            branches: vec![(active, term.clone())],
            else_branch: Box::new(identity),
            span,
        },
    ))
}

fn rectangularize_bounded_slice_assignments(
    statements: &[rumoca_core::Statement],
    shapes: &ShapeEnvironment,
) -> Vec<rumoca_core::Statement> {
    statements
        .iter()
        .map(|statement| match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                rectangularize_one_bounded_slice_assignment(comp, value, *span, shapes)
                    .unwrap_or_else(|| statement.clone())
            }
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => rumoca_core::Statement::For {
                indices: indices.clone(),
                equations: rectangularize_bounded_slice_assignments(equations, shapes),
                span: *span,
            },
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => rumoca_core::Statement::If {
                cond_blocks: cond_blocks
                    .iter()
                    .map(|block| rumoca_core::StatementBlock {
                        cond: block.cond.clone(),
                        stmts: rectangularize_bounded_slice_assignments(&block.stmts, shapes),
                    })
                    .collect(),
                else_block: else_block
                    .as_deref()
                    .map(|branch| rectangularize_bounded_slice_assignments(branch, shapes)),
                span: *span,
            },
            _ => statement.clone(),
        })
        .collect()
}

fn rectangularize_one_bounded_slice_assignment(
    target: &rumoca_core::ComponentReference,
    value: &Expression,
    span: Span,
    shapes: &ShapeEnvironment,
) -> Option<rumoca_core::Statement> {
    let mut parts = target.parts().to_vec();
    if parts.len() != 1 {
        return None;
    }
    let selected = find_bounded_slice(&parts, shapes)?;
    let BoundedSlice {
        part_ordinal,
        subscript_ordinal,
        source_range,
        source_start,
        source_end,
        lower,
        upper,
        subscript_span,
        range_span,
    } = selected;
    let binder_name = "__rumoca_slice_index";
    let fixed_range = integer_range(lower, upper, range_span);
    parts[part_ordinal].subs[subscript_ordinal] = Subscript::Expr {
        expr: Box::new(fixed_range.clone()),
        span: subscript_span,
    };
    let fixed_target = target.with_replaced_parts(parts.clone()).ok()?;
    parts[part_ordinal].subs[subscript_ordinal] = binder_subscript(binder_name, subscript_span);
    let element_subscripts = parts[part_ordinal].subs.clone();
    let mut base_parts = parts;
    base_parts[part_ordinal].subs.clear();
    let element_base = target.with_replaced_parts(base_parts).ok()?;
    let mut replacement = SliceElementReplacement {
        source_range: &source_range,
        binder_name,
        replacements: 0,
    };
    let active_value = replacement.rewrite_expression(value);
    if replacement.replacements == 0 {
        return None;
    }
    Some(bounded_slice_assignment(
        fixed_target,
        element_base,
        element_subscripts,
        active_value,
        (source_start, source_end),
        (binder_name, fixed_range, span),
    ))
}

struct BoundedSlice {
    part_ordinal: usize,
    subscript_ordinal: usize,
    source_range: Expression,
    source_start: Expression,
    source_end: Expression,
    lower: i64,
    upper: i64,
    subscript_span: Span,
    range_span: Span,
}

fn find_bounded_slice(
    parts: &[rumoca_core::ComponentRefPart],
    shapes: &ShapeEnvironment,
) -> Option<BoundedSlice> {
    let mut selected = None;
    for (part_ordinal, part) in parts.iter().enumerate() {
        for (subscript_ordinal, subscript) in part.subs.iter().enumerate() {
            let Subscript::Expr {
                expr,
                span: subscript_span,
            } = subscript
            else {
                continue;
            };
            let Expression::Range {
                start,
                step,
                end,
                span: range_span,
            } = expr.as_ref()
            else {
                continue;
            };
            if shapes.proven_extent(start).is_some() && shapes.proven_extent(end).is_some() {
                continue;
            }
            let step = step
                .as_deref()
                .map(|step| shapes.proven_extent(step))
                .unwrap_or(Some(1))?;
            if step != 1 || selected.is_some() {
                return None;
            }
            let (lower, _) = shapes.proven_integer_bounds(start)?;
            let (_, upper) = shapes.proven_integer_bounds(end)?;
            selected = Some(BoundedSlice {
                part_ordinal,
                subscript_ordinal,
                source_range: expr.as_ref().clone(),
                source_start: start.as_ref().clone(),
                source_end: end.as_ref().clone(),
                lower,
                upper,
                subscript_span: *subscript_span,
                range_span: *range_span,
            });
        }
    }
    selected
}

fn bounded_slice_assignment(
    fixed_target: rumoca_core::ComponentReference,
    element_base: rumoca_core::ComponentReference,
    element_subscripts: Vec<Subscript>,
    active_value: Expression,
    source_bounds: (Expression, Expression),
    domain: (&str, Expression, Span),
) -> rumoca_core::Statement {
    let (binder_name, fixed_range, span) = domain;
    let binder = || Expression::VarRef {
        name: Reference::new(binder_name),
        subscripts: Vec::new(),
        span,
    };
    let after_start = Expression::Binary {
        op: OpBinary::Ge,
        lhs: Box::new(binder()),
        rhs: Box::new(source_bounds.0),
        span,
    };
    let before_end = Expression::Binary {
        op: OpBinary::Le,
        lhs: Box::new(binder()),
        rhs: Box::new(source_bounds.1),
        span,
    };
    let active = Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(after_start),
        rhs: Box::new(before_end),
        span,
    };
    let prior = Expression::Index {
        base: Box::new(Expression::VarRef {
            name: Reference::from_component_reference(element_base),
            subscripts: Vec::new(),
            span,
        }),
        subscripts: element_subscripts,
        span,
    };
    let element = Expression::If {
        branches: vec![(active, active_value)],
        else_branch: Box::new(prior),
        span,
    };
    rumoca_core::Statement::Assignment {
        comp: fixed_target,
        value: Expression::ArrayComprehension {
            expr: Box::new(element),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: binder_name.to_owned(),
                range: fixed_range,
            }],
            filter: None,
            span,
        },
        span,
    }
}

fn binder_subscript(name: &str, span: Span) -> Subscript {
    Subscript::Expr {
        expr: Box::new(Expression::VarRef {
            name: Reference::new(name),
            subscripts: Vec::new(),
            span,
        }),
        span,
    }
}

struct SliceElementReplacement<'range> {
    source_range: &'range Expression,
    binder_name: &'range str,
    replacements: usize,
}

impl ExpressionRewriter for SliceElementReplacement<'_> {
    fn rewrite_subscript(&mut self, subscript: &Subscript) -> Subscript {
        if let Subscript::Expr { expr, span } = subscript
            && same_range_expression(expr, self.source_range)
        {
            self.replacements += 1;
            return binder_subscript(self.binder_name, *span);
        }
        match subscript {
            Subscript::Index { value, span } => Subscript::Index {
                value: *value,
                span: *span,
            },
            Subscript::Colon { span } => Subscript::Colon { span: *span },
            Subscript::Expr { expr, span } => Subscript::Expr {
                expr: Box::new(self.rewrite_expression(expr)),
                span: *span,
            },
        }
    }
}

fn same_range_expression(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Range {
            start: lhs_start,
            step: lhs_step,
            end: lhs_end,
            ..
        },
        Expression::Range {
            start: rhs_start,
            step: rhs_step,
            end: rhs_end,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    same_integer_expression(lhs_start, rhs_start)
        && match (lhs_step.as_deref(), rhs_step.as_deref()) {
            (None, None) => true,
            (Some(lhs), Some(rhs)) => same_integer_expression(lhs, rhs),
            _ => false,
        }
        && same_integer_expression(lhs_end, rhs_end)
}

fn same_integer_expression(lhs: &Expression, rhs: &Expression) -> bool {
    match (lhs, rhs) {
        (
            Expression::Literal {
                value: Literal::Integer(lhs),
                ..
            },
            Expression::Literal {
                value: Literal::Integer(rhs),
                ..
            },
        ) => lhs == rhs,
        (
            Expression::VarRef {
                name: lhs,
                subscripts: lhs_subscripts,
                ..
            },
            Expression::VarRef {
                name: rhs,
                subscripts: rhs_subscripts,
                ..
            },
        ) => {
            lhs_subscripts.is_empty()
                && rhs_subscripts.is_empty()
                && lhs.var_name() == rhs.var_name()
        }
        (
            Expression::Unary {
                op: lhs_op,
                rhs: lhs,
                ..
            },
            Expression::Unary {
                op: rhs_op, rhs, ..
            },
        ) => lhs_op == rhs_op && same_integer_expression(lhs, rhs),
        (
            Expression::Binary {
                op: lhs_op,
                lhs: lhs_left,
                rhs: lhs_right,
                ..
            },
            Expression::Binary {
                op: rhs_op,
                lhs: rhs_left,
                rhs: rhs_right,
                ..
            },
        ) => {
            lhs_op == rhs_op
                && same_integer_expression(lhs_left, rhs_left)
                && same_integer_expression(lhs_right, rhs_right)
        }
        _ => false,
    }
}

/// Inline straight-line scalar scratch definitions inside compact loops.
///
/// A call-local scalar has no observable identity once its final value is not
/// read after the loop. Within an assignment-only loop body, definitions
/// dominate every substitution in source order. Refusing self-reads without a
/// prior definition preserves genuinely loop-carried state; refusing locals
/// read by the containing suffix preserves values that escape the loop.
fn inline_dead_loop_scalar_locals(
    statements: &[rumoca_core::Statement],
    scalar_locals: &HashSet<VarName>,
    branch_locals: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    inline_dead_loop_locals_followed_by(statements, &[], scalar_locals, branch_locals)
}

fn inline_dead_loop_locals_followed_by(
    statements: &[rumoca_core::Statement],
    following: &[&[rumoca_core::Statement]],
    scalar_locals: &HashSet<VarName>,
    branch_locals: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    statements
        .iter()
        .enumerate()
        .map(|(ordinal, statement)| {
            let mut statement_following = Vec::with_capacity(following.len() + 1);
            statement_following.push(&statements[ordinal + 1..]);
            statement_following.extend_from_slice(following);
            inline_dead_scalars_in_statement(
                statement,
                &statement_following,
                scalar_locals,
                branch_locals,
            )
        })
        .collect()
}

fn inline_dead_scalars_in_statement(
    statement: &rumoca_core::Statement,
    following: &[&[rumoca_core::Statement]],
    scalar_locals: &HashSet<VarName>,
    branch_locals: &HashSet<VarName>,
) -> rumoca_core::Statement {
    match statement {
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        } => {
            let mut body_following: Vec<&[rumoca_core::Statement]> =
                Vec::with_capacity(following.len() + 1);
            body_following.push(equations.as_slice());
            body_following.extend_from_slice(following);
            let equations = inline_dead_loop_locals_followed_by(
                equations,
                &body_following,
                scalar_locals,
                branch_locals,
            );
            let equations =
                inline_straight_line_scalar_definitions(&equations, following, scalar_locals);
            statement_with_equations(indices, equations, *span)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => rumoca_core::Statement::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|block| {
                    let branch = inline_dead_loop_locals_followed_by(
                        &block.stmts,
                        following,
                        scalar_locals,
                        branch_locals,
                    );
                    rumoca_core::StatementBlock {
                        cond: block.cond.clone(),
                        stmts: inline_straight_line_scalar_definitions(
                            &branch,
                            following,
                            branch_locals,
                        ),
                    }
                })
                .collect(),
            else_block: else_block.as_ref().map(|branch| {
                let branch = inline_dead_loop_locals_followed_by(
                    branch,
                    following,
                    scalar_locals,
                    branch_locals,
                );
                inline_straight_line_scalar_definitions(&branch, following, branch_locals)
            }),
            span: *span,
        },
        _ => statement.clone(),
    }
}

fn inline_straight_line_scalar_definitions(
    statements: &[rumoca_core::Statement],
    following: &[&[rumoca_core::Statement]],
    scalar_locals: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    if !statements
        .iter()
        .all(|statement| matches!(statement, rumoca_core::Statement::Assignment { .. }))
    {
        return statements.to_vec();
    }
    let removable = scalar_locals
        .iter()
        .filter(|name| {
            !statement_segments_read_incoming_name(following, name)
                && !statements_read_nonrewritable_name(statements, name)
        })
        .cloned()
        .collect::<HashSet<_>>();
    let mut substitutions = HashMap::new();
    let mut inlined = Vec::with_capacity(statements.len());
    for (ordinal, statement) in statements.iter().enumerate() {
        let rumoca_core::Statement::Assignment { comp, value, span } = statement else {
            unreachable!("straight-line scalar substitution accepts assignments only")
        };
        let mut rewriter = LocalSubstitution {
            values: &substitutions,
        };
        let rewritten_value = rewriter.rewrite_expression(value);
        let rewritten_comp = rewriter.rewrite_component_reference(comp);
        let target = comp.to_var_name();
        let scalar_definition = comp
            .parts()
            .as_ref()
            .first()
            .is_some_and(|part| comp.parts().len() == 1 && part.subs.is_empty());
        if scalar_definition
            && removable.contains(&target)
            && !statements_partially_assign_name(&statements[ordinal + 1..], &target)
            && !expression_reads_name(&rewritten_value, &target)
        {
            substitutions.insert(target, rewritten_value);
            continue;
        }
        substitutions.remove(&target);
        inlined.push(rumoca_core::Statement::Assignment {
            comp: rewritten_comp,
            value: rewritten_value,
            span: *span,
        });
    }
    inlined
}

#[derive(Clone, Copy)]
struct IncomingValueFlow {
    read: bool,
    definitely_defined: bool,
}

fn statements_read_incoming_name(statements: &[rumoca_core::Statement], name: &VarName) -> bool {
    statement_segments_read_incoming_name(&[statements], name)
}

fn statement_segments_read_incoming_name(
    segments: &[&[rumoca_core::Statement]],
    name: &VarName,
) -> bool {
    let mut definitely_defined = false;
    for statements in segments {
        for statement in *statements {
            let flow = statement_incoming_value_flow(statement, name);
            if !definitely_defined && flow.read {
                return true;
            }
            definitely_defined |= flow.definitely_defined;
        }
    }
    false
}

fn statement_incoming_value_flow(
    statement: &rumoca_core::Statement,
    name: &VarName,
) -> IncomingValueFlow {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, .. } => IncomingValueFlow {
            read: expression_reads_name(value, name) || component_subscripts_read_name(comp, name),
            definitely_defined: scalar_assignment_target(comp).as_ref() == Some(name),
        },
        rumoca_core::Statement::For {
            indices, equations, ..
        } => IncomingValueFlow {
            read: indices
                .iter()
                .any(|index| expression_reads_name(&index.range, name))
                || statements_read_incoming_name(equations, name),
            // A compact domain may be empty, so its body cannot settle a value
            // observed after the loop.
            definitely_defined: false,
        },
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            let read = cond_blocks.iter().any(|block| {
                expression_reads_name(&block.cond, name)
                    || statements_read_incoming_name(&block.stmts, name)
            }) || else_block
                .as_ref()
                .is_some_and(|branch| statements_read_incoming_name(branch, name));
            let definitely_defined = else_block.as_ref().is_some_and(|fallback| {
                cond_blocks
                    .iter()
                    .all(|block| statements_definitely_define_name(&block.stmts, name))
                    && statements_definitely_define_name(fallback, name)
            });
            IncomingValueFlow {
                read,
                definitely_defined,
            }
        }
        _ => IncomingValueFlow {
            read: statements_read_name(std::slice::from_ref(statement), name),
            definitely_defined: false,
        },
    }
}

fn statements_definitely_define_name(
    statements: &[rumoca_core::Statement],
    name: &VarName,
) -> bool {
    statements
        .iter()
        .any(|statement| statement_incoming_value_flow(statement, name).definitely_defined)
}

fn scalar_assignment_target(component: &rumoca_core::ComponentReference) -> Option<VarName> {
    let [part] = component.parts() else {
        return None;
    };
    part.subs.is_empty().then(|| component.to_var_name())
}

fn component_subscripts_read_name(
    component: &rumoca_core::ComponentReference,
    name: &VarName,
) -> bool {
    component
        .parts()
        .iter()
        .flat_map(|part| &part.subs)
        .any(|subscript| match subscript {
            Subscript::Expr { expr, .. } => expression_reads_name(expr, name),
            Subscript::Index { .. } | Subscript::Colon { .. } => false,
        })
}

/// Turn an exhaustive, dependency-free conditional element definition into
/// direct conditional assignments inside a compact loop.
///
/// `if c then A[i] := x else A[i] := y` owns one tensor element for every
/// domain point exactly as `A[i] := if c then x else y`. The rewrite is
/// admitted only when every branch writes the same whole receiving list once
/// and no right-hand side reads any value in that list, so sequential
/// algorithm semantics cannot be erased accidentally.
fn compact_exhaustive_loop_conditionals(
    statements: &[rumoca_core::Statement],
    in_loop: bool,
) -> Vec<rumoca_core::Statement> {
    let mut compacted = Vec::new();
    for statement in statements {
        match statement {
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => compacted.push(rumoca_core::Statement::For {
                indices: indices.clone(),
                equations: compact_exhaustive_loop_conditionals(equations, true),
                span: *span,
            }),
            rumoca_core::Statement::If { .. } if in_loop => {
                if let Some(assignments) = collapse_exhaustive_assignment_conditional(statement) {
                    compacted.extend(assignments);
                } else {
                    compacted.push(statement.clone());
                }
            }
            _ => compacted.push(statement.clone()),
        }
    }
    compacted
}

fn collapse_exhaustive_assignment_conditional(
    statement: &rumoca_core::Statement,
) -> Option<Vec<rumoca_core::Statement>> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block: Some(fallback),
        span,
    } = statement
    else {
        return None;
    };
    let branches = cond_blocks
        .iter()
        .map(|block| branch_assignment_values(&block.stmts))
        .collect::<Option<Vec<_>>>()?;
    let fallback = branch_assignment_values(fallback)?;
    let first = branches.first()?.clone();
    if first.is_empty()
        || branches
            .iter()
            .chain(std::iter::once(&fallback))
            .any(|branch| !same_assignment_targets(&first, branch))
        || branches
            .iter()
            .chain(std::iter::once(&fallback))
            .any(|branch| branch_has_sequential_target_read(branch))
    {
        return None;
    }
    let mut assignments = Vec::with_capacity(first.len());
    for (ordinal, (target, _)) in first.iter().enumerate() {
        let values = branches
            .iter()
            .zip(cond_blocks)
            .map(|(branch, block)| (block.cond.clone(), branch[ordinal].1.clone()))
            .collect();
        assignments.push(rumoca_core::Statement::Assignment {
            comp: target.clone(),
            value: Expression::If {
                branches: values,
                else_branch: Box::new(fallback[ordinal].1.clone()),
                span: *span,
            },
            span: *span,
        });
    }
    Some(assignments)
}

type BranchAssignment = (rumoca_core::ComponentReference, Expression);

fn branch_assignment_values(
    statements: &[rumoca_core::Statement],
) -> Option<Vec<BranchAssignment>> {
    let mut assignments = Vec::new();
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                assignments.push((comp.clone(), value.clone()));
            }
            rumoca_core::Statement::If { .. } => {
                assignments.extend(
                    collapse_exhaustive_assignment_conditional(statement)?
                        .into_iter()
                        .map(collapsed_branch_assignment),
                );
            }
            _ => return None,
        }
    }
    Some(assignments)
}

fn collapsed_branch_assignment(statement: rumoca_core::Statement) -> BranchAssignment {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, .. } => (comp, value),
        _ => unreachable!("collapsed conditionals contain assignments"),
    }
}

fn same_assignment_targets(expected: &[BranchAssignment], found: &[BranchAssignment]) -> bool {
    expected.len() == found.len()
        && expected
            .iter()
            .zip(found)
            .all(|((expected, _), (found, _))| {
                assignment_targets_semantically_equal(expected, found)
            })
}

fn assignment_targets_semantically_equal(
    expected: &rumoca_core::ComponentReference,
    found: &rumoca_core::ComponentReference,
) -> bool {
    if expected.to_var_name() != found.to_var_name() {
        return false;
    }
    let expected = expected
        .parts()
        .iter()
        .flat_map(|part| &part.subs)
        .collect::<Vec<_>>();
    let found = found
        .parts()
        .iter()
        .flat_map(|part| &part.subs)
        .collect::<Vec<_>>();
    expected.len() == found.len()
        && expected
            .into_iter()
            .zip(found)
            .all(|(expected, found)| match (expected, found) {
                (
                    Subscript::Index {
                        value: expected, ..
                    },
                    Subscript::Index { value: found, .. },
                ) => expected == found,
                (Subscript::Colon { .. }, Subscript::Colon { .. }) => true,
                (Subscript::Expr { expr: expected, .. }, Subscript::Expr { expr: found, .. }) => {
                    rumoca_core::expressions_semantically_equal(expected, found)
                }
                _ => false,
            })
}

fn branch_has_sequential_target_read(assignments: &[BranchAssignment]) -> bool {
    let targets = assignments
        .iter()
        .map(|(target, _)| target.to_var_name())
        .collect::<HashSet<_>>();
    assignments
        .iter()
        .any(|(_, value)| expression_reads_any_name(value, &targets))
}

/// Substitute a scalar Integer local whose value is proved by specialization.
///
/// A top-level assignment such as `n := size(input, 1)` settles `n` for every
/// later statement when no later statement writes it. Replacing only that
/// suffix with the proven literal preserves algorithm order while exposing a
/// compact static loop domain. The assignment itself then has no observable
/// value and can be removed; function locals are call-scoped (MLS §12.2).
fn inline_settled_shape_integer_locals(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    function: &rumoca_core::Function,
    flat: &flat::Model,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let integer_locals = function
        .locals
        .iter()
        .filter(|local| {
            effective_function_scalar_type(flat, local) == Some(dae::ScalarType::Integer)
        })
        .map(|local| VarName::new(&local.name))
        .collect::<HashSet<_>>();
    inline_settled_integer_sequence(statements, static_integers, shapes, &integer_locals)
}

fn inline_settled_integer_sequence(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    integer_locals: &HashSet<VarName>,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let mut environment = static_integers.clone();
    let mut substitutions = HashMap::new();
    let mut normalized = Vec::with_capacity(statements.len());
    for (ordinal, statement) in statements.iter().enumerate() {
        let mut rewriter = LocalSubstitution {
            values: &substitutions,
        };
        let statement = rewriter.rewrite_statement(statement);
        let Some((target, expression)) =
            scalar_integer_local_definition(&statement, &statements[ordinal + 1..], integer_locals)
        else {
            normalized.push(statement);
            continue;
        };
        let Some(value) = static_shape_integer_expression(&expression, &environment, shapes)?
        else {
            normalized.push(statement);
            continue;
        };
        let span = expression_span(&expression)?;
        let literal = Expression::Literal {
            value: Literal::Integer(value),
            span,
        };
        environment.insert(target.clone(), value);
        substitutions.insert(target, literal);
    }
    Ok(normalized)
}

fn scalar_integer_local_definition(
    statement: &rumoca_core::Statement,
    suffix: &[rumoca_core::Statement],
    integer_locals: &HashSet<VarName>,
) -> Option<(VarName, Expression)> {
    let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
        return None;
    };
    let [part] = comp.parts() else {
        return None;
    };
    let target = comp.to_var_name();
    (part.subs.is_empty()
        && integer_locals.contains(&target)
        && !expression_reads_name(value, &target)
        && !assigned_function_targets(suffix).contains(target.as_str()))
    .then(|| (target, value.clone()))
}

fn push_invariant_conditions_to_fixpoint(
    mut statements: Vec<rumoca_core::Statement>,
    protected: &HashSet<VarName>,
    defined_protected: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    loop {
        let lowered =
            push_invariant_conditions_into_loops(&statements, protected, defined_protected);
        if lowered == statements {
            return lowered;
        }
        statements = lowered;
    }
}

fn push_invariant_conditions_into_loops(
    statements: &[rumoca_core::Statement],
    protected: &HashSet<VarName>,
    inherited_definitions: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    let mut lowered = Vec::new();
    let mut defined_protected = inherited_definitions.clone();
    for statement in statements {
        let statement = push_conditions_in_statement(statement, protected, &defined_protected);
        if let Some(split) =
            split_invariant_conditional_sequence(&statement, protected, &defined_protected)
        {
            lowered.extend(split);
            continue;
        }
        let rumoca_core::Statement::If {
            cond_blocks,
            else_block: None,
            span,
        } = &statement
        else {
            remember_whole_protected_assignment(&statement, protected, &mut defined_protected);
            lowered.push(statement);
            continue;
        };
        let [block] = cond_blocks.as_slice() else {
            lowered.push(statement.clone());
            continue;
        };
        if block.stmts.is_empty()
            || !block
                .stmts
                .iter()
                .any(|nested| matches!(nested, rumoca_core::Statement::For { .. }))
        {
            lowered.push(statement.clone());
            continue;
        }
        let mut assigned = HashSet::new();
        collect_assigned_values(&block.stmts, &mut assigned);
        if !protected_assignments_have_fallthrough_values(&assigned, protected, &defined_protected)
            || expression_reads_any_name(&block.cond, &assigned)
        {
            lowered.push(statement.clone());
            remember_whole_protected_assignment(&statement, protected, &mut defined_protected);
            continue;
        }
        lowered.extend(block.stmts.iter().map(|nested| match nested {
            rumoca_core::Statement::For {
                indices,
                equations,
                span: loop_span,
            } => rumoca_core::Statement::For {
                indices: indices.clone(),
                equations: vec![rumoca_core::Statement::If {
                    cond_blocks: vec![rumoca_core::StatementBlock {
                        cond: block.cond.clone(),
                        stmts: equations.clone(),
                    }],
                    else_block: None,
                    span: *span,
                }],
                span: *loop_span,
            },
            _ => rumoca_core::Statement::If {
                cond_blocks: vec![rumoca_core::StatementBlock {
                    cond: block.cond.clone(),
                    stmts: vec![nested.clone()],
                }],
                else_block: None,
                span: *span,
            },
        }));
        remember_whole_protected_assignment(&statement, protected, &mut defined_protected);
    }
    lowered
}

fn split_invariant_conditional_sequence(
    statement: &rumoca_core::Statement,
    protected: &HashSet<VarName>,
    defined_protected: &HashSet<VarName>,
) -> Option<Vec<rumoca_core::Statement>> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    else {
        return None;
    };
    if cond_blocks.len() < 2
        || !cond_blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .chain(else_block.iter().flat_map(|branch| branch.iter()))
            .any(|nested| matches!(nested, rumoca_core::Statement::For { .. }))
    {
        return None;
    }
    let mut all_assigned = HashSet::new();
    for block in cond_blocks {
        collect_assigned_values(&block.stmts, &mut all_assigned);
    }
    if let Some(fallback) = else_block {
        collect_assigned_values(fallback, &mut all_assigned);
    }
    if !protected_assignments_have_fallthrough_values(&all_assigned, protected, defined_protected) {
        return None;
    }
    for block in cond_blocks {
        let mut assigned = HashSet::new();
        collect_assigned_values(&block.stmts, &mut assigned);
        if expression_reads_any_name(&block.cond, &assigned) {
            return None;
        }
    }
    if let Some(fallback) = else_block {
        let mut assigned = HashSet::new();
        collect_assigned_values(fallback, &mut assigned);
        if cond_blocks
            .iter()
            .any(|block| expression_reads_any_name(&block.cond, &assigned))
        {
            return None;
        }
    }
    let mut prior_false: Option<Expression> = None;
    let mut split = Vec::new();
    for block in cond_blocks {
        let guard = match &prior_false {
            Some(prior) => Expression::Binary {
                op: OpBinary::And,
                lhs: Box::new(prior.clone()),
                rhs: Box::new(block.cond.clone()),
                span: *span,
            },
            None => block.cond.clone(),
        };
        split.extend(
            block
                .stmts
                .iter()
                .map(|nested| guarded_statement(guard.clone(), nested.clone(), *span)),
        );
        let not_condition = Expression::Unary {
            op: OpUnary::Not,
            rhs: Box::new(block.cond.clone()),
            span: *span,
        };
        prior_false = Some(match prior_false {
            Some(prior) => Expression::Binary {
                op: OpBinary::And,
                lhs: Box::new(prior),
                rhs: Box::new(not_condition),
                span: *span,
            },
            None => not_condition,
        });
    }
    if let (Some(fallback), Some(guard)) = (else_block, prior_false) {
        split.extend(
            fallback
                .iter()
                .map(|nested| guarded_statement(guard.clone(), nested.clone(), *span)),
        );
    }
    Some(split)
}

/// Predicate pushdown retains the pre-conditional value on every false arm.
/// An output may therefore move under a compact loop only after a whole-value
/// assignment has already established that fall-through value. Protected
/// locals need no extra condition because construction can own their dead
/// initial seed; declared outputs cannot acquire such a value by inference.
fn protected_assignments_have_fallthrough_values(
    assigned: &HashSet<VarName>,
    protected: &HashSet<VarName>,
    defined_protected: &HashSet<VarName>,
) -> bool {
    assigned
        .intersection(protected)
        .all(|target| defined_protected.contains(target))
}

fn remember_whole_protected_assignment(
    statement: &rumoca_core::Statement,
    protected: &HashSet<VarName>,
    defined_protected: &mut HashSet<VarName>,
) {
    let rumoca_core::Statement::Assignment { comp, .. } = statement else {
        return;
    };
    let [part] = comp.parts() else {
        return;
    };
    let target = comp.to_var_name();
    if part.subs.is_empty() && protected.contains(&target) {
        defined_protected.insert(target);
    }
}

fn guarded_statement(
    guard: Expression,
    statement: rumoca_core::Statement,
    span: Span,
) -> rumoca_core::Statement {
    rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: guard,
            stmts: vec![statement],
        }],
        else_block: None,
        span,
    }
}

fn push_conditions_in_statement(
    statement: &rumoca_core::Statement,
    protected: &HashSet<VarName>,
    defined_protected: &HashSet<VarName>,
) -> rumoca_core::Statement {
    match statement {
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        } => rumoca_core::Statement::For {
            indices: indices.clone(),
            equations: push_invariant_conditions_into_loops(
                equations,
                protected,
                defined_protected,
            ),
            span: *span,
        },
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => rumoca_core::Statement::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|block| rumoca_core::StatementBlock {
                    cond: block.cond.clone(),
                    stmts: push_invariant_conditions_into_loops(
                        &block.stmts,
                        protected,
                        defined_protected,
                    ),
                })
                .collect(),
            else_block: else_block.as_ref().map(|branch| {
                push_invariant_conditions_into_loops(branch, protected, defined_protected)
            }),
            span: *span,
        },
        _ => statement.clone(),
    }
}

fn expression_reads_any_name(expression: &Expression, names: &HashSet<VarName>) -> bool {
    struct Finder<'a> {
        names: &'a HashSet<VarName>,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            self.found |= self.names.contains(reference.var_name());
            self.walk_var_ref(reference, subscripts);
        }
    }
    let mut finder = Finder {
        names,
        found: false,
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut finder, expression);
    finder.found
}

/// Split independent outer partitions into sequential compact loop owners.
///
/// If every read and write of a value mutated below `rhs` is indexed by that
/// same `rhs`, different outer coordinates are disjoint. Distributing
/// `for rhs loop forward(rhs); reverse(rhs); end` into two compact loops then
/// preserves the per-coordinate dependency while avoiding a nested owner list.
fn distribute_partitioned_outer_loops(
    statements: &[rumoca_core::Statement],
) -> Vec<rumoca_core::Statement> {
    let mut distributed = Vec::new();
    for statement in statements {
        let statement = distribute_nested_outer_loops(statement);
        let rumoca_core::Statement::For {
            indices,
            equations,
            span,
        } = &statement
        else {
            distributed.push(statement);
            continue;
        };
        let [index] = indices.as_slice() else {
            distributed.push(statement);
            continue;
        };
        if equations.len() < 2
            || !equations
                .iter()
                .all(|nested| matches!(nested, rumoca_core::Statement::For { .. }))
            || !loop_values_partitioned_by(equations, &index.ident)
        {
            distributed.push(statement);
            continue;
        }
        distributed.extend(equations.iter().map(|nested| rumoca_core::Statement::For {
            indices: indices.clone(),
            equations: vec![nested.clone()],
            span: *span,
        }));
    }
    distributed
}

fn distribute_nested_outer_loops(statement: &rumoca_core::Statement) -> rumoca_core::Statement {
    match statement {
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        } => rumoca_core::Statement::For {
            indices: indices.clone(),
            equations: distribute_partitioned_outer_loops(equations),
            span: *span,
        },
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => rumoca_core::Statement::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|block| rumoca_core::StatementBlock {
                    cond: block.cond.clone(),
                    stmts: distribute_partitioned_outer_loops(&block.stmts),
                })
                .collect(),
            else_block: else_block
                .as_ref()
                .map(|branch| distribute_partitioned_outer_loops(branch)),
            span: *span,
        },
        _ => statement.clone(),
    }
}

fn loop_values_partitioned_by(statements: &[rumoca_core::Statement], binder: &str) -> bool {
    let mut mutated = HashSet::new();
    collect_assigned_values(statements, &mut mutated);
    if mutated.is_empty() {
        return false;
    }
    struct PartitionCheck<'a> {
        binder: &'a str,
        mutated: &'a HashSet<VarName>,
        valid: bool,
    }
    impl rumoca_core::ExpressionVisitor for PartitionCheck<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            if self.mutated.contains(reference.var_name())
                && !subscripts_read_binder(subscripts, self.binder)
            {
                self.valid = false;
            }
            self.walk_var_ref(reference, subscripts);
        }
    }
    impl rumoca_ir_flat::visitor::StatementVisitor for PartitionCheck<'_> {
        fn visit_component_reference(&mut self, reference: &rumoca_core::ComponentReference) {
            if self.mutated.contains(&reference.to_var_name())
                && !reference
                    .parts()
                    .iter()
                    .any(|part| subscripts_read_binder(&part.subs, self.binder))
            {
                self.valid = false;
            }
            reference
                .parts()
                .iter()
                .flat_map(|part| &part.subs)
                .for_each(|subscript| {
                    rumoca_core::ExpressionVisitor::visit_subscript(self, subscript);
                });
        }
    }
    let mut check = PartitionCheck {
        binder,
        mutated: &mutated,
        valid: true,
    };
    for statement in statements {
        rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut check, statement);
    }
    check.valid
}

fn collect_assigned_values(statements: &[rumoca_core::Statement], assigned: &mut HashSet<VarName>) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } => {
                if let Some(root) = comp.parts().first() {
                    assigned.insert(VarName::new(&root.ident));
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_assigned_values(equations, assigned);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_assigned_values(&block.stmts, assigned);
                }
                if let Some(branch) = else_block {
                    collect_assigned_values(branch, assigned);
                }
            }
            _ => {}
        }
    }
}

fn subscripts_read_binder(subscripts: &[Subscript], binder: &str) -> bool {
    subscripts.iter().any(|subscript| match subscript {
        Subscript::Expr { expr, .. } => expression_reads_name(expr, &VarName::new(binder)),
        Subscript::Index { .. } | Subscript::Colon { .. } => false,
    })
}
