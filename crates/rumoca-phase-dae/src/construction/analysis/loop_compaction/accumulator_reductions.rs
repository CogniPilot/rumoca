//! Compact ordered accumulator loops into tensor-native reductions.

use super::*;

/// Replace scalar slice accumulations with tensor-native reductions.
///
/// `value := seed; for k in r loop value := value - term[k]; end for`
/// is exactly `value := seed - sum(term[r])`, including an empty `r`. The
/// source loop therefore becomes one compact expression owner instead of a
/// scalar transition family.
pub(super) fn compact_accumulator_loops(
    statements: &[rumoca_core::Statement],
    shapes: &ShapeEnvironment,
) -> Vec<rumoca_core::Statement> {
    let nested = statements
        .iter()
        .map(|statement| compact_accumulators_in_statement(statement, shapes))
        .collect::<Vec<_>>();
    let mut compacted = Vec::with_capacity(nested.len());
    let mut ordinal = 0;
    while ordinal < nested.len() {
        if let Some(replacement) = nested.get(ordinal + 1).and_then(|loop_statement| {
            compact_one_accumulator(&nested[ordinal], loop_statement, shapes)
        }) {
            compacted.push(replacement);
            ordinal += 2;
        } else if let Some(replacement) = compact_standalone_accumulator(&nested[ordinal], shapes) {
            compacted.push(replacement);
            ordinal += 1;
        } else {
            compacted.push(nested[ordinal].clone());
            ordinal += 1;
        }
    }
    compacted
}

fn compact_standalone_accumulator(
    loop_statement: &rumoca_core::Statement,
    shapes: &ShapeEnvironment,
) -> Option<rumoca_core::Statement> {
    let rumoca_core::Statement::For {
        equations, span, ..
    } = loop_statement
    else {
        return None;
    };
    let [rumoca_core::Statement::Assignment { comp, .. }] = equations.as_slice() else {
        return None;
    };
    let [part] = comp.parts() else {
        return None;
    };
    if !part.subs.is_empty() {
        return None;
    }
    let seed = rumoca_core::Statement::Assignment {
        comp: comp.clone(),
        value: Expression::VarRef {
            name: Reference::from_component_reference(comp.clone()),
            subscripts: Vec::new(),
            span: *span,
        },
        span: *span,
    };
    compact_one_accumulator(&seed, loop_statement, shapes)
}

fn compact_accumulators_in_statement(
    statement: &rumoca_core::Statement,
    shapes: &ShapeEnvironment,
) -> rumoca_core::Statement {
    match statement {
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        } => rumoca_core::Statement::For {
            indices: indices.clone(),
            equations: compact_accumulator_loops(equations, shapes),
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
                    stmts: compact_accumulator_loops(&block.stmts, shapes),
                })
                .collect(),
            else_block: else_block
                .as_ref()
                .map(|statements| compact_accumulator_loops(statements, shapes)),
            span: *span,
        },
        _ => statement.clone(),
    }
}

fn compact_one_accumulator(
    seed: &rumoca_core::Statement,
    loop_statement: &rumoca_core::Statement,
    shapes: &ShapeEnvironment,
) -> Option<rumoca_core::Statement> {
    if let Some(compacted) = compact_first_match_accumulator(seed, loop_statement) {
        return Some(compacted);
    }
    let rumoca_core::Statement::Assignment {
        comp: seed_target,
        value: seed_value,
        span,
    } = seed
    else {
        return None;
    };
    let [seed_part] = seed_target.parts() else {
        return None;
    };
    let rumoca_core::Statement::For {
        indices, equations, ..
    } = loop_statement
    else {
        return None;
    };
    let [index] = indices.as_slice() else {
        return None;
    };
    let [
        rumoca_core::Statement::Assignment {
            comp: update_target,
            value: update,
            ..
        },
    ] = equations.as_slice()
    else {
        return None;
    };
    let [update_part] = update_target.parts() else {
        return None;
    };
    if !seed_part.subs.is_empty()
        || !update_part.subs.is_empty()
        || update_target.to_var_name() != seed_target.to_var_name()
    {
        return None;
    }
    let Expression::Binary {
        op: op @ (OpBinary::Add | OpBinary::Sub | OpBinary::Mul),
        lhs,
        rhs,
        ..
    } = update
    else {
        return None;
    };
    if !matches!(
        lhs.as_ref(),
        Expression::VarRef { name, subscripts, .. }
            if name.var_name() == &update_target.to_var_name() && subscripts.is_empty()
    ) {
        return None;
    }
    let target = update_target.to_var_name();
    // Moving the seed ahead of a tensor reduction is valid only when the
    // reduction domain and term do not read the carried value. In the source,
    // those reads observe the seed (and, for the term, potentially earlier
    // iterations); after compaction they would read the result being defined.
    // That is neither a dead read nor the same recurrence, so retain the loop
    // for its checked domain/transition owner instead of manufacturing one.
    if expression_reads_name(&index.range, &target) || expression_reads_name(rhs, &target) {
        return None;
    }
    let binder = VarName::new(&index.ident);
    if !expression_reads_name(rhs, &binder) {
        return None;
    }
    let (range, term) = bounded_reduction_domain(index, rhs, op, shapes, *span)?;
    let function = if *op == OpBinary::Mul {
        BuiltinFunction::Product
    } else {
        BuiltinFunction::Sum
    };
    let target_shape = shapes.get(&seed_target.to_var_name())?;
    let reduction_span = expression_span(&index.range).ok()?;
    let reduction = tensor_accumulator_reduction(
        term,
        rumoca_core::ComprehensionIndex {
            name: index.ident.clone(),
            range,
        },
        target_shape,
        function,
        reduction_span,
    );
    Some(rumoca_core::Statement::Assignment {
        comp: seed_target.clone(),
        value: Expression::Binary {
            op: op.clone(),
            lhs: Box::new(seed_value.clone()),
            rhs: Box::new(reduction),
            span: *span,
        },
        span: *span,
    })
}

fn tensor_accumulator_reduction(
    term: Expression,
    reduction_index: rumoca_core::ComprehensionIndex,
    target_shape: &[u32],
    function: BuiltinFunction,
    span: Span,
) -> Expression {
    if target_shape.is_empty() {
        return Expression::BuiltinCall {
            function,
            args: vec![Expression::ArrayComprehension {
                expr: Box::new(term),
                indices: vec![reduction_index],
                filter: None,
                span,
            }],
            span,
        };
    }
    let mut indices = Vec::with_capacity(target_shape.len());
    let mut subscripts = Vec::with_capacity(target_shape.len());
    for (axis, extent) in target_shape.iter().enumerate() {
        let name = rumoca_core::tensor_reduction_binder_name(span.start.0, axis);
        indices.push(rumoca_core::ComprehensionIndex {
            name: name.clone(),
            range: Expression::Range {
                start: Box::new(Expression::Literal {
                    value: Literal::Integer(1),
                    span,
                }),
                step: None,
                end: Box::new(Expression::Literal {
                    value: Literal::Integer(i64::from(*extent)),
                    span,
                }),
                span,
            },
        });
        subscripts.push(Subscript::Expr {
            expr: Box::new(Expression::VarRef {
                name: Reference::generated(&name),
                subscripts: Vec::new(),
                span,
            }),
            span,
        });
    }
    let scalar_term = Expression::Index {
        base: Box::new(term),
        subscripts,
        span,
    };
    let reduction = Expression::BuiltinCall {
        function,
        args: vec![Expression::ArrayComprehension {
            expr: Box::new(scalar_term),
            indices: vec![reduction_index],
            filter: None,
            span,
        }],
        span,
    };
    Expression::ArrayComprehension {
        expr: Box::new(reduction),
        indices,
        filter: None,
        span,
    }
}

/// Turn a runtime unit-step upper bound with a proven finite ceiling into one
/// fixed compact domain. Points beyond the runtime bound contribute the exact
/// reduction identity, so no scalar loop or guessed extent enters DAE IR.
fn bounded_reduction_domain(
    index: &rumoca_core::ForIndex,
    term: &Expression,
    op: &OpBinary,
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
    if shapes.proven_extent(start).is_some()
        && step
            .as_deref()
            .is_none_or(|step| shapes.proven_extent(step).is_some())
        && shapes.proven_extent(end).is_some()
    {
        return Some((index.range.clone(), term.clone()));
    }
    let start_value = shapes.proven_extent(start)?;
    let step_value = step
        .as_deref()
        .map(|step| shapes.proven_extent(step))
        .unwrap_or(Some(1))?;
    if step_value != 1 {
        return None;
    }
    let (_, upper) = shapes.proven_integer_bounds(end)?;
    let fixed_range = Expression::Range {
        start: start.clone(),
        step: None,
        end: Box::new(Expression::Literal {
            value: Literal::Integer(upper),
            span: *range_span,
        }),
        span: *range_span,
    };
    let binder = Expression::VarRef {
        name: Reference::new(&index.ident),
        subscripts: Vec::new(),
        span,
    };
    let active = Expression::Binary {
        op: OpBinary::Le,
        lhs: Box::new(binder),
        rhs: end.clone(),
        span,
    };
    let identity = Expression::Literal {
        value: Literal::Integer(if *op == OpBinary::Mul { 1 } else { 0 }),
        span,
    };
    let guarded = Expression::If {
        branches: vec![(active, term.clone())],
        else_branch: Box::new(identity),
        span,
    };
    // A ceiling below the fixed start denotes an empty compact domain; retaining
    // that exact range preserves the source loop's identity result.
    let _ = start_value;
    Some((fixed_range, guarded))
}

/// Replace an ordered first-match scan with one nonempty tensor reduction.
///
/// `found := 0; for i in r loop if found == 0 and p(i) then found := i; end if;
/// end for` is the minimum matching index over the ascending unit-step range.
/// Appending `upper + 1` makes the reduction total even when the range is empty;
/// the final conditional maps that sentinel back to the source seed.
fn compact_first_match_accumulator(
    seed: &rumoca_core::Statement,
    loop_statement: &rumoca_core::Statement,
) -> Option<rumoca_core::Statement> {
    let proof = first_match_accumulator(seed, loop_statement)?;
    Some(lower_first_match_accumulator(proof))
}

struct FirstMatchAccumulator {
    target: rumoca_core::ComponentReference,
    binder: String,
    range: Expression,
    end: Expression,
    predicate: Expression,
    span: Span,
}

fn first_match_accumulator(
    seed: &rumoca_core::Statement,
    loop_statement: &rumoca_core::Statement,
) -> Option<FirstMatchAccumulator> {
    let rumoca_core::Statement::Assignment {
        comp: target,
        value:
            Expression::Literal {
                value: Literal::Integer(0),
                ..
            },
        span,
    } = seed
    else {
        return None;
    };
    let [target_part] = target.parts() else {
        return None;
    };
    if !target_part.subs.is_empty() {
        return None;
    }
    let rumoca_core::Statement::For {
        indices, equations, ..
    } = loop_statement
    else {
        return None;
    };
    let [index] = indices.as_slice() else {
        return None;
    };
    let Expression::Range {
        start: _,
        step: None,
        end,
        ..
    } = &index.range
    else {
        return None;
    };
    let [
        rumoca_core::Statement::If {
            cond_blocks,
            else_block: None,
            ..
        },
    ] = equations.as_slice()
    else {
        return None;
    };
    let [block] = cond_blocks.as_slice() else {
        return None;
    };
    let [
        rumoca_core::Statement::Assignment {
            comp: update_target,
            value: update,
            ..
        },
    ] = block.stmts.as_slice()
    else {
        return None;
    };
    if update_target.to_var_name() != target.to_var_name()
        || !matches!(
            update,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == index.ident && subscripts.is_empty()
        )
    {
        return None;
    }
    let predicate = first_match_predicate(&block.cond, &target.to_var_name())?;
    Some(FirstMatchAccumulator {
        target: target.clone(),
        binder: index.ident.clone(),
        range: index.range.clone(),
        end: end.as_ref().clone(),
        predicate,
        span: *span,
    })
}

fn lower_first_match_accumulator(proof: FirstMatchAccumulator) -> rumoca_core::Statement {
    let FirstMatchAccumulator {
        target,
        binder,
        range,
        end,
        predicate,
        span,
    } = proof;
    let one = Expression::Literal {
        value: Literal::Integer(1),
        span,
    };
    let sentinel = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(end.clone()),
        rhs: Box::new(one.clone()),
        span,
    };
    let candidate = Expression::VarRef {
        name: Reference::new(&binder),
        subscripts: Vec::new(),
        span,
    };
    let candidate_or_sentinel = Expression::If {
        branches: vec![(predicate, candidate)],
        else_branch: Box::new(sentinel.clone()),
        span,
    };
    let candidates = Expression::ArrayComprehension {
        expr: Box::new(candidate_or_sentinel),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: binder,
            range,
        }],
        filter: None,
        span,
    };
    let nonempty = Expression::BuiltinCall {
        function: BuiltinFunction::Cat,
        args: vec![
            one,
            candidates,
            Expression::Array {
                elements: vec![sentinel.clone()],
                is_matrix: false,
                span,
            },
        ],
        span,
    };
    let first = Expression::BuiltinCall {
        function: BuiltinFunction::Min,
        args: vec![nonempty],
        span,
    };
    let matched = Expression::Binary {
        op: OpBinary::Le,
        lhs: Box::new(first.clone()),
        rhs: Box::new(end),
        span,
    };
    rumoca_core::Statement::Assignment {
        comp: target,
        value: Expression::If {
            branches: vec![(matched, first)],
            else_branch: Box::new(Expression::Literal {
                value: Literal::Integer(0),
                span,
            }),
            span,
        },
        span,
    }
}

fn first_match_predicate(condition: &Expression, target: &VarName) -> Option<Expression> {
    let Expression::Binary {
        op: OpBinary::And,
        lhs,
        rhs,
        ..
    } = condition
    else {
        return None;
    };
    if is_zero_target_guard(lhs, target) && !expression_reads_name(rhs, target) {
        return Some(rhs.as_ref().clone());
    }
    (is_zero_target_guard(rhs, target) && !expression_reads_name(lhs, target))
        .then(|| lhs.as_ref().clone())
}

fn is_zero_target_guard(expression: &Expression, target: &VarName) -> bool {
    let Expression::Binary {
        op: OpBinary::Eq,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return false;
    };
    (is_unsubscripted_name(lhs, target) && is_integer_zero(rhs))
        || (is_unsubscripted_name(rhs, target) && is_integer_zero(lhs))
}

fn is_unsubscripted_name(expression: &Expression, target: &VarName) -> bool {
    matches!(
        expression,
        Expression::VarRef { name, subscripts, .. }
            if name.var_name() == target && subscripts.is_empty()
    )
}

fn is_integer_zero(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::Literal {
            value: Literal::Integer(0),
            ..
        }
    )
}
