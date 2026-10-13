//! Private local decisions issued and consumed over the same source/plan tree.

use super::*;
use loop_compaction::StatementSequence;

/// The statements around one source position: those that run before it in the
/// enclosing sequences (outermost first), those after it, and the loop bodies
/// whose back edges reach it (both innermost first).
#[derive(Clone, Default)]
struct Flow<'a> {
    prefix: Vec<StatementSequence<'a>>,
    suffix: Vec<StatementSequence<'a>>,
    back_edges: Vec<StatementSequence<'a>>,
}

struct Decision {
    position: Vec<usize>,
    span: Option<Span>,
    source_depth: usize,
    targets: Option<Vec<VarName>>,
    locals: Vec<VarName>,
}

pub(super) fn annotate(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    let mut decisions = Vec::new();
    collect(
        StatementSequence {
            statements,
            plans: Some(plans),
        },
        context,
        &Flow::default(),
        &[],
        &mut decisions,
    )?;
    let mut decisions = decisions.into_iter();
    apply(
        statements,
        plans,
        &[],
        &mut decisions,
        context.function.span,
    )?;
    if decisions.next().is_some() {
        return Err(plan_error(
            "an iteration-local decision has no source loop",
            context.function.span,
        ));
    }
    Ok(())
}

fn collect<'a>(
    sequence: StatementSequence<'a>,
    context: FunctionValidationContext<'_>,
    enclosing: &Flow<'a>,
    position: &[usize],
    decisions: &mut Vec<Decision>,
) -> Result<(), ToDaeError> {
    let plans = sequence.plans.ok_or_else(|| {
        plan_error(
            "the statement sequence has no iteration-local plan",
            context.function.span,
        )
    })?;
    check_sequence_length(
        sequence.statements.len(),
        plans.len(),
        context.function.span,
    )?;
    for (index, (statement, plan)) in sequence.statements.iter().zip(plans).enumerate() {
        let mut path = position.to_vec();
        path.push(index);
        let mut flow = enclosing.clone();
        flow.prefix.push(StatementSequence {
            statements: &sequence.statements[..index],
            plans: Some(&plans[..index]),
        });
        flow.suffix.insert(
            0,
            StatementSequence {
                statements: &sequence.statements[index + 1..],
                plans: Some(&plans[index + 1..]),
            },
        );
        if !collect_loop((statement, plan), context, &flow, &path, decisions)? {
            collect_conditional((statement, plan), context, &flow, &path, decisions)?;
        }
    }
    Ok(())
}

fn collect_loop<'a>(
    (statement, plan): (&rumoca_core::Statement, &FunctionStatementPlan),
    context: FunctionValidationContext<'_>,
    flow: &Flow<'a>,
    position: &[usize],
    decisions: &mut Vec<Decision>,
) -> Result<bool, ToDaeError> {
    let (
        rumoca_core::Statement::For {
            indices, equations, ..
        },
        FunctionStatementPlan::For {
            lowering,
            statements: body_plans,
            source_depth,
            ..
        },
    ) = (statement, plan)
    else {
        return Ok(false);
    };
    let (_, body) = flattened_function_loop_source(indices, equations, *source_depth);
    check_sequence_length(
        body.len(),
        body_plans.len(),
        statement.source_span().unwrap_or(context.function.span),
    )?;
    let locals = context
        .function
        .locals
        .iter()
        .map(|local| VarName::new(&local.name))
        .collect::<HashSet<_>>();
    let generated = context
        .generated_booleans
        .iter()
        .filter(|guard| !guard.mutable)
        .map(|guard| guard.target.clone())
        .collect::<HashSet<_>>();
    decisions.push(Decision {
        position: position.to_vec(),
        span: statement.source_span(),
        source_depth: *source_depth,
        targets: match lowering {
            FunctionLoopLowering::Fold { targets, .. } => Some(targets.clone()),
            _ => None,
        },
        locals: iteration_local_targets(
            lowering,
            (body, body_plans),
            (&locals, &generated),
            (&flow.prefix, &flow.suffix, &flow.back_edges),
            context,
        ),
    });
    let child = StatementSequence {
        statements: body,
        plans: Some(body_plans),
    };
    let mut inner = flow.clone();
    inner.back_edges.insert(0, child);
    let mut path = position.to_vec();
    path.push(0);
    collect(child, context, &inner, &path, decisions)?;
    Ok(true)
}

fn collect_conditional<'a>(
    (statement, plan): (&rumoca_core::Statement, &FunctionStatementPlan),
    context: FunctionValidationContext<'_>,
    edges: &Flow<'a>,
    position: &[usize],
    decisions: &mut Vec<Decision>,
) -> Result<(), ToDaeError> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        ..
    } = statement
    else {
        return Ok(());
    };
    let span = statement.source_span().unwrap_or(context.function.span);
    match plan {
        FunctionStatementPlan::If {
            branches, fallback, ..
        } => {
            check_conditional_shape(
                cond_blocks.len(),
                branches.len(),
                else_block.is_some(),
                fallback.is_some(),
                span,
            )?;
            for (branch, (block, plans)) in cond_blocks.iter().zip(branches).enumerate() {
                let mut path = position.to_vec();
                path.push(branch);
                collect(
                    StatementSequence {
                        statements: &block.stmts,
                        plans: Some(plans),
                    },
                    context,
                    edges,
                    &path,
                    decisions,
                )?;
            }

            if let (Some(source), Some(plans)) = (else_block, fallback) {
                let mut path = position.to_vec();
                path.push(usize::MAX);
                collect(
                    StatementSequence {
                        statements: source,
                        plans: Some(plans),
                    },
                    context,
                    edges,
                    &path,
                    decisions,
                )?;
            }
        }
        FunctionStatementPlan::ProvenBranch {
            selected,
            statements,
        } => {
            let source = proven_branch_source(statement, *selected, span)?;
            let mut path = position.to_vec();
            path.push(selected.unwrap_or(usize::MAX));
            collect(
                StatementSequence {
                    statements: source,
                    plans: Some(statements),
                },
                context,
                edges,
                &path,
                decisions,
            )?;
        }
        _ => {}
    }
    Ok(())
}

fn apply(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    position: &[usize],
    decisions: &mut impl Iterator<Item = Decision>,
    function_span: Span,
) -> Result<(), ToDaeError> {
    check_sequence_length(statements.len(), plans.len(), function_span)?;
    for (index, (statement, plan)) in statements.iter().zip(plans).enumerate() {
        let mut path = position.to_vec();
        path.push(index);
        if !apply_loop(statement, plan, &path, decisions, function_span)? {
            apply_conditional(statement, plan, &path, decisions, function_span)?;
        }
    }
    Ok(())
}

fn apply_loop(
    statement: &rumoca_core::Statement,
    plan: &mut FunctionStatementPlan,
    position: &[usize],
    decisions: &mut impl Iterator<Item = Decision>,
    function_span: Span,
) -> Result<bool, ToDaeError> {
    let (
        rumoca_core::Statement::For {
            indices, equations, ..
        },
        FunctionStatementPlan::For {
            lowering,
            statements: body_plans,
            source_depth,
            ..
        },
    ) = (statement, plan)
    else {
        return Ok(false);
    };
    let span = statement.source_span().unwrap_or(function_span);
    let decision = decisions
        .next()
        .ok_or_else(|| plan_error("the source loop has no iteration-local decision", span))?;
    if decision.position != position
        || decision.span != statement.source_span()
        || decision.source_depth != *source_depth
    {
        return Err(plan_error(
            "the iteration-local decision belongs to another source loop",
            span,
        ));
    }
    match (lowering, decision.targets) {
        (
            FunctionLoopLowering::Fold {
                targets,
                iteration_locals,
                ..
            },
            Some(original),
        ) => {
            if *targets != original {
                return Err(plan_error(
                    "the iteration-local decision has different carried targets",
                    span,
                ));
            }
            for target in std::mem::take(targets) {
                if decision.locals.contains(&target) {
                    iteration_locals.push(target);
                } else {
                    targets.push(target);
                }
            }
        }
        (FunctionLoopLowering::TotalArrayDefinition, None) if decision.locals.is_empty() => {}
        _ => {
            return Err(plan_error(
                "the loop lowering and iteration-local decision differ",
                span,
            ));
        }
    }
    let (_, body) = flattened_function_loop_source(indices, equations, *source_depth);
    let mut path = position.to_vec();
    path.push(0);
    apply(body, body_plans, &path, decisions, function_span)?;
    Ok(true)
}

fn apply_conditional(
    statement: &rumoca_core::Statement,
    plan: &mut FunctionStatementPlan,
    position: &[usize],
    decisions: &mut impl Iterator<Item = Decision>,
    function_span: Span,
) -> Result<(), ToDaeError> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        ..
    } = statement
    else {
        return Ok(());
    };
    let span = statement.source_span().unwrap_or(function_span);
    match plan {
        FunctionStatementPlan::If {
            branches, fallback, ..
        } => {
            check_conditional_shape(
                cond_blocks.len(),
                branches.len(),
                else_block.is_some(),
                fallback.is_some(),
                span,
            )?;
            for (branch, (block, plans)) in cond_blocks.iter().zip(branches).enumerate() {
                let mut path = position.to_vec();
                path.push(branch);
                apply(&block.stmts, plans, &path, decisions, function_span)?;
            }

            if let (Some(source), Some(plans)) = (else_block, fallback) {
                let mut path = position.to_vec();
                path.push(usize::MAX);
                apply(source, plans, &path, decisions, function_span)?;
            }
        }
        FunctionStatementPlan::ProvenBranch {
            selected,
            statements,
        } => {
            let source = proven_branch_source(statement, *selected, span)?;
            let mut path = position.to_vec();
            path.push(selected.unwrap_or(usize::MAX));
            apply(source, statements, &path, decisions, function_span)?;
        }
        _ => {}
    }
    Ok(())
}

fn check_sequence_length(statements: usize, plans: usize, span: Span) -> Result<(), ToDaeError> {
    if statements != plans {
        return Err(plan_error(
            "the statement sequence and iteration-local plan differ",
            span,
        ));
    }
    Ok(())
}

fn check_conditional_shape(
    source_branches: usize,
    plan_branches: usize,
    source_fallback: bool,
    plan_fallback: bool,
    span: Span,
) -> Result<(), ToDaeError> {
    check_sequence_length(source_branches, plan_branches, span)?;
    if source_fallback != plan_fallback {
        return Err(plan_error(
            "the conditional fallback and iteration-local plan differ",
            span,
        ));
    }
    Ok(())
}

fn proven_branch_source(
    statement: &rumoca_core::Statement,
    selected: Option<usize>,
    span: Span,
) -> Result<&[rumoca_core::Statement], ToDaeError> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        ..
    } = statement
    else {
        return Err(plan_error(
            "the proven branch has no source conditional",
            span,
        ));
    };
    match selected {
        Some(index) => cond_blocks
            .get(index)
            .map(|block| block.stmts.as_slice())
            .ok_or_else(|| plan_error("the proven branch is outside its source conditional", span)),
        None => Ok(else_block.as_deref().unwrap_or(&[])),
    }
}

fn plan_error(reason: &'static str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat("function iteration-local plan", reason, span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::construction::function_shapes::ShapeEnvironment;

    /// A plan sequence that does not mirror its source sequence is an
    /// inconsistency of the planner, reported at the function's span.
    #[test]
    fn a_plan_sequence_shorter_than_its_source_is_reported_at_the_function() {
        let mut sources = SourceMap::new();
        let source = sources.add("plan.mo", "function f end f;");
        let span = Span::from_offsets(source, 0, 17);
        let function = rumoca_core::Function::new("f", span);
        let flat = flat::Model::new();
        let shape_analysis = FunctionShapeAnalysis::analyze(&flat, &EvalContext::new())
            .expect("an empty model has an exact shape analysis");
        let (roles, record_roots, static_integers) =
            (HashMap::new(), HashMap::new(), HashMap::new());
        let (staged_record_fields, loop_binders) = (HashSet::new(), HashSet::new());
        let shapes = ShapeEnvironment::default();
        let scalars = super::super::super::guard_facts::ValueKinds::default();
        let context = FunctionValidationContext {
            function: &function,
            flat: &flat,
            roles: &roles,
            record_roots: &record_roots,
            static_integers: &static_integers,
            comprehension_values: None,
            shapes: &shapes,
            shape_analysis: &shape_analysis,
            generated_booleans: &[],
            snapshot_loops: &[],
            staged_record_fields: &staged_record_fields,
            loop_binders: &loop_binders,
            scalars: &scalars,
            call_scoped_actions: false,
            inside_fold: false,
        };
        let statements = [rumoca_core::Statement::Empty { span }];
        let error = annotate(&statements, &mut [], context)
            .expect_err("one source statement has no matching plan");
        let ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span: reported,
        } = error
        else {
            panic!("a plan inconsistency is an unsupported semantic owner");
        };
        assert_eq!(feature, "function iteration-local plan");
        assert_eq!(
            detail,
            "the statement sequence and iteration-local plan differ"
        );
        assert_eq!(reported, span);
    }
}
