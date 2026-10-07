//! Definedness of function loops: compact folds resolved from one generic
//! iteration, and loops selected by one invariant condition.

use super::*;

/// A loop transition may only carry values whose elements already have a
/// definition, because MLS §12.4.4 gives the carried value no other owner.
pub(super) fn resolve_function_loop_definitions(
    source: (&[rumoca_core::ForIndex], &[rumoca_core::Statement], Span),
    planned: (
        &StructuredIndexDomain,
        usize,
        &mut FunctionLoopLowering,
        &mut [FunctionStatementPlan],
    ),
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let (indices, statements, span) = source;
    let (domain, source_depth, lowering, body) = planned;
    let binders = indices
        .iter()
        .map(|index| VarName::new(&index.ident))
        .collect::<Vec<_>>();
    definitions.enter_loop_facts(statements, &binders);
    match lowering {
        FunctionLoopLowering::TotalArrayDefinition => {
            for plan in body {
                if let FunctionStatementPlan::Assignment(assignment) = plan {
                    definitions.define_whole(&assignment.target);
                }
            }
        }
        FunctionLoopLowering::Fold {
            targets,
            iteration_locals,
        } => {
            let (flat_indices, flat_body) =
                flattened_function_loop_source(indices, statements, source_depth);
            // A statically decided condition is selected inside the fold, and a
            // condition over an enclosing binder is a binder-range selection the
            // generic iteration narrows exactly.
            let selection = invariant_selection(&flat_indices, flat_body).filter(|condition| {
                !matches!(static_boolean_expression(condition, context), Ok(Some(_)))
                    && !reads_enclosing_binder(condition, context, 0)
            });
            let Some(condition) = selection else {
                return resolve_fold_definitions(
                    (indices, statements, span),
                    (domain, source_depth, body, targets, iteration_locals),
                    context,
                    definitions,
                );
            };
            // The body runs under one condition no iteration changes: the loop
            // is `if condition then (the loop) end if` (SPEC_0022 ALG-019 moved
            // that guard into the body), so each side is resolved on its path.
            let prior =
                is_immutable_guard(condition, context).then(|| definitions.guarded_proofs(targets));
            let mut taken = definitions.clone();
            taken.enter_guard(condition, context);
            taken.enter_path(&[condition], 0, context);
            resolve_fold_definitions(
                (indices, statements, span),
                (domain, source_depth, body, targets, iteration_locals),
                context,
                &mut taken,
            )?;
            join_selected_loop(
                (condition, prior),
                taken,
                targets,
                context,
                definitions,
                span,
            )?;
        }
    }
    Ok(())
}

/// Join a loop resolved on the path its invariant `condition` selects with
/// the path that skips it, as the conditional `if condition then (the loop)
/// end if` joins its branch.
fn join_selected_loop(
    (condition, prior): (&Expression, Option<function_definitions::GuardedProofs>),
    mut taken: FunctionDefinitions,
    targets: &[VarName],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
    span: Span,
) -> Result<(), ToDaeError> {
    // The whole loop runs under `condition`, so a value it proved under a
    // guard that `condition` implies is defined on this path.
    taken.enter_guard(condition, context);
    definitions.enter_path(&[condition], 1, context);
    definitions.join_branches(
        std::slice::from_ref(&taken),
        false,
        targets,
        &HashSet::new(),
        context,
        span,
    )?;
    if let Some(prior) = prior {
        definitions.remember_guarded_branch((condition, prior), &taken, targets, context, span);
    }
    Ok(())
}

/// Resolve a compact fold's definedness from one generic iteration (see
/// `fold_scopes`): a learning round finds what every iteration certainly
/// defines by its end, and a judging round checks every read against it.
fn resolve_fold_definitions(
    source: (&[rumoca_core::ForIndex], &[rumoca_core::Statement], Span),
    planned: (
        &StructuredIndexDomain,
        usize,
        &mut [FunctionStatementPlan],
        &mut Vec<VarName>,
        &mut Vec<VarName>,
    ),
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let (indices, statements, span) = source;
    let (domain, source_depth, plans, targets, iteration_locals) = planned;
    let enclosing_definitions = definitions.clone();
    let (indices, statements) = flattened_function_loop_source(indices, statements, source_depth);
    let seeds = seed_guarded_sequence_scratch(statements, plans, context, definitions)?;
    let invalid = |problem: String| {
        ToDaeError::unsupported_flat(
            "function loop transition",
            format!(
                "`{}` has an invalid compact domain: {problem}",
                context.function.name
            ),
            span,
        )
    };
    let point_count = domain
        .scalar_count()
        .map_err(|error| invalid(error.to_string()))?;
    if point_count > 0 {
        let binders = domain
            .binders
            .iter()
            .zip(&indices)
            .map(|(binder, index)| {
                Progression::range(binder.lower, binder.step, binder.upper)
                    .map(|values| (VarName::new(&index.ident), values, binder.step < 0))
            })
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| invalid("a binder range overflows".to_string()))?;
        // MLS §11.2.2: a binder shadows every outer value of its name and is
        // fixed within one iteration.
        let mut integers = context.static_integers.clone();
        let mut loop_binders = context.loop_binders.clone();
        for (name, _, _) in &binders {
            integers.remove(name);
            loop_binders.insert(name.clone());
        }
        let body_context = FunctionValidationContext {
            static_integers: &integers,
            loop_binders: &loop_binders,
            ..context
        };
        definitions.folds.enter(binders);
        let mut learning = definitions.clone();
        let summary = resolve_fold_round(
            statements,
            plans,
            body_context,
            (&mut learning, None),
            iteration_locals,
        )?;
        resolve_fold_round(
            statements,
            plans,
            body_context,
            (definitions, Some(summary)),
            iteration_locals,
        )?;
        for (target, written) in definitions.folds.leave() {
            definitions.add_loop_coverage(&target, written);
        }
    }
    confine_guarded_seeds(statements, seeds, context, definitions);
    definitions.forget_varying_guard_paths(context.generated_booleans, iteration_locals);
    targets.retain(|target| {
        definitions.is_defined(target)
            || definitions.has_total_guarded_definition(target)
            || definitions.has_seeded_slot(target)
    });
    definitions.restore_names(&enclosing_definitions, iteration_locals);
    Ok(())
}

/// One round of a generic fold iteration; returns what it certainly defines
/// by its end.
fn resolve_fold_round(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    (definitions, previous): (&mut FunctionDefinitions, Option<Rc<IterationSummary>>),
    iteration_locals: &[VarName],
) -> Result<Rc<IterationSummary>, ToDaeError> {
    definitions.folds.begin_round(previous);
    definitions.clear_names(iteration_locals);
    resolve_fold_iteration(statements, plans, context, definitions)?;
    definitions.forget_varying_guard_paths(context.generated_booleans, iteration_locals);
    let defined = definitions.defined_names();
    Ok(definitions.folds.end_round(defined, iteration_locals))
}

fn resolve_fold_iteration(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let mut index = 0usize;
    while index < statements.len() {
        if let FunctionStatementPlan::RecordAssembly(assembly) = &mut plans[index] {
            let count = assembly.statement_count;
            resolve_fold_record_assembly(
                &statements[index..index + count],
                assembly,
                context,
                definitions,
            )?;
            definitions.advance_facts_over(&statements[index..index + count], context);
            index += count;
            continue;
        }
        resolve_fold_statement(&statements[index], &mut plans[index], context, definitions)?;
        definitions.advance_facts_over(std::slice::from_ref(&statements[index]), context);
        index += 1;
    }
    Ok(())
}

/// Resolve one planned statement of a generic fold iteration.
fn resolve_fold_statement(
    statement: &rumoca_core::Statement,
    plan: &mut FunctionStatementPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    match (statement, plan) {
        (statement, FunctionStatementPlan::ProvenAssertion) => {
            resolve_function_assertion_definition(statement, false, context, definitions)?;
        }
        (statement, FunctionStatementPlan::RuntimeAssertion) => {
            resolve_function_assertion_definition(statement, true, context, definitions)?;
        }
        (
            rumoca_core::Statement::Assignment { value, span, .. },
            FunctionStatementPlan::Assignment(assignment),
        ) => resolve_fold_assignment(value, *span, assignment, context, definitions)?,
        (statement, plan @ FunctionStatementPlan::GeneratedBooleanAssignment { .. }) => {
            resolve_function_definition(statement, plan, context, definitions)?;
        }
        (
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            },
            FunctionStatementPlan::If {
                branches,
                fallback,
                targets,
            },
        ) => {
            let point_targets = resolve_function_conditional(
                cond_blocks,
                else_block.as_deref(),
                branches,
                fallback.as_mut(),
                *span,
                context,
                definitions,
            )?;
            union_targets(targets, point_targets);
        }
        (
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            },
            FunctionStatementPlan::ProvenBranch {
                selected,
                statements,
            },
        ) => {
            let selected =
                selected_conditional_statements(cond_blocks, else_block.as_deref(), *selected);
            resolve_fold_iteration(selected, statements, context, definitions)?;
        }
        (
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            },
            FunctionStatementPlan::For {
                domain,
                lowering,
                statements,
                source_depth,
                ..
            },
        ) => resolve_function_loop_definitions(
            (indices, equations, *span),
            (domain, *source_depth, lowering, statements),
            context,
            definitions,
        )?,
        (
            rumoca_core::Statement::FunctionCall { args, span, .. },
            FunctionStatementPlan::MultiOutputCall { outputs },
        ) => resolve_fold_multi_output(args, *span, outputs, context, definitions)?,
        (statement, _) => {
            return Err(unsupported_statement(
                statement,
                "function loop transition",
                "function loop transition",
                format!(
                    "`{}` has a fold statement that its checked plan does not cover",
                    context.function.name
                ),
            ));
        }
    }
    Ok(())
}

fn resolve_fold_record_assembly(
    statements: &[rumoca_core::Statement],
    assembly: &mut FunctionRecordAssemblyPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for statement in statements {
        let rumoca_core::Statement::Assignment { value, span, .. } = statement else {
            return Err(unsupported_statement(
                statement,
                "function loop transition",
                "function loop transition",
                format!(
                    "`{}` has a fold statement that its checked plan does not cover",
                    context.function.name
                ),
            ));
        };
        definitions.require_readable(value, context, *span)?;
    }
    if !definitions.is_defined(&assembly.target)
        && !definitions.has_join_value(&assembly.target)
        && assembly.seed.is_none()
    {
        let span = required_statement_span(&statements[0], "function loop record assembly")?;
        assembly.seed = Some(definitions.whole_loop_seed(&assembly.target, context, span)?);
    }
    definitions.define_whole(&assembly.target);
    Ok(())
}

fn union_targets(targets: &mut Vec<VarName>, candidates: Vec<VarName>) {
    for target in candidates {
        if !targets.contains(&target) {
            targets.push(target);
        }
    }
}

fn resolve_fold_assignment(
    value: &Expression,
    span: Span,
    assignment: &mut FunctionAssignmentPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    definitions.require_readable(value, context, span)?;
    for expression in assignment
        .subscripts()
        .iter()
        .filter_map(subscript_expression)
    {
        definitions.require_readable(expression, context, span)?;
    }
    if assignment.subscripts().is_empty() {
        seed_undefined_whole_loop_value(assignment, context, definitions, span)?;
        definitions.define_whole(assignment.target());
        return Ok(());
    }
    let seed =
        definitions.write_elements(assignment.target(), assignment.subscripts(), context, span)?;
    assignment.seed = assignment.seed.take().or(seed);
    Ok(())
}

/// A multiple-output call in a fold: a whole output the loop defines before
/// any value exists is carried from a dead seed, as a whole assignment is.
fn resolve_fold_multi_output(
    arguments: &[Expression],
    span: Span,
    outputs: &mut [Option<FunctionAssignmentPlan>],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for output in outputs
        .iter_mut()
        .flatten()
        .filter(|output| output.is_whole())
    {
        seed_undefined_whole_loop_value(output, context, definitions, span)?;
    }
    resolve_multi_output_definitions(arguments, span, outputs, context, definitions)
}

fn seed_undefined_whole_loop_value(
    assignment: &mut FunctionAssignmentPlan,
    context: FunctionValidationContext<'_>,
    definitions: &FunctionDefinitions,
    span: Span,
) -> Result<(), ToDaeError> {
    if !definitions.is_defined(assignment.target())
        && !definitions.has_join_value(assignment.target())
        && assignment.seed.is_none()
    {
        assignment.seed = Some(definitions.whole_loop_seed(assignment.target(), context, span)?);
    }
    Ok(())
}

/// The condition of a loop body that is one conditional without an else whose
/// condition reads nothing the body writes and no binder of the loop, so every
/// iteration selects the same way.
fn invariant_selection<'a>(
    binders: &[&rumoca_core::ForIndex],
    body: &'a [rumoca_core::Statement],
) -> Option<&'a Expression> {
    let [
        rumoca_core::Statement::If {
            cond_blocks,
            else_block: None,
            ..
        },
    ] = body
    else {
        return None;
    };
    let [block] = cond_blocks.as_slice() else {
        return None;
    };
    let written = function_ranges::assigned_function_targets(body);
    let mut reads = Vec::new();
    block.cond.collect_var_refs(&mut reads);
    reads
        .iter()
        .all(|name| {
            !written.contains(name.as_str())
                && binders.iter().all(|binder| binder.ident != name.as_str())
        })
        .then_some(&block.cond)
}

/// Whether `condition`, or a generated selection it reads, reads a binder of
/// an enclosing loop.
fn reads_enclosing_binder(
    condition: &Expression,
    context: FunctionValidationContext<'_>,
    depth: usize,
) -> bool {
    let mut reads = Vec::new();
    condition.collect_var_refs(&mut reads);
    reads.iter().any(|name| {
        context.loop_binders.contains(name)
            || context
                .generated_booleans
                .iter()
                .find(|definition| &definition.target == name)
                .is_some_and(|definition| {
                    // Generated selections are defined before their readers,
                    // so the expansion is finite; the depth only bounds it.
                    depth >= 16 || reads_enclosing_binder(&definition.value, context, depth + 1)
                })
    })
}
