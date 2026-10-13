//! Definedness of function loops: compact folds resolved from one generic
//! iteration, and loops selected by one invariant condition.

use super::*;

/// Issued only after the same loop has resolved on its invariant selected path.
pub(in crate::construction) struct SelectedLoopEntry {
    span: Span,
    condition: Expression,
}

pub(in crate::construction) fn selected_loop_entry(
    entry: &SelectedLoopEntry,
    span: Span,
) -> &Expression {
    assert_eq!(
        entry.span, span,
        "the selected entry belongs to this source loop"
    );
    &entry.condition
}

/// Define the targets of a total array-definition loop, proving each read of
/// its statements against the definitions that precede it.
fn define_total_array_loop(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) {
    for (statement, plan) in statements.iter().zip(plans.iter_mut()) {
        if let (
            rumoca_core::Statement::Assignment { value, span, .. },
            FunctionStatementPlan::Assignment(assignment),
        ) = (statement, plan)
        {
            assignment.proven_reads = definitions.proven_assignment_reads(value, context, *span);
            definitions.define_whole(&assignment.target);
        }
    }
}

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
    if domain.scalar_count().is_ok_and(|count| count == 0) {
        return Ok(());
    }
    let binders = indices
        .iter()
        .map(|index| VarName::new(&index.ident))
        .collect::<Vec<_>>();
    definitions.enter_loop_facts(statements, &binders, context);
    match lowering {
        FunctionLoopLowering::TotalArrayDefinition => {
            let (_, flat_body) = flattened_function_loop_source(indices, statements, source_depth);
            define_total_array_loop(flat_body, body, context, definitions);
        }
        FunctionLoopLowering::Fold {
            targets,
            iteration_locals,
            selected_entry,
            entry_reads,
            entry_seeds,
        } => {
            let (flat_indices, flat_body) =
                flattened_function_loop_source(indices, statements, source_depth);
            // A statically decided condition is selected inside the fold.
            // The source invariant-selection check excludes this child's own
            // binders and writes; enclosing binders remain fixed during it.
            let lexical_selection = context
                .snapshot_loops
                .iter()
                .find_map(|entry| entry.condition_for(statements));
            let selection = lexical_selection
                .or_else(|| {
                    invariant_selection(&flat_indices, flat_body, context.generated_booleans)
                })
                .filter(|condition| {
                    !matches!(static_boolean_expression(condition, context), Ok(Some(_)))
                });
            let Some(condition) = selection else {
                return resolve_fold_definitions(
                    (indices, statements, span),
                    (
                        domain,
                        source_depth,
                        body,
                        targets,
                        iteration_locals,
                        entry_reads,
                        entry_seeds,
                    ),
                    context,
                    definitions,
                );
            };
            // The body runs under one condition no iteration changes: the loop
            // is `if condition then (the loop) end if` (SPEC_0022 ALG-019 moved
            // that guard into the body), so each side is resolved on its path.
            let prior =
                is_immutable_guard(condition, context).then(|| definitions.guarded_proofs(targets));
            let writes_before = definitions.folds.write_count();
            let mut taken = definitions.clone();
            taken.enter_guard(condition, context);
            taken.enter_path(&[condition], 0, context);
            // The enclosing iteration reaches the loop only at the binder values
            // the guard selects (a dependent range's membership test).
            taken.folds.assume(condition, true, context);
            let captured = captured_entry(condition, context, span);
            let capture_reads = if captured.is_some() {
                readonly_selected_entry_reads(flat_body, body, context, &taken, span)
            } else {
                Vec::new()
            };
            resolve_fold_definitions(
                (indices, statements, span),
                (
                    domain,
                    source_depth,
                    body,
                    targets,
                    iteration_locals,
                    entry_reads,
                    entry_seeds,
                ),
                context,
                &mut taken,
            )?;
            let taken_folds = taken.folds.clone();
            join_selected_loop(
                (condition, prior),
                taken,
                targets,
                context,
                definitions,
                span,
            )?;
            definitions
                .folds
                .absorb_branch_writes(writes_before, &[&taken_folds], false);
            entry_reads.extend(capture_reads);
            *selected_entry = captured.map(Box::new);
        }
    }
    Ok(())
}

fn readonly_selected_entry_reads(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &FunctionDefinitions,
    span: Span,
) -> Vec<(VarName, Span)> {
    let written = function_ranges::assigned_function_targets(statements);
    let sequence = loop_compaction::StatementSequence {
        statements,
        plans: Some(plans),
    };
    let unwritten = context
        .function
        .outputs
        .iter()
        .chain(&context.function.locals)
        .filter(|value| !written.contains(&value.name))
        .map(|value| VarName::new(&value.name))
        .collect::<Vec<_>>();
    let reads = loop_compaction::planned_sequences_read_incoming_names(
        &[sequence],
        &unwritten.iter().collect::<Vec<_>>(),
        context,
    );
    let captures = unwritten
        .into_iter()
        .zip(reads)
        .filter_map(|(name, read)| read.then_some(name))
        .collect::<Vec<_>>();
    definitions.proven_carried_entry_reads(&captures, context, span)
}

fn captured_entry(
    condition: &Expression,
    context: FunctionValidationContext<'_>,
    span: Span,
) -> Option<SelectedLoopEntry> {
    let mut reads = Vec::new();
    condition.collect_var_refs(&mut reads);
    let captured = !reads.is_empty()
        && reads.iter().all(|read| {
            context
                .generated_booleans
                .iter()
                .any(|capture| !capture.mutable && &capture.target == read)
        })
        && !condition.contains_subexpression(|expression| {
            matches!(expression, Expression::FunctionCall { .. })
        });
    captured.then(|| SelectedLoopEntry {
        span,
        condition: condition.clone(),
    })
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
    // Where the facts already select `condition`, as inside an enclosing
    // loop selected by it, the path that skips the loop is not reachable and
    // the loop defines what it defines on every path.
    let skipped = definitions.reachable_paths(&[condition], context)[1];
    definitions.enter_path(&[condition], 1, context);
    definitions.join_branches(
        std::slice::from_ref(&taken),
        (!skipped, &[]),
        targets,
        &PathPartialAdmission::default(),
        context,
        span,
    )?;
    if let Some(prior) = prior {
        definitions.remember_guarded_branch((condition, prior), &taken, targets, context, span);
    }
    Ok(())
}

type FoldDefinitionPlan<'plan> = (
    &'plan StructuredIndexDomain,
    usize,
    &'plan mut [FunctionStatementPlan],
    &'plan mut Vec<VarName>,
    &'plan mut Vec<VarName>,
    &'plan mut Vec<(VarName, Span)>,
    &'plan mut Vec<(VarName, FunctionValueSeed)>,
);

/// Resolve a compact fold's definedness from one generic iteration (see
/// `fold_scopes`): a learning round finds what every iteration certainly
/// defines by its end, and a judging round checks every read against it.
fn resolve_fold_definitions(
    source: (&[rumoca_core::ForIndex], &[rumoca_core::Statement], Span),
    planned: FoldDefinitionPlan<'_>,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let (indices, statements, span) = source;
    let (domain, source_depth, plans, targets, iteration_locals, entry_reads, entry_seeds) =
        planned;
    let enclosing_definitions = definitions.clone();
    let (indices, statements) = flattened_function_loop_source(indices, statements, source_depth);
    let seeds = seed_guarded_sequence_scratch(statements, plans, context, definitions)?;
    *entry_reads = definitions.proven_carried_entry_reads(targets, context, span);
    let joined_at_entry = targets
        .iter()
        .filter(|target| definitions.has_join_value(target))
        .cloned()
        .collect::<Vec<_>>();
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
    // A value the entry state holds on some paths only, which this loop alone
    // then defines in full, never has its prior elements read: its initial
    // value is dead, so the fold starts from a generated seed.
    for target in joined_at_entry {
        if definitions.is_total(&target) {
            entry_seeds.push((
                target.clone(),
                definitions.whole_loop_seed(&target, context, span)?,
            ));
        }
    }
    confine_guarded_seeds(statements, seeds, context, definitions);
    definitions.forget_varying_guard_paths(context.generated_booleans, iteration_locals);
    targets.retain(|target| {
        let carried = definitions.is_defined(target)
            || definitions.has_join_value(target)
            || definitions.has_total_guarded_definition(target)
            || definitions.has_seeded_slot(target);
        if !carried
            && context
                .function
                .locals
                .iter()
                .any(|local| VarName::new(&local.name) == *target)
            && !iteration_locals.contains(target)
        {
            // Definedness was checked on every source path above. A local
            // absent from the exit value still owns its branch-only writes.
            iteration_locals.push(target.clone());
        }
        carried
    });
    entry_reads.retain(|(name, _)| targets.contains(name) && !iteration_locals.contains(name));
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
                condition_reads,
            },
        ) => {
            let point_targets = resolve_function_conditional(
                cond_blocks,
                else_block.as_deref(),
                FunctionConditionalPlans {
                    branches,
                    fallback: fallback.as_mut(),
                    condition_reads,
                },
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
            FunctionStatementPlan::MultiOutputCall {
                outputs,
                argument_reads,
            },
        ) => {
            resolve_fold_multi_output(args, *span, (outputs, argument_reads), context, definitions)?
        }
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
    let mut proven_reads = definitions.proven_assignment_reads(value, context, span);
    for expression in assignment
        .subscripts()
        .iter()
        .filter_map(subscript_expression)
    {
        definitions.require_readable(expression, context, span)?;
        proven_reads.extend(definitions.proven_assignment_reads(expression, context, span));
    }
    assignment.proven_reads = proven_reads;
    if assignment.subscripts().is_empty() {
        seed_undefined_whole_loop_value(assignment, context, definitions, span)?;
        definitions.define_whole(assignment.target());
        return Ok(());
    }
    assignment.base_reads = definitions.proven_carried_entry_reads(
        std::slice::from_ref(assignment.target()),
        context,
        span,
    );
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
    planned: (
        &mut [Option<FunctionAssignmentPlan>],
        &mut Vec<(VarName, Span)>,
    ),
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let (outputs, argument_reads) = planned;
    for output in outputs
        .iter_mut()
        .flatten()
        .filter(|output| output.is_whole())
    {
        seed_undefined_whole_loop_value(output, context, definitions, span)?;
    }
    resolve_multi_output_definitions(
        arguments,
        span,
        (outputs, argument_reads),
        context,
        definitions,
    )
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

/// The condition of a loop body whose statements are each selected by the
/// same condition, which reads nothing the body writes and no binder of the
/// loop or of a loop in it, so every iteration selects the same way. A
/// statement is selected by a conditional without an else, or is a loop whose
/// body statements all are: the guarded statements the branch snapshot pass
/// makes of a selected sequence that owns the loop.
fn invariant_selection<'a>(
    binders: &[&rumoca_core::ForIndex],
    body: &'a [rumoca_core::Statement],
    generated: &[function_returns::GeneratedBooleanDefinition],
) -> Option<&'a Expression> {
    let mut bound = binders
        .iter()
        .map(|binder| binder.ident.clone())
        .collect::<Vec<_>>();
    let condition = sequence_selection(body, &mut bound)?;
    let written = function_ranges::assigned_function_targets_with_generated(body, generated);
    let mut reads = Vec::new();
    condition.collect_var_refs(&mut reads);
    reads
        .iter()
        .all(|name| {
            !written.contains(name.as_str()) && bound.iter().all(|binder| binder != name.as_str())
        })
        .then_some(condition)
}

/// The one condition that selects every statement of `statements`, recording
/// the binders of the loops among them in `bound`.
fn sequence_selection<'a>(
    statements: &'a [rumoca_core::Statement],
    bound: &mut Vec<String>,
) -> Option<&'a Expression> {
    let mut selected = None::<&'a Expression>;
    for statement in statements {
        let condition = match statement {
            rumoca_core::Statement::If {
                cond_blocks,
                else_block: None,
                ..
            } => match cond_blocks.as_slice() {
                [block] => &block.cond,
                _ => return None,
            },
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                bound.extend(indices.iter().map(|index| index.ident.clone()));
                sequence_selection(equations, bound)?
            }
            _ => return None,
        };
        match selected {
            Some(first) => {
                rumoca_core::expressions_semantically_equal(first, condition).then_some(())?
            }
            None => selected = Some(condition),
        }
    }
    selected
}

#[cfg(test)]
mod selected_entry_tests {
    use super::*;
    use crate::construction::analysis::loop_compaction::preservation_corpus::{
        assign, branch, for_loop, integer, var,
    };

    fn binder() -> rumoca_core::ForIndex {
        rumoca_core::ForIndex {
            binder: rumoca_core::IndexBinderProvenance::Generated,
            ident: "axis".into(),
            range: integer(2),
        }
    }

    #[test]
    fn selected_child_entry_keeps_an_enclosing_capture_but_not_its_own_binder() {
        let axis = binder();
        let body = vec![branch(
            vec![(var("captured_outer"), vec![assign("scratch", integer(1))])],
            None,
        )];
        assert!(invariant_selection(&[&axis], &body, &[]).is_some());
        let dependent = vec![branch(
            vec![(var("axis"), vec![assign("scratch", integer(1))])],
            None,
        )];
        assert!(invariant_selection(&[&axis], &dependent, &[]).is_none());
    }

    #[test]
    fn selected_child_entry_excludes_nested_binders_and_changed_selections() {
        let axis = binder();
        let nested = vec![for_loop(
            "column",
            2,
            vec![branch(
                vec![(var("column"), vec![assign("scratch", integer(1))])],
                None,
            )],
        )];
        assert!(invariant_selection(&[&axis], &nested, &[]).is_none());
        let changed = vec![branch(
            vec![(var("chosen"), vec![assign("chosen", integer(1))])],
            None,
        )];
        assert!(invariant_selection(&[&axis], &changed, &[]).is_none());
    }
}
