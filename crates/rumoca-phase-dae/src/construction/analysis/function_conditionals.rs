use super::*;

/// The statements MLS §11.5 executes when `selected` is the proven branch.
///
/// `Some(ordinal)` names the first condition branch whose condition holds and
/// `None` names the else part, which is empty when the conditional declares
/// none — exactly the two outcomes MLS §11.5 defines for a settled condition
/// sequence.
pub(in crate::construction) fn selected_conditional_statements<'statement>(
    blocks: &'statement [rumoca_core::StatementBlock],
    fallback: Option<&'statement [rumoca_core::Statement]>,
    selected: Option<usize>,
) -> &'statement [rumoca_core::Statement] {
    match selected {
        Some(ordinal) => &blocks[ordinal].stmts,
        None => fallback.unwrap_or_default(),
    }
}

/// Prove the checked owner shape of one MLS §11.5 function conditional.
pub(super) fn plan_function_conditional(
    blocks: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    span: Span,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    require_span(span, "function if statement")?;
    if blocks.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function conditional",
            "a function conditional must contain at least one condition branch",
            span,
        ));
    }
    if let Some(selected) = proven_conditional_branch(blocks, context.shapes) {
        return plan_proven_conditional_branch(blocks, fallback, selected, context);
    }
    // A conditional whose arm carries a loop reaches this planner as a generated
    // branch guard rather than its source predicate, which `proven_value` cannot
    // fold. Settling the guard chain recovers the same MLS §11.5 selection, so a
    // specialization that proves the predicate plans the executed arm as the
    // unconditional algorithm section it is instead of a runtime branch that has
    // no owner for the loop.
    if let Some(selected) = statically_selected_branch(blocks, fallback, context)? {
        return plan_proven_conditional_branch(blocks, fallback, selected, context);
    }
    // A runtime branch keeps the enclosing action owner: its assertions lower
    // to that owner guarded by the MLS §11.5 branch selection, so they keep
    // their source order and can fail only on the executed path.
    let branch_context = context;
    let mut branches = Vec::with_capacity(blocks.len());
    for block in blocks {
        validate_function_expression_with_roles(
            &block.cond,
            context.roles,
            context.flat,
            context.shapes,
        )?;
        branches.push(plan_function_statements(&block.stmts, branch_context)?);
    }
    let fallback = fallback
        .map(|statements| plan_function_statements(statements, branch_context))
        .transpose()?;
    Ok(FunctionStatementPlan::If {
        branches,
        fallback,
        targets: Vec::new(),
    })
}

/// Plan the one branch this specialization proves MLS §11.5 executes.
///
/// # Acceptance contract (SPEC_0008 §"Acceptance Contract Before Rejection")
///
/// MLS §11.5 executes the statements of the first branch whose condition
/// evaluates to `true`, and the else part when none does. When the enclosing
/// value-proven specialization settles every condition it has to evaluate, that
/// choice is fixed at translation time: the selected statements are the
/// function's behaviour and the others are unreachable. MLS §12.2's rule that a
/// body is written over the inputs is what makes the choice settled — the
/// specialization exists precisely because those inputs carry proven values.
///
/// **Accepted.** The selected statements, planned as the ordinary MLS §11
/// algorithm section they are: nested loops, nested conditionals, and array
/// assemblies all keep their usual owners, because nothing about them is
/// conditional any more. This is what the "direct value assignments in every
/// checked branch" rule below cannot admit, and it does not have to: that rule
/// exists because a *runtime* branch value has no owner outside the conditional
/// expression that selects it, and a proven branch has no such expression.
///
/// **Typed-rejected.** Nothing new about the executed path. A conditional this
/// scope does not settle keeps the checked-branch rule unchanged, reported at
/// the same statement.
///
/// **Gate.** This rule reaches only a conditional whose conditions the
/// *specialization key* settles, and `ValueReadInputs` puts an input in that key
/// only when a declared dimension, compact range, or `zeros`/`ones`/`fill`
/// extent reads its value. So `f(m)` with `output Real y[m]` folds `if m == 3`
/// and `f(m)` with `output Real y[3]` does not, even though both calls pass a
/// literal. That gate is deliberate and load-bearing in two directions: it is
/// what keeps MLS §4.5 non-structural parameters — values the initialization
/// problem may establish — out of translation-time control flow, and it is what
/// bounds the specialization count so a recursive callee reaches a repeated key.
/// The cost is that an unrelated edit to a declared dimension changes whether a
/// conditional folds, which is a visible property of the language rule and not
/// an accident of this analysis.
///
/// **Not proven, and never guessed.** The statements of the branches MLS §11.5
/// does not execute contribute no value, no call, and no shape to this
/// specialization: no certificate is minted for a callee only they reach, and
/// no extent proof is demanded of them. They are *not* thereby unchecked —
/// `check_unexecuted_branches` still proves every statement well formed under
/// MLS §11.2.1, which is what the base compiler's lowering did before the fold
/// existed.
///
/// **Owner.** `plan_function_conditional`, the only producer of a function
/// conditional's plan.
///
/// **Evidence.** `rumoca/tests/function_proven_branch_test.rs`:
/// `a_proven_false_condition_selects_the_else_arm` and
/// `a_proven_true_condition_selects_a_nested_arm` (accepted, over the MSL
/// `symmetricOrientation` shape, whose unexecuted arm keeps a recursive call
/// and a `fill` extent that nothing proves), and
/// `an_unproven_condition_keeps_the_checked_branch_rule` (rejected),
/// `a_declared_dimension_that_reads_the_input_is_what_enables_the_fold`
/// (the gate, both directions), and
/// `a_dead_arm_shape_error_is_still_rejected`.
fn plan_proven_conditional_branch(
    blocks: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    selected: Option<usize>,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    check_unexecuted_branches(blocks, fallback, selected, context)?;
    let statements = selected_conditional_statements(blocks, fallback, selected);
    Ok(FunctionStatementPlan::ProvenBranch {
        selected,
        statements: plan_function_statements(statements, context)?,
    })
}

/// Prove which values one function conditional defines on all of its paths.
///
/// Each branch is an ordinary MLS §11 algorithm section: its statements run in
/// order and the last write to a value wins, so a branch may assign the same
/// value repeatedly and may read what it already assigned. The branch keeps its
/// own definedness certificate, and the join keeps exactly the values the
/// conditional owns once it finishes.
pub(super) fn resolve_function_conditional(
    blocks: &[rumoca_core::StatementBlock],
    fallback_statements: Option<&[rumoca_core::Statement]>,
    branches: &mut [Vec<FunctionStatementPlan>],
    fallback_plans: Option<&mut Vec<FunctionStatementPlan>>,
    span: Span,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<Vec<VarName>, ToDaeError> {
    // Taken before any branch clones the certificate: only this conditional's
    // own join may admit path-partial values.
    let admit_path_partial = definitions.take_path_partial_admission();
    if let Some(selected) = statically_selected_branch(blocks, fallback_statements, context)? {
        return resolve_static_loop_branch(
            StaticLoopBranch {
                blocks,
                fallback_statements,
                branches,
                fallback_plans,
                selected,
                span,
            },
            context,
            definitions,
        );
    }
    // A conditional that only asserts defines no value but still owns the
    // guarded actions of its branches.
    let owns_actions = branches
        .iter()
        .map(Vec::as_slice)
        .chain(fallback_plans.as_deref().map(Vec::as_slice))
        .any(plans_carry_runtime_assertion);
    let writes_before = definitions.folds.write_count();
    let resolved = resolve_reachable_branches(
        (blocks, fallback_statements),
        (branches, fallback_plans),
        span,
        context,
        definitions,
    )?;
    let ResolvedBranches {
        states: branch_states,
        completing: completing_states,
        ordered,
        exhaustive,
        first_reached,
        pruned,
    } = resolved;
    // A conditional whose reachable branches write nothing has no effect at
    // the binder values that reach it; its other branches never run.
    if branch_states.is_empty() || (ordered.is_empty() && (owns_actions || pruned)) {
        return Ok(ordered);
    }
    if ordered.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function conditional",
            format!(
                "`{}` has a conditional branch without a value definition",
                context.function.name
            ),
            span,
        ));
    }
    // A branch that never completes defines nothing code after the
    // conditional can observe, so only the completing branches are joined.
    let joined_states = if completing_states.is_empty() {
        &branch_states
    } else {
        &completing_states
    };
    let remembers_guard =
        !exhaustive && blocks.len() == 1 && is_immutable_guard(&blocks[0].cond, context);
    let prior = remembers_guard.then(|| definitions.guarded_proofs(&ordered));
    let joined = definitions.join_branches(
        joined_states,
        exhaustive,
        &ordered,
        &admit_path_partial,
        context,
        span,
    )?;
    let branch_folds = branch_states
        .iter()
        .map(|state| &state.folds)
        .collect::<Vec<_>>();
    definitions
        .folds
        .absorb_branch_writes(writes_before, &branch_folds);
    if let Some(prior) = prior.filter(|_| first_reached) {
        definitions.remember_guarded_branch(
            (&blocks[0].cond, prior),
            &branch_states[0],
            &ordered,
            context,
            span,
        );
    }
    Ok(joined)
}

/// The resolved branches of one runtime conditional.
struct ResolvedBranches {
    states: Vec<FunctionDefinitions>,
    completing: Vec<FunctionDefinitions>,
    ordered: Vec<VarName>,
    exhaustive: bool,
    first_reached: bool,
    /// Some branch is reached by no binder value and was not resolved.
    pruned: bool,
}

/// Resolve every branch some binder value reaches.
///
/// Inside a generic loop iteration a branch runs at the binder values its
/// condition, and the failure of every earlier one, select (MLS §11.2.6). A
/// branch no binder value reaches is never executed and is not resolved. A
/// value an earlier iteration certainly defined is defined on a path whose
/// binder values all have an earlier iteration, including the fall-through.
fn resolve_reachable_branches(
    (blocks, fallback_statements): (
        &[rumoca_core::StatementBlock],
        Option<&[rumoca_core::Statement]>,
    ),
    (branches, fallback_plans): (
        &mut [Vec<FunctionStatementPlan>],
        Option<&mut Vec<FunctionStatementPlan>>,
    ),
    span: Span,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<ResolvedBranches, ToDaeError> {
    let mut resolved = ResolvedBranches {
        states: Vec::with_capacity(branches.len() + 1),
        completing: Vec::with_capacity(branches.len() + 1),
        ordered: Vec::new(),
        exhaustive: false,
        first_reached: false,
        pruned: false,
    };
    let conditions = blocks.iter().map(|block| &block.cond).collect::<Vec<_>>();
    // Value facts may show that no execution reaches a branch, or that every
    // execution reaching the conditional takes one; the binder region of a
    // certain branch is then the whole region.
    let reachable = definitions.reachable_paths(&conditions, context);
    let mut remaining = definitions.folds.clone();
    for (ordinal, (block, plans)) in blocks.iter().zip(branches.iter_mut()).enumerate() {
        definitions.require_readable(&block.cond, context, span)?;
        let mut state = definitions.clone();
        let certain = reachable[ordinal + 1..].iter().all(|reaches| !reaches);
        let reached = if certain {
            state.folds = remaining.clone();
            state.folds.reaches()
        } else {
            state.folds.assume_from(&remaining, &block.cond, context)
        };
        remaining.assume(&block.cond, false, context);
        if !reached || !reachable[ordinal] {
            resolved.pruned = true;
            continue;
        }
        resolved.first_reached |= ordinal == 0;
        state.enter_guard(&block.cond, context);
        state.enter_path(&conditions, ordinal, context);
        resolve_conditional_branch(&block.stmts, plans, context, &mut state)?;
        resolved.push(state, &block.stmts, plans);
    }
    let falls_through = remaining.reaches() && reachable[conditions.len()];
    resolved.exhaustive = match (fallback_statements, fallback_plans) {
        (Some(statements), Some(plans)) => {
            if falls_through {
                let mut state = definitions.clone();
                state.folds = remaining.clone();
                state.enter_path(&conditions, conditions.len(), context);
                resolve_conditional_branch(statements, plans, context, &mut state)?;
                resolved.push(state, statements, plans);
            }
            resolved.pruned |= !falls_through;
            true
        }
        (None, None) => !falls_through,
        _ => unreachable!("a planned function conditional keeps its source fallback shape"),
    };
    let ordered = resolved.ordered.clone();
    for state in resolved.states.iter_mut().chain(&mut resolved.completing) {
        state.define_from_earlier_iterations(&ordered);
    }
    if !resolved.exhaustive {
        definitions.enter_path(&conditions, conditions.len(), context);
        let fallthrough = std::mem::replace(&mut definitions.folds, remaining);
        definitions.define_from_earlier_iterations(&ordered);
        definitions.folds = fallthrough;
    }
    Ok(resolved)
}

impl ResolvedBranches {
    fn push(
        &mut self,
        state: FunctionDefinitions,
        statements: &[rumoca_core::Statement],
        plans: &[FunctionStatementPlan],
    ) {
        collect_branch_targets(plans, &mut self.ordered);
        if !branch_never_completes(statements) {
            self.completing.push(state.clone());
        }
        self.states.push(state);
    }
}

/// Whether a runtime branch hands at least one assertion to its action owner.
fn plans_carry_runtime_assertion(plans: &[FunctionStatementPlan]) -> bool {
    plans.iter().any(|plan| match plan {
        FunctionStatementPlan::RuntimeAssertion => true,
        FunctionStatementPlan::If {
            branches, fallback, ..
        } => {
            branches
                .iter()
                .any(|branch| plans_carry_runtime_assertion(branch))
                || fallback
                    .as_deref()
                    .is_some_and(plans_carry_runtime_assertion)
        }
        FunctionStatementPlan::ProvenBranch { statements, .. } => {
            plans_carry_runtime_assertion(statements)
        }
        _ => false,
    })
}

fn statically_selected_branch(
    blocks: &[rumoca_core::StatementBlock],
    _fallback: Option<&[rumoca_core::Statement]>,
    context: FunctionValidationContext<'_>,
) -> Result<Option<Option<usize>>, ToDaeError> {
    for (ordinal, block) in blocks.iter().enumerate() {
        match static_boolean_expression(&block.cond, context)? {
            Some(true) => return Ok(Some(Some(ordinal))),
            Some(false) => {}
            // MLS §11.5 reaches a later arm only after every earlier condition
            // evaluates to false. An unknown earlier condition therefore
            // makes the whole selection unknown, even if a later condition is
            // statically true.
            None => return Ok(None),
        }
    }
    Ok(Some(None))
}

struct StaticLoopBranch<'source, 'plan> {
    blocks: &'source [rumoca_core::StatementBlock],
    fallback_statements: Option<&'source [rumoca_core::Statement]>,
    branches: &'plan mut [Vec<FunctionStatementPlan>],
    fallback_plans: Option<&'plan mut Vec<FunctionStatementPlan>>,
    selected: Option<usize>,
    span: Span,
}

fn resolve_static_loop_branch(
    input: StaticLoopBranch<'_, '_>,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<Vec<VarName>, ToDaeError> {
    let StaticLoopBranch {
        blocks,
        fallback_statements,
        branches,
        mut fallback_plans,
        selected,
        span,
    } = input;
    // Resolution-time folding is subject to the same well-formedness rule as
    // specialization-time folding: MLS §11.5 selects execution, but it does
    // not make malformed statements in the other arms legal.
    check_unexecuted_branches(blocks, fallback_statements, selected, context)?;
    for block in blocks {
        definitions.require_readable(&block.cond, context, span)?;
    }
    match selected {
        Some(ordinal) => resolve_conditional_branch(
            &blocks[ordinal].stmts,
            &mut branches[ordinal],
            context,
            definitions,
        )?,
        None => {
            if let (Some(statements), Some(plans)) =
                (fallback_statements, fallback_plans.as_deref_mut())
            {
                resolve_conditional_branch(statements, plans, context, definitions)?;
            }
        }
    }
    // Resolving the selected branch may populate a nested conditional's target
    // certificate. Collect afterwards so this point contributes every value
    // its executed nested statements define; fold analysis unions those point
    // certificates over the compact domain.
    let mut ordered = Vec::new();
    for branch in branches.iter() {
        collect_branch_targets(branch, &mut ordered);
    }
    if let Some(fallback) = fallback_plans.as_deref() {
        collect_branch_targets(fallback, &mut ordered);
    }
    Ok(ordered)
}

pub(super) fn static_boolean_expression(
    expression: &Expression,
    context: FunctionValidationContext<'_>,
) -> Result<Option<bool>, ToDaeError> {
    static_boolean_expression_at_depth(expression, context, 0)
}

/// The value a specialization settles for one Boolean condition, if any.
///
/// A compiler-generated branch guard captures its MLS §11.5 branch predicate in
/// an immutable Boolean before the algorithm's mutable values can change; its
/// definition is an ordinary Boolean expression over the function inputs and
/// earlier guards. Folding a guard back to its definition is what lets a
/// value-proven specialization settle the branch selection of a conditional
/// whose arm carries a loop, which the guard rewrite would otherwise hide behind
/// the generated name. `depth` bounds that expansion; the guard chain is acyclic
/// because each guard reads only guards defined before it.
fn static_boolean_expression_at_depth(
    expression: &Expression,
    context: FunctionValidationContext<'_>,
    depth: usize,
) -> Result<Option<bool>, ToDaeError> {
    if depth >= 64 {
        return Ok(None);
    }
    match expression {
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Ok(Some(*value)),
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => Ok(static_boolean_expression_at_depth(rhs, context, depth + 1)?.map(|value| !value)),
        Expression::Binary {
            op: OpBinary::And,
            lhs,
            rhs,
            ..
        } => Ok(
            match (
                static_boolean_expression_at_depth(lhs, context, depth + 1)?,
                static_boolean_expression_at_depth(rhs, context, depth + 1)?,
            ) {
                (Some(false), _) | (_, Some(false)) => Some(false),
                (Some(true), Some(true)) => Some(true),
                _ => None,
            },
        ),
        Expression::Binary {
            op: OpBinary::Or,
            lhs,
            rhs,
            ..
        } => Ok(
            match (
                static_boolean_expression_at_depth(lhs, context, depth + 1)?,
                static_boolean_expression_at_depth(rhs, context, depth + 1)?,
            ) {
                (Some(true), _) | (_, Some(true)) => Some(true),
                (Some(false), Some(false)) => Some(false),
                _ => None,
            },
        ),
        Expression::Binary { op, lhs, rhs, .. }
            if matches!(
                op,
                OpBinary::Eq
                    | OpBinary::Neq
                    | OpBinary::Lt
                    | OpBinary::Le
                    | OpBinary::Gt
                    | OpBinary::Ge
            ) =>
        {
            let Some(lhs) =
                static_shape_integer_expression(lhs, context.static_integers, context.shapes)?
            else {
                return Ok(None);
            };
            let Some(rhs) =
                static_shape_integer_expression(rhs, context.static_integers, context.shapes)?
            else {
                return Ok(None);
            };
            Ok(Some(match op {
                OpBinary::Eq => lhs == rhs,
                OpBinary::Neq => lhs != rhs,
                OpBinary::Lt => lhs < rhs,
                OpBinary::Le => lhs <= rhs,
                OpBinary::Gt => lhs > rhs,
                OpBinary::Ge => lhs >= rhs,
                _ => unreachable!("guard admits relational operators"),
            }))
        }
        // MLS §11.5 evaluates a conditional's branch conditions in order; the
        // guard rewrite renders that same first-true selection as an `if`
        // expression, so folding it settles the guard exactly when the
        // specialization settles every condition it must evaluate.
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                match static_boolean_expression_at_depth(condition, context, depth + 1)? {
                    Some(true) => {
                        return static_boolean_expression_at_depth(value, context, depth + 1);
                    }
                    Some(false) => {}
                    None => return Ok(None),
                }
            }
            static_boolean_expression_at_depth(else_branch, context, depth + 1)
        }
        // A generated branch guard is a name for its definition; fold through it
        // so the branch selection is settled by the same inputs the guard reads.
        _ => match super::function_definitions::generated_boolean_value(expression, context) {
            Some(value) => static_boolean_expression_at_depth(value, context, depth + 1),
            None => Ok(None),
        },
    }
}

pub(super) fn is_immutable_guard(
    condition: &Expression,
    context: FunctionValidationContext<'_>,
) -> bool {
    let mut references = Vec::new();
    condition.collect_var_refs(&mut references);
    !references.is_empty()
        && references.iter().all(|target| {
            context.static_integers.contains_key(target)
                || context
                    .generated_booleans
                    .iter()
                    .any(|definition| &definition.target == target)
        })
}

/// Prove that every statement in a runtime branch has an expression owner.
///
/// A nested conditional has exactly such an owner: its own checked conditional
/// expressions, derived from the branch-local definition state.  A loop still
/// needs a compact transition owner, which cannot be embedded in an expression
/// branch, so it remains rejected here.
fn resolve_conditional_branch(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    validate_conditional_branch_shape(statements, plans, context)?;
    resolve_function_definitions(statements, plans, context, definitions)
}

fn validate_conditional_branch_shape(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    for (statement, plan) in statements.iter().zip(plans) {
        match (statement, plan) {
            (_, FunctionStatementPlan::ProvenAssertion) => continue,
            (_, FunctionStatementPlan::RuntimeAssertion) => continue,
            (_, FunctionStatementPlan::Assignment(_)) => continue,
            (_, FunctionStatementPlan::RecordAssembly(_))
            | (_, FunctionStatementPlan::RecordAssemblyMember) => continue,
            (_, FunctionStatementPlan::MultiOutputCall { .. }) => continue,
            (
                rumoca_core::Statement::If {
                    cond_blocks,
                    else_block,
                    ..
                },
                FunctionStatementPlan::If {
                    branches, fallback, ..
                },
            ) => {
                for (block, branch) in cond_blocks.iter().zip(branches) {
                    validate_conditional_branch_shape(&block.stmts, branch, context)?;
                }
                if let (Some(source), Some(branch)) = (else_block.as_deref(), fallback.as_deref()) {
                    validate_conditional_branch_shape(source, branch, context)?;
                }
                continue;
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
                let source =
                    selected_conditional_statements(cond_blocks, else_block.as_deref(), *selected);
                validate_conditional_branch_shape(source, statements, context)?;
                continue;
            }
            _ => {}
        }
        return Err(unsupported_statement(
            statement,
            "function conditional branch statement",
            "function conditional",
            format!(
                "`{}` requires assignments or nested conditionals in every checked branch",
                context.function.name
            ),
        ));
    }
    Ok(())
}

fn collect_branch_targets(plans: &[FunctionStatementPlan], ordered: &mut Vec<VarName>) {
    for plan in plans {
        match plan {
            FunctionStatementPlan::Assignment(assignment) => {
                collect_branch_target(assignment.target(), ordered);
            }
            FunctionStatementPlan::RecordAssembly(assembly) => {
                collect_branch_target(&assembly.target, ordered);
            }
            FunctionStatementPlan::MultiOutputCall { outputs } => {
                for output in outputs.iter().flatten() {
                    collect_branch_target(output.target(), ordered);
                }
            }
            FunctionStatementPlan::If { targets, .. } => {
                for target in targets {
                    collect_branch_target(target, ordered);
                }
            }
            FunctionStatementPlan::ProvenBranch { statements, .. } => {
                collect_branch_targets(statements, ordered);
            }
            _ => {}
        }
    }
}

fn collect_branch_target(target: &VarName, ordered: &mut Vec<VarName>) {
    if !ordered.contains(target) {
        ordered.push(target.clone());
    }
}

/// Whether a branch ends every execution in an assertion whose condition is
/// the literal `false` (MLS §8.3.7: the assertion fails and the function call
/// does not return), as in the `else assert(false, ...)` arm of an exhaustive
/// region dispatch.
pub(in crate::construction) fn branch_never_completes(
    statements: &[rumoca_core::Statement],
) -> bool {
    statements.iter().any(|statement| {
        matches!(
            statement,
            rumoca_core::Statement::Assert {
                condition: Expression::Literal {
                    value: Literal::Boolean(false),
                    ..
                },
                level: None,
                ..
            }
        )
    })
}
