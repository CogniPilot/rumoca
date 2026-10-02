//! MLS §12.4.4 definedness assertions of a function's top-level sequence.
//!
//! "It is an error to use or return an uninitialized variable in a function."
//! The error belongs to the executed path, not to the program text: an
//! `if`/`elseif` chain without an `else` may leave a value unwritten on the
//! paths the caller never takes. A top-level conditional therefore joins a
//! value that every writing path defines totally even when other paths leave
//! it undefined; the joined definition is dead on those paths. The value is
//! path-partial until a top-level statement reads it, which the construction
//! precedes with a call-scoped assertion that the executed path defined it.
//! Every path that survives the assertion has the total definition, so the
//! value is total afterwards. Reads in nested statements and writes the
//! construction does not correlate with the assertion predicate keep the
//! static rejection.

use super::*;

/// Plan and resolve the top-level sequence of a function body, returning the
/// definedness assertions construction emits for it.
pub(super) fn validate_top_level_statements(
    function: &rumoca_core::Function,
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(Vec<FunctionStatementPlan>, FunctionDefinednessPlan), ToDaeError> {
    let mut plans = plan_function_statements(statements, context)?;
    annotate_iteration_locals(statements, &mut plans, context, &[], &[]);
    let mut definedness = FunctionDefinednessPlan::default();
    resolve_sequence_definitions(
        statements,
        &mut plans,
        context,
        definitions,
        Some(&mut definedness),
    )?;
    let outputs = function
        .outputs
        .iter()
        .map(|output| VarName::new(&output.name))
        .collect::<Vec<_>>();
    definedness.returned = definitions.admit_asserted_reads(&outputs);
    Ok((plans, definedness))
}

/// Admit the reads of a flat top-level statement, or open the next
/// conditional's path-partial join. Returns the values that are path-partial
/// before a conditional, which `after_top_level_statement` correlates with
/// its join.
pub(super) fn before_top_level_statement(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    index: usize,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
    definedness: &mut FunctionDefinednessPlan,
) -> Vec<VarName> {
    match &plans[index] {
        FunctionStatementPlan::If { .. } => {
            definitions.admit_next_path_partial_join(later_top_level_uses(
                statements,
                plans,
                index + 1,
                context.function,
            ));
            statement_targets(&statements[index])
                .into_iter()
                .filter(|target| definitions.is_path_partial(target))
                .collect()
        }
        plan => {
            let Some(count) = flat_group_len(plan) else {
                return Vec::new();
            };
            let reads = flat_group_reads(&statements[index..index + count]);
            let admitted = definitions.admit_asserted_reads(&reads);
            if !admitted.is_empty() {
                definedness.asserted_reads.insert(index, admitted);
            }
            Vec::new()
        }
    }
}

/// Record the targets a top-level conditional leaves path-partial, and
/// withdraw the admission from values any other compound statement writes.
pub(super) fn after_top_level_statement(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    index: usize,
    partial_before: &[VarName],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
    definedness: &mut FunctionDefinednessPlan,
) -> Result<(), ToDaeError> {
    match &plans[index] {
        FunctionStatementPlan::If { targets, .. } => {
            let span = required_statement_span(&statements[index], "function conditional")?;
            let mut partial = Vec::new();
            for target in targets {
                if !definitions.is_path_partial(target) {
                    continue;
                }
                partial.push(PartialJoinPlan {
                    target: target.clone(),
                    was_partial: partial_before.contains(target),
                    seed: definitions.whole_loop_seed(target, context, span)?,
                });
            }
            if !partial.is_empty() {
                definedness.partial_joins.insert(index, partial);
            }
        }
        plan if flat_group_len(plan).is_some() => {}
        FunctionStatementPlan::RecordAssemblyMember
        | FunctionStatementPlan::RecordFieldAssemblyMember => {}
        _ => definitions.withdraw_path_partial(&statement_targets(&statements[index])),
    }
    Ok(())
}

/// The values a later top-level statement may read where the definedness
/// assertion can own the read, plus the outputs the function returns.
///
/// A conditional joins a path-partial value only when such a use exists:
/// a value nothing reads afterwards needs no definition past the
/// conditional, and keeps the branch-local form it has without one.
fn later_top_level_uses(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    start: usize,
    function: &rumoca_core::Function,
) -> HashSet<VarName> {
    let mut uses = function
        .outputs
        .iter()
        .map(|output| VarName::new(&output.name))
        .collect::<HashSet<_>>();
    let mut index = start;
    while index < statements.len() {
        let count = flat_group_len(&plans[index]);
        if let Some(count) = count {
            uses.extend(flat_group_reads(&statements[index..index + count]));
        }
        index += count.unwrap_or(1);
    }
    uses
}

/// The statement count of a top-level group whose statements all evaluate at
/// the top level, so a read in it is a top-level read.
fn flat_group_len(plan: &FunctionStatementPlan) -> Option<usize> {
    match plan {
        FunctionStatementPlan::Assignment(_)
        | FunctionStatementPlan::ProvenAssertion
        | FunctionStatementPlan::RuntimeAssertion
        | FunctionStatementPlan::GeneratedBooleanAssignment { .. }
        | FunctionStatementPlan::MultiOutputCall { .. }
        | FunctionStatementPlan::RecordMultiOutputAssembly(_) => Some(1),
        FunctionStatementPlan::RecordAssembly(assembly) => Some(assembly.statement_count),
        FunctionStatementPlan::RecordFieldAssembly(assembly) => Some(assembly.statement_count),
        _ => None,
    }
}

/// Every value a flat statement group reads, in source order.
fn flat_group_reads(statements: &[rumoca_core::Statement]) -> Vec<VarName> {
    let mut reads = Vec::new();
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                value.collect_var_refs(&mut reads);
                collect_reference_subscript_reads(comp, &mut reads);
            }
            rumoca_core::Statement::FunctionCall { args, outputs, .. } => {
                for argument in args {
                    argument.collect_var_refs(&mut reads);
                }
                for output in outputs.iter().flatten() {
                    collect_reference_subscript_reads(output, &mut reads);
                }
            }
            rumoca_core::Statement::Assert {
                condition,
                message,
                level,
                ..
            } => {
                condition.collect_var_refs(&mut reads);
                message.collect_var_refs(&mut reads);
                if let Some(level) = level {
                    level.collect_var_refs(&mut reads);
                }
            }
            _ => {}
        }
    }
    reads
}

fn collect_reference_subscript_reads(
    reference: &rumoca_core::ComponentReference,
    reads: &mut Vec<VarName>,
) {
    for part in reference.parts() {
        for subscript in &part.subs {
            if let Subscript::Expr { expr, .. } = subscript {
                expr.collect_var_refs(reads);
            }
        }
    }
}

/// Every value a statement assigns anywhere inside it.
fn statement_targets(statement: &rumoca_core::Statement) -> Vec<VarName> {
    let mut targets = Vec::new();
    collect_statement_targets(statement, &mut targets);
    targets
}

fn collect_statement_targets(statement: &rumoca_core::Statement, targets: &mut Vec<VarName>) {
    let mut push = |reference: &rumoca_core::ComponentReference| {
        let path = reference
            .parts()
            .iter()
            .map(|part| part.ident.as_str())
            .collect::<Vec<_>>();
        let name = VarName::new(path.join("."));
        if !targets.contains(&name) {
            targets.push(name);
        }
    };
    match statement {
        rumoca_core::Statement::Assignment { comp, .. } => push(comp),
        rumoca_core::Statement::FunctionCall { outputs, .. } => {
            outputs.iter().flatten().for_each(push);
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for statement in cond_blocks
                .iter()
                .flat_map(|block| &block.stmts)
                .chain(else_block.iter().flatten())
            {
                collect_statement_targets(statement, targets);
            }
        }
        rumoca_core::Statement::For { equations, .. } => {
            for statement in equations {
                collect_statement_targets(statement, targets);
            }
        }
        rumoca_core::Statement::While { block, .. } => {
            for statement in &block.stmts {
                collect_statement_targets(statement, targets);
            }
        }
        _ => {}
    }
}
