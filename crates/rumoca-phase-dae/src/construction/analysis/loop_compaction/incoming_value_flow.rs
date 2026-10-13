//! Ordered incoming-value reads over the same source/plan statement tree.
//!
//! One walk answers the question for any number of names: the undefined paths
//! of every name advance together, and names whose paths are equal share each
//! step, so the cost does not grow with the number of names asked about.

use super::super::guard_facts::{FactScope, PathSet};
use super::*;
use std::collections::BTreeMap;

#[derive(Clone, Copy)]
pub(in crate::construction::analysis) struct StatementSequence<'a> {
    pub(in crate::construction::analysis) statements: &'a [rumoca_core::Statement],
    pub(in crate::construction::analysis) plans: Option<&'a [FunctionStatementPlan]>,
}

#[cfg(test)]
pub(in crate::construction::analysis) fn statements_read_incoming_name(
    statements: &[rumoca_core::Statement],
    name: &VarName,
) -> bool {
    statement_segments_read_incoming_name(&[statements], name)
}

/// Which of `names` some path of `statements` reads before writing it whole,
/// one flag per name.
pub(in crate::construction::analysis) fn statements_read_incoming_names(
    statements: &[rumoca_core::Statement],
    names: &[&VarName],
) -> Vec<bool> {
    let shapes = ShapeEnvironment::default();
    let kinds = ValueKinds::default();
    read_incoming(
        &[StatementSequence {
            statements,
            plans: None,
        }],
        names,
        kinds.scope(&shapes),
        true,
    )
}

#[cfg(test)]
pub(in crate::construction::analysis) fn statement_segments_read_incoming_name(
    segments: &[&[rumoca_core::Statement]],
    name: &VarName,
) -> bool {
    sequences_read_incoming_name(
        &segments
            .iter()
            .map(|statements| StatementSequence {
                statements,
                plans: None,
            })
            .collect::<Vec<_>>(),
        name,
    )
}

#[cfg(test)]
pub(in crate::construction::analysis) fn sequences_read_incoming_name(
    sequences: &[StatementSequence<'_>],
    name: &VarName,
) -> bool {
    let shapes = ShapeEnvironment::default();
    let kinds = ValueKinds::default();
    read_incoming(sequences, &[name], kinds.scope(&shapes), true)[0]
}

pub(in crate::construction::analysis) fn planned_sequences_read_incoming_name(
    sequences: &[StatementSequence<'_>],
    name: &VarName,
    context: FunctionValidationContext<'_>,
) -> bool {
    planned_sequences_read_incoming_names(sequences, &[name], context)[0]
}

/// Which of `names` some path of the planned `sequences` reads before writing
/// it whole, one flag per name.
pub(in crate::construction::analysis) fn planned_sequences_read_incoming_names(
    sequences: &[StatementSequence<'_>],
    names: &[&VarName],
    context: FunctionValidationContext<'_>,
) -> Vec<bool> {
    read_incoming(sequences, names, context.fact_scope(), true)
}

/// Which of `names` some path of the planned `sequences` reads before writing
/// it whole, reasoning over the statement structure alone: no guard fact
/// removes a path, so a read under a condition counts on every path that
/// reaches the statement.
///
/// A value whose reads are all dominated by a whole definition in this sense
/// has a definition at each read without relying on a guard to exclude the
/// paths that leave it undefined.
pub(in crate::construction::analysis) fn planned_sequences_structurally_read_incoming_names(
    sequences: &[StatementSequence<'_>],
    names: &[&VarName],
) -> Vec<bool> {
    let shapes = ShapeEnvironment::default();
    let kinds = ValueKinds::default();
    read_incoming(sequences, names, kinds.scope(&shapes), false)
}

/// Which of `names` the planned `sequences` write whole on every path without
/// reading the incoming value first, reasoning over the statement structure
/// alone.
pub(in crate::construction::analysis) fn planned_sequences_define_names(
    sequences: &[StatementSequence<'_>],
    names: &[&VarName],
) -> Vec<bool> {
    let shapes = ShapeEnvironment::default();
    let kinds = ValueKinds::default();
    let (read, undefined) = walk(sequences, names, kinds.scope(&shapes), false);
    (0..names.len())
        .map(|index| !read[index] && !undefined.contains_key(&index))
        .collect()
}

pub(in crate::construction::analysis) fn planned_statement_reads_incoming(
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
    name: &VarName,
    context: FunctionValidationContext<'_>,
) -> bool {
    planned_sequences_read_incoming_name(
        &[StatementSequence {
            statements: std::slice::from_ref(statement),
            plans: Some(std::slice::from_ref(plan)),
        }],
        name,
        context,
    )
}

/// The paths that leave each still-unread name undefined, keyed by the name's
/// position in the query. A name leaves the map when it is read or when no
/// path leaves it undefined.
type Undefined = BTreeMap<usize, PathSet>;

struct Query<'a> {
    names: &'a [&'a VarName],
    scope: FactScope<'a>,
    /// Whether guard facts remove paths. Without them every path that reaches a
    /// statement counts, whatever condition the statement sits under.
    guarded: bool,
    read: Vec<bool>,
}

fn read_incoming(
    sequences: &[StatementSequence<'_>],
    names: &[&VarName],
    scope: FactScope<'_>,
    guarded: bool,
) -> Vec<bool> {
    walk(sequences, names, scope, guarded).0
}

/// Walk `sequences` once: which names some path reads before writing them
/// whole, and which names a path still leaves undefined at the end.
fn walk(
    sequences: &[StatementSequence<'_>],
    names: &[&VarName],
    scope: FactScope<'_>,
    guarded: bool,
) -> (Vec<bool>, Undefined) {
    let mut undefined: Undefined = (0..names.len())
        .map(|index| (index, PathSet::single(GuardFacts::entry())))
        .collect();
    let mut query = Query {
        names,
        scope,
        guarded,
        read: vec![false; names.len()],
    };
    for sequence in sequences {
        query.sequence(*sequence, &mut undefined);
    }
    (query.read, undefined)
}

/// Replace every path set by `step` of it. Equal sets are advanced once and
/// the result is shared by every name that held it.
fn advance_shared(undefined: &mut Undefined, mut step: impl FnMut(&PathSet) -> PathSet) {
    let mut advanced: Vec<(PathSet, PathSet)> = Vec::new();
    for paths in undefined.values_mut() {
        let next = match advanced.iter().find(|(before, _)| before == paths) {
            Some((_, after)) => after.clone(),
            None => {
                let after = step(paths);
                advanced.push((paths.clone(), after.clone()));
                after
            }
        };
        *paths = next;
    }
    undefined.retain(|_, paths| !paths.is_unreachable());
}

impl Query<'_> {
    /// Advance the path sets by `step` when guard facts reason about paths.
    fn advance(&self, undefined: &mut Undefined, step: impl FnMut(&PathSet) -> PathSet) {
        if self.guarded {
            advance_shared(undefined, step);
        }
    }

    fn mark_read(&mut self, undefined: &mut Undefined, index: usize) {
        self.read[index] = true;
        undefined.remove(&index);
    }

    /// The names still unread, with their paths, as a state to walk a child
    /// region from.
    fn unread(&self, undefined: &Undefined) -> Undefined {
        undefined
            .iter()
            .filter(|(index, _)| !self.read[**index])
            .map(|(index, paths)| (*index, paths.clone()))
            .collect()
    }

    fn sequence(&mut self, sequence: StatementSequence<'_>, undefined: &mut Undefined) {
        if let Some(plans) = sequence.plans {
            assert_eq!(sequence.statements.len(), plans.len());
        }
        for (index, statement) in sequence.statements.iter().enumerate() {
            if undefined.is_empty() {
                return;
            }
            self.statement(
                statement,
                sequence.plans.map(|plans| &plans[index]),
                undefined,
            );
        }
    }

    fn statement(
        &mut self,
        statement: &rumoca_core::Statement,
        plan: Option<&FunctionStatementPlan>,
        undefined: &mut Undefined,
    ) {
        for index in undefined.keys().copied().collect::<Vec<_>>() {
            if partial_base_read(plan, self.names[index]) {
                self.mark_read(undefined, index);
            }
        }
        if let Some(FunctionStatementPlan::GeneratedBooleanAssignment { target, value, .. }) = plan
        {
            return self.generated_assignment(target, value, undefined);
        }
        match statement {
            rumoca_core::Statement::For {
                indices, equations, ..
            } => self.loop_read(indices, equations, plan, undefined),
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => self.conditional_read(cond_blocks, else_block.as_deref(), plan, undefined),
            _ => self.leaf(statement, undefined),
        }
    }

    fn generated_assignment(
        &mut self,
        target: &VarName,
        value: &Expression,
        undefined: &mut Undefined,
    ) {
        for index in undefined.keys().copied().collect::<Vec<_>>() {
            let name = self.names[index];
            if expression_reads_name(value, name) {
                self.mark_read(undefined, index);
            } else if target == name {
                undefined.remove(&index);
            }
        }
        let scope = self.scope;
        self.advance(undefined, |paths| {
            let mut after = paths.clone();
            after.assign(target, value, scope);
            after
        });
    }

    /// A statement that owns no nested statements: each name is read by it,
    /// written whole by it, or merely carried past it.
    fn leaf(&mut self, statement: &rumoca_core::Statement, undefined: &mut Undefined) {
        for index in undefined.keys().copied().collect::<Vec<_>>() {
            let name = self.names[index];
            let (reads, writes_whole) = leaf_effect(statement, name);
            if reads {
                self.mark_read(undefined, index);
            } else if writes_whole {
                undefined.remove(&index);
            }
        }
        let scope = self.scope;
        self.advance(undefined, |paths| {
            let mut after = paths.clone();
            after.after(statement, scope);
            after
        });
    }

    fn loop_read(
        &mut self,
        indices: &[rumoca_core::ForIndex],
        equations: &[rumoca_core::Statement],
        plan: Option<&FunctionStatementPlan>,
        undefined: &mut Undefined,
    ) {
        let (indices, body, plans) = if let Some(FunctionStatementPlan::For {
            statements,
            source_depth,
            ..
        }) = plan
        {
            let (indices, body) = super::super::function_loops::flattened_function_loop_source(
                indices,
                equations,
                *source_depth,
            );
            (indices, body, Some(statements.as_slice()))
        } else {
            (indices.iter().collect::<Vec<_>>(), equations, None)
        };
        for index in undefined.keys().copied().collect::<Vec<_>>() {
            let name = self.names[index];
            if indices
                .iter()
                .any(|index| expression_reads_name(&index.range, name))
            {
                self.mark_read(undefined, index);
            }
        }
        let binders = indices
            .iter()
            .map(|index| VarName::new(&index.ident))
            .collect::<Vec<_>>();
        let scope = self.scope;
        let mut entry = self.unread(undefined);
        self.advance(&mut entry, |paths| paths.loop_entry(body, &binders, scope));
        // The child may execute zero times. Its writes cannot remove its
        // entry's undefined paths, and facts about anything it writes do not
        // survive it.
        let mut after = entry.clone();
        self.sequence(
            StatementSequence {
                statements: body,
                plans,
            },
            &mut entry,
        );
        after.retain(|index, _| !self.read[*index]);
        for (index, paths) in &entry {
            if let Some(joined) = after.get_mut(index) {
                joined.join_path(paths);
            }
        }
        *undefined = after;
    }

    fn conditional_read(
        &mut self,
        blocks: &[rumoca_core::StatementBlock],
        fallback: Option<&[rumoca_core::Statement]>,
        plan: Option<&FunctionStatementPlan>,
        undefined: &mut Undefined,
    ) {
        let conditions = blocks.iter().map(|block| &block.cond).collect::<Vec<_>>();
        for position in 0..conditions.len() {
            self.condition_read(&conditions, position, undefined);
        }
        if let Some(FunctionStatementPlan::ProvenBranch {
            selected,
            statements,
        }) = plan
        {
            let body = selected
                .map(|index| blocks[index].stmts.as_slice())
                .unwrap_or_else(|| fallback.unwrap_or(&[]));
            return self.sequence(
                StatementSequence {
                    statements: body,
                    plans: Some(statements),
                },
                undefined,
            );
        }
        let (branches, fallback_plans) =
            if let Some(FunctionStatementPlan::If {
                branches, fallback, ..
            }) = plan
            {
                assert_eq!(blocks.len(), branches.len());
                (Some(branches.as_slice()), fallback.as_deref())
            } else {
                (None, None)
            };
        let mut joined = Undefined::new();
        for (position, block) in blocks.iter().enumerate() {
            let mut entered = self.entering(undefined, &conditions, position);
            self.sequence(
                StatementSequence {
                    statements: &block.stmts,
                    plans: branches.map(|branches| branches[position].as_slice()),
                },
                &mut entered,
            );
            join_into(&mut joined, &entered);
        }
        let mut remaining = self.entering(undefined, &conditions, conditions.len());
        if let Some(body) = fallback {
            self.sequence(
                StatementSequence {
                    statements: body,
                    plans: fallback_plans,
                },
                &mut remaining,
            );
        }
        join_into(&mut joined, &remaining);
        joined.retain(|index, _| !self.read[*index]);
        *undefined = joined;
    }

    /// Mark the names the condition at `position` reads on a path that reaches
    /// it.
    fn condition_read(
        &mut self,
        conditions: &[&Expression],
        position: usize,
        undefined: &mut Undefined,
    ) {
        for index in undefined.keys().copied().collect::<Vec<_>>() {
            let reached = || {
                !self.guarded
                    || !undefined[&index]
                        .entering(&conditions[..position], position, self.scope)
                        .is_unreachable()
            };
            if expression_reads_name(conditions[position], self.names[index]) && reached() {
                self.mark_read(undefined, index);
            }
        }
    }

    fn entering(
        &self,
        undefined: &Undefined,
        conditions: &[&Expression],
        ordinal: usize,
    ) -> Undefined {
        let scope = self.scope;
        let mut entered = self.unread(undefined);
        self.advance(&mut entered, |paths| {
            paths.entering(conditions, ordinal, scope)
        });
        entered
    }
}

fn join_into(joined: &mut Undefined, branch: &Undefined) {
    for (index, paths) in branch {
        joined
            .entry(*index)
            .or_insert_with(PathSet::empty)
            .join_path(paths);
    }
}

/// Whether the plan assigns part of `name` or part of a multi-output target,
/// which reads the value the name held on entry.
fn partial_base_read(plan: Option<&FunctionStatementPlan>, name: &VarName) -> bool {
    match plan {
        Some(FunctionStatementPlan::Assignment(assignment)) => {
            assignment.target() == name && !assignment.is_whole()
        }
        Some(FunctionStatementPlan::MultiOutputCall { outputs, .. }) => outputs
            .iter()
            .flatten()
            .any(|output| output.target() == name && !output.is_whole()),
        _ => false,
    }
}

/// Whether a statement without a nested sequence reads `name`, and whether it
/// writes the whole scalar `name`.
fn leaf_effect(statement: &rumoca_core::Statement, name: &VarName) -> (bool, bool) {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, .. } => (
            expression_reads_name(value, name) || component_subscripts_read_name(comp, name),
            scalar_assignment_target(comp).as_ref() == Some(name),
        ),
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            ..
        } => (
            comp.component_ref()
                .is_some_and(|callable| component_subscripts_read_name(callable, name))
                || args.iter().any(|arg| expression_reads_name(arg, name))
                || outputs
                    .iter()
                    .flatten()
                    .any(|output| component_subscripts_read_name(output, name)),
            outputs
                .iter()
                .flatten()
                .any(|output| scalar_assignment_target(output).as_ref() == Some(name)),
        ),
        _ => (
            statements_read_name(std::slice::from_ref(statement), name),
            false,
        ),
    }
}

#[cfg(test)]
mod structural_tests {
    use super::super::liveness_corpus::{assign, branch};
    use super::*;

    fn structural_reads(statements: &[rumoca_core::Statement], names: &[&VarName]) -> Vec<bool> {
        planned_sequences_structurally_read_incoming_names(
            &[StatementSequence {
                statements,
                plans: None,
            }],
            names,
        )
    }

    #[test]
    fn a_read_under_the_guard_of_a_partial_definition_reads_the_incoming_value() {
        let a = VarName::new("a");
        let program = vec![
            branch(&["g"], vec![assign("a", &[])], None),
            branch(&["g"], vec![assign("c", &["a"])], None),
        ];
        assert_eq!(structural_reads(&program, &[&a]), vec![true]);
    }

    #[test]
    fn a_read_after_a_definition_on_every_path_does_not_read_the_incoming_value() {
        let a = VarName::new("a");
        let b = VarName::new("b");
        let program = vec![
            branch(
                &["g"],
                vec![assign("a", &[])],
                Some(vec![assign("a", &["b"])]),
            ),
            branch(&["g"], vec![assign("c", &["a"])], None),
        ];
        assert_eq!(structural_reads(&program, &[&a, &b]), vec![false, true]);
    }
}

#[cfg(test)]
mod definition_tests {
    use super::super::liveness_corpus::{assign, branch};
    use super::*;

    fn defined(statements: &[rumoca_core::Statement], names: &[&VarName]) -> Vec<bool> {
        planned_sequences_define_names(
            &[StatementSequence {
                statements,
                plans: None,
            }],
            names,
        )
    }

    #[test]
    fn a_value_written_on_every_path_is_defined() {
        let a = VarName::new("a");
        let program = vec![branch(
            &["g"],
            vec![assign("a", &[])],
            Some(vec![assign("a", &[])]),
        )];
        assert_eq!(defined(&program, &[&a]), vec![true]);
    }

    #[test]
    fn a_value_a_path_leaves_unwritten_is_not_defined() {
        let a = VarName::new("a");
        let b = VarName::new("b");
        let program = vec![
            branch(&["g"], vec![assign("a", &[])], None),
            assign("b", &["a"]),
        ];
        assert_eq!(defined(&program, &[&a, &b]), vec![false, true]);
    }
}
