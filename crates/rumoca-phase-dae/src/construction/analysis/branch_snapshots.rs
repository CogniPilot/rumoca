//! Branch-selection snapshots for runtime conditionals that own loops.
//!
//! MLS 3.6 §11.2.6 evaluates the conditions of an `if` statement once, in
//! order, at the statement, and then executes exactly the selected statement
//! sequence; statements in that sequence may change the values the conditions
//! read without reselecting a branch. A loop has no owner inside an expression
//! branch, so a selected sequence that contains a loop runs as statements
//! guarded by the branch's selection, and the loop-free remainder stays one
//! conditional over the same selections.
//!
//! Each selection whose condition reads a mutable value is captured at the
//! original source position in a generated immutable Boolean. The capture is a
//! statement of the sequence that holds the conditional, so a conditional
//! inside a loop body is captured again on every iteration (MLS §11.2.2),
//! while an inner loop reads the capture as an invariant of its enclosing
//! iteration. A guard pushed into a `for` body does not reselect a branch: its
//! range is statically proven, and the capture does not change in the loop.
//!
//! This pass runs after loop compaction, so no store-deleting or substituting
//! rewrite ever reasons about a capture whose reads it cannot see. Its result
//! carries a lexical scope certificate: every generated Boolean is defined once
//! per execution of the sequence holding it, before any read, and is read only
//! later in that sequence.

use super::function_bodies::statement_reads_target;
use super::function_returns::{GeneratedBooleanDefinition, and_condition, guarded_statement};
use super::*;
use rumoca_core::Reference;

/// Rewrite every conditional whose branches own a loop, at any nesting depth,
/// into captured selections, guarded statements through its last loop and one
/// loop-free remainder conditional. `guards` holds the return guards issued
/// before compaction and receives the captures issued here.
pub(super) fn snapshot_loop_conditionals(
    statements: &[rumoca_core::Statement],
    guards: &mut Vec<GeneratedBooleanDefinition>,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let mut snapshots = Snapshots {
        immutable: guards.iter().map(|guard| guard.target.clone()).collect(),
        guards,
    };
    let normalized = snapshots.sequence(statements, None)?;
    certify_generated_scopes(&normalized, snapshots.guards)?;
    Ok(normalized)
}

struct Snapshots<'guards> {
    guards: &'guards mut Vec<GeneratedBooleanDefinition>,
    /// Generated Booleans: no statement assigns them after their definition.
    immutable: HashSet<VarName>,
}

impl Snapshots<'_> {
    /// `active` is the immutable selection under which this sequence runs.
    fn sequence(
        &mut self,
        statements: &[rumoca_core::Statement],
        active: Option<&Expression>,
    ) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
        let mut normalized = Vec::with_capacity(statements.len());
        for statement in statements {
            self.statement(statement, active, &mut normalized)?;
        }
        Ok(normalized)
    }

    fn statement(
        &mut self,
        statement: &rumoca_core::Statement,
        active: Option<&Expression>,
        normalized: &mut Vec<rumoca_core::Statement>,
    ) -> Result<(), ToDaeError> {
        match statement {
            // The selection is invariant in the loop and the static range is
            // evaluated by no runtime path, so `if g then for .. B` is
            // `for .. if g then B`, and B's own conditionals are captured on
            // each of its iterations.
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => normalized.push(rumoca_core::Statement::For {
                indices: indices.clone(),
                equations: self.sequence(equations, active)?,
                span: *span,
            }),
            // A false selection must not iterate a `while`, so the guard stays
            // outside it; its body is captured per iteration.
            rumoca_core::Statement::While { block, span } => {
                let looped = rumoca_core::Statement::While {
                    block: rumoca_core::StatementBlock {
                        cond: block.cond.clone(),
                        stmts: self.sequence(&block.stmts, None)?,
                    },
                    span: *span,
                };
                normalized.push(match active {
                    Some(active) => guarded_statement(&looped, active.clone()),
                    None => looped,
                });
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } if branches_contain_loop(cond_blocks, else_block.as_deref()) => {
                self.conditional(
                    cond_blocks,
                    else_block.as_deref(),
                    *span,
                    active,
                    normalized,
                )?;
            }
            // Compaction may empty a guarded statement; with only immutable
            // selections to evaluate, the empty conditional has no effect.
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } if cond_blocks
                .iter()
                .all(|block| block.stmts.is_empty() && self.is_immutable(&block.cond))
                && else_block.as_deref().is_none_or(<[_]>::is_empty) => {}
            statement => normalized.push(match active {
                Some(active) => guarded_statement(statement, active.clone()),
                None => statement.clone(),
            }),
        }
        Ok(())
    }

    fn conditional(
        &mut self,
        cond_blocks: &[rumoca_core::StatementBlock],
        else_block: Option<&[rumoca_core::Statement]>,
        span: Span,
        active: Option<&Expression>,
        normalized: &mut Vec<rumoca_core::Statement>,
    ) -> Result<(), ToDaeError> {
        // `remaining` holds while no earlier branch was selected; each later
        // condition is evaluated only then (first-true selection).
        let mut remaining = active.cloned();
        let mut selections = Vec::with_capacity(cond_blocks.len());
        for block in cond_blocks {
            let guard_span = expression_span(&block.cond)?;
            let selected = match &remaining {
                Some(remaining) => and_condition(remaining.clone(), block.cond.clone(), guard_span),
                None => block.cond.clone(),
            };
            let selection = if self.is_immutable(&block.cond) {
                selected
            } else {
                self.capture(selected, guard_span, normalized)?
            };
            let unselected = Expression::Unary {
                op: OpUnary::Not,
                rhs: Box::new(selection.clone()),
                span: guard_span,
            };
            remaining = Some(match remaining {
                Some(remaining) => and_condition(remaining, unselected, guard_span),
                None => unselected,
            });
            selections.push(selection);
        }
        let remaining = remaining.ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "loop prefix conditional",
                "a conditional has no branch",
                span,
            )
        })?;
        let mut remainders = Vec::with_capacity(cond_blocks.len());
        for (block, selection) in cond_blocks.iter().zip(&selections) {
            let remainder = self.hoist_loop_prefix(&block.stmts, selection, normalized)?;
            remainders.push(rumoca_core::StatementBlock {
                cond: selection.clone(),
                stmts: remainder.to_vec(),
            });
        }
        let fallback = match else_block {
            Some(fallback) => Some(
                self.hoist_loop_prefix(fallback, &remaining, normalized)?
                    .to_vec(),
            ),
            None => None,
        }
        .filter(|statements| !statements.is_empty());
        // Every selection stays listed, even with an empty remainder, so the
        // remainder conditional joins each value over the complete selection
        // and the else part stands for exactly the remaining case.
        // Each selection already holds `active`, but the else part runs when
        // none holds, which includes every path where `active` fails; under an
        // active selection the remainder therefore stays guarded by it.
        if fallback.is_some() || remainders.iter().any(|block| !block.stmts.is_empty()) {
            let guarded_else = fallback.is_some();
            let remainder = rumoca_core::Statement::If {
                cond_blocks: remainders,
                else_block: fallback,
                span,
            };
            normalized.push(match active {
                Some(active) if guarded_else => guarded_statement(&remainder, active.clone()),
                _ => remainder,
            });
        }
        Ok(())
    }

    /// Emit `statements` through their last loop under `selection`, and return
    /// the loop-free remainder.
    fn hoist_loop_prefix<'statements>(
        &mut self,
        statements: &'statements [rumoca_core::Statement],
        selection: &Expression,
        normalized: &mut Vec<rumoca_core::Statement>,
    ) -> Result<&'statements [rumoca_core::Statement], ToDaeError> {
        let split = statements
            .iter()
            .rposition(statement_contains_loop)
            .map_or(0, |last| last + 1);
        for statement in &statements[..split] {
            self.statement(statement, Some(selection), normalized)?;
        }
        Ok(&statements[split..])
    }

    /// Define one generated Boolean with `value` at this source position.
    fn capture(
        &mut self,
        value: Expression,
        span: Span,
        normalized: &mut Vec<rumoca_core::Statement>,
    ) -> Result<Expression, ToDaeError> {
        // The definition is identified by its source occurrence; a copied
        // occurrence would alias two different selections.
        if self.guards.iter().any(|guard| guard.span == span) {
            return Err(scope_error(
                "repeats one source occurrence and needs a distinct identity",
                span,
            ));
        }
        let target = rumoca_core::function_branch_guard_name(span.start.0);
        normalized.push(rumoca_core::Statement::Empty { span });
        self.guards.push(GeneratedBooleanDefinition {
            target: target.clone(),
            value,
            span,
        });
        self.immutable.insert(target.clone());
        Ok(Expression::VarRef {
            name: Reference::generated(target.as_str()),
            subscripts: Vec::new(),
            span,
        })
    }

    /// A condition reading only generated Booleans cannot change in any branch.
    fn is_immutable(&self, condition: &Expression) -> bool {
        let mut references = Vec::new();
        condition.collect_var_refs(&mut references);
        references
            .iter()
            .all(|reference| self.immutable.contains(reference))
    }
}

/// Whether some conditional in `statements`, at any depth, owns a loop and so
/// needs the branch-selection captures of this pass.
pub(super) fn contains_loop_conditional(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::For { equations, .. } => contains_loop_conditional(equations),
        rumoca_core::Statement::While { block, .. } => contains_loop_conditional(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => branches_contain_loop(cond_blocks, else_block.as_deref()),
        _ => false,
    })
}

fn branches_contain_loop(
    cond_blocks: &[rumoca_core::StatementBlock],
    else_block: Option<&[rumoca_core::Statement]>,
) -> bool {
    cond_blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .chain(else_block.into_iter().flatten())
        .any(statement_contains_loop)
}

fn statement_contains_loop(statement: &rumoca_core::Statement) -> bool {
    match statement {
        rumoca_core::Statement::For { .. } | rumoca_core::Statement::While { .. } => true,
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => branches_contain_loop(cond_blocks, else_block.as_deref()),
        _ => false,
    }
}

/// Prove the lexical scope of every generated Boolean in `statements`.
///
/// A definition (an `Empty` statement carrying its source span) must come
/// before every read in the sequence that holds it, must not be repeated while
/// it is in scope, and must read only generated Booleans already in scope. A
/// read outside that scope is refused. Consumers rely on this certificate: a
/// definition in a loop body is that loop's per-iteration value, invariant in
/// every nested loop and never carried out of the loop.
pub(super) fn certify_generated_scopes(
    statements: &[rumoca_core::Statement],
    guards: &[GeneratedBooleanDefinition],
) -> Result<(), ToDaeError> {
    let generated = guards
        .iter()
        .map(|guard| guard.target.clone())
        .collect::<HashSet<_>>();
    ScopeCertificate { guards, generated }.sequence(statements, &HashSet::new())
}

struct ScopeCertificate<'guards> {
    guards: &'guards [GeneratedBooleanDefinition],
    generated: HashSet<VarName>,
}

impl ScopeCertificate<'_> {
    fn sequence(
        &self,
        statements: &[rumoca_core::Statement],
        enclosing: &HashSet<VarName>,
    ) -> Result<(), ToDaeError> {
        let mut scope = enclosing.clone();
        for statement in statements {
            self.statement(statement, &mut scope)?;
        }
        Ok(())
    }

    fn statement(
        &self,
        statement: &rumoca_core::Statement,
        scope: &mut HashSet<VarName>,
    ) -> Result<(), ToDaeError> {
        match statement {
            rumoca_core::Statement::Empty { span } => {
                let Some(guard) = self.guards.iter().find(|guard| guard.span == *span) else {
                    return Ok(());
                };
                self.reads_in_scope(&guard.value, scope, *span)?;
                if !scope.insert(guard.target.clone()) {
                    return Err(scope_error("is defined twice in one scope", *span));
                }
                Ok(())
            }
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => {
                for index in indices {
                    self.reads_in_scope(&index.range, scope, *span)?;
                }
                self.sequence(equations, scope)
            }
            rumoca_core::Statement::While { block, span } => {
                self.reads_in_scope(&block.cond, scope, *span)?;
                self.sequence(&block.stmts, scope)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => {
                for block in cond_blocks {
                    self.reads_in_scope(&block.cond, scope, *span)?;
                    self.sequence(&block.stmts, scope)?;
                }
                match else_block {
                    Some(fallback) => self.sequence(fallback, scope),
                    None => Ok(()),
                }
            }
            rumoca_core::Statement::When { blocks, span } => {
                for block in blocks {
                    self.reads_in_scope(&block.cond, scope, *span)?;
                    self.sequence(&block.stmts, scope)?;
                }
                Ok(())
            }
            leaf => {
                let out_of_scope = self
                    .generated
                    .iter()
                    .filter(|target| !scope.contains(*target))
                    .any(|target| statement_reads_target(leaf, target));
                if out_of_scope {
                    let span = required_statement_span(leaf, "generated Boolean read")?;
                    return Err(scope_error("is read outside its defining sequence", span));
                }
                Ok(())
            }
        }
    }

    fn reads_in_scope(
        &self,
        expression: &Expression,
        scope: &HashSet<VarName>,
        span: Span,
    ) -> Result<(), ToDaeError> {
        let mut references = Vec::new();
        expression.collect_var_refs(&mut references);
        if references
            .iter()
            .any(|reference| self.generated.contains(reference) && !scope.contains(reference))
        {
            return Err(scope_error("is read outside its defining sequence", span));
        }
        Ok(())
    }
}

fn scope_error(problem: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "function branch selection",
        format!("a generated branch selection {problem}"),
        span,
    )
}

#[cfg(test)]
mod tests;
