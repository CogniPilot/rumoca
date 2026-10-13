//! Lexical loop selections issued by the branch-snapshot normalization walk.

use super::*;

pub(in crate::construction::analysis) struct SnapshotLoopSelection {
    span: Span,
    binders: Vec<String>,
    condition: Option<Expression>,
}

impl SnapshotLoopSelection {
    pub(super) fn issue(
        indices: &[rumoca_core::ForIndex],
        span: Span,
        active: Option<&Expression>,
    ) -> Self {
        Self {
            span,
            binders: indices.iter().map(|index| index.ident.clone()).collect(),
            condition: active.cloned(),
        }
    }
}

pub(in crate::construction::analysis) struct BoundLoopSelection<'source> {
    body: &'source [rumoca_core::Statement],
    condition: Option<&'source Expression>,
}

impl BoundLoopSelection<'_> {
    pub(in crate::construction::analysis) fn condition_for(
        &self,
        body: &[rumoca_core::Statement],
    ) -> Option<&Expression> {
        std::ptr::eq(self.body, body)
            .then_some(self.condition)
            .flatten()
    }
}

pub(in crate::construction::analysis) fn bind_loop_selections<'source>(
    statements: &'source [rumoca_core::Statement],
    issued: &'source [SnapshotLoopSelection],
) -> Vec<BoundLoopSelection<'source>> {
    let mut decisions = issued.iter();
    let mut bound = Vec::with_capacity(issued.len());
    bind_sequence(statements, &mut decisions, &mut bound);
    assert!(
        decisions.next().is_none(),
        "every issued snapshot loop is bound"
    );
    bound
}

fn bind_sequence<'source>(
    statements: &'source [rumoca_core::Statement],
    decisions: &mut impl Iterator<Item = &'source SnapshotLoopSelection>,
    bound: &mut Vec<BoundLoopSelection<'source>>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => {
                let issued = decisions
                    .next()
                    .expect("the same normalized loop has an issued decision");
                assert_eq!(issued.span, *span);
                assert!(
                    indices
                        .iter()
                        .map(|index| &index.ident)
                        .eq(issued.binders.iter())
                );
                bound.push(BoundLoopSelection {
                    body: equations,
                    condition: issued.condition.as_ref(),
                });
                bind_sequence(equations, decisions, bound);
            }
            rumoca_core::Statement::While { block, .. } => {
                bind_sequence(&block.stmts, decisions, bound)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    bind_sequence(&block.stmts, decisions, bound);
                }
                if let Some(fallback) = else_block {
                    bind_sequence(fallback, decisions, bound);
                }
            }
            _ => {}
        }
    }
}
