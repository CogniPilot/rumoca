use rumoca_core::StatementRewriter;

use super::{Branch, NormalizationError, NormalizedStatement as N};

/// Source expression/target/leaf rewriting remains canonical. Structured
/// transformations explicitly implement this trait so lexical scope overrides
/// cannot be silently lost when migrating a source StatementRewriter.
pub trait NormalizedStatementRewriter<'locals>: StatementRewriter + Sized {
    fn rewrite_normalized_statements(
        &mut self,
        statements: &[N<'locals>],
    ) -> Result<Vec<N<'locals>>, NormalizationError> {
        statements
            .iter()
            .map(|statement| self.rewrite_normalized_statement(statement))
            .collect()
    }

    fn rewrite_normalized_statement(
        &mut self,
        statement: &N<'locals>,
    ) -> Result<N<'locals>, NormalizationError> {
        self.walk_normalized_statement(statement)
    }

    fn walk_normalized_statement(
        &mut self,
        statement: &N<'locals>,
    ) -> Result<N<'locals>, NormalizationError> {
        Ok(match statement {
            N::Source(source) => N::Source(source.rewrite(self)?),
            N::Definition(definition) => N::Definition(definition.rewrite(self)),
            N::For {
                indices,
                statements,
                span,
            } => N::For {
                indices: self.rewrite_for_indices(indices),
                statements: self.rewrite_normalized_statements(statements)?,
                span: *span,
            },
            N::While {
                condition,
                statements,
                span,
            } => N::While {
                condition: condition.rewrite(self),
                statements: self.rewrite_normalized_statements(statements)?,
                span: *span,
            },
            N::If {
                branches,
                fallback,
                span,
            } => N::If {
                branches: self.rewrite_normalized_branches(branches)?,
                fallback: fallback
                    .as_deref()
                    .map(|body| self.rewrite_normalized_statements(body))
                    .transpose()?,
                span: *span,
            },
            N::When { branches, span } => N::When {
                branches: self.rewrite_normalized_branches(branches)?,
                span: *span,
            },
        })
    }

    fn rewrite_normalized_branches(
        &mut self,
        branches: &[Branch<'locals>],
    ) -> Result<Vec<Branch<'locals>>, NormalizationError> {
        branches
            .iter()
            .map(|branch| {
                Ok(Branch {
                    condition: branch.condition.rewrite(self),
                    statements: self.rewrite_normalized_statements(&branch.statements)?,
                })
            })
            .collect()
    }
}

impl<'locals> N<'locals> {
    pub fn rewrite<R: NormalizedStatementRewriter<'locals>>(
        &self,
        rewriter: &mut R,
    ) -> Result<Self, NormalizationError> {
        rewriter.rewrite_normalized_statement(self)
    }
}
