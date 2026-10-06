use rumoca_core::{ForIndex, Span, Statement, StatementRewriter};

use super::{GeneratedDefinition, Guard, NormalizationError};

/// A leaf can never conceal a structured statement or generated definition.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceStatement(Statement);

impl SourceStatement {
    pub fn construct(source: Statement) -> Result<Self, NormalizationError> {
        match source {
            Statement::For { .. }
            | Statement::While { .. }
            | Statement::If { .. }
            | Statement::When { .. } => Err(NormalizationError::OpaqueControlLeaf),
            Statement::Empty { .. }
            | Statement::Assignment { .. }
            | Statement::Return { .. }
            | Statement::Break { .. }
            | Statement::FunctionCall { .. }
            | Statement::Reinit { .. }
            | Statement::Assert { .. } => Ok(Self(source)),
        }
    }

    pub fn source(&self) -> &Statement {
        &self.0
    }

    pub fn rewrite<R: StatementRewriter>(
        &self,
        rewriter: &mut R,
    ) -> Result<Self, NormalizationError> {
        // Invoke the actual leaf override; a control replacement must enter
        // through the explicit normalized-statement inventory instead.
        Self::construct(rewriter.rewrite_statement(&self.0))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Branch<'locals> {
    pub condition: Guard<'locals>,
    pub statements: Vec<NormalizedStatement<'locals>>,
}

/// The single explicit recursive inventory for source algorithm planning.
/// Retaining `When` and exits preserves the strict semantic checker's input;
/// representation alone does not make them legal function statements.
#[derive(Debug, Clone, PartialEq)]
pub enum NormalizedStatement<'locals> {
    Source(SourceStatement),
    Definition(GeneratedDefinition<'locals>),
    For {
        indices: Vec<ForIndex>,
        statements: Vec<Self>,
        span: Span,
    },
    While {
        condition: Guard<'locals>,
        statements: Vec<Self>,
        span: Span,
    },
    If {
        branches: Vec<Branch<'locals>>,
        fallback: Option<Vec<Self>>,
        span: Span,
    },
    When {
        branches: Vec<Branch<'locals>>,
        span: Span,
    },
}

pub(super) fn from_source<'locals>(
    statements: &[Statement],
) -> Result<Vec<NormalizedStatement<'locals>>, NormalizationError> {
    statements.iter().map(one_source).collect()
}

fn one_source<'locals>(
    statement: &Statement,
) -> Result<NormalizedStatement<'locals>, NormalizationError> {
    use NormalizedStatement as N;
    match statement {
        Statement::For {
            indices,
            equations,
            span,
        } => Ok(N::For {
            indices: indices.clone(),
            statements: from_source(equations)?,
            span: *span,
        }),
        Statement::While { block, span } => Ok(N::While {
            condition: Guard::Source(block.cond.clone()),
            statements: from_source(&block.stmts)?,
            span: *span,
        }),
        Statement::If {
            cond_blocks,
            else_block,
            span,
        } => Ok(N::If {
            branches: source_branches(cond_blocks)?,
            fallback: else_block.as_deref().map(from_source).transpose()?,
            span: *span,
        }),
        Statement::When { blocks, span } => Ok(N::When {
            branches: source_branches(blocks)?,
            span: *span,
        }),
        _ => Ok(N::Source(SourceStatement::construct(statement.clone())?)),
    }
}

fn source_branches<'locals>(
    branches: &[rumoca_core::StatementBlock],
) -> Result<Vec<Branch<'locals>>, NormalizationError> {
    branches
        .iter()
        .map(|branch| {
            Ok(Branch {
                condition: Guard::Source(branch.cond.clone()),
                statements: from_source(&branch.stmts)?,
            })
        })
        .collect()
}
