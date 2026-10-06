//! Structured function planning before DAE values and transitions are issued.
//!
//! Source leaves retain their resolved references. Introduced Boolean reads
//! and definitions use the independent, phase-branded Core local catalog.
//! This inventory does not claim a type, definedness or executable certificate;
//! those remain obligations of the ToDae function owner.

mod dataflow;
mod guard;
mod rewrite;
mod snapshot;
mod statement;
mod traversal;

pub use guard::{GeneratedDefinition, Guard, GuardVisitor};
pub use rewrite::NormalizedStatementRewriter;
pub use statement::{Branch, NormalizedStatement, SourceStatement};
pub use traversal::StatementVisitor;

use rumoca_core::{Function, GeneratedFunctionLocalCatalog, GeneratedLocalError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NormalizationError {
    Local(GeneratedLocalError),
    OpaqueControlLeaf,
    UndefinedGeneratedLocal { span: rumoca_core::Span },
    DuplicateGeneratedDefinition { span: rumoca_core::Span },
}

impl From<GeneratedLocalError> for NormalizationError {
    fn from(error: GeneratedLocalError) -> Self {
        Self::Local(error)
    }
}

/// Preserve every lexical statement, including exits and unsupported source
/// constructs, for the phase's semantic checker. No admission happens here.
/// Snapshot a conditional containing loops at its original execution site;
/// the selected branch cannot change as its body updates a source predicate.
pub fn normalize<'locals>(
    function: &Function,
    catalog: &mut GeneratedFunctionLocalCatalog<'locals, '_>,
) -> Result<Vec<NormalizedStatement<'locals>>, NormalizationError> {
    let mut locals = catalog.function(function)?;
    let statements = statement::from_source(&function.body)?;
    let statements = snapshot::snapshot_sequence(statements, &mut locals)?;
    dataflow::check(&statements)?;
    Ok(statements)
}

#[cfg(test)]
mod tests;
