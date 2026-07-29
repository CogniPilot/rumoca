//! Opaque checked Algorithm Code data.
//!
//! Raw syntax trees are accepted only at [`CheckedAlgorithmBlock::construct`],
//! which closes the whole-root language invariants. The projection package
//! adds only target-neutral semantic correlations that are not recoverable
//! from the block itself. Artifact identities, filenames, checksums, XML
//! hierarchy, target types, and representation structure belong to target
//! templates and the generic artifact graph.

use crate::ast::{InterfaceKind, ProtectedKind, ScalarType, TypeRef};
use serde::Serialize;

/// Failure to close a block or its projection metadata.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum PackageError {
    #[error("checked GALEC block is invalid: {0}")]
    Block(String),
    #[error("block and metadata declare different variable counts ({block} != {metadata})")]
    VariableCount { block: usize, metadata: usize },
    #[error("clock variable `{0}` is not a scalar Real constant")]
    InvalidClockReference(String),
}

/// Opaque proof that one Algorithm Code block passed whole-root validation.
#[derive(Debug, Clone, Serialize)]
#[serde(transparent)]
pub struct CheckedAlgorithmBlock(crate::Block);

impl CheckedAlgorithmBlock {
    pub fn construct(block: crate::Block) -> Result<Self, PackageError> {
        if let Err(diagnostics) = crate::validate(&block) {
            return Err(PackageError::Block(
                diagnostics
                    .into_iter()
                    .map(|diagnostic| diagnostic.to_string())
                    .collect::<Vec<_>>()
                    .join("; "),
            ));
        }
        Ok(Self(block))
    }

    #[must_use]
    pub fn block(&self) -> &crate::Block {
        &self.0
    }
}

/// Checked GALEC block plus target-neutral projection correlations.
#[derive(Debug, Clone, Serialize)]
pub struct AlgorithmCodePackage {
    block: CheckedAlgorithmBlock,
    variable_nominals: Vec<Option<f64>>,
    /// One-based ordinal in the block declaration order.
    clock_variable_ordinal: usize,
}

impl AlgorithmCodePackage {
    pub fn construct(
        block: crate::Block,
        variable_nominals: Vec<Option<f64>>,
        clock_variable_name: &str,
    ) -> Result<Self, PackageError> {
        let block = CheckedAlgorithmBlock::construct(block)?;
        let declarations = block_declarations(block.block());
        if declarations.len() != variable_nominals.len() {
            return Err(PackageError::VariableCount {
                block: declarations.len(),
                metadata: variable_nominals.len(),
            });
        }
        let clock_variable_ordinal = clock_ordinal(block.block(), clock_variable_name)
            .ok_or_else(|| PackageError::InvalidClockReference(clock_variable_name.to_owned()))?;
        Ok(Self {
            block,
            variable_nominals,
            clock_variable_ordinal,
        })
    }

    #[must_use]
    pub fn block(&self) -> &crate::Block {
        self.block.block()
    }

    #[must_use]
    pub const fn checked_block(&self) -> &CheckedAlgorithmBlock {
        &self.block
    }

    #[must_use]
    pub fn variable_nominals(&self) -> &[Option<f64>] {
        &self.variable_nominals
    }

    #[must_use]
    pub const fn clock_variable_ordinal(&self) -> usize {
        self.clock_variable_ordinal
    }
}

fn block_declarations(block: &crate::Block) -> Vec<&crate::ast::VariableDeclaration> {
    block
        .interface
        .iter()
        .map(|variable| &variable.decl)
        .chain(block.protected.iter().map(|variable| &variable.decl))
        .collect()
}

fn clock_ordinal(block: &crate::Block, name: &str) -> Option<usize> {
    block
        .interface
        .iter()
        .map(|variable| {
            (
                &variable.decl,
                matches!(variable.kind, InterfaceKind::TunableParameter),
            )
        })
        .chain(block.protected.iter().map(|variable| {
            (
                &variable.decl,
                matches!(variable.kind, ProtectedKind::Constant),
            )
        }))
        .enumerate()
        .find_map(|(index, (declaration, constant))| {
            let scalar_real = matches!(
                declaration.ty,
                TypeRef::Primitive(ScalarType::Real)
            ) && declaration.dimensions.is_empty();
            (constant && scalar_real && declaration.name.lexeme() == name).then_some(index + 1)
        })
}
