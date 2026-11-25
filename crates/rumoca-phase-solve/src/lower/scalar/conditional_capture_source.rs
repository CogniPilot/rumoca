//! Where one scalar capture of a function-conditional program comes from.
//!
//! A conditional region reads values of the compiler that owns it: a range of
//! a function definition, a register, a capture an enclosing fold passes in,
//! or a scalar of the owner's own capture catalog. The source is a key of the
//! region's capture catalog, so it is a plain value with no behaviour beyond
//! its width and the context it is read in.
use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum FunctionConditionalCaptureSource<'dae> {
    DefinitionRange {
        context: u64,
        definition: dae::FunctionDefinitionId<'dae>,
        count: usize,
    },
    DefinitionRecordFieldRange {
        context: u64,
        definition: dae::FunctionDefinitionId<'dae>,
        field: usize,
        count: usize,
    },
    /// A register of the compiler that owns the conditional, one scalar wide.
    ParentRegister { source: solve::Reg },
    /// A register an enclosing fold update passes in as a capture, resolved in
    /// the compiler that owns the conditional.
    ParentInherited { source: solve::Reg },
    /// The scalar at `ordinal` of the capture catalog of the compiler that owns
    /// the conditional, when that compiler is itself a conditional region: a
    /// region nested in a region reads the enclosing folds through the outer
    /// region, which loads only the scalars the inner one reads.
    ParentCatalog { ordinal: usize },
}

impl<'dae> FunctionConditionalCaptureSource<'dae> {
    pub(super) const fn width(self) -> usize {
        match self {
            Self::DefinitionRange { count, .. }
            | Self::DefinitionRecordFieldRange { count, .. } => count,
            Self::ParentRegister { .. }
            | Self::ParentInherited { .. }
            | Self::ParentCatalog { .. } => 1,
        }
    }

    pub(super) const fn with_context(self, context: u64) -> Self {
        match self {
            Self::DefinitionRange {
                definition, count, ..
            } => Self::DefinitionRange {
                context,
                definition,
                count,
            },
            Self::DefinitionRecordFieldRange {
                definition,
                field,
                count,
                ..
            } => Self::DefinitionRecordFieldRange {
                context,
                definition,
                field,
                count,
            },
            Self::ParentRegister { source } => Self::ParentRegister { source },
            Self::ParentInherited { source } => Self::ParentInherited { source },
            Self::ParentCatalog { ordinal } => Self::ParentCatalog { ordinal },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A source read through the owner keeps its register or ordinal in any
    /// context and is one scalar wide: only a definition range carries a
    /// context of its own.
    #[test]
    fn an_owner_source_is_one_scalar_wide_and_the_same_in_every_context() {
        for source in [
            FunctionConditionalCaptureSource::ParentRegister { source: 3 },
            FunctionConditionalCaptureSource::ParentInherited { source: 4 },
            FunctionConditionalCaptureSource::ParentCatalog { ordinal: 5 },
        ] {
            assert_eq!(source.width(), 1);
            assert_eq!(source.with_context(7), source);
        }
    }
}
