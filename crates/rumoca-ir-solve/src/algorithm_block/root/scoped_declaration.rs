//! Region-owned declarations whose storage cannot escape an invocation.

use rumoca_ir_galec::package::{
    AlgorithmCodeDeclarationClass, AlgorithmCodeSubjectCorrelation, DeclarationId,
    SemanticProvenance,
};

use crate::{SolveSlotAccess, SolveStorageClass, SolveValueType};

use super::{SolveAlgorithmBlockConstructionError, SolveAlgorithmDimension};

/// Closed lifetime carried by a declaration owned by an executable region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveAlgorithmScopedLifetime {
    /// Storage exists only while its owning lifecycle method is executing.
    MethodInvocation,
}

/// One package-correlated declaration owned by exactly one lifecycle method.
///
/// Ownership is represented by containment in [`super::SolveAlgorithmMethod`],
/// not by a detachable numeric owner. Its logical run is therefore meaningful
/// only inside that method's invocation-lifetime storage arena.
#[derive(Debug)]
pub struct SolveAlgorithmMethodLocal {
    value_type: SolveValueType,
    logical_storage: SolveMethodLocalStorageRun,
    dimensions: Box<[SolveAlgorithmDimension]>,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

impl SolveAlgorithmMethodLocal {
    #[must_use]
    pub const fn source_class(&self) -> AlgorithmCodeDeclarationClass {
        AlgorithmCodeDeclarationClass::MethodLocal
    }

    #[must_use]
    pub const fn storage(&self) -> SolveStorageClass {
        SolveStorageClass::MethodLocal
    }

    #[must_use]
    pub const fn access(&self) -> SolveSlotAccess {
        SolveSlotAccess::ReadWrite
    }

    #[must_use]
    pub const fn lifetime(&self) -> SolveAlgorithmScopedLifetime {
        SolveAlgorithmScopedLifetime::MethodInvocation
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }

    #[must_use]
    pub const fn logical_storage(&self) -> SolveMethodLocalStorageRun {
        self.logical_storage
    }

    #[must_use]
    pub fn dimensions(&self) -> &[SolveAlgorithmDimension] {
        &self.dimensions
    }

    #[must_use]
    pub const fn provenance(&self) -> SemanticProvenance {
        self.provenance
    }

    #[must_use]
    pub const fn correlation(&self) -> &AlgorithmCodeSubjectCorrelation {
        &self.correlation
    }
}

/// Compact logical scalar run inside one method invocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveMethodLocalStorageRun {
    scalar_base: u64,
    scalar_count: u64,
}

impl SolveMethodLocalStorageRun {
    #[must_use]
    pub const fn scalar_base(self) -> u64 {
        self.scalar_base
    }

    #[must_use]
    pub const fn scalar_count(self) -> u64 {
        self.scalar_count
    }

    #[must_use]
    pub const fn scalar_end(self) -> u64 {
        self.scalar_base + self.scalar_count
    }
}

pub(super) struct PendingSolveAlgorithmMethodLocal<'id> {
    pub(super) source: DeclarationId<'id>,
    pub(super) value_type: SolveValueType,
    pub(super) logical_storage: SolveMethodLocalStorageRun,
    pub(super) dimensions: Box<[Option<SolveAlgorithmDimension>]>,
    pub(super) provenance: SemanticProvenance,
    pub(super) correlation: AlgorithmCodeSubjectCorrelation,
}

pub(super) struct PendingSolveAlgorithmMethodLocals<'id> {
    locals: Vec<PendingSolveAlgorithmMethodLocal<'id>>,
    scalar_count: u64,
}

impl<'id> PendingSolveAlgorithmMethodLocals<'id> {
    pub(super) const fn empty() -> Self {
        Self {
            locals: Vec::new(),
            scalar_count: 0,
        }
    }

    pub(super) fn prepare_run(
        &self,
        scalar_count: u64,
        provenance: SemanticProvenance,
    ) -> Result<SolveMethodLocalStorageRun, SolveAlgorithmBlockConstructionError> {
        self.scalar_count.checked_add(scalar_count).ok_or(
            SolveAlgorithmBlockConstructionError::MethodLocalStorageOverflow { provenance },
        )?;
        Ok(SolveMethodLocalStorageRun {
            scalar_base: self.scalar_count,
            scalar_count,
        })
    }

    pub(super) fn commit(&mut self, local: PendingSolveAlgorithmMethodLocal<'id>) {
        self.scalar_count = local.logical_storage.scalar_end();
        self.locals.push(local);
    }

    pub(super) fn len(&self) -> usize {
        self.locals.len()
    }

    pub(super) fn sources(&self) -> impl ExactSizeIterator<Item = DeclarationId<'id>> + '_ {
        self.locals.iter().map(|local| local.source)
    }

    pub(super) fn get_mut(
        &mut self,
        index: usize,
    ) -> Option<&mut PendingSolveAlgorithmMethodLocal<'id>> {
        self.locals.get_mut(index)
    }

    pub(super) fn get(&self, index: usize) -> Option<&PendingSolveAlgorithmMethodLocal<'id>> {
        self.locals.get(index)
    }

    pub(super) fn finish(
        self,
    ) -> Result<FinishedSolveAlgorithmMethodLocals, SolveAlgorithmBlockConstructionError> {
        let locals = self
            .locals
            .into_iter()
            .map(finish_method_local)
            .collect::<Result<Vec<_>, _>>()?
            .into_boxed_slice();
        Ok(FinishedSolveAlgorithmMethodLocals {
            locals,
            scalar_count: self.scalar_count,
        })
    }
}

pub(super) struct FinishedSolveAlgorithmMethodLocals {
    pub(super) locals: Box<[SolveAlgorithmMethodLocal]>,
    pub(super) scalar_count: u64,
}

fn finish_method_local(
    local: PendingSolveAlgorithmMethodLocal<'_>,
) -> Result<SolveAlgorithmMethodLocal, SolveAlgorithmBlockConstructionError> {
    let dimensions = local
        .dimensions
        .into_vec()
        .into_iter()
        .enumerate()
        .map(|(axis, dimension)| {
            let axis = u32::try_from(axis).map_err(|_| {
                SolveAlgorithmBlockConstructionError::DeclarationDimensionAxisOverflow {
                    provenance: local.provenance,
                }
            })?;
            dimension.ok_or(
                SolveAlgorithmBlockConstructionError::MissingDeclarationDimension {
                    axis,
                    provenance: local.provenance,
                },
            )
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    Ok(SolveAlgorithmMethodLocal {
        value_type: local.value_type,
        logical_storage: local.logical_storage,
        dimensions,
        provenance: local.provenance,
        correlation: local.correlation,
    })
}
