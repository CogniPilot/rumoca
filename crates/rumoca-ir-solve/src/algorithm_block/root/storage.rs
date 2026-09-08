//! Construction-issued block-lifetime logical storage.

use rumoca_ir_galec::package::SemanticProvenance;

use crate::SolveStorageClass;

use super::SolveAlgorithmBlockConstructionError;

/// One construction-issued class-local logical scalar run.
///
/// This is target-neutral storage identity, not a C byte offset or physical
/// layout decision. Aggregate declarations retain one compact run plus their
/// exact value type; codegen must not recover storage from declaration order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveLogicalStorageRun {
    storage: SolveAlgorithmBlockStorageClass,
    scalar_base: u64,
    scalar_count: u64,
}

impl SolveLogicalStorageRun {
    #[must_use]
    pub const fn storage(self) -> SolveStorageClass {
        self.storage.storage()
    }

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

/// One block-lifetime storage class admitted by Algorithm Code refinement.
///
/// Method-local storage is deliberately absent: its capacity is owned by one
/// method invocation and is exposed through that method's checked view.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveAlgorithmBlockStorageClass {
    Input,
    Output,
    TunableParameter,
    CalculatedParameter,
    Constant,
    PersistentState,
    PreviousState,
    SignalStatus,
}

impl SolveAlgorithmBlockStorageClass {
    #[must_use]
    pub const fn storage(self) -> SolveStorageClass {
        match self {
            Self::Input => SolveStorageClass::Input,
            Self::Output => SolveStorageClass::Output,
            Self::TunableParameter => SolveStorageClass::TunableParameter,
            Self::CalculatedParameter => SolveStorageClass::CalculatedParameter,
            Self::Constant => SolveStorageClass::Constant,
            Self::PersistentState => SolveStorageClass::PersistentState,
            Self::PreviousState => SolveStorageClass::PreviousState,
            Self::SignalStatus => SolveStorageClass::SignalStatus,
        }
    }

    pub(super) const fn index(self) -> usize {
        match self {
            Self::Input => 0,
            Self::Output => 1,
            Self::TunableParameter => 2,
            Self::CalculatedParameter => 3,
            Self::Constant => 4,
            Self::PersistentState => 5,
            Self::PreviousState => 6,
            Self::SignalStatus => 7,
        }
    }
}

const SOLVE_ALGORITHM_BLOCK_STORAGE_CLASS_COUNT: usize = 8;

/// Complete logical scalar capacity for every block-lifetime storage class.
///
/// ```compile_fail
/// use rumoca_ir_solve::{SolveAlgorithmStorageTotals, SolveStorageClass};
///
/// fn method_local_capacity_is_scoped(totals: &SolveAlgorithmStorageTotals) {
///     totals.scalar_count(SolveStorageClass::MethodLocal);
/// }
/// ```
#[derive(Debug)]
pub struct SolveAlgorithmStorageTotals {
    scalar_counts: [u64; SOLVE_ALGORITHM_BLOCK_STORAGE_CLASS_COUNT],
}

impl SolveAlgorithmStorageTotals {
    #[must_use]
    pub const fn scalar_count(&self, storage: SolveAlgorithmBlockStorageClass) -> u64 {
        self.scalar_counts[storage.index()]
    }
}

pub(super) struct SolveLogicalStorageAllocator {
    pub(super) scalar_counts: [u64; SOLVE_ALGORITHM_BLOCK_STORAGE_CLASS_COUNT],
}

impl SolveLogicalStorageAllocator {
    pub(super) const fn new() -> Self {
        Self {
            scalar_counts: [0; SOLVE_ALGORITHM_BLOCK_STORAGE_CLASS_COUNT],
        }
    }

    pub(super) fn prepare(
        &self,
        storage: SolveAlgorithmBlockStorageClass,
        scalar_count: u64,
        provenance: SemanticProvenance,
    ) -> Result<SolveLogicalStorageRun, SolveAlgorithmBlockConstructionError> {
        let storage_index = storage.index();
        let scalar_base = self.scalar_counts[storage_index];
        scalar_base.checked_add(scalar_count).ok_or(
            SolveAlgorithmBlockConstructionError::LogicalStorageOverflow {
                storage: storage.storage(),
                provenance,
            },
        )?;
        Ok(SolveLogicalStorageRun {
            storage,
            scalar_base,
            scalar_count,
        })
    }

    pub(super) fn commit(&mut self, run: SolveLogicalStorageRun) {
        self.scalar_counts[run.storage.index()] = run.scalar_end();
    }

    pub(super) const fn finish(self) -> SolveAlgorithmStorageTotals {
        SolveAlgorithmStorageTotals {
            scalar_counts: self.scalar_counts,
        }
    }
}
