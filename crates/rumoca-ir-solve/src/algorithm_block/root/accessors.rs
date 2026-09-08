//! Read-only projections of the sealed executable Algorithm Code root.

use super::*;

impl SolveAlgorithmDeclaration {
    #[must_use]
    pub const fn source_class(&self) -> AlgorithmCodeDeclarationClass {
        self.source_class
    }

    #[must_use]
    pub const fn block_index(&self) -> Option<AlgorithmCodeBlockDeclarationIndex> {
        self.block_index
    }

    #[must_use]
    pub const fn evaluated_start(&self) -> AlgorithmCodeEvaluatedStart {
        self.evaluated_start
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }

    #[must_use]
    pub const fn storage(&self) -> SolveStorageClass {
        self.storage
    }

    #[must_use]
    pub const fn access(&self) -> SolveSlotAccess {
        self.access
    }

    #[must_use]
    pub const fn logical_storage(&self) -> SolveLogicalStorageRun {
        self.logical_storage
    }

    #[must_use]
    pub const fn initialization(&self) -> &SolveDeclarationInitialization {
        &self.initialization
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

impl SolveAlgorithmMethod {
    #[must_use]
    pub const fn kind(&self) -> SolveAlgorithmMethodKind {
        self.kind
    }

    /// Exact declarations owned by this method invocation.
    #[must_use]
    pub fn locals(&self) -> &[SolveAlgorithmMethodLocal] {
        &self.locals
    }

    /// Total compact scalar capacity of this method's nonescaping local arena.
    #[must_use]
    pub const fn method_local_scalar_count(&self) -> u64 {
        self.method_local_scalar_count
    }

    #[must_use]
    pub const fn program(&self) -> &TypedProgram {
        &self.program
    }

    #[must_use]
    pub fn actions(&self) -> &[SolveAlgorithmAction] {
        &self.actions
    }

    #[must_use]
    pub fn storage_bindings(&self) -> &[SolveProgramStorageBinding] {
        &self.storage_bindings
    }

    #[must_use]
    pub const fn error_effects(&self) -> SolveAlgorithmErrorEffects {
        self.error_effects
    }

    #[must_use]
    pub const fn abi(&self) -> SolveAlgorithmMethodAbi {
        self.abi
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

impl SolveAlgorithmAction {
    #[must_use]
    pub const fn kind(&self) -> &SolveAlgorithmActionKind {
        &self.kind
    }

    #[must_use]
    pub const fn provenance(&self) -> SemanticProvenance {
        self.provenance
    }

    #[must_use]
    pub const fn correlation(&self) -> &AlgorithmCodeSubjectCorrelation {
        &self.correlation
    }

    #[must_use]
    pub const fn execution(&self) -> SolveAlgorithmActionExecution {
        match self.kind {
            SolveAlgorithmActionKind::StartupInitialize { .. } => {
                SolveAlgorithmActionExecution::StartupOwned
            }
            SolveAlgorithmActionKind::AssignScalarLiteral {
                program_operations, ..
            } => SolveAlgorithmActionExecution::ProgramOwned {
                operations: program_operations,
            },
        }
    }
}

impl SolveProgramOperationRun {
    #[must_use]
    pub const fn first(self) -> u32 {
        self.first
    }

    #[must_use]
    pub const fn count(self) -> u32 {
        self.count
    }

    #[must_use]
    pub const fn end(self) -> u32 {
        self.first + self.count
    }
}

impl SolveProgramStorageBinding {
    #[must_use]
    pub const fn slot(self) -> SolveSlotId {
        self.slot
    }

    #[must_use]
    pub const fn declaration(self) -> u32 {
        self.declaration
    }

    #[must_use]
    pub const fn logical_storage(self) -> SolveLogicalStorageRun {
        self.logical_storage
    }
}
