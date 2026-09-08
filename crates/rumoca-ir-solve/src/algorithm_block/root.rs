//! Checked executable refinement of one Algorithm Code package.

mod accessors;
mod error;
mod scoped_declaration;
mod storage;
mod unsupported;

pub use error::SolveAlgorithmBlockConstructionError;
pub use scoped_declaration::{
    SolveAlgorithmMethodLocal, SolveAlgorithmScopedLifetime, SolveMethodLocalStorageRun,
};
pub use storage::{
    SolveAlgorithmBlockStorageClass, SolveAlgorithmStorageTotals, SolveLogicalStorageRun,
};
pub use unsupported::UnsupportedTensorInitializationPlan;

use storage::SolveLogicalStorageAllocator;

use std::marker::PhantomData;

use indexmap::{IndexMap, IndexSet, map::Entry};
use rumoca_ir_galec::TracedAlgorithmCodeProduct;
use rumoca_ir_galec::package::{
    AlgorithmCodeBlockDeclarationIndex, AlgorithmCodeChildRole, AlgorithmCodeDeclarationClass,
    AlgorithmCodeEvaluatedLiteral, AlgorithmCodeEvaluatedStart, AlgorithmCodeInspection,
    AlgorithmCodePackage, AlgorithmCodeReferenceTarget, AlgorithmCodeStatementKind,
    AlgorithmCodeSubject, AlgorithmCodeSubjectCorrelation, AlgorithmCodeSubjectId,
    AlgorithmCodeSubjectParent, DeclarationId, DeclarationSubject, ExpressionId, ExpressionSubject,
    LifecycleMethodId, LifecycleMethodSubject, ReferenceSubject, SemanticProvenance, StatementId,
    StatementSubject,
};

use crate::{
    ProgramSlot, SolveArithmeticProfile, SolveProgramConstructionError, SolveSlotAccess,
    SolveSlotId, SolveStorageClass, SolveValue, SolveValueType, TypedProgram,
};

use super::CallTransferPlanSet;
use super::lowering::{
    evaluated_start_matches_expression, has_external_initialization_authority, method_index,
    program_span, solve_arithmetic_profile, solve_declaration_start, solve_declaration_storage,
    solve_scalar_literal, solve_uniform_tensor_literal, solve_value_type,
};
use super::source_navigation::{direct_method_action_count, direct_statement_owner};
use scoped_declaration::{PendingSolveAlgorithmMethodLocal, PendingSolveAlgorithmMethodLocals};

/// The three lifecycle entry points every executable Algorithm Code block owns.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveAlgorithmMethodKind {
    Startup,
    Recalibrate,
    DoStep,
}

impl From<rumoca_ir_galec::BlockMethodKind> for SolveAlgorithmMethodKind {
    fn from(value: rumoca_ir_galec::BlockMethodKind) -> Self {
        match value {
            rumoca_ir_galec::BlockMethodKind::Startup => Self::Startup,
            rumoca_ir_galec::BlockMethodKind::Recalibrate => Self::Recalibrate,
            rumoca_ir_galec::BlockMethodKind::DoStep => Self::DoStep,
        }
    }
}

/// One typed lifecycle program paired with its exact source responsibility.
#[derive(Debug)]
pub struct SolveAlgorithmMethod {
    kind: SolveAlgorithmMethodKind,
    locals: Box<[SolveAlgorithmMethodLocal]>,
    method_local_scalar_count: u64,
    program: TypedProgram,
    actions: Box<[SolveAlgorithmAction]>,
    storage_bindings: Box<[SolveProgramStorageBinding]>,
    error_effects: SolveAlgorithmErrorEffects,
    abi: SolveAlgorithmMethodAbi,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

/// Complete initialization authority for one block declaration.
#[derive(Debug)]
pub enum SolveDeclarationInitialization {
    /// The embedding environment defines this read-only value before a method
    /// can observe it.
    External {
        suggested_value: SolveDeclarationStartValue,
        provenance: SemanticProvenance,
        correlation: AlgorithmCodeSubjectCorrelation,
    },
    /// One exact package-correlated initialization is materialized by Startup.
    Internal {
        value: SolveDeclarationStartValue,
        provenance: SemanticProvenance,
        correlation: AlgorithmCodeSubjectCorrelation,
    },
}

/// Compact initialization value owned by one declaration and, for an
/// internally initialized declaration, its exact Startup action.
#[derive(Debug, PartialEq, Eq)]
pub enum SolveDeclarationStartValue {
    Scalar(SolveValue),
    UniformTensorFill(SolveValue),
}

impl SolveDeclarationStartValue {
    #[must_use]
    pub const fn scalar(&self) -> &SolveValue {
        match self {
            Self::Scalar(value) | Self::UniformTensorFill(value) => value,
        }
    }
}

/// One statement-ordered executable lifecycle action.
#[derive(Debug)]
pub struct SolveAlgorithmAction {
    kind: SolveAlgorithmActionKind,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

/// Closed action subset currently admitted by Algorithm Code refinement.
#[derive(Debug)]
pub enum SolveAlgorithmActionKind {
    /// The exact GALEC Startup occurrence that materializes one declaration's
    /// package-catalog start. This is not general write authority.
    StartupInitialize {
        declaration: u32,
        value: SolveDeclarationStartValue,
        target_provenance: SemanticProvenance,
        target_correlation: AlgorithmCodeSubjectCorrelation,
        value_provenance: SemanticProvenance,
        value_correlation: AlgorithmCodeSubjectCorrelation,
    },
    AssignScalarLiteral {
        declaration: u32,
        value: SolveValue,
        program_operations: SolveProgramOperationRun,
        target_provenance: SemanticProvenance,
        target_correlation: AlgorithmCodeSubjectCorrelation,
        value_provenance: SemanticProvenance,
        value_correlation: AlgorithmCodeSubjectCorrelation,
    },
}

/// Exact contiguous typed-program operation run owned by one lifecycle action.
///
/// The fields and constructor are private. A run can therefore enter an
/// exposed Solve root only while the typed program is being constructed from
/// the same action sequence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveProgramOperationRun {
    first: u32,
    count: u32,
}

/// Closed execution disposition for one retained lifecycle action.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveAlgorithmActionExecution {
    StartupOwned,
    ProgramOwned {
        operations: SolveProgramOperationRun,
    },
}

/// Exact binding from one typed-program slot to block-owned logical storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveProgramStorageBinding {
    slot: SolveSlotId,
    declaration: u32,
    logical_storage: SolveLogicalStorageRun,
}

/// Checked interaction with the Algorithm Code error-signal word.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveAlgorithmErrorEffects {
    /// Method entry resets the word once; admitted actions neither read nor
    /// modify it and no error signal escapes.
    ResetOnly,
}

/// Target-neutral lifecycle calling convention.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveAlgorithmMethodAbi {
    /// The method receives its block storage owner, has no value arguments or
    /// results, and cannot fail in the admitted action subset.
    ParameterFreeInfallible,
}

/// One package-correlated block declaration in executable storage.
#[derive(Debug)]
pub struct SolveAlgorithmDeclaration {
    source_class: AlgorithmCodeDeclarationClass,
    block_index: Option<AlgorithmCodeBlockDeclarationIndex>,
    evaluated_start: AlgorithmCodeEvaluatedStart,
    value_type: SolveValueType,
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    logical_storage: SolveLogicalStorageRun,
    initialization: SolveDeclarationInitialization,
    dimensions: Box<[SolveAlgorithmDimension]>,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

/// One compact tensor axis correlated to its exact package dimension subject.
#[derive(Debug)]
pub struct SolveAlgorithmDimension {
    extent: u32,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

impl SolveAlgorithmDimension {
    #[must_use]
    pub const fn extent(&self) -> u32 {
        self.extent
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

struct PendingSolveAlgorithmDeclaration {
    source_class: AlgorithmCodeDeclarationClass,
    block_index: Option<AlgorithmCodeBlockDeclarationIndex>,
    evaluated_start: AlgorithmCodeEvaluatedStart,
    value_type: SolveValueType,
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    logical_storage: SolveLogicalStorageRun,
    initialization: Option<SolveDeclarationInitialization>,
    dimensions: Box<[Option<SolveAlgorithmDimension>]>,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

struct PendingSolveAlgorithmMethod<'id> {
    source: LifecycleMethodId<'id>,
    kind: SolveAlgorithmMethodKind,
    locals: PendingSolveAlgorithmMethodLocals<'id>,
    actions: Vec<Option<PendingSolveAlgorithmAction>>,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

struct PendingSolveAlgorithmAction {
    target: Option<PendingAssignmentTarget>,
    value: Option<PendingAssignmentValue>,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

struct PendingAssignmentTarget {
    declaration: usize,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

struct PendingAssignmentValue {
    value: SolveDeclarationStartValue,
    source_literal: AlgorithmCodeEvaluatedLiteral,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

#[derive(Clone, Copy)]
enum PendingDeclarationLocation<'id> {
    Block(usize),
    MethodLocal {
        owner: LifecycleMethodId<'id>,
        index: usize,
    },
}

struct PendingFinishedSolveAlgorithmAction {
    kind: PendingFinishedSolveAlgorithmActionKind,
    provenance: SemanticProvenance,
    correlation: AlgorithmCodeSubjectCorrelation,
}

enum PendingFinishedSolveAlgorithmActionKind {
    StartupInitialize {
        declaration: u32,
        value: SolveDeclarationStartValue,
        target_provenance: SemanticProvenance,
        target_correlation: AlgorithmCodeSubjectCorrelation,
        value_provenance: SemanticProvenance,
        value_correlation: AlgorithmCodeSubjectCorrelation,
    },
    AssignScalarLiteral {
        declaration: u32,
        value: SolveValue,
        target_provenance: SemanticProvenance,
        target_correlation: AlgorithmCodeSubjectCorrelation,
        value_provenance: SemanticProvenance,
        value_correlation: AlgorithmCodeSubjectCorrelation,
    },
}

struct ActionWithExecution {
    action: PendingFinishedSolveAlgorithmAction,
    execution: Option<SolveAlgorithmActionExecution>,
}

/// The non-cloneable executable owner produced by Algorithm Code refinement.
///
/// This root contains executable Solve semantics only. Its owning
/// [`SolveAlgorithmProduct`] retains the checked source package without giving
/// this block a path back to Algorithm Code.
///
/// ```compile_fail
/// use rumoca_ir_solve::SolveAlgorithmBlock;
///
/// fn duplicate(block: &SolveAlgorithmBlock) -> SolveAlgorithmBlock {
///     block.clone()
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_ir_solve::SolveAlgorithmBlock;
///
/// let _ = SolveAlgorithmBlock::default();
/// ```
///
/// ```compile_fail
/// use rumoca_ir_solve::SolveAlgorithmBlock;
///
/// fn inspect_algorithm_code(block: &SolveAlgorithmBlock) {
///     let _ = block.algorithm_code();
/// }
/// ```
#[derive(Debug)]
pub struct SolveAlgorithmBlock {
    arithmetic: SolveArithmeticProfile,
    declarations: Box<[SolveAlgorithmDeclaration]>,
    storage_totals: SolveAlgorithmStorageTotals,
    methods: [SolveAlgorithmMethod; 3],
    call_transfers: CallTransferPlanSet,
}

type FinishedAlgorithmBlockParts = (
    Box<[SolveAlgorithmDeclaration]>,
    SolveAlgorithmStorageTotals,
    [SolveAlgorithmMethod; 3],
    CallTransferPlanSet,
);

type BuiltMethodProgram = (
    TypedProgram,
    Box<[SolveAlgorithmAction]>,
    Box<[SolveProgramStorageBinding]>,
);

/// One non-cloneable co-emission owner with representation-separated borrows.
///
/// Algorithm Code rendering borrows [`Self::algorithm_code`]. Production
/// execution and rendering borrow [`Self::solve_algorithm_block`]. The sealed
/// Solve block contains no path back to Algorithm Code, and neither root can be
/// extracted from this product by value.
///
/// ```compile_fail
/// use rumoca_ir_solve::SolveAlgorithmProduct;
///
/// fn duplicate<'inv>(product: &SolveAlgorithmProduct<'inv>) -> SolveAlgorithmProduct<'inv> {
///     product.clone()
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_ir_solve::SolveAlgorithmProduct;
///
/// let _ = SolveAlgorithmProduct::<'static>::default();
/// ```
///
/// ```compile_fail
/// use rumoca_ir_solve::{SolveAlgorithmBlock, SolveAlgorithmProduct};
///
/// fn detach_block<'inv>(product: SolveAlgorithmProduct<'inv>) -> SolveAlgorithmBlock {
///     product.block
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_ir_galec::package::AlgorithmCodePackage;
/// use rumoca_ir_solve::SolveAlgorithmProduct;
///
/// fn detach_package<'inv>(product: SolveAlgorithmProduct<'inv>) -> AlgorithmCodePackage {
///     product.algorithm_code
/// }
/// ```
#[derive(Debug)]
pub struct SolveAlgorithmProduct<'inv> {
    algorithm_code: TracedAlgorithmCodeProduct<'inv>,
    block: SolveAlgorithmBlock,
}

impl<'inv> SolveAlgorithmProduct<'inv> {
    #[must_use]
    pub const fn algorithm_code(&self) -> &AlgorithmCodePackage {
        self.algorithm_code.package()
    }

    #[must_use]
    pub const fn traced_algorithm_code(&self) -> &TracedAlgorithmCodeProduct<'inv> {
        &self.algorithm_code
    }

    #[must_use]
    pub const fn solve_algorithm_block(&self) -> &SolveAlgorithmBlock {
        &self.block
    }
}

impl SolveAlgorithmBlock {
    /// Construct one sealed executable refinement under a fresh package brand.
    ///
    /// The callback receives the only branded inspection and a builder tied to
    /// that brand. Root close rejects missing lifecycle methods, unmapped
    /// semantic subjects and incomplete call-transfer coverage before this
    /// type can be returned. Executable arithmetic is derived from the
    /// retained package profile; no second selection enters this constructor.
    pub fn construct<'inv, E>(
        algorithm_code: TracedAlgorithmCodeProduct<'inv>,
        build: impl for<'id> FnOnce(
            &AlgorithmCodeInspection<'_, 'id>,
            &mut SolveAlgorithmBlockBuilder<'id>,
        ) -> Result<(), E>,
    ) -> Result<SolveAlgorithmProduct<'inv>, E>
    where
        E: From<SolveAlgorithmBlockConstructionError>,
    {
        let package = algorithm_code.package();
        let arithmetic = solve_arithmetic_profile(package.arithmetic_profile());

        let (declarations, storage_totals, methods, call_transfers) =
            package.inspect(|inspection| {
                let expected_subjects = inspection
                    .subjects()
                    .map(AlgorithmCodeSubject::id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let expected_correlations = expected_subjects
                    .iter()
                    .copied()
                    .map(|subject| Some(inspection.own_correlation(subject)))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let mut builder = SolveAlgorithmBlockBuilder {
                    arithmetic,
                    expected_subjects,
                    expected_correlations,
                    next_subject: 0,
                    declarations: Vec::new(),
                    declaration_indices: IndexMap::new(),
                    logical_storage: SolveLogicalStorageAllocator::new(),
                    pending_method_locals: IndexMap::new(),
                    method_indices: IndexMap::new(),
                    methods: [None, None, None],
                    statement_indices: IndexMap::new(),
                    aggregate_literal_roots: IndexSet::new(),
                    call_transfers: None,
                    _brand: PhantomData,
                };
                build(&inspection, &mut builder)?;
                builder.finish().map_err(E::from)
            })?;

        Ok(SolveAlgorithmProduct {
            algorithm_code,
            block: Self {
                arithmetic,
                declarations,
                storage_totals,
                methods,
                call_transfers,
            },
        })
    }

    #[must_use]
    pub const fn arithmetic(&self) -> SolveArithmeticProfile {
        self.arithmetic
    }

    #[must_use]
    pub fn declarations(&self) -> &[SolveAlgorithmDeclaration] {
        &self.declarations
    }

    #[must_use]
    pub const fn storage_totals(&self) -> &SolveAlgorithmStorageTotals {
        &self.storage_totals
    }

    #[must_use]
    pub fn method(&self, kind: SolveAlgorithmMethodKind) -> &SolveAlgorithmMethod {
        &self.methods[method_index(kind)]
    }

    #[must_use]
    pub const fn call_transfers(&self) -> &CallTransferPlanSet {
        &self.call_transfers
    }
}

/// Construction authority tied invariantly to one package inspection brand.
pub struct SolveAlgorithmBlockBuilder<'id> {
    arithmetic: SolveArithmeticProfile,
    expected_subjects: Box<[AlgorithmCodeSubjectId<'id>]>,
    expected_correlations: Box<[Option<AlgorithmCodeSubjectCorrelation>]>,
    next_subject: usize,
    declarations: Vec<PendingSolveAlgorithmDeclaration>,
    declaration_indices: IndexMap<DeclarationId<'id>, PendingDeclarationLocation<'id>>,
    logical_storage: SolveLogicalStorageAllocator,
    pending_method_locals: IndexMap<LifecycleMethodId<'id>, PendingSolveAlgorithmMethodLocals<'id>>,
    method_indices: IndexMap<LifecycleMethodId<'id>, usize>,
    methods: [Option<PendingSolveAlgorithmMethod<'id>>; 3],
    statement_indices: IndexMap<StatementId<'id>, (usize, usize)>,
    aggregate_literal_roots: IndexSet<ExpressionId<'id>>,
    call_transfers: Option<CallTransferPlanSet>,
    _brand: PhantomData<fn(&'id mut ()) -> &'id mut ()>,
}

impl<'id> SolveAlgorithmBlockBuilder<'id> {
    /// Consume one exact package declaration into executable storage.
    pub fn issue_declaration(
        &mut self,
        inspection: &AlgorithmCodeInspection<'_, 'id>,
        subject: DeclarationSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        if subject.class() == AlgorithmCodeDeclarationClass::MethodLocal {
            return self.issue_method_local(inspection, subject);
        }
        self.issue_block_declaration(subject)
    }

    fn issue_block_declaration(
        &mut self,
        subject: DeclarationSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let (storage, access) = solve_declaration_storage(subject.class(), subject.provenance())?;
        let evaluated_start = subject.evaluated_start().ok_or(
            SolveAlgorithmBlockConstructionError::MissingEvaluatedDeclarationStart {
                provenance: subject.provenance(),
            },
        )?;
        if matches!(
            evaluated_start,
            AlgorithmCodeEvaluatedStart::Missing
                | AlgorithmCodeEvaluatedStart::UnsupportedScalarExpression
        ) {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedEvaluatedDeclarationStart {
                    provenance: subject.provenance(),
                },
            );
        }
        let unsupported_tensor_plan = match evaluated_start {
            AlgorithmCodeEvaluatedStart::UnsupportedNonUniformTensor => {
                Some(UnsupportedTensorInitializationPlan::NonUniformLiteral)
            }
            AlgorithmCodeEvaluatedStart::UnsupportedSymbolicTensor => {
                Some(UnsupportedTensorInitializationPlan::Symbolic)
            }
            AlgorithmCodeEvaluatedStart::Missing
            | AlgorithmCodeEvaluatedStart::Scalar(_)
            | AlgorithmCodeEvaluatedStart::UniformTensorFill(_)
            | AlgorithmCodeEvaluatedStart::UnsupportedScalarExpression => None,
        };
        if let Some(plan) = unsupported_tensor_plan {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedTensorInitializationPlan {
                    plan,
                    provenance: subject.provenance(),
                },
            );
        }
        let source = subject.id();
        let value_type = solve_value_type(subject.value(), self.arithmetic).map_err(|source| {
            SolveAlgorithmBlockConstructionError::ValueType {
                source,
                provenance: subject.provenance(),
            }
        })?;
        let scalar_count = u64::from(value_type.scalar_count());
        let logical_storage =
            self.logical_storage
                .prepare(storage, scalar_count, subject.provenance())?;
        let dimensions = value_type
            .dimensions()
            .iter()
            .map(|_| None)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let declaration_index = self.declarations.len();
        let Entry::Vacant(index_entry) = self.declaration_indices.entry(source) else {
            return Err(
                SolveAlgorithmBlockConstructionError::DuplicateDeclarationSubject {
                    provenance: subject.provenance(),
                },
            );
        };
        let correlation = issue_subject_from_parts(
            &self.expected_subjects,
            &mut self.expected_correlations,
            &mut self.next_subject,
            AlgorithmCodeSubjectId::Declaration(source),
        )?;
        self.logical_storage.commit(logical_storage);
        index_entry.insert(PendingDeclarationLocation::Block(declaration_index));
        self.declarations.push(PendingSolveAlgorithmDeclaration {
            source_class: subject.class(),
            block_index: subject.block_index(),
            evaluated_start,
            value_type,
            storage: storage.storage(),
            access,
            logical_storage,
            initialization: None,
            dimensions,
            provenance: subject.provenance(),
            correlation,
        });
        Ok(())
    }

    fn issue_method_local(
        &mut self,
        inspection: &AlgorithmCodeInspection<'_, 'id>,
        subject: DeclarationSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let AlgorithmCodeSubjectParent::Subject {
            owner: AlgorithmCodeSubjectId::LifecycleMethod(owner),
            role: AlgorithmCodeChildRole::MethodLocal(index),
        } = inspection.parent(AlgorithmCodeSubjectId::Declaration(subject.id()))
        else {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedMethodLocalOwner {
                    provenance: subject.provenance(),
                },
            );
        };
        let index = usize::try_from(index).map_err(|_| {
            SolveAlgorithmBlockConstructionError::MethodLocalIndexOverflow {
                provenance: subject.provenance(),
            }
        })?;
        let expected_index = match self.pending_method_locals.get(&owner) {
            Some(locals) => locals.len(),
            None => 0,
        };
        if index != expected_index {
            return Err(
                SolveAlgorithmBlockConstructionError::MethodLocalOrderMismatch {
                    provenance: subject.provenance(),
                },
            );
        }
        let Entry::Vacant(index_entry) = self.declaration_indices.entry(subject.id()) else {
            return Err(
                SolveAlgorithmBlockConstructionError::DuplicateDeclarationSubject {
                    provenance: subject.provenance(),
                },
            );
        };
        let value_type = solve_value_type(subject.value(), self.arithmetic).map_err(|source| {
            SolveAlgorithmBlockConstructionError::ValueType {
                source,
                provenance: subject.provenance(),
            }
        })?;
        let scalar_count = u64::from(value_type.scalar_count());
        let logical_storage = match self.pending_method_locals.get(&owner) {
            Some(locals) => locals.prepare_run(scalar_count, subject.provenance())?,
            None => PendingSolveAlgorithmMethodLocals::empty()
                .prepare_run(scalar_count, subject.provenance())?,
        };
        let dimensions = value_type
            .dimensions()
            .iter()
            .map(|_| None)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let correlation = issue_subject_from_parts(
            &self.expected_subjects,
            &mut self.expected_correlations,
            &mut self.next_subject,
            AlgorithmCodeSubjectId::Declaration(subject.id()),
        )?;
        index_entry.insert(PendingDeclarationLocation::MethodLocal { owner, index });
        let local = PendingSolveAlgorithmMethodLocal {
            source: subject.id(),
            value_type,
            logical_storage,
            dimensions,
            provenance: subject.provenance(),
            correlation,
        };
        match self.pending_method_locals.entry(owner) {
            Entry::Occupied(mut entry) => entry.get_mut().commit(local),
            Entry::Vacant(entry) => {
                let mut locals = PendingSolveAlgorithmMethodLocals::empty();
                locals.commit(local);
                entry.insert(locals);
            }
        }
        Ok(())
    }

    /// Correlate one exact declaration-dimension subject to its compact axis.
    pub fn issue_declaration_dimension(
        &mut self,
        inspection: &AlgorithmCodeInspection<'_, 'id>,
        subject: ExpressionSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let AlgorithmCodeSubjectParent::Subject {
            owner: AlgorithmCodeSubjectId::Declaration(owner),
            role: AlgorithmCodeChildRole::DeclarationDimension(axis),
        } = inspection.parent(AlgorithmCodeSubjectId::Expression(subject.id()))
        else {
            return Err(
                SolveAlgorithmBlockConstructionError::ExpressionIsNotDeclarationDimension {
                    provenance: subject.provenance(),
                },
            );
        };
        let declaration_location = self.declaration_indices.get(&owner).copied().ok_or(
            SolveAlgorithmBlockConstructionError::ForeignDeclarationDimension {
                provenance: subject.provenance(),
            },
        )?;
        let axis_u32 = axis;
        let axis = usize::try_from(axis_u32).map_err(|_| {
            SolveAlgorithmBlockConstructionError::DeclarationDimensionOutOfBounds {
                axis: axis_u32,
                provenance: subject.provenance(),
            }
        })?;
        let (extent, already_issued) =
            self.declaration_dimension_state(declaration_location, axis, axis_u32, subject)?;
        if already_issued {
            return Err(
                SolveAlgorithmBlockConstructionError::DuplicateDeclarationDimension {
                    axis: axis_u32,
                    provenance: subject.provenance(),
                },
            );
        }
        let correlation = self.issue_subject(AlgorithmCodeSubjectId::Expression(subject.id()))?;
        let dimension = self
            .declaration_dimension_mut(declaration_location, axis)
            .ok_or(
                SolveAlgorithmBlockConstructionError::ForeignDeclarationDimension {
                    provenance: subject.provenance(),
                },
            )?;
        *dimension = Some(SolveAlgorithmDimension {
            extent,
            provenance: subject.provenance(),
            correlation,
        });
        Ok(())
    }

    fn declaration_dimension_state(
        &self,
        declaration_location: PendingDeclarationLocation<'id>,
        axis: usize,
        axis_u32: u32,
        subject: ExpressionSubject<'_, 'id>,
    ) -> Result<(u32, bool), SolveAlgorithmBlockConstructionError> {
        let state = match declaration_location {
            PendingDeclarationLocation::Block(declaration) => {
                let declaration = self.declarations.get(declaration).ok_or(
                    SolveAlgorithmBlockConstructionError::ForeignDeclarationDimension {
                        provenance: subject.provenance(),
                    },
                )?;
                let extent = declaration
                    .value_type
                    .dimensions()
                    .get(axis)
                    .copied()
                    .ok_or(
                        SolveAlgorithmBlockConstructionError::DeclarationDimensionOutOfBounds {
                            axis: axis_u32,
                            provenance: subject.provenance(),
                        },
                    )?;
                let dimension = declaration.dimensions.get(axis).ok_or(
                    SolveAlgorithmBlockConstructionError::DeclarationDimensionOutOfBounds {
                        axis: axis_u32,
                        provenance: subject.provenance(),
                    },
                )?;
                (extent, dimension.is_some())
            }
            PendingDeclarationLocation::MethodLocal { owner, index } => {
                let method_index = self.method_indices.get(&owner).copied().ok_or(
                    SolveAlgorithmBlockConstructionError::MethodLocalBeforeLifecycleMethod {
                        provenance: subject.provenance(),
                    },
                )?;
                let local = self.methods[method_index]
                    .as_ref()
                    .and_then(|method| method.locals.get(index))
                    .ok_or(
                        SolveAlgorithmBlockConstructionError::ForeignDeclarationDimension {
                            provenance: subject.provenance(),
                        },
                    )?;
                let extent = local.value_type.dimensions().get(axis).copied().ok_or(
                    SolveAlgorithmBlockConstructionError::DeclarationDimensionOutOfBounds {
                        axis: axis_u32,
                        provenance: subject.provenance(),
                    },
                )?;
                let dimension = local.dimensions.get(axis).ok_or(
                    SolveAlgorithmBlockConstructionError::DeclarationDimensionOutOfBounds {
                        axis: axis_u32,
                        provenance: subject.provenance(),
                    },
                )?;
                (extent, dimension.is_some())
            }
        };
        Ok(state)
    }

    fn declaration_dimension_mut(
        &mut self,
        declaration_location: PendingDeclarationLocation<'id>,
        axis: usize,
    ) -> Option<&mut Option<SolveAlgorithmDimension>> {
        match declaration_location {
            PendingDeclarationLocation::Block(declaration) => self
                .declarations
                .get_mut(declaration)
                .and_then(|declaration| declaration.dimensions.get_mut(axis)),
            PendingDeclarationLocation::MethodLocal { owner, index } => self
                .method_indices
                .get(&owner)
                .copied()
                .and_then(|method| self.methods[method].as_mut())
                .and_then(|method| method.locals.get_mut(index))
                .and_then(|local| local.dimensions.get_mut(axis)),
        }
    }

    /// Begin one exact package-issued lifecycle method and reserve its complete
    /// direct action sequence.
    pub fn issue_lifecycle_method(
        &mut self,
        inspection: &AlgorithmCodeInspection<'_, 'id>,
        subject: LifecycleMethodSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let kind = subject.kind().into();
        if !subject.signals().is_empty() {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedLifecycleSource {
                    method: kind,
                    provenance: subject.provenance(),
                },
            );
        }
        let method_index = method_index(kind);
        if self.methods[method_index].is_some() {
            return Err(
                SolveAlgorithmBlockConstructionError::DuplicateLifecycleMethod { method: kind },
            );
        }
        let expected_locals = match self.pending_method_locals.get(&subject.id()) {
            Some(locals) => locals.sources().collect::<Vec<_>>(),
            None => Vec::new(),
        };
        let action_count = direct_method_action_count(inspection, subject, &expected_locals)?;
        if self.method_indices.contains_key(&subject.id()) {
            return Err(
                SolveAlgorithmBlockConstructionError::DuplicateLifecycleMethod { method: kind },
            );
        }
        let correlation =
            self.issue_subject(AlgorithmCodeSubjectId::LifecycleMethod(subject.id()))?;
        let locals = match self.pending_method_locals.swap_remove(&subject.id()) {
            Some(locals) => locals,
            None => PendingSolveAlgorithmMethodLocals::empty(),
        };
        self.method_indices.insert(subject.id(), method_index);
        self.methods[method_index] = Some(PendingSolveAlgorithmMethod {
            source: subject.id(),
            kind,
            locals,
            actions: (0..action_count).map(|_| None).collect(),
            provenance: subject.provenance(),
            correlation,
        });
        Ok(())
    }

    /// Consume one direct scalar assignment statement.
    pub fn issue_statement(
        &mut self,
        inspection: &AlgorithmCodeInspection<'_, 'id>,
        subject: StatementSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        if subject.kind() != AlgorithmCodeStatementKind::Assignment
            || subject.effect() != rumoca_ir_galec::StatusEffect::Inert
        {
            return Err(SolveAlgorithmBlockConstructionError::UnsupportedStatement {
                kind: subject.kind(),
                provenance: subject.provenance(),
            });
        }
        let (method, action) = direct_statement_owner(inspection, subject)?;
        let method_index = method_index(method.kind().into());
        let pending_method = self.methods[method_index].as_ref().ok_or(
            SolveAlgorithmBlockConstructionError::StatementBeforeLifecycleMethod {
                provenance: subject.provenance(),
            },
        )?;
        if pending_method.source != method.id() {
            return Err(
                SolveAlgorithmBlockConstructionError::ForeignLifecycleStatement {
                    provenance: subject.provenance(),
                },
            );
        }
        let action_index = usize::try_from(action).map_err(|_| {
            SolveAlgorithmBlockConstructionError::LifecycleActionOutOfBounds {
                action,
                provenance: subject.provenance(),
            }
        })?;
        if pending_method
            .actions
            .get(action_index)
            .is_none_or(Option::is_some)
        {
            return Err(
                SolveAlgorithmBlockConstructionError::LifecycleActionOutOfBounds {
                    action,
                    provenance: subject.provenance(),
                },
            );
        }
        if self.statement_indices.contains_key(&subject.id()) {
            return Err(SolveAlgorithmBlockConstructionError::DuplicateStatement {
                provenance: subject.provenance(),
            });
        }
        let correlation = self.issue_subject(AlgorithmCodeSubjectId::Statement(subject.id()))?;
        let method = self.methods[method_index].as_mut().ok_or(
            SolveAlgorithmBlockConstructionError::StatementBeforeLifecycleMethod {
                provenance: subject.provenance(),
            },
        )?;
        method.actions[action_index] = Some(PendingSolveAlgorithmAction {
            target: None,
            value: None,
            provenance: subject.provenance(),
            correlation,
        });
        self.statement_indices
            .insert(subject.id(), (method_index, action_index));
        Ok(())
    }

    /// Consume one exact expression according to its construction-issued
    /// declaration or assignment role.
    pub fn issue_expression(
        &mut self,
        inspection: &AlgorithmCodeInspection<'_, 'id>,
        subject: ExpressionSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        match inspection.parent(AlgorithmCodeSubjectId::Expression(subject.id())) {
            AlgorithmCodeSubjectParent::Subject {
                owner: AlgorithmCodeSubjectId::Declaration(_),
                role: AlgorithmCodeChildRole::DeclarationDimension(_),
            } => self.issue_declaration_dimension(inspection, subject),
            AlgorithmCodeSubjectParent::Subject {
                owner: AlgorithmCodeSubjectId::Declaration(owner),
                role: AlgorithmCodeChildRole::DeclarationStart,
            } => self.issue_declaration_initialization(owner, subject),
            AlgorithmCodeSubjectParent::Subject {
                owner: AlgorithmCodeSubjectId::Statement(owner),
                role: AlgorithmCodeChildRole::AssignmentValue,
            } => self.issue_assignment_value(owner, subject),
            AlgorithmCodeSubjectParent::Subject {
                owner: AlgorithmCodeSubjectId::Expression(_),
                role: AlgorithmCodeChildRole::ArrayElement(_),
            } => self.issue_aggregate_literal_member(inspection, subject),
            _ => Err(
                SolveAlgorithmBlockConstructionError::UnsupportedExpressionContext {
                    provenance: subject.provenance(),
                },
            ),
        }
    }

    fn issue_aggregate_literal_member(
        &mut self,
        inspection: &AlgorithmCodeInspection<'_, 'id>,
        subject: ExpressionSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let mut owner = match inspection.parent(AlgorithmCodeSubjectId::Expression(subject.id())) {
            AlgorithmCodeSubjectParent::Subject {
                owner: AlgorithmCodeSubjectId::Expression(owner),
                role: AlgorithmCodeChildRole::ArrayElement(_),
            } => owner,
            _ => {
                return Err(
                    SolveAlgorithmBlockConstructionError::UnsupportedExpressionContext {
                        provenance: subject.provenance(),
                    },
                );
            }
        };
        loop {
            match inspection.parent(AlgorithmCodeSubjectId::Expression(owner)) {
                AlgorithmCodeSubjectParent::Subject {
                    owner: AlgorithmCodeSubjectId::Expression(parent),
                    role: AlgorithmCodeChildRole::ArrayElement(_),
                } => owner = parent,
                AlgorithmCodeSubjectParent::Subject {
                    owner: AlgorithmCodeSubjectId::Declaration(_),
                    role: AlgorithmCodeChildRole::DeclarationStart,
                }
                | AlgorithmCodeSubjectParent::Subject {
                    owner: AlgorithmCodeSubjectId::Statement(_),
                    role: AlgorithmCodeChildRole::AssignmentValue,
                } => break,
                _ => {
                    return Err(
                        SolveAlgorithmBlockConstructionError::UnsupportedExpressionContext {
                            provenance: subject.provenance(),
                        },
                    );
                }
            }
        }
        if !self.aggregate_literal_roots.contains(&owner) {
            return Err(
                SolveAlgorithmBlockConstructionError::AggregateLiteralMemberBeforeOwner {
                    provenance: subject.provenance(),
                },
            );
        }
        let _root_owned_member =
            self.issue_subject(AlgorithmCodeSubjectId::Expression(subject.id()))?;
        Ok(())
    }

    /// Consume one exact assignment-target reference and its branded resolved
    /// declaration identity.
    pub fn issue_reference(
        &mut self,
        inspection: &AlgorithmCodeInspection<'_, 'id>,
        subject: ReferenceSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let AlgorithmCodeSubjectParent::Subject {
            owner: AlgorithmCodeSubjectId::Statement(statement),
            role: AlgorithmCodeChildRole::AssignmentTarget,
        } = inspection.parent(AlgorithmCodeSubjectId::Reference(subject.id()))
        else {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedReferenceContext {
                    provenance: subject.provenance(),
                },
            );
        };
        let AlgorithmCodeReferenceTarget::Declaration(target) = subject.target() else {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedReferenceTarget {
                    provenance: subject.provenance(),
                },
            );
        };
        let declaration_location = self.declaration_indices.get(&target.id()).copied().ok_or(
            SolveAlgorithmBlockConstructionError::ForeignAssignmentTarget {
                provenance: subject.provenance(),
            },
        )?;
        let PendingDeclarationLocation::Block(declaration) = declaration_location else {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedMethodLocalReference {
                    provenance: subject.provenance(),
                },
            );
        };
        let (method, action) = self.pending_action_location(statement, subject.provenance())?;
        let is_startup_tensor_initialization = self.methods[method]
            .as_ref()
            .is_some_and(|method| method.kind == SolveAlgorithmMethodKind::Startup)
            && matches!(
                self.declarations[declaration].initialization,
                Some(SolveDeclarationInitialization::Internal {
                    value: SolveDeclarationStartValue::UniformTensorFill(_),
                    ..
                })
            );
        if !subject.value().extents().is_empty() && !is_startup_tensor_initialization {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedReferenceShape {
                    provenance: subject.provenance(),
                },
            );
        }
        if self.methods[method]
            .as_ref()
            .and_then(|method| method.actions[action].as_ref())
            .and_then(|action| action.target.as_ref())
            .is_some()
        {
            return Err(
                SolveAlgorithmBlockConstructionError::DuplicateAssignmentTarget {
                    provenance: subject.provenance(),
                },
            );
        }
        let correlation = self.issue_subject(AlgorithmCodeSubjectId::Reference(subject.id()))?;
        self.pending_action_mut(method, action, subject.provenance())?
            .target = Some(PendingAssignmentTarget {
            declaration,
            provenance: subject.provenance(),
            correlation,
        });
        Ok(())
    }

    fn issue_declaration_initialization(
        &mut self,
        owner: DeclarationId<'id>,
        subject: ExpressionSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let declaration_location = self.declaration_indices.get(&owner).copied().ok_or(
            SolveAlgorithmBlockConstructionError::ForeignDeclarationInitialization {
                provenance: subject.provenance(),
            },
        )?;
        let PendingDeclarationLocation::Block(declaration) = declaration_location else {
            return Err(
                SolveAlgorithmBlockConstructionError::ForeignDeclarationInitialization {
                    provenance: subject.provenance(),
                },
            );
        };
        let pending = &self.declarations[declaration];
        if pending.initialization.is_some() {
            return Err(
                SolveAlgorithmBlockConstructionError::DuplicateDeclarationInitialization {
                    provenance: subject.provenance(),
                },
            );
        }
        let evaluated_start = pending.evaluated_start;
        let value_type = pending.value_type.clone();
        let source_class = pending.source_class;
        if !evaluated_start_matches_expression(evaluated_start, subject.evaluated_literal()) {
            return Err(
                SolveAlgorithmBlockConstructionError::DeclarationInitializationCatalogMismatch {
                    provenance: subject.provenance(),
                },
            );
        }
        let value = solve_declaration_start(
            evaluated_start,
            &value_type,
            self.arithmetic,
            subject.provenance(),
        )?;
        let correlation = self.issue_subject(AlgorithmCodeSubjectId::Expression(subject.id()))?;
        self.declarations[declaration].initialization =
            Some(if has_external_initialization_authority(source_class) {
                SolveDeclarationInitialization::External {
                    suggested_value: value,
                    provenance: subject.provenance(),
                    correlation,
                }
            } else {
                SolveDeclarationInitialization::Internal {
                    value,
                    provenance: subject.provenance(),
                    correlation,
                }
            });
        if matches!(
            self.declarations[declaration].initialization,
            Some(SolveDeclarationInitialization::External {
                suggested_value: SolveDeclarationStartValue::UniformTensorFill(_),
                ..
            }) | Some(SolveDeclarationInitialization::Internal {
                value: SolveDeclarationStartValue::UniformTensorFill(_),
                ..
            })
        ) {
            self.aggregate_literal_roots.insert(subject.id());
        }
        Ok(())
    }

    fn issue_assignment_value(
        &mut self,
        owner: StatementId<'id>,
        subject: ExpressionSubject<'_, 'id>,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let (method, action) = self.pending_action_location(owner, subject.provenance())?;
        if self.methods[method]
            .as_ref()
            .and_then(|method| method.actions[action].as_ref())
            .and_then(|action| action.value.as_ref())
            .is_some()
        {
            return Err(
                SolveAlgorithmBlockConstructionError::DuplicateAssignmentValue {
                    provenance: subject.provenance(),
                },
            );
        }
        let value = if subject.value().extents().is_empty() {
            SolveDeclarationStartValue::Scalar(solve_scalar_literal(subject, self.arithmetic)?)
        } else {
            solve_uniform_tensor_literal(subject, self.arithmetic)?
        };
        let aggregate = matches!(value, SolveDeclarationStartValue::UniformTensorFill(_));
        let correlation = self.issue_subject(AlgorithmCodeSubjectId::Expression(subject.id()))?;
        self.pending_action_mut(method, action, subject.provenance())?
            .value = Some(PendingAssignmentValue {
            value,
            source_literal: subject.evaluated_literal(),
            provenance: subject.provenance(),
            correlation,
        });
        if aggregate {
            self.aggregate_literal_roots.insert(subject.id());
        }
        Ok(())
    }

    fn pending_action_location(
        &self,
        statement: StatementId<'id>,
        provenance: SemanticProvenance,
    ) -> Result<(usize, usize), SolveAlgorithmBlockConstructionError> {
        self.statement_indices
            .get(&statement)
            .copied()
            .ok_or(SolveAlgorithmBlockConstructionError::ForeignAssignmentChild { provenance })
    }

    fn pending_action_mut(
        &mut self,
        method: usize,
        action: usize,
        provenance: SemanticProvenance,
    ) -> Result<&mut PendingSolveAlgorithmAction, SolveAlgorithmBlockConstructionError> {
        self.methods[method]
            .as_mut()
            .and_then(|method| method.actions.get_mut(action))
            .and_then(Option::as_mut)
            .ok_or(SolveAlgorithmBlockConstructionError::ForeignAssignmentChild { provenance })
    }

    /// Install the one complete SOLVE-C59 transfer catalog.
    pub fn issue_call_transfers(
        &mut self,
        call_transfers: CallTransferPlanSet,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        if self.call_transfers.is_some() {
            return Err(SolveAlgorithmBlockConstructionError::DuplicateCallTransferCatalog);
        }
        self.call_transfers = Some(call_transfers);
        Ok(())
    }

    fn issue_subject(
        &mut self,
        subject: AlgorithmCodeSubjectId<'id>,
    ) -> Result<AlgorithmCodeSubjectCorrelation, SolveAlgorithmBlockConstructionError> {
        issue_subject_from_parts(
            &self.expected_subjects,
            &mut self.expected_correlations,
            &mut self.next_subject,
            subject,
        )
    }

    fn finish(self) -> Result<FinishedAlgorithmBlockParts, SolveAlgorithmBlockConstructionError> {
        if !self.pending_method_locals.is_empty() {
            return Err(SolveAlgorithmBlockConstructionError::UnownedMethodLocals);
        }
        if self.next_subject != self.expected_subjects.len() {
            return Err(SolveAlgorithmBlockConstructionError::UnmappedSubjects {
                expected: self.expected_subjects.len(),
                mapped: self.next_subject,
            });
        }
        let [startup, recalibrate, do_step] = self.methods;
        let call_transfers = self
            .call_transfers
            .ok_or(SolveAlgorithmBlockConstructionError::MissingCallTransferCatalog)?;
        let declarations = self
            .declarations
            .into_iter()
            .map(finish_declaration)
            .collect::<Result<Vec<_>, _>>()?
            .into_boxed_slice();
        let mut startup_initializations = StartupInitializationCoverage::construct(&declarations);
        let methods = [
            finish_required_method(
                startup,
                SolveAlgorithmMethodKind::Startup,
                &declarations,
                self.arithmetic,
                Some(&mut startup_initializations),
            )?,
            finish_required_method(
                recalibrate,
                SolveAlgorithmMethodKind::Recalibrate,
                &declarations,
                self.arithmetic,
                None,
            )?,
            finish_required_method(
                do_step,
                SolveAlgorithmMethodKind::DoStep,
                &declarations,
                self.arithmetic,
                None,
            )?,
        ];
        startup_initializations.finish(&declarations)?;
        Ok((
            declarations,
            self.logical_storage.finish(),
            methods,
            call_transfers,
        ))
    }
}

fn issue_subject_from_parts<'id>(
    expected_subjects: &[AlgorithmCodeSubjectId<'id>],
    expected_correlations: &mut [Option<AlgorithmCodeSubjectCorrelation>],
    next_subject: &mut usize,
    subject: AlgorithmCodeSubjectId<'id>,
) -> Result<AlgorithmCodeSubjectCorrelation, SolveAlgorithmBlockConstructionError> {
    let subject_index = *next_subject;
    let Some(expected) = expected_subjects.get(subject_index).copied() else {
        return Err(SolveAlgorithmBlockConstructionError::ForeignOrDuplicateSubject);
    };
    if expected != subject {
        return Err(SolveAlgorithmBlockConstructionError::ReorderedSubject);
    }
    let following_subject = subject_index
        .checked_add(1)
        .ok_or(SolveAlgorithmBlockConstructionError::SubjectCountOverflow)?;
    let correlation = expected_correlations
        .get_mut(subject_index)
        .and_then(Option::take)
        .ok_or(SolveAlgorithmBlockConstructionError::MissingSubjectCorrelation)?;
    *next_subject = following_subject;
    Ok(correlation)
}

fn finish_declaration(
    declaration: PendingSolveAlgorithmDeclaration,
) -> Result<SolveAlgorithmDeclaration, SolveAlgorithmBlockConstructionError> {
    let dimensions = declaration
        .dimensions
        .into_vec()
        .into_iter()
        .enumerate()
        .map(|(axis, dimension)| {
            let axis = u32::try_from(axis).map_err(|_| {
                SolveAlgorithmBlockConstructionError::DeclarationDimensionAxisOverflow {
                    provenance: declaration.provenance,
                }
            })?;
            dimension.ok_or(
                SolveAlgorithmBlockConstructionError::MissingDeclarationDimension {
                    axis,
                    provenance: declaration.provenance,
                },
            )
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    Ok(SolveAlgorithmDeclaration {
        source_class: declaration.source_class,
        block_index: declaration.block_index,
        evaluated_start: declaration.evaluated_start,
        value_type: declaration.value_type,
        storage: declaration.storage,
        access: declaration.access,
        logical_storage: declaration.logical_storage,
        initialization: declaration.initialization.ok_or(
            SolveAlgorithmBlockConstructionError::MissingDeclarationInitialization {
                provenance: declaration.provenance,
            },
        )?,
        dimensions,
        provenance: declaration.provenance,
        correlation: declaration.correlation,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LifecycleActionClass {
    StartupInitialization,
    Assignment,
}

struct StartupInitializationCoverage {
    outstanding: usize,
    consumed: Box<[bool]>,
}

impl StartupInitializationCoverage {
    fn construct(declarations: &[SolveAlgorithmDeclaration]) -> Self {
        let consumed = declarations
            .iter()
            .map(|declaration| {
                matches!(
                    declaration.initialization(),
                    SolveDeclarationInitialization::External { .. }
                )
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let outstanding = consumed.iter().filter(|consumed| !**consumed).count();
        Self {
            outstanding,
            consumed,
        }
    }

    fn consume(
        &mut self,
        declaration: usize,
        provenance: SemanticProvenance,
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let consumed = self
            .consumed
            .get_mut(declaration)
            .ok_or(SolveAlgorithmBlockConstructionError::ForeignAssignmentTarget { provenance })?;
        if *consumed {
            return Err(
                SolveAlgorithmBlockConstructionError::DuplicateStartupInitialization { provenance },
            );
        }
        *consumed = true;
        self.outstanding = self.outstanding.checked_sub(1).ok_or(
            SolveAlgorithmBlockConstructionError::DuplicateStartupInitialization { provenance },
        )?;
        Ok(())
    }

    fn finish(
        self,
        declarations: &[SolveAlgorithmDeclaration],
    ) -> Result<(), SolveAlgorithmBlockConstructionError> {
        if self.outstanding == 0 {
            return Ok(());
        }
        let provenance = self
            .consumed
            .iter()
            .position(|consumed| !*consumed)
            .and_then(|index| declarations.get(index))
            .map(SolveAlgorithmDeclaration::provenance)
            .ok_or(SolveAlgorithmBlockConstructionError::StartupInitializationCoverageMismatch)?;
        Err(SolveAlgorithmBlockConstructionError::MissingStartupInitialization { provenance })
    }
}

fn admit_lifecycle_write(
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    method: SolveAlgorithmMethodKind,
    action: LifecycleActionClass,
    provenance: SemanticProvenance,
) -> Result<(), SolveAlgorithmBlockConstructionError> {
    let admitted = match (storage, action) {
        (
            SolveStorageClass::Output
            | SolveStorageClass::CalculatedParameter
            | SolveStorageClass::Constant
            | SolveStorageClass::PersistentState,
            LifecycleActionClass::StartupInitialization,
        ) => method == SolveAlgorithmMethodKind::Startup,
        (
            SolveStorageClass::Output
            | SolveStorageClass::CalculatedParameter
            | SolveStorageClass::PersistentState,
            LifecycleActionClass::Assignment,
        ) => method != SolveAlgorithmMethodKind::Startup && access == SolveSlotAccess::ReadWrite,
        (
            SolveStorageClass::Input
            | SolveStorageClass::TunableParameter
            | SolveStorageClass::PreviousState
            | SolveStorageClass::MethodLocal
            | SolveStorageClass::SignalStatus,
            LifecycleActionClass::StartupInitialization,
        )
        | (
            SolveStorageClass::Input
            | SolveStorageClass::TunableParameter
            | SolveStorageClass::Constant
            | SolveStorageClass::PreviousState
            | SolveStorageClass::MethodLocal
            | SolveStorageClass::SignalStatus,
            LifecycleActionClass::Assignment,
        ) => false,
    };
    if !admitted {
        return Err(if action == LifecycleActionClass::StartupInitialization {
            SolveAlgorithmBlockConstructionError::StartupInitializationOutsideStartup { provenance }
        } else {
            SolveAlgorithmBlockConstructionError::IllegalLifecycleWrite {
                storage,
                method,
                provenance,
            }
        });
    }
    Ok(())
}

fn finish_required_method(
    method: Option<PendingSolveAlgorithmMethod<'_>>,
    kind: SolveAlgorithmMethodKind,
    declarations: &[SolveAlgorithmDeclaration],
    arithmetic: SolveArithmeticProfile,
    mut startup_initializations: Option<&mut StartupInitializationCoverage>,
) -> Result<SolveAlgorithmMethod, SolveAlgorithmBlockConstructionError> {
    let method = method
        .ok_or(SolveAlgorithmBlockConstructionError::MissingLifecycleMethod { method: kind })?;
    let finished_locals = method.locals.finish()?;
    let actions = method
        .actions
        .into_iter()
        .map(|action| {
            finish_action(
                action,
                method.kind,
                declarations,
                startup_initializations.as_deref_mut(),
            )
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    let (program, actions, storage_bindings) =
        build_method_program(actions, declarations, arithmetic)?;
    Ok(SolveAlgorithmMethod {
        kind: method.kind,
        locals: finished_locals.locals,
        method_local_scalar_count: finished_locals.scalar_count,
        program,
        actions,
        storage_bindings,
        error_effects: SolveAlgorithmErrorEffects::ResetOnly,
        abi: SolveAlgorithmMethodAbi::ParameterFreeInfallible,
        provenance: method.provenance,
        correlation: method.correlation,
    })
}

fn finish_action(
    action: Option<PendingSolveAlgorithmAction>,
    method: SolveAlgorithmMethodKind,
    declarations: &[SolveAlgorithmDeclaration],
    startup_initializations: Option<&mut StartupInitializationCoverage>,
) -> Result<PendingFinishedSolveAlgorithmAction, SolveAlgorithmBlockConstructionError> {
    let action =
        action.ok_or(SolveAlgorithmBlockConstructionError::MissingLifecycleAction { method })?;
    let target = action.target.ok_or(
        SolveAlgorithmBlockConstructionError::MissingAssignmentTarget {
            provenance: action.provenance,
        },
    )?;
    let value = action.value.ok_or(
        SolveAlgorithmBlockConstructionError::MissingAssignmentValue {
            provenance: action.provenance,
        },
    )?;
    let declaration = u32::try_from(target.declaration).map_err(|_| {
        SolveAlgorithmBlockConstructionError::DeclarationIndexOverflow {
            provenance: target.provenance,
        }
    })?;
    let declaration_facts = declarations.get(target.declaration).ok_or(
        SolveAlgorithmBlockConstructionError::ForeignAssignmentTarget {
            provenance: target.provenance,
        },
    )?;
    let startup_value = match declaration_facts.initialization() {
        SolveDeclarationInitialization::Internal { value, .. } => Some(value),
        SolveDeclarationInitialization::External { .. } => None,
    };
    let kind = if method == SolveAlgorithmMethodKind::Startup
        && let Some(expected) = startup_value
    {
        if !evaluated_start_matches_expression(
            declaration_facts.evaluated_start(),
            value.source_literal,
        ) {
            return Err(
                SolveAlgorithmBlockConstructionError::StartupInitializationValueMismatch {
                    provenance: value.provenance,
                },
            );
        }
        if expected != &value.value {
            return Err(
                SolveAlgorithmBlockConstructionError::StartupInitializationValueMismatch {
                    provenance: value.provenance,
                },
            );
        }
        startup_initializations
            .ok_or(
                SolveAlgorithmBlockConstructionError::StartupInitializationOutsideStartup {
                    provenance: action.provenance,
                },
            )?
            .consume(target.declaration, target.provenance)?;
        LifecycleActionClass::StartupInitialization
    } else {
        LifecycleActionClass::Assignment
    };
    admit_lifecycle_write(
        declaration_facts.storage(),
        declaration_facts.access(),
        method,
        kind,
        target.provenance,
    )?;
    let kind = match kind {
        LifecycleActionClass::StartupInitialization => {
            PendingFinishedSolveAlgorithmActionKind::StartupInitialize {
                declaration,
                value: value.value,
                target_provenance: target.provenance,
                target_correlation: target.correlation,
                value_provenance: value.provenance,
                value_correlation: value.correlation,
            }
        }
        LifecycleActionClass::Assignment => {
            let SolveDeclarationStartValue::Scalar(scalar) = value.value else {
                return Err(
                    SolveAlgorithmBlockConstructionError::UnsupportedTensorAssignmentPlan {
                        provenance: value.provenance,
                    },
                );
            };
            PendingFinishedSolveAlgorithmActionKind::AssignScalarLiteral {
                declaration,
                value: scalar,
                target_provenance: target.provenance,
                target_correlation: target.correlation,
                value_provenance: value.provenance,
                value_correlation: value.correlation,
            }
        }
    };
    Ok(PendingFinishedSolveAlgorithmAction {
        kind,
        provenance: action.provenance,
        correlation: action.correlation,
    })
}

fn build_method_program(
    actions: Box<[PendingFinishedSolveAlgorithmAction]>,
    declarations: &[SolveAlgorithmDeclaration],
    arithmetic: SolveArithmeticProfile,
) -> Result<BuiltMethodProgram, SolveAlgorithmBlockConstructionError> {
    let mut actions = actions
        .into_vec()
        .into_iter()
        .map(|action| ActionWithExecution {
            action,
            execution: None,
        })
        .collect::<Vec<_>>();
    let mut binding_declarations = Vec::new();
    let program = build_pending_method_program(
        &mut actions,
        declarations,
        arithmetic,
        &mut binding_declarations,
    )?;
    let actions = finish_method_actions(actions)?;
    let storage_bindings =
        finish_program_storage_bindings(&program, declarations, &binding_declarations)?;
    Ok((program, actions, storage_bindings))
}

fn build_pending_method_program(
    actions: &mut [ActionWithExecution],
    declarations: &[SolveAlgorithmDeclaration],
    arithmetic: SolveArithmeticProfile,
    binding_declarations: &mut Vec<usize>,
) -> Result<TypedProgram, SolveAlgorithmBlockConstructionError> {
    let program = TypedProgram::construct(arithmetic, |builder| {
        let mut slots: Vec<Option<ProgramSlot<'_>>> =
            (0..declarations.len()).map(|_| None).collect();
        for pending in &mut *actions {
            let action = &pending.action;
            let (declaration, value, value_provenance) = match &action.kind {
                PendingFinishedSolveAlgorithmActionKind::StartupInitialize { .. } => {
                    pending.execution = Some(SolveAlgorithmActionExecution::StartupOwned);
                    continue;
                }
                PendingFinishedSolveAlgorithmActionKind::AssignScalarLiteral {
                    declaration,
                    value,
                    value_provenance,
                    ..
                } => (declaration, value, value_provenance),
            };
            let action_span = program_span(action.provenance)?;
            let declaration_index = usize::try_from(*declaration).map_err(|_| {
                SolveProgramConstructionError::IdentityOverflow {
                    provenance: action_span,
                }
            })?;
            let target = declarations.get(declaration_index).ok_or(
                SolveProgramConstructionError::UnknownSlot {
                    provenance: action_span,
                },
            )?;
            let slot = match slots[declaration_index] {
                Some(slot) => slot,
                None => {
                    let slot = builder.declare_slot(
                        target.value_type().clone(),
                        target.storage(),
                        target.access(),
                        program_span(target.provenance())?,
                    )?;
                    slots[declaration_index] = Some(slot);
                    binding_declarations.push(declaration_index);
                    slot
                }
            };
            let first = u32::try_from(builder.operation_count()).map_err(|_| {
                SolveProgramConstructionError::IdentityOverflow {
                    provenance: action_span,
                }
            })?;
            let value_span = program_span(*value_provenance)?;
            let source = builder.constant(value.clone(), value_span)?;
            let source = builder.coerce_to(source, target.value_type(), value_span)?;
            builder.store(slot, source, action_span)?;
            let end = u32::try_from(builder.operation_count()).map_err(|_| {
                SolveProgramConstructionError::IdentityOverflow {
                    provenance: action_span,
                }
            })?;
            let count = end.checked_sub(first).filter(|count| *count != 0).ok_or(
                SolveProgramConstructionError::IdentityOverflow {
                    provenance: action_span,
                },
            )?;
            pending.execution = Some(SolveAlgorithmActionExecution::ProgramOwned {
                operations: SolveProgramOperationRun { first, count },
            });
        }
        Ok(())
    })
    .map_err(SolveAlgorithmBlockConstructionError::Program)?;
    Ok(program)
}

fn finish_method_actions(
    actions: Vec<ActionWithExecution>,
) -> Result<Box<[SolveAlgorithmAction]>, SolveAlgorithmBlockConstructionError> {
    let actions = actions
        .into_iter()
        .map(|pending| {
            let provenance = pending.action.provenance;
            let execution = pending.execution.ok_or(
                SolveAlgorithmBlockConstructionError::MissingActionExecution { provenance },
            )?;
            let kind = match (pending.action.kind, execution) {
                (
                    PendingFinishedSolveAlgorithmActionKind::StartupInitialize {
                        declaration,
                        value,
                        target_provenance,
                        target_correlation,
                        value_provenance,
                        value_correlation,
                    },
                    SolveAlgorithmActionExecution::StartupOwned,
                ) => SolveAlgorithmActionKind::StartupInitialize {
                    declaration,
                    value,
                    target_provenance,
                    target_correlation,
                    value_provenance,
                    value_correlation,
                },
                (
                    PendingFinishedSolveAlgorithmActionKind::AssignScalarLiteral {
                        declaration,
                        value,
                        target_provenance,
                        target_correlation,
                        value_provenance,
                        value_correlation,
                    },
                    SolveAlgorithmActionExecution::ProgramOwned { operations },
                ) => SolveAlgorithmActionKind::AssignScalarLiteral {
                    declaration,
                    value,
                    program_operations: operations,
                    target_provenance,
                    target_correlation,
                    value_provenance,
                    value_correlation,
                },
                _ => {
                    return Err(
                        SolveAlgorithmBlockConstructionError::ActionExecutionDispositionMismatch {
                            provenance,
                        },
                    );
                }
            };
            Ok(SolveAlgorithmAction {
                kind,
                provenance,
                correlation: pending.action.correlation,
            })
        })
        .collect::<Result<Vec<_>, SolveAlgorithmBlockConstructionError>>()?
        .into_boxed_slice();
    Ok(actions)
}

fn finish_program_storage_bindings(
    program: &TypedProgram,
    declarations: &[SolveAlgorithmDeclaration],
    binding_declarations: &[usize],
) -> Result<Box<[SolveProgramStorageBinding]>, SolveAlgorithmBlockConstructionError> {
    if program.slots().len() != binding_declarations.len() {
        return Err(SolveAlgorithmBlockConstructionError::ProgramStorageBindingMismatch);
    }
    let storage_bindings = program
        .slots()
        .iter()
        .enumerate()
        .map(|(index, slot)| {
            let declaration = *binding_declarations
                .get(index)
                .ok_or(SolveAlgorithmBlockConstructionError::ProgramStorageBindingMismatch)?;
            let declaration_u32 = u32::try_from(declaration)
                .map_err(|_| SolveAlgorithmBlockConstructionError::ProgramStorageBindingMismatch)?;
            let logical_storage = declarations
                .get(declaration)
                .ok_or(SolveAlgorithmBlockConstructionError::ProgramStorageBindingMismatch)?
                .logical_storage();
            Ok(SolveProgramStorageBinding {
                slot: slot.id(),
                declaration: declaration_u32,
                logical_storage,
            })
        })
        .collect::<Result<Vec<_>, SolveAlgorithmBlockConstructionError>>()?
        .into_boxed_slice();
    Ok(storage_bindings)
}

#[cfg(test)]
mod tests;
