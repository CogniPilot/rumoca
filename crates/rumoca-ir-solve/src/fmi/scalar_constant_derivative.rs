//! The bounded Solve-to-FMI-3 agreement check for one scalar constant
//! derivative.
//!
//! The admitted profile is structural, never nominal: one event-free Binary64
//! scalar continuous state whose start is fixed and bitwise exact and whose
//! derivative kernel is exactly one constant load followed by one output
//! store. No model name, variable name, scalar label, span, or unit
//! participates in admission or agreement; the fact arrays below physically
//! cannot carry one.
//!
//! [`check_scalar_constant_derivative_projection`] is a separate, pure, total
//! function over two eagerly materialized closed fact arrays: the Solve side
//! projected from one sealed [`SolveModel`] and the candidate FMI 3 side
//! projected from the checked event-free inventory and projection. Acceptance
//! is a positive, exhaustive case analysis; every case the checker does not
//! handle refuses by name as either a typed unsupported capability
//! ([`ScalarConstantDerivativeUnsupported`]) or a typed disagreement
//! ([`ScalarConstantDerivativeDisagreement`]). Absence of a refusal is never
//! permission, and no other kernel shape falls back to a weaker check.
//!
//! On success the checker mints [`ScalarConstantDerivativeReceipt`], which is
//! deliberately not `Clone`, not `Default`, and not serializable to any wire.
//! The receipt certifies only the exact fact arrays that were checked. The
//! correlated claim is [`Fmi3ScalarConstantDerivativeCarrier`], whose sole
//! constructor projects both fact arrays from the one retained event-free
//! view, so a receipt over foreign facts cannot be paired with a kernel.
//!
//! In SPEC_0037 terms every claim here is at the symbolic-system and
//! bit-representation layer: the checked relation covers the source-identity
//! join across every candidate leg (the catalog-issued source occurrence has
//! no candidate-side representation, so it is carried into the receipt rather
//! than compared), role, causality, variability, fixity, state
//! initialization, dimensions, scalar count, storage association, bit-exact
//! start agreement across the catalog, runtime, and XML-facing legs, the
//! unique time/state/derivative value-reference inventory, the
//! state-to-derivative and storage correlation, the exact ModelStructure
//! membership, and the exact two-operation kernel. It does not cover
//! minimum/maximum/nominal/unit attributes, the FMI 2 projection, FMI 2 model
//! indices, rendered XML or C text, numeric evaluation, trajectories, or any
//! cross-root or persisted identity.

use super::Fmi3WriteModes;
use super::event_free::FmiEventFreeCodegenView;
use super::metadata::{
    FmiCausality, FmiInitial, FmiStateInitial, FmiStateReinit, FmiStorageRun, FmiVariability,
    FmiWritePolicy,
};

/// The write policy a scalar constant-derivative state must carry: an exact,
/// non-reinitialized continuous state. Named once so the inventory check and
/// the projected-mask check read the same fact.
const SCALAR_STATE_POLICY: FmiWritePolicy = FmiWritePolicy::ContinuousState {
    initial: FmiStateInitial::Exact,
    reinit: FmiStateReinit::False,
};
use crate::{
    ComputeNode, LinearOp, SolveModel, SolveRealFormat, SolveStateInitialization,
    SolveStorageColumn, SolveStorageCoordinate, SolveVariableCausality, SolveVariableStorageRole,
    SolveVariableValueKind, SolveVariableVariability,
};
use rumoca_core::{Fixity, SourceOccurrenceId};

/// FMI value reference zero is the independent `time` variable.
const TIME_VALUE_REFERENCE: u32 = 0;

/// One closed, name-free Solve-side fact array.
///
/// This is untrusted checker input: construction performs no validation and
/// grants no authority. The trusted producer is
/// [`project_scalar_constant_derivative_solve_facts`], which materializes the
/// array from one sealed root; the checker proves the same facts either way.
#[derive(Debug)]
pub struct ScalarConstantDerivativeSolveFacts {
    pub real_format: SolveRealFormat,
    pub state_scalar_count: usize,
    pub y_scalar_count: usize,
    pub p_scalar_count: usize,
    pub system: ScalarConstantDerivativeSystemFacts,
    pub variables: Vec<SolveScalarVariableFact>,
    /// Bit patterns of the complete runtime initial solver vector.
    pub initial_y_bits: Vec<u64>,
    pub kernel: DerivativeKernelFacts,
}

/// Counts of executable owners outside the admitted derivative kernel.
///
/// The continuous count sums retained compute nodes, implicit row targets,
/// and issued refresh rows; the remaining counts are nonempty owner classes
/// per partition, in the same field walk the crate's structural presence
/// queries use. The profile admits only a root in which every count is zero,
/// so the two-op kernel claim cannot silently coexist with another
/// executable owner.
#[derive(Debug)]
pub struct ScalarConstantDerivativeSystemFacts {
    pub nonderivative_continuous_owner_count: usize,
    pub initialization_owner_class_count: usize,
    pub discrete_owner_class_count: usize,
    pub event_owner_class_count: usize,
    pub runtime_event_owner_class_count: usize,
    pub clock_owner_class_count: usize,
}

/// One name-free Solve catalog declaration fact.
#[derive(Debug)]
pub struct SolveScalarVariableFact {
    pub source_occurrence: SourceOccurrenceId,
    pub source_id_index: usize,
    pub role: SolveVariableStorageRole,
    pub causality: SolveVariableCausality,
    pub variability: SolveVariableVariability,
    pub fixed: Fixity,
    pub state_initialization: SolveStateInitialization,
    pub value_kind: SolveVariableValueKind,
    pub dimensions: Vec<u32>,
    pub scalar_identity_count: usize,
    pub storage: SolveStorageFact,
    pub tunable: bool,
    /// Bit patterns of the catalog start values, absent when no start exists.
    pub start_bits: Option<Vec<u64>>,
}

/// A declaration's storage association, projected without names or spans.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveStorageFact {
    Y { base: usize, scalar_count: usize },
    P { base: usize, scalar_count: usize },
    NonAddressable,
}

/// The derivative kernel as a closed operation-shape array.
#[derive(Debug)]
pub struct DerivativeKernelFacts {
    pub scalar_program_count: usize,
    pub tensor_node_count: usize,
    pub output_indices: Vec<usize>,
    pub operations: Vec<KernelOperationFact>,
}

/// One derivative-kernel operation in the closed shape vocabulary.
///
/// The two admitted shapes carry their exact operands; every other operation
/// keeps only its operation-kind name so the refusal can say which operation
/// the profile does not admit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KernelOperationFact {
    ConstantBits { destination: u32, bits: u64 },
    StoreOutput { source: u32 },
    Unsupported { operation: &'static str },
}

const fn kernel_operation_name(operation: &KernelOperationFact) -> &'static str {
    match operation {
        KernelOperationFact::ConstantBits { .. } => "ConstantBits",
        KernelOperationFact::StoreOutput { .. } => "StoreOutput",
        KernelOperationFact::Unsupported { operation } => operation,
    }
}

/// One closed, name-free candidate FMI 3 fact array.
///
/// Like the Solve side, this is untrusted checker input. The trusted producer
/// is [`project_scalar_constant_derivative_fmi3_facts`].
#[derive(Debug)]
pub struct ScalarConstantDerivativeFmi3Facts {
    /// The value-reference inventory leg consumed by both FMI versions.
    pub inventory: Vec<Fmi3InventoryEntryFact>,
    pub state_variable_ordinals: Vec<usize>,
    pub derivative_value_reference_base: u32,
    /// The FMI 3 tensor-variable leg the XML and C artifacts consume.
    pub variables: Vec<Fmi3StateVariableFact>,
    pub derivatives: Vec<Fmi3StateDerivativeFact>,
    pub output_value_references: Vec<u32>,
    pub derivative_value_references: Vec<u32>,
    pub initial_unknown_value_references: Vec<u32>,
    pub continuous_state_scalar_count: usize,
}

/// One value-reference inventory entry, without names or spans.
#[derive(Debug)]
pub struct Fmi3InventoryEntryFact {
    pub source_id_index: Option<usize>,
    pub value_reference: u32,
    pub role: Option<SolveVariableStorageRole>,
    pub storage: Option<FmiStorageRun>,
    pub dimensions: Vec<u32>,
    pub value_kind: SolveVariableValueKind,
    pub scalar_identity_count: usize,
    pub causality: FmiCausality,
    pub variability: FmiVariability,
    pub initial: Option<FmiInitial>,
    pub write_policy: FmiWritePolicy,
    pub tunable: bool,
    pub start_bits: Option<Vec<u64>>,
}

/// One FMI 3 tensor variable, without names or spans.
#[derive(Debug)]
pub struct Fmi3StateVariableFact {
    pub source_id_index: usize,
    pub value_reference: u32,
    pub storage: FmiStorageRun,
    pub dimensions: Vec<u32>,
    pub causality: FmiCausality,
    pub variability: FmiVariability,
    pub initial: Option<FmiInitial>,
    pub write_modes: Fmi3WriteModes,
    pub output_member: bool,
    pub continuous_state_derivative_member: bool,
    pub initial_unknown_member: bool,
    pub link_state_value_reference: Option<u32>,
    pub link_derivative_value_reference: Option<u32>,
    /// Bit patterns of the `start` attribute the XML consumes.
    pub xml_start_bits: Option<Vec<u64>>,
    /// Bit patterns of the runtime initialization values the C consumes.
    pub runtime_start_bits: Vec<u64>,
}

/// One FMI 3 derivative variable, without names or spans.
#[derive(Debug)]
pub struct Fmi3StateDerivativeFact {
    pub source_id_index: usize,
    pub link_state_value_reference: u32,
    pub link_derivative_value_reference: u32,
    pub storage_base: usize,
    pub storage_scalar_count: usize,
    pub dimensions: Vec<u32>,
    pub causality: FmiCausality,
    pub variability: FmiVariability,
    pub initial: FmiInitial,
    pub output_member: bool,
    pub continuous_state_derivative_member: bool,
    pub initial_unknown_member: bool,
}

/// A start-value leg compared bitwise against the catalog leg.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StartLeg {
    /// The runtime initial solver vector retained by the sealed root.
    RuntimeVector,
    /// The value-reference inventory entry's projected start.
    InventoryEntry,
    /// The FMI 3 variable `start` attribute the XML consumes.
    ProjectionStart,
    /// The FMI 3 variable runtime initialization values the C consumes.
    ProjectionRuntimeStart,
}

/// A Solve root outside the admitted structural profile.
///
/// Every variant is a typed unsupported capability: the named structure is
/// what this bounded profile does not admit. None of these is a fallback and
/// none is repaired.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ScalarConstantDerivativeUnsupported {
    #[error("profile admits only Binary64 arithmetic, not {actual:?}")]
    RealFormat { actual: SolveRealFormat },
    #[error("profile admits no continuous owners beside the derivative kernel; found {count}")]
    NonderivativeContinuousOwners { count: usize },
    #[error("profile admits no initialization owners; found {class_count} nonempty classes")]
    InitializationOwners { class_count: usize },
    #[error("profile admits no discrete owners; found {class_count} nonempty classes")]
    DiscreteOwners { class_count: usize },
    #[error("profile admits no event owners; found {class_count} nonempty classes")]
    EventOwners { class_count: usize },
    #[error("profile admits no runtime event owners; found {class_count} nonempty classes")]
    RuntimeEventOwners { class_count: usize },
    #[error("profile admits no clock owners; found {class_count} nonempty classes")]
    ClockOwners { class_count: usize },
    #[error("profile admits exactly one state scalar; found {actual}")]
    StateWidth { actual: usize },
    #[error("profile admits exactly one solver scalar; found {actual}")]
    SolverWidth { actual: usize },
    #[error("profile admits no parameter scalars; found {actual}")]
    ParameterWidth { actual: usize },
    #[error("profile admits exactly one declaration; found {actual}")]
    DeclarationCount { actual: usize },
    #[error("profile admits only the State role; found {actual:?}")]
    Role { actual: SolveVariableStorageRole },
    #[error("profile admits only Real declarations; found {actual:?}")]
    ValueKind { actual: SolveVariableValueKind },
    #[error("profile admits only a rank-zero scalar declaration; found rank {rank}")]
    Dimensions { rank: usize },
    #[error("profile admits exactly one scalar identity; found {actual}")]
    ScalarIdentityCount { actual: usize },
    #[error("profile admits only the first Y scalar as storage; found {actual:?}")]
    Storage { actual: SolveStorageFact },
    #[error("profile admits only Local causality; found {actual:?}")]
    Causality { actual: SolveVariableCausality },
    #[error("profile admits only Continuous variability; found {actual:?}")]
    Variability { actual: SolveVariableVariability },
    #[error("profile admits only a fixed start; found {actual:?}")]
    Fixity { actual: Fixity },
    #[error("profile admits only Exact state initialization; found {actual:?}")]
    StateInitialization { actual: SolveStateInitialization },
    #[error("profile admits no tunable declaration")]
    Tunable,
    #[error("profile requires one catalog start scalar; present: {present}, width: {width}")]
    CatalogStartShape { present: bool, width: usize },
    #[error("profile requires one runtime initial scalar; found {actual}")]
    RuntimeWidth { actual: usize },
    #[error(
        "profile admits exactly one scalar derivative program; found {scalar_program_count} scalar programs and {tensor_node_count} tensor nodes"
    )]
    KernelNodeShape {
        scalar_program_count: usize,
        tensor_node_count: usize,
    },
    #[error("profile requires the kernel to own exactly output zero; found {output_indices:?}")]
    KernelOutputIdentity { output_indices: Vec<usize> },
    #[error("profile admits exactly two kernel operations; found {actual}")]
    KernelOperationCount { actual: usize },
    #[error("profile does not admit kernel operation `{operation}` at position {position}")]
    KernelOperation {
        position: usize,
        operation: &'static str,
    },
    #[error(
        "profile requires the store to read the constant register; constant writes {destination}, store reads {source_register}"
    )]
    KernelStoreSource {
        destination: u32,
        source_register: u32,
    },
}

/// A candidate FMI 3 fact array that contradicts the admitted Solve facts or
/// the value-reference assignment rule re-derived by the checker.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ScalarConstantDerivativeDisagreement {
    #[error("candidate inventory must hold exactly one entry; found {actual}")]
    InventoryCount { actual: usize },
    #[error("candidate FMI 3 projection must hold exactly one variable; found {actual}")]
    VariableCount { actual: usize },
    #[error("candidate FMI 3 projection must hold exactly one derivative; found {actual}")]
    DerivativeCount { actual: usize },
    #[error("candidate state ordinals must be exactly [0]; found {actual:?}")]
    StateOrdinalInventory { actual: Vec<usize> },
    #[error(
        "time, state, and derivative value references must be pairwise distinct; time {time}, state {state}, derivative {derivative}"
    )]
    ValueReferenceCollision {
        time: u32,
        state: u32,
        derivative: u32,
    },
    #[error("inventory entry must own value reference {expected}; found {actual}")]
    InventoryValueReference { expected: u32, actual: u32 },
    #[error("inventory entry must carry source identity {expected}; found {actual:?}")]
    InventorySourceIdentity {
        expected: usize,
        actual: Option<usize>,
    },
    #[error("inventory entry must carry the State role; found {actual:?}")]
    InventoryRole {
        actual: Option<SolveVariableStorageRole>,
    },
    #[error("inventory entry must be stored at the first Y scalar; found {actual:?}")]
    InventoryStorage { actual: Option<FmiStorageRun> },
    #[error("inventory entry must be a rank-zero scalar; found rank {rank}")]
    InventoryDimensions { rank: usize },
    #[error("inventory entry must be Real; found {actual:?}")]
    InventoryValueKind { actual: SolveVariableValueKind },
    #[error("inventory entry must own exactly one scalar identity; found {actual}")]
    InventoryScalarIdentityCount { actual: usize },
    #[error("inventory entry must carry Local causality; found {actual:?}")]
    InventoryCausality { actual: FmiCausality },
    #[error("inventory entry must carry Continuous variability; found {actual:?}")]
    InventoryVariability { actual: FmiVariability },
    #[error("inventory entry must carry initial Exact; found {actual:?}")]
    InventoryInitial { actual: Option<FmiInitial> },
    #[error("inventory entry must carry the reinit-false state write policy; found {actual:?}")]
    InventoryWritePolicy { actual: FmiWritePolicy },
    #[error("inventory entry must not be tunable")]
    InventoryTunability,
    #[error("FMI 3 variable must own value reference {expected}; found {actual}")]
    VariableValueReference { expected: u32, actual: u32 },
    #[error("FMI 3 variable must carry source identity {expected}; found {actual}")]
    VariableSourceIdentity { expected: usize, actual: usize },
    #[error("FMI 3 variable must be stored at the first Y scalar; found {actual:?}")]
    VariableStorage { actual: FmiStorageRun },
    #[error("FMI 3 variable must be a rank-zero scalar; found rank {rank}")]
    VariableDimensions { rank: usize },
    #[error("FMI 3 variable must carry Local causality; found {actual:?}")]
    VariableCausality { actual: FmiCausality },
    #[error("FMI 3 variable must carry Continuous variability; found {actual:?}")]
    VariableVariability { actual: FmiVariability },
    #[error("FMI 3 variable must carry initial Exact; found {actual:?}")]
    VariableInitial { actual: Option<FmiInitial> },
    #[error("FMI 3 variable must carry the reinit-false state write modes; found {actual:?}")]
    VariableWriteModes { actual: Fmi3WriteModes },
    #[error(
        "FMI 3 variable ModelStructure membership must be empty; found output {output}, derivative {continuous_state_derivative}, initial unknown {initial_unknown}"
    )]
    VariableModelStructure {
        output: bool,
        continuous_state_derivative: bool,
        initial_unknown: bool,
    },
    #[error(
        "FMI 3 variable derivative link must pair state {expected_state} with derivative {expected_derivative}; found state {actual_state:?}, derivative {actual_derivative:?}"
    )]
    VariableDerivativeLink {
        expected_state: u32,
        expected_derivative: u32,
        actual_state: Option<u32>,
        actual_derivative: Option<u32>,
    },
    #[error("start leg {leg:?} must hold one scalar; present: {present}, width: {width}")]
    StartWidth {
        leg: StartLeg,
        present: bool,
        width: usize,
    },
    #[error(
        "start leg {leg:?} must agree bitwise with the catalog start; expected bits {expected_bits:#018x}, found {actual_bits:#018x}"
    )]
    StartBits {
        leg: StartLeg,
        expected_bits: u64,
        actual_bits: u64,
    },
    #[error("FMI 3 derivative must carry source identity {expected}; found {actual}")]
    DerivativeSourceIdentity { expected: usize, actual: usize },
    #[error(
        "FMI 3 derivative link must pair state {expected_state} with derivative {expected_derivative}; found state {actual_state}, derivative {actual_derivative}"
    )]
    DerivativeLink {
        expected_state: u32,
        expected_derivative: u32,
        actual_state: u32,
        actual_derivative: u32,
    },
    #[error(
        "FMI 3 derivative storage must be the first derivative scalar; found base {base}, scalar count {scalar_count}"
    )]
    DerivativeStorage { base: usize, scalar_count: usize },
    #[error("FMI 3 derivative must be a rank-zero scalar; found rank {rank}")]
    DerivativeDimensions { rank: usize },
    #[error("FMI 3 derivative must carry Local causality; found {actual:?}")]
    DerivativeCausality { actual: FmiCausality },
    #[error("FMI 3 derivative must carry Continuous variability; found {actual:?}")]
    DerivativeVariability { actual: FmiVariability },
    #[error("FMI 3 derivative must carry initial Calculated; found {actual:?}")]
    DerivativeInitial { actual: FmiInitial },
    #[error(
        "FMI 3 derivative ModelStructure membership must be derivative and initial unknown only; found output {output}, derivative {continuous_state_derivative}, initial unknown {initial_unknown}"
    )]
    DerivativeModelStructure {
        output: bool,
        continuous_state_derivative: bool,
        initial_unknown: bool,
    },
    #[error("derivative value-reference base must be {expected}; found {actual}")]
    DerivativeValueReferenceBase { expected: u32, actual: u32 },
    #[error("ModelStructure output inventory must be empty; found {actual:?}")]
    OutputInventory { actual: Vec<u32> },
    #[error("ModelStructure derivative inventory must be exactly [{expected}]; found {actual:?}")]
    DerivativeInventory { expected: u32, actual: Vec<u32> },
    #[error(
        "ModelStructure initial-unknown inventory must be exactly [{expected}]; found {actual:?}"
    )]
    InitialUnknownInventory { expected: u32, actual: Vec<u32> },
    #[error("continuous state scalar count must be one; found {actual}")]
    ContinuousStateScalarCount { actual: usize },
}

/// A refused scalar constant-derivative check.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ScalarConstantDerivativeError {
    #[error("Solve root is outside the scalar constant-derivative profile: {0}")]
    Unsupported(#[from] ScalarConstantDerivativeUnsupported),
    #[error("candidate FMI 3 facts disagree with the admitted Solve facts: {0}")]
    Disagreement(#[from] ScalarConstantDerivativeDisagreement),
}

/// Evidence that one pair of fact arrays passed
/// [`check_scalar_constant_derivative_projection`].
///
/// The receipt certifies exactly the arrays that were checked and records the
/// admitted coordinates. Only the checker constructs it, and it deliberately
/// implements neither `Clone`, `Default`, nor any serialization, so it cannot
/// be duplicated, minted from nothing, or moved across a wire:
///
/// ```compile_fail
/// fn clone_generically<T: Clone>(value: &T) -> T { value.clone() }
/// fn duplicate(receipt: &rumoca_ir_solve::fmi::ScalarConstantDerivativeReceipt) {
///     let _ = clone_generically(receipt);
/// }
/// ```
///
/// ```compile_fail
/// fn mint() -> rumoca_ir_solve::fmi::ScalarConstantDerivativeReceipt {
///     Default::default()
/// }
/// ```
///
/// ```compile_fail
/// fn requires_serialize<T: serde::Serialize>(_: &T) {}
/// fn escape(receipt: &rumoca_ir_solve::fmi::ScalarConstantDerivativeReceipt) {
///     requires_serialize(receipt);
/// }
/// ```
#[derive(Debug)]
pub struct ScalarConstantDerivativeReceipt {
    source_occurrence: SourceOccurrenceId,
    state_value_reference: u32,
    derivative_value_reference: u32,
    start_bits: u64,
    derivative_constant_bits: u64,
}

impl ScalarConstantDerivativeReceipt {
    /// The Flat-issued source occurrence of the one admitted declaration.
    ///
    /// This is carried from the sealed catalog leg of the checked arrays. The
    /// candidate FMI legs cannot represent an occurrence, so the FMI join is
    /// proved over the catalog-issued variable identity instead; the receipt
    /// records which occurrence that identity belongs to.
    #[must_use]
    pub const fn source_occurrence(&self) -> SourceOccurrenceId {
        self.source_occurrence
    }

    #[must_use]
    pub const fn state_value_reference(&self) -> u32 {
        self.state_value_reference
    }

    #[must_use]
    pub const fn derivative_value_reference(&self) -> u32 {
        self.derivative_value_reference
    }

    /// The one bit pattern all five start legs carry.
    #[must_use]
    pub const fn start_bits(&self) -> u64 {
        self.start_bits
    }

    /// The bit pattern of the admitted kernel constant.
    #[must_use]
    pub const fn derivative_constant_bits(&self) -> u64 {
        self.derivative_constant_bits
    }
}

/// The Solve-side facts the profile admits, retained for the candidate half.
struct AdmittedSolveFacts {
    source_occurrence: SourceOccurrenceId,
    source_id_index: usize,
    start_bits: u64,
    derivative_constant_bits: u64,
}

/// Prove one Solve/FMI-3 fact-array pair inside the bounded profile.
///
/// Pure and total over its arguments: no environment, no panic path, no
/// unhandled case. The first half admits the Solve facts into the structural
/// profile or refuses with a typed unsupported capability; the second half
/// proves the candidate facts are the exact projection of the admitted facts
/// or refuses with a typed disagreement.
///
/// The receipt binds only to the arrays checked here. The correlated
/// live claim is minted by [`Fmi3ScalarConstantDerivativeCarrier::admit`],
/// which is the sole caller that projects both arrays from one retained view.
pub fn check_scalar_constant_derivative_projection(
    solve: &ScalarConstantDerivativeSolveFacts,
    candidate: &ScalarConstantDerivativeFmi3Facts,
) -> Result<ScalarConstantDerivativeReceipt, ScalarConstantDerivativeError> {
    let admitted = admit_solve_facts(solve)?;
    check_candidate_facts(&admitted, candidate)
}

fn admit_solve_facts(
    solve: &ScalarConstantDerivativeSolveFacts,
) -> Result<AdmittedSolveFacts, ScalarConstantDerivativeError> {
    use ScalarConstantDerivativeUnsupported as Unsupported;

    admit_arithmetic_and_system(solve)?;
    if solve.variables.len() != 1 {
        return Err(Unsupported::DeclarationCount {
            actual: solve.variables.len(),
        }
        .into());
    }
    let Some(variable) = solve.variables.first() else {
        return Err(Unsupported::DeclarationCount { actual: 0 }.into());
    };
    admit_declaration(variable)?;
    let start_bits = admit_start_agreement(solve, variable)?;
    let derivative_constant_bits = admit_kernel(&solve.kernel)?;
    Ok(AdmittedSolveFacts {
        source_occurrence: variable.source_occurrence,
        source_id_index: variable.source_id_index,
        start_bits,
        derivative_constant_bits,
    })
}

fn admit_arithmetic_and_system(
    solve: &ScalarConstantDerivativeSolveFacts,
) -> Result<(), ScalarConstantDerivativeUnsupported> {
    use ScalarConstantDerivativeUnsupported as Unsupported;

    if solve.real_format != SolveRealFormat::Binary64 {
        return Err(Unsupported::RealFormat {
            actual: solve.real_format,
        });
    }
    let system = &solve.system;
    if system.nonderivative_continuous_owner_count != 0 {
        return Err(Unsupported::NonderivativeContinuousOwners {
            count: system.nonderivative_continuous_owner_count,
        });
    }
    if system.initialization_owner_class_count != 0 {
        return Err(Unsupported::InitializationOwners {
            class_count: system.initialization_owner_class_count,
        });
    }
    if system.discrete_owner_class_count != 0 {
        return Err(Unsupported::DiscreteOwners {
            class_count: system.discrete_owner_class_count,
        });
    }
    if system.event_owner_class_count != 0 {
        return Err(Unsupported::EventOwners {
            class_count: system.event_owner_class_count,
        });
    }
    if system.runtime_event_owner_class_count != 0 {
        return Err(Unsupported::RuntimeEventOwners {
            class_count: system.runtime_event_owner_class_count,
        });
    }
    if system.clock_owner_class_count != 0 {
        return Err(Unsupported::ClockOwners {
            class_count: system.clock_owner_class_count,
        });
    }
    if solve.state_scalar_count != 1 {
        return Err(Unsupported::StateWidth {
            actual: solve.state_scalar_count,
        });
    }
    if solve.y_scalar_count != 1 {
        return Err(Unsupported::SolverWidth {
            actual: solve.y_scalar_count,
        });
    }
    if solve.p_scalar_count != 0 {
        return Err(Unsupported::ParameterWidth {
            actual: solve.p_scalar_count,
        });
    }
    Ok(())
}

fn admit_declaration(
    variable: &SolveScalarVariableFact,
) -> Result<(), ScalarConstantDerivativeUnsupported> {
    use ScalarConstantDerivativeUnsupported as Unsupported;

    if variable.role != SolveVariableStorageRole::State {
        return Err(Unsupported::Role {
            actual: variable.role,
        });
    }
    if variable.value_kind != SolveVariableValueKind::Real {
        return Err(Unsupported::ValueKind {
            actual: variable.value_kind,
        });
    }
    if !variable.dimensions.is_empty() {
        return Err(Unsupported::Dimensions {
            rank: variable.dimensions.len(),
        });
    }
    if variable.scalar_identity_count != 1 {
        return Err(Unsupported::ScalarIdentityCount {
            actual: variable.scalar_identity_count,
        });
    }
    if variable.storage
        != (SolveStorageFact::Y {
            base: 0,
            scalar_count: 1,
        })
    {
        return Err(Unsupported::Storage {
            actual: variable.storage,
        });
    }
    if variable.causality != SolveVariableCausality::Local {
        return Err(Unsupported::Causality {
            actual: variable.causality,
        });
    }
    if variable.variability != SolveVariableVariability::Continuous {
        return Err(Unsupported::Variability {
            actual: variable.variability,
        });
    }
    if variable.fixed != Fixity::Fixed {
        return Err(Unsupported::Fixity {
            actual: variable.fixed,
        });
    }
    if variable.state_initialization != SolveStateInitialization::Exact {
        return Err(Unsupported::StateInitialization {
            actual: variable.state_initialization,
        });
    }
    if variable.tunable {
        return Err(Unsupported::Tunable);
    }
    Ok(())
}

fn admit_start_agreement(
    solve: &ScalarConstantDerivativeSolveFacts,
    variable: &SolveScalarVariableFact,
) -> Result<u64, ScalarConstantDerivativeError> {
    use ScalarConstantDerivativeUnsupported as Unsupported;

    let Some(start_bits) = &variable.start_bits else {
        return Err(Unsupported::CatalogStartShape {
            present: false,
            width: 0,
        }
        .into());
    };
    let [catalog_bits] = start_bits.as_slice() else {
        return Err(Unsupported::CatalogStartShape {
            present: true,
            width: start_bits.len(),
        }
        .into());
    };
    let [runtime_bits] = solve.initial_y_bits.as_slice() else {
        return Err(Unsupported::RuntimeWidth {
            actual: solve.initial_y_bits.len(),
        }
        .into());
    };
    if runtime_bits != catalog_bits {
        return Err(ScalarConstantDerivativeDisagreement::StartBits {
            leg: StartLeg::RuntimeVector,
            expected_bits: *catalog_bits,
            actual_bits: *runtime_bits,
        }
        .into());
    }
    Ok(*catalog_bits)
}

fn admit_kernel(kernel: &DerivativeKernelFacts) -> Result<u64, ScalarConstantDerivativeError> {
    use ScalarConstantDerivativeUnsupported as Unsupported;

    if kernel.tensor_node_count != 0 || kernel.scalar_program_count != 1 {
        return Err(Unsupported::KernelNodeShape {
            scalar_program_count: kernel.scalar_program_count,
            tensor_node_count: kernel.tensor_node_count,
        }
        .into());
    }
    if kernel.output_indices.as_slice() != [0] {
        return Err(Unsupported::KernelOutputIdentity {
            output_indices: kernel.output_indices.clone(),
        }
        .into());
    }
    let [first, second] = kernel.operations.as_slice() else {
        return Err(Unsupported::KernelOperationCount {
            actual: kernel.operations.len(),
        }
        .into());
    };
    let KernelOperationFact::ConstantBits { destination, bits } = first else {
        return Err(Unsupported::KernelOperation {
            position: 0,
            operation: kernel_operation_name(first),
        }
        .into());
    };
    let KernelOperationFact::StoreOutput { source } = second else {
        return Err(Unsupported::KernelOperation {
            position: 1,
            operation: kernel_operation_name(second),
        }
        .into());
    };
    if source != destination {
        return Err(Unsupported::KernelStoreSource {
            destination: *destination,
            source_register: *source,
        }
        .into());
    }
    Ok(*bits)
}

fn check_candidate_facts(
    admitted: &AdmittedSolveFacts,
    candidate: &ScalarConstantDerivativeFmi3Facts,
) -> Result<ScalarConstantDerivativeReceipt, ScalarConstantDerivativeError> {
    use ScalarConstantDerivativeDisagreement as Disagreement;

    if candidate.inventory.len() != 1 {
        return Err(Disagreement::InventoryCount {
            actual: candidate.inventory.len(),
        }
        .into());
    }
    if candidate.variables.len() != 1 {
        return Err(Disagreement::VariableCount {
            actual: candidate.variables.len(),
        }
        .into());
    }
    if candidate.derivatives.len() != 1 {
        return Err(Disagreement::DerivativeCount {
            actual: candidate.derivatives.len(),
        }
        .into());
    }
    let (Some(entry), Some(variable), Some(derivative)) = (
        candidate.inventory.first(),
        candidate.variables.first(),
        candidate.derivatives.first(),
    ) else {
        return Err(Disagreement::InventoryCount { actual: 0 }.into());
    };
    if candidate.state_variable_ordinals.as_slice() != [0] {
        return Err(Disagreement::StateOrdinalInventory {
            actual: candidate.state_variable_ordinals.clone(),
        }
        .into());
    }

    // The checker re-derives the value-reference assignment rule instead of
    // trusting the candidate's numbers as a set: value reference zero is
    // `time`, inventory entry `k` owns `k + 1`, and derivatives start at
    // `inventory length + 1`. A swap that preserves the value-reference
    // multiset therefore still refuses here.
    let expected_state_value_reference = TIME_VALUE_REFERENCE + 1;
    let expected_derivative_value_reference = expected_state_value_reference + 1;

    let state_value_reference = variable.value_reference;
    let derivative_value_reference = derivative.link_derivative_value_reference;
    if state_value_reference == TIME_VALUE_REFERENCE
        || derivative_value_reference == TIME_VALUE_REFERENCE
        || state_value_reference == derivative_value_reference
    {
        return Err(Disagreement::ValueReferenceCollision {
            time: TIME_VALUE_REFERENCE,
            state: state_value_reference,
            derivative: derivative_value_reference,
        }
        .into());
    }

    check_inventory_entry(admitted, entry, expected_state_value_reference)?;
    check_projected_variable(
        admitted,
        variable,
        expected_state_value_reference,
        expected_derivative_value_reference,
    )?;
    check_projected_derivative(
        admitted,
        derivative,
        expected_state_value_reference,
        expected_derivative_value_reference,
    )?;
    check_model_structure_inventories(candidate, expected_derivative_value_reference)?;

    Ok(ScalarConstantDerivativeReceipt {
        source_occurrence: admitted.source_occurrence,
        state_value_reference: expected_state_value_reference,
        derivative_value_reference: expected_derivative_value_reference,
        start_bits: admitted.start_bits,
        derivative_constant_bits: admitted.derivative_constant_bits,
    })
}

fn check_model_structure_inventories(
    candidate: &ScalarConstantDerivativeFmi3Facts,
    expected_derivative_value_reference: u32,
) -> Result<(), ScalarConstantDerivativeError> {
    use ScalarConstantDerivativeDisagreement as Disagreement;

    if candidate.derivative_value_reference_base != expected_derivative_value_reference {
        return Err(Disagreement::DerivativeValueReferenceBase {
            expected: expected_derivative_value_reference,
            actual: candidate.derivative_value_reference_base,
        }
        .into());
    }
    if !candidate.output_value_references.is_empty() {
        return Err(Disagreement::OutputInventory {
            actual: candidate.output_value_references.clone(),
        }
        .into());
    }
    if candidate.derivative_value_references.as_slice() != [expected_derivative_value_reference] {
        return Err(Disagreement::DerivativeInventory {
            expected: expected_derivative_value_reference,
            actual: candidate.derivative_value_references.clone(),
        }
        .into());
    }
    if candidate.initial_unknown_value_references.as_slice()
        != [expected_derivative_value_reference]
    {
        return Err(Disagreement::InitialUnknownInventory {
            expected: expected_derivative_value_reference,
            actual: candidate.initial_unknown_value_references.clone(),
        }
        .into());
    }
    if candidate.continuous_state_scalar_count != 1 {
        return Err(Disagreement::ContinuousStateScalarCount {
            actual: candidate.continuous_state_scalar_count,
        }
        .into());
    }
    Ok(())
}

fn is_first_y_scalar_run(run: FmiStorageRun) -> bool {
    run.column() == SolveStorageColumn::Y && run.base() == 0 && run.scalar_count() == 1
}

fn check_one_start_leg(
    leg: StartLeg,
    expected_bits: u64,
    actual: Option<&[u64]>,
) -> Result<(), ScalarConstantDerivativeDisagreement> {
    let Some(values) = actual else {
        return Err(ScalarConstantDerivativeDisagreement::StartWidth {
            leg,
            present: false,
            width: 0,
        });
    };
    let [actual_bits] = values else {
        return Err(ScalarConstantDerivativeDisagreement::StartWidth {
            leg,
            present: true,
            width: values.len(),
        });
    };
    if *actual_bits != expected_bits {
        return Err(ScalarConstantDerivativeDisagreement::StartBits {
            leg,
            expected_bits,
            actual_bits: *actual_bits,
        });
    }
    Ok(())
}

fn check_inventory_entry(
    admitted: &AdmittedSolveFacts,
    entry: &Fmi3InventoryEntryFact,
    expected_state_value_reference: u32,
) -> Result<(), ScalarConstantDerivativeError> {
    use ScalarConstantDerivativeDisagreement as Disagreement;

    if entry.value_reference != expected_state_value_reference {
        return Err(Disagreement::InventoryValueReference {
            expected: expected_state_value_reference,
            actual: entry.value_reference,
        }
        .into());
    }
    if entry.source_id_index != Some(admitted.source_id_index) {
        return Err(Disagreement::InventorySourceIdentity {
            expected: admitted.source_id_index,
            actual: entry.source_id_index,
        }
        .into());
    }
    if entry.role != Some(SolveVariableStorageRole::State) {
        return Err(Disagreement::InventoryRole { actual: entry.role }.into());
    }
    match entry.storage {
        Some(run) if is_first_y_scalar_run(run) => {}
        actual => return Err(Disagreement::InventoryStorage { actual }.into()),
    }
    if !entry.dimensions.is_empty() {
        return Err(Disagreement::InventoryDimensions {
            rank: entry.dimensions.len(),
        }
        .into());
    }
    if entry.value_kind != SolveVariableValueKind::Real {
        return Err(Disagreement::InventoryValueKind {
            actual: entry.value_kind,
        }
        .into());
    }
    if entry.scalar_identity_count != 1 {
        return Err(Disagreement::InventoryScalarIdentityCount {
            actual: entry.scalar_identity_count,
        }
        .into());
    }
    if entry.causality != FmiCausality::Local {
        return Err(Disagreement::InventoryCausality {
            actual: entry.causality,
        }
        .into());
    }
    if entry.variability != FmiVariability::Continuous {
        return Err(Disagreement::InventoryVariability {
            actual: entry.variability,
        }
        .into());
    }
    if entry.initial != Some(FmiInitial::Exact) {
        return Err(Disagreement::InventoryInitial {
            actual: entry.initial,
        }
        .into());
    }
    if entry.write_policy != SCALAR_STATE_POLICY {
        return Err(Disagreement::InventoryWritePolicy {
            actual: entry.write_policy,
        }
        .into());
    }
    if entry.tunable {
        return Err(Disagreement::InventoryTunability.into());
    }
    check_one_start_leg(
        StartLeg::InventoryEntry,
        admitted.start_bits,
        entry.start_bits.as_deref(),
    )?;
    Ok(())
}

fn check_projected_variable(
    admitted: &AdmittedSolveFacts,
    variable: &Fmi3StateVariableFact,
    expected_state_value_reference: u32,
    expected_derivative_value_reference: u32,
) -> Result<(), ScalarConstantDerivativeError> {
    check_projected_variable_declaration(admitted, variable, expected_state_value_reference)?;
    check_projected_variable_structure_and_starts(
        admitted,
        variable,
        expected_state_value_reference,
        expected_derivative_value_reference,
    )
}

fn check_projected_variable_declaration(
    admitted: &AdmittedSolveFacts,
    variable: &Fmi3StateVariableFact,
    expected_state_value_reference: u32,
) -> Result<(), ScalarConstantDerivativeError> {
    use ScalarConstantDerivativeDisagreement as Disagreement;

    if variable.value_reference != expected_state_value_reference {
        return Err(Disagreement::VariableValueReference {
            expected: expected_state_value_reference,
            actual: variable.value_reference,
        }
        .into());
    }
    if variable.source_id_index != admitted.source_id_index {
        return Err(Disagreement::VariableSourceIdentity {
            expected: admitted.source_id_index,
            actual: variable.source_id_index,
        }
        .into());
    }
    if !is_first_y_scalar_run(variable.storage) {
        return Err(Disagreement::VariableStorage {
            actual: variable.storage,
        }
        .into());
    }
    if !variable.dimensions.is_empty() {
        return Err(Disagreement::VariableDimensions {
            rank: variable.dimensions.len(),
        }
        .into());
    }
    if variable.causality != FmiCausality::Local {
        return Err(Disagreement::VariableCausality {
            actual: variable.causality,
        }
        .into());
    }
    if variable.variability != FmiVariability::Continuous {
        return Err(Disagreement::VariableVariability {
            actual: variable.variability,
        }
        .into());
    }
    if variable.initial != Some(FmiInitial::Exact) {
        return Err(Disagreement::VariableInitial {
            actual: variable.initial,
        }
        .into());
    }
    if variable.write_modes != Fmi3WriteModes::of(SCALAR_STATE_POLICY) {
        return Err(Disagreement::VariableWriteModes {
            actual: variable.write_modes,
        }
        .into());
    }
    Ok(())
}

fn check_projected_variable_structure_and_starts(
    admitted: &AdmittedSolveFacts,
    variable: &Fmi3StateVariableFact,
    expected_state_value_reference: u32,
    expected_derivative_value_reference: u32,
) -> Result<(), ScalarConstantDerivativeError> {
    use ScalarConstantDerivativeDisagreement as Disagreement;

    if variable.output_member
        || variable.continuous_state_derivative_member
        || variable.initial_unknown_member
    {
        return Err(Disagreement::VariableModelStructure {
            output: variable.output_member,
            continuous_state_derivative: variable.continuous_state_derivative_member,
            initial_unknown: variable.initial_unknown_member,
        }
        .into());
    }
    if variable.link_state_value_reference != Some(expected_state_value_reference)
        || variable.link_derivative_value_reference != Some(expected_derivative_value_reference)
    {
        return Err(Disagreement::VariableDerivativeLink {
            expected_state: expected_state_value_reference,
            expected_derivative: expected_derivative_value_reference,
            actual_state: variable.link_state_value_reference,
            actual_derivative: variable.link_derivative_value_reference,
        }
        .into());
    }
    check_one_start_leg(
        StartLeg::ProjectionStart,
        admitted.start_bits,
        variable.xml_start_bits.as_deref(),
    )?;
    check_one_start_leg(
        StartLeg::ProjectionRuntimeStart,
        admitted.start_bits,
        Some(variable.runtime_start_bits.as_slice()),
    )?;
    Ok(())
}

fn check_projected_derivative(
    admitted: &AdmittedSolveFacts,
    derivative: &Fmi3StateDerivativeFact,
    expected_state_value_reference: u32,
    expected_derivative_value_reference: u32,
) -> Result<(), ScalarConstantDerivativeError> {
    use ScalarConstantDerivativeDisagreement as Disagreement;

    if derivative.source_id_index != admitted.source_id_index {
        return Err(Disagreement::DerivativeSourceIdentity {
            expected: admitted.source_id_index,
            actual: derivative.source_id_index,
        }
        .into());
    }
    if derivative.link_state_value_reference != expected_state_value_reference
        || derivative.link_derivative_value_reference != expected_derivative_value_reference
    {
        return Err(Disagreement::DerivativeLink {
            expected_state: expected_state_value_reference,
            expected_derivative: expected_derivative_value_reference,
            actual_state: derivative.link_state_value_reference,
            actual_derivative: derivative.link_derivative_value_reference,
        }
        .into());
    }
    if derivative.storage_base != 0 || derivative.storage_scalar_count != 1 {
        return Err(Disagreement::DerivativeStorage {
            base: derivative.storage_base,
            scalar_count: derivative.storage_scalar_count,
        }
        .into());
    }
    if !derivative.dimensions.is_empty() {
        return Err(Disagreement::DerivativeDimensions {
            rank: derivative.dimensions.len(),
        }
        .into());
    }
    if derivative.causality != FmiCausality::Local {
        return Err(Disagreement::DerivativeCausality {
            actual: derivative.causality,
        }
        .into());
    }
    if derivative.variability != FmiVariability::Continuous {
        return Err(Disagreement::DerivativeVariability {
            actual: derivative.variability,
        }
        .into());
    }
    if derivative.initial != FmiInitial::Calculated {
        return Err(Disagreement::DerivativeInitial {
            actual: derivative.initial,
        }
        .into());
    }
    if derivative.output_member
        || !derivative.continuous_state_derivative_member
        || !derivative.initial_unknown_member
    {
        return Err(Disagreement::DerivativeModelStructure {
            output: derivative.output_member,
            continuous_state_derivative: derivative.continuous_state_derivative_member,
            initial_unknown: derivative.initial_unknown_member,
        }
        .into());
    }
    Ok(())
}

/// Materialize the closed Solve-side fact array from one sealed root.
///
/// Eager and total: unsupported structure becomes a refusable fact rather
/// than an error here, so the checker owns every admission decision.
#[must_use]
pub fn project_scalar_constant_derivative_solve_facts(
    model: &SolveModel,
) -> ScalarConstantDerivativeSolveFacts {
    let problem = model.problem();
    ScalarConstantDerivativeSolveFacts {
        real_format: model.pure_calls().arithmetic().real_format(),
        state_scalar_count: problem.solve_layout().state_scalar_count,
        y_scalar_count: problem.layout().y_scalars(),
        p_scalar_count: problem.layout().p_scalars(),
        system: project_system_facts(problem),
        variables: project_catalog_variables(model),
        initial_y_bits: float_bits(model.initial_y()),
        kernel: project_derivative_kernel(problem.continuous().derivative_rhs()),
    }
}

fn project_system_facts(problem: &crate::SolveProblem) -> ScalarConstantDerivativeSystemFacts {
    let continuous = problem.continuous();
    let owners = continuous.refresh_owners();
    let nonderivative_continuous_owner_count = continuous
        .implicit_rhs()
        .nodes
        .len()
        .saturating_add(continuous.implicit_row_targets().len())
        .saturating_add(continuous.residual().nodes.len())
        .saturating_add(continuous.manifold_residual().nodes.len())
        .saturating_add(owners.algebraic().rows().len())
        .saturating_add(owners.derivative().rows().len())
        .saturating_add(owners.root().rows().len())
        .saturating_add(owners.event().rows().len())
        .saturating_add(owners.clock_events().len());

    let initialization = &problem.initialization;
    let initialization_owner_class_count = usize::from(!initialization.residual().is_empty())
        + usize::from(!initialization.projection_unknowns().is_empty())
        + usize::from(!initialization.projection_plan().is_empty())
        + usize::from(!initialization.update_rhs().is_empty())
        + usize::from(!initialization.update_targets().is_empty());

    let discrete = &problem.discrete;
    let discrete_owner_class_count = usize::from(!discrete.event_iteration_plan.runs.is_empty())
        + usize::from(!discrete.runtime_assignment_rhs.is_empty())
        + usize::from(!discrete.post_commit_assignment_rhs.is_empty())
        + usize::from(!discrete.rhs.is_empty())
        + usize::from(!discrete.structured_rhs.is_empty())
        + usize::from(!discrete.structured_updates.is_empty())
        + usize::from(!discrete.guarded_assignments.is_empty())
        + usize::from(!discrete.update_targets.is_empty());

    let events = &problem.events;
    let event_owner_class_count = usize::from(!events.root_conditions.is_empty())
        + usize::from(!events.condition_memory_parameter_indices.is_empty())
        + usize::from(!events.scheduled_root_conditions.is_empty())
        + usize::from(!events.scheduled_time_events.is_empty())
        + usize::from(!events.dynamic_time_event_names.is_empty())
        + usize::from(!events.dynamic_time_event_rhs.is_empty())
        + usize::from(!events.action_conditions.is_empty())
        + usize::from(!events.actions.is_empty());

    let delays = &events.delays;
    let runtime_event_owner_class_count = usize::from(events.has_terminal_event)
        + usize::from(!delays.source_rhs.is_empty())
        + usize::from(!delays.delay_time_rhs.is_empty())
        + usize::from(!delays.delay_max_rhs.is_empty())
        + usize::from(!delays.value_parameter_indices.is_empty());

    let clocks = &problem.clocks;
    let clock_owner_class_count = usize::from(!clocks.periodic_event_schedules.is_empty())
        + usize::from(!clocks.activation_parameter_indices.is_empty())
        + usize::from(discrete.clock_owners.iter().any(Option::is_some))
        + usize::from(
            discrete
                .structured_updates
                .iter()
                .any(|update| update.clock_owner.is_some()),
        );

    ScalarConstantDerivativeSystemFacts {
        nonderivative_continuous_owner_count,
        initialization_owner_class_count,
        discrete_owner_class_count,
        event_owner_class_count,
        runtime_event_owner_class_count,
        clock_owner_class_count,
    }
}

fn project_catalog_variables(model: &SolveModel) -> Vec<SolveScalarVariableFact> {
    let mut variables = Vec::with_capacity(model.variable_catalog().len());
    for entry in model.variable_catalog().entries() {
        let run = entry.storage();
        let storage = match run.base {
            SolveStorageCoordinate::Y(index) => SolveStorageFact::Y {
                base: index,
                scalar_count: run.scalar_count,
            },
            SolveStorageCoordinate::P(index) => SolveStorageFact::P {
                base: index,
                scalar_count: run.scalar_count,
            },
        };
        variables.push(SolveScalarVariableFact {
            source_occurrence: entry.source_occurrence(),
            source_id_index: entry.id().index(),
            role: entry.role(),
            causality: entry.causality(),
            variability: entry.variability(),
            fixed: entry.fixed(),
            state_initialization: entry.state_initialization(),
            value_kind: entry.value_kind(),
            dimensions: entry.dimensions().to_vec(),
            scalar_identity_count: entry.scalar_names().len(),
            storage,
            tunable: entry.is_tunable(),
            start_bits: entry.start().map(float_bits),
        });
    }
    variables
}

fn project_derivative_kernel(block: &crate::ComputeBlock) -> DerivativeKernelFacts {
    let mut kernel = DerivativeKernelFacts {
        scalar_program_count: 0,
        tensor_node_count: 0,
        output_indices: Vec::new(),
        operations: Vec::new(),
    };
    for node in &block.nodes {
        match node {
            ComputeNode::ScalarPrograms(programs) => {
                append_scalar_program_facts(&mut kernel, programs);
            }
            ComputeNode::MatMul { .. }
            | ComputeNode::LinSolve { .. }
            | ComputeNode::Map { .. }
            | ComputeNode::AffineStencil { .. } => {
                kernel.tensor_node_count = kernel.tensor_node_count.saturating_add(1);
            }
        }
    }
    kernel
}

fn append_scalar_program_facts(
    kernel: &mut DerivativeKernelFacts,
    programs: &crate::ScalarProgramBlock,
) {
    kernel.scalar_program_count = kernel
        .scalar_program_count
        .saturating_add(programs.programs().len());
    kernel
        .output_indices
        .extend_from_slice(programs.output_indices());
    for program in programs.programs() {
        for operation in program {
            kernel.operations.push(project_kernel_operation(operation));
        }
    }
}

fn project_kernel_operation(operation: &LinearOp) -> KernelOperationFact {
    match operation {
        LinearOp::Const { dst, value } => KernelOperationFact::ConstantBits {
            destination: *dst,
            bits: value.to_bits(),
        },
        LinearOp::StoreOutput { src } => KernelOperationFact::StoreOutput { source: *src },
        other => KernelOperationFact::Unsupported {
            operation: other.kind_name(),
        },
    }
}

fn float_bits(values: &[f64]) -> Vec<u64> {
    let mut bits = Vec::with_capacity(values.len());
    for value in values {
        bits.push(value.to_bits());
    }
    bits
}

/// Materialize the closed candidate FMI 3 fact array from one checked
/// event-free view.
#[must_use]
pub fn project_scalar_constant_derivative_fmi3_facts(
    view: &FmiEventFreeCodegenView,
) -> ScalarConstantDerivativeFmi3Facts {
    let metadata = view.metadata();
    let fmi3 = view.fmi3();

    let mut inventory = Vec::with_capacity(metadata.variables().len());
    for variable in metadata.variables() {
        inventory.push(Fmi3InventoryEntryFact {
            source_id_index: variable.source_id().map(crate::SolveVariableId::index),
            value_reference: variable.value_reference_fmi3(),
            role: variable.role(),
            storage: variable.storage(),
            dimensions: variable.dimensions().to_vec(),
            value_kind: variable.value_kind(),
            scalar_identity_count: variable.scalar_names().len(),
            causality: variable.causality(),
            variability: variable.variability(),
            initial: variable.initial(),
            write_policy: variable.write_policy(),
            tunable: variable.is_tunable(),
            start_bits: variable.start().map(float_bits),
        });
    }

    let mut variables = Vec::with_capacity(fmi3.variables().len());
    for variable in fmi3.variables() {
        let structure = variable.model_structure();
        variables.push(Fmi3StateVariableFact {
            source_id_index: variable.source_id().index(),
            value_reference: variable.value_reference(),
            storage: variable.storage(),
            dimensions: variable.dimensions().to_vec(),
            causality: variable.causality(),
            variability: variable.variability(),
            initial: variable.initial(),
            write_modes: variable.write_modes(),
            output_member: structure.output(),
            continuous_state_derivative_member: structure.continuous_state_derivative(),
            initial_unknown_member: structure.initial_unknown(),
            link_state_value_reference: variable
                .derivative()
                .map(super::FmiDerivativeLink::state_value_reference),
            link_derivative_value_reference: variable
                .derivative()
                .map(super::FmiDerivativeLink::derivative_value_reference),
            xml_start_bits: variable.start().map(float_bits),
            runtime_start_bits: float_bits(variable.runtime_start()),
        });
    }

    let mut derivatives = Vec::with_capacity(fmi3.derivatives().len());
    for derivative in fmi3.derivatives() {
        let structure = derivative.model_structure();
        derivatives.push(Fmi3StateDerivativeFact {
            source_id_index: derivative.source_id().index(),
            link_state_value_reference: derivative.link().state_value_reference(),
            link_derivative_value_reference: derivative.link().derivative_value_reference(),
            storage_base: derivative.storage().base(),
            storage_scalar_count: derivative.storage().scalar_count(),
            dimensions: derivative.dimensions().to_vec(),
            causality: derivative.causality(),
            variability: derivative.variability(),
            initial: derivative.initial(),
            output_member: structure.output(),
            continuous_state_derivative_member: structure.continuous_state_derivative(),
            initial_unknown_member: structure.initial_unknown(),
        });
    }

    ScalarConstantDerivativeFmi3Facts {
        inventory,
        state_variable_ordinals: metadata.state_variable_indices().to_vec(),
        derivative_value_reference_base: metadata.derivative_value_reference_base_fmi3(),
        variables,
        derivatives,
        output_value_references: fmi3.output_value_references().to_vec(),
        derivative_value_references: fmi3.derivative_value_references().to_vec(),
        initial_unknown_value_references: fmi3.initial_unknown_value_references().to_vec(),
        continuous_state_scalar_count: fmi3.continuous_state_scalar_count(),
    }
}

/// The checked FMI 3 carrier of one admitted scalar constant-derivative
/// kernel.
///
/// This is the correlated claim: the sole constructor projects the Solve
/// facts and the candidate FMI 3 facts from the same retained event-free view
/// and retains the checker's receipt beside it. A receipt minted over
/// independently supplied fact arrays cannot enter this type, and no
/// constructor accepts one.
///
/// Deliberately not `Clone` and not `Default`:
///
/// ```compile_fail
/// fn clone_generically<T: Clone>(value: &T) -> T { value.clone() }
/// fn duplicate(carrier: &rumoca_ir_solve::fmi::Fmi3ScalarConstantDerivativeCarrier) {
///     let _ = clone_generically(carrier);
/// }
/// ```
///
/// ```compile_fail
/// fn mint() -> rumoca_ir_solve::fmi::Fmi3ScalarConstantDerivativeCarrier {
///     Default::default()
/// }
/// ```
///
/// The checked view cannot be extracted while discarding its receipt:
///
/// ```compile_fail
/// fn erase_proof(
///     carrier: rumoca_ir_solve::fmi::Fmi3ScalarConstantDerivativeCarrier,
/// ) -> rumoca_ir_solve::fmi::FmiEventFreeCodegenView {
///     carrier.into_event_free()
/// }
/// ```
#[derive(Debug)]
pub struct Fmi3ScalarConstantDerivativeCarrier {
    view: FmiEventFreeCodegenView,
    receipt: ScalarConstantDerivativeReceipt,
}

impl Fmi3ScalarConstantDerivativeCarrier {
    /// Admit one event-free view into the bounded profile, or refuse with the
    /// exact typed unsupported capability or disagreement.
    ///
    /// Both fact arrays are projected here, from the one kernel and inventory
    /// the view retains, before the pure checker proves their relation. A
    /// refused view is dropped; there is no fallback claim.
    pub fn admit(view: FmiEventFreeCodegenView) -> Result<Self, ScalarConstantDerivativeError> {
        let solve_facts = project_scalar_constant_derivative_solve_facts(view.solve_model());
        let fmi3_facts = project_scalar_constant_derivative_fmi3_facts(&view);
        let receipt = check_scalar_constant_derivative_projection(&solve_facts, &fmi3_facts)?;
        Ok(Self { view, receipt })
    }

    /// The retained event-free view the facts were projected from.
    #[must_use]
    pub const fn event_free(&self) -> &FmiEventFreeCodegenView {
        &self.view
    }

    /// The retained checker receipt for this exact carrier.
    #[must_use]
    pub const fn receipt(&self) -> &ScalarConstantDerivativeReceipt {
        &self.receipt
    }
}
