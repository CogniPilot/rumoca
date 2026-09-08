use super::*;
use std::collections::BTreeSet;
use std::sync::Arc;

mod clock_partition;
mod event_transaction;

pub use event_transaction::*;

/// One checked continuous Solve system with its complete refresh ownership.
///
/// Generic construction cannot issue the refresh relations required by the
/// executable system:
///
/// ```compile_fail
/// use rumoca_ir_solve::ContinuousSolveSystem;
///
/// let _ = ContinuousSolveSystem::default();
/// ```
///
/// A checked owner cannot be installed after a continuous system escapes:
///
/// ```compile_fail
/// use rumoca_ir_solve::{ContinuousRefreshOwners, ContinuousSolveSystem};
///
/// fn patch(system: &mut ContinuousSolveSystem, owners: ContinuousRefreshOwners) {
///     system.refresh_owners = owners;
/// }
/// ```
///
/// Replacing the canonical program after refresh issuance is equally closed,
/// even when the replacement has the same output shape:
///
/// ```compile_fail
/// use rumoca_ir_solve::{ComputeBlock, ContinuousSolveSystem};
///
/// fn replace_source(system: &mut ContinuousSolveSystem, same_shape: ComputeBlock) {
///     system.implicit_rhs = same_shape;
/// }
/// ```
#[derive(Clone, Debug, Serialize)]
pub struct ContinuousSolveSystem {
    pub(crate) implicit_rhs: ComputeBlock,
    pub(crate) implicit_row_targets: Vec<Option<ScalarSlot>>,
    pub(crate) algebraic_projection_plan: AlgebraicProjectionPlan,
    pub(crate) residual: ComputeBlock,
    /// Lower-order holonomic and velocity residuals retained when structural
    /// index reduction replaces them with acceleration-level equations.
    pub(crate) manifold_residual: ComputeBlock,
    /// Connected state-coordinate blocks used to project accepted numerical
    /// steps onto `manifold_residual = 0`. Blocks may have more state
    /// coordinates than residual rows; runtimes use a minimum-norm correction.
    pub(crate) manifold_projection_plan: AlgebraicProjectionPlan,
    pub(crate) derivative_rhs: ComputeBlock,
    /// Exact checked refresh owners issued during Solve construction. Runtime
    /// adapters prepare these schedules but never discover or filter them.
    pub(crate) refresh_owners: ContinuousRefreshOwners,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ContinuousSolveSystemWire {
    pub(super) implicit_rhs: ComputeBlock,
    pub(super) implicit_row_targets: Vec<Option<ScalarSlot>>,
    pub(super) algebraic_projection_plan: AlgebraicProjectionPlan,
    pub(super) residual: ComputeBlock,
    pub(super) manifold_residual: ComputeBlock,
    pub(super) manifold_projection_plan: AlgebraicProjectionPlan,
    pub(super) derivative_rhs: ComputeBlock,
    pub(super) refresh_owners: ContinuousRefreshOwnersWire,
}

/// Unissued continuous-system components consumed atomically with one exact
/// [`SolveLayout`]. Fields remain private so callers cannot patch a partially
/// assembled input after construction.
///
/// ```compile_fail
/// use rumoca_ir_solve::ContinuousSolveSystemInputs;
///
/// let _ = ContinuousSolveSystemInputs::default();
/// ```
#[derive(Clone, Debug)]
pub struct ContinuousSolveSystemInputs {
    implicit_rhs: ComputeBlock,
    implicit_row_targets: Vec<Option<ScalarSlot>>,
    algebraic_projection_plan: AlgebraicProjectionPlan,
    residual: ComputeBlock,
    manifold: (ComputeBlock, AlgebraicProjectionPlan),
    derivative_rhs: ComputeBlock,
    refresh_plans: ContinuousRefreshPlanInputs,
}

impl ContinuousSolveSystemInputs {
    #[must_use]
    pub fn new(
        implicit_rhs: ComputeBlock,
        implicit_row_targets: Vec<Option<ScalarSlot>>,
        algebraic_projection_plan: AlgebraicProjectionPlan,
        residual: ComputeBlock,
        manifold: (ComputeBlock, AlgebraicProjectionPlan),
        derivative_rhs: ComputeBlock,
        refresh_plans: ContinuousRefreshPlanInputs,
    ) -> Self {
        Self {
            implicit_rhs,
            implicit_row_targets,
            algebraic_projection_plan,
            residual,
            manifold,
            derivative_rhs,
            refresh_plans,
        }
    }
}

impl ContinuousSolveSystem {
    #[must_use]
    pub const fn implicit_rhs(&self) -> &ComputeBlock {
        &self.implicit_rhs
    }

    #[must_use]
    pub fn implicit_row_targets(&self) -> &[Option<ScalarSlot>] {
        &self.implicit_row_targets
    }

    #[must_use]
    pub const fn algebraic_projection_plan(&self) -> &AlgebraicProjectionPlan {
        &self.algebraic_projection_plan
    }

    #[must_use]
    pub const fn residual(&self) -> &ComputeBlock {
        &self.residual
    }

    #[must_use]
    pub const fn manifold_residual(&self) -> &ComputeBlock {
        &self.manifold_residual
    }

    #[must_use]
    pub const fn manifold_projection_plan(&self) -> &AlgebraicProjectionPlan {
        &self.manifold_projection_plan
    }

    #[must_use]
    pub const fn derivative_rhs(&self) -> &ComputeBlock {
        &self.derivative_rhs
    }

    #[must_use]
    pub const fn refresh_owners(&self) -> &ContinuousRefreshOwners {
        &self.refresh_owners
    }

    /// Construct the complete continuous system only after refresh ownership
    /// has been checked against this exact canonical implicit program.
    pub fn construct(
        solve_layout: &SolveLayout,
        inputs: ContinuousSolveSystemInputs,
    ) -> Result<Self, ContinuousRefreshConstructionError> {
        let ContinuousSolveSystemInputs {
            implicit_rhs,
            implicit_row_targets,
            algebraic_projection_plan,
            residual,
            manifold,
            derivative_rhs,
            refresh_plans,
        } = inputs;
        let refresh_owners = ContinuousRefreshOwners::checked_for_source(
            &implicit_rhs,
            &implicit_row_targets,
            &algebraic_projection_plan,
            solve_layout,
            refresh_plans,
        )?;
        Ok(Self {
            implicit_rhs,
            implicit_row_targets,
            algebraic_projection_plan,
            residual,
            manifold_residual: manifold.0,
            manifold_projection_plan: manifold.1,
            derivative_rhs,
            refresh_owners,
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct AlgebraicProjectionPlan {
    pub blocks: Vec<AlgebraicProjectionBlock>,
}

impl AlgebraicProjectionPlan {
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct AlgebraicProjectionBlock {
    pub rows: Vec<usize>,
    pub y_indices: Vec<usize>,
    /// Structural tearing of this coupled block. When present, the runtime
    /// projection iterates Newton only over the tear variables and recovers
    /// the remaining unknowns by ordered back-substitution, matching the
    /// causalized solve OpenModelica performs. Absence selects the dense
    /// block Newton over every unknown.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub tearing: Option<BlockTearing>,
}

/// Tearing of one coupled algebraic block into a reduced iteration set plus an
/// ordered back-substitution, expressed in the same solver-index space as the
/// enclosing [`AlgebraicProjectionBlock`].
#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct BlockTearing {
    /// Solver-Y indices iterated by the reduced Newton. A subset of the
    /// block's `y_indices`, equal in count to `residual_rows`.
    pub tear_y_indices: Vec<usize>,
    /// Residual rows forming the reduced Newton system driven to zero over the
    /// tear variables. A subset of the block's `rows`.
    pub residual_rows: Vec<usize>,
    /// Back-substitution steps evaluated in order once the tear variables are
    /// fixed: each step solves its residual row for its unknown.
    pub causal_steps: Vec<CausalStep>,
}

/// One back-substitution step: solve `row` for solver-Y unknown `y_index`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct CausalStep {
    pub row: usize,
    pub y_index: usize,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationProjectionPlan {
    pub blocks: Vec<InitializationProjectionBlock>,
}

impl InitializationProjectionPlan {
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationProjectionBlock {
    pub rows: Vec<usize>,
    /// Initialization unknowns may reside in either solver Y storage or
    /// parameter P storage.  Time and constant slots are invalid here.
    pub unknowns: Vec<ScalarSlot>,
}

/// Untrusted derived-program inputs consumed only by [`SolveModel::construct`].
///
/// Intentional absence of optional derivative products is named explicitly;
/// generic default construction cannot silently select an artifact ABI:
///
/// ```compile_fail
/// use rumoca_ir_solve::SolveArtifactInputs;
///
/// let _ = SolveArtifactInputs::default();
/// ```
#[derive(Clone, Debug)]
pub struct SolveArtifactInputs {
    pub continuous: ContinuousSolveArtifacts,
    pub initialization: InitializationSolveArtifacts,
}

impl SolveArtifactInputs {
    /// Explicit artifact input containing no optional derivative products.
    #[must_use]
    pub fn empty() -> Self {
        Self {
            continuous: ContinuousSolveArtifacts::default(),
            initialization: InitializationSolveArtifacts::default(),
        }
    }
}

/// Construction-sealed derived programs correlated with one exact Solve root.
///
/// Fields are deliberately private: only the `SolveModel` join can issue this
/// aggregate after local child-shape and exact row-coverage proofs.
#[derive(Debug)]
pub struct SolveArtifacts {
    continuous: ContinuousSolveArtifacts,
    initialization: InitializationSolveArtifacts,
}

impl SolveArtifacts {
    #[must_use]
    pub const fn continuous(&self) -> &ContinuousSolveArtifacts {
        &self.continuous
    }

    #[must_use]
    pub const fn initialization(&self) -> &InitializationSolveArtifacts {
        &self.initialization
    }
}

fn validate_artifact_child_shapes(
    artifacts: &SolveArtifactInputs,
) -> Result<(), SolveModelConstructionError> {
    for (context, block) in [
        (
            "continuous.implicit_jacobian_v",
            &artifacts.continuous.implicit_jacobian_v,
        ),
        (
            "continuous.manifold_jacobian_v",
            &artifacts.continuous.manifold_jacobian_v,
        ),
        (
            "initialization.residual_jacobian_v",
            &artifacts.initialization.residual_jacobian_v,
        ),
    ] {
        block.validate_shape_contract(context)?;
    }
    Ok(())
}

fn validate_artifact_exact_row_coverage(
    problem: &SolveProblem,
    artifacts: &SolveArtifactInputs,
) -> Result<(), SolveModelConstructionError> {
    for (artifact, block, source) in [
        (
            "continuous.implicit_jacobian_v",
            &artifacts.continuous.implicit_jacobian_v,
            &problem.continuous().implicit_rhs,
        ),
        (
            "continuous.manifold_jacobian_v",
            &artifacts.continuous.manifold_jacobian_v,
            &problem.continuous().manifold_residual,
        ),
        (
            "initialization.residual_jacobian_v",
            &artifacts.initialization.residual_jacobian_v,
            &problem.initialization().residual,
        ),
    ] {
        validate_compute_artifact_exact_coverage(artifact, block, source)?;
    }
    for (artifact, block, source) in [
        (
            "continuous.implicit_jacobian_v_scalar",
            &artifacts.continuous.implicit_jacobian_v_scalar,
            &problem.continuous().implicit_rhs,
        ),
        (
            "continuous.full_jacobian_v",
            &artifacts.continuous.full_jacobian_v,
            &problem.continuous().derivative_rhs,
        ),
    ] {
        let expected = source
            .produced_output_indices(artifact)
            .map_err(SolveModelConstructionError::from)?;
        validate_scalar_artifact_exact_coverage(artifact, block, &expected)?;
    }
    Ok(())
}

fn validate_compute_artifact_exact_coverage(
    artifact: &'static str,
    block: &ComputeBlock,
    source: &ComputeBlock,
) -> Result<(), SolveModelConstructionError> {
    let actual = block
        .produced_output_indices(artifact)
        .map_err(|error| artifact_coverage_error(artifact, error))?;
    let expected = source
        .produced_output_indices(artifact)
        .map_err(SolveModelConstructionError::from)?;
    validate_artifact_row_inventory(artifact, &actual, &expected)
}

fn validate_scalar_artifact_exact_coverage(
    artifact: &'static str,
    block: &ScalarProgramBlock,
    expected: &[usize],
) -> Result<(), SolveModelConstructionError> {
    let mut outputs = block.output_indices().to_vec();
    outputs.sort_unstable();
    validate_artifact_row_inventory(artifact, &outputs, expected)
}

fn validate_artifact_row_inventory(
    artifact: &'static str,
    actual: &[usize],
    expected: &[usize],
) -> Result<(), SolveModelConstructionError> {
    let mismatch = actual
        .iter()
        .zip(expected)
        .position(|(actual, expected)| actual != expected)
        .unwrap_or_else(|| actual.len().min(expected.len()));
    if actual == expected {
        return Ok(());
    }
    Err(SolveModelConstructionError::ArtifactOutputCoverage {
        artifact,
        index: mismatch,
        detail: "artifact rows do not exactly match their source-row inventory",
    })
}

fn artifact_coverage_error(
    artifact: &'static str,
    error: SolveProblemShapeContractError,
) -> SolveModelConstructionError {
    match error {
        SolveProblemShapeContractError::DerivativeOutputCoverage { kind, index, .. } => {
            let detail = match kind {
                DerivativeOutputCoverageKind::NonInjective => "native output map is non-injective",
                DerivativeOutputCoverageKind::Overlap => "output ownership overlaps",
                DerivativeOutputCoverageKind::Hole => "logical output row is unowned",
            };
            SolveModelConstructionError::ArtifactOutputCoverage {
                artifact,
                index,
                detail,
            }
        }
        SolveProblemShapeContractError::SolverIndexOutOfBounds { index, .. } => {
            SolveModelConstructionError::ArtifactOutputCoverage {
                artifact,
                index,
                detail: "logical output row is out of range",
            }
        }
        SolveProblemShapeContractError::ScalarProgramCountMismatch {
            expected, actual, ..
        } => SolveModelConstructionError::ArtifactOutputCoverage {
            artifact,
            index: expected.min(actual),
            detail: "declared output cardinality does not match its source",
        },
        other => SolveModelConstructionError::from(other),
    }
}

#[derive(Clone, Debug)]
pub struct JacobianStructure {
    pattern: StructuralPattern,
    coloring: ColumnColoring,
}

impl JacobianStructure {
    pub fn derived(pattern: StructuralPattern) -> Self {
        let coloring = pattern.column_coloring();
        Self { pattern, coloring }
    }

    pub const fn pattern(&self) -> &StructuralPattern {
        &self.pattern
    }

    pub const fn coloring(&self) -> &ColumnColoring {
        &self.coloring
    }
}

#[derive(Clone, Debug, Default)]
pub struct ContinuousStructuralArtifacts {
    implicit: Option<JacobianStructure>,
    algebraic_projection: Box<[JacobianStructure]>,
    algebraic_invalidates_earlier: Box<[bool]>,
    manifold: Option<JacobianStructure>,
    manifold_projection: Box<[JacobianStructure]>,
    derivative: Option<JacobianStructure>,
}

impl ContinuousStructuralArtifacts {
    pub fn derived(
        implicit: Option<StructuralPattern>,
        algebraic_projection: Vec<StructuralPattern>,
        algebraic_invalidates_earlier: Vec<bool>,
        manifold: Option<StructuralPattern>,
        manifold_projection: Vec<StructuralPattern>,
        derivative: Option<StructuralPattern>,
    ) -> Self {
        Self {
            implicit: implicit.map(JacobianStructure::derived),
            algebraic_projection: algebraic_projection
                .into_iter()
                .map(JacobianStructure::derived)
                .collect(),
            algebraic_invalidates_earlier: algebraic_invalidates_earlier.into_boxed_slice(),
            manifold: manifold.map(JacobianStructure::derived),
            manifold_projection: manifold_projection
                .into_iter()
                .map(JacobianStructure::derived)
                .collect(),
            derivative: derivative.map(JacobianStructure::derived),
        }
    }

    pub const fn implicit(&self) -> Option<&JacobianStructure> {
        self.implicit.as_ref()
    }

    pub fn algebraic_projection(&self) -> &[JacobianStructure] {
        &self.algebraic_projection
    }

    pub fn algebraic_invalidates_earlier(&self, block_index: usize) -> Option<bool> {
        self.algebraic_invalidates_earlier.get(block_index).copied()
    }

    pub const fn manifold(&self) -> Option<&JacobianStructure> {
        self.manifold.as_ref()
    }

    pub fn manifold_projection(&self) -> &[JacobianStructure] {
        &self.manifold_projection
    }

    pub const fn derivative(&self) -> Option<&JacobianStructure> {
        self.derivative.as_ref()
    }
}

#[derive(Clone, Debug, Default)]
pub struct InitializationStructuralArtifacts {
    residual: Option<JacobianStructure>,
    projection: Box<[JacobianStructure]>,
}

impl InitializationStructuralArtifacts {
    pub fn derived(
        residual: Option<StructuralPattern>,
        projection: Vec<StructuralPattern>,
    ) -> Self {
        Self {
            residual: residual.map(JacobianStructure::derived),
            projection: projection
                .into_iter()
                .map(JacobianStructure::derived)
                .collect(),
        }
    }

    pub const fn residual(&self) -> Option<&JacobianStructure> {
        self.residual.as_ref()
    }

    pub fn projection(&self) -> &[JacobianStructure] {
        &self.projection
    }
}

/// Compact solver-facing mass-matrix representation.
///
/// The matrix dimension is the state scalar count in the accompanying
/// [`SolveLayout`]. Identity therefore needs no payload, while general sparse
/// matrices retain only their nonzero entries.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum MassMatrix {
    #[default]
    Identity,
    Diagonal {
        values: Vec<f64>,
    },
    Sparse {
        entries: Vec<MassMatrixEntry>,
    },
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
pub struct MassMatrixEntry {
    pub row: usize,
    pub column: usize,
    pub value: f64,
}

#[derive(Clone, Debug, Default)]
pub struct ContinuousSolveArtifacts {
    /// Constructor-derived metadata; canonical Solve replay reconstructs it.
    pub structural: ContinuousStructuralArtifacts,
    pub mass_matrix: MassMatrix,
    pub implicit_jacobian_v: ComputeBlock,
    /// Per-row forward-mode AD JVP of the *scalarized* `implicit_rhs`, row-aligned
    /// with successful `to_scalar_program_block(implicit_rhs)` output (and hence
    /// with the algebraic refresh plan's `row_idx`). Used by the state-only path
    /// to propagate the state seed through the algebraic projection
    /// (`d(alg)/d(state)`). Distinct from the tensor `implicit_jacobian_v`, whose
    /// scalarization is not row-aligned when the system has linear
    /// (`LinSolve`/`MatMul`) blocks.
    pub implicit_jacobian_v_scalar: ScalarProgramBlock,
    /// Forward-mode state Jacobian-vector product for
    /// [`ContinuousSolveSystem::manifold_residual`].
    pub manifold_jacobian_v: ComputeBlock,
    pub full_jacobian_v: ScalarProgramBlock,
}

#[derive(Clone, Debug, Default)]
pub struct InitializationSolveArtifacts {
    /// Constructor-derived metadata; canonical Solve replay reconstructs it.
    pub structural: InitializationStructuralArtifacts,
    pub residual_jacobian_v: ComputeBlock,
}

/// The one executable MLS §8.6 initialization aggregate.
///
/// Fields are private and every part is minted together through
/// [`InitializationSolveSystem::construct`], which proves the bidirectional
/// correlation among row obligations, row roles, row targets, projection
/// blocks, the unknown inventory, and the update targets. Neither direct
/// callers nor decoded wire bytes can hold an initialization system whose
/// parts disagree: deserialization replays the same constructor instead of
/// trusting the bytes. There is deliberately no `Default`; the empty system is
/// the explicit [`InitializationSolveSystem::empty`] value, whose every
/// correlation is vacuous over zero rows.
#[derive(Clone, Debug, Serialize)]
pub struct InitializationSolveSystem {
    pub(crate) residual: ComputeBlock,
    pub(crate) row_targets: Vec<Option<ScalarSlot>>,
    /// What the initialization projection does with each residual row, indexed
    /// by equation index alongside `row_targets`. Roles are total: every
    /// retained row is either solved for exactly one projection unknown or is
    /// the stated-value agreement check a structural proof minted. A row
    /// outside that vocabulary is a Solve construction error, never a
    /// representable executable state.
    pub(crate) row_roles: Vec<InitializationRowRole>,
    /// The row-obligation witness: rows `0..mandatory_row_count` are the
    /// mandatory source rows (initial equations and initial-algorithm
    /// residuals), and rows at or past it are the structurally minted carried
    /// stated-value rows, which construction always appends last. A mandatory
    /// row must be solved; only a carried row may stand as a check. This is
    /// what lets wire replay refuse a mutation that erases a mandatory source
    /// row's owner by relabeling it as a check.
    pub(crate) mandatory_row_count: usize,
    pub(crate) projection_unknowns: Vec<ScalarSlot>,
    pub(crate) projection_plan: InitializationProjectionPlan,
    pub(crate) update_rhs: ScalarProgramBlock,
    pub(crate) update_targets: Vec<ScalarSlot>,
}

/// Decoded initialization bytes before the constructor has replayed its proof.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct InitializationSolveSystemWire {
    pub(crate) residual: ComputeBlock,
    pub(crate) row_targets: Vec<Option<ScalarSlot>>,
    pub(crate) row_roles: Vec<InitializationRowRole>,
    pub(crate) mandatory_row_count: usize,
    pub(crate) projection_unknowns: Vec<ScalarSlot>,
    pub(crate) projection_plan: InitializationProjectionPlan,
    pub(crate) update_rhs: ScalarProgramBlock,
    pub(crate) update_targets: Vec<ScalarSlot>,
}

impl InitializationSolveSystemWire {
    /// Reissue the constructor proof for decoded bytes.
    pub(crate) fn replay(
        self,
    ) -> Result<InitializationSolveSystem, SolveProblemShapeContractError> {
        InitializationSolveSystem::construct(
            self.residual,
            self.row_targets,
            self.row_roles,
            self.mandatory_row_count,
            self.projection_unknowns,
            self.projection_plan,
            (self.update_rhs, self.update_targets),
        )
    }
}

/// Refuse non-writable operands at initialization's owning boundary.
fn writable_storage_coordinate(
    slot: ScalarSlot,
    what: &str,
) -> Result<SolveStorageCoordinate, SolveProblemShapeContractError> {
    slot.storage_coordinate().ok_or_else(|| {
        initialization_correlation(format!(
            "{what} names {slot:?}, which has no writable runtime storage"
        ))
    })
}

fn initialization_correlation(detail: String) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::InitializationCorrelation { detail }
}

fn claim_initialization_projection_owner(
    owner_of_row: &mut [Option<ScalarSlot>],
    unknown_identities: &mut BTreeSet<SolveStorageCoordinate>,
    unknowns_in_order: &mut Vec<ScalarSlot>,
    row_count: usize,
    block: usize,
    row: usize,
    unknown: ScalarSlot,
) -> Result<(), SolveProblemShapeContractError> {
    let Some(entry) = owner_of_row.get_mut(row) else {
        return Err(initialization_correlation(format!(
            "projection block {block} names residual row {row}, but the system has only \
             {row_count} row(s)"
        )));
    };
    if entry.is_some() {
        return Err(initialization_correlation(format!(
            "residual row {row} is claimed by two projection blocks"
        )));
    }
    let identity = writable_storage_coordinate(
        unknown,
        &format!("the unknown of projection block {block} for row {row}"),
    )?;
    if !unknown_identities.insert(identity) {
        return Err(initialization_correlation(format!(
            "projection unknown {identity:?} is claimed by two block positions"
        )));
    }
    *entry = Some(unknown);
    unknowns_in_order.push(unknown);
    Ok(())
}

fn validate_initialization_row_owner(
    row: usize,
    role: InitializationRowRole,
    owner: Option<ScalarSlot>,
    target: Option<ScalarSlot>,
    mandatory_row_count: usize,
) -> Result<(), SolveProblemShapeContractError> {
    match role {
        InitializationRowRole::Solved => validate_solved_initialization_row(row, owner, target),
        InitializationRowRole::StatedValueCheck => {
            validate_stated_initialization_row(row, owner, target, mandatory_row_count)
        }
    }
}

fn validate_solved_initialization_row(
    row: usize,
    owner: Option<ScalarSlot>,
    target: Option<ScalarSlot>,
) -> Result<(), SolveProblemShapeContractError> {
    let Some(unknown) = owner else {
        return Err(initialization_correlation(format!(
            "row {row} is recorded solved but no projection block claims it"
        )));
    };
    if target != Some(unknown) {
        return Err(initialization_correlation(format!(
            "row {row} is solved for {unknown:?} by its block but targets {target:?}"
        )));
    }
    Ok(())
}

fn validate_stated_initialization_row(
    row: usize,
    owner: Option<ScalarSlot>,
    target: Option<ScalarSlot>,
    mandatory_row_count: usize,
) -> Result<(), SolveProblemShapeContractError> {
    if row < mandatory_row_count {
        return Err(initialization_correlation(format!(
            "mandatory source row {row} is recorded as a stated-value check, which would erase \
             its owner; only the carried rows at or past mandatory_row_count \
             {mandatory_row_count} may stand as checks"
        )));
    }
    if owner.is_some() || target.is_some() {
        return Err(initialization_correlation(format!(
            "row {row} is a stated-value check but holds a block owner {owner:?} or target \
             {target:?}"
        )));
    }
    Ok(())
}

impl InitializationSolveSystem {
    /// Mint one exactly correlated initialization aggregate, atomically.
    ///
    /// The proof obligations, refused with
    /// [`SolveProblemShapeContractError::InitializationCorrelation`]:
    ///
    /// * `row_targets` and `row_roles` each cover the residual rows exactly,
    ///   and `mandatory_row_count` is within them;
    /// * every projection block pairs as many rows as unknowns, each row index
    ///   in range and claimed by at most one block;
    /// * every stored slot (block unknowns, targets, update targets) is the
    ///   exact layout-derived canonical `Y`/`P` form, and uniqueness runs over
    ///   the storage identity, never the representation;
    /// * the block unknowns, concatenated in block order, are exactly
    ///   `projection_unknowns`, with no storage identity repeated;
    /// * a `Solved` row sits in exactly one block and its `row_targets` entry
    ///   names exactly its block unknown; a `StatedValueCheck` row sits in no
    ///   block and has no target;
    /// * every mandatory row (index below `mandatory_row_count`) is `Solved`,
    ///   so no relabeling can erase a mandatory source row's owner;
    /// * `update_targets` covers `update_rhs` exactly, each target unique, and
    ///   no update target aliases a projection unknown. The pair arrives as one
    ///   `updates` value, `(update_rhs, update_targets)`, minted together.
    pub fn construct(
        residual: ComputeBlock,
        row_targets: Vec<Option<ScalarSlot>>,
        row_roles: Vec<InitializationRowRole>,
        mandatory_row_count: usize,
        projection_unknowns: Vec<ScalarSlot>,
        projection_plan: InitializationProjectionPlan,
        updates: (ScalarProgramBlock, Vec<ScalarSlot>),
    ) -> Result<Self, SolveProblemShapeContractError> {
        let (update_rhs, update_targets) = updates;
        let system = Self {
            residual,
            row_targets,
            row_roles,
            mandatory_row_count,
            projection_unknowns,
            projection_plan,
            update_rhs,
            update_targets,
        };
        system.validate_correlation()?;
        Ok(system)
    }

    /// The valid empty system: zero rows, zero unknowns, zero updates.
    ///
    /// This is the exact MLS §8.6 shape of a model whose declarations
    /// determine every coordinate. Every constructor obligation quantifies
    /// over rows, blocks, or targets, so all of them are vacuous here: the
    /// empty value cannot violate an invariant, which is why it is stated as
    /// a value instead of asserting a check that cannot fail.
    /// `empty_system_is_exactly_correlated` keeps the evidence that
    /// [`Self::construct`] agrees.
    #[must_use]
    pub fn empty() -> Self {
        Self {
            residual: ComputeBlock::default(),
            row_targets: Vec::new(),
            row_roles: Vec::new(),
            mandatory_row_count: 0,
            projection_unknowns: Vec::new(),
            projection_plan: InitializationProjectionPlan::default(),
            update_rhs: ScalarProgramBlock::default(),
            update_targets: Vec::new(),
        }
    }

    fn validate_correlation(&self) -> Result<(), SolveProblemShapeContractError> {
        let rows = self.validate_correlation_counts()?;
        let mut owner_of_row: Vec<Option<ScalarSlot>> = vec![None; rows];
        let mut block_unknowns_in_order: Vec<ScalarSlot> = Vec::new();
        let mut unknown_identities: BTreeSet<SolveStorageCoordinate> = BTreeSet::new();
        for (index, block) in self.projection_plan.blocks.iter().enumerate() {
            if block.rows.is_empty() || block.rows.len() != block.unknowns.len() {
                return Err(initialization_correlation(format!(
                    "projection block {index} pairs {} row(s) with {} unknown(s)",
                    block.rows.len(),
                    block.unknowns.len(),
                )));
            }
            for (row, unknown) in block
                .rows
                .iter()
                .copied()
                .zip(block.unknowns.iter().copied())
            {
                claim_initialization_projection_owner(
                    &mut owner_of_row,
                    &mut unknown_identities,
                    &mut block_unknowns_in_order,
                    rows,
                    index,
                    row,
                    unknown,
                )?;
            }
        }
        if block_unknowns_in_order != self.projection_unknowns {
            return Err(initialization_correlation(format!(
                "the block unknown inventory {block_unknowns_in_order:?} is not \
                 projection_unknowns {:?}",
                self.projection_unknowns,
            )));
        }
        self.validate_initialization_row_owners(&owner_of_row)?;
        self.validate_initialization_update_targets(&unknown_identities)
    }

    fn validate_correlation_counts(&self) -> Result<usize, SolveProblemShapeContractError> {
        let rows = self.residual.len()?;
        if self.row_targets.len() != rows || self.row_roles.len() != rows {
            return Err(initialization_correlation(format!(
                "the residual holds {rows} row(s), row_targets {} and row_roles {}",
                self.row_targets.len(),
                self.row_roles.len(),
            )));
        }
        if self.mandatory_row_count > rows {
            return Err(initialization_correlation(format!(
                "mandatory_row_count {} exceeds the {rows} residual row(s)",
                self.mandatory_row_count,
            )));
        }
        if self.update_targets.len() != self.update_rhs.len() {
            return Err(initialization_correlation(format!(
                "update_rhs holds {} row(s), update_targets {}",
                self.update_rhs.len(),
                self.update_targets.len(),
            )));
        }
        Ok(rows)
    }

    fn validate_initialization_row_owners(
        &self,
        owner_of_row: &[Option<ScalarSlot>],
    ) -> Result<(), SolveProblemShapeContractError> {
        for (row, role) in self.row_roles.iter().copied().enumerate() {
            validate_initialization_row_owner(
                row,
                role,
                owner_of_row[row],
                self.row_targets[row],
                self.mandatory_row_count,
            )?;
        }
        Ok(())
    }

    fn validate_initialization_update_targets(
        &self,
        unknown_identities: &BTreeSet<SolveStorageCoordinate>,
    ) -> Result<(), SolveProblemShapeContractError> {
        let mut target_identities: BTreeSet<SolveStorageCoordinate> = BTreeSet::new();
        for (index, target) in self.update_targets.iter().copied().enumerate() {
            let identity = writable_storage_coordinate(target, &format!("update target {index}"))?;
            if !target_identities.insert(identity) {
                return Err(initialization_correlation(format!(
                    "update target {identity:?} is written by two update rows"
                )));
            }
            if unknown_identities.contains(&identity) {
                return Err(initialization_correlation(format!(
                    "update target {identity:?} aliases a projection unknown, so the update \
                     and the block would fight over one storage slot"
                )));
            }
        }
        Ok(())
    }

    #[must_use]
    pub const fn residual(&self) -> &ComputeBlock {
        &self.residual
    }

    #[must_use]
    pub fn row_targets(&self) -> &[Option<ScalarSlot>] {
        &self.row_targets
    }

    #[must_use]
    pub fn row_roles(&self) -> &[InitializationRowRole] {
        &self.row_roles
    }

    /// The count of mandatory source rows; carried stated-value rows follow.
    #[must_use]
    pub const fn mandatory_row_count(&self) -> usize {
        self.mandatory_row_count
    }

    #[must_use]
    pub fn projection_unknowns(&self) -> &[ScalarSlot] {
        &self.projection_unknowns
    }

    #[must_use]
    pub const fn projection_plan(&self) -> &InitializationProjectionPlan {
        &self.projection_plan
    }

    #[must_use]
    pub const fn update_rhs(&self) -> &ScalarProgramBlock {
        &self.update_rhs
    }

    #[must_use]
    pub fn update_targets(&self) -> &[ScalarSlot] {
        &self.update_targets
    }
}

/// What the MLS §8.6 initialization projection does with one residual row.
///
/// The vocabulary is closed over executable states: a retained row either
/// solves exactly one projection unknown or is the stated-value agreement
/// check a structural proof minted. There is deliberately no default role, no
/// unowned escape hatch, and no arm without a producer; a row that reads a
/// coordinate the projection cannot own, and a fixed algebraic/output row
/// whose transitive incidence is not yet computed, are Solve construction
/// errors, never representable values.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum InitializationRowRole {
    /// A projection block solves this row for the coordinate `row_targets` names.
    Solved,
    /// Two declarations state one coordinate's initial value, and the structural
    /// phase could not decide symbolically whether they agree (the difference
    /// still reads parameters). The row restates a §8.6 equation another
    /// declaration already contributes, so it owns no unknown; the
    /// initialization instant answers with numbers whether the stated values
    /// coincide. A failure of one is a contradiction between declarations, not
    /// an unsolved coordinate. This is the only role a row without a
    /// `row_targets` owner may hold, and only a carried row past
    /// `mandatory_row_count` may hold it.
    StatedValueCheck,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum EventIterationValueKind {
    Real,
    Integer,
    Boolean,
    /// Positive integral ordinal. The DAE currently erases the declared upper
    /// literal bound; restoring that bound is a tracked upstream obligation.
    Enumeration,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum EventIterationOwner {
    Hold,
    ScalarRows {
        start_row: usize,
    },
    StructuredUpdate {
        update_index: usize,
    },
    GuardedAssignment {
        program_index: usize,
        target_range_index: usize,
    },
    EventTransaction {
        program_index: usize,
        target_index: usize,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Deserialize, Serialize)]
pub struct EventIterationRun {
    /// Canonical typed variable-storage owner.
    pub variable: usize,
    pub pre_binding_start: usize,
    pub owner: EventIterationOwner,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct EventIterationPlan {
    pub runs: Vec<EventIterationRun>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct DiscreteSolveSystem {
    /// Compact compiler-owned Appendix-B iteration catalog.
    pub event_iteration_plan: EventIterationPlan,
    /// Exact main `rhs` output row copied by each runtime assignment.
    ///
    /// Construction proves this is the complete owner-ordered, root-reachable
    /// projection of eligible P-backed scalar discrete owners. Runtime code
    /// never rediscovers source identity from program equality.
    pub runtime_assignment_source_rows: Vec<usize>,
    pub runtime_assignment_rhs: ScalarProgramBlock,
    pub runtime_assignment_targets: Vec<ScalarSlot>,
    /// Compiler certificate for whether each runtime assignment evaluates a
    /// relation (directly or through another runtime assignment).
    pub runtime_assignment_roles: Vec<RuntimeAssignmentRole>,
    /// Relation-free root-driven assignments that remain valid after event
    /// `pre` history has committed.
    pub post_commit_assignment_rhs: ScalarProgramBlock,
    pub post_commit_assignment_targets: Vec<ScalarSlot>,
    /// Runtime-row owner copied by each post-commit row. Shape validation
    /// proves the copy is exact and the owner is relation-free.
    pub post_commit_assignment_runtime_rows: Vec<usize>,
    pub rhs: ScalarProgramBlock,
    pub update_targets: Vec<ScalarSlot>,
    pub row_roles: Vec<DiscreteRowRole>,
    pub pre_modes: Vec<DiscreteEventPreMode>,
    pub observation_refresh: Vec<bool>,
    /// Construction certificate: at least one selected scalar observation
    /// row reads Y storage and therefore must be rechecked after continuous
    /// algebraic projection. False proves that one discrete refresh followed
    /// by one projection is the complete public fixed point.
    pub observation_refresh_reads_y: bool,
    /// Compiler-derived effect of changing each scalar update target on an
    /// integrator's continuous multistep history.
    ///
    /// This vector is row-aligned with `rhs`. A runtime may join the effect
    /// with exact update changes, but must not recover it from row position or
    /// model identity.
    pub integrator_history_effects: Vec<IntegratorHistoryEffect>,
    /// Periodic activation owner for each discrete row.
    ///
    /// `None` denotes an ordinary event-iteration row. A clock-owned row is
    /// evaluated only when the referenced exact lattice ticks.
    pub clock_owners: Vec<Option<PeriodicClockId>>,
    /// Correlated guarded updates retain one compact result program and one
    /// ordered target-range catalog. Scalar coordinate rows are derived only
    /// by evaluator/backend adapters.
    pub guarded_assignments: Vec<GuardedAssignmentProgram>,
    /// Model-level event algorithms remain one ordered typed transaction
    /// across mixed discrete Real and discrete-valued storage.
    pub event_transactions: Vec<EventTransactionProgram>,
    /// Compact B.1c maps. Scalar owners remain in `rhs`; a structured owner is
    /// represented exactly once here and is scalarized only by evaluation or
    /// backend adapter APIs.
    pub structured_rhs: ComputeBlock,
    pub structured_updates: Vec<StructuredDiscreteUpdate>,
    /// SPEC_0040 SOLVE-C57 (implementing toward SPEC_0046 SDO-001/SDO-002):
    /// the construction-issued same-tick execution order for every clock-owned
    /// discrete producer.
    ///
    /// Rank is the vector position — an issued value. The runtime executes the
    /// steps in this order against private work state so an ordinary
    /// same-instant read observes this tick's value while `pre`/`previous`/
    /// `sample(u)` reads keep their history lanes; the complete final target
    /// tuple still commits atomically after the last step. Equation-shaped
    /// producers owned by a DAE-C21/SOLVE-C55 event transaction are excluded;
    /// the complete transaction replaces them as one opaque outer-producer
    /// step. A runtime must never reconstruct this order from targets,
    /// names, spans, provenance, or program shape.
    pub clock_partition_order: Vec<ClockPartitionStep>,
    /// Exact causal definitions of the algebraic intermediates that sit on a
    /// same-tick path between two ordered producers (for example an alias
    /// chain `slow = fastAlias; fastAlias = fast`). Each output row refreshes
    /// one scalar coordinate into private work state only — intermediates
    /// become visible to later producers without committing unrelated targets.
    pub clock_partition_intermediates: ScalarProgramBlock,
    /// Work-state storage slot for each `clock_partition_intermediates` output
    /// row (row-aligned).
    pub clock_partition_intermediate_targets: Vec<ScalarSlot>,
    /// Exact consumer clock domains for each intermediate output row.
    ///
    /// A row is refreshed iff at least one domain in its non-empty set ticks.
    /// The structural same-tick proof issues these sets; execution only
    /// replays them and never reconstructs dependency liveness.
    pub clock_partition_intermediate_clocks: Vec<Vec<PeriodicClockId>>,
}

/// One issued step of the SOLVE-C57 clock-partition same-tick schedule.
///
/// Each variant names one complete producer (or one intermediate-definition
/// refresh row) by its typed index; membership and order are decided once at
/// construction and never repaired at runtime.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum ClockPartitionStep {
    /// One clock-owned producer stored as contiguous output rows of
    /// [`DiscreteSolveSystem::rhs`] (all rows of one producer program).
    ScalarRows { start_row: usize, count: usize },
    /// One clock-owned [`DiscreteSolveSystem::guarded_assignments`] owner.
    GuardedAssignment { program_index: usize },
    /// One clock-owned [`DiscreteSolveSystem::structured_updates`] owner.
    StructuredUpdate { update_index: usize },
    /// One complete clock-owned [`EventTransactionProgram`] outer producer.
    /// Its source-ordered statement body remains opaque to this schedule; only
    /// its complete final tuple becomes visible to later steps.
    EventTransaction { program_index: usize },
    /// Refresh one [`DiscreteSolveSystem::clock_partition_intermediates`] row
    /// into private work state (never committed by the discrete pass).
    Intermediate { row: usize },
}

/// One compact mutable-storage destination for a guarded assignment result.
#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct GuardedAssignmentTargetRange {
    base: ScalarSlot,
    count: usize,
}

impl GuardedAssignmentTargetRange {
    pub const fn base(self) -> ScalarSlot {
        self.base
    }

    pub const fn count(self) -> usize {
        self.count
    }
}

/// One checked correlated guarded update.
///
/// `program` produces the concatenation of `target_ranges` in source order.
/// The compact ranges, rather than a per-coordinate target vector, are the
/// authoritative simultaneous-assignment relation.
#[derive(Clone, Debug, Serialize)]
pub struct GuardedAssignmentProgram {
    program: Arc<[LinearOp]>,
    span: Span,
    target_ranges: Box<[GuardedAssignmentTargetRange]>,
    #[serde(skip)]
    output_count: usize,
    #[serde(skip)]
    register_count: usize,
    role: DiscreteRowRole,
    pre_mode: DiscreteEventPreMode,
    observation_refresh: bool,
    integrator_history_effect: IntegratorHistoryEffect,
    clock_owner: Option<PeriodicClockId>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct GuardedAssignmentProgramWire {
    program: Vec<LinearOp>,
    span: Span,
    target_ranges: Box<[GuardedAssignmentTargetRangeWire]>,
    role: DiscreteRowRole,
    pre_mode: DiscreteEventPreMode,
    observation_refresh: bool,
    integrator_history_effect: IntegratorHistoryEffect,
    #[serde(deserialize_with = "deserialize_required_option")]
    clock_owner: Option<PeriodicClockId>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct GuardedAssignmentTargetRangeWire {
    base: ScalarSlot,
    count: usize,
}

impl<'de> Deserialize<'de> for GuardedAssignmentProgram {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = GuardedAssignmentProgramWire::deserialize(deserializer)?;
        let provenance = wire
            .span
            .require_provenance("GuardedAssignmentProgram")
            .map_err(serde::de::Error::custom)?;
        Self::checked(GuardedAssignmentProgramInput {
            program: wire.program,
            provenance,
            target_ranges: wire
                .target_ranges
                .iter()
                .map(|range| (range.base, range.count))
                .collect(),
            role: wire.role,
            pre_mode: wire.pre_mode,
            observation_refresh: wire.observation_refresh,
            integrator_history_effect: wire.integrator_history_effect,
            clock_owner: wire.clock_owner,
        })
        .map_err(serde::de::Error::custom)
    }
}

impl GuardedAssignmentProgram {
    pub fn checked(
        input: GuardedAssignmentProgramInput,
    ) -> Result<Self, SolveProblemShapeContractError> {
        let GuardedAssignmentProgramInput {
            program,
            provenance,
            target_ranges,
            role,
            pre_mode,
            observation_refresh,
            integrator_history_effect,
            clock_owner,
        } = input;
        let span = provenance.span();
        let target_ranges = target_ranges
            .into_iter()
            .map(|(base, count)| GuardedAssignmentTargetRange { base, count })
            .collect::<Box<[_]>>();
        validate_guarded_assignment_targets(&target_ranges, span)?;
        let expected_outputs = target_ranges.iter().try_fold(0usize, |total, range| {
            total.checked_add(range.count).ok_or(
                SolveProblemShapeContractError::GuardedAssignmentProgram {
                    program_index: 0,
                    detail: "target result width overflows",
                    span: Some(span),
                },
            )
        })?;
        let actual_outputs = crate::checked_linear_op_output_count(&program).ok_or(
            SolveProblemShapeContractError::GuardedAssignmentProgram {
                program_index: 0,
                detail: "program output width overflows host range",
                span: Some(span),
            },
        )?;
        if actual_outputs != expected_outputs {
            return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
                program_index: 0,
                detail: "program output width does not equal its compact target ranges",
                span: Some(span),
            });
        }
        crate::validate_function_conditional_owners(
            "GuardedAssignmentProgram",
            0,
            std::slice::from_ref(&program),
            &[span],
        )?;
        let register_count = crate::derive_scalar_program_register_counts(
            "GuardedAssignmentProgram",
            0,
            std::slice::from_ref(&program),
            &[span],
        )?[0];
        Ok(Self {
            program: program.into(),
            span,
            target_ranges,
            output_count: expected_outputs,
            register_count,
            role,
            pre_mode,
            observation_refresh,
            integrator_history_effect,
            clock_owner,
        })
    }

    pub fn program(&self) -> &[LinearOp] {
        &self.program
    }

    pub fn shared_program(&self) -> Arc<[LinearOp]> {
        Arc::clone(&self.program)
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub fn target_ranges(&self) -> &[GuardedAssignmentTargetRange] {
        &self.target_ranges
    }

    pub const fn role(&self) -> DiscreteRowRole {
        self.role
    }

    pub const fn pre_mode(&self) -> DiscreteEventPreMode {
        self.pre_mode
    }

    pub const fn observation_refresh(&self) -> bool {
        self.observation_refresh
    }

    pub const fn integrator_history_effect(&self) -> IntegratorHistoryEffect {
        self.integrator_history_effect
    }

    pub const fn clock_owner(&self) -> Option<PeriodicClockId> {
        self.clock_owner
    }

    pub const fn output_count(&self) -> usize {
        self.output_count
    }

    /// Exact register capacity proved with this compact owner.
    pub const fn register_count(&self) -> usize {
        self.register_count
    }
}

/// Untrusted fields consumed together by [`GuardedAssignmentProgram::checked`].
pub struct GuardedAssignmentProgramInput {
    pub program: Vec<LinearOp>,
    pub provenance: ProvenanceSpan,
    pub target_ranges: Vec<(ScalarSlot, usize)>,
    pub role: DiscreteRowRole,
    pub pre_mode: DiscreteEventPreMode,
    pub observation_refresh: bool,
    pub integrator_history_effect: IntegratorHistoryEffect,
    pub clock_owner: Option<PeriodicClockId>,
}

fn validate_guarded_assignment_targets(
    target_ranges: &[GuardedAssignmentTargetRange],
    span: Span,
) -> Result<(), SolveProblemShapeContractError> {
    if target_ranges.is_empty() {
        return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
            program_index: 0,
            detail: "target-range catalog is empty",
            span: Some(span),
        });
    }
    let mut covered = Vec::<(u8, usize, usize)>::new();
    for range in target_ranges {
        if range.count == 0 {
            return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
                program_index: 0,
                detail: "target range is empty",
                span: Some(span),
            });
        }
        let (storage, start) = match range.base {
            ScalarSlot::Y { index } => (0_u8, index),
            ScalarSlot::P { index } => (1_u8, index),
            ScalarSlot::Time | ScalarSlot::Constant(_) => {
                return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
                    program_index: 0,
                    detail: "target range is not mutable Y/P storage or overflows",
                    span: Some(span),
                });
            }
        };
        let end = start.checked_add(range.count).ok_or(
            SolveProblemShapeContractError::GuardedAssignmentProgram {
                program_index: 0,
                detail: "target range is not mutable Y/P storage or overflows",
                span: Some(span),
            },
        )?;
        if covered
            .iter()
            .any(|&(other_storage, other_start, other_end)| {
                storage == other_storage && start < other_end && other_start < end
            })
        {
            return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
                program_index: 0,
                detail: "target ranges overlap",
                span: Some(span),
            });
        }
        covered.push((storage, start, end));
    }
    Ok(())
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub enum RuntimeAssignmentRole {
    /// The row consumes already-selected values without evaluating a relation.
    RelationFree,
    /// The row evaluates a relation, depends on such a row, or writes relation memory.
    #[default]
    RelationEvaluating,
}

/// Compact target projection and row policy for one structured B.1c map node.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct StructuredDiscreteUpdate {
    /// Absolute index into [`DiscreteSolveSystem::structured_rhs`] nodes.
    pub node_index: usize,
    pub target: StructuredDiscreteTargetMap,
    pub role: DiscreteRowRole,
    pub pre_mode: DiscreteEventPreMode,
    pub observation_refresh: bool,
    pub integrator_history_effect: IntegratorHistoryEffect,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub clock_owner: Option<PeriodicClockId>,
}

/// One compact affine projection from map points to consecutive Y/P storage.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct StructuredDiscreteTargetMap {
    pub base: ScalarSlot,
    pub map: TensorOutputMap,
}

impl DiscreteSolveSystem {
    /// Checked scalar adapter view for one compact structured update.
    ///
    /// Each pair is `(target slot, structured_rhs output lane)`. Backends use
    /// this at their scalar boundary; the Solve IR retains only the compact map.
    pub fn structured_assignments(
        &self,
        update_index: usize,
    ) -> Result<Vec<(ScalarSlot, usize)>, SolveProblemShapeContractError> {
        let update = self.structured_updates.get(update_index).ok_or(
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: usize::MAX,
                detail: "update index is out of bounds",
                span: None,
            },
        )?;
        let node = self.structured_rhs.nodes.get(update.node_index).ok_or(
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute node index is out of bounds",
                span: None,
            },
        )?;
        let ComputeNode::Map {
            domain,
            output_map,
            span,
            ..
        } = node
        else {
            return Err(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute node is not a Map",
                span: None,
            });
        };
        let sources = output_map.output_indices(domain).map_err(|_| {
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute output projection is invalid",
                span: Some(*span),
            }
        })?;
        let targets = update.target.map.output_indices(domain).map_err(|_| {
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "target projection is invalid",
                span: Some(*span),
            }
        })?;
        if sources.len() != targets.len() {
            return Err(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute and target projections have different cardinality",
                span: Some(*span),
            });
        }
        targets
            .into_iter()
            .zip(sources)
            .map(|(offset, source)| {
                offset_scalar_slot(update.target.base, offset)
                    .map(|target| (target, source))
                    .ok_or(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                        update_index,
                        node_index: update.node_index,
                        detail: "target base is not Y/P storage or its offset overflows",
                        span: Some(*span),
                    })
            })
            .collect()
    }
}

fn offset_scalar_slot(base: ScalarSlot, offset: usize) -> Option<ScalarSlot> {
    match base {
        ScalarSlot::Y { index } => index.checked_add(offset).map(scalar_slot_y),
        ScalarSlot::P { index } => index.checked_add(offset).map(scalar_slot_p),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveEventPartition {
    pub root_conditions: ScalarProgramBlock,
    pub root_relation_memory_targets: Vec<Option<ScalarSlot>>,
    pub root_zero_domains: Vec<RootZeroDomain>,
    /// Compiler certificate describing which root memories may participate in
    /// post-commit algebraic coupling.
    pub root_relation_refresh_roles: Vec<RootRelationRefreshRole>,
    /// Hidden P slots that retain the previous value of each DAE condition.
    ///
    /// Event-action programs read these slots to distinguish a rising edge
    /// from a condition that merely remains true across an unrelated event.
    pub condition_memory_parameter_indices: Vec<usize>,
    pub scheduled_root_conditions: Vec<ScheduledRootCondition>,
    pub scheduled_time_events: Vec<f64>,
    pub dynamic_time_event_names: Vec<String>,
    pub dynamic_time_event_rhs: ScalarProgramBlock,
    pub action_conditions: ScalarProgramBlock,
    pub actions: Vec<SolveEventAction>,
    pub has_terminal_event: bool,
    pub delays: SolveDelayPartition,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub enum RootRelationRefreshRole {
    #[default]
    Frozen,
    AlgebraicDependent,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveDelayPartition {
    /// Current-value expressions whose accepted values populate history.
    pub source_rhs: ScalarProgramBlock,
    /// Delay amount evaluated at the query time.
    pub delay_time_rhs: ScalarProgramBlock,
    /// Maximum retained history horizon. When source `delayMax` is omitted,
    /// this row is identical to the corresponding delay-time row.
    pub delay_max_rhs: ScalarProgramBlock,
    /// Runtime-managed P slot receiving the delayed value for each row.
    pub value_parameter_indices: Vec<usize>,
    /// Whether the source uses piecewise-constant history rather than linear
    /// interpolation between accepted points.
    pub source_is_discrete: Vec<bool>,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub enum RootZeroDomain {
    Positive,
    NonPositive,
    #[default]
    Previous,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ScheduledRootCondition {
    pub root_index: usize,
    pub period_seconds: f64,
    pub phase_seconds: f64,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct SolveEventAction {
    pub kind: SolveEventActionKind,
    pub message: SolveEventMessage,
    pub span: rumoca_core::Span,
    pub origin: String,
    /// Exact periodic owner for a clock-scoped action.
    ///
    /// `None` denotes an ordinary state/event action. A clock-owned action is
    /// eligible only when this schedule ticks; the condition program retains
    /// its activation lane as a local semantic guard.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub clock_owner: Option<PeriodicClockId>,
}

#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct SolveEventMessage {
    pub parts: Vec<SolveEventMessagePart>,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum SolveEventMessagePart {
    Text(String),
    Conversion {
        value: Vec<LinearOp>,
        source: SolveStringConversionSource,
        format: SolveStringConversionFormat,
    },
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum SolveStringConversionSource {
    Real,
    Integer,
    Boolean,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum SolveStringConversionFormat {
    Options {
        #[serde(deserialize_with = "deserialize_required_option")]
        minimum_length: Option<Vec<LinearOp>>,
        #[serde(deserialize_with = "deserialize_required_option")]
        left_justified: Option<Vec<LinearOp>>,
        #[serde(deserialize_with = "deserialize_required_option")]
        significant_digits: Option<Vec<LinearOp>>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum SolveEventActionKind {
    Assert,
    Terminate,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveClockPartition {
    pub periodic_event_schedules: Vec<PeriodicEventSchedule>,
    /// Hidden Boolean-as-Real P slot for each typed periodic clock.
    ///
    /// The runtime derives each value from the schedule at the current event
    /// instant. These lanes make clock leaves computable inside mixed
    /// condition DAGs without creating another clock or row owner.
    pub activation_parameter_indices: Vec<usize>,
}

impl SolveClockPartition {
    pub fn periodic_clock_id(&self, index: usize) -> Option<PeriodicClockId> {
        self.periodic_event_schedules
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(PeriodicClockId)
    }

    pub fn periodic_schedule(&self, clock: PeriodicClockId) -> Option<&PeriodicEventSchedule> {
        self.periodic_event_schedules.get(clock.index())
    }
}

/// Typed identity of one periodic schedule in a [`SolveClockPartition`].
#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct PeriodicClockId(u32);

impl PeriodicClockId {
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub enum DiscreteEventPreMode {
    /// Use the value from the start of the current clock/event tick.
    EventEntry,
    /// Hold `pre(..)` fixed for one event-iteration pass.
    Fixed,
    /// Read the current event-iteration fixed-point state.
    #[default]
    FollowCurrent,
}

/// Whether changing one typed discrete owner can invalidate continuous
/// integrator history.
///
/// `Preserve` is positive compiler evidence. The fail-closed default is
/// `Restart`, used whenever lowering cannot prove the dependency absent.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum IntegratorHistoryEffect {
    Preserve,
    #[default]
    Restart,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum DiscreteRowRole {
    /// A B.1c equation that participates in initialization and event iteration.
    Equation,
    /// An assignment that executes only on its owning event edge.
    EventAction,
    /// Runtime memory for detecting a condition edge.
    ConditionMemory,
}

#[derive(Clone, Debug, Serialize)]
pub struct PeriodicEventSchedule {
    lattice: rumoca_core::ClockLattice,
    anchor: rumoca_core::ClockPhaseAnchor,
}

impl PeriodicEventSchedule {
    /// Construct an exact rational schedule from finite second values.
    ///
    /// This is the external-boundary constructor for solver fixtures and
    /// decoded configuration. Compiler lowering should pass its already proven
    /// [`rumoca_core::ClockLattice`] through [`Self::new`].
    pub fn from_seconds(
        period: f64,
        phase: f64,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        Self::new(rumoca_core::ClockLattice::from_seconds(period, phase)?)
    }

    pub fn new(
        lattice: rumoca_core::ClockLattice,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        Self::from_schedule(rumoca_core::PeriodicClockSchedule::absolute(lattice)?)
    }

    pub fn from_schedule(
        schedule: rumoca_core::PeriodicClockSchedule,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        let schedule = match schedule.anchor() {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                rumoca_core::PeriodicClockSchedule::absolute(schedule.lattice())?
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(schedule.lattice())?
            }
        };
        Ok(Self {
            lattice: schedule.lattice(),
            anchor: schedule.anchor(),
        })
    }

    /// The authoritative exact rational lattice (MLS §16.3/§16.5).
    pub const fn lattice(&self) -> rumoca_core::ClockLattice {
        self.lattice
    }

    pub const fn anchor(&self) -> rumoca_core::ClockPhaseAnchor {
        self.anchor
    }

    /// Resolve a simulation-start-relative phase for one ME instance.
    pub fn resolved_at(&self, start_time: f64) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        let schedule = match self.anchor {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                rumoca_core::PeriodicClockSchedule::absolute(self.lattice)?
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(self.lattice)?
            }
        };
        Self::from_schedule(schedule.resolve_at(start_time)?)
    }

    pub fn period_seconds(&self) -> f64 {
        self.lattice.period_seconds()
    }

    pub fn phase_seconds(&self) -> f64 {
        self.lattice.phase_seconds()
    }

    /// Instant of tick `index` in seconds, computed exactly then rounded once.
    ///
    /// A schedule with no rational form or a tick outside the exact integer
    /// representation reports the original lattice error. Authoritative
    /// schedulers must not replace that failure with floating-point arithmetic.
    pub fn exact_tick_time_seconds(
        &self,
        index: impl Into<i128>,
    ) -> Result<f64, rumoca_core::ClockLatticeErrorKind> {
        self.lattice.tick_time_seconds(index)
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PeriodicEventScheduleWire {
    lattice: rumoca_core::ClockLattice,
    anchor: rumoca_core::ClockPhaseAnchor,
}

impl<'de> Deserialize<'de> for PeriodicEventSchedule {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = PeriodicEventScheduleWire::deserialize(deserializer)?;
        let schedule = match wire.anchor {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                rumoca_core::PeriodicClockSchedule::absolute(wire.lattice)
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(wire.lattice)
            }
        }
        .map_err(serde::de::Error::custom)?;
        Self::from_schedule(schedule).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolverNameIndexMaps {
    pub names: Vec<String>,
    pub name_to_idx: IndexMap<String, usize>,
    pub base_to_indices: IndexMap<String, Vec<usize>>,
}

/// Source slot for a `__pre__.*` parameter binding.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum PreParamSource {
    /// Copy from `y[index]` at event entry.
    Y { index: usize },
    /// Copy from `p[index]` (snapshot) at event entry.
    P { index: usize },
}

/// Maps a `__pre__.*` parameter's P-slot to the source slot it should be
/// snapshot-copied from at event entry. Built by phase-solve-lower from the
/// VarLayout after DAE-IR pre_lowering has run.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PreParamBinding {
    pub dest_p_index: usize,
    pub source: PreParamSource,
    /// Owning periodic clock for MLS §16 `previous()` history.
    ///
    /// `None` denotes ordinary Modelica `pre()` history and is committed after
    /// every event. A scheduled binding is committed only when this clock
    /// ticks, so unrelated roots and other clocks cannot advance its history.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub clock_schedule: Option<PeriodicEventSchedule>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum SolveVariableStorageRole {
    Parameter,
    Constant,
    ExternalInput,
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum SolveVariableValueKind {
    Real,
    Integer,
    Boolean,
    Enumeration,
    String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveVariableTimeDomain {
    Static,
    EventDiscrete,
    EventDiscontinuous,
    ContinuousTime,
}

impl SolveVariableTimeDomain {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Static => "static",
            Self::EventDiscrete => "event-discrete",
            Self::EventDiscontinuous => "event-discontinuous",
            Self::ContinuousTime => "continuous-time",
        }
    }
}

/// A catalog run starts at a writable logical coordinate, never an operand.
///
/// ```compile_fail
/// use rumoca_ir_solve::{ScalarSlot, SolveVariableStorageRun,
///     SolveVariableStorageRole, SolveVariableValueKind};
/// let _ = SolveVariableStorageRun {
///     base: ScalarSlot::Time,
///     scalar_count: 1,
///     role: SolveVariableStorageRole::State,
///     value_kind: SolveVariableValueKind::Real,
/// };
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct SolveVariableStorageRun {
    pub base: SolveStorageCoordinate,
    pub scalar_count: usize,
    pub role: SolveVariableStorageRole,
    pub value_kind: SolveVariableValueKind,
}

/// Immutable typed declaration replayed independently of storage projection.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct SolveVariableDeclaration {
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
    time_domain: SolveVariableTimeDomain,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SolveVariableDeclarationWire {
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
    time_domain: SolveVariableTimeDomain,
}

impl<'de> Deserialize<'de> for SolveVariableDeclaration {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = SolveVariableDeclarationWire::deserialize(deserializer)?;
        let declaration = if wire.time_domain == SolveVariableTimeDomain::EventDiscontinuous {
            Self::event_discontinuous(wire.role, wire.value_kind)
                .map_err(serde::de::Error::custom)?
        } else {
            Self::new(wire.role, wire.value_kind)
        };
        if declaration.time_domain() != wire.time_domain {
            return Err(serde::de::Error::custom(format!(
                "{:?} {:?} storage requires time domain `{}`, not `{}`",
                wire.role,
                wire.value_kind,
                declaration.time_domain().as_str(),
                wire.time_domain.as_str()
            )));
        }
        Ok(declaration)
    }
}

impl SolveVariableDeclaration {
    pub const fn new(role: SolveVariableStorageRole, value_kind: SolveVariableValueKind) -> Self {
        Self {
            role,
            value_kind,
            time_domain: default_time_domain(role),
        }
    }

    pub fn event_discontinuous(
        role: SolveVariableStorageRole,
        value_kind: SolveVariableValueKind,
    ) -> Result<Self, SolveVariableDeclarationError> {
        if !matches!(
            role,
            SolveVariableStorageRole::Algebraic | SolveVariableStorageRole::Output
        ) || value_kind != SolveVariableValueKind::Real
        {
            return Err(SolveVariableDeclarationError { role, value_kind });
        }
        Ok(Self {
            role,
            value_kind,
            time_domain: SolveVariableTimeDomain::EventDiscontinuous,
        })
    }

    pub const fn role(self) -> SolveVariableStorageRole {
        self.role
    }

    pub const fn value_kind(self) -> SolveVariableValueKind {
        self.value_kind
    }

    pub const fn time_domain(self) -> SolveVariableTimeDomain {
        self.time_domain
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SolveVariableDeclarationError {
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
}

impl std::fmt::Display for SolveVariableDeclarationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "{:?} {:?} storage cannot be event-discontinuous",
            self.role, self.value_kind
        )
    }
}

impl std::error::Error for SolveVariableDeclarationError {}

const fn default_time_domain(role: SolveVariableStorageRole) -> SolveVariableTimeDomain {
    match role {
        SolveVariableStorageRole::Parameter | SolveVariableStorageRole::Constant => {
            SolveVariableTimeDomain::Static
        }
        SolveVariableStorageRole::DiscreteReal | SolveVariableStorageRole::DiscreteValue => {
            SolveVariableTimeDomain::EventDiscrete
        }
        SolveVariableStorageRole::ExternalInput
        | SolveVariableStorageRole::State
        | SolveVariableStorageRole::Algebraic
        | SolveVariableStorageRole::Output => SolveVariableTimeDomain::ContinuousTime,
    }
}

impl SolveVariableStorageRun {
    pub fn event_iteration_kind(self) -> Option<EventIterationValueKind> {
        match (self.role, self.value_kind) {
            (SolveVariableStorageRole::DiscreteReal, SolveVariableValueKind::Real) => {
                Some(EventIterationValueKind::Real)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Integer) => {
                Some(EventIterationValueKind::Integer)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Boolean) => {
                Some(EventIterationValueKind::Boolean)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Enumeration) => {
                Some(EventIterationValueKind::Enumeration)
            }
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveLayout {
    pub solver_maps: SolverNameIndexMaps,
    /// Dense DAE variable ordinal to its first Solve storage slot.
    ///
    /// Scalar `k` of a variable is stored at `base + k` in the same column.
    /// This is the canonical cross-phase coordinate map; display names are not
    /// used to recover compiler identity.
    pub variable_storage_runs: Vec<SolveVariableStorageRun>,
    /// Canonical typed DAE declarations in the same dense identity order.
    pub variable_declarations: Vec<SolveVariableDeclaration>,
    pub state_scalar_count: usize,
    pub algebraic_scalar_count: usize,
    pub output_scalar_count: usize,
    pub parameter_count: usize,
    /// Parameter and constant scalar names in their exact leading P-slot order.
    ///
    /// Inputs and discrete/runtime storage follow this prefix and have their own
    /// typed catalogs below. Keeping this list beside `parameter_count` means a
    /// consumer never has to reconstruct the static interface from aliased
    /// aggregate bindings or from a DAE side channel.
    pub static_parameter_names: Vec<String>,
    pub compiled_parameter_len: usize,
    pub input_scalar_names: Vec<String>,
    pub discrete_real_scalar_names: Vec<String>,
    pub discrete_valued_scalar_names: Vec<String>,
    pub relation_memory_parameter_indices: Vec<usize>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub initial_event_parameter_index: Option<usize>,
    /// P-slot that is true only while applying the final simulation event.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub terminal_event_parameter_index: Option<usize>,
    /// Hidden P-slot used by initialization residuals that contain
    /// `homotopy(actual, simplified)`.
    ///
    /// The initialization driver advances this value from zero to one. Models
    /// without homotopy expressions omit the slot entirely.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub initial_homotopy_parameter_index: Option<usize>,
    /// Snapshot bindings for `__pre__.*` parameters created by DAE-IR
    /// pre_lowering. At event entry the runtime copies each source slot into
    /// the corresponding dest P-slot before the event equations evaluate.
    pub pre_param_bindings: Vec<PreParamBinding>,
}

impl SolveLayout {
    pub fn solver_maps(&self) -> &SolverNameIndexMaps {
        &self.solver_maps
    }

    pub fn variable_scalar_slot(&self, variable: usize, scalar: usize) -> Option<ScalarSlot> {
        let run = self.variable_storage_runs.get(variable)?;
        if scalar >= run.scalar_count {
            return None;
        }
        match run.base {
            SolveStorageCoordinate::Y(index) => index.checked_add(scalar).map(scalar_slot_y),
            SolveStorageCoordinate::P(index) => index.checked_add(scalar).map(scalar_slot_p),
        }
    }

    pub fn state_scalar_count(&self) -> usize {
        self.state_scalar_count
    }

    pub fn algebraic_scalar_count(&self) -> usize {
        self.algebraic_scalar_count
    }

    pub fn output_scalar_count(&self) -> usize {
        self.output_scalar_count
    }

    pub fn solver_scalar_count(&self) -> usize {
        self.solver_maps.names.len()
    }

    /// Issue the closed admission disposition for a pure explicit ODE ABI.
    #[must_use]
    pub fn pure_explicit_state_disposition(&self) -> PureExplicitLayoutDisposition {
        let Some(state_count) = std::num::NonZeroUsize::new(self.state_scalar_count) else {
            return PureExplicitLayoutDisposition::Unsupported(
                PureExplicitLayoutDiagnostic::ZeroState,
            );
        };
        if self.algebraic_scalar_count != 0 {
            return PureExplicitLayoutDisposition::Unsupported(
                PureExplicitLayoutDiagnostic::AlgebraicYTail,
            );
        }
        if self.output_scalar_count != 0 {
            return PureExplicitLayoutDisposition::Unsupported(
                PureExplicitLayoutDiagnostic::OutputYTail,
            );
        }
        if self.solver_scalar_count() != state_count.get() {
            return PureExplicitLayoutDisposition::Unsupported(
                PureExplicitLayoutDiagnostic::SolverYExtentMismatch,
            );
        }
        PureExplicitLayoutDisposition::Supported(PureExplicitStateCount(state_count))
    }

    pub fn input_scalar_names(&self) -> &[String] {
        &self.input_scalar_names
    }

    pub fn static_parameter_names(&self) -> &[String] {
        &self.static_parameter_names
    }

    pub fn input_parameter_index(&self, name: &str) -> Option<usize> {
        self.input_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| self.parameter_count + offset)
    }

    pub fn discrete_real_parameter_index(&self, name: &str) -> Option<usize> {
        self.discrete_real_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| self.parameter_count + self.input_scalar_names.len() + offset)
    }

    pub fn discrete_valued_parameter_index(&self, name: &str) -> Option<usize> {
        self.discrete_valued_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| {
                self.parameter_count
                    + self.input_scalar_names.len()
                    + self.discrete_real_scalar_names.len()
                    + offset
            })
    }

    pub fn solver_idx_for_target(&self, target: &str) -> Option<usize> {
        solver_idx_for_target(target, &self.solver_maps.name_to_idx)
    }
}

/// Opaque proof that the complete solver-Y ABI is one positive state prefix.
///
/// ```compile_fail
/// use rumoca_ir_solve::PureExplicitStateCount;
/// let _ = PureExplicitStateCount(std::num::NonZeroUsize::MIN);
/// ```
///
/// ```compile_fail
/// use rumoca_ir_solve::PureExplicitStateCount;
/// let _ = PureExplicitStateCount::default();
/// ```
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PureExplicitStateCount(std::num::NonZeroUsize);

impl PureExplicitStateCount {
    #[must_use]
    pub const fn get(self) -> usize {
        self.0.get()
    }
}

/// Closed result of pure-explicit layout admission.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PureExplicitLayoutDisposition {
    Supported(PureExplicitStateCount),
    Unsupported(PureExplicitLayoutDiagnostic),
}

/// Stable reason a Solve layout cannot enter a pure-explicit backend.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PureExplicitLayoutDiagnostic {
    ZeroState,
    AlgebraicYTail,
    OutputYTail,
    SolverYExtentMismatch,
}

impl std::fmt::Display for PureExplicitLayoutDiagnostic {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::ZeroState => "the state inventory is empty",
            Self::AlgebraicYTail => "the solver-Y layout contains algebraic storage",
            Self::OutputYTail => "the solver-Y layout contains output storage",
            Self::SolverYExtentMismatch => "the solver-Y extent differs from the state extent",
        })
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SolveVariableMeta {
    pub name: String,
    pub source_span: Span,
    pub role: String,
    pub is_state: bool,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub value_type: Option<String>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub variability: Option<String>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub time_domain: Option<String>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub unit: Option<String>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub start: Option<String>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub min: Option<String>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub max: Option<String>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub nominal: Option<String>,
    /// The total MLS §4.8.1 `fixed` value, decided at DAE construction.
    pub fixed: rumoca_core::Fixity,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub description: Option<String>,
}

/// Failure to construct one complete, internally correlated Solve root.
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum SolveModelConstructionError {
    #[error("SolveModel {field} contains {actual} entries; expected {expected}")]
    VectorLength {
        field: &'static str,
        expected: usize,
        actual: usize,
    },
    #[error("SolveModel solver nominal at index {index} must be a strictly positive normal number")]
    InvalidSolverNominal { index: usize },
    #[error("SolveModel initial value at index {index} must be finite")]
    InvalidInitialValue { index: usize },
    #[error("SolveModel visible-value rows must use one dense local output per visible name")]
    VisibleOutputIndices,
    #[error("SolveModel artifact `{artifact}` contains {actual} outputs; expected {expected}")]
    ArtifactOutputCount {
        artifact: &'static str,
        expected: usize,
        actual: usize,
    },
    #[error(
        "SolveModel artifact `{artifact}` has invalid exact row coverage at output {index}: {detail}"
    )]
    ArtifactOutputCoverage {
        artifact: &'static str,
        index: usize,
        detail: &'static str,
    },
    #[error(
        "SolveModel artifact `{artifact}` has an invalid {actual_rows}x{actual_columns} pattern; expected {expected_rows}x{expected_columns}"
    )]
    ArtifactPatternShape {
        artifact: &'static str,
        expected_rows: usize,
        expected_columns: usize,
        actual_rows: usize,
        actual_columns: usize,
    },
    #[error("SolveModel artifact `{artifact}` is missing its derived structural pattern")]
    MissingArtifactPattern { artifact: &'static str },
    #[error("SolveModel artifact `{artifact}` unexpectedly carries a structural pattern")]
    UnexpectedArtifactPattern { artifact: &'static str },
    #[error(
        "SolveModel artifact `{artifact}` contains {actual} projection patterns; expected {expected}"
    )]
    ArtifactProjectionCount {
        artifact: &'static str,
        expected: usize,
        actual: usize,
    },
    #[error("SolveModel mass matrix is invalid: {reason}")]
    MassMatrix { reason: &'static str },
    /// Boxed because the shape-contract payload dominates the enum footprint;
    /// construction errors travel cold paths only.
    #[error(transparent)]
    Shape(Box<SolveProblemShapeContractError>),
    #[error(transparent)]
    VariableCatalog(#[from] SolveVariableCatalogError),
}

impl From<SolveProblemShapeContractError> for SolveModelConstructionError {
    fn from(error: SolveProblemShapeContractError) -> Self {
        Self::Shape(Box::new(error))
    }
}

/// One ordered, untrusted variable-catalog construction input.
pub type SolveVariableCatalogSourceEntry = (
    SolveVariableSource,
    SolveVariableSourceAttributes,
    SolveVariableEvaluatedValues,
);

/// Untrusted runtime-vector inputs consumed while sealing a [`SolveModel`].
///
/// This transient bundle is not a model or a proof authority. It has no
/// builder, default, validation, serialization, or conversion into checked IR;
/// only [`SolveModel::construct`] can correlate and retain its values.
pub struct SolveModelRuntimeInputs {
    pub initial_y: Vec<f64>,
    pub solver_nominals: Vec<f64>,
    pub parameters: Vec<f64>,
}

/// Construction-proved dense-prefix projections of one sealed Solve root.
///
/// [`SolveProblem`] construction proves that the state inventory prefixes the
/// solver-name partition and that the static-parameter inventory prefixes the
/// P column. These projections are read out exactly once at the
/// [`SolveModel::construct`] boundary, where a violated prefix is a typed
/// refusal; afterwards every accessor over them is total, so no hot-path read
/// carries a panic or a fallback. They are projections of the sealed vectors
/// retained beside them, not an independent authority: the root is immutable,
/// so they cannot diverge.
#[derive(Debug)]
struct SolveDensePrefixes {
    state_names: Box<[String]>,
    initial_state_values: Box<[f64]>,
    state_nominal_values: Box<[f64]>,
    static_parameter_values: Box<[f64]>,
}

impl SolveDensePrefixes {
    /// Project the proved prefixes out of the sealed problem and runtime
    /// vectors.
    ///
    /// This is not a second semantic validator. `SolveProblem::construct`
    /// already proves `state + algebraic + output == solver_scalar_count`,
    /// `solver_scalar_count` is by definition `solver_maps.names.len()`, that
    /// count equals `layout.y_scalars()`, and the enclosing constructor pins
    /// `initial_y.len()` to `y_scalars` and `parameters.len()` to
    /// `p_scalars >= parameter_count`, so every `None` arm below is
    /// unreachable for a constructible input. The checked accesses exist only
    /// because slicing is inherently bounds-checked in safe Rust; the dead
    /// arms surface as typed refusals, never as a panic or a truncation.
    fn mint(
        problem: &SolveProblem,
        initial_y: &[f64],
        solver_nominals: &[f64],
        parameters: &[f64],
    ) -> Result<Self, SolveModelConstructionError> {
        let state_count = problem.solve_layout.state_scalar_count();
        let parameter_count = problem.solve_layout.parameter_count;
        let state_names = checked_prefix(
            "solver_maps.names[..state_scalar_count]",
            &problem.solve_layout.solver_maps.names,
            state_count,
        )?;
        let initial_state_values =
            checked_prefix("initial_y[..state_scalar_count]", initial_y, state_count)?;
        let state_nominal_values = checked_prefix(
            "solver_nominals[..state_scalar_count]",
            solver_nominals,
            state_count,
        )?;
        let static_parameter_values =
            checked_prefix("parameters[..parameter_count]", parameters, parameter_count)?;
        Ok(Self {
            state_names: state_names.to_vec().into_boxed_slice(),
            initial_state_values: initial_state_values.to_vec().into_boxed_slice(),
            state_nominal_values: state_nominal_values.to_vec().into_boxed_slice(),
            static_parameter_values: static_parameter_values.to_vec().into_boxed_slice(),
        })
    }
}

fn checked_prefix<'values, T>(
    field: &'static str,
    values: &'values [T],
    count: usize,
) -> Result<&'values [T], SolveModelConstructionError> {
    values
        .get(..count)
        .ok_or(SolveModelConstructionError::VectorLength {
            field,
            expected: count,
            actual: values.len(),
        })
}

/// Solver-facing Solve IR package.
///
/// This is pure data. DAE inspection, scalarization, start evaluation, and
/// mass-matrix extraction happen before this value is constructed.
///
/// The sealed root is deliberately non-cloneable; consumers share one root
/// behind an `Arc` instead of duplicating it:
///
/// ```compile_fail,E0599
/// fn duplicate(model: rumoca_ir_solve::SolveModel) {
///     let _: rumoca_ir_solve::SolveModel = model.clone();
/// }
/// ```
#[derive(Debug)]
pub struct SolveModel {
    problem: SolveProblem,
    /// Exact pure DAE call frames shared by value, root, action, and visible
    /// projections. Even an empty fixture carries its caller-selected explicit
    /// arithmetic profile; lowering never reconstructs this table from rows.
    pure_calls: SolvePureCallTable,
    artifacts: SolveArtifacts,
    initial_y: Vec<f64>,
    /// Strictly positive normal nominal values aligned with solver `y` slots.
    solver_nominals: Vec<f64>,
    /// Finite positive runtime scales aligned with every solver `y` slot.
    solver_scales: Box<[f64]>,
    parameters: Vec<f64>,
    /// Construction-proved dense-prefix projections; see
    /// [`SolveDensePrefixes`].
    dense_prefixes: SolveDensePrefixes,
    visible_value_rows: ScalarProgramBlock,
    /// The sole declaration, metadata, storage, and evaluated-value authority.
    ///
    /// Entries are immutable and dense in `problem.solve_layout` declaration
    /// order. Trace and FMI surfaces derive their projections from this
    /// aggregate rather than storing parallel metadata vectors.
    variable_catalog: SolveVariableCatalog,
}

/// Opaque exact-refresh execution view borrowed from one sealed Solve root.
///
/// The schedule, programs, targets, layout extents, and pure-call table cannot
/// be independently paired by a backend caller.
pub struct ExactRefreshAssignmentExecution<'model> {
    sequence: RefreshSequenceId,
    programs: Box<[ExactRefreshAssignmentExecutionProgram<'model>]>,
    pure_calls: &'model SolvePureCallTable,
    y_extent: usize,
    p_extent: usize,
}

/// One borrowed program/target pair from an exact execution view.
pub struct ExactRefreshAssignmentExecutionProgram<'model> {
    program: crate::ScalarProgramExecution<'model>,
    targets: &'model [usize],
}

impl ExactRefreshAssignmentExecution<'_> {
    #[must_use]
    pub const fn sequence(&self) -> RefreshSequenceId {
        self.sequence
    }

    #[must_use]
    pub const fn y_extent(&self) -> usize {
        self.y_extent
    }

    #[must_use]
    pub const fn p_extent(&self) -> usize {
        self.p_extent
    }

    #[must_use]
    pub const fn pure_calls(&self) -> &SolvePureCallTable {
        self.pure_calls
    }

    pub fn programs(
        &self,
    ) -> impl ExactSizeIterator<Item = &ExactRefreshAssignmentExecutionProgram<'_>> {
        self.programs.iter()
    }
}

impl ExactRefreshAssignmentExecutionProgram<'_> {
    #[must_use]
    pub const fn operations(&self) -> &[LinearOp] {
        self.program.operations()
    }

    #[must_use]
    pub const fn register_count(&self) -> usize {
        self.program.register_count()
    }

    #[must_use]
    pub const fn output_sources(&self) -> &[usize] {
        self.program.output_sources()
    }

    #[must_use]
    pub const fn output_count(&self) -> usize {
        self.program.output_sources().len()
    }

    #[must_use]
    pub const fn targets(&self) -> &[usize] {
        self.targets
    }
}

impl SolveModel {
    /// Construct and seal one complete Solve root.
    ///
    /// Catalog entries are untrusted construction inputs. The catalog is issued
    /// internally against the exact problem and runtime vectors retained by the
    /// returned root; failure exposes no partial `SolveModel`.
    pub fn construct(
        problem: SolveProblem,
        pure_calls: SolvePureCallTable,
        artifact_inputs: SolveArtifactInputs,
        runtime: SolveModelRuntimeInputs,
        visible_value_rows: ScalarProgramBlock,
        variable_entries: impl IntoIterator<Item = SolveVariableCatalogSourceEntry>,
    ) -> Result<Self, SolveModelConstructionError> {
        let SolveModelRuntimeInputs {
            initial_y,
            solver_nominals,
            parameters,
        } = runtime;
        require_model_vector_length("initial_y", problem.layout().y_scalars(), initial_y.len())?;
        require_model_vector_length(
            "solver_nominals",
            problem.layout().y_scalars(),
            solver_nominals.len(),
        )?;
        require_model_vector_length("parameters", problem.layout().p_scalars(), parameters.len())?;
        // Nominals must be strictly positive *normal* numbers: the scale vector
        // is bounded below by the nominal, and downstream reciprocal weighting
        // divides by it. Every positive normal binary64 has a finite
        // reciprocal, so refusing zero, subnormal, negative, and non-finite
        // nominals here makes an overflowing scale reciprocal unrepresentable
        // instead of a runtime surprise.
        if let Some(index) = solver_nominals
            .iter()
            .position(|value| !(value.is_normal() && *value > 0.0))
        {
            return Err(SolveModelConstructionError::InvalidSolverNominal { index });
        }
        // Initial values must be finite for every solver slot, whether or not
        // a declared start covers the slot; this is the single choke point
        // that keeps `solver_scale` total over its admitted domain.
        if let Some(index) = initial_y.iter().position(|value| !value.is_finite()) {
            return Err(SolveModelConstructionError::InvalidInitialValue { index });
        }
        let solver_scales: Box<[f64]> = solver_nominals
            .iter()
            .zip(&initial_y)
            .map(|(nominal, start)| solver_scale(*nominal, *start))
            .collect();
        let artifacts = SolveArtifacts::checked(&problem, artifact_inputs)?;
        let mut catalog =
            SolveVariableCatalog::begin(&problem, &initial_y, &solver_nominals, &parameters);
        for (identity, attributes, values) in variable_entries {
            catalog.issue(identity, attributes, values)?;
        }
        let variable_catalog = catalog.finish()?;
        let visible_name_count = variable_catalog.visible_scalar_count();
        require_model_vector_length(
            "visible_value_rows",
            visible_name_count,
            visible_value_rows.row_count(),
        )?;
        require_model_vector_length(
            "visible_value_outputs",
            visible_name_count,
            visible_value_rows.output_count(),
        )?;
        if !visible_value_rows.uses_local_contiguous_output_indices() {
            return Err(SolveModelConstructionError::VisibleOutputIndices);
        }
        let dense_prefixes =
            SolveDensePrefixes::mint(&problem, &initial_y, &solver_nominals, &parameters)?;
        let model = Self {
            problem,
            pure_calls,
            artifacts,
            initial_y,
            solver_nominals,
            solver_scales,
            parameters,
            dense_prefixes,
            visible_value_rows,
            variable_catalog,
        };
        crate::variable_bounds::validate_solve_model_program_bounds(&model)?;
        crate::validate_solve_model_pure_call_sites(&model)?;
        Ok(model)
    }

    #[must_use]
    pub const fn problem(&self) -> &SolveProblem {
        &self.problem
    }

    #[must_use]
    pub const fn pure_calls(&self) -> &SolvePureCallTable {
        &self.pure_calls
    }

    #[must_use]
    pub const fn artifacts(&self) -> &SolveArtifacts {
        &self.artifacts
    }

    /// Borrow one exact assignment execution from this model's sealed owner.
    #[must_use]
    pub fn exact_refresh_assignment_execution(
        &self,
        sequence: RefreshSequenceId,
    ) -> Option<ExactRefreshAssignmentExecution<'_>> {
        let owners = self.problem.continuous().refresh_owners();
        let schedule = owners.exact_assignment_schedule(sequence)?;
        let programs = schedule
            .program_ids()
            .iter()
            .map(|id| {
                let owner = owners.exact_assignment_program(*id)?;
                Some(ExactRefreshAssignmentExecutionProgram {
                    program: owner.final_program().sole_execution_program()?,
                    targets: owner.target_indices(),
                })
            })
            .collect::<Option<Vec<_>>>()?
            .into_boxed_slice();
        Some(ExactRefreshAssignmentExecution {
            sequence,
            programs,
            pure_calls: &self.pure_calls,
            y_extent: self.problem.layout().y_scalars(),
            p_extent: self.problem.layout().p_scalars(),
        })
    }

    #[must_use]
    pub fn initial_y(&self) -> &[f64] {
        &self.initial_y
    }

    #[must_use]
    pub fn solver_nominals(&self) -> &[f64] {
        &self.solver_nominals
    }

    #[must_use]
    pub fn parameters(&self) -> &[f64] {
        &self.parameters
    }

    #[must_use]
    pub fn visible_name_count(&self) -> usize {
        self.variable_catalog.visible_scalar_count()
    }

    pub fn visible_names(&self) -> impl Iterator<Item = &str> {
        self.variable_catalog.visible_scalar_names()
    }

    #[must_use]
    pub const fn visible_value_rows(&self) -> &ScalarProgramBlock {
        &self.visible_value_rows
    }

    #[must_use]
    pub const fn variable_catalog(&self) -> &SolveVariableCatalog {
        &self.variable_catalog
    }

    /// Borrow only the name-free facts required to prove DAE-to-Solve
    /// variable refinement.
    #[must_use]
    pub fn variable_refinement(&self) -> SolveVariableRefinementView {
        SolveVariableRefinementView::new(self.variable_catalog.entries())
    }

    /// Derive scalar trace/report metadata from the declaration catalog.
    ///
    /// Only externally visible numeric storage participates, in the same dense
    /// scalar order as `visible_names` and `visible_value_rows`.
    #[must_use]
    pub fn variable_meta(&self) -> Vec<SolveVariableMeta> {
        self.variable_catalog
            .entries()
            .iter()
            .filter(|entry| is_visible_catalog_role(entry.role()))
            .flat_map(variable_entry_meta)
            .collect()
    }

    pub fn state_scalar_count(&self) -> usize {
        self.problem.solve_layout.state_scalar_count()
    }

    pub fn solver_scalar_count(&self) -> usize {
        self.problem.solve_layout.solver_scalar_count()
    }

    /// State names in exact leading Y-slot order.
    ///
    /// The prefix was proved and projected at construction; this read is
    /// total.
    pub fn state_names(&self) -> &[String] {
        &self.dense_prefixes.state_names
    }

    /// Constructor-evaluated state starts in the same order as [`Self::state_names`].
    ///
    /// The prefix was proved and projected at construction; this read is
    /// total.
    pub fn initial_state_values(&self) -> &[f64] {
        &self.dense_prefixes.initial_state_values
    }

    /// Effective continuous-state nominals in the same order as
    /// [`Self::state_names`]. These are the declared values after Modelica's
    /// nominal default has been applied; they are deliberately distinct from
    /// the start-sensitive numerical scales returned by
    /// [`Self::solver_variable_scales`].
    ///
    /// The prefix was proved and projected at construction; this read is
    /// total.
    pub(crate) fn state_nominal_values(&self) -> &[f64] {
        &self.dense_prefixes.state_nominal_values
    }

    /// Parameter and constant names in exact leading P-slot order.
    pub fn static_parameter_names(&self) -> &[String] {
        self.problem.solve_layout.static_parameter_names()
    }

    /// Constructor-evaluated parameter and constant values in the same order as
    /// [`Self::static_parameter_names`].
    ///
    /// The prefix was proved and projected at construction; this read is
    /// total.
    pub fn static_parameter_values(&self) -> &[f64] {
        &self.dense_prefixes.static_parameter_values
    }

    /// Return the complete finite-positive solver-variable scale vector.
    ///
    /// The declared nominal is the baseline. A larger start magnitude expands
    /// the scale so solver tolerances remain meaningful for large initial
    /// values and runtime start overrides.
    #[must_use]
    pub fn solver_variable_scales(&self) -> &[f64] {
        &self.solver_scales
    }
}

/// Total over the constructor-admitted domain: the nominal is a strictly
/// positive normal number and the start is finite, both refused otherwise
/// before this runs. `f64::max` therefore never observes a NaN operand, and
/// the result is bounded below by the normal nominal, so every scale and its
/// reciprocal stay finite.
pub(crate) fn solver_scale(nominal: f64, start: f64) -> f64 {
    nominal.max(start.abs())
}

fn require_model_vector_length(
    field: &'static str,
    expected: usize,
    actual: usize,
) -> Result<(), SolveModelConstructionError> {
    if actual == expected {
        return Ok(());
    }
    Err(SolveModelConstructionError::VectorLength {
        field,
        expected,
        actual,
    })
}

impl SolveArtifacts {
    fn checked(
        problem: &SolveProblem,
        inputs: SolveArtifactInputs,
    ) -> Result<Self, SolveModelConstructionError> {
        validate_artifact_child_shapes(&inputs)?;
        let dimensions = validate_artifact_output_dimensions(problem, &inputs)?;
        validate_artifact_exact_row_coverage(problem, &inputs)?;
        validate_artifact_structural_dimensions(problem, &inputs, dimensions)?;
        Ok(Self {
            continuous: inputs.continuous,
            initialization: inputs.initialization,
        })
    }
}

#[derive(Clone, Copy)]
struct ArtifactDimensions {
    implicit_rows: usize,
    manifold_rows: usize,
    derivative_rows: usize,
    initialization_rows: usize,
    solver_columns: usize,
    full_columns: usize,
}

fn validate_artifact_output_dimensions(
    problem: &SolveProblem,
    artifacts: &SolveArtifactInputs,
) -> Result<ArtifactDimensions, SolveModelConstructionError> {
    let implicit_rows = problem.continuous().implicit_rhs.len()?;
    let manifold_rows = problem.continuous().manifold_residual.len()?;
    let derivative_rows = problem.continuous().derivative_rhs.len()?;
    let initialization_rows = problem.initialization().residual.len()?;
    require_artifact_outputs(
        "continuous.implicit_jacobian_v",
        implicit_rows,
        artifacts.continuous.implicit_jacobian_v.len()?,
    )?;
    require_artifact_outputs(
        "continuous.implicit_jacobian_v_scalar",
        implicit_rows,
        artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .output_count(),
    )?;
    require_artifact_outputs(
        "continuous.manifold_jacobian_v",
        manifold_rows,
        artifacts.continuous.manifold_jacobian_v.len()?,
    )?;
    require_artifact_outputs(
        "continuous.full_jacobian_v",
        derivative_rows,
        artifacts.continuous.full_jacobian_v.output_count(),
    )?;
    require_artifact_outputs(
        "initialization.residual_jacobian_v",
        initialization_rows,
        artifacts.initialization.residual_jacobian_v.len()?,
    )?;
    validate_mass_matrix(
        &artifacts.continuous.mass_matrix,
        problem.solve_layout().state_scalar_count(),
    )?;
    let solver_columns = problem.solve_layout().solver_scalar_count();
    let full_columns = problem
        .layout()
        .y_scalars()
        .checked_add(problem.layout().p_scalars())
        .ok_or(SolveModelConstructionError::MassMatrix {
            reason: "Solve vector cardinality overflows host index range",
        })?;
    Ok(ArtifactDimensions {
        implicit_rows,
        manifold_rows,
        derivative_rows,
        initialization_rows,
        solver_columns,
        full_columns,
    })
}

fn validate_artifact_structural_dimensions(
    problem: &SolveProblem,
    artifacts: &SolveArtifactInputs,
    dimensions: ArtifactDimensions,
) -> Result<(), SolveModelConstructionError> {
    let ArtifactDimensions {
        implicit_rows,
        manifold_rows,
        derivative_rows,
        initialization_rows,
        solver_columns,
        full_columns,
    } = dimensions;
    require_optional_pattern(
        "continuous.structural.implicit",
        artifacts.continuous.structural.implicit(),
        implicit_rows,
        solver_columns,
    )?;
    require_optional_pattern(
        "continuous.structural.manifold",
        artifacts.continuous.structural.manifold(),
        manifold_rows,
        solver_columns,
    )?;
    require_optional_pattern(
        "continuous.structural.derivative",
        artifacts.continuous.structural.derivative(),
        derivative_rows,
        full_columns,
    )?;
    require_optional_pattern(
        "initialization.structural.residual",
        artifacts.initialization.structural.residual(),
        initialization_rows,
        full_columns,
    )?;
    require_projection_count(
        "continuous.structural.algebraic_projection",
        problem.continuous().algebraic_projection_plan.blocks.len(),
        artifacts.continuous.structural.algebraic_projection().len(),
    )?;
    require_projection_count(
        "continuous.structural.manifold_projection",
        problem.continuous().manifold_projection_plan.blocks.len(),
        artifacts.continuous.structural.manifold_projection().len(),
    )?;
    require_projection_count(
        "initialization.structural.projection",
        problem.initialization().projection_plan.blocks.len(),
        artifacts.initialization.structural.projection().len(),
    )?;
    require_projection_count(
        "continuous.structural.algebraic_invalidates_earlier",
        problem.continuous().algebraic_projection_plan.blocks.len(),
        artifacts
            .continuous
            .structural
            .algebraic_invalidates_earlier
            .len(),
    )?;
    Ok(())
}

fn require_artifact_outputs(
    artifact: &'static str,
    expected: usize,
    actual: usize,
) -> Result<(), SolveModelConstructionError> {
    if expected == actual {
        return Ok(());
    }
    Err(SolveModelConstructionError::ArtifactOutputCount {
        artifact,
        expected,
        actual,
    })
}

fn require_projection_count(
    artifact: &'static str,
    expected: usize,
    actual: usize,
) -> Result<(), SolveModelConstructionError> {
    if expected == actual {
        return Ok(());
    }
    Err(SolveModelConstructionError::ArtifactProjectionCount {
        artifact,
        expected,
        actual,
    })
}

fn require_optional_pattern(
    artifact: &'static str,
    pattern: Option<&JacobianStructure>,
    expected_rows: usize,
    expected_columns: usize,
) -> Result<(), SolveModelConstructionError> {
    let Some(pattern) = pattern else {
        return if expected_rows == 0 {
            Ok(())
        } else {
            Err(SolveModelConstructionError::MissingArtifactPattern { artifact })
        };
    };
    if expected_rows == 0 {
        return Err(SolveModelConstructionError::UnexpectedArtifactPattern { artifact });
    }
    let pattern = pattern.pattern();
    let actual_rows = pattern.rows() as usize;
    let actual_columns = pattern.columns() as usize;
    if actual_rows == expected_rows && actual_columns == expected_columns {
        return Ok(());
    }
    Err(SolveModelConstructionError::ArtifactPatternShape {
        artifact,
        expected_rows,
        expected_columns,
        actual_rows,
        actual_columns,
    })
}

fn validate_mass_matrix(
    matrix: &MassMatrix,
    state_count: usize,
) -> Result<(), SolveModelConstructionError> {
    match matrix {
        MassMatrix::Identity => Ok(()),
        MassMatrix::Diagonal { values } => {
            if values.len() != state_count {
                return Err(SolveModelConstructionError::MassMatrix {
                    reason: "diagonal dimension does not match the state count",
                });
            }
            if values.iter().any(|value| !value.is_finite()) {
                return Err(SolveModelConstructionError::MassMatrix {
                    reason: "diagonal coefficient is non-finite",
                });
            }
            Ok(())
        }
        MassMatrix::Sparse { entries } => {
            if entries.iter().any(|entry| {
                entry.row >= state_count || entry.column >= state_count || !entry.value.is_finite()
            }) {
                return Err(SolveModelConstructionError::MassMatrix {
                    reason: "sparse entry is out of bounds or non-finite",
                });
            }
            Ok(())
        }
    }
}

fn variable_entry_meta(entry: &SolveVariableCatalogEntry) -> Vec<SolveVariableMeta> {
    entry
        .scalar_names()
        .iter()
        .enumerate()
        .map(|(scalar, name)| SolveVariableMeta {
            name: name.clone(),
            source_span: entry.provenance(),
            role: catalog_role_name(entry.role()).to_string(),
            is_state: entry.role() == SolveVariableStorageRole::State,
            value_type: Some(format!("{:?}", entry.value_kind())),
            variability: Some(catalog_trace_variability(entry.variability()).to_string()),
            time_domain: Some(entry.time_domain().as_str().to_string()),
            unit: entry.unit().map(str::to_string),
            start: scalar_attribute(entry.start(), scalar),
            min: scalar_attribute(entry.minimum(), scalar),
            max: scalar_attribute(entry.maximum(), scalar),
            nominal: scalar_attribute(entry.nominal(), scalar),
            fixed: entry.fixed(),
            description: entry.description().map(str::to_string),
        })
        .collect()
}

fn scalar_attribute(values: Option<&[f64]>, scalar: usize) -> Option<String> {
    values
        .and_then(|values| values.get(scalar))
        .map(ToString::to_string)
}

const fn is_visible_catalog_role(role: SolveVariableStorageRole) -> bool {
    matches!(
        role,
        SolveVariableStorageRole::ExternalInput
            | SolveVariableStorageRole::State
            | SolveVariableStorageRole::Algebraic
            | SolveVariableStorageRole::Output
            | SolveVariableStorageRole::DiscreteReal
            | SolveVariableStorageRole::DiscreteValue
    )
}

const fn catalog_role_name(role: SolveVariableStorageRole) -> &'static str {
    match role {
        SolveVariableStorageRole::Parameter => "parameter",
        SolveVariableStorageRole::Constant => "constant",
        SolveVariableStorageRole::ExternalInput => "input",
        SolveVariableStorageRole::State => "state",
        SolveVariableStorageRole::Algebraic => "algebraic",
        SolveVariableStorageRole::Output => "output",
        SolveVariableStorageRole::DiscreteReal => "discrete-real",
        SolveVariableStorageRole::DiscreteValue => "discrete-valued",
    }
}

const fn catalog_trace_variability(variability: SolveVariableVariability) -> &'static str {
    match variability {
        SolveVariableVariability::Constant => "Constant",
        SolveVariableVariability::Fixed | SolveVariableVariability::Tunable => "Parameter",
        SolveVariableVariability::Discrete => "Discrete",
        SolveVariableVariability::Continuous => "Continuous",
    }
}

pub fn solver_idx_for_target(target: &str, name_to_idx: &IndexMap<String, usize>) -> Option<usize> {
    if let Some(&idx) = name_to_idx.get(target) {
        return Some(idx);
    }
    if let Some(scalar) = rumoca_core::parse_scalar_name(target)
        && scalar.indices.iter().all(|index| *index == 1)
    {
        return name_to_idx.get(scalar.base).copied();
    }
    None
}
