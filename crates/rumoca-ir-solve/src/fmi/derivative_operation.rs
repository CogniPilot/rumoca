//! The complete retained source of one FMI continuous-derivative operation.
//!
//! This is a source refinement, not target or execution admission. It retains
//! the original tensor values and refresh owner together, before a target
//! issues the operation's executable schedule.

mod causal;
mod control_builder;
mod control_plan;
mod kinematic;
pub use causal::{
    FmiDerivativeCausalOperation, FmiDerivativeCausalRefusal, FmiDerivativeStatus,
    FmiDerivativeWorkspace,
};
pub use control_plan::{
    FmiDerivativeControlPlan, Helper as FmiDerivativeHelper,
    Instruction as FmiDerivativeControlInstruction,
};
pub use kinematic::FmiKinematicDerivativeOperation;

use super::{FmiMetadata, FmiRuntimeView};
use crate::{
    ComputeBlock, RefreshPlan, RetainedAssertionRoot, SolveModel, SolveProblem,
    SolveProblemShapeContractError, SolvePureCallTable, StagedRefreshRefusal, StagedRefreshStep,
};

/// A refused derivative vector contract of an otherwise described component.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FmiDerivativeOperationRefusal {
    Shape(Box<SolveProblemShapeContractError>),
    OutputExtent { states: usize, outputs: usize },
}

impl std::fmt::Display for FmiDerivativeOperationRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Shape(error) => error.fmt(f),
            Self::OutputExtent { states, outputs } => write!(
                f,
                "FMI derivative source has {outputs} output slots for {states} states"
            ),
        }
    }
}

impl std::error::Error for FmiDerivativeOperationRefusal {}

/// One original component lends both the derivative dependency closure and
/// tensor result. No public constructor accepts a block, layout, or metadata
/// from another root. Deliberately neither cloneable nor serializable.
///
/// This does not certify that a target can execute the complete operation;
/// delay, coupled, event and storage requirements remain in the same source.
///
/// ```compile_fail
/// use rumoca_ir_solve::fmi::{FmiDerivativeOperationSource, FmiRuntimeView};
/// fn forge(source: FmiRuntimeView<'_>) -> FmiDerivativeOperationSource<'_> {
///     let (model, metadata, _) = source.into_parts();
///     FmiDerivativeOperationSource { model, metadata, outputs: 0 }
/// }
/// ```
#[derive(Debug)]
pub struct FmiDerivativeOperationSource<'component> {
    model: &'component SolveModel,
    metadata: &'component FmiMetadata,
    outputs: usize,
}

impl<'component> FmiRuntimeView<'component> {
    /// Consume the correlated source view without scalarizing its tensors.
    /// The new source has no partial-result or caller-supplied selector path.
    pub fn try_derivative_operation(
        self,
    ) -> Result<FmiDerivativeOperationSource<'component>, FmiDerivativeOperationRefusal> {
        borrow_original(self.model, self.metadata)
    }
}

impl RetainedAssertionRoot {
    /// Borrow the same complete operation from the correlated source already
    /// retained by a runtime. Standalone Solve models cannot issue this view.
    pub fn try_derivative_operation(
        &self,
    ) -> Result<FmiDerivativeOperationSource<'_>, FmiDerivativeOperationRefusal> {
        borrow_original(self.model(), self.metadata())
    }
}

fn borrow_original<'component>(
    model: &'component SolveModel,
    metadata: &'component FmiMetadata,
) -> Result<FmiDerivativeOperationSource<'component>, FmiDerivativeOperationRefusal> {
    let problem = &model.problem;
    let outputs = problem
        .continuous
        .derivative_rhs
        .checked_source_view("FMI continuous-derivative operation")
        .map_err(|error| FmiDerivativeOperationRefusal::Shape(Box::new(error)))?
        .output_count();
    let states = problem.solve_layout.state_scalar_count();
    if outputs != states {
        return Err(FmiDerivativeOperationRefusal::OutputExtent { states, outputs });
    }
    Ok(FmiDerivativeOperationSource {
        model,
        metadata,
        outputs,
    })
}

impl<'component> FmiDerivativeOperationSource<'component> {
    /// Original tensor input/output extent in FMI state order.
    pub fn vector_extent(&self) -> usize {
        self.outputs
    }

    /// The original complete problem: requirements must not be inferred from
    /// just the last output expression or from a reconstructed scalar view.
    pub fn problem(&self) -> &'component SolveProblem {
        &self.model.problem
    }

    /// Canonical dependency stages, including their checked projection owners.
    pub fn dependencies(&self) -> &'component RefreshPlan {
        self.problem().continuous.refresh_owners.derivative()
    }

    /// Read the complete refresh dispatch with this same root's structural
    /// artifacts. A consumer cannot substitute a foreign certificate or
    /// choose the certified dispatch itself. Projection steps and the
    /// complete-projection fallback remain explicit; no unsupported step is
    /// silently replaced by an empty dependency closure.
    ///
    /// These are borrowed steps of the canonical refresh owner, not new
    /// assignments or a target execution admission. Static-cache, rollback
    /// and fault semantics remain those of the original refresh operation.
    pub fn staged_dependencies(
        &self,
    ) -> Result<Vec<StagedRefreshStep<'component>>, StagedRefreshRefusal> {
        let dependencies = self.dependencies();
        let structural = &self.model.artifacts.continuous.structural;
        self.problem()
            .continuous
            .refresh_owners
            .staged_refresh_steps(dependencies, dependencies.stage_schedule(structural))
    }

    /// The authoritative compact result, with source scopes and placements.
    pub fn values(&self) -> &'component ComputeBlock {
        &self.problem().continuous.derivative_rhs
    }

    /// Calls belong to the same retained source, not a caller-supplied table.
    pub fn calls(&self) -> &'component SolvePureCallTable {
        &self.model.pure_calls
    }

    /// Original typed vector inventory paired with this operation's source.
    pub fn inventory(&self) -> &'component FmiMetadata {
        self.metadata
    }
}
