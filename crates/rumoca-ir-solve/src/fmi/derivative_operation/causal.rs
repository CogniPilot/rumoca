//! Complete causal derivative control and private workspace from one FMI root.
use super::*;
use crate::{CheckedNativeWasmCapacityPolicy, ExactRefreshAssignmentSchedule, IndexIntervals};

/// An original operation that this exact-assignment execution form cannot print.
#[derive(Debug)]
pub enum FmiDerivativeCausalRefusal {
    Dispatch(StagedRefreshRefusal),
    ProjectionRequired,
    Source(crate::ContinuousRefreshConstructionError),
    Storage(String),
}

impl std::fmt::Display for FmiDerivativeCausalRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dispatch(error) => write!(f, "derivative dispatch refused: {error:?}"),
            Self::ProjectionRequired => {
                f.write_str("derivative operation requires its projection executor")
            }
            Self::Source(error) => error.fmt(f),
            Self::Storage(error) => f.write_str(error),
        }
    }
}
impl std::error::Error for FmiDerivativeCausalRefusal {}

/// The full causal operation, rather than just its last value expression.
/// Fields cannot be assembled from unrelated schedules, cache keys or values.
/// A projection-bearing operation remains explicit and is not certified here.
#[derive(Debug)]
pub struct FmiDerivativeCausalOperation<'source> {
    source: FmiDerivativeOperationSource<'source>,
    static_schedule: Option<&'source ExactRefreshAssignmentSchedule>,
    dynamic_schedule: Option<&'source ExactRefreshAssignmentSchedule>,
    static_targets: IndexIntervals,
    parameter_keys: Box<[usize]>,
    parameter_runs: Vec<(usize, usize, usize)>,
    kinematic: Result<super::FmiKinematicDerivativeOperation, super::FmiKinematicRefusal>,
}

impl<'source> FmiDerivativeOperationSource<'source> {
    pub fn try_causal_operation(
        self,
    ) -> Result<FmiDerivativeCausalOperation<'source>, FmiDerivativeCausalRefusal> {
        let steps = self
            .staged_dependencies()
            .map_err(FmiDerivativeCausalRefusal::Dispatch)?;
        let plan = self.dependencies();
        // This is exactly the evaluator's complete causal branch. A warm-start
        // sequence cannot become a solution by dropping its final projection.
        if !plan.causal_solution_certified && !steps.is_empty() {
            return Err(FmiDerivativeCausalRefusal::ProjectionRequired);
        }
        let (mut static_schedule, mut dynamic_schedule) = (None, None);
        for step in steps {
            match step {
                StagedRefreshStep::Assignments(schedule)
                    if schedule.sequence_id() == plan.static_causal_sequence =>
                {
                    static_schedule = Some(schedule);
                }
                StagedRefreshStep::Assignments(schedule)
                    if schedule.sequence_id() == plan.dynamic_causal_sequence =>
                {
                    dynamic_schedule = Some(schedule);
                }
                _ => return Err(FmiDerivativeCausalRefusal::ProjectionRequired),
            }
        }
        let static_targets =
            IndexIntervals::of(plan.static_causal_rows().iter().map(|r| r.target_index()));
        let parameter_keys = self
            .problem()
            .continuous
            .refresh_owners
            .static_cache_parameters(&self.problem().continuous.implicit_rhs)
            .map_err(FmiDerivativeCausalRefusal::Source)?;
        let parameter_runs = parameter_runs(&parameter_keys);
        let kinematic = super::FmiKinematicDerivativeOperation::issue(&self);
        Ok(FmiDerivativeCausalOperation {
            source: self,
            static_schedule,
            dynamic_schedule,
            static_targets,
            parameter_keys,
            parameter_runs,
            kinematic,
        })
    }
}

impl<'source> FmiDerivativeCausalOperation<'source> {
    pub fn source(&self) -> &FmiDerivativeOperationSource<'source> {
        &self.source
    }
    pub fn static_schedule(&self) -> Option<&'source ExactRefreshAssignmentSchedule> {
        self.static_schedule
    }
    pub fn dynamic_schedule(&self) -> Option<&'source ExactRefreshAssignmentSchedule> {
        self.dynamic_schedule
    }
    pub fn static_targets(&self) -> &IndexIntervals {
        &self.static_targets
    }
    pub fn parameter_keys(&self) -> &[usize] {
        &self.parameter_keys
    }

    pub fn parameter_runs(&self) -> &[(usize, usize, usize)] {
        &self.parameter_runs
    }

    /// The kinematic rows over the settled coordinate, when the source issues
    /// a proper partition (see [`super::FmiKinematicDerivativeOperation`]).
    pub fn kinematic(&self) -> Option<&super::FmiKinematicDerivativeOperation> {
        self.kinematic.as_ref().ok()
    }
    /// The kinematic entry, or why none was issued.
    pub fn kinematic_issuance(
        &self,
    ) -> Result<&super::FmiKinematicDerivativeOperation, super::FmiKinematicRefusal> {
        self.kinematic.as_ref().map_err(|refusal| *refusal)
    }

    /// Child executables report their actual private scratch demand. These
    /// calls are sequential in this operation, so one disjoint helper region
    /// serves the maximum demand. No child may alias the rollback coordinate
    /// or operation status record.
    pub fn workspace(
        &self,
        helper_bytes: impl IntoIterator<Item = u32>,
        capacity: CheckedNativeWasmCapacityPolicy,
    ) -> Result<FmiDerivativeWorkspace, FmiDerivativeCausalRefusal> {
        let cells = |n: usize| {
            n.checked_mul(8)
                .and_then(|n| u32::try_from(n).ok())
                .ok_or_else(|| {
                    FmiDerivativeCausalRefusal::Storage(
                        "derivative memory32 extent overflows".into(),
                    )
                })
        };
        let y_cells = self.source.problem().layout.y_scalars();
        let p_cells = self.source.problem().layout.p_scalars();
        if self.static_targets.iter().any(|target| target >= y_cells)
            || self
                .parameter_keys
                .iter()
                .any(|&parameter| parameter >= p_cells)
        {
            return Err(FmiDerivativeCausalRefusal::Storage(
                "derivative cache coordinates exceed the original layout".into(),
            ));
        }
        let y_bytes = cells(y_cells)?;
        let output_bytes = cells(self.source.vector_extent())?;
        let helper = 16u32.checked_add(y_bytes).ok_or_else(|| {
            FmiDerivativeCausalRefusal::Storage("derivative snapshot overflows".into())
        })?;
        let helper_bytes = helper_bytes
            .into_iter()
            .max()
            .unwrap_or(0)
            .max(output_bytes);
        let bytes = helper.checked_add(helper_bytes).ok_or_else(|| {
            FmiDerivativeCausalRefusal::Storage("derivative helper extent overflows".into())
        })?;
        capacity
            .require_public_bytes(bytes)
            .map_err(FmiDerivativeCausalRefusal::Storage)?;
        let p_bytes = cells(self.source.problem().layout.p_scalars())?;
        let cache_values = if self.static_schedule.is_some() {
            y_bytes
        } else {
            0
        };
        let cache_keys = if self.static_schedule.is_some() {
            cells(self.parameter_keys.len())?
        } else {
            0
        };
        Ok(FmiDerivativeWorkspace {
            bytes,
            y_bytes,
            output_bytes,
            snapshot: 16,
            helper,
            p_bytes,
            cache_values,
            cache_keys,
        })
    }
}

/// Issued disjoint private regions. Words 0/4/8/12 are phase/native-status/
/// nonfinite-index/cache-prepared. Refresh errors restore `snapshot`; value
/// errors retain the refreshed Y. The complete value vector precedes finite
/// validation, and only a zero final status is a successful operation.
#[derive(Clone, Copy, Debug)]
pub struct FmiDerivativeWorkspace {
    bytes: u32,
    y_bytes: u32,
    output_bytes: u32,
    snapshot: u32,
    helper: u32,
    p_bytes: u32,
    cache_values: u32,
    cache_keys: u32,
}
impl FmiDerivativeWorkspace {
    /// Argument ordinals and disjoint memory spans of the memory32 ABI.
    pub fn regions(self) -> [(u32, u32); 7] {
        [
            (0, self.y_bytes),
            (1, self.p_bytes),
            (3, self.output_bytes),
            (4, self.bytes),
            (5, self.cache_values),
            (6, self.cache_values / 8),
            (7, self.cache_keys),
        ]
    }
    pub fn bytes(self) -> u32 {
        self.bytes
    }
    pub fn y_bytes(self) -> u32 {
        self.y_bytes
    }
    pub fn output_bytes(self) -> u32 {
        self.output_bytes
    }
    pub fn snapshot(self) -> u32 {
        self.snapshot
    }
    pub fn helper(self) -> u32 {
        self.helper
    }
}

/// Complete-operation result; a child status is retained separately.
#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FmiDerivativeStatus {
    Success = 0,
    InvalidBuffers = 1,
    StaticRefresh = 2,
    DynamicRefresh = 3,
    Values = 4,
    NonFinite = 5,
}

fn parameter_runs(keys: &[usize]) -> Vec<(usize, usize, usize)> {
    let mut runs: Vec<(usize, usize, usize)> = Vec::new();
    for (position, &parameter) in keys.iter().enumerate() {
        if let Some((_, first, count)) = runs.last_mut()
            && first.checked_add(*count) == Some(parameter)
        {
            *count += 1;
        } else {
            runs.push((position, parameter, 1));
        }
    }
    runs
}

impl FmiDerivativeStatus {
    pub fn from_code(code: u32) -> Option<Self> {
        match code {
            0 => Some(Self::Success),
            1 => Some(Self::InvalidBuffers),
            2 => Some(Self::StaticRefresh),
            3 => Some(Self::DynamicRefresh),
            4 => Some(Self::Values),
            5 => Some(Self::NonFinite),
            _ => None,
        }
    }
    /// This same failure effect applies when an engine trap interrupts a helper.
    pub fn restores_coordinate(self) -> bool {
        matches!(self, Self::StaticRefresh | Self::DynamicRefresh)
    }
    pub fn has_evaluated_values(self) -> bool {
        matches!(self, Self::Success | Self::Values | Self::NonFinite)
    }
}
