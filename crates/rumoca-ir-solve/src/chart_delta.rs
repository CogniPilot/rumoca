//! Typed deltas of an alternate reduced chart's plan against the primary basis
//! (SPEC_0040 STRUCT-T07 constraint-fold chart rows).
//!
//! An alternate chart is prepared and lowered through the same checked
//! construction as the primary, and its plan differs from the primary in a few
//! rows: the state-binding rows of the exchanged coordinates, the projection
//! blocks that reconstruct them, and the refresh-owner rows and stages that
//! schedule those blocks. The wire form stores each alternate as replacements
//! of those entries; decoding patches the primary to reproduce the plan. A
//! delta is only ever issued after [`ChartPlanDelta::diff`] proves that
//! applying it to the primary reproduces the lowered plan exactly.

use serde::{Deserialize, Serialize};

use crate::{
    AlgebraicProjectionPlan, ComputeBlock, ComputeNode, ContinuousRefreshOwners,
    ContinuousSolveArtifacts, ContinuousSolveSystem, LinearOp, ReducedChartPlan, RefreshPlan,
    ScalarProgramBlock, ScalarSlot,
};

/// Entries of a base vector replaced by index when the lengths agree, an edit
/// script when they differ by a few insertions and deletions, or a whole
/// replacement.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub(crate) enum VecDelta<T> {
    Replace(Vec<(u32, T)>),
    Edits(Vec<Edit<T>>),
    Whole(Vec<T>),
}

/// One step of an edit script over a base vector.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub(crate) enum Edit<T> {
    /// Keep the next `count` base entries.
    Keep(u32),
    /// Drop the next `count` base entries.
    Delete(u32),
    /// Insert these entries.
    Insert(Vec<T>),
}

/// Most insertions plus deletions an edit script searches for before the
/// delta falls back to a whole replacement.
const MAX_EDIT_DISTANCE: usize = 1024;

/// Fewest index replacements of an equal-length vector for which an edit
/// script is also searched.
const MIN_SHIFT_REPLACEMENTS: usize = 16;

impl<T: Clone + PartialEq> VecDelta<T> {
    /// The delta turning `base` into `target`.
    pub(crate) fn diff(base: &[T], target: &[T]) -> Self {
        // A shifted run makes many index replacements but few edits; carry
        // whichever form holds fewer entries.
        let replace = (base.len() == target.len()).then(|| {
            base.iter()
                .zip(target)
                .enumerate()
                .filter(|(_, (base, target))| base != target)
                .map(|(index, (_, target))| (index as u32, target.clone()))
                .collect::<Vec<_>>()
        });
        // Only a long run of replacements can be a shift worth an edit search.
        let edits = match &replace {
            Some(replace) if replace.len() <= MIN_SHIFT_REPLACEMENTS => None,
            _ => edit_script(base, target),
        };
        let carried = |edits: &[Edit<T>]| {
            edits
                .iter()
                .map(|edit| match edit {
                    Edit::Insert(values) => values.len(),
                    Edit::Keep(_) | Edit::Delete(_) => 0,
                })
                .sum::<usize>()
        };
        match (replace, edits) {
            (Some(replace), Some(edits)) if carried(&edits) < replace.len() => Self::Edits(edits),
            (Some(replace), _) => Self::Replace(replace),
            (None, Some(edits)) => Self::Edits(edits),
            (None, None) => Self::Whole(target.to_vec()),
        }
    }

    /// `base` with this delta applied.
    pub(crate) fn apply(&self, base: &[T]) -> Result<Vec<T>, ChartDeltaError> {
        match self {
            Self::Whole(values) => Ok(values.clone()),
            Self::Replace(replacements) => {
                let mut values = base.to_vec();
                for (index, value) in replacements {
                    let slot = values
                        .get_mut(*index as usize)
                        .ok_or(ChartDeltaError::IndexOutOfRange)?;
                    *slot = value.clone();
                }
                Ok(values)
            }
            Self::Edits(edits) => apply_edits(base, edits),
        }
    }
}

fn apply_edits<T: Clone>(base: &[T], edits: &[Edit<T>]) -> Result<Vec<T>, ChartDeltaError> {
    let mut values = Vec::with_capacity(base.len());
    let mut cursor = 0usize;
    for edit in edits {
        match edit {
            Edit::Keep(count) => {
                let end = cursor + *count as usize;
                let kept = base
                    .get(cursor..end)
                    .ok_or(ChartDeltaError::IndexOutOfRange)?;
                values.extend_from_slice(kept);
                cursor = end;
            }
            Edit::Delete(count) => cursor += *count as usize,
            Edit::Insert(inserted) => values.extend_from_slice(inserted),
        }
    }
    if cursor != base.len() {
        return Err(ChartDeltaError::IndexOutOfRange);
    }
    Ok(values)
}

/// A shortest edit script from `base` to `target` (Myers' greedy algorithm),
/// or `None` past [`MAX_EDIT_DISTANCE`].
fn edit_script<T: Clone + PartialEq>(base: &[T], target: &[T]) -> Option<Vec<Edit<T>>> {
    let (n, m) = (base.len() as isize, target.len() as isize);
    let offset = MAX_EDIT_DISTANCE as isize + 1;
    let mut frontier = vec![0isize; 2 * offset as usize + 1];
    let mut trace = Vec::new();
    for distance in 0..=MAX_EDIT_DISTANCE as isize {
        trace.push(frontier.clone());
        for diagonal in (-distance..=distance).step_by(2) {
            let x = furthest_reach(&frontier, offset, distance, diagonal, base, target);
            frontier[(diagonal + offset) as usize] = x;
            if x >= n && x - diagonal >= m {
                return Some(backtrack(&trace, base.len(), target, offset, distance));
            }
        }
    }
    None
}

/// The furthest base index diagonal `diagonal` reaches at `distance`, after
/// following its snake of equal entries.
fn furthest_reach<T: PartialEq>(
    frontier: &[isize],
    offset: isize,
    distance: isize,
    diagonal: isize,
    base: &[T],
    target: &[T],
) -> isize {
    let at = |k: isize| frontier[(k + offset) as usize];
    let mut x =
        if diagonal == -distance || (diagonal != distance && at(diagonal - 1) < at(diagonal + 1)) {
            at(diagonal + 1)
        } else {
            at(diagonal - 1) + 1
        };
    let mut y = x - diagonal;
    while (x as usize) < base.len()
        && (y as usize) < target.len()
        && base[x as usize] == target[y as usize]
    {
        x += 1;
        y += 1;
    }
    x
}

/// Walk the recorded frontiers back from the end into an edit script.
fn backtrack<T: Clone>(
    trace: &[Vec<isize>],
    n: usize,
    target: &[T],
    offset: isize,
    distance: isize,
) -> Vec<Edit<T>> {
    let (mut x, mut y) = (n as isize, target.len() as isize);
    let mut steps = Vec::new();
    for d in (1..=distance).rev() {
        let frontier = &trace[d as usize];
        let at = |k: isize| frontier[(k + offset) as usize];
        let diagonal = x - y;
        let previous = if diagonal == -d || (diagonal != d && at(diagonal - 1) < at(diagonal + 1)) {
            diagonal + 1
        } else {
            diagonal - 1
        };
        let previous_x = at(previous);
        let previous_y = previous_x - previous;
        let snake = (x - previous_x).min(y - previous_y).max(0);
        steps.extend(std::iter::repeat_n(Step::Keep, snake as usize));
        x -= snake;
        y -= snake;
        steps.push(if x == previous_x {
            Step::Insert(y as usize - 1)
        } else {
            Step::Delete
        });
        (x, y) = (previous_x, previous_y);
    }
    steps.extend(std::iter::repeat_n(Step::Keep, x as usize));
    steps.reverse();
    compress(&steps, target)
}

#[derive(Clone, Copy)]
enum Step {
    Keep,
    Delete,
    Insert(usize),
}

fn compress<T: Clone>(steps: &[Step], target: &[T]) -> Vec<Edit<T>> {
    let mut edits: Vec<Edit<T>> = Vec::new();
    for step in steps {
        match (step, edits.last_mut()) {
            (Step::Keep, Some(Edit::Keep(count))) | (Step::Delete, Some(Edit::Delete(count))) => {
                *count += 1;
            }
            (Step::Insert(index), Some(Edit::Insert(values))) => {
                values.push(target[*index].clone())
            }
            (Step::Keep, _) => edits.push(Edit::Keep(1)),
            (Step::Delete, _) => edits.push(Edit::Delete(1)),
            (Step::Insert(index), _) => edits.push(Edit::Insert(vec![target[*index].clone()])),
        }
    }
    edits
}

/// A chart delta that cannot be applied to its primary, or does not reproduce
/// the plan it was issued for.
#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum ChartDeltaError {
    #[error("a reduced-chart delta replaces an entry past the primary's end")]
    IndexOutOfRange,
    #[error("a reduced-chart delta does not reproduce a valid program block")]
    InvalidProgramBlock,
    #[error("a reduced-chart delta does not reproduce valid refresh owners")]
    InvalidRefreshOwners,
    #[error("a reduced-chart delta does not reproduce the lowered alternate plan")]
    NotFaithful,
}

/// A compute block as row replacements of the primary's single scalar-program
/// node, or whole when either block has another shape.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) enum ComputeBlockDelta {
    Rows {
        programs: VecDelta<Vec<LinearOp>>,
        program_spans: VecDelta<rumoca_core::Span>,
        output_indices: VecDelta<usize>,
    },
    Whole(ComputeBlock),
}

fn single_scalar_block(block: &ComputeBlock) -> Option<&ScalarProgramBlock> {
    match block.nodes.as_slice() {
        [ComputeNode::ScalarPrograms(programs)] => Some(programs),
        _ => None,
    }
}

impl ComputeBlockDelta {
    fn diff(base: &ComputeBlock, target: &ComputeBlock) -> Self {
        match (single_scalar_block(base), single_scalar_block(target)) {
            (Some(base), Some(target)) => Self::Rows {
                programs: VecDelta::diff(base.programs(), target.programs()),
                program_spans: VecDelta::diff(base.program_spans(), target.program_spans()),
                output_indices: VecDelta::diff(base.output_indices(), target.output_indices()),
            },
            _ => Self::Whole(target.clone()),
        }
    }

    fn apply(&self, base: &ComputeBlock) -> Result<ComputeBlock, ChartDeltaError> {
        let (programs, program_spans, output_indices) = match self {
            Self::Whole(block) => return Ok(block.clone()),
            Self::Rows {
                programs,
                program_spans,
                output_indices,
            } => (programs, program_spans, output_indices),
        };
        let empty = ScalarProgramBlock::default();
        let base = single_scalar_block(base).unwrap_or(&empty);
        let block = ScalarProgramBlock::with_output_indices(
            programs.apply(base.programs())?,
            program_spans.apply(base.program_spans())?,
            output_indices.apply(base.output_indices())?,
        )
        .map_err(|_| ChartDeltaError::InvalidProgramBlock)?;
        Ok(ComputeBlock::from_scalar_program_block(block))
    }
}

/// Two single scalar-program blocks with the same stored programs; any other
/// shape is not compared and counts as different.
fn same_compute_block(a: &ComputeBlock, b: &ComputeBlock) -> bool {
    match (single_scalar_block(a), single_scalar_block(b)) {
        (Some(a), Some(b)) => {
            a.programs() == b.programs()
                && a.program_spans() == b.program_spans()
                && a.output_indices() == b.output_indices()
        }
        _ => false,
    }
}

impl ComputeBlockDelta {
    /// Whether `applied` reproduces `target`. A whole replacement is the
    /// target itself; a row delta is checked row by row.
    fn reproduces(&self, applied: &ComputeBlock, target: &ComputeBlock) -> bool {
        match self {
            Self::Whole(_) => true,
            Self::Rows { .. } => same_compute_block(applied, target),
        }
    }
}

/// One refresh plan as replacements of the primary's.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct RefreshPlanDelta {
    simultaneous_blocks: VecDelta<crate::AlgebraicProjectionBlock>,
    simultaneous_block_indices: VecDelta<usize>,
    value_projection_blocks: VecDelta<crate::AlgebraicProjectionBlock>,
    rows: VecDelta<crate::AlgebraicRefreshRow>,
    causal_seed_rows: crate::RefreshRowSelection,
    static_causal_seed_rows: crate::RefreshRowSelection,
    dynamic_causal_seed_rows: crate::RefreshRowSelection,
    value_stages: VecDelta<crate::RefreshStage>,
    causal_solution_certified: bool,
}

impl RefreshPlanDelta {
    fn diff(base: &RefreshPlan, target: &RefreshPlan) -> Self {
        Self {
            simultaneous_blocks: VecDelta::diff(
                &base.simultaneous_plan.blocks,
                &target.simultaneous_plan.blocks,
            ),
            simultaneous_block_indices: VecDelta::diff(
                &base.simultaneous_block_indices,
                &target.simultaneous_block_indices,
            ),
            value_projection_blocks: VecDelta::diff(
                &base.value_projection_plan.blocks,
                &target.value_projection_plan.blocks,
            ),
            rows: VecDelta::diff(&base.rows, &target.rows),
            causal_seed_rows: target.causal_seed_rows.clone(),
            static_causal_seed_rows: target.static_causal_seed_rows.clone(),
            dynamic_causal_seed_rows: target.dynamic_causal_seed_rows.clone(),
            value_stages: VecDelta::diff(&base.value_stages, &target.value_stages),
            causal_solution_certified: target.causal_solution_certified,
        }
    }

    fn apply(&self, base: &RefreshPlan) -> Result<RefreshPlan, ChartDeltaError> {
        Ok(RefreshPlan {
            static_causal_sequence: Default::default(),
            dynamic_causal_sequence: Default::default(),
            simultaneous_plan: AlgebraicProjectionPlan {
                blocks: self
                    .simultaneous_blocks
                    .apply(&base.simultaneous_plan.blocks)?,
            },
            simultaneous_block_indices: self
                .simultaneous_block_indices
                .apply(&base.simultaneous_block_indices)?,
            value_projection_plan: AlgebraicProjectionPlan {
                blocks: self
                    .value_projection_blocks
                    .apply(&base.value_projection_plan.blocks)?,
            },
            rows: self.rows.apply(&base.rows)?,
            causal_seed_rows: self.causal_seed_rows.clone(),
            static_causal_seed_rows: self.static_causal_seed_rows.clone(),
            dynamic_causal_seed_rows: self.dynamic_causal_seed_rows.clone(),
            value_stages: self.value_stages.apply(&base.value_stages)?,
            causal_solution_certified: self.causal_solution_certified,
        })
    }
}

/// Wire-visible fields of one refresh plan; the sequence identities are
/// reissued by the checked owner construction.
fn same_refresh_plan(a: &RefreshPlan, b: &RefreshPlan) -> bool {
    a.simultaneous_plan == b.simultaneous_plan
        && a.simultaneous_block_indices == b.simultaneous_block_indices
        && a.value_projection_plan == b.value_projection_plan
        && a.rows == b.rows
        && a.causal_seed_rows == b.causal_seed_rows
        && a.static_causal_seed_rows == b.static_causal_seed_rows
        && a.dynamic_causal_seed_rows == b.dynamic_causal_seed_rows
        && a.value_stages == b.value_stages
        && a.causal_solution_certified == b.causal_solution_certified
}

/// Complete refresh owners as per-plan deltas of the primary's.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct RefreshOwnersDelta {
    algebraic: RefreshPlanDelta,
    derivative: RefreshPlanDelta,
    root: RefreshPlanDelta,
    event: RefreshPlanDelta,
    clock_events: Vec<RefreshPlan>,
}

impl RefreshOwnersDelta {
    fn diff(base: &ContinuousRefreshOwners, target: &ContinuousRefreshOwners) -> Self {
        Self {
            algebraic: RefreshPlanDelta::diff(base.algebraic(), target.algebraic()),
            derivative: RefreshPlanDelta::diff(base.derivative(), target.derivative()),
            root: RefreshPlanDelta::diff(base.root(), target.root()),
            event: RefreshPlanDelta::diff(base.event(), target.event()),
            clock_events: target.clock_events().to_vec(),
        }
    }

    fn apply(
        &self,
        base: &ContinuousRefreshOwners,
    ) -> Result<ContinuousRefreshOwners, ChartDeltaError> {
        ContinuousRefreshOwners::from_wire_plans(
            self.algebraic.apply(base.algebraic())?,
            self.derivative.apply(base.derivative())?,
            self.root.apply(base.root())?,
            self.event.apply(base.event())?,
            self.clock_events.clone(),
        )
        .map_err(|_| ChartDeltaError::InvalidRefreshOwners)
    }
}

fn same_refresh_owners(a: &ContinuousRefreshOwners, b: &ContinuousRefreshOwners) -> bool {
    same_refresh_plan(a.algebraic(), b.algebraic())
        && same_refresh_plan(a.derivative(), b.derivative())
        && same_refresh_plan(a.root(), b.root())
        && same_refresh_plan(a.event(), b.event())
        && a.clock_events().len() == b.clock_events().len()
        && a.clock_events()
            .iter()
            .zip(b.clock_events())
            .all(|(a, b)| same_refresh_plan(a, b))
}

/// One alternate chart's plan as typed deltas against the primary system.
/// The residual doubles as the implicit right-hand side in every lowered
/// system, so one delta carries both.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ChartPlanDelta {
    residual: ComputeBlockDelta,
    implicit_rhs: Option<ComputeBlockDelta>,
    implicit_row_targets: VecDelta<Option<ScalarSlot>>,
    projection_blocks: VecDelta<crate::AlgebraicProjectionBlock>,
    derivative_rhs: ComputeBlock,
    refresh_owners: RefreshOwnersDelta,
}

impl ChartPlanDelta {
    /// The delta of `plan` against `primary`, issued only once applying it to
    /// the primary is proved to reproduce `plan` exactly.
    pub fn diff(
        primary: &ContinuousSolveSystem,
        plan: &ReducedChartPlan,
    ) -> Result<Self, ChartDeltaError> {
        let implicit_rhs = (!same_compute_block(&plan.implicit_rhs, &plan.residual))
            .then(|| ComputeBlockDelta::diff(&primary.implicit_rhs, &plan.implicit_rhs));
        let delta = Self {
            residual: ComputeBlockDelta::diff(&primary.residual, &plan.residual),
            implicit_rhs,
            implicit_row_targets: VecDelta::diff(
                &primary.implicit_row_targets,
                &plan.implicit_row_targets,
            ),
            projection_blocks: VecDelta::diff(
                &primary.algebraic_projection_plan.blocks,
                &plan.algebraic_projection_plan.blocks,
            ),
            derivative_rhs: plan.derivative_rhs.clone(),
            refresh_owners: RefreshOwnersDelta::diff(&primary.refresh_owners, &plan.refresh_owners),
        };
        let applied = delta.apply(primary)?;
        if delta.faithful(&applied, plan) {
            Ok(delta)
        } else {
            Err(ChartDeltaError::NotFaithful)
        }
    }

    /// The plan this delta encodes, patched onto `primary`. Its artifacts are
    /// derived data rebuilt by the owning phase.
    pub(crate) fn apply(
        &self,
        primary: &ContinuousSolveSystem,
    ) -> Result<ReducedChartPlan, ChartDeltaError> {
        let residual = self.residual.apply(&primary.residual)?;
        let implicit_rhs = match &self.implicit_rhs {
            Some(delta) => delta.apply(&primary.implicit_rhs)?,
            None => residual.clone(),
        };
        Ok(ReducedChartPlan {
            implicit_rhs,
            implicit_row_targets: self
                .implicit_row_targets
                .apply(&primary.implicit_row_targets)?,
            algebraic_projection_plan: AlgebraicProjectionPlan {
                blocks: self
                    .projection_blocks
                    .apply(&primary.algebraic_projection_plan.blocks)?,
            },
            residual,
            derivative_rhs: self.derivative_rhs.clone(),
            refresh_owners: self.refresh_owners.apply(&primary.refresh_owners)?,
            artifacts: ContinuousSolveArtifacts::default(),
            delta: Some(self.clone()),
        })
    }
}

impl ChartPlanDelta {
    /// Whether `applied` carries the same lowered content as `plan`: every
    /// program, target, block, and refresh-owner field the wire records. The
    /// derivative kernel is carried whole.
    fn faithful(&self, applied: &ReducedChartPlan, plan: &ReducedChartPlan) -> bool {
        let implicit_rhs = match &self.implicit_rhs {
            Some(delta) => delta.reproduces(&applied.implicit_rhs, &plan.implicit_rhs),
            None => same_compute_block(&applied.implicit_rhs, &plan.implicit_rhs),
        };
        self.residual.reproduces(&applied.residual, &plan.residual)
            && implicit_rhs
            && applied.implicit_row_targets == plan.implicit_row_targets
            && applied.algebraic_projection_plan == plan.algebraic_projection_plan
            && same_refresh_owners(&applied.refresh_owners, &plan.refresh_owners)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_vec_delta_replaces_changed_entries_or_the_whole_vector() {
        let base = [1, 2, 3, 4];
        let delta = VecDelta::diff(&base, &[1, 9, 3, 4]);
        assert_eq!(delta, VecDelta::Replace(vec![(1, 9)]));
        assert_eq!(delta.apply(&base).unwrap(), vec![1, 9, 3, 4]);
        let whole = VecDelta::Whole(vec![5]);
        assert_eq!(whole.apply(&base).unwrap(), vec![5]);
        assert_eq!(
            VecDelta::Replace(vec![(7, 0)]).apply(&base),
            Err(ChartDeltaError::IndexOutOfRange)
        );
    }

    #[test]
    fn an_edit_script_reproduces_insertions_and_deletions() {
        let base = [1, 2, 3, 4, 5, 6, 7, 8];
        for target in [
            vec![1, 2, 9, 3, 4, 5, 6, 7, 8],
            vec![1, 3, 4, 5, 6, 8],
            vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            vec![2, 2, 3, 4, 4, 5, 6],
            vec![],
        ] {
            let delta = VecDelta::diff(&base, &target);
            assert!(matches!(delta, VecDelta::Edits(_)), "{target:?}");
            assert_eq!(delta.apply(&base).unwrap(), target);
        }
        let delta = VecDelta::diff(&[] as &[i32], &[1, 2]);
        assert_eq!(delta.apply(&[]).unwrap(), vec![1, 2]);
    }
}
