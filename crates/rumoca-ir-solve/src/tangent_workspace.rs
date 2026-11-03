//! The ordered occurrences and workspace of one directional-derivative
//! evaluation, issued from its checked phase schedule.
//!
//! A tangent evaluation of a [`SolveProblem`] keeps one seed column, over
//! the problem's seed domain (its `y` and `p` scalars and time), and one
//! column of its state scalars for its whole run, and runs its occurrences
//! one after another, in exactly this order: an optional implicit solve,
//! each simultaneous assignment segment, the implicit solve's verification
//! (present exactly when the solve is), an optional consistency check, and
//! the right-hand side. [`SolveTangentSchedule`] admits only that order,
//! built from the checked program of each occurrence; each occurrence's
//! demand is read from its program. A consumer runs the issued occurrences
//! in issued order and decides none of it.
//!
//! Because the occurrences are sequential, one scratch region of the largest
//! demand serves all of them, and the implicit solve, its verification and
//! the consistency check, the occurrences that also need a private region
//! (each beginning with its own saved seed), share one region of the largest
//! demand. The issued workspace keeps the schedule it was issued for (the
//! problem and each occurrence's program, by identity), so a consumer
//! refuses a workspace issued for another problem or programs.

#[cfg(test)]
mod tests;

use serde::Serialize;

use crate::{ScalarProgramBlock, SolveProblem};

/// One occurrence of a tangent schedule, in issued order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SolveTangentOccurrence {
    /// Solve the implicit system's `unknowns` targets.
    ImplicitSolve { unknowns: usize },
    /// Assign simultaneous segment `segment` of `targets` targets.
    Assignment { segment: usize, targets: usize },
    /// Verify the implicit solve's residuals.
    ImplicitVerification { unknowns: usize },
    /// Check `rows` consistency rows.
    Consistency { rows: usize },
    /// Evaluate the right-hand side.
    Rhs,
}

/// The checked, sequential occurrence schedule of one tangent evaluation.
#[derive(Debug, Clone)]
pub struct SolveTangentSchedule<'problem> {
    problem: &'problem SolveProblem,
    seed: usize,
    states: usize,
    occurrences: Vec<(SolveTangentOccurrence, ScalarProgramBlock)>,
}

/// The issued occurrences and workspace extents, with the schedule they were
/// issued for.
#[derive(Debug, Clone)]
pub struct SolveTangentWorkspace<'problem> {
    implicit_matrix_len: usize,
    implicit_pivot_len: usize,
    scratch_len: usize,
    private_len: usize,
    work_len: usize,
    schedule: SolveTangentSchedule<'problem>,
}

/// Why a tangent schedule or workspace is refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveTangentWorkspaceError {
    /// The seed domain of the problem overflows.
    SeedDomain,
    /// An assignment segment's outputs are not exactly its targets in order.
    SegmentCoverage,
    /// An occurrence was added out of order or twice, or the schedule is
    /// incomplete: no right-hand side, or an implicit solve without exactly
    /// its verification.
    OccurrenceOrder,
    ImplicitMatrix,
    ImplicitPivot,
    ImplicitPrivate,
    ConsistencyPrivate,
    Workspace,
    /// The workspace was issued for another schedule.
    ForeignSchedule,
}

impl std::fmt::Display for SolveTangentWorkspaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::SeedDomain => "tangent seed extent overflow",
            Self::SegmentCoverage => {
                "tangent segment does not cover its simultaneous target inventory"
            }
            Self::OccurrenceOrder => "tangent occurrences are not in their sequential order",
            Self::ImplicitMatrix => "implicit matrix extent overflow",
            Self::ImplicitPivot => "implicit pivot extent overflow",
            Self::ImplicitPrivate => "implicit workspace extent overflow",
            Self::ConsistencyPrivate => "consistency workspace extent overflow",
            Self::Workspace => "tangent workspace extent overflow",
            Self::ForeignSchedule => "tangent workspace was issued for another schedule",
        })
    }
}

impl std::error::Error for SolveTangentWorkspaceError {}

impl SolveTangentOccurrence {
    /// Occurrences run in this order; only assignments repeat.
    const fn rank(self) -> u8 {
        match self {
            Self::ImplicitSolve { .. } => 0,
            Self::Assignment { .. } => 1,
            Self::ImplicitVerification { .. } => 2,
            Self::Consistency { .. } => 3,
            Self::Rhs => 4,
        }
    }
}

impl<'problem> SolveTangentSchedule<'problem> {
    /// Elements of `problem`'s tangent seed domain: one direction per `y`
    /// and `p` scalar, and one for time.
    pub fn seed_len(problem: &SolveProblem) -> Result<usize, SolveTangentWorkspaceError> {
        problem
            .layout
            .y_scalars()
            .checked_add(problem.layout.p_scalars())
            .and_then(|length| length.checked_add(1))
            .ok_or(SolveTangentWorkspaceError::SeedDomain)
    }

    /// An empty schedule of `problem`; its seed and state columns are the
    /// problem's own.
    pub fn new(problem: &'problem SolveProblem) -> Result<Self, SolveTangentWorkspaceError> {
        Ok(Self {
            problem,
            seed: Self::seed_len(problem)?,
            states: problem.solve_layout.state_scalar_count(),
            occurrences: Vec::new(),
        })
    }

    /// The implicit solve of `unknowns` simultaneous targets over the
    /// residual `program`.
    pub fn implicit_solve(
        self,
        program: &ScalarProgramBlock,
        unknowns: usize,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        self.push(SolveTangentOccurrence::ImplicitSolve { unknowns }, program)
    }

    /// The next simultaneous assignment segment of `targets` targets, whose
    /// program writes exactly its targets in order.
    pub fn assignment(
        self,
        program: &ScalarProgramBlock,
        targets: usize,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        if program.output_indices().iter().copied().ne(0..targets) {
            return Err(SolveTangentWorkspaceError::SegmentCoverage);
        }
        let segment = self
            .occurrences
            .iter()
            .filter(|(occurrence, _)| {
                matches!(occurrence, SolveTangentOccurrence::Assignment { .. })
            })
            .count();
        self.push(
            SolveTangentOccurrence::Assignment { segment, targets },
            program,
        )
    }

    /// The verification of the implicit solve, over the solve's own
    /// residual program.
    pub fn implicit_verification(
        self,
        program: &ScalarProgramBlock,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        let Some((SolveTangentOccurrence::ImplicitSolve { unknowns }, solved)) =
            self.occurrences.first()
        else {
            return Err(SolveTangentWorkspaceError::OccurrenceOrder);
        };
        if !solved.shares_program_owner(program) {
            return Err(SolveTangentWorkspaceError::OccurrenceOrder);
        }
        let unknowns = *unknowns;
        self.push(
            SolveTangentOccurrence::ImplicitVerification { unknowns },
            program,
        )
    }

    /// The consistency check of `rows` checked rows.
    pub fn consistency(
        self,
        program: &ScalarProgramBlock,
        rows: usize,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        self.push(SolveTangentOccurrence::Consistency { rows }, program)
    }

    /// The right-hand side, the last occurrence.
    pub fn rhs(self, program: &ScalarProgramBlock) -> Result<Self, SolveTangentWorkspaceError> {
        self.push(SolveTangentOccurrence::Rhs, program)
    }

    fn push(
        mut self,
        occurrence: SolveTangentOccurrence,
        program: &ScalarProgramBlock,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        if let Some((last, _)) = self.occurrences.last() {
            let repeats = matches!(occurrence, SolveTangentOccurrence::Assignment { .. });
            if occurrence.rank() < last.rank() || (occurrence.rank() == last.rank() && !repeats) {
                return Err(SolveTangentWorkspaceError::OccurrenceOrder);
            }
        }
        self.occurrences.push((occurrence, program.clone()));
        Ok(self)
    }

    fn has(&self, rank: u8) -> bool {
        self.occurrences
            .iter()
            .any(|(occurrence, _)| occurrence.rank() == rank)
    }

    /// Whether `other` schedules the same problem, occurrences and programs.
    fn same(&self, other: &Self) -> bool {
        std::ptr::eq(self.problem, other.problem)
            && self.occurrences.len() == other.occurrences.len()
            && self
                .occurrences
                .iter()
                .zip(&other.occurrences)
                .all(|((a, pa), (b, pb))| a == b && pa.shares_program_owner(pb))
    }

    /// Issue the occurrences and the workspace this schedule needs.
    pub fn issue(self) -> Result<SolveTangentWorkspace<'problem>, SolveTangentWorkspaceError> {
        let complete = self.has(SolveTangentOccurrence::Rhs.rank()) && self.has(0) == self.has(2);
        if !complete {
            return Err(SolveTangentWorkspaceError::OccurrenceOrder);
        }
        let (mut scratch_len, mut private_len) = (0usize, 0usize);
        let (mut implicit_matrix_len, mut implicit_pivot_len) = (0, 0);
        for (occurrence, program) in &self.occurrences {
            scratch_len = scratch_len.max(program.output_count());
            match *occurrence {
                SolveTangentOccurrence::Assignment { targets, .. } => {
                    scratch_len = scratch_len.max(targets);
                }
                SolveTangentOccurrence::ImplicitSolve { unknowns: n } => {
                    implicit_pivot_len = n
                        .checked_mul(2)
                        .ok_or(SolveTangentWorkspaceError::ImplicitPivot)?;
                    implicit_matrix_len = n
                        .checked_mul(n)
                        .ok_or(SolveTangentWorkspaceError::ImplicitMatrix)?;
                    let own = self
                        .seed
                        .checked_add(implicit_matrix_len)
                        .and_then(|length| length.checked_add(n))
                        .ok_or(SolveTangentWorkspaceError::ImplicitPrivate)?;
                    private_len = private_len.max(own);
                }
                SolveTangentOccurrence::ImplicitVerification { unknowns }
                | SolveTangentOccurrence::Consistency { rows: unknowns } => {
                    let own = self
                        .seed
                        .checked_add(unknowns)
                        .ok_or(SolveTangentWorkspaceError::ConsistencyPrivate)?;
                    private_len = private_len.max(own);
                }
                SolveTangentOccurrence::Rhs => {}
            }
        }
        let work_len = self
            .seed
            .checked_add(self.states)
            .and_then(|length| length.checked_add(scratch_len))
            .and_then(|length| length.checked_add(private_len))
            .ok_or(SolveTangentWorkspaceError::Workspace)?;
        Ok(SolveTangentWorkspace {
            implicit_matrix_len,
            implicit_pivot_len,
            scratch_len,
            private_len,
            work_len,
            schedule: self,
        })
    }
}

impl<'problem> SolveTangentWorkspace<'problem> {
    /// This workspace, when it was issued for exactly `schedule`.
    pub fn for_schedule(
        &self,
        schedule: &SolveTangentSchedule<'_>,
    ) -> Result<&Self, SolveTangentWorkspaceError> {
        if self.schedule.same(schedule) {
            Ok(self)
        } else {
            Err(SolveTangentWorkspaceError::ForeignSchedule)
        }
    }

    /// The occurrences to run, in order.
    pub fn occurrences(&self) -> impl Iterator<Item = SolveTangentOccurrence> + '_ {
        self.schedule
            .occurrences
            .iter()
            .map(|(occurrence, _)| *occurrence)
    }

    /// Whether any occurrence reads the seed column.
    #[must_use]
    pub const fn uses_seed(&self) -> bool {
        self.schedule.seed > 0
    }

    /// Whether the right-hand side writes any state.
    #[must_use]
    pub const fn uses_states(&self) -> bool {
        self.schedule.states > 0
    }

    /// Whether any occurrence but the right-hand side uses the scratch
    /// region.
    #[must_use]
    pub fn uses_scratch(&self) -> bool {
        self.occurrences()
            .any(|occurrence| occurrence != SolveTangentOccurrence::Rhs)
    }

    /// Whether any occurrence uses the private region.
    #[must_use]
    pub const fn uses_private(&self) -> bool {
        self.private_len > 0
    }

    /// Whether the implicit solve's pivots are used.
    #[must_use]
    pub const fn uses_pivots(&self) -> bool {
        self.implicit_pivot_len > 0
    }

    /// Elements of the implicit system's dense matrix.
    #[must_use]
    pub const fn implicit_matrix_len(&self) -> usize {
        self.implicit_matrix_len
    }

    /// Pivot and row-size entries of the implicit solve.
    #[must_use]
    pub const fn implicit_pivot_len(&self) -> usize {
        self.implicit_pivot_len
    }

    /// The scratch region every occurrence reuses in turn.
    #[must_use]
    pub const fn scratch_len(&self) -> usize {
        self.scratch_len
    }

    /// The private region the implicit solve, its verification and the
    /// consistency check reuse in turn.
    #[must_use]
    pub const fn private_len(&self) -> usize {
        self.private_len
    }

    /// The whole workspace: seed, states, scratch and the private region.
    #[must_use]
    pub const fn work_len(&self) -> usize {
        self.work_len
    }
}
